/*
 * Copyright © 2015 Reactific Software LLC. All Rights Reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package rxmongo.driver

import java.net.{ InetSocketAddress }

import akka.actor.SupervisorStrategy._
import akka.actor._
import akka.routing.{ Broadcast, DefaultResizer, SmallestMailboxPool }
import akka.pattern.ask
import rxmongo.bson.BSONBoolean

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration

object Connection {
  def props(uri : MongoURI) = Props(classOf[Connection], uri)

  sealed trait ConnectionRequest
  case class ChannelClosed(chan : ActorRef) extends ConnectionRequest
  case object OpenChannel extends ConnectionRequest
  case class RunCommand(command : Command) extends ConnectionRequest
}

/** Connection To MongoDB Replica Set
  *
  * This represents RxMongo's connection to a replica set.
  */
class Connection(uri : MongoURI) extends Actor with ActorLogging {

  import rxmongo.driver.Connection._

  /** Supervision Strategy Decider
    * This "Decider" determines what to do for a given exception type. For now, all exceptions cause restart of the
    * actor.
    * @return An Akka Decider object
    */
  final def decider : Decider = { case t : Throwable ⇒ SupervisorStrategy.Restart }

  /** Supervision Object
    * We use a OneForOneStrategy so that failure decisions are done on an actor-by-actor basis.
    */
  val supervisionStrategy = OneForOneStrategy(
    maxNrOfRetries = -1,
    withinTimeRange = Duration.Inf,
    loggingEnabled = true)(decider = decider)

  val poolResizer =
    DefaultResizer(
      lowerBound = uri.options.minPoolSize,
      upperBound = uri.options.maxPoolSize,
      pressureThreshold = 1, // Considered "busy" if 1 message in the mailbox
      rampupRate = uri.options.rampupRate,
      backoffThreshold = uri.options.backoffThreshold,
      backoffRate = uri.options.backoffRate,
      messagesPerResize = uri.options.messagesPerResize
    )

  /** The Channel pool configuration.
    *
    * This provides the configuration details for the router(s) the
    */
  val routerConfig = SmallestMailboxPool(
    nrOfInstances = uri.options.minPoolSize,
    supervisorStrategy = supervisionStrategy,
    resizer = Some(poolResizer)
  )

  /** The address of the primary node in the replica set. It is assumed, at initialization, that the first host in
    * URI is the primary. If that doesn't hold out to be true, it will be remedied at the first message request by
    * polling for the primary.
    */
  var primary_addr : InetSocketAddress = uri.hosts.head
  var msgs_to_next_check : Int = 1

  def maybeCheckReplicaSet() = {
    implicit val executionContext : ExecutionContext = context.dispatcher
    msgs_to_next_check -= 1
    if (msgs_to_next_check == 0) {
      msgs_to_next_check = uri.options.messagesPerResize
      primary_router.ask(IsMasterCmd)(Driver.defaultTimeout) map {
        case msg : ReplyMessage ⇒ {
          if (msg.numberReturned > 0) {
            val doc = msg.documents.head
            /*          doc.getAs[Boolean]("ismaster") match {
              case Success(Some(x)) ⇒
                log.warning("Current primary is confirmed as primary")
              case Success(None) ⇒
                log.warning("Response from IsMasterCmd had no 'ismaster' field")
              case Failure(xcptn) ⇒
                log.warning("Could not extract 'ismaster' field from IsMasterCmd response", xcptn)
            } */
            doc.get("ismaster") match {
              case Some(v) ⇒
                v match {
                  case b : BSONBoolean ⇒
                    if (b.value) {
                      log.warning("Current primary is confirmed as primary.")
                    } else {
                      log.warning("Current primary is no longer the primary node.")
                    }
                }
              case None ⇒
                log.warning("Response from IsMasterCmd had no 'ismaster' field")
            }
          } else {
            log.warning("Got no response from IsMasterCmd")
          }
        }
        case x : Any ⇒ log.warning(s"Got junk from IsMasterCmd")
      }
    }
  }

  /** Primary Router
    * This is the router that manages a pool of Channels to the MongoDB Primary server. It is configured according
    * to the ConnectionOptions in the MongoURI. All messages sent to this connection are distributed across the
    * Channels in this router. The router is lazy instantiated so there is no cost of pool set up if this connection
    * is never used.
    */
  lazy val primary_router = context.actorOf(
    Channel.
      props(primary_addr, uri.options, self, isPrimary = true).
      withRouter(routerConfig),
    Driver.actorName("PrimaryChannel")
  )

  override def preStart() = {
    super.preStart()
  }

  def receive = {
    case PoisonPill ⇒
      primary_router ! Broadcast(PoisonPill)
      primary_router ! PoisonPill
      context become closing

    case RunCommand(command) ⇒
      maybeCheckReplicaSet()
      primary_router ! command

    case Channel.UnsolicitedReply(reply) ⇒
      log.warning(s"Unsolicited Reply from MongoDB: $reply (ignored).")

  }

  def closing : Receive = {
    case RunCommand ⇒
      log.warning("Ignoring RunCommand request while closing connection")
    case Terminated(actor : ActorRef) ⇒
      if (actor == primary_router) {
        context.stop(self)
      }
  }
}
