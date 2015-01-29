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

import java.net.InetSocketAddress

import akka.actor.SupervisorStrategy._
import akka.actor._
import akka.event.LoggingReceive
import akka.routing.{ Broadcast, DefaultResizer, SmallestMailboxPool }

import rxmongo.bson.{ BSONObject, BSONString, BSONBoolean }

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.collection.mutable

object Connection {
  def props(uri : MongoURI) = Props(classOf[Connection], uri)

  sealed trait ConnectionRequest

  case class ChannelClosed(chan : ActorRef) extends ConnectionRequest

  case object GetStatus extends ConnectionRequest

  case object Close extends ConnectionRequest

  case object CheckReplicaSet extends ConnectionRequest

  case object Closed

  sealed trait ConnectionResponse

  case class ConnectionStatus(
    numChannels : Int,
    numMessages : Int,
    numResponses : Int,
    maxBsonObjectSize : Int,
    maxMessageSize : Int,
    maxWriteBatchWize : Int,
    maxWireVersion : Int,
    minWireVersion : Int) extends ConnectionResponse

}

/** Connection To MongoDB Replica Set
  *
  * This represents RxMongo's connection to a replica set.
  */
class Connection(uri : MongoURI) extends Actor with ActorLogging {

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

  implicit val ec : ExecutionContext = context.system.dispatcher

  /** The addresses of the nodes in the replica set.
    *
    * This list starts out empty and is updated by the initial MongoURI's addresses and then subsequently by
    * the responses to isMaster command. As the replica set changes so will this list
    */
  var addresses : List[InetSocketAddress] = List.empty[InetSocketAddress]

  /** Update The Replica Set
    *
    * This function processes the reply from the IsMaster command that contains replica set and other information.
    * The
    * @see [[http://docs.mongodb.org/master/reference/command/isMaster/]]
    */
  def handleReplicaSetUpdate(doc : BSONObject) = {
    // We're going to query this doc a lot, convert it to a map
    val map = doc.toMap
    map.contains("")
  }

  /** Obtain the address of the next node in the replica set.
    *
    * This function treats the list of addresses like a ring buffer wrapping back to the head when it walks
    * off the tail.
    * @return
    */
  def nextAddr : InetSocketAddress = {
    if (addresses.isEmpty) {
      addresses = uri.hosts
    }
    val result = addresses.head
    addresses = addresses.tail
    result
  }

  var current_addr = nextAddr

  /** Primary Router
    * This is the router that manages a pool of Channels to the MongoDB Primary server. It is configured according
    * to the ConnectionOptions in the MongoURI. All messages sent to this connection are distributed across the
    * Channels in this router. The router is lazy instantiated so there is no cost of pool set up if this connection
    * is never used.
    */
  var primary_router : ActorRef = null

  var msgs_to_next_check : Int = 1

  override def preStart() = {
    open()
  }

  def open() : Unit = {
    val router = context.actorOf(
      Channel.
        props(current_addr, uri.options, self, isPrimary = true).
        withRouter(routerConfig),
      Driver.actorName("PrimaryChannel")
    )
    log.debug("Created Router: {}", router)
    context become (waitForChannelConnected(router), discardOld = true)
  }

  def close() : Unit = {
    log.debug("Closing")
    if (primary_router != null) {
      primary_router ! Broadcast(Channel.Close)
      context become (closing(sender()), discardOld = true)
    } else {
      context.stop(self)
    }
  }

  val pendingRequestQueue = mutable.Queue.empty[Channel.SendMessage]

  def enqueue(msg : RequestMessage) = {
    log.debug("Queueing message {}", msg)
    pendingRequestQueue.enqueue(Channel.SendMessage(msg, replyTo = sender()))
  }

  def dequeueAll() : Unit = {
    assert(primary_router != null)
    for (e ← pendingRequestQueue) { primary_router ! e }
    pendingRequestQueue.clear()
  }

  def handleUnsolicited(reply : ReplyMessage) = {
    log.warning(s"Unsolicited Reply from MongoDB: $reply (ignored).")
  }

  def retryChannelConnect() = {
    primary_router = null // FIXME: Try others in replica set
    context become (waitForChannelRetry, discardOld = true)
    context.system.scheduler.scheduleOnce(uri.options.channelReconnectPeriod, self, "retry")
  }

  def waitForChannelConnected(pendingRouter : ActorRef) : Receive = LoggingReceive {
    case Connection.Close ⇒
      close()

    case msg : RequestMessage ⇒
      enqueue(msg)

    case Channel.UnsolicitedReply(reply) ⇒
      handleUnsolicited(reply)

    case Channel.ConnectionFailed(conn) ⇒
      val old_addr = current_addr
      current_addr = nextAddr
      log.debug("Connection to {} failed, trying {} next.", old_addr, current_addr)
      retryChannelConnect()

    case Channel.ConnectionSucceeded(conn) ⇒
      log.debug("Connection to {} succeeded", uri)
      primary_router = pendingRouter
      dequeueAll()
      context.become(receive, discardOld = true)
  }

  def waitForChannelRetry : Receive = LoggingReceive {
    case Connection.Close ⇒
      close()

    case msg : RequestMessage ⇒
      enqueue(msg)

    case Channel.UnsolicitedReply(reply) ⇒
      handleUnsolicited(reply)

    case "retry" ⇒
      open()
  }

  def receive : Receive = LoggingReceive {
    case Connection.Close ⇒
      close()

    case Connection.CheckReplicaSet ⇒
      msgs_to_next_check = uri.options.messagesPerResize
      val cmd = IsMasterCmd()
      context become waitForReplicaSet(cmd)
      primary_router ! Channel.SendMessage(cmd, replyTo = self)

    case msg : RequestMessage ⇒
      if (primary_router == null) {
        // There is no primary router, which shouldn't happen. Enqueue the message and then try to open again
        log.warning("No primary while in normal receive mode. Queuing and attempting open again")
        enqueue(msg)
        open()
      } else {
        msgs_to_next_check -= 1
        if (msgs_to_next_check == 0) {
          // It is time to check the primary router, first reset the counter
          msgs_to_next_check = uri.options.messagesPerResize
          enqueue(msg)
          val cmd = IsMasterCmd()
          context become waitForReplicaSet(cmd)
          primary_router ! Channel.SendMessage(cmd, replyTo = self)
        } else {
          // We assume the primary_router is still valid and send the request message
          primary_router ! Channel.SendMessage(msg, replyTo = sender())
        }
      }

    case Channel.UnsolicitedReply(reply) ⇒
      handleUnsolicited(reply)
  }

  def waitForReplicaSet(cmd : IsMasterCmd) : Receive = LoggingReceive {
    case Connection.Close ⇒
      close()

    case msg : RequestMessage ⇒
      enqueue(msg)

    case msg : ReplyMessage ⇒
      if (msg.responseTo == cmd.requestId) {
        if (msg.numberReturned > 0) {
          val doc = msg.documents.head
          log.debug("Reply from IsMasterCmd: {}", doc)
          doc.get("ismaster") match {
            case Some(v) ⇒
              v match {
                case b : BSONBoolean ⇒
                  if (b.value) {
                    log.debug("Current primary is confirmed as primary.")
                    dequeueAll()
                  } else {
                    log.debug("Current primary is no longer the primary node.")
                    primary_router ! PoisonPill
                    doc.get("primary") match {
                      case Some(x : BSONString) ⇒
                        val host_port = x.value.split(":")
                        current_addr = InetSocketAddress.createUnresolved(host_port(0), host_port(1).toInt)
                        open()
                      case _ ⇒
                        throw new IllegalStateException("MongoDB replied to IsMaster without a primary")
                    }
                  }
              }
            case None ⇒
              log.error("Reply from IsMasterCmd had no 'ismaster' field")
          }
        } else {
          log.error("No document in IsMasterCmd response")
        }
      } else {
        log.error("Expected reply to IsMasterCmd.{} but got responseTo=={}", cmd.requestId, msg.responseTo)
      }

    case Channel.UnsolicitedReply(reply) ⇒
      handleUnsolicited(reply)
  }

  def closing(replyTo : ActorRef) : Receive = {
    case x : RequestMessage ⇒
      log.info("Ignoring RunCommand request while closing connection")
    case Terminated(actor : ActorRef) ⇒
      if (actor == primary_router) {
        replyTo ! Connection.Closed
        context.stop(self)
      }
  }
}
