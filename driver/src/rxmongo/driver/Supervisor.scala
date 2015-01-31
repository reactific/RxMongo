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

import akka.actor._
import akka.event.LoggingReceive

import scala.collection.mutable

object Supervisor {
  def props() : Props = {
    Props(classOf[Supervisor])
  }

  sealed trait Request
  case class AddConnection(uri : MongoURI, name : String) extends Request // reply with Connection actor
  case class DropConnection(uri : MongoURI) extends Request // no reply on success, NoSuchConnection if not found
  case object Shutdown extends Request // no reply, Supervisor shuts down
  case object NumConnections extends Request // reply with NumConnectionsReply
  case object GetConnections extends Request

  sealed trait Reply
  case class NumConnectionsReply(num : Int) extends Reply
  case class NoSuchConnection(uri : MongoURI) extends Reply
  case class GetConnectionsReply(connections: Map[MongoURI,ActorRef]) extends Reply
}

class Supervisor extends Actor with ActorLogging {

  import Supervisor._

  /** Keep a list of all connections so that we can terminate the actors */
  val connections = mutable.HashMap.empty[MongoURI, ActorRef]

  def numConnections : Int = connections.size

  def removeConnection(connection : ActorRef) : Unit = {
    connections.find { case (uri, conn) ⇒ conn == connection } match {
      case Some((uri, actor)) ⇒ connections.remove(uri)
      case None ⇒ // do nothing
    }
  }

  def exit() = {
    log.debug("RxMongo Supervisor has terminated connections and is stopping")
    context stop self
  }

  override def receive = LoggingReceive {
    case AddConnection(uri : MongoURI, name : String) ⇒
      log.debug(s"AddConnection($uri,$name)")
      val connection = context.actorOf(Connection.props(uri), Driver.actorName(name))
      context.watch(connection)
      connections.put(uri, connection)
      sender ! connection

    case DropConnection(uri : MongoURI) ⇒
      log.debug(s"DropConnection($uri)")
      connections.get(uri) match {
        case Some(connection) ⇒ connection ! Connection.Close
        case None ⇒ sender() ! NoSuchConnection(uri)
      }

    case NumConnections ⇒
      sender() ! NumConnectionsReply(numConnections)

    case GetConnections ⇒
      sender() ! GetConnectionsReply(connections.toMap)

    case Terminated(actor) ⇒
      removeConnection(actor)

    case Shutdown ⇒
      log.debug("RxMongo Supervisor is terminating connections")
      if (connections.isEmpty) {
        exit()
      } else {
        connections.foreach {
          case (uri, connection) ⇒
            connection ! PoisonPill
        }
        context.become(closing)
      }
  }

  def closing : Receive = LoggingReceive {
    case ac : AddConnection ⇒
      log.warning("Refusing to add connection while RxMongo Supervisor is closing.")

    case dc : DropConnection ⇒
      log.warning("Refusing to drop connection while RxMongo Supervisor is closing.")

    case Shutdown ⇒
      log.warning("Terminate ignored, already terminating.")

    case NumConnections ⇒
      sender ! NumConnectionsReply(numConnections)

    case Terminated(actor) ⇒
      removeConnection(actor)
      if (connections.isEmpty) {
        exit()
      }
  }
}
