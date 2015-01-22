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

import scala.collection.mutable
import scala.concurrent.duration._

object Supervisor {
  def props(driver : Driver) : Props = {
    Props(classOf[Supervisor], driver)
  }

  sealed trait Request
  case class AddConnection(uri : MongoURI) extends Request
  case class Terminate(timeout : FiniteDuration) extends Request
  case object NumConnections extends Request

  sealed trait Reply
  case class NumConnectionsReply(num : Int) extends Reply
}

class Supervisor(driver : Driver) extends Actor with ActorLogging {

  import Supervisor._

  /** Keep a list of all connections so that we can terminate the actors */
  val connections = mutable.HashSet.empty[ActorRef]

  def numConnections : Int = connections.size

  def isEmpty = connections.isEmpty

  override def receive = {
    case AddConnection(uri : MongoURI) ⇒
      val connection = driver.system.actorOf(Connection.props(uri))
      connections.add(connection)
      context.watch(connection)
      sender ! connection
    case Terminated(actor) ⇒
      connections.remove(actor)
    case Terminate(timeout) ⇒
      if (isEmpty) {
        context.stop(self)
      } else {
        connections.foreach { connection ⇒
          connection ! Connection.CloseConnection
        }
        context.become(closing(timeout))
      }
  }

  def closing(shutdownTimeout : FiniteDuration) : Receive = {
    case ac : AddConnection ⇒
      log.warning("Refusing to add connection while RxMongo Supervisor is closing.")
    case Terminate(timeout) ⇒
      log.warning("Terminate ignored, already terminating.")
    case NumConnections ⇒
      sender ! NumConnectionsReply(numConnections)
    case Terminated(actor) ⇒
      connections.remove(actor)
      if (isEmpty) {
        context.stop(self)
      }
  }

  override def postStop {
    driver.system.shutdown()
  }
}
