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

import akka.actor.{ Actor, Props, ActorRef }
import akka.io.Tcp.Connect
import akka.io.{ IO, Tcp }
import akka.util.ByteString
import rxmongo.driver.Connection.ConnectionFailed

object Connection {
  def props(remote: InetSocketAddress, replies: ActorRef) =
    Props(classOf[Connection], remote, replies)

  sealed trait ConnectionRequest
  case class MongoCommand() extends ConnectionRequest
  case class MongoQuery() extends ConnectionRequest

  sealed trait ConnectionResponse
  case class ConnectionFailed(conn: Connect) extends ConnectionResponse
}

class Connection(remote: InetSocketAddress, listener: ActorRef) extends Actor {

  import Tcp._
  import context.system

  IO(Tcp) ! Connect(remote)

  def receive = {
    case CommandFailed(conn: Connect) =>
      listener ! ConnectionFailed(conn)
      context stop self

    case c @ Connected(remote_addr, local_addr) =>
      listener ! c
      val connection = sender()
      connection ! Register(self)
      context become connected(connection)
  }

  def connected(connection: ActorRef): Receive = {
    case data: ByteString =>
      connection ! Write(data)
    case Received(data: ByteString) =>
      listener ! data
    case CommandFailed(w: Write) =>
      // O/S buffer was full
      listener ! "write failed"

    case Closed =>
      /**
       * The connection has been closed normally in response to a [[Close]] command.
       */
      context stop self
    case Aborted =>
      /**
       * The connection has been aborted in response to an [[Abort]] command.
       */
      context stop self
    case ConfirmedClosed =>
      /**
       * The connection has been half-closed by us and then half-close by the peer
       * in response to a [[ConfirmedClose]] command.
       */
      context stop self
    case PeerClosed =>
      /**
       * The peer has closed its writing half of the connection.
       */
      context stop self
    case ErrorClosed(cause: String) =>
      /**
       * The connection has been closed due to an IO error.
       */
      context stop self
    case _: ConnectionClosed =>
      listener ! "connection closed"
      context stop self
  }
}
