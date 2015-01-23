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

import java.net.{ InetAddress, Socket, InetSocketAddress }

import akka.actor.{ ActorLogging, Actor, Props, ActorRef }
import akka.io.Inet.SocketOption
import akka.io.Tcp.{ Event, Connect }
import akka.io.{ Inet, IO, Tcp }
import akka.util.ByteString
import rxmongo.driver.Channel.{ Ack, ConnectionFailed }

import scala.concurrent.duration.Duration

object Channel {
  def props(remote : InetSocketAddress, options : ConnectionOptions, replies : ActorRef, isPrimary : Boolean) =
    Props(classOf[Channel], remote, options, replies)

  case object Ack extends Event
  case class CloseWithAck(ack : Any)

  sealed trait ConnectionResponse
  case class ConnectionFailed(conn : Connect) extends ConnectionResponse
}

class Channel(remote : InetSocketAddress, options : ConnectionOptions, listener : ActorRef)
  extends Actor with ActorLogging {

  import Tcp._
  import context.system

  val manager = IO(Tcp)

  manager ! Connect(
    remoteAddress = remote,
    localAddress = Some(new InetSocketAddress(options.localIP.getOrElse(InetAddress.getLocalHost), options.localPort)),
    options = List[SocketOption](
      SO.KeepAlive(options.tcpKeepAlive),
      SO.OOBInline(options.tcpOOBInline),
      SO.TcpNoDelay(options.tcpNoDelay)
    ),
    timeout = options.connectTimeoutMS match { case 0 ⇒ None; case x : Long ⇒ Some(Duration(x, "ms")) },
    pullMode = true)

  def receive = {
    case CommandFailed(conn : Connect) ⇒
      listener ! ConnectionFailed(conn)
      context stop self

    case c @ Connected(remote_addr, local_addr) ⇒
      listener ! c
      val connection = sender()
      connection ! Register(self)
      connection ! ResumeReading
      context become connected(connection)
  }

  def connected(connection : ActorRef) : Receive = {

    case message : RequestMessage ⇒ // Send a message to Mongo via connection actor
      connection ! Write(message.finish, Channel.Ack)

    case Ack ⇒ // Receive Write Ack from connection actor
      connection ! ResumeReading // Tell connection actor we can read more now

    case Received(data : ByteString) ⇒ // Receive a reply from Mongo
      val reply = ReplyMessage(data) // Encapsulate that in a ReplyMessage
      listener ! reply // Pass the buck

    case CommandFailed(w : Write) ⇒ // A write has failed
      // O/S buffer was full
      listener ! "write failed"

    case Closed ⇒
      /** The connection has been closed normally in response to a [[Close]] command.
        */
      context stop self
    case Aborted ⇒
      /** The connection has been aborted in response to an [[Abort]] command.
        */
      context stop self
    case ConfirmedClosed ⇒
      /** The connection has been half-closed by us and then half-close by the peer
        * in response to a [[ConfirmedClose]] command.
        */
      context stop self
    case PeerClosed ⇒
      /** The peer has closed its writing half of the connection.
        */
      context stop self
    case ErrorClosed(cause : String) ⇒
      /** The connection has been closed due to an IO error.
        */
      context stop self
    case _ : ConnectionClosed ⇒
      listener ! "connection closed"
      context stop self
  }
}
