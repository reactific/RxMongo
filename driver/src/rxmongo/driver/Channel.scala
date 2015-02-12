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

import akka.actor._
import akka.event.LoggingReceive
import akka.io.Inet.SocketOption
import akka.io.{ IO, Tcp }
import akka.util.ByteString
import rxmongo.driver.Channel.{ ConnectionSucceeded, Ack, ConnectionFailed }

import scala.collection.mutable
import scala.concurrent.duration.Duration

object Channel {
  def props(remote : InetSocketAddress, options : ConnectionOptions, replies : ActorRef, isPrimary : Boolean) =
    Props(classOf[Channel], remote, options, replies)

  sealed trait ChannelRequest
  case object Ack extends Tcp.Event
  case class CloseWithAck(ack : Any)
  case class SendMessage(msg : RequestMessage, replyTo : ActorRef) extends ChannelRequest
  case object Close extends ChannelRequest
  case object GetStatistics extends ChannelRequest

  sealed trait ChannelResponse
  case class ConnectionFailed(conn : Tcp.Connect) extends ChannelResponse
  case class ConnectionSucceeded(conn : Tcp.Connect) extends ChannelResponse
  case class UnsolicitedReply(reply : ReplyMessage) extends ChannelResponse
  case class Statistics(
    numMessages : Long,
    numReplies : Long,
    numFailures : Long) extends ChannelResponse
}

class Channel(remote : InetSocketAddress, options : ConnectionOptions, listener : ActorRef)
  extends Actor with ActorLogging {

  import Tcp._
  import context.system

  val manager = IO(Tcp)

  val pendingResponses = mutable.HashMap.empty[Int, ActorRef]

  val connectionMsg = Tcp.Connect(
    remoteAddress = remote,
    localAddress = Some(
      new InetSocketAddress(options.localIP.orNull, options.localPort)
    ),
    options = List[SocketOption](
      SO.KeepAlive(options.tcpKeepAlive),
      SO.OOBInline(options.tcpOOBInline),
      SO.TcpNoDelay(options.tcpNoDelay)
    ),
    timeout = options.connectTimeoutMS match { case 0 ⇒ None; case x : Long ⇒ Some(Duration(x, "ms")) },
    pullMode = true
  )

  var msgCounter : Long = 0L
  var replyCounter : Long = 0L
  var writeFailures : Long = 0L

  manager ! connectionMsg

  log.debug("Connection request to TCP Manager sent")

  override def preStart() = {
    manager ! ResumeReading
  }

  def doReply(msg : ByteString) = {
    replyCounter += 1
    val reply = ReplyMessage(msg) // Encapsulate that in a ReplyMessage
    pendingResponses.get(reply.responseTo) match {
      case Some(actor) ⇒ actor ! reply
      case None ⇒
        log.info(s"Received reply ($reply) but matching request was not found")
        listener ! Channel.UnsolicitedReply(reply)
    }
  }

  log.debug(s"New Channel to $remote established")

  def receive = LoggingReceive {
    case Channel.Close ⇒
      log.debug(s"Channel.Close requested while waiting for connection (terminating)")
      context stop self

    case CommandFailed(conn : Connect) ⇒
      log.debug(s"CommandFailed for connection: $conn")
      listener ! ConnectionFailed(conn)
      context stop self

    case c @ Connected(remote_addr, local_addr) ⇒
      log.debug(s"Connected to $remote_addr at $local_addr")
      val connection = sender()
      connection ! Register(self)
      context become connected(connection)
      listener ! ConnectionSucceeded(connectionMsg)

    case Terminated(actor) ⇒ // The TCP
      if (actor == manager) {
        log.info(s"TCP Manager terminated while connecting: $actor")
        listener ! ConnectionFailed(connectionMsg)
        context stop self
      } else {
        log.info(s"Spurious termination: $actor")
      }

    case Channel.GetStatistics ⇒
      sender() ! Channel.Statistics(msgCounter, replyCounter, writeFailures)

    case x : Any ⇒
      log.debug(s"Got other message: $x")
  }

  def connected(connection : ActorRef) : Receive = {
    case Channel.Close ⇒
      log.debug("Channel closing")
      connection ! Close
      context become closing

    case Channel.GetStatistics ⇒
      sender() ! Channel.Statistics(msgCounter, replyCounter, writeFailures)

    case Channel.SendMessage(message : RequestMessage, replyTo : ActorRef) ⇒
      msgCounter += 1
      // Send a message to Mongo via connection actor
      val msg_to_send = message.finish
      log.debug("Sending message to Mongo: {} ({} bytes)", message, msg_to_send.length)
      val msg = Write(msg_to_send, Channel.Ack)
      connection ! msg
      if (message.requiresResponse)
        pendingResponses.put(message.requestId, replyTo)

    case Ack ⇒ // Receive Write Ack from connection actor
      log.warning("Got Ack in connected state")
      connection ! ResumeReading // Tell connection actor we can read more now

    case Received(data : ByteString) ⇒ // Receive a reply from Mongo
      doReply(data)

    case Terminated(actor) ⇒ // The TCP connection has terminated
      if (actor == connection)
        log.debug("TCP Connection terminated unexpectedly: {}", actor)
      else
        log.debug(s"Spurious termination: $actor")

    case CommandFailed(w : Write) ⇒ // A write has failed
      writeFailures += 1
      log.debug("CommandFailed: {}", w)
      // O/S buffer was full
      listener ! "write failed"

    case x : Any ⇒
      log.debug(s"In connected, got other message: $x")
  }

  def closing : Receive = LoggingReceive {
    case Channel.Close ⇒
      log.debug("Channel.Close ignored as channel is already closing")

    case Channel.SendMessage ⇒
      log.debug("Channel.SendMessage ignored as channel is closing")

    case Received(data : ByteString) ⇒
      doReply(data)

    case Tcp.Closed ⇒
      log.debug("Closed")
      /** The connection has been closed normally in response to a [[Close]] command.
        */
      context stop self
    case Tcp.Aborted ⇒
      log.debug("Aborted")
      /** The connection has been aborted in response to an [[Abort]] command.
        */
      context stop self
    case Tcp.ConfirmedClosed ⇒
      log.debug("ConfirmedClosed")
      /** The connection has been half-closed by us and then half-close by the peer
        * in response to a [[ConfirmedClose]] command.
        */
      context stop self
    case Tcp.PeerClosed ⇒
      log.debug("PeerClosed")
      /** The peer has closed its writing half of the connection.
        */
      context stop self
    case Tcp.ErrorClosed(cause : String) ⇒
      log.debug(s"ErrorClosed: $cause")
      /** The connection has been closed due to an IO error.
        */
      context stop self
    case _ : Tcp.ConnectionClosed ⇒
      log.debug("Other ConnectionClosed")
      context stop self
    case x : Any ⇒
      log.debug(s"In closing, got other message: $x")
  }
}
