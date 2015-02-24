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

import akka.actor.{ Terminated, ActorRef }
import akka.io.Inet.SocketOption
import akka.io.{ IO, Tcp }
import akka.util.ByteString

import scala.collection.mutable
import scala.concurrent.duration.Duration

object AkkaIOChannel {
  case object Ack extends Tcp.Event
}

/** An RxMongo Channel that is implemented with Akka IO
  *
  * This kind of Channel implements the Channel protocol with RxMongo by managing back pressure manually
  * with the Akka IO Tcp Manager.
  */

class AkkaIOChannel(remote : InetSocketAddress, options : ConnectionOptions, listener : ActorRef, isPrimary : Boolean)
  extends Channel(remote, options, listener, isPrimary) {

  import context.system

  import Tcp._
  import AkkaIOChannel._

  val manager = IO(Tcp)
  var connection : ActorRef = null
  var ackPending : Boolean = false
  var responsePending : Boolean = false

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

  manager ! connectionMsg

  log.debug("Connection request to TCP Manager sent")

  val pendingRequests = mutable.Queue.empty[RequestMessage]

  @inline def sendMessage(requestMsg : RequestMessage) : Unit = {
    val msg_to_send = requestMsg.finish
    val msg = Write(msg_to_send, Ack)
    connection ! msg
    ackPending = true
    responsePending = requestMsg.requiresResponse
    log.debug("Sent Request: {} ({} bytes, queuelen={})", requestMsg, msg_to_send.length, pendingRequests.length)
  }

  @inline def handleRequest(requestMsg : RequestMessage) : Unit = {
    if (ackPending || responsePending) {
      pendingRequests += requestMsg
      log.debug("Qued Request: {} (queuelen={})", requestMsg, pendingRequests.length)
    } else {
      sendMessage(requestMsg)
    }
  }

  @inline override def handleReply(replyMsg : ReplyMessage, toActor : ActorRef) : Unit = {
    toActor ! replyMsg
    connection ! ResumeReading
    responsePending = false
    if (!ackPending && pendingRequests.nonEmpty) {
      sendMessage(pendingRequests.dequeue())
    }
  }

  @inline override def handleClose() : Unit = {
    connection ! Close
    super.handleClose()
  }

  override def unconnected : Receive = {
    case Ack ⇒ // Receive Write Ack from connection actor
      log.warning("In unconnected, got unexpected Ack ")

    case CommandFailed(conn : Connect) ⇒
      val msg = Channel.ConnectionFailed(s"CommandFailed for connection: $conn")
      log.debug(msg.toString)
      listener ! msg
      context stop self

    case c @ Connected(remote_addr, local_addr) ⇒
      log.debug("Connected to {} at {}", remote_addr, local_addr)
      connection = sender()
      connection ! Register(self)
      connection ! ResumeReading
      context watch connection
      context become connected
      listener ! Channel.ConnectionSucceeded(remote_addr)

    case Terminated(actor) ⇒ // The TCP
      if (actor == manager) {
        val msg = Channel.ConnectionFailed(s"TCP Manager terminated while connecting: $actor")
        log.error(msg.toString)
        listener ! msg
        context stop self
      } else {
        log.warning(s"Spurious termination of: $actor")
        spuriousMessages += 1
      }

    case Received(data : ByteString) ⇒ // Receive a reply from Mongo
      doReply(data)

    case x ⇒
      super.unconnected(x)
  }

  override def connected : Receive = {
    case Ack ⇒ // Receive Write Ack from connection actor
      ackPending = false
      log.debug("Ack with queuelen={}", pendingRequests.length)
      if (!responsePending && pendingRequests.nonEmpty) {
        sendMessage(pendingRequests.dequeue())
      }

    case Received(data : ByteString) ⇒ // Receive a reply from Mongo
      doReply(data)

    case Terminated(actor) ⇒ // The TCP connection has terminated
      if (actor == connection)
        log.debug("TCP Connection terminated unexpectedly: {}", actor)
      else
        log.debug("Spurious termination: {}", actor)

    case CommandFailed(w : Write) ⇒ // A write has failed
      writeFailures += 1
      val msg = Channel.WriteFailed(s"Command Failed: $w")
      log.warning(msg.toString)
      // O/S buffer was full
      listener ! msg

    case x ⇒
      super.connected(x)
  }

  override def closing : Receive = {
    case Received(data : ByteString) ⇒
      doReply(data)

    case Tcp.Closed ⇒
      log.debug("Closed")
      /** The connection has been closed normally in response to a [[Close]] command. */
      context stop self

    case Tcp.Aborted ⇒
      log.debug("Aborted")
      /** The connection has been aborted in response to an [[Abort]] command. */
      context stop self

    case Tcp.ConfirmedClosed ⇒
      log.debug("ConfirmedClosed")
      /** The connection has been half-closed by us and then half-close by the peer
        * in response to a [[ConfirmedClose]] command.
        */
      context stop self

    case Tcp.PeerClosed ⇒
      log.debug("PeerClosed")
      /** The peer has closed its writing half of the connection. */
      context stop self

    case Tcp.ErrorClosed(cause : String) ⇒
      log.debug(s"ErrorClosed: $cause")
      /** The connection has been closed due to an IO error. */
      context stop self

    case _ : Tcp.ConnectionClosed ⇒
      log.debug("Other ConnectionClosed")
      context stop self

    case x ⇒
      super.closing(x)
  }

}
