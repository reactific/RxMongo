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
import akka.util.ByteString

import scala.collection.mutable

object Channel {
  def props(remote : InetSocketAddress, options : ConnectionOptions, replies : ActorRef,
    isPrimary : Boolean, useStreams : Boolean = false) = {
    if (useStreams) {
      Props(classOf[TcpStreamChannel], remote, options, replies, isPrimary)
    } else {
      Props(classOf[AkkaIOChannel], remote, options, replies, isPrimary)
    }
  }

  sealed trait ChannelRequest
  case class CloseWithAck(ack : Any) extends ChannelRequest
  case class SendMessage(msg : RequestMessage, replyTo : ActorRef) extends ChannelRequest
  case object Close extends ChannelRequest
  case object GetStatistics extends ChannelRequest

  sealed trait ChannelResponse
  case class ConnectionFailed(msg : String) extends ChannelResponse
  case class ConnectionSucceeded(remote : InetSocketAddress) extends ChannelResponse
  case class UnsolicitedReply(reply : ReplyMessage) extends ChannelResponse
  case class WriteFailed(msg : String) extends ChannelResponse
  case class Statistics(
    requests : Long,
    requestBytes : Long,
    replies : Long,
    replyBytes : Long,
    writeFailures : Long,
    unsolicitedReplies : Long,
    spuriousMessages : Long) extends ChannelResponse
}

abstract class Channel(remote : InetSocketAddress, options : ConnectionOptions, listener : ActorRef, isPrimary : Boolean)
  extends Actor with ActorLogging {

  log.debug(s"Establishing ${if (isPrimary) "primary" else "secondary"} Channel to $remote ")

  val pendingResponses = mutable.HashMap.empty[Int, ActorRef]
  var requestCounter : Long = 0L
  var requestBytes : Long = 0L
  var replyCounter : Long = 0L
  var replyBytes : Long = 0L
  var writeFailures : Long = 0L
  var unsolicitedReplies : Long = 0L
  var spuriousMessages : Long = 0L

  def makeStats : Channel.Statistics = {
    Channel.Statistics(requestCounter, requestBytes, replyCounter, replyBytes,
      writeFailures, unsolicitedReplies, spuriousMessages)
  }

  def handleRequest(requestMsg : RequestMessage) : Unit

  def handleReply(replyMsg : ReplyMessage, toActor : ActorRef) : Unit = {
    toActor ! replyMsg
  }

  def handleClose() : Unit = {
    context become closing
  }

  final def doRequest(msg : Channel.SendMessage) = {
    val message = msg.msg
    val replyTo = msg.replyTo
    requestCounter += 1
    // Send a message to Mongo via connection actor
    val msg_to_send = message.finish
    requestBytes += msg_to_send.length
    if (message.requiresResponse)
      pendingResponses.put(message.requestId, replyTo)
    handleRequest(message)
  }

  final def doReply(msg : ByteString) = {
    replyCounter += 1
    replyBytes += msg.length
    val reply = ReplyMessage(msg) // Encapsulate that in a ReplyMessage
    pendingResponses.get(reply.responseTo) match {
      case Some(actor) ⇒
        pendingResponses.remove(reply.responseTo)
        log.debug("Handling Reply: {} ({} bytes)", reply, msg.length)
        handleReply(reply, actor)
      case None ⇒
        log.warning(s"Received reply ({}) but matching request was not found", reply)
        listener ! Channel.UnsolicitedReply(reply)
    }
  }

  override def preStart() = {
    context.become(unconnected)
  }

  def unconnected = LoggingReceive {
    case Channel.Close ⇒
      log.info(s"Channel.Close requested while unconnected so terminating")
      context stop self

    case Channel.GetStatistics ⇒
      sender() ! makeStats

    case x : Any ⇒
      log.debug(s"In unconnected, got other message: {}", x)
      spuriousMessages += 1
  }

  def connected : Receive = LoggingReceive {
    case Channel.Close ⇒
      log.info("Channel is closing")
      handleClose

    case Channel.GetStatistics ⇒
      sender() ! makeStats

    case msg : Channel.SendMessage ⇒
      doRequest(msg)

    case x : Any ⇒
      log.debug(s"In connected, got other message: {}", x)
      spuriousMessages += 1
  }

  def closing : Receive = LoggingReceive {
    case Channel.Close ⇒
      log.info("Channel.Close ignored as channel is already closing")

    case Channel.SendMessage ⇒
      log.info("Channel.SendMessage ignored as channel is closing")

    case x : Any ⇒
      log.debug(s"In closing, got other message: {}", x)
      spuriousMessages += 1
  }

  final def receive : Receive = {
    case x : Any ⇒
      throw new IllegalStateException("Channels should not be in receive state")
  }
}
