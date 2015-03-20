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

import akka.actor.{ ActorRef }
import akka.io.Inet.SocketOption
import akka.io.Tcp.SO
import akka.stream.stage.{ Directive, Context, PushStage }
import akka.stream.{ ActorFlowMaterializerSettings, ActorFlowMaterializer }
import akka.stream.scaladsl._
import akka.util.ByteString
import rxmongo.messages.RequestMessage

import scala.collection.mutable
import scala.concurrent.duration.Duration

/** Tcp Channel
  *
  * Description of thing
  */
case class StreamTcpChannel(
  remote : InetSocketAddress,
  options : ConnectionOptions,
  listener : ActorRef,
  isPrimary : Boolean) extends Channel(remote, options, listener, isPrimary) {

  implicit val system = context.system

  val streamTcp = StreamTcp(system)

  implicit val materializer = ActorFlowMaterializer(
    ActorFlowMaterializerSettings(system)
      .withInputBuffer(
        initialSize = 64,
        maxSize = 64)
  )

  val connection = streamTcp.outgoingConnection(
    remoteAddress = remote,
    localAddress = Some(new InetSocketAddress(options.localIP.orNull, options.localPort)),
    options = List[SocketOption](
      SO.KeepAlive(options.tcpKeepAlive),
      SO.OOBInline(options.tcpOOBInline),
      SO.TcpNoDelay(options.tcpNoDelay)
    ),
    connectTimeout = options.connectTimeoutMS match { case 0 ⇒ Duration.Inf; case x : Long ⇒ Duration(x, "ms") }
  )

  val stage = new PushStage[ByteString, ByteString] {
    def onPush(elem : ByteString, ctx : Context[ByteString]) : Directive = {
      doReply(elem)
      ctx.push(ByteString.empty)
    }
  }

  val handler = Flow[ByteString].map { msg ⇒ doReply(msg); msg }

  val materialized = connection.join[Unit](handler)

  val pendingRequestQueue = mutable.Queue.empty[RequestMessage]

  def handleRequest(requestMsg : RequestMessage, msg_to_send : ByteString) : Unit = {}
}

