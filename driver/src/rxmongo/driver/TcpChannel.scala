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

import akka.actor.{ ActorSystem, ActorRef }
import akka.io.Inet.SocketOption
import akka.io.Tcp.SO
import akka.stream.{ ActorFlowMaterializerSettings, ActorFlowMaterializer }
import akka.stream.scaladsl._
import akka.stream.scaladsl.StreamTcp.OutgoingConnection
import akka.util.ByteString

import scala.collection.mutable
import scala.concurrent.duration.Duration

/** Tcp Channel
  *
  * Description of thing
  */
case class TcpChannel(remote : InetSocketAddress, options : ConnectionOptions)(implicit system : ActorSystem) {

  val streamTcp = StreamTcp(system)

  implicit val materializer = ActorFlowMaterializer(
    ActorFlowMaterializerSettings(system)
      .withInputBuffer(
        initialSize = 64,
        maxSize = 64)
  )

  type RequestSink = Sink[RequestMessage]
  type RequestSource = Source[RequestMessage]

  val connection : OutgoingConnection = streamTcp.outgoingConnection(
    remoteAddress = remote,
    localAddress = Some(new InetSocketAddress(options.localIP.orNull, options.localPort)),
    options = List[SocketOption](
      SO.KeepAlive(options.tcpKeepAlive),
      SO.OOBInline(options.tcpOOBInline),
      SO.TcpNoDelay(options.tcpNoDelay)
    ),
    connectTimeout = options.connectTimeoutMS match { case 0 ⇒ Duration.Inf; case x : Long ⇒ Duration(x, "ms") }
  )

  val toByteStringFlow = Flow[RequestMessage].map { msg ⇒ msg.finish }

  val pendingRequestQueue = mutable.Queue.empty[RequestMessage]

  val source : Source[RequestMessage] = Source { () ⇒ pendingRequestQueue.iterator }

  val materialized = connection.flow.map { msg ⇒ }

  def send(msg : RequestMessage, replyTo : ActorRef) = {

  }

}
