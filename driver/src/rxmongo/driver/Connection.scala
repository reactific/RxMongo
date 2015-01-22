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

import akka.actor.{ Actor, ActorLogging, Props, ActorRef }

import scala.collection.mutable

object Connection {
  def props(uri : MongoURI) = Props(classOf[Connection], uri)

  sealed trait ConnectionCommand
  case object CloseConnection extends ConnectionCommand
  case class ChannelClosed(chan : ActorRef) extends ConnectionCommand
  case object OpenChannel extends ConnectionCommand
}

/** Connection To MongoDB Replica Set
  *
  * This represents RxMongo's connection to a replica set.
  */
class Connection(uri : MongoURI) extends Actor with ActorLogging {

  import rxmongo.driver.Connection._

  val channels : mutable.HashSet[ActorRef] = mutable.HashSet.empty[ActorRef]

  override def preStart() = {
    super.preStart()
    identifyReplicaSet
    for (i ← 1 to uri.options.minPoolSize) { addChannel }
  }

  def receive = {
    case CloseConnection ⇒
      for (chan ← channels) { chan ! Channel.CloseWithAck(ChannelClosed(chan)) }
      context become closing
  }

  def closing : Receive = {
    case OpenChannel ⇒
      log.warning("Ignoring OpenChannel command while closing connection")
    case ChannelClosed(channel : ActorRef) ⇒
      channels.remove(channel)
      if (channels.isEmpty)
        context.stop(self)
  }

  def identifyReplicaSet = {

  }

  def addChannel = {

  }
}
