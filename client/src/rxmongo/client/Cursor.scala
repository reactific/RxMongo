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

package rxmongo.client

import java.io.Closeable

import akka.actor.ActorRef

import rxmongo.bson.BSONObject
import rxmongo.driver.{ ReplyMessage, QueryMessage }

import scala.concurrent.Future

case class Cursor private[client] (
  collection : Collection,
  connection : ActorRef,
  private val query : QueryMessage,
  private var reply : ReplyMessage)
  extends RxMongoComponent(collection.driver) with Iterator[Future[BSONObject]] with Closeable {

  private var list = reply.documents.toList

  override def hasNext : Boolean = list.nonEmpty

  def hasMore : Boolean = hasNext

  override def next() : Future[BSONObject] = {
    if (list.isEmpty)
      throw new NoSuchElementException("No more documents in RxMongo Cursor")
    val result = list.head
    list = list.tail
    if (list.isEmpty) {
      // FIXME: Fetch More
      Future.successful(result)
    } else {
      Future.successful(result)
    }
  }

  def getNextDocument : Future[BSONObject] = next()

  def close() = {}

  override def toSeq : Seq[Future[BSONObject]] = { for (f ← this) yield { f } }.toSeq

  def toFlatSeq : Future[Seq[BSONObject]] = Future.sequence(toSeq)
}
