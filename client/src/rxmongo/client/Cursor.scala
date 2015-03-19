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

import akka.pattern.ask
import akka.util.Timeout

import rxmongo.bson.BSONDocument
import rxmongo.messages.{ReplyMessage, KillCursorsMessage, GetMoreMessage}

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration._

case class ReplyIterator(
  collection : Collection,
  reply : ReplyMessage,
  var batchSize : Int = 10,
  implicit val timeOut : Timeout = 5.seconds) extends Iterator[Seq[BSONDocument]] with Closeable {

  private var pending : Future[(Long, Seq[BSONDocument])] = Future.successful(reply.cursorID -> reply.documents)

  def hasNext : Boolean = {
    pending != null && {
      val (cursorID, docs) = Await.result(pending, timeOut.duration)
      cursorID != 0 || docs.nonEmpty
    }
  }

  def next() : Seq[BSONDocument] = {
    val (cursorID, docs) = Await.result(pending, timeOut.duration)
    pending = retrieveMore(cursorID)
    docs
  }

  def retrieveMore(cursorID : Long) : Future[(Long, Seq[BSONDocument])] = {
    val msg = GetMoreMessage(collection.fullName, batchSize, cursorID)
    implicit val ec = collection.executionContext
    collection.connection.ask(msg)(timeOut) map { any : Any ⇒
      any match {
        case msg : ReplyMessage ⇒ msg.cursorID -> msg.documents
        case _ ⇒ 0L -> Seq.empty[BSONDocument]
      }
    }
  }

  def close() = {
    val (cursorID, docs) = Await.result(pending, timeOut.duration)
    if (cursorID > 0) {
      val msg = KillCursorsMessage(Seq(cursorID))
      collection.connection ! msg
    }
    pending = null
  }
}

case class Cursor private[client] (
  collection : Collection,
  private val reply : ReplyMessage,
  batchSize : Int = 10,
  implicit val timeOut : Timeout = 5.seconds) extends RxMongoComponent(collection.driver) with Iterator[Future[BSONDocument]] with Closeable {

  private val itr = ReplyIterator(collection, reply, batchSize, timeOut)

  private var list : Seq[BSONDocument] = if (itr.hasNext) itr.next() else Seq.empty[BSONDocument]

  override def hasNext : Boolean = list.nonEmpty || itr.hasNext

  override def next() : Future[BSONDocument] = {
    if (list.isEmpty) {
      if (itr.isEmpty)
        throw new NoSuchElementException("No more documents in RxMongo Cursor")
      else
        list = itr.next()
    }
    val result = list.head
    list = list.tail
    Future.successful(result)
  }

  /** MongoDB API to test if iterator has more */
  def hasMore : Boolean = hasNext

  /** MongoDB API to get the next document */
  def getNextDocument : Future[BSONDocument] = next()

  def close() = {

  }

  override def toSeq : Seq[Future[BSONDocument]] = { for (f ← this) yield { f } }.toSeq

  def toFlatSeq : Future[Seq[BSONDocument]] = Future.sequence(toSeq)

  /** Returns the number of documents left in the current cursor batch.
    *
    * @return
    */
  def objsLeftInBatch() = {
    list.length
  }

  def toArray : Array[BSONDocument] = {
    Await.result(toFlatSeq, 5.seconds).toArray
  }

  /**
    * Name	Description
    * cursor.addOption()	Adds special wire protocol flags that modify the behavior of the query.’
    * cursor.batchSize()	Controls the number of documents MongoDB will return to the client in a single network message.
    * cursor.count()	Returns the total number of documents in a cursor.
    * cursor.explain()	Reports on the query execution plan, including index use, for a cursor.
    * cursor.forEach()	Applies a JavaScript function for every document in a cursor.
    * cursor.hint()	Forces MongoDB to use a specific index for a query.
    * cursor.itcount()	Returns the number of documents remaining in a cursor.
    * cursor.limit()	Constrains the size of a cursor’s result set.
    * cursor.map()	Applies a function to each document in a cursor and collects the return values in an array.
    * cursor.maxTimeMS()	Specifies a cumulative time limit in milliseconds for processing operations on a cursor.
    * cursor.max()	Specifies an exclusive upper index bound for a cursor. For use with cursor.hint()
    * cursor.min()	Specifies an inclusive lower index bound for a cursor. For use with cursor.hint()
    * cursor.pretty()	Configures the cursor to display results in an easy-to-read format.
    * cursor.readPref()	Specifies a read preference to a cursor to control how the client directs queries to a replica set.
    * cursor.showDiskLoc()	Returns a cursor with modified documents that include the on-disk location of the document.
    * cursor.size()	Returns a count of the documents in the cursor after applying skip() and limit() methods.
    * cursor.skip()	Returns a cursor that begins returning results only after passing or skipping a number of documents.
    * cursor.snapshot()	Forces the cursor to use the index on the _id field. Ensures that the cursor returns each document, with regards to the value of the _id field, only once.
    * cursor.sort()	Returns results ordered according to a sort specification.
    */
}
