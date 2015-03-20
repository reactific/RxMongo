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

import java.util.concurrent.atomic.{ AtomicLong, AtomicReference }

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout

import rxmongo.bson._
import rxmongo.messages.cmds._
import rxmongo.messages._
import rxmongo.messages.replies.CollStatsReply

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration._
import scala.util.{ Failure, Success, Try }

/** Represents a MongoDB Collection
  *
  * @see [[http://docs.mongodb.org/master/reference/method/js-collection/]]
  * @param name THe name of the collection in the database
  * @param db A database object
  */
class Collection(val name : String, val db : Database)(
  override implicit val timeout : Timeout = db.timeout,
  override implicit val writeConcern : WriteConcern = db.writeConcern,
  implicit val retryStrategy : RetryStrategy = RetryStrategy.default,
  implicit val statsRefresh : FiniteDuration = 1.day,
  val statsScale : Int = 1024) extends RxMongoComponent(db.driver) {

  val fullName = db.namespace + "." + name

  private[rxmongo] def connection : ActorRef = db.client.connection

  require(!name.contains("$"), "Collection names must not contain a $")
  require(name.length > 0, "Collection names may not be empty")
  require(!name.startsWith("system."), "Collection names may not start with 'system.'")

  private val _stats = new AtomicReference[CollStatsReply](null)
  private val _stats_refreshed_at = new AtomicLong(0)
  private def getRefreshedStats() : CollStatsReply = Try {
    val timeNow = System.currentTimeMillis()
    if (_stats_refreshed_at.get() + statsRefresh.toMillis < timeNow) {
      this.synchronized {
        val result = (db.client.connection ? CollStatsCmd(db.namespace, name, statsScale)).mapTo[ReplyMessage] map { reply ⇒
          if (reply.numberReturned >= 1) {
            val stats = reply.documents.head.to[CollStatsReply]
            _stats.set(stats)
            _stats_refreshed_at.set(timeNow)
            stats
          } else {
            throw new IllegalStateException("Mongo didn't return a document for ColLStatsCmd")
          }
        }
        Await.result(result, timeout.duration)
      }
    } else {
      _stats.get
    }
  } match {
    case Success(stats) ⇒ stats
    case Failure(xcptn) ⇒
      log.error(xcptn, "Failed to acquire collection statistics")
      throw xcptn
  }

  /** Return the average size of an object (document) in the collection. This value is refreshed only as frequently
    * as statsRefresh permits.
    */
  def avgObjSize = getRefreshedStats().avgObjSize

  def capped : Boolean = getRefreshedStats().capped

  /** Count Of Documents.
    * @return a count of the number of documents in this collection.
    */
  def count = getRefreshedStats().count

  /** Storage Size of Collection.
    * @return The size of the data storage for the collection, in bytes.
    */
  def storageSize : Long = getRefreshedStats().storageSize * statsScale

  /** Index Size Of Collection.
    * @return
    */
  def indexSize : Long = getRefreshedStats().totalIndexSize * statsScale

  /** Last Extent Size.
    * @return The size of the last extent added to the collection.
    */
  def lastExtentSize = getRefreshedStats().lastExtentSize

  /** Number of indexes.
    * @return The number of indexes associated with the collection
    */
  def numIndexes = getRefreshedStats().nindexes

  /** Number of database extents.
    *
    * @return The number of space allocation extents allocated to the collection
    */
  def numExtents = getRefreshedStats().numExtents

  /** Collection Stats
    * @return Returns the CollStatsReply object that contains all collection statistics
    */
  def stats : CollStatsReply = getRefreshedStats()

  /** Provides access to the aggregation pipeline.
    *
    */
  def aggregate() = ???

  /** Count matching documents
    *
    * @param selector A query that selects which documents to count in a collection.
    * @param limit The maximum number of matching documents to return.
    * @param skip The number of matching documents to skip before returning results.
    * @param hint The index to use. Specify either the index name as a string or the index specification document.
    * @return
    */
  def count(selector : Query, limit : Option[Int], skip : Option[Int], hint : Option[String]) : Future[Int] = {
    val cmd = CountCmd(db.name, name, selector, limit, skip, hint)
    db.client.connection.ask(cmd) map processReplyMessage(cmd) map { obj ⇒ obj.asInt("n") }
  }

  /** Wraps eval to copy data between collections in a single MongoDB instance. */
  def copyTo() = ???

  /** Builds an index on a collection. */
  def createIndex(keys : Index, options : IndexOptions) : Future[BSONDocument] = {
    options.name.map { name ⇒ require(name.length + fullName.length + 1 < 128, "Full index name must be < 128 characters") }
    val cmd = CreateIndicesCmd(db.name, name, Seq(keys -> options))
    db.client.connection.ask(cmd) map processReplyMessage(cmd)
  }

  def createIndices(indices : (Index, IndexOptions)*) : Future[BSONDocument] = {
    for ((index, options) ← indices) {
      options.name.map { name ⇒ require(name.length + fullName.length + 1 < 128, "Full index name must be < 128 characters") }
    }
    val cmd = CreateIndicesCmd(db.name, name, indices)
    db.client.connection.ask(cmd) map processReplyMessage(cmd)
  }

  def deleteOne(delete : Delete, ordered : Boolean = true)(implicit to : Timeout = timeout, wc : WriteConcern = writeConcern) : Future[BSONDocument] = {
    remove(Seq(delete), ordered)(to, wc)
  }

  def delete(deletes : Seq[Delete], ordered : Boolean = true)(implicit to : Timeout = timeout, wc : WriteConcern = writeConcern) : Future[BSONDocument] = {
    remove(deletes, ordered)(to, wc)
  }

  /** Returns an array of documents that have distinct values for the specified field. */
  def distinct() = ???

  /** Removes the specified collection from the database. */
  def drop() : Future[Boolean] = {
    val drop = DropCollectionCmd(db.name, name)
    db.client.connection.ask(drop) map processDoubleOkCommandResult(drop)
  }

  /** Removes a specified index on a collection. */
  def dropIndex() = ???
  /** Removes all indexes on a collection. */
  def dropIndexes() = ???
  /** Deprecated. Use db.collection.createIndex(). */
  def ensureIndex() = ???

  def exists() : Future[Boolean] = ???

  /** Returns information on the query execution of various methods. */
  def explain() = ???

  def findRaw(selector : Query,
    projection : Option[Projection] = None,
    options : QueryOptions = QueryOptions()) : Future[ReplyMessage] = {
    val msg = QueryMessage(fullName, selector.result, projection.map { p ⇒ p.result }, options)
    db.client.connection.ask(msg) map {
      case reply : ReplyMessage ⇒
        reply.error match {
          case Some(msg) ⇒ throw new RxMongoError(s"Error while processing query {$selector}: $msg")
          case None ⇒ reply
        }
    }
  }

  /** Performs a query on a collection and returns a cursor object.
    *
    * @param selector The Query that selects the documents to return.
    * @param projection Optional. The Projection that selects the fields in the documents to return.
    * @param options Options that control how the query is done and the kind of cursor returned.
    * @return A Cursor to allow iteration over the result set.
    */
  def find(
    selector : Query,
    projection : Option[Projection] = None,
    options : QueryOptions = QueryOptions(),
    batchSize : Int = 10) : Future[Cursor] = {
    val msg = QueryMessage(fullName, selector.result, projection.map { p ⇒ p.result }, options)
    db.client.connection.ask(msg) map {
      case reply : ReplyMessage ⇒
        reply.error match {
          case Some(msg) ⇒ throw new RxMongoError(s"Error while processing query {$selector}: $msg")
          case None ⇒ Cursor(this, reply, batchSize)
        }
    }
  }

  /** Performs a query to return all documents in a collection
    *
    * @param projection Optional. The Projectiont hat selects the fields in the documents to return.
    * @return
    */
  def findAll(
    projection : Option[Projection] = None,
    options : QueryOptions = QueryOptions(),
    batchSize : Int = 10) : Future[Cursor] = {
    find(Query(), projection, options)
  }

  /** Performs a query and returns a single document.
    * This is simply a convenience function for calling find with the options.numberToReturn set to 1
    * @param selector The Query that selects the documents to return.
    * @param projection Optional. The Projection that selects the fields in the documents to return.
    * @param options Options that control how the query is done and the kind of cursor returned.
    * @return A Cursor to allow iteration over the result set.
    */
  def findOne(
    selector : Query,
    projection : Option[Projection] = None,
    options : QueryOptions = QueryOptions()) : Future[Option[BSONDocument]] = {
    findRaw(selector, projection, options.withNumberToReturn(-1)) map { reply : ReplyMessage ⇒
      if (reply.numberReturned <= 0)
        None
      else
        Some(reply.documents.head)
    }
  }

  /** Atomically modifies and returns a single document.
    * @param selector The query selector to find the document to be modified
    * @param update The update specification for modifying the found document
    * @param remove A boolean value to determine whether the found document should be deleted. If true, the update
    *           parameter is ignored.
    * @param projection The field projection to apply after the modification
    * @param fetchNewObject Return the new object instead of the old one
    * @param upsert A boolean value to determine whether the update should be used to construct a new document if
    *           the selector doesn't find a match.
    */
  def findAndModify(
    selector : Option[Query] = None,
    update : Update,
    remove : Boolean = false,
    projection : Option[Projection] = None,
    fetchNewObject : Boolean = true,
    upsert : Boolean = false) = {
    // TODO: Finish implementing FindAndModify
    val msg = FindAndModifyCmd(db.name, name, selector)
  }

  /** Returns an array of documents that describe the existing indexes on a collection. */
  def getIndexes() = ???

  /** For collections in sharded clusters, db.collection.getShardDistribution() reports data of chunk distribution. */
  def getShardDistribution() = ???

  /** Internal diagnostic method for shard cluster. */
  def getShardVersion() = ???

  /** Provides simple data aggregation function.
    * Groups documents in a collection by a key, and processes the results.
    * Use aggregate() for more complex data aggregation.
    */
  def group() = ???
  /** Creates a new document in a collection. */
  def insertRaw(objs : Seq[BSONObject], ordered : Boolean = true) : Unit = {
    val msg = InsertMessage(fullName, objs, ordered)
    db.client.connection ! msg
  }

  def insertOne(obj : BSONObject)(implicit to : Timeout = timeout, wc : WriteConcern = writeConcern) : Future[BSONDocument] = {
    insert(Seq(obj), ordered = true)(to, wc)
  }

  def insert(objs : Seq[BSONObject], ordered : Boolean = true)(implicit to : Timeout = timeout, wc : WriteConcern = writeConcern) : Future[BSONDocument] = {
    val insert = InsertCmd(db.name, name, objs, ordered, wc)
    db.client.connection.ask(insert)(to) map processReplyMessage(insert)
  }

  /** Reports if a collection is a capped collection. */
  def isCapped : Boolean = getRefreshedStats().capped

  /** Performs map-reduce style data aggregation. */
  def mapReduce() = ???

  /** Rebuilds all existing indexes on a collection.
    * @see http://docs.mongodb.org/manual/reference/command/reIndex/#dbcmd.reIndex
    * @return
    */
  def reIndex() = ???
  /** Deletes documents from a collection. */
  def removeRaw(selector : Query) : Unit = {
    val delete = DeleteMessage(fullName, selector.result)
    db.client.connection ! delete
  }

  def remove(deletes : Seq[Delete], ordered : Boolean = true)(implicit to : Timeout = timeout, wc : WriteConcern = writeConcern) : Future[BSONDocument] = {
    val delete = DeleteCmd(db.name, name, deletes, ordered, wc)
    db.client.connection.ask(delete)(to) map processReplyMessage(delete)
  }

  /** Changes the name of a collection. */
  def renameCollection(newName : String, dropTarget : Boolean) : Future[Option[Collection]] = {
    val rename = RenameCollectionCmd(db.name, name, newName, dropTarget)
    (db.client.connection.ask(rename) map processDoubleOkCommandResult(rename)) map { v ⇒
      if (v)
        Some(Collection(newName, db)(timeout, writeConcern, retryStrategy, statsRefresh))
      else
        None
    }
  }

  /** Provides a wrapper around an insert() and update() to insert new documents. */
  def save() = ???

  /** Modifies a document in a collection. */
  def update(updates : Seq[Update], ordered : Boolean = true)(implicit to : Timeout = timeout, wc : WriteConcern = writeConcern) : Future[WriteResult] = {
    val update = UpdateCmd(db.name, name, updates, ordered, wc)
    db.client.connection.ask(update)(to) map processWriteCommandResult(update)
  }

  /** Update One
    * Apply a single Update selector and updater to the collection.
    * @param u The Update specification
    * @param ordered If true, then when an update statement fails, return without performing the remaining update
    *         statements. If false, then when an update fails, continue with the remaining update statements,
    *         if any. Defaults to true.
    * @param to The timeout for the update operation
    * @param wc The write concern for the update operation
    * @return A future WriteResult that returns the result of the update operation
    */
  def updateOne(u : Update, ordered : Boolean = true)(implicit to : Timeout = timeout, wc : WriteConcern = writeConcern) : Future[WriteResult] = {
    update(Seq(u), ordered)(to, wc)
  }
  /** Performs diagnostic operations on a collection. */
  def validate() = ???

  private def processReplyMessage[T](cmd : Command)(any : Any) : BSONDocument = {
    any match {
      case reply : ReplyMessage ⇒
        require(cmd.requestId == reply.responseTo,
          s"Response to #${cmd.requestId} is actually for #${reply.responseTo}")
        reply.error match {
          case Some(msg) ⇒
            throw new RxMongoError(s"Error while processing $cmd: $msg")
          case None ⇒
            require(reply.numberReturned > 0, s"No write result from $cmd")
            reply.documents.head
        }
      case foo ⇒ {
        throw new RxMongoError(s"Unknown result from Connection: $foo")
      }
    }
  }

  private def processWriteCommandResult(cmd : Command)(any : Any) : WriteResult = {
    val doc = processReplyMessage(cmd)(any)
    WriteResult(doc)
  }

  private def processDoubleOkCommandResult(cmd : Command)(any : Any) : Boolean = {
    val doc = processReplyMessage(cmd)(any)
    doc.asDouble("ok") == 1.0
  }

}

object Collection {
  def apply(name : String, db : Database)(
    implicit timeout : Timeout = db.timeout,
    writeConcern : WriteConcern = db.writeConcern,
    retryStrategy : RetryStrategy = RetryStrategy.default,
    statsRefresh : FiniteDuration = 1.day) = new Collection(name, db)(timeout, writeConcern, retryStrategy, statsRefresh)
}
