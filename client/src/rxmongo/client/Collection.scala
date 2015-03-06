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

import akka.pattern.ask
import akka.util.Timeout

import rxmongo.bson._
import rxmongo.driver._
import rxmongo.driver.cmds._
import rxmongo.messages.{Delete, Query, Update}

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration._
import scala.util.{ Failure, Success, Try }

/** Represents a MongoDB Collection
  *
  * @see [[http://docs.mongodb.org/master/reference/method/js-collection/]]
  * @param name THe name of the collection in the database
  * @param db A database object
  */
case class Collection(name : String, db : Database, statsRefresh : FiniteDuration = 1.day)(override implicit val timeout : Timeout = db.timeout,
  override implicit val writeConcern : WriteConcern = db.writeConcern)
  extends RxMongoComponent(db.driver) {

  val fullName = db.namespace + "." + name

  require(!name.contains("$"), "Collection names must not contain a $")
  require(name.length > 0, "Collection names may not be empty")
  require(!name.startsWith("system."), "Collection names may not start with 'system.'")

  private val _stats = new AtomicReference[CollStatsReply](null)
  private val _stats_refreshed_at = new AtomicLong(0)
  private def getRefreshedStats() : CollStatsReply = Try {
    val timeNow = System.currentTimeMillis()
    if (_stats_refreshed_at.get() + statsRefresh.toMillis < timeNow) {
      val result = (db.client.connection ? CollStatsCmd(db.namespace, name)).mapTo[ReplyMessage] map { reply ⇒
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
    } else {
      _stats.get
    }
  } match {
    case Success(stats) ⇒ stats
    case Failure(xcptn) ⇒
      log.error(xcptn, "Failed to acquire collection statustics")
      throw xcptn
  }

  /** Provides access to the aggregation pipeline.
    *
    */
  def aggregate() = ???

  /** Return the average size of an object (document) in the collection. This value is refreshed only as frequently
    * as statsRefresh permits.
    */
  def avgObjSize() = getRefreshedStats().avgObjSize

  /** Return a count of the number of documents in this collection. This value is refreshed only as frequently as
    * statsRefresh permits.
    */
  def count() = getRefreshedStats().count

  /** Wraps eval to copy data between collections in a single MongoDB instance. */
  def copyTo() = ???

  /** Builds an index on a collection. */
  def createIndex(keys : Index, options : IndexOptions) : Future[BSONObject] = {
    options.name.map { name ⇒ require(name.length + fullName.length + 1 < 128, "Full index name must be < 128 characters") }
    val cmd = CreateIndicesCmd(db.name, name, Seq(keys -> options))
    db.client.connection.ask(cmd) map processReplyMessage(cmd)
  }

  def createIndices(indices : (Index, IndexOptions)*) : Future[BSONObject] = {
    for ((index, options) ← indices) {
      options.name.map { name ⇒ require(name.length + fullName.length + 1 < 128, "Full index name must be < 128 characters") }
    }
    val cmd = CreateIndicesCmd(db.name, name, indices)
    db.client.connection.ask(cmd) map processReplyMessage(cmd)
  }

  /** Renders a human-readable view of the data collected by indexStats which reflects B-tree utilization. */
  def getIndexStats() = ???
  /** Renders a human-readable view of the data collected by indexStats which reflects B-tree utilization. */
  def indexStats() = ???
  /** Returns the size of the collection. Wraps the size field in the output of the collStats. */
  def dataSize() = ???

  def deleteOne(delete : Delete, ordered : Boolean = true)(implicit to : Timeout = timeout, wc : WriteConcern = writeConcern) : Future[BSONObject] = {
    remove(Seq(delete), ordered)(to, wc)
  }

  def delete(deletes : Seq[Delete], ordered : Boolean = true)(implicit to : Timeout = timeout, wc : WriteConcern = writeConcern) : Future[BSONObject] = {
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
    options : QueryOptions = QueryOptions()) : Future[Cursor] = {
    val msg = QueryMessage(fullName, selector.result, projection.map { p ⇒ p.result }, options)
    db.client.connection.ask(msg) map {
      case reply : ReplyMessage ⇒
        reply.error match {
          case Some(msg) ⇒ throw new RxMongoError(s"Error while processing query {$selector}: $msg")
          case None ⇒ Cursor(this, db.client.connection, msg, reply)
        }
    }
  }

  /** Performs a query to return all documents in a collection
    *
    * @param projection Optional. The Projectiont hat selects the fields in the documents to return.
    * @return
    */
  def findAll(projection : Option[Projection] = None) : Future[Cursor] = find(Query(), projection)

  /** Performs a query and returns a single document.
    * This is simply a convenience function for calling find with the options.numberToReturn set to 1
    * @param selector The Query that selects the documents to return.
    * @param projection Optional. The Projection that selects the fields in the documents to return.
    * @param options Options that control how the query is done and the kind of cursor returned.
    * @return A Cursor to allow iteration over the result set.
    */
  def findOne(selector : Query, projection : Option[Projection] = None, options : QueryOptions = QueryOptions()) : Future[Cursor] = {
    find(selector, projection, options.withNumberToReturn(1))
  }

  /** Atomically modifies and returns a single document. */
  def findAndModify() = ???

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

  def insertOne(obj : BSONObject)(implicit to : Timeout = timeout, wc : WriteConcern = writeConcern) : Future[BSONObject] = {
    insert(Seq(obj), ordered = true)(to, wc)
  }

  def insert(objs : Seq[BSONObject], ordered : Boolean = true)(implicit to : Timeout = timeout, wc : WriteConcern = writeConcern) : Future[BSONObject] = {
    val insert = InsertCmd(db.name, name, objs, ordered, wc)
    db.client.connection.ask(insert)(to) map processReplyMessage(insert)
  }

  /** Reports if a collection is a capped collection. */
  def isCapped : Boolean = getRefreshedStats().capped

  /** Performs map-reduce style data aggregation. */
  def mapReduce() = ???
  /** Rebuilds all existing indexes on a collection. */
  def reIndex() = ???
  /** Deletes documents from a collection. */
  def removeRaw(selector : Query) : Unit = {
    val delete = DeleteMessage(fullName, selector.result)
    db.client.connection ! delete
  }

  def remove(deletes : Seq[Delete], ordered : Boolean = true)(implicit to : Timeout = timeout, wc : WriteConcern = writeConcern) : Future[BSONObject] = {
    val delete = DeleteCmd(db.name, name, deletes, ordered, wc)
    db.client.connection.ask(delete)(to) map processReplyMessage(delete)
  }

  /** Changes the name of a collection. */
  def renameCollection(newName : String, dropTarget : Boolean) : Future[Option[Collection]] = {
    val rename = RenameCollectionCmd(db.name, name, newName, dropTarget)
    (db.client.connection.ask(rename) map processDoubleOkCommandResult(rename)) map { v ⇒
      if (v)
        Some(Collection(newName, db, statsRefresh)(timeout, writeConcern))
      else
        None
    }
  }

  /** Provides a wrapper around an insert() and update() to insert new documents. */
  def save() = ???
  /** Reports on the state of a collection. */
  def stats() : CollStatsReply = getRefreshedStats()

  /** Reports the total size used by the collection in bytes. Provides a wrapper around the storageSize field of the collStats output. */
  def storageSize() = getRefreshedStats().storageSize
  /** Reports the total size of a collection, including the size of all documents and all indexes on a collection. */
  def totalSize() = getRefreshedStats().size
  /** Reports the total size used by the indexes on a collection. Provides a wrapper around the totalIndexSize field of the collStats output. */
  def totalIndexSize() = getRefreshedStats().totalIndexSize

  /** Modifies a document in a collection. */
  def update(updates : Seq[Update], ordered : Boolean = true)(implicit to : Timeout = timeout, wc : WriteConcern = writeConcern) : Future[WriteResult] = {
    val update = UpdateCmd(db.name, name, updates, ordered, wc)
    db.client.connection.ask(update)(to) map processWriteCommandResult(update)
  }

  /** Update One
    * Apply a single Update selector and updater to the collection.
    * @param u The Update specification
    * @param ordered If true, then when an update statement fails, return without performing the remaining update
    *     statements. If false, then when an update fails, continue with the remaining update statements,
    *     if any. Defaults to true.
    * @param to The timeout for the update operation
    * @param wc The write concern for the update operation
    * @return A future WriteResult that returns the result of the update operation
    */
  def updateOne(u : Update, ordered : Boolean = true)(implicit to : Timeout = timeout, wc : WriteConcern = writeConcern) : Future[WriteResult] = {
    update(Seq(u), ordered)(to, wc)
  }
  /** Performs diagnostic operations on a collection. */
  def validate() = ???

  private def processReplyMessage[T](cmd : Command)(any : Any) : BSONObject = {
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
    doc.getAsDouble("ok") == 1.0
  }

}
