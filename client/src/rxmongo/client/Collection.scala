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

import rxmongo.bson.{BSONObject, RxMongoError}
import rxmongo.driver._

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration._
import scala.util.{ Failure, Success, Try }

/** Represents a MongoDB Collection
  *
  * @see [[http://docs.mongodb.org/master/reference/method/js-collection/]]
  * @param name THe name of the collection in the database
  * @param db A database object
  */
case class Collection(name : String, db : Database, statsRefesh : FiniteDuration = 1.day)(implicit timeout : Timeout = Driver.defaultTimeout)
  extends RxMongoComponent(db.driver) {

  val fullName = db.namespace + "." + name

  private val _stats = new AtomicReference[CollStatsReply](null)
  private val _stats_refreshed_at = new AtomicLong(0)
  private def getRefreshedStats() : CollStatsReply = Try {
    val timeNow = System.currentTimeMillis()
    if (_stats_refreshed_at.get() + statsRefesh.toMillis < timeNow) {
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
  def createIndex() = ???
  /** Renders a human-readable view of the data collected by indexStats which reflects B-tree utilization. */
  def getIndexStats() = ???
  /** Renders a human-readable view of the data collected by indexStats which reflects B-tree utilization. */
  def indexStats() = ???
  /** Returns the size of the collection. Wraps the size field in the output of the collStats. */
  def dataSize() = ???
  /** Returns an array of documents that have distinct values for the specified field. */
  def distinct() = ???
  /** Removes the specified collection from the database. */
  def drop() = ???
  /** Removes a specified index on a collection. */
  def dropIndex() = ???
  /** Removes all indexes on a collection. */
  def dropIndexes() = ???
  /** Deprecated. Use db.collection.createIndex(). */
  def ensureIndex() = ???
  /** Returns information on the query execution of various methods. */
  def explain() = ???

  /** Performs a query on a collection and returns a cursor object. */
  def find(
    selector : Query,
    projection : Option[Projection] = None,
    options : QueryOptions = QueryOptions()) : Future[Cursor] = {
    val msg = QueryMessage(fullName, selector.result, projection.map { p => p.result }, options)
    db.client.connection.ask(msg) map {
      case reply : ReplyMessage if reply.QueryFailure ⇒
        // TODO: extract error message from reply and add to exception
        throw new RxMongoError("Query Failure")
      case reply : ReplyMessage =>
        Cursor(this, db.client.connection, msg, reply)
    }
  }


  def findAll(projection : Option[Projection] = None) : Future[Cursor] = find(Query(), projection)

  /** Atomically modifies and returns a single document. */
  def findAndModify() = ???
  /** Performs a query and returns a single document. */
  def findOne() = ???
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
  def insert() = ???
  /** Reports if a collection is a capped collection. */
  def isCapped() : Boolean = getRefreshedStats().capped

  /** Performs map-reduce style data aggregation. */
  def mapReduce() = ???
  /** Rebuilds all existing indexes on a collection. */
  def reIndex() = ???
  /** Deletes documents from a collection. */
  def remove() = ???
  /** Changes the name of a collection. */
  def renameCollection() = ???
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
  def update() = ???
  /** Performs diagnostic operations on a collection. */
  def validate() = ???
}
