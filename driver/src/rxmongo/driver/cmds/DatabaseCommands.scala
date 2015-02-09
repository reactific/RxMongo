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

package rxmongo.driver.cmds

import rxmongo.bson._
import rxmongo.driver.{ AdminCommand, Index, IndexOptions, Command }

/** listCollections
  * @see [[http://docs.mongodb.org/master/reference/command/listCollections/]]
  * @param db The database in which to list the collections
  * @param filter A filter to trim the results
  */
case class ListCollectionsCmd(db : String, filter : Query)
  extends Command(db, BSONObject("listCollections" → 1, "filter" → filter.result))

/** dropCollection
  *
  * @see [[http://docs.mongodb.org/master/reference/command/drop/]]
  * @param db The database the contains the collection to be dropped
  * @param coll The collection to be dropped
  */
case class DropCollectionCmd(
  db : String,
  coll : String) extends Command(db, BSONObject("drop" → coll))

/** create - Explicitly creates a collection.
  *
  * @see [[http://docs.mongodb.org/master/reference/command/create/]]
  * Create has the following form:
  * {{{
  * { create: <collection_name>,
  * capped: <true|false>,
  * autoIndexId: <true|false>,
  * size: <max_size>,
  * max: <max_documents>,
  * flags: <0|1>
  * }
  * }}}
  * @param db THe database in which to create the collection
  * @param coll The collection name to create
  * @param capped Whether this is a capped collection or not
  * @param autoIndexId Whether to create the _id index automatically or not
  * @param size The maximum collection size (in bytes) for capped collections
  * @param max THe maximum number of documents for capped collections
  * @param usePowerOf2Sizes Whether to use power of 2 sizes for space allocation
  */
case class CreateCollectionCmd(
  db : String,
  coll : String,
  capped : Boolean = false,
  autoIndexId : Boolean = true,
  size : Option[Int] = None,
  max : Option[Int] = None,
  usePowerOf2Sizes : Int = 1) extends Command(db, BSONObject())

/** cloneCollection
  * Copies a collection from a remote host to the current host.
  * @see [[http://docs.mongodb.org/master/reference/command/cloneCollection/]]
  * @param db The database name containing the collection to clone
  * @param coll The name of the collection to clone
  * @param host The host on which the source collection resides
  * @param query A query to filter the documents cloned
  */
case class CloneCollectionCmd(db : String, coll : String, host : String, query : Query)
  extends AdminCommand(BSONObject("cloneCollection" → (db + "." + coll), "host" → host, "query" → query.result))

/** cloneCollectionAsCapped
  * Copies a non-capped collection as a new capped collection.
  * @param db The database name to operate on
  * @param existing THe existing collection to clone documents from
  * @param target The name of the new capped collection
  * @param size The maximum size of the new capped collection (bytes not # of documents)
  */
case class CloneCollectionAsCapped(db : String, existing : String, target : String, size : Int)
  extends AdminCommand(
    BSONObject("cloneCollectionAsCapped" → (db + "." + existing), "toCollection" → (db + "." + target), "size" → size)
  )

/** convertToCapped
  * Converts a non-capped collection to a capped collection.
  * @see [[http://docs.mongodb.org/master/reference/command/convertToCapped/]]
  * @param db The database name to operate on
  * @param coll The name of an existing collectiont to be converted to a capped collection
  * @param size The maximum size, in bytes, of the new capped collection
  */
case class ConvertToCapped(db : String, coll : String, size : Int)
  extends AdminCommand(
    BSONObject("convertToCapped" → (db + "." + coll), "size" → size)
  )

/** createIndexes
  * @see [[http://docs.mongodb.org/master/reference/command/createIndexes/]]
  * @param db The database containing the collection
  * @param coll The name of the collection to get the indices
  * @param indices The indices and their options to add to the collection
  */
case class CreateIndicesCmd(
  db : String,
  coll : String,
  indices : Iterable[(Index, IndexOptions)]) extends Command(db,
  BSONObject(
    "createIndexes" -> coll,
    "indexes" -> BSONArray(
      for ((keys, options) ← indices) yield {
        val obj : BSONObject = options.result + ("key" -> keys.result)
        if (obj.contains("name"))
          obj
        else {
          obj + ("name" -> BSONString(db + "." + coll + "." + keys.name))
        }
      }
    )
  ))

/** listIndexes
  * @see [[http://docs.mongodb.org/master/reference/command/listIndexes/]]
  * @param db The database containing the collection
  * @param coll THe collection for which indices should be listed
  */
case class ListIndicesCmd(
  db : String,
  coll : String) extends Command(db, BSONObject("listIndexes" → coll))

/** dropIndexes
  * @see [[http://docs.mongodb.org/master/reference/command/dropIndexes/]]
  * @param db The database containing the collection
  * @param coll The collection on which the index should be dropped
  * @param index The name of the index to drop
  */
case class DropIndicesCmd(
  db : String,
  coll : String,
  index : String) extends Command(db, BSONObject("dropIndexes" → coll, "index" → index))

/** dropIndexes
  *
  * @see [[http://docs.mongodb.org/master/reference/command/dropIndexes/]]
  * @param db The database containing the collection
  * @param coll The collection on which all indexes should be dropped
  */
case class DropAllIndicesCmd(db : String, coll : String)
  extends Command(db, BSONObject("dropIndexes" → coll, "index" → "*"))

/** compact
  * Defragments a collection and rebuilds the indexes.
  * @param db The name of the database.
  * @param coll The name of the collection.
  * @param force If true, compact can run on the primary in a replica set. If false, compact returns an error when
  *         run on a primary, because the command blocks all other activity. Compact blocks activity only for
  *         the database it is compacting.
  * @param paddingFactor Describes the record size allocated for each document as a factor of the document size for
  *                 all records compacted during the compact operation. The paddingFactor does not affect
  *                 the padding of subsequent record allocations after compact completes.
  * @param paddingBytes Sets the padding as an absolute number of bytes for all records compacted during the compact
  *                operation. After compact completes, paddingBytes does not affect the padding of subsequent
  *                record allocations.
  */
case class CompactCmd(
  db : String,
  coll : String,
  force : Option[Boolean] = None,
  paddingFactor : Option[Double] = None,
  paddingBytes : Option[Int] = None) extends Command(db, {
  val b = BSONBuilder()
  b.string("compact", coll)
  force.map { f ⇒ b.boolean("force", f) }
  paddingFactor.map { pf ⇒ b.double("paddingFactor", pf) }
  paddingBytes.map { pb ⇒ b.integer("paddingBytes", pb) }
  b.result
})

/** collMod
  * @see http://docs.mongodb.org/master/reference/command/collMod/
  * @param db The database name
  * @param coll The TTL collection name
  * @param fieldName The name of the data typed field for the TTL index
  * @param newExpireAfterSeconds The new value for expireAfterSeconds
  */
case class SetExpirationCmd(
  db : String,
  coll : String,
  fieldName : String,
  newExpireAfterSeconds : Int) extends Command(db,
  BSONObject(
    "collMod" → coll,
    "index" → BSONObject("keyPattern" → BSONObject(fieldName → 1), "expireAfterSeconds" → newExpireAfterSeconds)
  )
)

/** collMod
  * @see [[http://docs.mongodb.org/master/reference/command/collMod/]]
  * @param db The database name
  * @param coll The collection to have no padding
  */
case class SetNoPaddingCmd(
  db : String,
  coll : String) extends Command(db, BSONObject("collMod" → coll, "noPadding" → 1))

/** reindex
  * @see [[http://docs.mongodb.org/master/reference/command/reIndex/]]
  * @param db The database name
  * @param coll The name of the collection to reindex
  */
case class ReIndexCmd(
  db : String,
  coll : String) extends Command(db, BSONObject("reindex" → coll))

/** touch
  * @see [[http://docs.mongodb.org/master/reference/command/touch/]]
  * @param db The database name
  * @param coll The collection name to touch
  * @param data Whether to touch the data (documents)
  * @param index Whether to touch the index
  */
case class TouchCmd(
  db : String,
  coll : String,
  data : Boolean = false,
  index : Boolean = false) extends Command(db, BSONObject("touch" -> coll, "data" -> data, "index" -> index))

