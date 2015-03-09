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

import rxmongo.bson.{ BSONBuilder, BSONObject }
import rxmongo.driver.{ AdminCommand, WriteConcern }

/** flushRouterConfig
  * Forces an update to the cluster metadata cached by a mongos.
  * @see [[http://docs.mongodb.org/master/reference/command/flushRouterConfig/]]
  */
case class FlushRouterConfigCmd() extends AdminCommand(BSONObject("flushRouterConfig" → 1))

/** addShard
  * Adds a shard to a sharded cluster.
  */
case class AddShardCmd(host : String, maxSize : Option[Int], name : Option[String]) extends AdminCommand({
  val b = BSONBuilder()
  b.string("addShard", host)
  maxSize.map { ms ⇒ b.integer("maxSize", ms) }
  name.map { n ⇒ b.string("name", n) }
  b.result
})

/** cleanupOrphaned
  * Removes orphaned data with shard key values outside of the ranges of the chunks owned by a shard.
  * @see [[http://docs.mongodb.org/master/reference/command/cleanupOrphaned/]]
  * @param db The name of the database containing the collection for which orphan data will be cleaned
  * @param coll The name of the collection for which orphan data will be cleaned
  * @param startingFromKey Optional. The shard key value that determines the lower bound of the cleanup range.
  *              The default value is MinKey. If the range that contains the specified startingFromKey value
  *              belongs to a chunk owned by the shard, cleanupOrphaned continues to examine the next
  *              ranges until it finds a range not owned by the shard.
  * @param secondaryThrottle Optional. If true, each delete operation must be replicated to another secondary before
  *                the cleanup operation proceeds further. If false, do not wait for replication. Defaults
  *                to false. Independent of the secondaryThrottle setting, after the final delete,
  *                cleanupOrphaned waits for all deletes to replicate to a majority of replica set members
  *                before returning.
  * @param writeConcern Optional. A document that expresses the write concern that the secondaryThrottle will use to
  *           wait for the secondaries when removing orphaned data. Any specified writeConcern implies
  *           _secondaryThrottle.
  */
case class CleanupOrphanedCmd(
  db : String,
  coll : String,
  startingFromKey : Option[BSONObject] = None,
  secondaryThrottle : Option[Boolean] = None,
  writeConcern : Option[WriteConcern] = None) extends AdminCommand({
  val b = BSONBuilder()
  b.string("cleanupOrphaned", db + "." + coll)
  startingFromKey.map { sfk ⇒ b.obj("startingFromKey", sfk) }
  secondaryThrottle.map { st ⇒ b.boolean("secondaryThrottle", st) }
  writeConcern.map { wc ⇒ b.obj("writeConcern", wc.result) }
  b.result
})

/** enableSharding
  * Enables sharding on a specific database.
  * @see [[http://docs.mongodb.org/master/reference/command/enableSharding/]]
  * @param db The name of the database on which sharding will be enabled
  */
case class EnableShardingCmd(db : String) extends AdminCommand(BSONObject("enableSharding" → db))

/** listShards
  * Returns a list of configured shards.
  * @see [[http://docs.mongodb.org/master/reference/command/listShards/]]
  */
case class ListShardsCmd() extends AdminCommand(BSONObject("listShards" → 1))

/** removeShard
  * Starts the process of removing a shard from a sharded cluster.
  * @see [[http://docs.mongodb.org/master/reference/command/removeShard/]]
  * @param shardName the name of the shard to remove
  */
case class RemoveShardCmd(shardName : String) extends AdminCommand(BSONObject("removeShard" → shardName))

/** mergeChunks
  * Provides the ability to combine chunks on a single shard.
  */
case class MergeChunksCmd(db : String, coll : String, bounds : Seq[BSONObject]) extends AdminCommand({
  val b = BSONBuilder()
  b.string("mergeChunks", db + "." + coll)
  b.array("bounds", bounds)
  b.result
})

/** shardCollection
  * Enables the sharding functionality for a collection, allowing the collection to be sharded.
  * @see [[http://docs.mongodb.org/master/reference/command/shardCollection/]]
  * @param db The database that contains the collection to be sharded
  * @param coll The collection to be sharded
  * @param key The index specification document to use as the shard key. The index must exist prior to the
  *  shardCollection command, unless the collection is empty. If the collection is empty, in which case
  *  MongoDB creates the index prior to sharding the collection. The key may be in the form
  *  { field : "hashed" }, which will use the specified field as a hashed shard key.
  * @param unique When true, the unique option ensures that the underlying index enforces a unique constraint. Hashed
  *     shard keys do not support unique constraints.
  * @param numInitialChunks Specifies the number of chunks to create initially when sharding an empty collection with a
  *               hashed shard key. MongoDB will then create and balance chunks across the cluster. The
  *               numInitialChunks must be less than 8192 per shard. If the collection is not empty,
  *               numInitialChunks has no effect.
  */
case class ShardCollectionCmd(
  db : String,
  coll : String,
  key : BSONObject,
  unique : Boolean,
  numInitialChunks : Integer) extends AdminCommand(BSONObject(
  "shardCollection" → (db + "." + coll), "key" → key, "unique" → unique, "numInitialChunks" → numInitialChunks)
)

/** shardingState
  * Reports whether the mongod is a member of a sharded cluster.
  * @see [[http://docs.mongodb.org/master/reference/command/shardingState/]]
  */
case class ShardingStateCmd() extends AdminCommand(BSONObject("shardingState" → 1))

/** split
  * Creates a new chunk.
  * @see [[http://docs.mongodb.org/master/reference/command/split/]]
  */
case class SplitCmd(
  db : String,
  coll : String,
  find : BSONObject,
  bounds : Seq[BSONObject],
  middle : BSONObject) extends AdminCommand(BSONObject(
  "split" → (db + "." + coll), "find" → find, "bounds" → bounds, "middle" → middle
))

/** isdbgrid
  * Verifies that a process is a mongos.
  * @see [[http://docs.mongodb.org/master/reference/command/isdbgrid/]]
  */
case class IsDbGridCmd() extends AdminCommand(BSONObject("isdbgrid" → 1))

