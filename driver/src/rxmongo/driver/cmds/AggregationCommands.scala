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

import rxmongo.bson.{ Query, BSONArray, BSONBuilder, BSONObject }
import rxmongo.driver.Command

/** aggregate
  * Performs aggregation tasks such as group using the aggregation framework.
  * @see [[http://docs.mongodb.org/master/reference/command/aggregate/]]
  * @param db The database containing the collection to aggregate
  * @param coll The name of the collection to as the input for the aggregation pipeline.
  * @param pipeline An array of aggregation pipeline stages that process and transform the document stream as
  *         part of the aggregation pipeline.
  * @param explain Optional. Specifies to return the information on the processing of the pipeline.
  * @param allowDiskUse Optional. Enables writing to temporary files. When set to true, aggregation stages can write
  *             data to the _tmp subdirectory in the dbPath directory.
  * @param batchSize Optional. Specify a document that contains options that control the creation of the cursor object.
  */
case class AggregateCmd(
  db : String,
  coll : String,
  pipeline : BSONArray,
  explain : Option[Boolean] = None,
  allowDiskUse : Option[Boolean] = None,
  batchSize : Int = 0) extends Command(db, {
  val b = BSONBuilder()
  b.string("aggregate", coll)
  b.array("pipeline", pipeline)
  explain.map { exp ⇒ b.boolean("explain", exp) }
  allowDiskUse.map { adu ⇒ b.boolean("allowDiskUse", adu) }
  b.obj("cursor", BSONObject("batchSize" → batchSize))
  b.result
})

/** count
  * Counts the number of documents in a collection.
  * @see [[http://docs.mongodb.org/master/reference/command/count/]]
  * @param db The database containing the collection to count
  * @param coll The name of the collection to count.
  * @param query A query that selects which documents to count in a collection.
  * @param limit The maximum number of matching documents to return.
  * @param skip The number of matching documents to skip before returning results.
  * @param hint The index to use. Specify either the index name as a string or the index specification document.
  */
case class CountCmd(
  db : String,
  coll : String,
  query : Query,
  limit : Option[Int] = None,
  skip : Option[Int] = None,
  hint : Option[String] = None) extends Command(db, {
  val b = BSONBuilder()
  b.string("count", coll)
  limit.map { l ⇒ b.integer("limit", l) }
  skip.map { s ⇒ b.integer("skip", s) }
  hint.map { h ⇒ b.obj("hint", BSONObject(h → 1)) }
  b.result
})

/** distinct
  * Displays the distinct values found for a specified key in a collection.
  * @see [[http://docs.mongodb.org/master/reference/command/distinct/]]
  * @param db The name of the database containing the collection to query for distinct values.
  * @param coll The name of the collection to query for distinct values.
  * @param key The field to collect distinct values from.
  * @param query Optional. A query specification to limit the input documents in the distinct analysis.
  */
case class DistinctCmd(
  db : String,
  coll : String,
  key : String,
  query : Option[Query] = None) extends Command(db, {
  val b = BSONBuilder()
  b.string("distinct", coll)
  b.string("key", key)
  query.map { q ⇒ b.obj("query", q.result) }
  b.result
})

/** group
  * Groups documents in a collection by the specified key and performs simple aggregation.
  * @see [[http://docs.mongodb.org/master/reference/command/group/]]
  * @param db The name of the database that contains the collection from which to perform the group by operation.
  * @param coll The name of the collection from which to perform the group by operation.
  * @param key The field or fields to group. Returns a “key object” for use as the grouping key.
  * @param reduce An aggregation function that operates on the documents during the grouping operation. These functions
  *       may return a sum or a count. The function takes two arguments: the current document and an
  *       aggregation result document for that group.
  * @param initial Initializes the aggregation result document.
  * @param keyf Optional. Alternative to the key field. Specifies a function that creates a “key object” for use as the
  *     grouping key. Use \$keyf instead of key to group by calculated fields rather than existing document
  *     fields.
  * @param cond Optional. The selection criteria to determine which documents in the collection to process. If you
  *     omit the cond field, group processes all the documents in the collection for the group operation.
  * @param finalizer Optional. A function that runs each item in the result set before group returns the final value.
  *         This function can either modify the result document or replace the result document as a whole.
  *         Unlike the \$keyf and \$reduce fields that also specify a function, this field name is finalize,
  *         not \$finalize.
  */
case class GroupCmd(
  db : String,
  coll : String,
  key : BSONObject,
  reduce : String,
  initial : BSONObject,
  keyf : Option[String] = None,
  cond : Option[Query] = None,
  finalizer : Option[String] = None) extends Command(db, {
  val b = BSONBuilder()
  b.string("ns", coll)
  b.obj("key", key)
  b.string("$reduce", reduce)
  b.obj("initial", initial)
  keyf.map { kf ⇒ b.string("$keyf", kf) }
  cond.map { c ⇒ b.obj("cond", c.result) }
  finalizer.map { f ⇒ b.string("finalize", f) }
  BSONObject("group" → b.result)
})

/** mapReduce
  * Performs map-reduce aggregation for large data sets.
  * @see [[http://docs.mongodb.org/master/reference/command/mapReduce/]]
  * @param db The name of the database containing the collection on which you want to perform map-reduce.
  * @param coll The name of the collection on which you want to perform map-reduce. This collection will be filtered
  *     using query before being processed by the map function.
  * @param map A JavaScript function that associates or “maps” a value with a key and emits the key and value pair.
  * @param reduce A JavaScript function that “reduces” to a single object all the values associated with a particular key.
  * @param out Specifies where to output the result of the map-reduce operation. You can either output to a collection
  *    or return the result inline. On a primary member of a replica set you can output either to a collection
  *    or inline, but on a secondary, only inline output is possible. When None is specified, you get inline
  *    output. Otherwise, the document provided specifies output to a collection.
  * @param query Optional. Specifies the selection criteria using query operators for determining the documents input
  *      to the map function.
  * @param sort Optional. Sorts the input documents. This option is useful for optimization. For example, specify the
  *     sort key to be the same as the emit key so that there are fewer reduce operations. The sort key must
  *     be in an existing index for this collection.
  * @param limit Optional. Specifies a maximum number of documents for the input into the map function.
  * @param finalizer Optional. Follows the reduce method and modifies the output.
  * @param scope Optional. Specifies global variables that are accessible in the map, reduce and finalize functions.
  * @param jsMode Optional. Specifies whether to convert intermediate data into BSON format between the execution of
  *       the map and reduce functions. Defaults to false. If false: Internally, MongoDB converts the
  *       JavaScript objects emitted by the map function to BSON objects. These BSON objects are then converted
  *       back to JavaScript objects when calling the reduce function. The map-reduce operation places the
  *       intermediate BSON objects in temporary, on-disk storage. This allows the map-reduce operation to
  *       execute over arbitrarily large data sets. If true: Internally, the JavaScript objects emitted during
  *       map function remain as JavaScript objects. There is no need to convert the objects for the reduce
  *       function, which can result in faster execution. You can only use jsMode for result sets with fewer
  *       than 500,000 distinct key arguments to the mapper’s emit() function. The jsMode defaults to false.
  * @param verbose Optional. Specifies whether to include the timing information in the result information. The verbose
  *        defaults to true to include the timing information.
  */
case class MapReduceCmd(
  db : String,
  coll : String,
  map : String,
  reduce : String,
  out : Option[BSONObject] = None,
  query : Option[Query] = None,
  sort : Option[BSONObject] = None,
  limit : Option[Int] = None,
  finalizer : Option[String] = None,
  scope : Option[BSONObject] = None,
  jsMode : Option[Boolean] = None,
  verbose : Option[Boolean] = None) extends Command(db, {

  val b = BSONBuilder()
  b.string("mapReduce", coll)
  b.string("map", map)
  b.string("reduce", reduce)
  if (out.isDefined) {
    b.obj("out", out.get)
  } else {
    b.obj("out", BSONObject("inline" → 1))
  }
  query.map { q ⇒ b.obj("query", q.result) }
  sort.map { s ⇒ b.obj("sort", s) }
  limit.map { l ⇒ b.integer("limit", l) }
  scope.map { s ⇒ b.obj("scope", s) }
  jsMode.map { m ⇒ b.boolean("jsMode", m) }
  verbose.map { v ⇒ b.boolean("verbose", v) }
  b.result
})
