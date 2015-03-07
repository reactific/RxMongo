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
import rxmongo.driver.{ Command, Projection, WriteConcern }
import rxmongo.messages.{Delete, Query, Update}

/** Represents A MongoDB Write Command
  * @see [[http://docs.mongodb.org/master/reference/command/insert/#dbcmd.insert]]
  * @param db
  * @param coll
  * @param documents
  * @param ordered
  * @param writeConcern
  */
case class InsertCmd(
  db : String,
  coll : String,
  documents : Seq[BSONObject],
  ordered : Boolean,
  writeConcern : WriteConcern) extends Command(db, {
  val wc = WriteConcern.Codec.write(writeConcern)
  val docLen = documents.foldLeft(0) { case (sum, obj) ⇒ sum + obj.buffer.length }
  val lenHint = 9 + coll.length + 15 + 8 * documents.size + docLen + 9 + 19 + wc.buffer.length
  val b = BSONBuilder(lenHint)
  b.string("insert", coll)
  b.array("documents", documents)
  b.boolean("ordered", ordered)
  b.obj("writeConcern", wc)
  b.result
})

/** Represents a MongoDB Delete Command
  *
  * @see [[http://docs.mongodb.org/master/reference/command/delete/]]
  * @param db
  * @param coll
  * @param deletes
  * @param ordered
  * @param writeConcern
  */
case class DeleteCmd(
  db : String,
  coll : String,
  deletes : Seq[Delete],
  ordered : Boolean,
  writeConcern : WriteConcern) extends Command(db, {
  val wc = WriteConcern.Codec.write(writeConcern)
  var sum = 0
  val dels = deletes.map { d ⇒ val obj = Delete.Codec.write(d); sum += obj.length; obj }
  val lenHint = 9 + coll.length + 8 + deletes.size * 8 + sum + 9 + 19 + wc.buffer.length
  val b = BSONBuilder(lenHint)
  b.string("delete", coll)
  b.array("deletes", dels)
  b.boolean("ordered", ordered)
  b.obj("writeConcern", wc)
  b.result
}
)

/** Representation Of An Update Command
  *
  * {{{
  * {
  * update: <collection>,
  * updates:
  * [
  * { q: <query>, u: <update>, upsert: <boolean>, multi: <boolean> },
  * { q: <query>, u: <update>, upsert: <boolean>, multi: <boolean> },
  * { q: <query>, u: <update>, upsert: <boolean>, multi: <boolean> },
  * ...
  * ],
  * ordered: <boolean>,
  * writeConcern: { <write concern> }
  * }
  * }}}
  *
  * @see [[http://docs.mongodb.org/master/reference/command/update/]]
  * @param db
  * @param coll
  * @param updates
  * @param ordered
  * @param writeConcern
  */
case class UpdateCmd(
  db : String,
  coll : String,
  updates : Seq[Update],
  ordered : Boolean,
  writeConcern : WriteConcern) extends Command(db, {
  val wc = WriteConcern.Codec.write(writeConcern)
  var sum = 0
  val dels = updates.map { u ⇒ val obj = Update.Codec.write(u); sum += obj.buffer.length; obj }
  val lenHint = 9 + coll.length + 8 + updates.size * 8 + sum + 9 + 19 + wc.buffer.length
  val b = BSONBuilder(lenHint)
  b.string("update", coll)
  b.array("updates", dels)
  b.boolean("ordered", ordered)
  b.obj("writeConcern", wc)
  b.result
}
)

/** findAndModify
  * Returns and modifies a single document.
  * @see [[http://docs.mongodb.org/master/reference/command/findAndModify/#dbcmd.findAndModify]]
  * @param db The name of the database containing the collection against which the command is run
  * @param coll The collection against which to run the command.
  * @param query Optional. The selection criteria for the modification. The query field employs the same query
  *   selectors as used in the db.collection.find() method. Although the query may match multiple documents,
  *   findAndModify will only select one document to modify.
  * @param sortBy Optional. Determines which document the operation modifies if the query selects multiple documents.
  *    findAndModify modifies the first document in the sort order specified by this argument.
  * @param update Must specify either the remove or the update field. Performs an update of the selected document. The
  *    update field employs the same update operators or field: value specifications to modify the
  *    selected document.
  * @param remove Must specify either the remove or the update field. Removes the document specified in the query field.
  *    Set this to true to remove the selected document . The default is false.
  * @param returnNew Optional. When true, returns the modified document rather than the original. The findAndModify
  *       method ignores the new option for remove operations. The default is false.
  * @param upsert Optional. Used in conjunction with the update field. When true, findAndModify creates a new document
  *    if no document matches the query, or if documents match the query, findAndModify performs an update.
  *    To avoid multiple upserts, ensure that the query fields are uniquely indexed. The default is false.
  * @param fields Optional. A subset of fields to return. The fields document specifies an inclusion of a field with 1,
  *    as in: fields: { <field1>: 1, <field2>: 1, ... }.
  */
case class FindAndModifyCmd(
  db : String,
  coll : String,
  query : Option[Query] = None,
  sortBy : Seq[(String, Boolean)] = Seq.empty[(String, Boolean)],
  update : Option[BSONObject] = None,
  remove : Option[Boolean] = None,
  returnNew : Option[Boolean] = None,
  upsert : Option[Boolean] = None,
  fields : Option[Projection] = None) extends Command(db, {
  val b = BSONBuilder()
  b.string("findAndModify", coll)
  query.map { q ⇒ b.obj("query", q.result) }
  if (sortBy.nonEmpty) {
    val s = BSONBuilder()
    sortBy.map { case (field, ascending) ⇒ s.integer(field, if (ascending) 1 else -1) }
  }
  update.map { u ⇒ b.obj("update", u) }
  remove.map { r ⇒ b.boolean("remove", r) }
  upsert.map { u ⇒ b.boolean("upsert", u) }
  returnNew.map { rn ⇒ b.boolean("new", rn) }
  fields.map { f ⇒ b.obj("fields", f.result) }
  b.result
})

/** getLastError
  * Returns the success status of the last operation. Probably not needed since the WriteConcern change in 2.6
  * @see [[http://docs.mongodb.org/master/reference/command/getLastError/#dbcmd.getLastError]]
  * @param db The database on which the operation should be sent
  * @param writeConcern The write concern
  * @param fsync Whether to fsync or not
  */
case class GetLastErrorCmd(
  db : String,
  writeConcern : WriteConcern,
  fsync : Boolean = false) extends Command(db, {
  val b = BSONBuilder()
  b.integer("getLastError", 1)
  b.boolean("fsync", fsync)
  BSONObject(b.result ++ writeConcern.toBSONObject)
})

/** eval
  * The eval command evaluates JavaScript functions on the database server.
  * @see [[http://docs.mongodb.org/master/reference/command/eval/]]
  * @param db The database to send the command to
  * @param function The function to invoke in the database server
  * @param args Optional. An array of arguments to pass to the JavaScript function. Omit if the function does not
  *  take arguments.
  * @param nolock Optional. By default, eval takes a global write lock before evaluating the JavaScript function.
  *    As a result, eval blocks all other read and write operations to the database while the eval operation
  *    runs. Set nolock to true on the eval command to prevent the eval command from taking the global write
  *    lock before evaluating the JavaScript. nolock does not impact whether operations within the JavaScript
  *    code itself takes a write lock.
  */
case class EvalCmd(
  db : String,
  function : String,
  args : Option[BSONArray] = None,
  nolock : Option[Boolean] = None) extends Command(db, {
  val b = BSONBuilder()
  b.string("eval", function)
  args.map { a ⇒ b.array("args", a) }
  nolock.map { nl ⇒ b.boolean("nolock", nl) }
  b.result
})

/** parallelCollectionScan
  * Lets applications use multiple parallel cursors when reading documents from a collection.
  * @see [[http://docs.mongodb.org/master/reference/command/parallelCollectionScan/#dbcmd.parallelCollectionScan]]
  * @param db The database containing the collection
  * @param coll The collection to scan in parallel
  * @param numCursors The number of cursors to return for scanning in parallel
  */
case class ParallelCollectionScan(
  db : String,
  coll : String,
  numCursors : Int) extends Command(db, {
  val b = BSONBuilder()
  b.string("parallelCollectionScan", coll)
  b.integer("numCursors", numCursors)
  b.result
}
)
