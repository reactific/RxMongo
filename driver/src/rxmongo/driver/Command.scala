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

import rxmongo.bson._
import rxmongo.bson.BSONCodec._

class Command(db : String, val selector : BSONObject) extends GenericQueryMessage {
  val fullCollectionName = s"$db.$$cmd"
  val options = QueryOptions.default
  val returnFieldsSelector : Option[BSONObject] = None
}

case class GetLastErrorCmd(db : String) extends Command(db, BSONObject("getLastError" → 1))
case class DBStatsCmd(db : String) extends Command(db, BSONObject("dbStats" → 1))
case class CollStatsCmd(db : String, collection : String, scale : Int = 1024, verbose : Boolean = true)
  extends Command(db, BSONObject("collStats" → collection, "scale" → scale, "verbose" → verbose))

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
  writeConcern : WriteConcern) extends Command(db, BSONObject(
  "insert" → coll,
  "documents" → BSONArray(documents),
  "ordered" → ordered,
  "writeConcern" → WriteConcern.Codec.write(writeConcern)
))

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
  writeConcern : WriteConcern) extends Command(db,
  BSONObject(
    "delete" → coll,
    "deletes" → BSONArray[Delete, BSONObject](deletes),
    "ordered" → ordered,
    "writeConcern" → WriteConcern.Codec.write(writeConcern)
  ))

/** Representation Of An Update Command
  *
  * {{{
  * {
  * update: <collection>,
  * updates:
  * [
  *  { q: <query>, u: <update>, upsert: <boolean>, multi: <boolean> },
  *  { q: <query>, u: <update>, upsert: <boolean>, multi: <boolean> },
  *  { q: <query>, u: <update>, upsert: <boolean>, multi: <boolean> },
  *  ...
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
  writeConcern : WriteConcern) extends Command(db,
  BSONObject(
    "update" → coll,
    "updates" → BSONArray[Update, BSONObject](updates),
    "ordered" → ordered,
    "writeConcern" → WriteConcern.Codec.write(writeConcern)
  ))

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

