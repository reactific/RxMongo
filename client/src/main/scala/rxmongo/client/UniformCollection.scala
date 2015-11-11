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

import akka.util.Timeout
import rxmongo.bson._
import rxmongo.messages._
import rxmongo.messages.replies.WriteResult

import scala.concurrent.duration._
import scala.concurrent.{ Await, ExecutionContext, Future }

/** The Base Class Of All Uniform Strables
  *
  * Case classes that are stored in mongo should include this trait to define the required _id field that provides
  * the index field for the document.
  * @tparam IdType The type of the index field which must have a collation order
  */
trait Storable[IdType] {
  def _id : IdType
}

/** A Uniform Collection Of Objects Of One Type
  *
  * Mongo collections can store any kind of document in them but often we wish to constrain the contents of a collection
  * to a single shape. This UniformCollection enforces that exactly one kind of object can be stored in the collection.
  * It also provides the mapping to and from those
  * @param coll The collection in which the objects are stored
  * @param modelCodec The codec for the object type stored in the collection
  * @param idCodec The codec for the object's index type
  * @tparam Model The type of the object stored in the collection
  * @tparam Id The type of the _id field of the Storable
  */
abstract class UniformCollection[Model <: Storable[Id], Id](coll : Collection)(
  implicit val modelCodec : DocumentCodec[Model], implicit val idCodec : Codec[Id]) {

  type Converter = (Id) ⇒ BSONValue

  // implicit def converter : Converter

  /** Returns the number of documents in this collection matching the given selector.
    *
    * @param selector Selector document which may be empty.
    */
  def count(selector : Query,
    limit : Option[Int] = None, skip : Option[Int] = None, hint : Option[String] = None) : Future[Int] = {
    coll.count(selector, limit, skip, hint)
  }

  def countSync(selector : Query,
    limit : Option[Int] = None, skip : Option[Int] = None, hint : Option[String] = None)(implicit timeout : FiniteDuration = coll.timeout.duration) : Int = {
    Await.result(count(selector, limit, skip, hint), timeout)
  }

  /** Fetch An Object By Its Primary Identifier
    * The most common way to fetch an object is by its primary identifier. This just simplifies the syntax by only
    * requiring the value of the identifier
    * @param id The id value to look up
    * @return an optional Model instance
    */
  def fetch(id : Id) : Future[Option[Model]] = {
    implicit val ec = coll.executionContext
    coll.findRaw(Query("_id" $eq id), options = QueryOptions(numberToReturn = 1)) map { msg : ReplyMessage ⇒
      if (msg.documents.isEmpty)
        None
      else
        Some(msg.documents.head.to[Model])
    }
  }

  /** Synchronous version of [[fetch]]
    *
    * @param id The id value to look up.
    * @return an optional Model instance
    */
  def fetchSync(id : Id)(implicit timeout : FiniteDuration = coll.timeout.duration) : Option[Model] = {
    Await.result(fetch(id), timeout)
  }

  /** Find Any objects matching the list of primary identifiers
    *
    * @param ids The list of ids to find
    * @return A Future Sequence of
    */
  def fetchAny(ids : Id*) : Future[Seq[Model]] = {
    findAll(Query("_id" $in (ids : _*)))
  }

  def fetchAll() : Future[Seq[Model]] = {
    implicit val ec = coll.executionContext
    coll.findRaw(Query(), None, QueryOptions(numberToReturn = 0)) map { msg : ReplyMessage ⇒
      if (msg.documents.isEmpty)
        Seq.empty[Model]
      else
        msg.documents.map { doc ⇒ doc.to[Model] }
    }
  }

  def fetchAllSync()(implicit timeout : FiniteDuration = coll.timeout.duration) : Seq[Model] = {
    Await.result(fetchAll(), timeout)
  }

  /** Retrieves at most one model matching the given selector. */
  def findOne(selector : Query) : Future[Option[Model]] = {
    implicit val ec = coll.executionContext
    coll.findRaw(selector, None, QueryOptions(numberToReturn = -1)) map { msg : ReplyMessage ⇒
      if (msg.documents.isEmpty)
        None
      else
        Some(msg.documents.head.to[Model])
    }
  }

  /** Find all matching instances of Model for the given selector and return them.
    * Note that you should only do this for smallish collection. For larger connections,
    * page through them with the [[findByPage]] method.
    *
    * @param selector Document for selecting the instances
    */
  def findAll(selector : Query) : Future[Seq[Model]] = {
    implicit val ec = coll.executionContext
    coll.find(selector) flatMap { c : Cursor ⇒
      c.toFlatSeq map { docs : Seq[BSONDocument] ⇒
        for (doc ← docs) yield { doc.to[Model] }
      }
    }
  }

  /** Find matching instances of Model for the given selector and return a sorted page of them
    * This allows you to constrain the size of the result set so you don't unload too many instances into memory.
    *
    * @param selector Document for selecting the instances.
    * @param page The page number to retrieve (0-based).
    * @param pageSize Maximum number of elements in each page.
    */

  def findByPage(selector : Query, page : Int, pageSize : Int) : Future[Seq[Model]] = {
    coll.find(selector, None, QueryOptions(pageSize * page, pageSize)).flatMap ({ c : Cursor ⇒
      c.toFlatSeq.map ({ docs : Seq[BSONDocument] ⇒
        for (doc ← docs) yield { doc.to[Model] }
      })(coll.executionContext)
    })(coll.executionContext)
  }

  /** Updates and returns a single model. It returns the old document by default.
    *
    * @param update The update object that indicates which object to update and how
    * @param projection The projection to apply to the result set to filter fields
    * @param fetchNewObject When true, returns the updated model rather than the original.
    * @param upsert When true, findAndUpdate() creates a new model if no model matches the query.
    */
  def findAndUpdate(
    update : Update,
    remove : Option[Boolean] = None,
    sort : Sort = Sort.empty,
    projection : Option[Projection] = None,
    fetchNewObject : Boolean = true,
    upsert : Boolean = false) : Future[Option[Model]] = {
    coll.findAndModify(Some(update), remove, sort, projection, fetchNewObject, upsert).map(
      { doc : BSONDocument ⇒ Some(modelCodec.read(doc)) }
    )(coll.executionContext)
  }

  /** Find a random instance matching the selector provided.
    * This method obtains the size of the collection and then attempts to find a random document within it
    * @param selector Document for selecting the instances
    * @return A future to an option of the instance of the Model
    */
  def findRandom(selector : Query) : Future[Option[Model]] = {
    coll.findOne(selector).map({ opt ⇒ opt.map { obj ⇒ modelCodec.read(obj) } })(coll.executionContext)
  }

  /** Inserts the given model. */
  def insertOne(obj : Model)(implicit to : Timeout = coll.timeout, wc : WriteConcern = coll.writeConcern) : Future[WriteResult] = {
    coll.insertOne(BSONObject(modelCodec.write(obj)))(to, wc)
  }

  def insert(objs : Seq[Model], ordered : Boolean = true)(implicit to : Timeout = coll.timeout, wc : WriteConcern = coll.writeConcern) : Future[WriteResult] = {
    val objects = objs.map { m ⇒ BSONObject(modelCodec.write(m)) }
    coll.insert(objects, ordered)(to, wc)
  }

  def insertSync(model : Model, wait : Duration = 5.seconds)(implicit to : Timeout = coll.timeout, wc : WriteConcern = coll.writeConcern) : WriteResult = {
    implicit val ec = coll.executionContext
    Await.result(coll.insertOne(BSONObject(modelCodec.write(model)))(to, wc), wait)
  }

  def upsert(model : Model) : Future[Model] = {
    implicit val ec = coll.executionContext
    val update = Update(Query("_id" $eq model._id), UpdateExpression(BSONDocument(modelCodec.write(model))),
      upsert = true, multi = false, isolated = false)
    findAndUpdate(update, fetchNewObject = true, upsert = true) map { option ⇒
      option match {
        case Some(obj) ⇒ obj
        case None ⇒ model
      }
    }
  }

  def upsertSync(model : Model, wait : Duration = 5.seconds)(implicit to : Timeout = coll.timeout, wc : WriteConcern = coll.writeConcern) : Model = {
    implicit val ec = coll.executionContext
    Await.result(upsert(model), wait)
  }

  /** Bulk inserts multiple models.
    *
    * @param objs A collection of model objects (any traversable collection type)
    * @param bulkSize The size of the bulk insert
    * @param bulkByteSize The size in bytes of the bulk insert
    * @return The number of successful insertions.
    */
  def bulkInsert(objs : TraversableOnce[Model],
    bulkSize : Int /*= bulk.MaxDocs*/ , bulkByteSize : Int /*= bulk.MaxBulkSize*/ )(implicit ec : ExecutionContext) : Future[Int] = ???

  /** Updates the documents matching the given selector.
    *
    * @param selector Selector query.
    * @param update Update query.
    * @param upsert Create the document if it does not exist.
    * @param multi Update multiple documents.
    */
  def update(selector : Model, update : UpdateExpression, upsert : Boolean = false, multi : Boolean = false)(implicit to : Timeout = coll.timeout, wc : WriteConcern = coll.writeConcern) : Future[WriteResult] = ???
  /** Updates the document with the given `id`.
    *
    * @param id ID of the document that will be updated.
    * @param update Update query.
    */
  def updateById(id : Id, update : Model)(implicit to : Timeout = coll.timeout, wc : WriteConcern = coll.writeConcern) : Future[WriteResult] = ???

  /** Removes model(s) matching the given selector.
    *
    * In order to remove multiple documents `firstMatchOnly` has to be `false`.
    *
    * @param selector Selector document.
    * @param firstMatchOnly Remove only the first matching document.
    */
  def remove(selector : BSONDocument, firstMatchOnly : Boolean = false)(implicit to : Timeout = coll.timeout, wc : WriteConcern = coll.writeConcern) : Future[WriteResult] = ???

  /** Removes the document with the given ID. */
  def removeById(id : Id)(implicit to : Timeout = coll.timeout, wc : WriteConcern = coll.writeConcern) : Future[WriteResult] = ???

  /** Removes all documents in this collection. */
  def removeAll()(implicit to : Timeout = coll.timeout, wc : WriteConcern = coll.writeConcern) : Future[WriteResult] = ???

  def drop(implicit ec : ExecutionContext) : Future[Unit] = ???

  /** Drops this collection and awaits until it has been dropped or a timeout has occured.
    * @param timeout Maximum amount of time to await until this collection has been dropped.
    * @return true if the collection has been successfully dropped, otherwise false.
    */
  def dropSync(timeout : Duration = 5.seconds) : Unit = {
    implicit val ec = coll.executionContext
    Await.result(drop, timeout)
  }

  /** Folds the documents matching the given selector by applying the function `f`.
    *
    * @param selector Selector document.
    * @param sort Sorting document.
    * @param state Initial state for the fold operation.
    * @param f Folding function.
    * @tparam A Type of fold result.
    */
  def fold[A](selector : Query, sort : Sort, state : A)(f : (A, Model) ⇒ A)(implicit to : Timeout = coll.timeout, wc : WriteConcern = coll.writeConcern) : Future[A] = ???

  /** Iterates over the documents matching the given selector in the given sort order and applies the function `f`.
    *
    * @param selector Selector document.
    * @param sort Sorting document.
    * @param f function to be applied.
    */
  def foreach(selector : Query, sort : Sort)(f : (Model) ⇒ Unit) : Unit = ???

  /** Lists indexes that are currently ensured in this collection.
    *
    * This list may not be equal to `autoIndexes` in case of index creation failure.
    */
  def listIndices()(implicit ec : ExecutionContext) : Future[List[Index]] = ???

  /** The list of indexes to be ensured on DAO load.
    *
    * Because of Scala initialization order there are exactly 2 ways
    * of defining auto indexes.
    *
    * First way is to use an '''early definition''':
    *
    * {{{
    * object PersonDao extends {
    * override val autoIndexes = Seq(
    *   Index(Seq("name" -> IndexType.Ascending), unique = true, background = true),
    *   Index(Seq("age" -> IndexType.Ascending), background = true))
    * } with BsonDao[Person, BSONObjectID](MongoContext.db, "persons")
    * }}}
    *
    * Second way is to '''override def'''. Be careful __not to change declaration to `val` instead of `def`__.
    *
    * {{{
    * object PersonDao extends BsonDao[Person, BSONObjectID](MongoContext.db, "persons") {
    *
    * override def autoIndexes = Seq(
    *   Index(Seq("name" -> IndexType.Ascending), unique = true, background = true),
    *   Index(Seq("age" -> IndexType.Ascending), background = true))
    * }
    * }}}
    */
  def indices : Traversable[Index] = ???

  /** Ensures indexes defined by `autoIndexes`. */
  def ensureIndices(implicit ec : ExecutionContext) : Future[Traversable[Boolean]] = ???

  def ensureIndicesSync() : Traversable[Boolean] = {
    implicit val ec = coll.executionContext
    Await.result(ensureIndices, 5.seconds)
  }
}
