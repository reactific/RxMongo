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

import rxmongo.bson.{ Query, BSONBuilder, BSONObject }
import rxmongo.driver.{ Projection, Command }

/** planCacheListFilters
  * Lists the index filters for a collection.
  * @param db The database containing the collection for which the index filters will be listed.
  * @param coll The name of the collection.
  */
case class PlanCacheListFiltersCmd(
  db : String,
  coll : String) extends Command(db, BSONObject("planCacheListFilters" → coll))

/** planCacheSetFilter
  * Sets an index filter for a collection.
  * @see [[http://docs.mongodb.org/master/reference/command/planCacheSetFilter/]]
  * @param db The database containing the collection to set the filter on
  * @param coll The name of the collection on which to set the filter
  * @param query The query predicate associated with the index filter. Together with the sort and the projection, the
  *        query predicate make up the query shape for the specified index filter.  Only the structure of the
  *        predicate, including the field names, are significant; the values in the query predicate are
  *        insignificant. As such, query predicates cover similar queries that differ only in the values.
  * @param sort Optional. The sort associated with the filter. Together with the query and the projection, the sort
  *       make up the query shape for the specified index filter.
  * @param projection Optional. The projection associated with the filter. Together with the query and the sort,
  *             the projection make up the query shape for the specified index filter.
  * @param indexes An array of index specification documents that act as the index filter for the specified query
  *          shape. Because the query optimizer chooses among the collection scan and these indexes, if the
  *          indexes are non-existent, the optimizer will choose the collection scan.
  */
case class PlanCacheSetFilterCmd(
  db : String,
  coll : String,
  query : Query,
  sort : Option[BSONObject] = None,
  projection : Option[Projection] = None,
  indexes : Seq[String] = Seq.empty[String]) extends Command(db, {
  val b = BSONBuilder()
  b.string("planCacheSetFilter", coll)
  b.obj("query", query.result)
  sort.map { s ⇒ b.obj("sort", s) }
  projection.map { p ⇒ b.obj("projection", p.result) }
  b.array("indexes", indexes)
  b.result
})

/** planCacheClearFilters
  * Clears index filter(s) for a collection.
  * @see [[http://docs.mongodb.org/master/reference/command/planCacheClearFilters/]]
  * @param db The name of the database
  * @param coll The name of the collection
  * @param query Optional. The query predicate associated with the filter to remove. If omitted, clears all filters
  *        from the collection. The values in the query predicate are insignificant in determining the query
  *        shape, so the values used in the query need not match the values shown using planCacheListFilters.
  * @param sort Optional. The sort associated with the filter to remove, if any.
  * @param projection Optional. The projection associated with the filter to remove, if any.
  */
case class PlanCacheClearFiltersCmd(
  db : String,
  coll : String,
  query : Option[Query] = None,
  sort : Option[BSONObject] = None,
  projection : Option[Projection] = None) extends Command(db, {
  val b = BSONBuilder()
  b.string("planCacheClearFilters", coll)
  query.map { q ⇒ b.obj("query", q.result) }
  sort.map { s ⇒ b.obj("sort", s) }
  projection.map { p ⇒ b.obj("projection", p.result) }
  b.result
})

/** planCacheListQueryShapes
  * Displays the query shapes for which cached query plans exist.
  * @see [[http://docs.mongodb.org/master/reference/command/planCacheListQueryShapes/]]
  * @param db The name of the database
  * @param coll The name of the collection
  */
case class PlanCacheListQueryShapesCmd(
  db : String,
  coll : String) extends Command(db, BSONObject("planCacheListQueryShapes" → coll))

/** planCacheListPlans
  * Displays the cached query plans for the specified query shape.
  * @see [[http://docs.mongodb.org/master/reference/command/planCacheListPlans/]]
  * @param db The name of the database towards which the command should be directed.
  * @param coll The name of the collection which will have have plans listed
  * @param query The query predicate of the query shape. Only the structure of the predicate, including the field
  *          names, are significant to the shape; the values in the query predicate are insignificant.
  * @param sort Optional. The sort associated with the query shape.
  * @param projection Optional. The projection associated with the query shape.
  */
case class PlanCacheListPlansCmd(
  db : String,
  coll : String,
  query : Query,
  sort : Option[BSONObject] = None,
  projection : Option[Projection] = None) extends Command(db, {
  val b = BSONBuilder()
  b.string("planCacheListPlans", coll)
  b.obj("query", query.result)
  sort.map { s ⇒ b.obj("sort", s) }
  projection.map { p ⇒ b.obj("projection", p.result) }
  b.result
})

/** planCacheClear
  * Removes cached query plan(s) for a collection.
  * @see [[http://docs.mongodb.org/master/reference/command/planCacheClear/]]
  * @param db The name of the database.
  * @param coll The name of the collection.
  * @param query Optional. The query predicate of the query shape. Only the structure of the predicate, including
  *        the field names, are significant to the shape; the values in the query predicate are insignificant.
  * @param sort Optional. The projection associated with the query shape.
  * @param projection Optional. The sort associated with the query shape.
  */
case class PlanCacheClearCmd(
  db : String,
  coll : String,
  query : Option[Query] = None,
  sort : Option[BSONObject] = None,
  projection : Option[Projection] = None) extends Command(db, {
  val b = BSONBuilder()
  b.string("planCacheClear", coll)
  query.map { q ⇒ b.obj("query", q.result) }
  sort.map { s ⇒ b.obj("sort", s) }
  projection.map { p ⇒ b.obj("projection", p.result) }
  b.result
})
