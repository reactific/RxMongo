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

import rxmongo.bson.{ BSONBuilder, BSONProvider, Query }

/** Represents A MongoDB Query Projection
  *
  * These objects may be passed to the [[rxmongo.client.Collection]].find method to limit what is returned in
  * the query. Typically fields are simply included or excluded, but mongo supports array slicing and array
  * element matches as well.
  */
class Projection extends BSONProvider {

  private val builder = BSONBuilder()

  def toByteString = builder.toByteString

  /** Include named fields in the query results
    *
    * @param names The names of the fields to include
    * @return A projection with fields included
    */
  def include(names : String*) : Projection = {
    for (name ← names) {
      builder.integer(name, 1)
    }
    this
  }

  /** Exclude named fields from the query results
    *
    * @param names The names of the field to exclude
    * @return A Projection with fields excluded
    */
  def exclude(names : String*) : Projection = {
    for (name ← names) {
      builder.integer(name, 0)
    }
    this
  }

  /** Project a slice of an array, from the start
    *
    * Returns the first `count` elements of the array named `name`
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/projection/slice/]]
    * @param name The name of the array field to slice
    * @param count The number of elements of the array to return, from the start
    * @return
    */
  def slice(name : String, count : Int) : Projection = {
    builder.obj(name, BSONBuilder().integer("$slice", count))
    this
  }

  /** Project a slice of an array, from the start.
    *
    * This is just a synonym for the two argument `slice` method.
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/projection/slice/]]
    * @param name The name of the array field to slice
    * @param count The number of elements of the array to return, from the start
    * @return A projection that returns the first `count` elements of the array named `name`
    */
  def sliceFromStart(name : String, count : Int) : Projection = slice(name, count)

  /** Project a slice of an array, from the end.
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/projection/slice/]]
    * @param name The name of the array field to slice
    * @param count The number of elements of the array to return, from the end
    * @return A projection that returns the last `count` elements of the array named `name`
    */
  def sliceFromEnd(name : String, count : Int) : Projection = {
    builder.obj(name, BSONBuilder().integer("$slice", -count))
    this
  }

  /** Project a slice of an array, from the start
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/projection/slice/]]
    * @param name The name of the array field to slice
    * @param skip The number of elements of the array to skip, from the start
    * @param count The number of elements of the array to return, from the skip point
    * @return A projection that returns the `count` elements of the array named `name` starting at
    * `skip` elements from the start
    */
  def slice(name : String, skip : Int, count : Int) : Projection = {
    builder.obj(name, BSONBuilder().array("$slice", skip, count))
    this
  }

  /** Project a slice of an array, from the start
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/projection/slice/]]
    * @param name The name of the array field to slice
    * @param skip The number of elements of the array to skip, from the start
    * @param count The number of elements of the array to return, from the skip point
    * @return A projection that returns the `count` elements of the array named `name` starting at
    * `skip` elements from the start
    */
  def sliceFromStart(name : String, skip : Int, count : Int) : Projection = slice(name, skip, count)

  /** Project a slice of an array, from the end
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/projection/slice/]]
    * @param name The name of the array field to slice
    * @param skip The number of elements of the array to skip, from the end
    * @param count The number of elements of the array to return, from the skip point
    * @return A projection that returns `count` elements of the array named `name` starting from
    * `skip` elements from the end.
    */
  def sliceFromEnd(name : String, skip : Int, count : Int) : Projection = {
    builder.obj(name, BSONBuilder().array("$slice", -skip, count))
    this
  }

  /** Include the meta textScore field
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/projection/meta/]]
    * @param name The name of the field for the textScore meta value
    * @return
    */
  def metaTextScore(name : String) : Projection = {
    builder.obj(name, BSONBuilder().string("$meta", "textScore"))
    this
  }

  /** Return the first array element that matches a query
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/projection/elemMatch/]]
    * @param name The name of the array field to match
    * @param query The query to select which elements of the array to return
    * @return A projection with the element match included.
    */
  def elemMatch(name : String, query : Query) : Projection = {
    builder.obj(name, BSONBuilder().obj("$elemMatch", query.result))
    this
  }

  /** Do a positional slicing of an array
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/projection/positional/]]
    * @param name The name of the array field
    * @return A projection with the positional slicing included.
    */
  def positional(name : String) : Projection = {
    builder.integer(name + ".$", 1)
    this
  }
}

object Projection {

  def apply() : Projection = new Projection

  /** Projection Constructor for excluding fields
    *
    * This creates a projection that excludes the named fields in the query result. If nothing else is added to
    * the returned project, all other fields will be included.
    *
    * @see [[http://docs.mongodb.org/master/core/read-operations-introduction/]]
    * @param names
    * @return
    */
  def except(names : String*) : Projection = {
    val result = Projection()
    for (name ← names) { result.builder.integer(name, 0) }
    result
  }

  /** Projection constructor for including only some fields
    *
    * This creates a projection that includes only specifically named fields in the query result. Normally the
    * _id field is returned by default but this constructor will specifically exclude it.
    *
    * @see [[http://docs.mongodb.org/master/core/read-operations-introduction/]]
    * @param names The names of the fields to include
    * @return A Projection that causes the query to only return certain fields.
    */
  def only(names : String*) : Projection = {
    val result = Projection()
    for (name ← names) { result.builder.integer(name, 1) }
    result.builder.integer("_id", 0)
    result
  }

  /** Projection constructor for specifically including and excluding fields
    *
    * This creates a projection that includes some fields and excludes others. The pairs of (String,Boolean) indicate
    * the fields and their disposition, true means included the field, false means exclude the field.
    *
    * @see [[http://docs.mongodb.org/master/core/read-operations-introduction/]]
    * @param names
    * @return
    */
  def specific(names : (String, Boolean)*) : Projection = {
    val result = Projection()
    for ((name, include) ← names) { result.builder.integer(name, if (include) 1 else 0) }
    result
  }

}
