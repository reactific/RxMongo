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

package rxmongo.bson

/** Represent a MongoDB Query
  *
  * A query is built
  */
case class Query() extends BSONProvider {
  private val bson = BSONBuilder()
  def toByteString = bson.toByteString

  def select(what : BooleanExpression) : Query = {
    bson.obj("$query", what)
    this
  }

  def comment(msg : String) : Query = {
    bson.string("$comment", msg)
    this
  }

  /** Forces MongoDB to use a specific index. See hint()
    * @see [[http://docs.mongodb.org/master/reference/operator/meta/hint/]]
    * @param index
    * @return
    */
  def hint(index : String) : Query = {
    bson.obj("$hint", Map(index → 1))
    this
  }

  /** Limits the number of documents scanned.
    * @see [[http://docs.mongodb.org/master/reference/operator/meta/maxScan/]]
    * @param num_docs
    * @return
    */
  def maxScan(num_docs : Int) : Query = {
    bson.integer("$maxScan", num_docs)
    this
  }

  /** Specifies a cumulative time limit in milliseconds for processing operations on a cursor.
    * @see [[http://docs.mongodb.org/master/reference/operator/meta/maxTimeMS/]]
    * @param millis
    * @return
    */
  def maxTimeMS(millis : Long) : Query = {
    bson.long("$maxTimeMS", millis)
    this
  }

  /** Specifies an exclusive upper limit for the index to use in a query.
    * @see [[http://docs.mongodb.org/master/reference/operator/meta/max/]]
    * @param fields
    * @return
    */
  def max(fields : (String, Any)*) : Query = {
    bson.obj("$max", fields.head, fields.tail : _*)
    this
  }

  /** Specifies an inclusive lower limit for the index to use in a query.
    * @see [[http://docs.mongodb.org/master/reference/operator/meta/min/]]
    * @param fields
    * @return
    */
  def min(fields : (String, Any)*) : Query = {
    bson.obj("$min", fields.head, fields.tail : _*)
    this
  }

  /** Returns a cursor with documents sorted according to a sort specification.
    * @see [[http://docs.mongodb.org/master/reference/operator/meta/orderby/]]
    * @param field
    * @param ascending
    * @return
    */
  def orderBy(field : String, ascending : Boolean = true) : Query = {
    bson.obj("$orderby", Map(field → (if (ascending) 1 else -1)))
    this
  }

  /** Forces the cursor to only return fields included in the index.
    * @see [[http://docs.mongodb.org/master/reference/operator/meta/returnKey/]]
    * @return
    */
  def returnKey() : Query = {
    bson.boolean("$returnKey", value = true)
    this
  }

  /** Modifies the documents returned to include references to the on-disk location of each document.
    * @see [[http://docs.mongodb.org/master/reference/operator/meta/showDiskLoc/]]
    * @return
    */
  def showDiskLoc() : Query = {
    bson.boolean("$showDiskLoc", value = true)
    this
  }

  /** Forces the query to use the index on the _id field.
    * @see [[http://docs.mongodb.org/master/reference/operator/meta/snapshot/]]
    * @return
    */
  def snapshot() : Query = {
    bson.boolean("$snapshot", value = true)
    this
  }

  /** A special sort order that orders documents using the order of documents on disk.
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/meta/natural/]]
    * @param reverse
    * @return
    */
  def natural(reverse : Boolean = false) : Query = {
    bson.integer("$natural", if (reverse) -1 else 1)
    this
  }

}

object Query {
  def apply(what : BooleanExpression) : Query = new Query().select(what)

  implicit class LimitQueryModifier(q : Query) {}

}
