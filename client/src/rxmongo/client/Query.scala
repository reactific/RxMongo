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

import rxmongo.bson._

/** Represent a MongoDB Query
  *
  * A query is built
  */
trait Query extends BSONProvider {
  val bson = BSONBuilder()
  def toByteString = bson.toByteString

  def sort(how: BSONObject) : Query = {
    bson.obj("$orderby", how)
    this
  }
  def sortBy(field: String, ascending: Boolean = true) = {
    sort(BSONObject(field → (if(ascending) 1 else -1)))
  }
  def sortWith( f: () ⇒ BSONObject) = sort(f())

}

trait Selector extends Query {

  override def toByteString = {
    val content = super.result
    val result = BSONBuilder()
    result.obj("$query", content)
    result.toByteString
  }
}

object Selector {
  def apply() = ???
}

object Query {

  def apply(): Query = ???


  implicit class QueryModifiers(q: Query) {
  }

  implicit class LimitQueryModifier(q: Query) {}

  implicit class CommentQueryModifier(q: Query) {
    def comment(msg: String) = {
      q.bson.string("$comment", msg)
    }
  }
}
