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

package rxmongo.messages

import rxmongo.bson._

case class Delete(query : BSONObject, limit : Int)

object Delete {

  def apply(selector : BooleanExpression, limit : Int) : Delete = {
    Delete(selector.result, limit)
  }

  def apply(query : Query, limit : Int = 0) : Delete = {
    Delete(query.result, limit)
  }

  implicit object Codec extends BSONCodec[Delete, BSONObject] {
    def code : TypeCode = ObjectCode
    def write(value : Delete) : BSONObject = {
      BSONBuilder().
        obj("q", value.query).
        integer("limit", value.limit).
        result
    }
    def read(value : BSONObject) : Delete = {
      Delete(value.getObj("q"), value.getAsInt("limit"))
    }
  }
}

