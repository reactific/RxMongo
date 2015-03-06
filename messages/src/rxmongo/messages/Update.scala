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

case class Update(selector : BSONObject, updater : BSONObject, upsert : Boolean, multi : Boolean, isolated : Boolean)

object Update {
  def apply(selector : BooleanExpression, updater : UpdateExpression, upsert : Boolean, multi : Boolean, isolated : Boolean) : Update = {
    Update(selector.result, updater.result, upsert, multi, isolated)
  }

  def apply(selector : Query, updater : UpdateExpression, upsert : Boolean, multi : Boolean, isolated : Boolean) : Update = {
    Update(selector.result, updater.result, upsert, multi, isolated)
  }

  implicit object Codec extends BSONCodec[Update, BSONObject] {
    def code : TypeCode = ObjectCode

    def write(value : Update) : BSONObject = {
      val selector = if (value.isolated) {
        val bldr = BSONBuilder()
        value.selector.doc.addTo(bldr)
        bldr.integer("$isolated", 1)
        bldr.result
      } else value.selector
      BSONBuilder().
        obj("q", selector).
        obj("u", value.updater).
        boolean("upsert", value.upsert).
        boolean("multi", value.multi).
        result
    }

    def read(value : BSONObject) : Update = {
      val q = value.getObj("q")
      val (selector, isolated) = {
        if (q.contains("$isolated"))
          (q - "$isolated") → true
        else
          q → false
      }
      Update(selector, value.getObj("u"), value.getAsBoolean("upsert"), value.getAsBoolean("multi"), isolated)
    }
  }
}
