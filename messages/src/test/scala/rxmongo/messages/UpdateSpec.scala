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

import org.specs2.mutable.Specification

import rxmongo.bson._

class UpdateSpec extends Specification {

  "Update" should {
    "construct from BooleanExpression" in {
      Update("a" $ne "b", $set("foo" → "bar"), upsert = true, multi = false, isolated = false) must beEqualTo(
        Update(BSONObject("a" → BSONObject("$ne" → "b")), BSONObject("$set" → BSONObject("foo" → "bar")), true, false, false)
      )
    }

    "construct from Query" in {
      val actual = Update(Query("a" $ne "b"), $set("foo" → "bar"), upsert = true, multi = false, isolated = false)
      val expected = Update(BSONObject("$query" → BSONObject("a" → BSONObject("$ne" → "b"))),
        BSONObject("$set" → BSONObject("foo" → "bar")), upsert = true, multi = false, isolated = false)
      actual must beEqualTo(expected)
    }

    "produce correct BSONObject" in {
      val bs = Update.Codec.write(Update("a" $ne "b", $set("foo" → "bar"), upsert = true, multi = false, isolated = true))
      val obj = BSONObject(bs)
      obj must beEqualTo(
        BSONObject(
          "q" → BSONObject("a" → BSONObject("$ne" → "b"), "$isolated" → 1),
          "u" → BSONObject("$set" → BSONObject("foo" → "bar")),
          "upsert" → true,
          "multi" → false
        )
      )
    }
  }

}
