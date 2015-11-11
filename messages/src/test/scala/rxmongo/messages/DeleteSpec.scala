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

class DeleteSpec extends Specification {

  "Delete" should {
    "construction from boolean expression" in {
      Delete("a" $ne "b", 1) must beEqualTo(Delete(BSONObject("a" → BSONObject("$ne" -> "b")), 1))
    }
    "construct from Query" in {
      Delete(Query("a" $ne "b"), 1) must beEqualTo(Delete(
        BSONObject("$query" → BSONObject("a" → BSONObject("$ne" -> "b"))), 1))
    }
    "produce correct BSONObject" in {
      val bs = Delete.Codec.write(Delete("a" $ne "b", 1))
      val obj = BSONObject(bs)
      obj must beEqualTo(BSONObject("q" → BSONObject("a" → BSONObject("$ne" -> "b")), "limit" → 1)
      )
    }
  }

}
