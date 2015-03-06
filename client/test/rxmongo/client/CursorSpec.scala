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
import rxmongo.messages._
import rxmongo.driver.{ WriteResult, QueryOptions }

import scala.concurrent.Await
import scala.concurrent.duration.FiniteDuration

class CursorSpec extends RxMongoTest("rxmongo", "cursor") {

  val objs = Seq(
    BSONObject("a" → 21.0, "b" → 21L, "c" → 21),
    BSONObject("a" → 28.0, "b" → 28L, "c" → 28),
    BSONObject("a" → 35.0, "b" → 35L, "c" → 35),
    BSONObject("a" → 42.0, "b" → 42L, "c" → 42),
    BSONObject("a" → 49.0, "b" → 49L, "c" → 49),
    BSONObject("a" → 56.0, "b" → 56L, "c" → 56)
  )

  "Cursor" should {
    "drop collection before populating" in mongoTest { () ⇒
      val result = Await.result(collection.drop(), FiniteDuration(1, "seconds"))
      success
    }

    "insert 6 records to test" in mongoTest { () ⇒
      val future = collection.insert(objs) map { doc ⇒ WriteResult(doc) }
      val result = Await.result(future, FiniteDuration(1, "seconds"))
      result.ok must beEqualTo(1)
      result.n must beEqualTo(6)
    }

    "find all 6 documents" in mongoTest { () ⇒
      val result = Await.result(collection.find(Query("a" $gte 21.0),
        options = QueryOptions(numberToReturn = 10)), FiniteDuration(1, "seconds"))
      result.hasNext must beEqualTo(true)
      val contents = Await.result(result.toFlatSeq, FiniteDuration(1, "seconds"))
      contents.length must beEqualTo(6)
      contents.exists(obj ⇒ obj.matches(BSONObject("a" → 21.0, "b" → 21L, "c" → 21))) must beTrue
      contents.exists(obj ⇒ obj.matches(BSONObject("a" → 28.0, "b" → 28L, "c" → 28))) must beTrue
      contents.exists(obj ⇒ obj.matches(BSONObject("a" → 35.0, "b" → 35L, "c" → 35))) must beTrue
      contents.exists(obj ⇒ obj.matches(BSONObject("a" → 42.0, "b" → 42L, "c" → 42))) must beTrue
      contents.exists(obj ⇒ obj.matches(BSONObject("a" → 49.0, "b" → 49L, "c" → 49))) must beTrue
      contents.exists(obj ⇒ obj.matches(BSONObject("a" → 56.0, "b" → 56L, "c" → 56))) must beTrue
    }
  }
}
