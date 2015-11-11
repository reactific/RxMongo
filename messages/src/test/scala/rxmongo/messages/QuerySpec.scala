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

class QuerySpec extends Specification with ByteStringTestUtils {

  "Query" should {
    "construct valid byte string" in { pending }
    /** TODO: Validate Query against byte string
      * val q = Query("a" $eq "b")
      * val embedded : ByteString
      * val bytes : ByteString = {
      * val builder = preamble(21, 1, "$query")
      * builder.putDouble(data) // double value
      * builder.putByte(0) // terminating null
      * builder.result()
      * }
      *
      * }
      */

    "accept a simple selector in constructor" in {
      val q = Query("a" $eq "b")
      q.result must beEqualTo(BSONObject("$query" → BSONObject("a" → "b")))
    }

    "accept a complex selector" in {
      val q = Query((("a" $ne 42.0) $and ("b" $gte 21.0)) $or ("c" $lt 84.0))
      q.result must beEqualTo(BSONObject(
        "$query" → BSONObject("$or" →
          BSONArray(
            BSONObject("$and" →
              BSONArray(
                BSONObject("a" → BSONObject("$ne" → 42.0)),
                BSONObject("b" → BSONObject("$gte" → 21.0))
              )
            ),
            BSONObject("c" → BSONObject("$lt" → 84.0))
          )
        )
      ))
    }

    "handle ascending sorting on one field" in {
      val q = Query("a" $eq "b").orderBy("c")
      q.result must beEqualTo(BSONObject("$query" → BSONObject("a" → "b"), "$orderby" → BSONObject("c" → 1)))
    }

    "handle descending sorting on one field" in {
      val q = Query("a" $eq "b").orderBy("c", ascending = false)
      q.result must beEqualTo(BSONObject("$query" → BSONObject("a" → "b"), "$orderby" → BSONObject("c" → -1)))
    }

    "handle adding a comment" in {
      val q = Query("a" $eq "b").comment("This is for profiling")
      q.result must beEqualTo(BSONObject("$query" → BSONObject("a" → "b"), "$comment" → "This is for profiling"))
    }

    "handle adding an index hint" in {
      val q = Query("a" $eq "b").hint("_id")
      q.result must beEqualTo(BSONObject("$query" → BSONObject("a" → "b"), "$hint" → BSONObject("_id" → 1)))

    }

    "handle maxScan(num_docs)" in {
      pending
    }

    "handle maxTimeMS(millis)" in {
      pending
    }

    "handle max(fields)" in {
      pending
    }
    "handle min(fields)" in {
      pending
    }

    "handle returnKey()" in {
      pending
    }

    "handle showDiskLoc()" in {
      pending
    }

    "handle snapshot()" in {
      pending
    }

    "handle natural()" in {
      pending
    }

    "handle complex query construction" in {
      val q = Query("a" $eq "b").
        comment("foo").
        hint("a").
        maxScan(1000).
        maxTimeMS(1000).
        max("count" → 10).
        min("count" → 1).
        orderBy("count").
        returnKey().
        showDiskLoc().
        snapshot().
        natural()
      val actual = q.result
      val expected = BSONObject(
        "$query" → BSONObject("a" → "b"),
        "$comment" → "foo",
        "$hint" → BSONObject("a" → 1),
        "$maxScan" → 1000,
        "$maxTimeMS" → 1000L,
        "$max" → BSONObject("count" → 10),
        "$min" → BSONObject("count" → 1),
        "$orderby" → BSONObject("count" → 1),
        "$returnKey" → true,
        "$showDiskLoc" → true,
        "$snapshot" → true,
        "$natural" → 1
      )

      actual must beEqualTo(expected)
    }
  }
}
