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

import org.specs2.mutable.Specification
import rxmongo.bson.BSONObject

class ProjectionSpec extends Specification {

  "Projection" should {
    "construct from except" in {
      val p1 = Projection.except("one")
      p1.result must beEqualTo(BSONObject("one" → 0))
      val p2 = Projection.except("one", "two")
      p2.result must beEqualTo(BSONObject("one" → 0, "two" → 0))
    }

    "construct from only" in {
      val p1 = Projection.only("one")
      p1.result must beEqualTo(BSONObject("one" → 1, "_id" → 0))
      val p2 = Projection.only("one", "two")
      p2.result must beEqualTo(BSONObject("one" → 1, "two" → 1, "_id" → 0))
    }
    "construct from specific" in {
      val p1 = Projection.specific("one" → true)
      p1.result must beEqualTo(BSONObject("one" → 1))
      val p2 = Projection.specific("one" → true, "two" → false)
      p2.result must beEqualTo(BSONObject("one" → 1, "two" → 0))
    }
    "allow includes" in {
      val p1 = Projection()
      p1.result must beEqualTo(BSONObject())
      p1.include("a", "b", "c")
      p1.result must beEqualTo(BSONObject("a" → 1, "b" → 1, "c" → 1))

    }
    "allow excludes" in {
      val p1 = Projection()
      p1.result must beEqualTo(BSONObject())
      p1.exclude("a", "b", "c")
      p1.result must beEqualTo(BSONObject("a" → 0, "b" → 0, "c" → 0))
    }
    "allow slice" in {
      pending
    }
    "allow sliceFromStart" in {
      pending
    }
    "allow sliceFromEnd" in {
      pending
    }
    "allow metaTextSearch" in {
      pending
    }
    "allow elemMatch" in {
      pending
    }
    "allow positional" in {
      pending
    }
  }
}
