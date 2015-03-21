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

import rxmongo.bson.BSONObject
import rxmongo.messages.replies.WriteResult

import scala.concurrent.Await
import scala.concurrent.duration.FiniteDuration

/** Test Suite For Collection Statistics  */
class CollStatsSpec extends RxMongoTest("rxmongo", "collstats") {

  val objs = Seq(
    BSONObject("a" → 21.0, "b" → 21L, "c" → 21),
    BSONObject("a" → 28.0, "b" → 28L, "c" → 28),
    BSONObject("a" → 35.0, "b" → 35L, "c" → 35),
    BSONObject("a" → 42.0, "b" → 42L, "c" → 42),
    BSONObject("a" → 49.0, "b" → 49L, "c" → 49),
    BSONObject("a" → 56.0, "b" → 56L, "c" → 56)
  )

  "Collection" should {
    "drop collection before populating" in mongoTest { () ⇒
      val result = Await.result(collection.drop(), FiniteDuration(1, "seconds"))
      success
    }

    "populate collection before testing" in mongoTest { () ⇒
      val future = collection.insert(objs)
      val result = Await.result(future, FiniteDuration(1, "seconds"))
      result.ok must beEqualTo(1)
      result.n must beEqualTo(6)
    }

    "permit collection stats retrieval" in mongoTest { () ⇒
      val stats = collection.stats
      stats.count must beEqualTo(6)
      collection.count must beEqualTo(stats.count)
    }

    "implmenet avgObjSize" in mongoTest { () ⇒
      val size = collection.avgObjSize
      size must beGreaterThan(26)
    }

    "implement capped" in mongoTest { () ⇒
      val capped = collection.capped
      capped must beFalse
    }

    "implement storageSize" in mongoTest { () ⇒
      val size = collection.storageSize
      size must beGreaterThan(26 * 6L)
    }

    "implement indexSize" in mongoTest { () ⇒
      val size = collection.indexSize
      size must beEqualTo(7168L)
    }

    "implement numIndexes" in mongoTest { () ⇒
      val size = collection.numIndexes
      size must beEqualTo(1)
    }

    "implement numExtents" in mongoTest { () ⇒
      val size = collection.numExtents
      size must beEqualTo(1)
    }

  }

}
