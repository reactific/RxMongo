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
import rxmongo.driver.{ IndexOptions, Index }

import scala.concurrent.Await
import scala.concurrent.duration.FiniteDuration

class CollectionSpec extends RxMongoTest("rxmongo", "collection") {

  val obj1 = BSONObject("key1" → 42.0, "key2" → 42L, "key3" → 42)

  val atMost = FiniteDuration(2, "seconds")

  "Collection" should {

    "insert" in mongoTest { () ⇒
      val result = Await.result(collection.insertOne(obj1), atMost)
      result.ok must beEqualTo(1)
      result.n must beEqualTo(1)
    }

    "create index" in mongoTest { () ⇒
      val index = Index("key1", ascending = true)
      val options = IndexOptions()
      val result = Await.result(collection.createIndex(index, options), atMost)
      result.getAsDouble("ok") must beEqualTo(1.0)
    }

    "find" in mongoTest { () ⇒
      val result = Await.result(collection.findOne(Query("key1" $eq 42.0)), atMost)
      result.hasNext must beEqualTo(true)
      val obj = Await.result(result.next(), atMost)
      obj.contains("key1") must beTrue
      obj.contains("key2") must beTrue
      obj.contains("key3") must beTrue
      obj.getAsDouble("key1") must beEqualTo(42.0)
      obj.getAsLong("key2") must beEqualTo(42L)
      obj.getAsInt("key3") must beEqualTo(42)
    }

    "update" in mongoTest { () ⇒
      val upd = Update("key1" → 42.0, $set("key2", 84L), upsert = false, multi = false, isolated = false)
      val result = Await.result(collection.updateOne(upd), atMost)
      result.ok must beEqualTo(1)
      result.n must beEqualTo(1)
    }

    "delete" in mongoTest { () ⇒
      val del = Delete("key1" → 42.0, limit = 1)
      val result = Await.result(collection.deleteOne(del), atMost)
      result.ok must beEqualTo(1)
      result.n must beEqualTo(1)
    }

    "rename" in mongoTest { () ⇒
      val result = Await.result(collection.renameCollection("collection2", dropTarget = true), atMost)
      result.isEmpty must beFalse
      collection = result.get
      collection.name must beEqualTo("collection2")
    }

    "drop" in mongoTest { () ⇒
      val result = Await.result(collection.drop(), atMost)
      result must beTrue
    }
  }

}
