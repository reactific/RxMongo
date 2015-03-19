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

/** Test Case For Indices module */
class IndicesSpec extends Specification {

  "WireTigerConfig" should {
    "construct from an object" in {
      val wtc = WiredTigerConfig(BSONObject("config" -> "value"))
      wtc.result must beEqualTo(BSONObject("WiredTiger" -> BSONObject("config" -> "value")))
    }

  }

  "MMapV1Config" should {
    "construct from an object" in {
      val wtc = MMapV1Config(BSONObject("config" -> "value"))
      wtc.result must beEqualTo(BSONObject("mmapv1" -> BSONObject("config" -> "value")))
    }
  }

  "IndexOptions" should {
    "construct with all default arguments" in {
      val io = IndexOptions()
      io.result must beEqualTo(BSONObject())
    }
    "construct with some arguments defaulted" in {
      val io = IndexOptions(sparse = Some(true), unique = Some(false))
      io.result must beEqualTo(BSONObject("unique" -> false, "sparse" -> true))
    }
    "construct with all arguments specified" in {
      val io = IndexOptions(Some(true), Some(false), Some(true), Some("name"), Some(10),
        Some(MMapV1Config(BSONObject("a" -> "b"))),
        Seq("foo" -> 1, "bar" -> 10), Some("en"), Some("lang"),
        bits = Some(1), min = Some(1.0), max = Some(100.0), bucketSize = Some(32.0))
      io.result must beEqualTo(BSONObject(
        "unique" -> true, "sparse" -> false, "background" -> true, "name" -> "name",
        "expireAfterSeconds" -> 10, "storageEngine" -> BSONObject("mmapv1" -> BSONObject("a" -> "b")),
        "weights" -> BSONObject("foo" -> 1, "bar" -> 10), "default_language" -> "en",
        "language_override" -> "lang", "bits" -> 1, "min" -> 1.0, "max" -> 100.0,
        "bucketSize" -> 32.0
      ))
    }
  }

  "Index" should {
    "construct with a simple field" in {
      val i = Index("name", ascending = true)
      i.result must beEqualTo(BSONObject("name" -> 1))
    }
  }

  "CompoundIndex" should {
    "construct with a sequence of pairs" in {
      val ci = CompoundIndex("name" -> true, "score" -> false)
      ci.result must beEqualTo(BSONObject("name" -> 1, "score" -> -1))
    }
  }

  "TextIndex" should {
    "construct with a sequence of field names" in {
      val ti = TextIndex("a", "b", "c")
      ti.result must beEqualTo(BSONObject("a" -> "text", "b" -> "text", "c" -> "text"))
    }
  }

  "HashedIndex" should {
    "construct with a single field name" in {
      val hi = HashedIndex("a")
      hi.result must beEqualTo(BSONObject("a" -> "hashed"))
    }
  }

  "TwoDIndex" should {
    "construct with a single location field" in {
      val twodi = TwoDIndex("a")
      twodi.result must beEqualTo(BSONObject("a" -> "2d"))
    }
    "construct with a location and other fields" in {
      val twodi = TwoDIndex("a", "b" -> true, "c" -> false)
      twodi.result must beEqualTo(BSONObject("a" -> "2d", "b" -> 1, "c" -> -1))
    }
  }

  "TwoDSphereIndex" should {
    "construct with a single location field" in {
      val twodsi = TwoDSphereIndex(locationField = "a")
      twodsi.result must beEqualTo(BSONObject("a" -> "2dsphere"))
    }
    "construct with a prefix and a location" in {
      val twodsi = TwoDSphereIndex(Seq("a" -> true), "b")
      twodsi.result must beEqualTo(BSONObject("a" -> 1, "b" -> "2dsphere"))
    }
    "construct wiht a prefix, location and suffix" in {
      val twodsi = TwoDSphereIndex(Seq("a" -> true), "b", Seq("c" -> false))
      twodsi.result must beEqualTo(BSONObject("a" -> 1, "b" -> "2dsphere", "c" -> -1))
    }
  }
  "GeoHaystackIndex" should {
    "construct with a single location field" in {
      val gh = GeoHaystackIndex("a")
      gh.result must beEqualTo(BSONObject("a" -> "geoHaystack"))
    }
    "construct with a location and other fields" in {
      val gh = GeoHaystackIndex("a", "b" -> true, "c" -> false)
      gh.result must beEqualTo(BSONObject("a" -> "geoHaystack", "b" -> 1, "c" -> -1))
    }
  }
}
