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

package rxmongo.bson

import java.util.Date

import org.specs2.mutable.Specification

import Expression._

class OperatorsSpec extends Specification {

  val foo_str = "foo" $eq "bar"
  val foo_str_equiv = BSONObject("foo" -> "bar")
  val foo_bool = "foo" $ne false
  val foo_bool_equiv = BSONObject("foo" -> BSONObject("$ne" -> false))
  val foo_int = "foo" $gt 23
  val foo_int_equiv = BSONObject("foo" -> BSONObject("$gt" -> 23))
  val foo_date = "foo" $lt new Date(42)
  val foo_date_equiv = BSONObject("foo" -> BSONObject("$lt" -> new Date(42)))
  val foo_dbl = "foo" $gte 42.0
  val foo_dbl_equiv = BSONObject("foo" -> BSONObject("$gte" -> 42.0))
  val foo_null = "foo" $lte Seq("bar")
  val foo_null_equiv = BSONObject("foo" -> BSONObject("$lte" -> BSONArray("bar")))

  "Operators" should {
    "construct $eq properly" in {
      foo_str.result() must beEqualTo (foo_str_equiv)
    }

    "construct $ne properly" in {
      foo_bool.result() must beEqualTo(foo_bool_equiv)
    }

    "construct $gt properly" in {
      foo_int.result() must beEqualTo(foo_int_equiv)
    }

    "construct $lt properly" in {
      foo_date.result() must beEqualTo(foo_date_equiv)
    }

    "construct $gte properly" in {
      foo_dbl.result() must beEqualTo(foo_dbl_equiv)
    }

    "construct $lte properly" in {
      foo_null.result() must beEqualTo(foo_null_equiv)
    }

    "construct $in properly" in {
      val obj = "foo" $in ("bar", false, 23, new Date(42), 42.0)
      obj.result must beEqualTo(BSONObject("foo" -> BSONObject("$in" ->
        BSONArray("bar", false, 23, new Date(42), 42.0)
      )))
    }
    "construct $nin properly" in {
      val obj = "foo" $nin ("bar", false, 23, new Date(42), 42.0)
      obj.result must beEqualTo(BSONObject("foo" -> BSONObject("$nin" ->
        BSONArray("bar", false, 23, new Date(42), 42.0)
      )))
    }

    "construct infix $and properly" in {
      val obj = foo_str $and foo_int
      obj.result() must beEqualTo(BSONObject("$and" -> BSONArray(foo_str_equiv, foo_int_equiv)))
    }
    "construct prefix $and properly" in {
      val obj = $and(foo_str, foo_bool, foo_int, foo_date, foo_dbl)
      obj.result must beEqualTo(BSONObject("$and" -> BSONArray(
        foo_str_equiv, foo_bool_equiv, foo_int_equiv, foo_date_equiv, foo_dbl_equiv
      )))
    }
  }
}
