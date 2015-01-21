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

import java.util

import org.specs2.mutable.Specification
import rxmongo.bson.BinarySubtype.UserDefinedBinary

/** Test Suite For BSON object */
class BSONSpec extends Specification {

  "BSON" should {
    "build and interpret reflectively" in {
      val data = Array[Byte](0,1,2,3,4,5,6,7,8,9,10,11,12)
      val b = Builder()
      b.double("double", 42.0D)
      b.string("string", "fourty-two")
      val anObject = BSONObject("one" -> BSONDouble(84.0D), "two" -> BSONString("eighty-four"))
      b.obj("obj", anObject )
      b.array("array", Seq(BSONDouble(42.0D), BSONString("fourty-two")))
      b.binary("binary", data, UserDefinedBinary)

      val buffer = b.result

      val bsonObject = BSONObject(buffer)

      val map = bsonObject.value

      val double = map.get("double")
      double.isDefined must beTrue

      val string = map.get("string")
      string.isDefined must beTrue

      val obj = map.get("obj")
      obj.isDefined must beTrue

      val array = map.get("array")
      array.isDefined must beTrue

      val binary = map.get("binary")
      binary.isDefined must beTrue

      double.get.value must beEqualTo(42.0D)
      string.get.value must beEqualTo("fourty-two")
      obj.get.value.asInstanceOf[Map[String,BSONValue]] must beEqualTo(
        Map("one" -> BSONDouble(84.0D), "two" -> BSONString("eighty-four"))
      )
      array.get.value.asInstanceOf[Iterator[BSONValue]].toSeq must beEqualTo(
        Seq(BSONDouble(42.0D), BSONString("fourty-two"))
      )
      val pair = binary.get.value.asInstanceOf[(BinarySubtype, Array[Byte])]

      pair._1 must beEqualTo(UserDefinedBinary)

      pair._2 must beEqualTo(data)
    }
  }
}
