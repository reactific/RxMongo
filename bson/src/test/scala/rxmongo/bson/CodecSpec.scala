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

import java.time.Instant
import java.util
import java.util.Date
import java.util.regex.Pattern

import akka.util.ByteString
import org.specs2.mutable.Specification

/** Test Cases For Codec */
class CodecSpec extends Specification with ByteStringTestUtils {

  "Codec" should {
    "reflect Double" in {
      import Codec.DoubleCodec._
      val bldr = BSONBuilder()
      read(write(42.0,bldr).bldr.result().iterator) must beEqualTo(42.0)
    }
    "reflect String" in {
      import Codec.StringCodec._
      val bldr = BSONBuilder()
      read(write("foo",bldr).bldr.result().iterator) must beEqualTo("foo")
    }
    "reflect BSONObject" in {
      import Codec.BSONObjectCodec._
      val bldr = BSONBuilder()
      val obj = makeObject()
      read(write(obj,bldr).bldr.result().iterator) must beEqualTo(obj)
    }
    "reflect BSONArray" in {
      import Codec.BSONArrayCodec._
      val bldr = BSONBuilder()
      val array = BSONArray.fromAny(Seq(1,2L,"3",4.0,false))
      val result = read(write(array,bldr).bldr.result().iterator)
      result.equals(array) must beTrue
    }
    "reflect Binary" in {
      import Codec.BinaryCodec._
      val bldr = BSONBuilder()
      val binary = (BinarySubtype.GenericBinary, Array[Byte](1,2))
      val result = read(write(binary,bldr).bldr.result().iterator)
      result._1 must beEqualTo(binary._1)
      util.Arrays.equals(result._2, binary._2)
    }
    "reflect BSONBinary" in {
      import Codec.BSONBinaryCodec._
      val bldr = BSONBuilder()
      val binary = BSONBinary(Array[Byte](1,2), BinarySubtype.GenericBinary)
      read(write(binary,bldr).bldr.result().iterator) must beEqualTo(binary)
    }
    "reflect ObjectID" in {
      import Codec.BSONObjectIDCodec._
      val bldr = BSONBuilder()
      val objid = BSONObjectID(Array[Byte](1,2,3,4,5,6,7,8,9,10,11,12))
      read(write(objid,bldr).bldr.result().iterator) must beEqualTo(objid)
    }
    "reflect Boolean" in {
      import Codec.BooleanCodec._
      val bldr = BSONBuilder()
      read(write(false,bldr).bldr.result().iterator) must beEqualTo(false)
    }
    "reflect Date" in {
      import Codec.DateCodec._
      val bldr = BSONBuilder()
      val now = new Date()
      read(write(now,bldr).bldr.result().iterator) must beEqualTo(now)
    }
    "reflect Regex" in {
      import Codec.RegexCodec._
      val bldr = BSONBuilder()
      val pattern : Pattern = ".*".r.pattern
      val result = read(write(pattern,bldr).bldr.result().iterator)
      result.pattern.equals(pattern.pattern) must beTrue
    }
    "reflect DBPointer" in {
      import Codec.DBPointerCodec._
      val bldr = BSONBuilder()
      val dbp = BSONDBPointer("foo",Array[Byte](0,1,2,3,4,5,6,7,8,9,10,11))
      val result = read(write(dbp,bldr).bldr.result().iterator)
      result.equals(dbp) must beTrue
    }
    "reflect JavaScript" in {
      import Codec.JavaScriptCodec._
      val bldr = BSONBuilder()
      read(write("foo",bldr).bldr.result().iterator) must beEqualTo("foo")
    }
    "reflect Symbol" in {
      import Codec.SymbolCodec._
      val bldr = BSONBuilder()
      read(write("foo",bldr).bldr.result().iterator) must beEqualTo("foo")
    }
    "reflect ScopedJavaScript" in {
      import Codec.ScopedJavaScriptCodec._
      val bldr = BSONBuilder()
      val sjs = ("foo", makeObject())
      val result = read(write(sjs,bldr).bldr.result().iterator)
      result.equals(sjs) must beTrue
    }
    "reflect Int" in {
      import Codec.IntCodec._
      val bldr = BSONBuilder()
      read(write(42,bldr).bldr.result().iterator) must beEqualTo(42)
    }
    "reflect Instant" in {
      import Codec.InstantCodec._
      val bldr = BSONBuilder()
      val now = Instant.now()
      read(write(now,bldr).bldr.result().iterator) must beEqualTo(now)
    }
    "reflect Long" in {
      import Codec.LongCodec._
      val bldr = BSONBuilder()
      read(write(42L,bldr).bldr.result().iterator) must beEqualTo(42L)
    }
    "reflect ByteString" in {
      import Codec.ByteStringCodec._
      val bldr = BSONBuilder()
      val bs = ByteString.newBuilder.putByte(1).putByte(2).putByte(3).result()
      val result = read(write(bs,bldr).bldr.result().iterator)
      result.equals(bs) must beTrue
    }
  }
}
