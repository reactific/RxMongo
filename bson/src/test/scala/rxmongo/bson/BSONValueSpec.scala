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

import java.nio.charset.StandardCharsets
import java.util.regex.Pattern

import akka.util.{ ByteStringBuilder, ByteIterator, ByteString }
import org.specs2.mutable.Specification
import rxmongo.bson.BinarySubtype.UserDefinedBinary

/** BSON Document Test Suite */
class BSONValueSpec extends Specification with ByteStringTestUtils {

  "BSONObject" should {
    "construct empty" in {
      val doc = BSONObject()
      val itr = doc.iterator
      itr.hasNext must beFalse
    }

    "interpret double correctly" in {
      val data = 42.0D
      val bytes : ByteString = {
        val builder = preamble(21, 1, "double")
        builder.putDouble(data) // double value
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONObject(bytes)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, value) = itr.next()
      key must beEqualTo("double")
      value.value must beEqualTo(data)
    }

    "interpret string correctly" in {
      val data = "fourty-two"
      val bytes : ByteString = {
        val builder = preamble(28, 2, "string")
        string(builder, data)
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONObject(bytes)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, value) = itr.next()
      key must beEqualTo("string")
      value.value must beEqualTo(data)
    }

    "interpret object correctly" in {
      val bytes : ByteString = {
        val builder = preamble(9 + 4 + 41 + 1, 3, "obj")
        builder.putInt(12 + 8 + 8 + 15 + 1) // length of object
        field(builder, 1, "double")
        builder.putDouble(42.0D)
        field(builder, 2, "string")
        string(builder, "fourty-two")
        builder.putByte(0) // terminating null of embedded object
        builder.putByte(0) // terminating null of outer object
        builder.result()
      }
      val doc = BSONObject(bytes)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, value) = itr.next()
      key must beEqualTo("obj")
      value.isInstanceOf[BSONObject] must beTrue
      val obj = value.asInstanceOf[BSONObject]
      obj.toMap must beEqualTo(Map[String, BSONValue]("double" -> BSONDouble(42.0D), "string" -> BSONString("fourty-two")))
    }

    "interpret array correctly" in {
      val bytes : ByteString = {
        val builder = preamble(9 + 4 + 41 + 1, 4, "array")
        builder.putInt(7 + 8 + 3 + 15 + 1) // length of object
        field(builder, 1, "0")
        builder.putDouble(42.0D)
        field(builder, 2, "1")
        string(builder, "fourty-two")
        builder.putByte(0) // terminating null of embedded object
        builder.putByte(0) // terminating null of outer object
        builder.result()
      }
      val doc = BSONObject(bytes)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, value) = itr.next()
      key must beEqualTo("array")
      value.isInstanceOf[BSONArray] must beTrue
      val obj = value.asInstanceOf[BSONArray]
      obj.seq must beEqualTo(Seq[BSONValue](BSONDouble(42.0D), BSONString("fourty-two")))
    }

    "interpret binary correctly" in {
      val data = "fourty-two"
      val bytes : ByteString = {
        val builder = preamble(28, 5, "binary")
        val str = data.getBytes(StandardCharsets.UTF_8)
        builder.putInt(str.length) // length of string
        builder.putByte(0x80.toByte) // user defined code
        builder.putBytes(str) // data string
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONObject(bytes)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, value) = itr.next()
      key must beEqualTo("binary")
      val (subtype, arr) = value.value
      subtype must beEqualTo(UserDefinedBinary)
      arr must beEqualTo(data.getBytes(StandardCharsets.UTF_8))
    }

    "interpret undefined correctly" in {
      val bytes : ByteString = {
        val builder = preamble(15 + 1, 6, "undefined")
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONObject(bytes)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, value) = itr.next()
      key must beEqualTo("undefined")
      value.isInstanceOf[BSONUndefined.type] must beTrue
    }

    "interpret objectID correctly" in {
      val data = Array[Byte](0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
      val bytes : ByteString = {
        val builder = preamble(14 + 12 + 1, 7, "objectID")
        builder.putBytes(data)
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONObject(bytes)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, value) = itr.next()
      key must beEqualTo("objectID")
      value.isInstanceOf[BSONObjectID] must beTrue
      val objid = value.asInstanceOf[BSONObjectID].value
      objid must beEqualTo(data)
    }

    "interpret boolean correctly" in {
      val bytes : ByteString = {
        val builder = preamble(10 + 1 + 1, 8, "true")
        builder.putByte(1)
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONObject(bytes)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, value) = itr.next()
      key must beEqualTo("true")
      value.isInstanceOf[BSONBoolean] must beTrue
      val b = value.asInstanceOf[BSONBoolean].value
      b must beEqualTo(true)
    }

    "interpret date correctly" in {
      val data = System.currentTimeMillis()
      val bytes : ByteString = {
        val builder = preamble(10 + 8 + 1, 9, "date")
        builder.putLong(data)
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONObject(bytes)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, value) = itr.next()
      key must beEqualTo("date")
      value.isInstanceOf[BSONDate] must beTrue
      val time = value.asInstanceOf[BSONDate].value.getTime
      time must beEqualTo(data)
    }

    "interpret null correctly" in {
      val bytes : ByteString = {
        val builder = preamble(10 + 1, 10, "null")
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONObject(bytes)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, value) = itr.next()
      key must beEqualTo("null")
      value.isInstanceOf[BSONNull.type] must beTrue
    }

    "interpret regex correctly" in {
      val bytes : ByteString = {
        val builder = preamble(11 + 8 + 6 + 1, 11, "regex")
        cstring(builder, "pattern")
        cstring(builder, "imsux")
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONObject(bytes)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, value) = itr.next()
      key must beEqualTo("regex")
      value.isInstanceOf[BSONRegex] must beTrue
      val regex = value.asInstanceOf[BSONRegex].value
      regex.pattern must beEqualTo("pattern")
      val ALL_FLAGS : Int = Pattern.CASE_INSENSITIVE |
        Pattern.MULTILINE |
        Pattern.DOTALL |
        Pattern.UNICODE_CHARACTER_CLASS |
        Pattern.UNICODE_CASE |
        Pattern.COMMENTS
      regex.flags must beEqualTo(ALL_FLAGS)
    }

    "interpret dbpointer correctly" in {
      val data = Array[Byte](0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
      val bytes : ByteString = {
        val builder = preamble(15 + 13 + 12 + 1, 12, "dbpointer")
        string(builder, "referent")
        builder.putBytes(data)
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONObject(bytes)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, value) = itr.next()
      key must beEqualTo("dbpointer")
      value.isInstanceOf[BSONDBPointer] must beTrue
      val (referent, objid) = value.asInstanceOf[BSONDBPointer].value
      referent must beEqualTo("referent")
      objid must beEqualTo(data)
    }

    "interpret javascriptcode correctly" in {
      val data = "function(x) { return x + 1; };"
      val bytes : ByteString = {
        val builder = preamble(12 + data.length + 5 + 1, 13, "jscode")
        string(builder, data)
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONObject(bytes)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, value) = itr.next()
      key must beEqualTo("jscode")
      value.isInstanceOf[BSONJsCode] must beTrue
      val jsc = value.asInstanceOf[BSONJsCode].value
      jsc must beEqualTo(data)
    }

    "interpret symbol correctly" in {
      val bytes : ByteString = {
        val builder = preamble(12 + 11 + 1, 14, "symbol")
        string(builder, "symbol")
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONObject(bytes)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, value) = itr.next()
      key must beEqualTo("symbol")
      value.isInstanceOf[BSONSymbol] must beTrue
      val sym = value.asInstanceOf[BSONSymbol].value
      sym must beEqualTo("symbol")
    }

    "interpret scopedjscode correctly" in {
      val code = "function(x) { return x + 1; };"
      val bytes : ByteString = {
        val builder = preamble(18 + 4 + code.length + 5 + 4 + 40 + 1, 15, "scopedjscode")
        builder.putInt(code.length + 5 + 4 + 44) // length of the string
        string(builder, code)
        builder.putInt(12 + 8 + 8 + 15 + 1) // length of object
        field(builder, 1, "double")
        builder.putDouble(42.0D)
        field(builder, 2, "string")
        string(builder, "fourty-two")
        builder.putByte(0) // terminating null of embedded object
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONObject(bytes)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, value) = itr.next()
      key must beEqualTo("scopedjscode")
      value.isInstanceOf[BSONScopedJsCode] must beTrue
      val (js, obj) = value.asInstanceOf[BSONScopedJsCode].value
      js must beEqualTo(code)
      obj.toMap must beEqualTo(Map("double" -> BSONDouble(42.0D), "string" -> BSONString("fourty-two")))
    }

    "interpret integer correctly" in {
      val data = 42
      val bytes : ByteString = {
        val builder = preamble(9 + 4 + 1, 16, "int")
        builder.putInt(data)
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONObject(bytes)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, value) = itr.next()
      key must beEqualTo("int")
      value.isInstanceOf[BSONInteger] must beTrue
      val int = value.asInstanceOf[BSONInteger].value
      int must beEqualTo(data)
    }

    "interpret timestamp correctly" in {
      val data = System.currentTimeMillis()
      val bytes : ByteString = {
        val builder = preamble(15 + 8 + 1, 17, "timestamp")
        builder.putLong(data)
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONObject(bytes)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, value) = itr.next()
      key must beEqualTo("timestamp")
      value.isInstanceOf[BSONTimestamp] must beTrue
      val ts = value.asInstanceOf[BSONTimestamp].value
      ts must beEqualTo(data)
    }

    "interpret long correctly" in {
      val data = 42L
      val bytes : ByteString = {
        val builder = preamble(10 + 8 + 1, 18, "long")
        builder.putLong(data)
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONObject(bytes)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, value) = itr.next()
      key must beEqualTo("long")
      value.isInstanceOf[BSONLong] must beTrue
      val long = value.asInstanceOf[BSONLong].value
      long must beEqualTo(data)
    }

    "throw on getAsString with non-string value" in {
      val obj = BSONObject("str" -> "fourty-two", "num" -> 42.0)
      obj.getAsString("str") must beEqualTo("fourty-two")
      obj.getAsString("num") must throwA[IllegalArgumentException]
    }

    "throw on getAsInt with non-Int value" in {
      val obj = BSONObject("str" -> "fourty-two", "num" -> 42)
      obj.getAsInt("num") must beEqualTo(42)
      obj.getAsInt("str") must throwA[IllegalArgumentException]
    }

    "throw on getAsDouble with non-Double value" in {
      val obj = BSONObject("str" -> "fourty-two", "num" -> 42.0)
      obj.getAsDouble("num") must beEqualTo(42.0)
      obj.getAsDouble("str") must throwA[IllegalArgumentException]
    }

    "construct from a Map" in {
      val embedded : ByteString = {
        val builder = preamble(4 + 3 + 5, 16, "a")
        builder.putInt(1)
        builder.putByte(0)
        builder.result()
      }
      val expected : ByteString = {
        val builder = preamble(embedded.length + 6 + 5, 3, "$inc")
        builder ++= embedded
        builder.putByte(0)
        builder.result()
      }

      val obj = BSONObject("$inc" → BSONObject("a" → 1))
      val actual = obj.toByteString
      actual must beEqualTo(expected)
    }

    "find a user implemented Codec" in {
      case class UserDefined(val1 : String, val2 : Double)
      implicit object UserDefinedCodec extends Codec[UserDefined] {
        override val code = ObjectCode
        def read(itr : ByteIterator) : UserDefined = {
          val doc = BSONDocument(itr)
          UserDefined(doc.asString("val1"), doc.asDouble("val2"))
        }
        def write(value : UserDefined, bldr : BSONBuilder) : BSONBuilder = {
          bldr.string("val1", value.val1)
          bldr.double("val2", value.val2)
          bldr
        }
      }
      val obj = UserDefined("val1", 42.0)
      val obj2 = BSONObject("obj", obj)
      obj2.getAsObject[UserDefined]("obj") must beEqualTo(obj)
    }

    "support equality of empty" in {
      val obj1 = BSONObject()
      val obj2 = BSONObject()
      obj1 must beEqualTo(obj2)
    }

    "support equality of complex" in {
      val obj1 = BSONObject(
        "$query" → BSONObject("a" → "b"),
        "$comment" → "foo",
        "$hint" → BSONObject("a" → 1),
        "$maxScan" → 1000,
        "$maxTimeMS" → 1000,
        "$max" → BSONObject("count" → 10),
        "$min" → BSONObject("count" → 1),
        "$orderby" → BSONObject("count" → 1),
        "$returnKey" → true,
        "$showDiskLoc" → true,
        "$snapshot" → true,
        "$natural" → 1
      )
      val obj2 = BSONObject(
        "$query" → BSONObject("a" → "b"),
        "$comment" → "foo",
        "$hint" → BSONObject("a" → 1),
        "$maxScan" → 1000,
        "$maxTimeMS" → 1000,
        "$max" → BSONObject("count" → 10),
        "$min" → BSONObject("count" → 1),
        "$orderby" → BSONObject("count" → 1),
        "$returnKey" → true,
        "$showDiskLoc" → true,
        "$snapshot" → true,
        "$natural" → 1
      )
      obj1 must beEqualTo(obj2)

    }
  }
}
