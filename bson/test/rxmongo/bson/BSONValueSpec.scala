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

import java.nio.ByteOrder
import java.util.Date
import java.util.regex.Pattern

import akka.util.{ ByteStringBuilder, ByteString }
import org.specs2.mutable.Specification
import rxmongo.bson.BinarySubtype.UserDefinedBinary

/** BSON Document Test Suite */
class BSONValueSpec extends Specification {

  implicit val byteOrder = ByteOrder.LITTLE_ENDIAN

  def cstring(bldr: ByteStringBuilder, str: String): ByteStringBuilder = {
    bldr.putBytes(str.getBytes(utf8)) // c string
    bldr.putByte(0) // termination of c string
    bldr
  }

  def string(bldr: ByteStringBuilder, str: String): ByteStringBuilder = {
    val bytes = str.getBytes(utf8)
    bldr.putInt(bytes.length + 1)
    bldr.putBytes(bytes)
    bldr.putByte(0)
  }

  def field(bldr: ByteStringBuilder, code: Byte, fieldName: String): ByteStringBuilder = {
    bldr.putByte(code) // code
    cstring(bldr, fieldName)
  }

  def preamble(len: Int, code: Byte, fieldName: String): ByteStringBuilder = {
    val bldr: ByteStringBuilder = ByteString.newBuilder
    bldr.putInt(len)
    field(bldr, code, fieldName)
  }

  "BSONObject" should {

    "interpret double correctly" in {
      val data = 42.0D
      val bytes: ByteString = {
        val builder = preamble(17, 1, "double")
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
      BSONDouble.unapply(value.asInstanceOf[BSONDouble]) must beEqualTo(data)
    }

    "interpret string correctly" in {
      val data = "fourty-two"
      val bytes: ByteString = {
        val builder = preamble(24, 2, "string")
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
      val bytes: ByteString = {
        val builder = preamble(5 + 4 + 41 + 1, 3, "obj")
        builder.putInt(8 + 8 + 8 + 15 + 1) // length of object
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
      obj.value must beEqualTo(Map[String, BSONValue]("double" -> BSONDouble(42.0D), "string" -> BSONString("fourty-two")))
    }

    "interpret array correctly" in {
      val bytes: ByteString = {
        val builder = preamble(5 + 4 + 41 + 1, 4, "array")
        builder.putInt(3 + 8 + 3 + 15 + 1) // length of object
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
      obj.value.toSeq must beEqualTo(Seq[BSONValue](BSONDouble(42.0D), BSONString("fourty-two")))
    }

    "interpret binary correctly" in {
      val data = "fourty-two"
      val bytes: ByteString = {
        val builder = preamble(24, 5, "binary")
        val str = data.getBytes(utf8)
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
      arr must beEqualTo(data.getBytes(utf8))
    }

    "interpret undefined correctly" in {
      val bytes: ByteString = {
        val builder = preamble(11 + 1, 6, "undefined")
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONObject(bytes)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, value) = itr.next()
      key must beEqualTo("undefined")
      value.isInstanceOf[BSONUndefined] must beTrue
    }

    "interpret objectID correctly" in {
      val data = Array[Byte](0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
      val bytes: ByteString = {
        val builder = preamble(10 + 12 + 1, 7, "objectID")
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
      val bytes: ByteString = {
        val builder = preamble(6 + 1 + 1, 8, "true")
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
      val bytes: ByteString = {
        val builder = preamble(6 + 8 + 1, 9, "date")
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
      val time = value.asInstanceOf[BSONDate].value
      time must beEqualTo(data)
    }

    "interpret null correctly" in {
      val bytes: ByteString = {
        val builder = preamble(6 + 1, 10, "null")
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONObject(bytes)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, value) = itr.next()
      key must beEqualTo("null")
      value.isInstanceOf[BSONNull] must beTrue
    }

    "interpret regex correctly" in {
      val bytes: ByteString = {
        val builder = preamble(7 + 8 + 6 + 1, 11, "regex")
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
      regex.pattern.pattern must beEqualTo("(?imsUx)pattern")
    }

    "interpret dbpointer correctly" in {
      val data = Array[Byte](0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
      val bytes: ByteString = {
        val builder = preamble(11 + 13 + 12 + 1, 12, "dbpointer")
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
      val bytes: ByteString = {
        val builder = preamble(8 + data.length + 5 + 1, 13, "jscode")
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
      val bytes: ByteString = {
        val builder = preamble(8 + 11 + 1, 14, "symbol")
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
      val bytes: ByteString = {
        val builder = preamble(14 + 4 + code.length + 5 + 4 + 40 + 1, 15, "scopedjscode")
        builder.putInt(code.length + 5 + 4 + 40)
        string(builder, code)
        builder.putInt(8 + 8 + 8 + 15 + 1) // length of object
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
      obj.value must beEqualTo(Map("double" -> BSONDouble(42.0D), "string" -> BSONString("fourty-two")))
    }

    "interpret integer correctly" in {
      val data = 42
      val bytes: ByteString = {
        val builder = preamble(5 + 4 + 1, 16, "int")
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
      val bytes: ByteString = {
        val builder = preamble(11 + 8 + 1, 17, "timestamp")
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
      val bytes: ByteString = {
        val builder = preamble(6 + 8 + 1, 18, "long")
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
  }
}
