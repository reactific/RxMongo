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
import java.util.regex.Pattern

import akka.util.ByteString
import com.reactific.hsp.Profiler
import org.specs2.mutable.Specification
import rxmongo.bson.BinarySubtype.UserDefinedBinary

class BSONDocumentSpec extends Specification with ByteStringUtils {

  "BSONDocument" should {
    "construct empty" in {
      val doc = BSONObject()
      val itr = doc.iterator
      itr.hasNext must beFalse
    }

    "interpret double from bytes correctly" in {
      val data = 42.0D
      val bytes : ByteString = {
        val builder = preamble(21, 1, "double")
        builder.putDouble(data) // double value
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONDocument(bytes)
      doc.asDouble("double") must beEqualTo(data)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, (typeCode, byteIterator)) = itr.next()
      key must beEqualTo("double")
      typeCode must beEqualTo(DoubleCode.code)
      byteIterator.getDouble must beEqualTo(data)
    }

    "interpret string correctly" in {
      val data = "fourty-two"
      val bytes : ByteString = {
        val builder = preamble(28, 2, "string")
        string(builder, data)
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONDocument(bytes)
      doc.asString("string") must beEqualTo(data)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, (typeCode, byteIterator)) = itr.next()
      key must beEqualTo("string")
      typeCode must beEqualTo(StringCode.code)
      byteIterator.getStr must beEqualTo(data)
    }

    "interpret object correctly" in {
      val data = Map[String, BSONValue]("double" -> BSONDouble(42.0D), "string" -> BSONString("fourty-two"))
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
      val doc = BSONDocument(bytes)
      doc.asObject("obj").toMap must beEqualTo(data)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, (typeCode, byteIterator)) = itr.next()
      key must beEqualTo("obj")
      typeCode must beEqualTo(ObjectCode.code)
      val obj = byteIterator.getObject
      obj.isInstanceOf[BSONObject] must beTrue
      obj.toMap must beEqualTo(data)
    }

    "interpret array correctly" in {
      val data = Seq[BSONValue](BSONDouble(42.0D), BSONString("fourty-two"))
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
      val doc = BSONDocument(bytes)
      doc.asArray("array").seq must beEqualTo(data)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, (typeCode, byteIterator)) = itr.next()
      key must beEqualTo("array")
      typeCode must beEqualTo(ArrayCode.code)
      val arr = byteIterator.getArray
      arr.seq must beEqualTo(data)
    }

    "interpret binary correctly" in {
      val data = "fourty-two"
      val bytes : ByteString = {
        val builder = preamble(28, 5, "binary")
        val str = data.getBytes(utf8)
        builder.putInt(str.length) // length of string
        builder.putByte(0x80.toByte) // user defined code
        builder.putBytes(str) // data string
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONDocument(bytes)
      val (subtype1, array1) = doc.asBinary("binary")
      subtype1 must beEqualTo(UserDefinedBinary)
      array1 must beEqualTo(data.getBytes(utf8))
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, (typeCode, byteIterator)) = itr.next()
      key must beEqualTo("binary")
      typeCode must beEqualTo(BinaryCode.code)
      val (subtype2, array2) = byteIterator.getBinary
      subtype2 must beEqualTo(UserDefinedBinary)
      array2 must beEqualTo(data.getBytes(utf8))
    }

    "interpret undefined correctly" in {
      val bytes : ByteString = {
        val builder = preamble(15 + 1, 6, "undefined")
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONDocument(bytes)
      doc.asUndefined("undefined")
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, (typeCode, byteIterator)) = itr.next()
      key must beEqualTo("undefined")
      typeCode must beEqualTo(UndefinedCode.code)
      byteIterator.isEmpty must beTrue
    }

    "interpret objectID correctly" in {
      val data = Array[Byte](0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
      val bytes : ByteString = {
        val builder = preamble(14 + 12 + 1, 7, "objectID")
        builder.putBytes(data)
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONDocument(bytes)
      doc.asObjectID("objectID") must beEqualTo(data)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, (typeCode, byteIterator)) = itr.next()
      key must beEqualTo("objectID")
      typeCode must beEqualTo(ObjectIDCode.code)
      val objid = byteIterator.getBytes(12)
      objid must beEqualTo(data)
    }

    "interpret boolean correctly" in {
      val bytes : ByteString = {
        val builder = preamble(10 + 1 + 1, 8, "true")
        builder.putByte(1)
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONDocument(bytes)
      doc.asBoolean("true") must beEqualTo(true)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, (typeCode, byteIterator)) = itr.next()
      key must beEqualTo("true")
      typeCode must beEqualTo(BooleanCode.code)
      val b = byteIterator.getByte
      b must beEqualTo(1)
    }

    "interpret date correctly" in {
      val data = System.currentTimeMillis()
      val bytes : ByteString = {
        val builder = preamble(10 + 8 + 1, 9, "date")
        builder.putLong(data)
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONDocument(bytes)
      doc.asDate("date") must beEqualTo(new Date(data))
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, (typeCode, byteIterator)) = itr.next()
      key must beEqualTo("date")
      typeCode must beEqualTo(DateCode.code)
      byteIterator.getLong must beEqualTo(data)
    }

    "interpret null correctly" in {
      val bytes : ByteString = {
        val builder = preamble(10 + 1, 10, "null")
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONDocument(bytes)
      doc.asNull("null")
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, (typeCode, byteIterator)) = itr.next()
      key must beEqualTo("null")
      typeCode must beEqualTo(NullCode.code)
      byteIterator.isEmpty must beTrue
    }

    "interpret regex correctly" in {
      val bytes : ByteString = {
        val builder = preamble(11 + 8 + 6 + 1, 11, "regex")
        cstring(builder, "pattern")
        cstring(builder, "imsux")
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONDocument(bytes)
      doc.asRegex("regex").pattern must beEqualTo("pattern")
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, (typeCode, byteIterator)) = itr.next()
      key must beEqualTo("regex")
      typeCode must beEqualTo(RegexCode.code)
      val regex = byteIterator.getRegex
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
      val doc = BSONDocument(bytes)
      val (referent1, objid1) = doc.asDBPointer("dbpointer")
      referent1 must beEqualTo("referent")
      objid1 must beEqualTo(data)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, (typeCode, byteIterator)) = itr.next()
      key must beEqualTo("dbpointer")
      typeCode must beEqualTo(DBPointerCode.code)
      val (referent, objid) = byteIterator.getDBPointer
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
      val doc = BSONDocument(bytes)
      doc.asJavaScript("jscode") must beEqualTo(data)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, (typeCode, byteIterator)) = itr.next()
      key must beEqualTo("jscode")
      typeCode must beEqualTo(JavaScriptCode.code)
      val jsc = byteIterator.getStr
      jsc must beEqualTo(data)
    }

    "interpret symbol correctly" in {
      val data = "symbol"
      val bytes : ByteString = {
        val builder = preamble(12 + 11 + 1, 14, "symbol")
        string(builder, data)
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONDocument(bytes)
      doc.asSymbol("symbol") must beEqualTo(data)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, (typeCode, byteIterator)) = itr.next()
      key must beEqualTo("symbol")
      typeCode must beEqualTo(SymbolCode.code)
      val sym = byteIterator.getStr
      sym must beEqualTo(data)
    }

    "interpret scopedjscode correctly" in {
      val data = Map("double" -> BSONDouble(42.0D), "string" -> BSONString("fourty-two"))
      val code = "function(x) { return x + 1; };"
      val bytes : ByteString = {
        val builder = preamble(18 + 4 + code.length + 5 + 4 + 40 + 1, 15, "scopedjscode")
        builder.putInt(code.length + 5 + 4 + 44) // length of the string and object
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
      val doc = BSONDocument(bytes)
      val (js1, obj1) = doc.asScopedJavaScript("scopedjscode")
      js1 must beEqualTo(code)
      obj1.toMap must beEqualTo(data)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, (typeCode, byteIterator)) = itr.next()
      key must beEqualTo("scopedjscode")
      typeCode must beEqualTo(ScopedJSCode.code)
      val (js, obj) = byteIterator.getScopedJavaScript
      js must beEqualTo(code)
      obj.toMap must beEqualTo(data)
    }

    "interpret integer correctly" in {
      val data = 42
      val bytes : ByteString = {
        val builder = preamble(9 + 4 + 1, 16, "int")
        builder.putInt(data)
        builder.putByte(0) // terminating null
        builder.result()
      }
      val doc = BSONDocument(bytes)
      doc.asInt("int") must beEqualTo(data)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, (typeCode, byteIterator)) = itr.next()
      key must beEqualTo("int")
      typeCode must beEqualTo(IntegerCode.code)
      val int = byteIterator.getInt
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
      val doc = BSONDocument(bytes)
      doc.asTimestamp("timestamp") must beEqualTo(data)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, (typeCode, byteIterator)) = itr.next()
      key must beEqualTo("timestamp")
      typeCode must beEqualTo(TimestampCode.code)
      val ts = byteIterator.getLong
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
      val doc = BSONDocument(bytes)
      doc.asLong("long") must beEqualTo(data)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, (typeCode, byteIterator)) = itr.next()
      key must beEqualTo("long")
      typeCode must beEqualTo(LongCode.code)
      val long = byteIterator.getLong
      long must beEqualTo(data)
    }

    "construct from Map" in {
      pending
    }

    "support equality of empty" in {
      val obj1 = BSONDocument.empty
      val obj2 = BSONDocument()
      obj1 must beEqualTo(obj2)
    }

    "support equality of complex" in {
      val obj1 : BSONDocument = makeDocument
      val obj2 : BSONDocument = makeDocument
      obj1.equals(obj2) must beTrue
    }

    "interpret documents from ByteString quickly" in {
      val bs : ByteString = makeAnObject.toByteString
      val repetitions = 100000
      val limit = 80000.0 * repetitions
      val profiler = timedTest(limit, "ByteString Interpretation", { profiler : Profiler ⇒
        val docs = profiler.profile("Construction") {
          for (i ← 1 to 100000) yield {
            BSONDocument(bs)
          }
        }
        profiler.profile("Validation") {
          for (doc ← docs) {
            doc.asDouble("double") must beEqualTo(42.0D)
            doc.asBoolean("boolean") must beEqualTo(true)
            doc.asDate("date").getTime must beEqualTo(aTime)
            doc.asSymbol("symbol") must beEqualTo("symbol")
            doc.asLong("long") must beEqualTo(42L)
            doc.asInt("integer") must beEqualTo(42)
            doc.asString("string") must beEqualTo("fourty-two")
            doc.asTimestamp("timestamp") must beEqualTo(42L)
            doc.asObject("obj") must beEqualTo(anObject)
          }
        }
      })
      val construction = profiler.get_one_item("Construction")._2
      construction / repetitions must beLessThan(50000.0)
    }
  }

  def makeDocument : BSONDocument = BSONDocument(makeAnObject.toByteString)

}
