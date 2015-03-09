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

import akka.util.{ByteStringBuilder, ByteString}
import org.specs2.mutable.Specification

class ByteStringBuilderPimpsSpec extends Specification with ByteStringUtils {

  "ByteStringBuilderPimps" should {
    "have working putCStr" in {
      val builder = ByteString.newBuilder
      val result = builder.putCStr("foo").result()
      val extract = result.iterator.getCStr
      extract must beEqualTo("foo")
    }

    "have working putStr" in {
      val builder = ByteString.newBuilder
      val result = builder.putStr("foo").result()
      val extract = result.iterator.getStr
      extract must beEqualTo("foo")
    }

    "have working putPrefix(TypeCode,String)" in {
      val builder = ByteString.newBuilder
      val result = builder.putPrefix(IntegerCode, "foo").result()
      val i = result.iterator
      val code = i.getByte
      val name = i.getCStr
      code must beEqualTo(16)
      name must beEqualTo("foo")
    }

    "have working putPrefix(Byte,String)" in {
      val builder = ByteString.newBuilder
      val result = builder.putPrefix(16.toByte, "foo").result()
      val i = result.iterator
      val code = i.getByte
      val name = i.getCStr
      code must beEqualTo(16)
      name must beEqualTo("foo")
    }

    "build double correctly" in {
      val data = 42.0D
      val expected : ByteString = {
        val builder = preamble(21, 1, "double")
        builder.putDouble(data) // double value
        builder.putByte(0) // terminating null
        builder.result()
      }
      val builder = ByteString.newBuilder
      builder.double("double", data)
      builder.toByteString must beEqualTo(expected)
    }

    "build string correctly" in {
      val data = "fourty-two"
      val expected : ByteString = {
        val builder = preamble(28, 2, "string")
        val str = data.getBytes(utf8)
        builder.putInt(str.length + 1) // length of string
        builder.putBytes(str) // data string
        builder.putByte(0) // string terminator
        builder.putByte(0) // terminating null
        builder.result()
      }
      val builder = ByteString.newBuilder
      builder.string("string", data)
      builder.toByteString must beEqualTo(expected)
    }

    "build object correctly #1" in {
      val expected = makeObject()
      val bs : ByteString = expected.toByteString
      val builder = ByteString.newBuilder
      builder.obj("obj", expected)
      val container = builder.toBSONObject
      val actual = container.getObj("obj")
      actual.equals(expected) must beTrue
    }

    "build object correctly #2" in {
      val expected = makeObject()
      val bs : ByteString = expected.toByteString
      val builder = ByteString.newBuilder
      builder.obj("obj", bs)
      val container = builder.toBSONObject
      val actual = container.getObj("obj")
      actual.equals(expected) must beTrue
    }

    /*
        @inline def obj(key : String, value : BSONObject) : ByteStringBuilder = {
          putPrefix(ObjectCode, key)
          putObject(value)
        }

        @inline def obj(key : String, value : BSONBuilder) : ByteStringBuilder = {
          putPrefix(ObjectCode, key)
          putDoc(value)
        }

        @inline def anyObj(key : String, value: Iterable[(String,Any)]) : ByteStringBuilder = {
          putPrefix(ObjectCode, key)
          putAnyObject(value)
        }

        @inline def obj[T](key : String, values: Iterable[(String,T)])(implicit codec: Codec[T]) : ByteStringBuilder = {
          putPrefix(ObjectCode, key)
          putObject(values)
        }

        @inline def obj[T](key : String, value : T)(implicit encoder: Encoder[T]) = {
          putPrefix(ObjectCode, key)
          bldr ++= encoder.write(value)
        }

        @inline def obj(key : String, value: ByteString) : ByteStringBuilder = {
          putPrefix(ObjectCode, key)
          bldr ++= value
        }

    */
    "build array correctly" in {
      val data1 = "fourty-two"
      val data2 = 42.0D
      val expected : ByteString = {
        val builder = preamble(46, 4, "array")
        builder.putInt(34)
        builder.putByte(2) // string code
        builder.putBytes("0".getBytes(utf8)) // c string
        builder.putByte(0) // termination of c string
        val str = data1.getBytes(utf8)
        builder.putInt(str.length + 1) // length of string
        builder.putBytes(str) // data string
        builder.putByte(0) // string terminator
        builder.putByte(1) // code
        builder.putBytes("1".getBytes(utf8)) // c string
        builder.putByte(0) // termination of c string
        builder.putDouble(data2) // double value
        builder.putByte(0) // terminating null
        builder.putByte(0) // terminating null
        builder.result()
      }
      val str = BSONString("fourty-two")
      val dbl = BSONDouble(42.0)
      val builder = ByteString.newBuilder
      builder.array("array", data1, data2)
      builder.toByteString must beEqualTo(expected)
    }

    "build binary correctly" in {
      val data = "fourty-two"
      val expected : ByteString = {
        val builder = preamble(28, 5, "binary")
        val str = data.getBytes(utf8)
        builder.putInt(str.length) // length of string
        builder.putByte(0x80.toByte) // user defined code
        builder.putBytes(str) // data string
        builder.putByte(0) // terminating null
        builder.result()
      }
      val builder = ByteString.newBuilder
      builder.binary("binary", data.getBytes(utf8), BinarySubtype.UserDefinedBinary)
      builder.toByteString must beEqualTo(expected)

    }

    "build undefined correctly" in {
      val expected : ByteString = {
        val builder = preamble(16, 6, "undefined")
        builder.putByte(0) // terminating null
        builder.result()
      }
      val builder = ByteString.newBuilder
      builder.undefined("undefined")
      builder.toByteString must beEqualTo(expected)
    }

    "build objectID correctly" in {
      val data = Array[Byte](0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
      val expected : ByteString = {
        val builder = preamble(27, 7, "objectid")
        builder.putBytes(data)
        builder.putByte(0)
        builder.result()
      }
      val builder = ByteString.newBuilder
      builder.objectID("objectid", data)
      builder.toByteString must beEqualTo(expected)
    }

    "build boolean correctly" in {
      val expected : ByteString = {
        val builder = preamble(20, 8, "true")
        builder.putByte(1)
        field(builder, 8, "false")
        builder.putByte(0)
        builder.putByte(0)
        builder.result()
      }
      val builder = ByteString.newBuilder
      builder.boolean("true", value = true)
      builder.boolean("false", value = false)
      builder.toByteString must beEqualTo(expected)
    }

    "build utcDate correctly" in {
      val data = System.currentTimeMillis()
      val expected : ByteString = {
        val builder = preamble(18, 9, "utc")
        builder.putLong(data)
        builder.putByte(0)
        builder.result()
      }
      val builder = ByteString.newBuilder
      builder.date("utc", data)
      builder.toByteString must beEqualTo(expected)
    }

    "build nil correctly" in {
      val expected : ByteString = {
        val builder = preamble(10, 10, "nil")
        builder.putByte(0)
        builder.result()
      }
      val builder = ByteString.newBuilder
      builder.nil("nil")
      builder.toByteString must beEqualTo(expected)
    }

    "build regex correctly" in {
      val expected : ByteString = {
        val builder = preamble(27, 11, "regex")
        cstring(builder, "pattern")
        cstring(builder, "ilmsux")
        builder.putByte(0)
        builder.result()
      }
      val builder = ByteString.newBuilder
      builder.regex("regex", "pattern", "ilmsux")
      builder.toByteString must beEqualTo(expected)
    }

    "build dbPointer correctly" in {
      val data = Array[Byte](0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
      val expected : ByteString = {
        val builder = preamble(41, 12, "dbpointer")
        string(builder, "referent")
        builder.putBytes(data)
        builder.putByte(0)
        builder.result()
      }
      val builder = ByteString.newBuilder
      builder.dbPointer("dbpointer", "referent", data)
      builder.toByteString must beEqualTo(expected)
    }

    "build jsCode correctly" in {
      val code = "function(x) { return 0; };"
      val expected : ByteString = {
        val builder = preamble(code.length + 5 + 8 + 5, 13, "jscode")
        string(builder, code)
        builder.putByte(0)
        builder.result()
      }
      val builder = ByteString.newBuilder
      builder.jsCode("jscode", code)
      builder.toByteString must beEqualTo(expected)
    }

    "build symbol correctly" in {
      val symbol = "symbol"
      val expected : ByteString = {
        val builder = preamble(symbol.length + 5 + 8 + 5, 14, "symbol")
        string(builder, symbol)
        builder.putByte(0)
        builder.result()
      }
      val builder = ByteString.newBuilder
      builder.symbol("symbol", symbol)
      builder.toByteString must beEqualTo(expected)
    }

    "build scopedJsCode correctly" in {
      val code = "function(x) { return 0; };"
      val expected : ByteString = {
        val builder = preamble(4 + 4 + 1 + 12 + 1 + code.length + 5 + 44 + 1, 15, "scopedJsCode")
        builder.putInt(4 + code.length + 5 + 44) // length of scopedJsCode content
        builder.putStr(code)
        builder.putInt(4 + 1 + 7 + 15 + 1 + 7 + 8 + 1) // length of the embedded object
        builder.putByte(2) // string code
        builder.putCStr("string")
        builder.putStr("fourty-two")
        builder.putByte(1) // double code
        builder.putCStr("double")
        builder.putDouble(42.0) // double value
        builder.putByte(0) // embedded object terminating null
        builder.putByte(0) // terminating null
        builder.result()
      }
      val obj = BSONObject(Map("string" -> "fourty-two", "double" -> 42.0))
      val builder2 = ByteString.newBuilder
      builder2.scopedJsCode("scopedJsCode", code, obj)
      builder2.toByteString must beEqualTo(expected)

    }

    "build integer correctly" in {
      val data = 42
      val expected : ByteString = {
        val builder = preamble(4 + 5 + 9, 16, "integer")
        builder.putInt(data)
        builder.putByte(0)
        builder.result()
      }
      val builder = ByteString.newBuilder
      builder.integer("integer", data)
      builder.toByteString must beEqualTo(expected)
    }

    "build timestamp correctly" in {
      val data = System.currentTimeMillis()
      val expected : ByteString = {
        val builder = preamble(4 + 9 + 11, 17, "timestamp")
        builder.putLong(data)
        builder.putByte(0)
        builder.result()
      }
      val builder = ByteString.newBuilder
      builder.timestamp("timestamp", data)
      builder.toByteString must beEqualTo(expected)
    }

    "build long correctly" in {
      val data = 42L
      val expected : ByteString = {
        val builder = preamble(4 + 9 + 6, 18, "long")
        builder.putLong(data)
        builder.putByte(0)
        builder.result()
      }
      val builder = ByteString.newBuilder
      builder.long("long", data)
      builder.toByteString must beEqualTo(expected)
    }
  }}
