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

import akka.util.{ByteStringBuilder, ByteString}
import org.specs2.mutable.Specification
import rxmongo.bson.BinarySubtype.UserDefinedBinary

/** BSON Document Test Suite */
class BSONValueSpec extends Specification {

  implicit val byteOrder = ByteOrder.LITTLE_ENDIAN

  def cstring(bldr: ByteStringBuilder, str: String) : ByteStringBuilder = {
    bldr.putBytes(str.getBytes(utf8)) // c string
    bldr.putByte(0) // termination of c string
    bldr
  }

  def string(bldr: ByteStringBuilder, str: String) : ByteStringBuilder = {
    val bytes = str.getBytes(utf8)
    bldr.putInt(bytes.length+1)
    bldr.putBytes(bytes)
    bldr.putByte(0)
  }

  def field(bldr: ByteStringBuilder, code: Byte, fieldName: String) : ByteStringBuilder = {
    bldr.putByte(code) // code
    cstring(bldr, fieldName)
  }

  def preamble(len: Int, code: Byte, fieldName: String) : ByteStringBuilder = {
    val bldr: ByteStringBuilder = ByteString.newBuilder
    bldr.putInt(len)
    field(bldr, code, fieldName)
  }

  "BSONObject" should {

    "interpret double correctly" in {
      val data = 42.0D
      val bytes : ByteString = {
        val builder = preamble(17, 1, "double")
        builder.putDouble(data)   // double value
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
      val bytes : ByteString = {
        val builder = preamble(5+4+41+1, 3, "obj")
        builder.putInt(8+8+8+15+1) // length of object
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
      obj.value must beEqualTo(Map[String,BSONValue]("double" -> BSONDouble(42.0D), "string" -> BSONString("fourty-two")))
    }

    "interpret array correctly" in {
      val bytes : ByteString = {
        val builder = preamble(5+4+41+1, 4, "array")
        builder.putInt(3+8+3+15+1) // length of object
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
      val bytes : ByteString = {
        val builder = ByteString.newBuilder
        builder.putInt(24)        // length
        builder.putByte(5.toByte) // string code
        builder.putBytes("binary".getBytes(utf8)) // c string
        builder.putByte(0.toByte) // termination of c string
        val str = data.getBytes(utf8)
        builder.putInt(str.length) // length of string
        builder.putByte(0x80.toByte) // user defined code
        builder.putBytes(str)   // data string
        builder.putByte(0.toByte) // terminating null
        builder.result()
      }
      val doc = BSONObject(bytes)
      val itr = doc.iterator
      itr.hasNext must beTrue
      val (key, value) = itr.next()
      key must beEqualTo("binary")
      val (subtype,arr) = value.value
      subtype must beEqualTo(UserDefinedBinary)
      arr must beEqualTo(data.getBytes(utf8))
    }
  }
}
