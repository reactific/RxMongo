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

import akka.util.ByteString
import org.specs2.mutable.Specification

/** Title Of Thing.
  *
  * Description of thing
  */
class BuilderSpec extends Specification {

  "Builder" should {
    "build double correctly" in {
      val data = 42.0D
      val expected : ByteString = {
        implicit val byteOrder = ByteOrder.LITTLE_ENDIAN
        val builder = ByteString.newBuilder
        builder.putInt(17)        // length
        builder.putByte(1.toByte) // code
        builder.putBytes("double".getBytes(utf8)) // c string
        builder.putByte(0.toByte) // termination of c string
        builder.putDouble(data)   // double value
        builder.putByte(0.toByte) // terminating null
        builder.result()
      }
      val builder = Builder()
      builder.double("double", data)
      builder.result must beEqualTo(expected)
    }
    "build string correctly" in {
      val data = "fourty-two"
      val expected : ByteString = {
        implicit val byteOrder = ByteOrder.LITTLE_ENDIAN
        val builder = ByteString.newBuilder
        builder.putInt(24)        // length
        builder.putByte(2.toByte) // string code
        builder.putBytes("string".getBytes(utf8)) // c string
        builder.putByte(0.toByte) // termination of c string
        val str = data.getBytes(utf8)
        builder.putInt(str.length+1) // length of string
        builder.putBytes(str)   // data string
        builder.putByte(0.toByte) // string terminator
        builder.putByte(0.toByte) // terminating null
        builder.result()
      }
      val builder = Builder()
      builder.string("string", data)
      builder.result must beEqualTo(expected)
    }

    "build object correctly" in {
      val expected : ByteString = {
        implicit val byteOrder = ByteOrder.LITTLE_ENDIAN
        val builder = ByteString.newBuilder
        builder.putInt(50)        // length
        builder.putByte(3.toByte)
        builder.putBytes("obj".getBytes(utf8))
        builder.putByte(0.toByte)
        builder.putInt(40)
        builder.putByte(2.toByte) // string code
        builder.putBytes("string".getBytes(utf8)) // c string
        builder.putByte(0.toByte) // termination of c string
        val str = "fourty-two".getBytes(utf8)
        builder.putInt(str.length+1) // length of string
        builder.putBytes(str)   // data string
        builder.putByte(0.toByte) // string terminator
        builder.putByte(1.toByte) // code
        builder.putBytes("double".getBytes(utf8)) // c string
        builder.putByte(0.toByte) // termination of c string
        builder.putDouble(42.0)   // double value
        builder.putByte(0.toByte) // terminating null
        builder.putByte(0.toByte) // terminating null
        builder.result()
      }
      val builder1 = Builder()
      builder1.string("string", "fourty-two")
      builder1.double("double", 42.0)
      val builder2 = Builder()
      builder2.obj("obj", builder1)
      builder2.result must beEqualTo(expected)
    }
  }

  "build array correctly" in {
    pending(": implementation of Builder.array is not done")
  }

  "build binary correctly" in {
    val data = "fourty-two"
    val expected : ByteString = {
      implicit val byteOrder = ByteOrder.LITTLE_ENDIAN
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
    val builder = Builder()
    builder.binary("binary", data.getBytes(utf8), BinarySubtype.UserDefinedBinary)
    builder.result must beEqualTo(expected)

  }
}
