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

import akka.util.{ ByteString, ByteStringBuilder }

trait ByteStringUtils {

  implicit val byteOrder = ByteOrder.LITTLE_ENDIAN

  def cstring(bldr : ByteStringBuilder, str : String) : ByteStringBuilder = {
    bldr.putBytes(str.getBytes(utf8)) // c string
    bldr.putByte(0) // termination of c string
    bldr
  }

  def string(bldr : ByteStringBuilder, str : String) : ByteStringBuilder = {
    val bytes = str.getBytes(utf8)
    bldr.putInt(bytes.length + 1)
    bldr.putBytes(bytes)
    bldr.putByte(0)
  }

  def field(bldr : ByteStringBuilder, code : Byte, fieldName : String) : ByteStringBuilder = {
    bldr.putByte(code) // code
    cstring(bldr, fieldName)
  }

  def preamble(len : Int, code : Byte, fieldName : String) : ByteStringBuilder = {
    val bldr : ByteStringBuilder = ByteString.newBuilder
    bldr.putInt(len)
    field(bldr, code, fieldName)
  }

}
