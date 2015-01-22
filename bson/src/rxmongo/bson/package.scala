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

package rxmongo

import java.nio.ByteOrder
import java.nio.charset.Charset

import akka.util.{ ByteIterator, ByteStringBuilder }

/** The bson package object.
  *
  * This just contains values that are used throughout the bson package.
  */
package object bson {

  // Everything in Mongo is Little Endian
  implicit val byteOrder = ByteOrder.LITTLE_ENDIAN

  // Byte code identifiers from here: http://docs.mongodb.org/manual/reference/bson-types/
  sealed trait TypeCode { val code : Byte }

  case object DoubleCode extends { val code : Byte = 1 } with TypeCode
  case object StringCode extends { val code : Byte = 2 } with TypeCode
  case object ObjectCode extends { val code : Byte = 3 } with TypeCode
  case object ArrayCode extends { val code : Byte = 4 } with TypeCode
  case object BinaryCode extends { val code : Byte = 5 } with TypeCode
  case object UndefinedCode extends { val code : Byte = 6 } with TypeCode // Note: Value 6 is deprecated
  case object ObjectIDCode extends { val code : Byte = 7 } with TypeCode
  case object BooleanCode extends { val code : Byte = 8 } with TypeCode
  case object DateCode extends { val code : Byte = 9 } with TypeCode
  case object NullCode extends { val code : Byte = 10 } with TypeCode
  case object RegexCode extends { val code : Byte = 11 } with TypeCode
  case object DBPointerCode extends { val code : Byte = 12 } with TypeCode // Note: Value 12 is deprecated
  case object JavaScriptCode extends { val code : Byte = 13 } with TypeCode
  case object SymbolCode extends { val code : Byte = 14 } with TypeCode // Note: Value 14 is deprecated
  case object ScopedJSCode extends { val code : Byte = 15 } with TypeCode
  case object IntegerCode extends { val code : Byte = 16 } with TypeCode
  case object TimestampCode extends { val code : Byte = 17 } with TypeCode
  case object LongCode extends { val code : Byte = 18 } with TypeCode
  case object MinKey extends { val code : Byte = 255.toByte } with TypeCode
  case object MaxKey extends { val code : Byte = 127 } with TypeCode

  object TypeCode {
    def apply(code : Byte) : TypeCode = {
      code match {
        case 1   ⇒ DoubleCode
        case 2   ⇒ StringCode
        case 3   ⇒ ObjectCode
        case 4   ⇒ ArrayCode
        case 5   ⇒ BinaryCode
        case 6   ⇒ UndefinedCode
        case 7   ⇒ ObjectIDCode
        case 8   ⇒ BooleanCode
        case 9   ⇒ DateCode
        case 10  ⇒ NullCode
        case 11  ⇒ RegexCode
        case 12  ⇒ DBPointerCode
        case 13  ⇒ JavaScriptCode
        case 14  ⇒ SymbolCode
        case 15  ⇒ ScopedJSCode
        case 16  ⇒ IntegerCode
        case 17  ⇒ TimestampCode
        case 18  ⇒ LongCode
        case -1  ⇒ MinKey
        case 127 ⇒ MaxKey
        case _ ⇒
          throw new NoSuchElementException(s"BSON TypeCode($code)")
      }
    }
    def apply(v : BSONValue) : TypeCode = {
      v match {
        case x : BSONDouble ⇒ DoubleCode
        case x : BSONString ⇒ StringCode
        case x : BSONObject ⇒ ObjectCode
        case x : BSONArray ⇒ ArrayCode
        case x : BSONBinary ⇒ BinaryCode
        case _ ⇒ throw new NoSuchElementException(s"BSON TypeCode($v)")
      }
    }
  }

  val utf8 = Charset.forName("UTF-8")

  implicit class ByteStringBuilderPimps(bldr : ByteStringBuilder) {

    def putCStr(s : String) : ByteStringBuilder = {
      val bytes = s.getBytes(utf8)
      for (by ← bytes if by == 0) {
        throw new IllegalArgumentException("UTF-8 encoding of BSON keys must not contain a 0 byte")
      }
      bldr.putBytes(bytes)
      bldr.putByte(0)
      bldr
    }

    def putStr(value : String) : ByteStringBuilder = {
      val bytes = value.getBytes(utf8)
      val length = bytes.length + 1
      bldr.putInt(bytes.length + 1)
      bldr.putBytes(bytes)
      bldr.putByte(0)
      bldr
    }

    def putDoc(value : BSONDocument) : ByteStringBuilder = {
      bldr ++= value.buffer
      bldr
    }

    def putObj(value : BSONObject) : ByteStringBuilder = putDoc(value)

    def putDoc(value : Option[BSONDocument]) : ByteStringBuilder = {
      value match {
        case Some(doc) ⇒
          bldr ++= doc.buffer; bldr
        case None ⇒ bldr
      }
    }

    def putObj(value : Option[BSONObject]) : ByteStringBuilder = putDoc(value)

    def putDocs(docs : Seq[BSONDocument]) : ByteStringBuilder = {
      for (doc ← docs) { bldr ++= doc.buffer }
      bldr
    }

    def putObjs(objs : Seq[BSONObject]) : ByteStringBuilder = putDocs(objs)

  }

  // Use Mongo's maximum doc size to ensure that we're having sane reading of length fields and we don't OOM
  // by trying to allocate all memory.

  final val maxDocSize = 16 * 1024 * 1024
  implicit class ByteIteratorPimps(itr : ByteIterator) {

    def getCStr : String = {
      val s = itr.clone().takeWhile { p ⇒ p != 0 }
      itr.drop(s.len + 1)
      val buf = Array.ofDim[Byte](s.len)
      s.copyToArray(buf)
      new String(buf, utf8)
    }

    def getStr : String = {
      val len = itr.getInt - 1
      require(len < 16 * 1024 * 1024, s"Maximum string size is $maxDocSize bytes")
      val buf = Array.ofDim[Byte](len)
      itr.getBytes(buf)
      require(itr.getByte == 0.toByte, "Failed to read terminating null in String")
      new String(buf, utf8)
    }

    def getBytes(len : Int) : Array[Byte] = {
      val res = Array.ofDim[Byte](len)
      itr.getBytes(res)
      res
    }

    def getObj : BSONObject = {
      val save = itr.clone()
      val len = itr.getInt
      require(len < 16 * 1024 * 1024, s"Maximum object size is $maxDocSize bytes")
      itr.drop(len)
      val buffer = save.slice(0, len + 4).toByteString
      new BSONObject(buffer)
    }

    def getObjs(count : Int) : Seq[BSONObject] = {
      for (x ← 1 to count) yield { getObj }
    }
  }
}
