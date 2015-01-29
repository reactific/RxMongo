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
import java.util.regex.Pattern

import akka.util.{ ByteIterator, ByteStringBuilder }

import scala.annotation.switch
import scala.util.matching.Regex

/** The bson package object.
  *
  * This just contains values that are used throughout the bson package.
  */
package object bson {

  case class RxMongoError(message : String, cause : Option[Throwable] = None) extends Exception {
    override def getMessage = message
    override def getCause = cause.orNull
  }

  // Everything in Mongo is Little Endian
  implicit val byteOrder = ByteOrder.LITTLE_ENDIAN

  val utf8 = Charset.forName("UTF-8")

  /** Maximum Document Size.
    *
    * Use Mongo's maximum doc size to ensure that we're having sane reading of length fields and we don't OOM
    * by trying to allocate all memory.
    * @see [[http://docs.mongodb.org/manual/reference/command/isMaster/#dbcmd.isMaster]]
    *
    */
  final val maxDocSize = 16 * 1024 * 1024

  /** Implicit Extensions to ByteStringBuilder
    *
    * This makes using a ByteStringBuilder with BSON easier by providing functions that build BSON data structures.
    *
    * @param bldr The builder we are extending
    */
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
      bldr.putInt(bytes.length + 1)
      bldr.putBytes(bytes)
      bldr.putByte(0)
      bldr
    }

    def putRegex(pattern : String, options : String) : ByteStringBuilder = {
      require(options.matches("i?l?m?s?u?x?"), "Regex options allowed are: ilmsux")
      putCStr(pattern)
      putCStr(options)
      bldr
    }

    def putRegex(r : Regex) : ByteStringBuilder = {
      val pattern : String = r.pattern.pattern()
      val options : String = {
        //
        // 'i' for case insensitive matching,
        // 'l' to make \w, \W, etc. locale dependent,
        // 'm' for multiline matching,
        // 's' for dotall mode ('.' matches everything),
        // and 'u' to make \w, \W, etc. match unicode.
        // 'x' for verbose mode,
        val flags = r.pattern.flags()
        var result = ""
        if ((flags & Pattern.CASE_INSENSITIVE) != 0)
          result += "i"
        if ((flags & Pattern.MULTILINE) != 0)
          result += "m"
        if ((flags & Pattern.DOTALL) != 0)
          result += "s"
        if ((flags & Pattern.UNICODE_CHARACTER_CLASS) != 0)
          result += "u"
        if ((flags & Pattern.COMMENTS) != 0)
          result += "x"
        result
      }
      putRegex(pattern, options)
    }

    def putDoc(value : BSONDocument) : ByteStringBuilder = {
      require(bldr.length + value.buffer.length <= maxDocSize, s"Document size exceeds maximum of $maxDocSize")
      bldr ++= value.buffer
      bldr
    }

    def putDoc(value : Option[BSONDocument]) : ByteStringBuilder = {
      value match {
        case Some(doc) ⇒ putDoc(doc)
        case None ⇒ bldr
      }
    }

    def putDocs(values : Seq[BSONDocument]) : ByteStringBuilder = {
      for (doc ← values) { putDoc(doc) }
      bldr
    }

    def putObj(value : BSONObject) : ByteStringBuilder = putDoc(value)
    def putObj(value : Option[BSONObject]) : ByteStringBuilder = putDoc(value)
    def putObjs(value : Seq[BSONObject]) : ByteStringBuilder = putDocs(value)

    def putArr(value : BSONArray) : ByteStringBuilder = putDoc(value)
    def putArr(value : Option[BSONArray]) : ByteStringBuilder = putDoc(value)
    def putArrs(value : Seq[BSONArray]) : ByteStringBuilder = putDocs(value)

  }

  /** Implicit Extensions To ByteIterator
    *
    * This makes it easier to use a ByteIterator with MongoDB. The extensions implement recognition of various
    * data types that BSON encoding requires.
    * @param itr The wrapped iterator
    */
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
      require(len < maxDocSize, s"Maximum string size is $maxDocSize bytes")
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
      require(len < maxDocSize, s"Maximum object size is $maxDocSize bytes")
      itr.drop(len - 4)
      val buffer = save.slice(0, len).toByteString
      new BSONObject(buffer)
    }

    def getObjs(count : Int) : Seq[BSONObject] = {
      for (x ← 1 to count) yield { getObj }
    }

    def getRegex : Regex = {
      val pattern = itr.getCStr
      val options = itr.getCStr
      val regex_options = {
        options.map {
          case ch : Char ⇒
            (ch : @switch) match {
              case 'i' ⇒ "i"
              case 'l' ⇒ ""
              case 'm' ⇒ "m"
              case 's' ⇒ "s"
              case 'u' ⇒ "U"
              case 'x' ⇒ "x"
              case _   ⇒ ""
            }
        }
      }.mkString
      new Regex("(?" + regex_options + ")" + pattern)
    }

    def skipLength : Int = {
      val len = itr.getInt
      itr.drop(len)
      len + 4
    }

    def skipCStr : Int = {
      var count = 0
      itr.dropWhile { ch ⇒ count += 1; ch != 0 }
      itr.drop(1)
      count
    }

    def skipDocument : Int = {
      val len = itr.getInt
      itr.drop(len - 4)
      len
    }

    def skipLong : Int = { itr.drop(8); 8 }
    def skipDouble : Int = skipLong
    def skipInt : Int = { itr.drop(4); 4 }
    def skipObjId : Int = { itr.drop(12); 12 }
    def skipByte : Int = { itr.drop(1); 1 }

  }
}
