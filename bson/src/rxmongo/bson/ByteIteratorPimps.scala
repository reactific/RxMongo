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

import java.util.regex.Pattern

import akka.util.ByteIterator

import scala.annotation.switch

/** Implicit Extensions To ByteIterator
  *
  * This makes it easier to use a ByteIterator with MongoDB. The extensions implement recognition of various
  * data types that BSON encoding requires.
  */
trait ByteIteratorPimps {

  val itr : ByteIterator

  @inline def getCStr : String = {
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

  def getDoc : BSONDocument = {
    val save = itr.clone()
    val len = itr.getInt
    require(len < maxDocSize, s"Maximum object size is $maxDocSize bytes")
    itr.drop(len - 4)
    val bitr = save.slice(0, len)
    BSONDocument(bitr)
  }

  @inline def getObject : BSONObject = BSONObject(getDoc)

  @inline def getArray : BSONArray = BSONArray(getDoc)

  def getObjects(count : Int) : Seq[BSONObject] = {
    for (x ← 1 to count) yield { getObject }
  }

  def getBinary : (BinarySubtype, Array[Byte]) = {
    val len = itr.getInt
    val st = BinarySubtype(itr.getByte)
    val value = itr.getBytes(len)
    st → value
  }

  @inline def getObjectID : Array[Byte] = itr.getBytes(12)

  @inline def getBoolean : Boolean = itr.getByte != 0

  def getRegex : Pattern = {
    val pattern = itr.getCStr
    val options = itr.getCStr
    val flags = options.foldLeft(0) {
      case (flg, ch) ⇒
        (ch : @switch) match {
          case 'i' ⇒ flg | Pattern.CASE_INSENSITIVE
          case 'm' ⇒ flg | Pattern.MULTILINE
          case 's' ⇒ flg | Pattern.DOTALL
          case 'u' ⇒ flg | Pattern.UNICODE_CHARACTER_CLASS | Pattern.UNICODE_CASE
          case 'x' ⇒ flg | Pattern.COMMENTS
          case _   ⇒ flg
        }
    }
    Pattern.compile(pattern, flags)
  }

  @inline def getDBPointer : (String, Array[Byte]) = {
    itr.getStr → itr.getBytes(12)
  }

  @inline def getScopedJavaScript : (String, BSONObject) = {
    itr.getInt
    itr.getStr → itr.getObject
  }

  @inline def skipLength : Int = {
    val len = itr.getInt
    itr.drop(len)
    len + 4
  }

  @inline def skipCStr : Int = {
    var count = 0
    itr.dropWhile { ch ⇒ count = count + 1; ch != 0 }
    itr.drop(1)
    count
  }

  @inline def skipDocument : Int = {
    val len = itr.getInt
    itr.drop(len - 4)
    len
  }

  @inline def skipLong : Int = { itr.drop(8); 8 }
  @inline def skipDouble : Int = skipLong
  @inline def skipInt : Int = { itr.drop(4); 4 }
  @inline def skipObjId : Int = { itr.drop(12); 12 }
  @inline def skipByte : Int = { itr.drop(1); 1 }

}
