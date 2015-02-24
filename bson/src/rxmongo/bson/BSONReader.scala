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

import akka.util.{ ByteIterator, ByteString }

import scala.annotation.switch
import scala.collection.{ Map, mutable }

class BSONReader private[rxmongo] (itr : ByteIterator) {

  def toMap : Map[String, BSONValue] = {
    doc.map { case (key, (code, bs)) ⇒ key -> BSONValue(code, bs) }
  }

  type DecodedDoc = Map[String, (TypeCode, ByteString)]

  private val doc : DecodedDoc = {

      def advance(i : ByteIterator, code : Byte) : Int = {
        (code : @switch) match {
          case 1  ⇒ i.skipDouble // Double
          case 2  ⇒ i.skipLength // String
          case 3  ⇒ i.skipDocument // Object
          case 4  ⇒ i.skipDocument // Array
          case 5  ⇒ i.skipLength + i.skipByte // Binary
          case 6  ⇒ 0 // Undefined
          case 7  ⇒ i.skipObjId // ObjectID
          case 8  ⇒ i.skipByte // Boolean
          case 9  ⇒ i.skipLong // Date
          case 10 ⇒ 0 // Null
          case 11 ⇒ i.skipCStr + i.skipCStr // Regex
          case 12 ⇒ i.skipLength + i.skipObjId // DBPointer
          case 13 ⇒ i.skipLength // JavaScript
          case 14 ⇒ i.skipLength // Symbol
          case 15 ⇒ i.skipDocument // Scoped JavaScript
          case 16 ⇒ i.skipInt // Integer
          case 17 ⇒ i.skipLong // Timestamp
          case 18 ⇒ i.skipLong // Long
          case _  ⇒ throw new NoSuchElementException("Unrecognized BSON Type Code")
        }
      }
    val bldr = new mutable.MapBuilder[String, (TypeCode, ByteString), Map[String, (TypeCode, ByteString)]](Map.empty[String, (TypeCode, ByteString)])
    val byteLen = itr.getInt
    var code = itr.getByte
    while (itr.hasNext && code != 0) {
      val key = itr.getCStr
      val save = itr.clone()
      val len = advance(itr, code)
      bldr += key -> (TypeCode(code) → save.slice(0, len).toByteString)
      code = itr.getByte
    }
    bldr.result()
  }
}

object BSONReader {
  def apply(bs : ByteString) = new BSONReader(bs.iterator)
}
