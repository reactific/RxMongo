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

import java.util

import akka.util.{ ByteIterator, ByteString }

import scala.annotation.switch
import scala.collection.{ Map, MapLike, mutable }

case class BSONDocument private[rxmongo] (
  private[bson] val data : Map[String, (Byte, ByteIterator)],
  docItr : Option[ByteIterator] = None
) extends BSONProvider with MapLike[String, (Byte, ByteIterator), BSONDocument] with Map[String, (Byte, ByteIterator)] {

  def get(key : String) : Option[(Byte, ByteIterator)] = data.get(key)

  def as[T](key: String, code: TypeCode, conv: (ByteIterator) ⇒ T) : T = {
    data.get(key) match {
      case Some((c,itr)) if c == code.code ⇒ conv(itr)
      case Some((c,itr)) ⇒ throw new IllegalArgumentException(s"Field '$key' has type ${TypeCode(c).typeName} not ${code.typeName}")
      case None ⇒ throw new NoSuchElementException(s"Field '$key' does not exist.")
    }
  }
  def asInt(key: String) : Int = as[Int](key, IntegerCode, { itr ⇒ itr.clone().getInt })
  def asLong(key: String) : Long = as[Long](key, LongCode, { itr ⇒ itr.clone().getLong })
  def asDouble(key: String) : Double = as[Double](key, DoubleCode, { itr ⇒ itr.clone().getDouble })
  def asString(key: String) : String = as[String](key, StringCode, { itr ⇒ itr.clone().getStr })
  def asObject(key: String) : BSONObject = as[BSONObject](key, ObjectCode, { itr ⇒ itr.clone().getObject })
  def asArray(key: String) : BSONArray = as[BSONArray](key, ArrayCode, { itr ⇒ itr.clone().getArray })

  def +[B1 >: (Byte, ByteIterator)](kv : (String, B1)) : BSONDocument = {
    val pair = kv._1 → kv._2.asInstanceOf[(Byte, ByteIterator)]
    BSONDocument(data.+(pair), None)
  }

  def iterator : Iterator[(String, (Byte, ByteIterator))] = data.iterator

  def -(key : String) : BSONDocument = {
    BSONDocument(data.-(key))
  }

  override def empty : BSONDocument = BSONDocument.empty

  override protected[this] def newBuilder : mutable.Builder[(String, (Byte, ByteIterator)), BSONDocument] = ???

  def addTo(b : BSONBuilder) : Unit = {
    for ((k, (typeCode, byteIterator)) ← data) {
      b.putPrefix(typeCode, k)
      b.buffer ++= byteIterator.clone()
    }
  }

  override def toByteString : ByteString = {
    docItr match {
      case Some(itr) ⇒ itr.clone().toByteString
      case None ⇒
        val bldr = BSONBuilder()
        for ((k, (typeCode, byteIterator)) ← data) {
          bldr.putPrefix(typeCode, k)
          bldr.buffer ++= byteIterator.clone()
        }
        bldr.toByteString
    }
  }

  override def equals(other : Any) : Boolean = other match {
    case that : BSONDocument ⇒
      (this eq that) ||
        (that canEqual this) &&
        (this.size == that.size) && {
          try {
            this forall {
              case (k, (tc1, bi1)) ⇒ that.get(k) match {
                case Some((tc2, bi2)) ⇒ tc1 == tc2 && util.Arrays.equals(bi1.clone().toArray, bi2.clone().toArray)
                case None ⇒ false
                case _ ⇒ false
              }
            }
          } catch {
            case ex : ClassCastException ⇒
              println("class cast "); false
          }
        }
    case _ ⇒
      false
  }
}

object BSONDocument {
  object empty extends BSONDocument(Map.empty[String, (Byte, ByteIterator)], None)

  def apply() : BSONDocument = empty

  def apply(fields : Map[String, BSONValue]) : BSONDocument = {
    val b = BSONBuilder()
    for ((k, v) ← fields) b.append(k, v)
    BSONDocument(b.toByteString)
  }

  def apply(buffer : ByteString) : BSONDocument = BSONDocument(buffer.iterator)
  def apply(itr : ByteIterator) : BSONDocument = {
    val bldr = new mutable.MapBuilder[String, (Byte, ByteIterator), Map[String, (Byte, ByteIterator)]](Map.empty[String, (Byte, ByteIterator)])
    val docItr = itr.clone()
    val byteLen = itr.getInt
    var code = itr.getByte
    while (itr.hasNext && code != 0) {
      val key = itr.getCStr
      val save = itr.clone()
      val bi : ByteIterator = (code : @switch) match {
        case 1  ⇒ save.slice(0, itr.skipDouble) // Double
        case 2  ⇒ save.slice(0, itr.skipLength) // String
        case 3  ⇒ save.slice(0, itr.skipDocument) // Object
        case 4  ⇒ save.slice(0, itr.skipDocument) // Array
        case 5  ⇒ save.slice(0, itr.skipLength + itr.skipByte) // Binary
        case 6  ⇒ save.slice(0, 0) // Undefined
        case 7  ⇒ save.slice(0, itr.skipObjId) // ObjectID
        case 8  ⇒ save.slice(0, itr.skipByte) // Boolean
        case 9  ⇒ save.slice(0, itr.skipLong) // Date
        case 10 ⇒ save.slice(0, 0) // Null
        case 11 ⇒ save.slice(0, itr.skipCStr + itr.skipCStr) // Regex
        case 12 ⇒ save.slice(0, itr.skipLength + itr.skipObjId) // DBPointer
        case 13 ⇒ save.slice(0, itr.skipLength) // JavaScript
        case 14 ⇒ save.slice(0, itr.skipLength) // Symbol
        case 15 ⇒ save.slice(0, itr.skipDocument) // Scoped JavaScript
        case 16 ⇒ save.slice(0, itr.skipInt) // Integer
        case 17 ⇒ save.slice(0, itr.skipLong) // Timestamp
        case 18 ⇒ save.slice(0, itr.skipLong) // Long
        case _  ⇒ throw new NoSuchElementException("Unrecognized BSON Type Code")
      }
      bldr += key -> (code → bi)
      code = itr.getByte
    }
    val map = bldr.result()
    new BSONDocument(map, Some(docItr))
  }
}
