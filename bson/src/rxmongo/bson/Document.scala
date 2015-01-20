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

import akka.util.{ByteIterator, ByteString, ByteStringBuilder}

/** Primary Interface To BSON Model */
object Document {


  /*
  case class Double private[bson] (buffer: ByteBuffer, offset: Int) extends Field {
    require(code == 0x01.toByte)

  }

  case class BSONString private[bson] (buffer: ByteBuffer, offset: Int) extends Field {
    require(code == StringCode)
  }

  case class Object(buffer: ByteBuffer) extends { val offset: Int = 4 } with Value {
    lazy val map : Map[String,Value] = {
      buffer.rewind()
      while (buffer.hasRemaining) {}
    }
  }

  case class Integer(buffer: ByteBuffer, offset: Int) extends Field {
    require(code == IntegerCode)
  }
*/
}

trait Value {
  implicit val byteOrder = ByteOrder.LITTLE_ENDIAN
  val buffer: ByteString
  def value : Any

  protected def getCStr(itr: ByteIterator) : String = {
    val s = itr.clone.takeWhile { p => p != 0 }
    itr.drop(s.len + 1)
    val buf = Array.ofDim[Byte](s.len)
    s.copyToArray(buf)
    new String(buf, utf8)
  }
  protected def getStr(itr: ByteIterator) : String = {
    val len = itr.getInt - 1
    val buf = Array.ofDim[Byte](len)
    itr.copyToArray(buf)
    require(itr.getByte == 0.toByte)
    new String(buf, utf8)
  }
}

case class BSONDouble private[bson] (buffer: ByteString) extends Value {
  def value : Double = { buffer.iterator.getDouble }
}

case class BSONString private[bson] (buffer: ByteString) extends Value {
  def value : String = { getStr(buffer.iterator) }
}

case class BSONBinary private[bson] (buffer: ByteString) extends Value {
  def value : (BinarySubtype, Array[Byte]) = {
    val itr = buffer.iterator
    val len = itr.getInt
    val subtype = itr.getByte
    val result = Array.ofDim[Byte](len)
    itr.copyToArray(result)
    BinarySubtype(subtype) -> result
  }

  def subtype : BinarySubtype = { BinarySubtype(buffer.iterator.drop(4).getByte) }
}

case class Document private[bson] (buffer: ByteString) extends { val itr = buffer.iterator }
with Value with Iterable[(String,Value)] {

  case class DocIterator private[bson] (itr: ByteIterator) extends Iterator[(String,Value)] {
    private def getKey: String = { getCStr(itr) }

    def hasNext: Boolean = itr.hasNext
    def next(): (String, Value) = {
      if (!hasNext)
        Iterator.empty.next()
      else {
        val code = itr.getByte
        val key = getKey
        TypeCode(code) match {
          case DoubleCode => key -> BSONDouble(itr.clone().toByteString)
          case StringCode => key -> BSONString(itr.clone().toByteString)
          case ObjectCode => key -> ???
          case ArrayCode  => key -> ???
          case BinaryCode => key -> BSONBinary(itr.clone().toByteString)
          case _ => ???
        }
      }
    }
  }

  def iterator : DocIterator = {
    val itr = buffer.iterator
    val length = itr.getInt
    DocIterator(itr.slice(0,length-1))
  }

  def toMap : Map[String,Value] = iterator.toMap

  def value : Map[String,Value] = iterator.toMap
}



