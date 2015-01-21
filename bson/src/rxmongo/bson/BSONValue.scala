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

import akka.util.{ByteIterator, ByteString}

trait BSONValue {
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

  def code : TypeCode = TypeCode(this)
}

trait BSONDocument extends BSONValue {

  case class DocumentIterator private[bson] (itr: ByteIterator) extends Iterator[(String,BSONValue)] {
    private def getKey: String = { getCStr(itr) }

    private def skipLength : Int = {
      val len = itr.getInt
      itr.drop(len)
      len + 4
    }

    def hasNext: Boolean = itr.hasNext
    def next(): (String, BSONValue) = {
      if (hasNext) {
        val code = itr.getByte
        val key = getKey
        val save = itr.clone
        TypeCode(code) match {
          case DoubleCode =>
            itr.getDouble
            key -> BSONDouble(save.slice(0,8).toByteString)
          case StringCode =>
            val len = skipLength
            key -> BSONString(save.slice(0,len).toByteString)
          case ObjectCode =>
            val len = skipLength
            key -> BSONObject(save.slice(0,len).toByteString)
          case ArrayCode  =>
            val len = skipLength
            key -> BSONArray(save.slice(0,len).toByteString)
          case BinaryCode =>
            val len = skipLength + 1
            itr.getByte
            key -> BSONBinary(save.slice(0,len).toByteString)
          case _ => ???
        }
      } else
        Iterator.empty.next()
    }
  }

  def iterator : Iterator[(String,BSONValue)] = {
    val itr = buffer.iterator
    val length = itr.getInt
    DocumentIterator(itr.slice(0,length-1))
  }

  def valueIterator : Iterator[BSONValue] = {
    iterator.map { case (key,value) => value }
  }

}

case class BSONDouble private[bson] (buffer: ByteString) extends BSONValue {
  def value : Double = { buffer.iterator.getDouble }
}

object BSONDouble {
  def apply(d: Double) = {
    val buffer = ByteString.newBuilder
    buffer.putDouble(d)(ByteOrder.LITTLE_ENDIAN)
    new BSONDouble(buffer.result)
  }
}

case class BSONString private[bson] (buffer: ByteString) extends BSONValue {
  def value : String = { getStr(buffer.iterator) }
}

object BSONString {
  def apply(s: String) = {
    val buffer = Builder()
    buffer.putStr(s)
    new BSONString(buffer.buffer.result())
  }
}

case class BSONObject private[bson] (buffer: ByteString) extends BSONDocument {

  def toMap : Map[String,BSONValue] = iterator.toMap

  def value : Map[String,BSONValue] = iterator.toMap
}

object BSONObject {
  def apply(data: Map[String,BSONValue]) : BSONObject = {
    val bldr = Builder()
    data.foreach { case (key, value) => bldr.value(key, value) }
    new BSONObject(bldr.result)
  }
}

case class BSONArray private[bson] (buffer: ByteString) extends BSONDocument {
  def value : Iterator[BSONValue] = valueIterator
}

object BSONArray {
  def apply(data: Iterable[BSONValue]) : BSONArray = {
    val bldr = Builder()
    bldr.array(data)
    new BSONArray(bldr.buffer.result())
  }
}

case class BSONBinary private[bson] (buffer: ByteString) extends BSONValue {
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

object BSONBinary {
  def apply(array: Array[Byte], subtype: BinarySubtype) : BSONBinary = {
    val bldr = Builder()
    bldr.binary(array, subtype)
    new BSONBinary( bldr.buffer.result() )
  }
}


