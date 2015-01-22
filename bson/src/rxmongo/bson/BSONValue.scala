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
import java.util.Date
import java.util.regex.Pattern

import akka.util.{ ByteIterator, ByteString }

import scala.util.matching.Regex

trait BSONValue {
  implicit val byteOrder = ByteOrder.LITTLE_ENDIAN
  private[bson] val buffer: ByteString
  def value: Any

  protected def getCStr(itr: ByteIterator): String = {
    val s = itr.clone.takeWhile { p => p != 0 }
    itr.drop(s.len + 1)
    val buf = Array.ofDim[Byte](s.len)
    s.copyToArray(buf)
    new String(buf, utf8)
  }
  protected def getStr(itr: ByteIterator): String = {
    val len = itr.getInt - 1
    require(len < 16 * 1024 * 1024)
    val buf = Array.ofDim[Byte](len)
    itr.getBytes(buf)
    require(itr.getByte == 0.toByte)
    new String(buf, utf8)
  }

  protected def getBytes(itr: ByteIterator, len: Int): Array[Byte] = {
    val res = Array.ofDim[Byte](len)
    itr.copyToArray(res)
    res
  }

  protected def getObj(itr: ByteIterator): BSONObject = {
    val save = itr.clone()
    val len = itr.getInt
    itr.drop(len)
    val buffer = save.slice(0, len + 4).toByteString
    new BSONObject(buffer)
  }

  def code: TypeCode = TypeCode(this)

  def length: Int = buffer.length
}

trait BSONDocument extends BSONValue {

  case class DocumentIterator private[bson] (itr: ByteIterator) extends Iterator[(String, BSONValue)] {
    private def getKey: String = { getCStr(itr) }

    private def skipLength: Int = {
      val len = itr.getInt
      itr.drop(len)
      len + 4
    }

    private def skipCStr: Int = {
      var count = 0
      itr.dropWhile { ch => count += 1; ch != 0 }
      itr.drop(1)
      count
    }

    private def skipLong: Int = { itr.drop(8); 8 }
    private def skipDouble: Int = skipLong
    private def skipInt: Int = { itr.drop(4); 4 }
    private def skipObjId: Int = { itr.drop(12); 12 }
    private def skipByte: Int = { itr.drop(1); 1 }

    def hasNext: Boolean = itr.hasNext
    def next(): (String, BSONValue) = {
      if (hasNext) {
        val code = itr.getByte
        val key = getKey
        val save = itr.clone()
        TypeCode(code) match {
          case IntegerCode =>
            key -> BSONInteger(save.slice(0, skipInt).toByteString)
          case LongCode =>
            key -> BSONLong(save.slice(0, skipLong).toByteString)
          case DoubleCode =>
            key -> BSONDouble(save.slice(0, skipDouble).toByteString)
          case StringCode =>
            key -> BSONString(save.slice(0, skipLength).toByteString)
          case ObjectCode =>
            key -> BSONObject(save.slice(0, skipLength).toByteString)
          case ArrayCode =>
            key -> BSONArray(save.slice(0, skipLength).toByteString)
          case BinaryCode =>
            val len = skipLength + 1
            itr.drop(1)
            key -> BSONBinary(save.slice(0, len).toByteString)
          case ObjectIDCode =>
            key -> BSONObjectID(save.slice(0, skipObjId).toByteString)
          case BooleanCode =>
            key -> BSONBoolean(save.slice(0, skipByte).toByteString)
          case DateCode =>
            key -> BSONDate(save.slice(0, skipLong).toByteString)
          case NullCode =>
            key -> BSONNull(ByteString.empty)
          case RegexCode =>
            key -> BSONRegex(save.slice(0, skipCStr + skipCStr).toByteString)
          case JavaScriptCode =>
            key -> BSONJsCode(save.slice(0, skipLength).toByteString)
          case ScopedJSCode =>
            key -> BSONScopedJsCode(save.slice(0, skipLength).toByteString)
          case DBPointerCode =>
            key -> BSONDBPointer(save.slice(0, skipLength + skipObjId).toByteString)
          case TimestampCode =>
            key -> BSONTimestamp(save.slice(0, skipLong).toByteString)
          case UndefinedCode =>
            key -> BSONUndefined(ByteString.empty)
          case SymbolCode =>
            key -> BSONSymbol(save.slice(0, skipLength).toByteString)
          case x: TypeCode => throw new NoSuchElementException(s"Unrecognized: $x")
        }
      } else
        Iterator.empty.next()
    }
  }

  def iterator: Iterator[(String, BSONValue)] = {
    val itr = buffer.iterator
    val length = itr.getInt
    DocumentIterator(itr.slice(0, length - 1))
  }

  def valueIterator: Iterator[BSONValue] = {
    iterator.map { case (key, value) => value }
  }

  def compact: BSONDocument
}

case class BSONDouble private[bson] (buffer: ByteString) extends BSONValue {
  def value: Double = { buffer.iterator.getDouble }
}

object BSONDouble {
  def apply(d: Double) = {
    val buffer = ByteString.newBuilder
    buffer.putDouble(d)(ByteOrder.LITTLE_ENDIAN)
    new BSONDouble(buffer.result())
  }
}

case class BSONString private[bson] (buffer: ByteString) extends BSONValue {
  def value: String = { getStr(buffer.iterator) }
}

object BSONString {
  def apply(s: String) = {
    val buffer = ByteString.newBuilder
    Builder.putStr(buffer, s)
    new BSONString(buffer.result())
  }
}

case class BSONObject private[bson] (buffer: ByteString) extends BSONDocument {

  def toMap: Map[String, BSONValue] = iterator.toMap

  def value: Map[String, BSONValue] = iterator.toMap

  def compact: BSONObject = BSONObject(buffer.compact)
}

object BSONObject {

  def apply(data: (String, BSONValue)*): BSONObject = from(data.toSeq)
  def apply(data: Map[String, BSONValue]): BSONObject = from(data.toSeq)

  def from(data: Seq[(String, BSONValue)]): BSONObject = {
    val bldr = Builder()
    data.foreach { case (key, value) => bldr.value(key, value) }
    new BSONObject(bldr.result)
  }

}

case class BSONArray private[bson] (buffer: ByteString) extends BSONDocument {
  def value: Iterator[BSONValue] = valueIterator
  def compact: BSONArray = BSONArray(buffer.compact)
}

object BSONArray {
  def apply(data: Iterable[BSONValue]): BSONArray = {
    val bldr = Builder()
    bldr.array(data)
    new BSONArray(bldr.buffer.result())
  }
}

case class BSONBinary private[bson] (buffer: ByteString) extends BSONValue {
  def value: (BinarySubtype, Array[Byte]) = {
    val itr = buffer.iterator
    val len = itr.getInt
    val subtype = itr.getByte
    val result = getBytes(itr, len)
    BinarySubtype(subtype) -> result
  }

  def subtype: BinarySubtype = { BinarySubtype(buffer.iterator.drop(4).getByte) }
}

object BSONBinary {
  def apply(array: Array[Byte], subtype: BinarySubtype): BSONBinary = {
    val bldr = Builder()
    bldr.binary(array, subtype)
    new BSONBinary(bldr.buffer.result())
  }
}

case class BSONUndefined private[bson] (buffer: ByteString) extends BSONValue {
  def value: Unit = {}
}

object BSONUndefined {
  def apply(): BSONUndefined = new BSONUndefined(ByteString.empty)
}

case class BSONObjectID private[bson] (buffer: ByteString) extends BSONValue {
  def value: Array[Byte] = {
    val itr = buffer.iterator
    getBytes(itr, 12)
  }
}

object BSONObjectID {
  def apply(bytes: Array[Byte]): BSONObjectID = {
    val bldr = Builder()
    bldr.objectID(bytes)
    new BSONObjectID(bldr.buffer.result)
  }
}

case class BSONBoolean private[bson] (buffer: ByteString) extends BSONValue {
  def value: Boolean = {
    if (buffer.iterator.getByte == 0) false else true
  }
}

object BSONBoolean {
  def apply(b: Boolean): BSONBoolean = {
    val bldr = Builder()
    bldr.boolean(b)
    new BSONBoolean(bldr.buffer.result)
  }
}

case class BSONDate private[bson] (buffer: ByteString) extends BSONValue {
  def value: Long = {
    buffer.iterator.getLong
  }
  def toDate: Date = { new Date(buffer.iterator.getLong) }
}

object BSONDate {
  def apply(d: Long): BSONDate = {
    val buffer = ByteString.newBuilder
    buffer.putLong(d)(ByteOrder.LITTLE_ENDIAN)
    new BSONDate(buffer.result())
  }
  def apply(d: Date): BSONDate = apply(d.getTime)
}

case class BSONNull private[bson] (buffer: ByteString) extends BSONValue {
  def value: Unit = {}
}

object BSONNull {
  def apply(): BSONNull = { new BSONNull(ByteString.empty) }
}

case class BSONRegex private[bson] (buffer: ByteString) extends BSONValue {
  def value: Regex = {
    val itr = buffer.iterator
    val pattern = getCStr(itr)
    val options = getCStr(itr)
    val regex_options = {
      options.map {
        case ch: Char =>
          ch match {
            case 'i' => "i"
            case 'l' => ""
            case 'm' => "m"
            case 's' => "s"
            case 'u' => "U"
            case 'x' => "x"
          }
      }
    }.mkString
    new Regex("(?" + regex_options + ")" + pattern)
  }
}

object BSONRegex {
  def apply(r: Regex): BSONRegex = {
    val pattern: String = r.pattern.pattern()
    val options: String = {
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
    val bldr = Builder()
    bldr.regex(pattern, options)
    new BSONRegex(bldr.buffer.result())
  }
}

case class BSONDBPointer private[bson] (buffer: ByteString) extends BSONValue {
  def value: (String, Array[Byte]) = {
    val itr = buffer.iterator
    getStr(itr) -> getBytes(itr, 12)
  }
}

object BSONDBPointer {
  def apply(referent: String, objectID: Array[Byte]): BSONDBPointer = {
    val bldr = Builder()
    bldr.dbPointer(referent, objectID)
    new BSONDBPointer(bldr.buffer.result())
  }
}

case class BSONJsCode private[bson] (buffer: ByteString) extends BSONValue {
  def value: String = {
    getStr(buffer.iterator)
  }
}

object BSONJsCode {
  def apply(s: String): BSONJsCode = {
    val buffer = ByteString.newBuilder
    Builder.putStr(buffer, s)
    new BSONJsCode(buffer.result)
  }
}

case class BSONSymbol private[bson] (buffer: ByteString) extends BSONValue {
  def value: String = {
    getStr(buffer.iterator)
  }
}

object BSONSymbol {
  def apply(s: String): BSONSymbol = {
    val buffer = ByteString.newBuilder
    Builder.putStr(buffer, s)
    new BSONSymbol(buffer.result())
  }
}

case class BSONScopedJsCode private[bson] (buffer: ByteString) extends BSONValue {
  def value: (String, BSONObject) = {
    val itr = buffer.iterator
    itr.getInt
    val code = getStr(itr)
    val obj = getObj(itr)
    code -> obj
  }
}

object BSONScopedJsCode {
  def apply(code: String, scope: BSONObject): BSONScopedJsCode = {
    val bldr = Builder()
    bldr.scopedJsCode(code, scope)
    new BSONScopedJsCode(bldr.buffer.result())
  }
}

case class BSONInteger private[bson] (buffer: ByteString) extends BSONValue {
  def value: Int = {
    buffer.iterator.getInt
  }
}

object BSONInteger {
  def apply(i: Int): BSONInteger = {
    val buffer = ByteString.newBuilder
    buffer.putInt(i)(ByteOrder.LITTLE_ENDIAN)
    new BSONInteger(buffer.result())

  }
}

case class BSONTimestamp private[bson] (buffer: ByteString) extends BSONValue {
  def value: Long = {
    buffer.iterator.getLong
  }
}

object BSONTimestamp {
  def apply(t: Long): BSONTimestamp = {
    val buffer = ByteString.newBuilder
    buffer.putLong(t)(ByteOrder.LITTLE_ENDIAN)
    new BSONTimestamp(buffer.result())
  }
}

case class BSONLong private[bson] (buffer: ByteString) extends BSONValue {
  def value: Long = {
    buffer.iterator.getLong
  }
}

object BSONLong {
  def apply(l: Long): BSONLong = {
    val buffer = ByteString.newBuilder
    buffer.putLong(l)(ByteOrder.LITTLE_ENDIAN)
    new BSONLong(buffer.result())
  }
}

