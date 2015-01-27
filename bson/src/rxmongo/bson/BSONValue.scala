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

import akka.util.{ ByteIterator, ByteString }

import scala.collection.{ Map, MapLike }
import scala.util.Try
import scala.util.matching.Regex

trait BSONValue {
  implicit val byteOrder = ByteOrder.LITTLE_ENDIAN
  private[bson] val buffer : ByteString
  def value : Any

  def code : TypeCode

  def length : Int = buffer.length
}

trait BSONDocument extends BSONValue {

  class DocumentIterator private[bson] (docItr : ByteIterator) extends Iterator[(String, BSONValue)] {

    val byteLen = docItr.getInt
    val itr = docItr.slice(0, byteLen - 5) // remove 4 for the byteLen, and 1 for the terminating 0

    private def getKey : String = { itr.getCStr }

    private def skipLength : Int = {
      val len = itr.getInt
      itr.drop(len)
      len + 4
    }

    private def skipCStr : Int = {
      var count = 0
      itr.dropWhile { ch ⇒ count += 1; ch != 0 }
      itr.drop(1)
      count
    }

    private def skipLong : Int = { itr.drop(8); 8 }
    private def skipDouble : Int = skipLong
    private def skipInt : Int = { itr.drop(4); 4 }
    private def skipObjId : Int = { itr.drop(12); 12 }
    private def skipByte : Int = { itr.drop(1); 1 }

    private def skipDocument : Int = {
      val len = itr.getInt
      itr.drop(len - 4)
      len
    }

    def hasNext : Boolean = itr.hasNext
    def next() : (String, BSONValue) = {
      if (hasNext) {
        val code = itr.getByte
        val key = getKey
        val save = itr.clone()
        TypeCode(code) match {
          case IntegerCode ⇒
            key -> BSONInteger(save.slice(0, skipInt).toByteString)
          case LongCode ⇒
            key -> BSONLong(save.slice(0, skipLong).toByteString)
          case DoubleCode ⇒
            key -> BSONDouble(save.slice(0, skipDouble).toByteString)
          case StringCode ⇒
            key -> BSONString(save.slice(0, skipLength).toByteString)
          case ObjectCode ⇒
            key -> BSONObject(save.slice(0, skipDocument).toByteString)
          case ArrayCode ⇒
            key -> BSONArray(save.slice(0, skipDocument).toByteString)
          case BinaryCode ⇒
            val len = skipLength + 1
            itr.drop(1)
            key -> BSONBinary(save.slice(0, len).toByteString)
          case ObjectIDCode ⇒
            key -> BSONObjectID(save.slice(0, skipObjId).toByteString)
          case BooleanCode ⇒
            key -> BSONBoolean(save.slice(0, skipByte).toByteString)
          case DateCode ⇒
            key -> BSONDate(save.slice(0, skipLong).toByteString)
          case NullCode ⇒
            key -> BSONNull
          case RegexCode ⇒
            key -> BSONRegex(save.slice(0, skipCStr + skipCStr).toByteString)
          case JavaScriptCode ⇒
            key -> BSONJsCode(save.slice(0, skipLength).toByteString)
          case ScopedJSCode ⇒
            key -> BSONScopedJsCode(save.slice(0, skipDocument).toByteString)
          case DBPointerCode ⇒
            key -> BSONDBPointer(save.slice(0, skipLength + skipObjId).toByteString)
          case TimestampCode ⇒
            key -> BSONTimestamp(save.slice(0, skipLong).toByteString)
          case UndefinedCode ⇒
            key -> BSONUndefined
          case SymbolCode ⇒
            key -> BSONSymbol(save.slice(0, skipLength).toByteString)
          case x : TypeCode ⇒ throw new NoSuchElementException(s"Unrecognized: $x")
        }
      } else
        Iterator.empty.next()
    }
  }

  def compact : BSONDocument
}

case class BSONDouble private[bson] (buffer : ByteString) extends BSONValue {
  def code = DoubleCode
  def value : Double = { buffer.iterator.getDouble }
}

object BSONDouble {
  def apply(d : Double) = {
    val buffer = ByteString.newBuilder
    buffer.putDouble(d)(ByteOrder.LITTLE_ENDIAN)
    new BSONDouble(buffer.result())
  }
}

case class BSONString private[bson] (buffer : ByteString) extends BSONValue {
  def value : String = { buffer.iterator.getStr }
  val code = StringCode
}

object BSONString {
  def apply(s : String) = {
    val buffer = ByteString.newBuilder
    buffer.putStr(s)
    new BSONString(buffer.result())
  }
}

case class BSONObject private[bson] (buffer : ByteString)
  extends BSONDocument with MapLike[String, BSONValue, Map[String, BSONValue]] with Map[String, BSONValue] {

  def -(key : String) : BSONObject = ???

  def +[B1 >: BSONValue](kv : (String, B1)) : BSONObject = {
    val itr = buffer.iterator
    val len = itr.getInt
    val content = itr.slice(0, len - 5).toByteString
    val bldr = Builder()
    bldr.buffer ++= content
    bldr.value(kv._1, kv._2.asInstanceOf[BSONValue])
    new BSONObject(bldr.result)
  }

  def iterator : Iterator[(String, BSONValue)] = {
    new DocumentIterator(buffer.iterator)
  }

  override def foreach[U](f : ((String, BSONValue)) ⇒ U) : Unit = {
    iterator.foreach[U](f)
  }

  override def size : Int = {
    // FIXME: This is horribly inefficient
    iterator.size
  }

  def get(key : String) : Option[BSONValue] = {
    // FIXME: This is horribly inefficient
    toMap.get(key)
  }

  /** Get with conversion to another type
    *
    * Gets the BSONValue associated with the provided `key`, and converts it to `T` via the BSONCodec[T].
    *
    * @tparam T The type to which the call wishes the BSONValue to be decoded.
    * @param key The key to look up in the object to obtain the value
    * @return Success(None) if the key is not found, Success(Some(T)) if the key is found and converted successfully,
    *     Failure(Throwable) if the conversion failed
    *
    * If there is no matching value, or the value could not be deserialized or converted, returns a `None`.
    */
  def getAs[T](key : String)(implicit codec : BSONCodec[T]) : Try[Option[T]] = Try {
    get(key) match {
      case Some(value) ⇒ Some(codec.read(value))
      case None ⇒ None
    }
  }

  override def empty : BSONObject = BSONObject.empty

  override def seq : Map[String, BSONValue] = iterator.toMap

  def code = ObjectCode

  def toMap : Map[String, BSONValue] = iterator.toMap

  override def toSeq : Seq[(String, BSONValue)] = iterator.toSeq

  def value : Map[String, BSONValue] = iterator.toMap

  def compact : BSONObject = BSONObject(buffer.compact)
}

object BSONObject {

  object empty extends BSONObject(ByteString.empty)

  def apply(data : Map[String, Any]) : BSONObject = from(data.toSeq)
  def apply(data : (String, Any)*) : BSONObject = from(data.toSeq)

  def from(data : Seq[(String, Any)]) : BSONObject = {
    val bldr = Builder()
    data.foreach {
      case (key, value) ⇒ bldr.value(key, value)
    }
    new BSONObject(bldr.result)
  }
}

case class BSONArray private[bson] (buffer : ByteString) extends BSONDocument {
  def code = ArrayCode
  def value : Iterator[BSONValue] = iterator
  def compact : BSONArray = BSONArray(buffer.compact)

  def iterator : Iterator[BSONValue] = {
    val docItr = new DocumentIterator(buffer.iterator)
    docItr.map { case (key, value) ⇒ value }
  }
}

object BSONArray {
  def apply(data : Iterable[BSONValue]) : BSONArray = {
    val bldr = Builder()
    bldr.array(data)
    new BSONArray(bldr.buffer.result())
  }
}

case class BSONBinary private[bson] (buffer : ByteString) extends BSONValue {
  def code = BinaryCode

  def value : (BinarySubtype, Array[Byte]) = {
    val itr = buffer.iterator
    val len = itr.getInt
    val subtype = itr.getByte
    val result = itr.getBytes(len)
    BinarySubtype(subtype) -> result
  }

  def subtype : BinarySubtype = { BinarySubtype(buffer.iterator.drop(4).getByte) }
}

object BSONBinary {
  def apply(array : Array[Byte], subtype : BinarySubtype) : BSONBinary = {
    val bldr = Builder()
    bldr.binary(array, subtype)
    new BSONBinary(bldr.buffer.result())
  }
  def apply(array : ByteString, subtype : BinarySubtype) : BSONBinary = {
    val bldr = Builder()
    bldr.binary(array, subtype)
    new BSONBinary(bldr.buffer.result())
  }
}

case object BSONUndefined extends BSONValue {
  val buffer = ByteString.empty
  def code = UndefinedCode
  def value : Unit = {}
}

case class BSONObjectID private[bson] (buffer : ByteString) extends BSONValue {
  def code = ObjectIDCode
  def value : Array[Byte] = { buffer.iterator.getBytes(12) }
}

object BSONObjectID {
  def apply(bytes : Array[Byte]) : BSONObjectID = {
    val bldr = Builder()
    bldr.objectID(bytes)
    new BSONObjectID(bldr.buffer.result)
  }
}

case class BSONBoolean private[bson] (buffer : ByteString) extends BSONValue {
  def code = BooleanCode
  def value : Boolean = {
    if (buffer.iterator.getByte == 0) false else true
  }
}

object BSONBoolean {
  def apply(value : Boolean) : BSONBoolean = {
    val bldr = Builder()
    bldr.boolean(value)
    new BSONBoolean(bldr.buffer.result)
  }
}

case class BSONDate private[bson] (buffer : ByteString) extends BSONValue {
  def code = DateCode
  def value : Long = {
    buffer.iterator.getLong
  }
  def toDate : Date = { new Date(buffer.iterator.getLong) }
}

object BSONDate {
  def apply(d : Long) : BSONDate = {
    val buffer = ByteString.newBuilder
    buffer.putLong(d)(ByteOrder.LITTLE_ENDIAN)
    new BSONDate(buffer.result())
  }
  def apply(d : Date) : BSONDate = apply(d.getTime)
}

case object BSONNull extends BSONValue {
  val buffer = ByteString.empty
  def code = NullCode
  def value : Unit = {}
}

case class BSONRegex private[bson] (buffer : ByteString) extends BSONValue {
  def code = RegexCode
  def value : Regex = {
    buffer.iterator.getRegex
  }
}

object BSONRegex {
  def apply(r : Regex) : BSONRegex = {
    val bldr = ByteString.newBuilder
    bldr.putRegex(r)
    BSONRegex(bldr.result())
  }
}

case class BSONDBPointer private[bson] (buffer : ByteString) extends BSONValue {
  def code = DBPointerCode
  def value : (String, Array[Byte]) = {
    val itr = buffer.iterator
    itr.getStr -> itr.getBytes(12)
  }
}

object BSONDBPointer {
  def apply(referent : String, objectID : Array[Byte]) : BSONDBPointer = {
    val bldr = Builder()
    bldr.dbPointer(referent, objectID)
    new BSONDBPointer(bldr.buffer.result())
  }
}

case class BSONJsCode private[bson] (buffer : ByteString) extends BSONValue {
  def code = JavaScriptCode
  def value : String = {
    buffer.iterator.getStr
  }
}

object BSONJsCode {
  def apply(s : String) : BSONJsCode = {
    val buffer = ByteString.newBuilder
    buffer.putStr(s)
    new BSONJsCode(buffer.result())
  }
}

case class BSONSymbol private[bson] (buffer : ByteString) extends BSONValue {
  def code = SymbolCode
  def value : String = {
    buffer.iterator.getStr
  }
}

object BSONSymbol {
  def apply(s : String) : BSONSymbol = {
    val buffer = ByteString.newBuilder
    buffer.putStr(s)
    new BSONSymbol(buffer.result())
  }
}

case class BSONScopedJsCode private[bson] (buffer : ByteString) extends BSONValue {
  def code = ScopedJSCode
  def value : (String, BSONObject) = {
    val itr = buffer.iterator
    itr.getInt
    val code = itr.getStr
    val obj = itr.getObj
    code -> obj
  }
}

object BSONScopedJsCode {
  def apply(code : String, scope : BSONObject) : BSONScopedJsCode = {
    val bldr = Builder()
    bldr.scopedJsCode(code, scope)
    new BSONScopedJsCode(bldr.buffer.result())
  }
}

case class BSONInteger private[bson] (buffer : ByteString) extends BSONValue {
  def code = IntegerCode
  def value : Int = {
    buffer.iterator.getInt
  }
}

object BSONInteger {
  def apply(i : Int) : BSONInteger = {
    val buffer = ByteString.newBuilder
    buffer.putInt(i)(ByteOrder.LITTLE_ENDIAN)
    new BSONInteger(buffer.result())

  }
}

case class BSONTimestamp private[bson] (buffer : ByteString) extends BSONValue {
  def code = TimestampCode
  def value : Long = {
    buffer.iterator.getLong
  }
}

object BSONTimestamp {
  def apply(t : Long) : BSONTimestamp = {
    val buffer = ByteString.newBuilder
    buffer.putLong(t)(ByteOrder.LITTLE_ENDIAN)
    new BSONTimestamp(buffer.result())
  }
}

case class BSONLong private[bson] (buffer : ByteString) extends BSONValue {
  def code = LongCode
  def value : Long = {
    buffer.iterator.getLong
  }
}

object BSONLong {
  def apply(l : Long) : BSONLong = {
    val buffer = ByteString.newBuilder
    buffer.putLong(l)(ByteOrder.LITTLE_ENDIAN)
    new BSONLong(buffer.result())
  }
}

