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

import scala.annotation.switch
import scala.collection.{ Map, MapLike }
import scala.util.matching.Regex
import scala.language.implicitConversions

trait BSONValue {
  implicit val byteOrder = ByteOrder.LITTLE_ENDIAN
  private[bson] val buffer : ByteString
  def value : Any

  def code : TypeCode

  def length : Int = buffer.length

  override def toString = value.toString

}

trait BSONDocument extends BSONValue {

  class DocumentIterator private[bson] (docItr : ByteIterator) extends Iterator[(String, BSONValue)] {

    val byteLen = docItr.getInt
    val itr = docItr.slice(0, byteLen - 5) // remove 4 for the byteLen, and 1 for the terminating 0

    private def getKey : String = { itr.getCStr }

    def hasNext : Boolean = itr.hasNext

    private def advance(i : ByteIterator, code : Byte) : Int = {
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

    private def toNext[T](i : ByteIterator)(f : (ByteIterator, TypeCode, String, Int) ⇒ T) : T = {
      val code = i.getByte
      val key = i.getCStr
      val save = i.clone()
      val len = advance(i, code)
      f(save.slice(0, len), TypeCode(code), key, len)
    }

    override def size : Int = {
      var result = 0
      val i = itr.clone()
      while (i.hasNext) {
        val code = i.getByte
        i.skipCStr
        advance(i, code)
        result += 1
      }
      result
    }

    def next() : (String, BSONValue) = {
      if (hasNext) {
        val save : ByteIterator = itr.clone()
        toNext[(String, BSONValue)](itr) {
          case (an_itr : ByteIterator, tc : TypeCode, key : String, len : Int) ⇒ {
            val bytes = an_itr.toByteString
            tc match {
              case IntegerCode ⇒ key -> BSONInteger(bytes)
              case LongCode ⇒ key -> BSONLong(bytes)
              case DoubleCode ⇒ key -> BSONDouble(bytes)
              case StringCode ⇒ key -> BSONString(bytes)
              case ObjectCode ⇒ key -> BSONObject(bytes)
              case ArrayCode ⇒ key -> BSONArray(bytes)
              case BinaryCode ⇒ key -> BSONBinary(bytes)
              case ObjectIDCode ⇒ key -> BSONObjectID(bytes)
              case BooleanCode ⇒ key -> BSONBoolean(bytes)
              case DateCode ⇒ key -> BSONDate(bytes)
              case NullCode ⇒ key -> BSONNull
              case RegexCode ⇒ key -> BSONRegex(bytes)
              case JavaScriptCode ⇒ key -> BSONJsCode(bytes)
              case ScopedJSCode ⇒ key -> BSONScopedJsCode(bytes)
              case DBPointerCode ⇒ key -> BSONDBPointer(bytes)
              case TimestampCode ⇒ key -> BSONTimestamp(bytes)
              case UndefinedCode ⇒ key -> BSONUndefined
              case SymbolCode ⇒ key -> BSONSymbol(bytes)
              case x : TypeCode ⇒ throw new NoSuchElementException(s"Unrecognized: $x")
            }
          }
        }
      } else {
        Iterator.empty.next()
      }
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
  override def toString = value
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
    val bldr = BSONBuilder()
    bldr.buffer ++= content
    bldr.append(kv._1, kv._2.asInstanceOf[BSONValue])
    bldr.result()
  }

  def iterator : Iterator[(String, BSONValue)] = {
    new DocumentIterator(buffer.iterator)
  }

  override def foreach[U](f : ((String, BSONValue)) ⇒ U) : Unit = {
    iterator.foreach[U](f)
  }

  override def size : Int = iterator.size

  def get(key_to_find : String) : Option[BSONValue] = {
    iterator.find { case (key, value) ⇒ key == key_to_find } map { case (key, value) ⇒ value }
  }

  /** Get with conversion to another type
    *
    * Gets the BSONValue associated with the provided `key`, and converts it to `T` via the BSONCodec[T].
    *
    * @tparam T The type to which the call wishes the BSONValue to be decoded.
    * @param key The key to look up in the object to obtain the value
    * @return Success(None) if the key is not found, Success(Some(T)) if the key is found and converted successfully,
    * Failure(Throwable) if the conversion failed
    *
    * If there is no matching value, or the value could not be deserialized or converted, returns a `None`.
    */
  def getAs[T, B <: BSONValue](key : String)(implicit codec : BSONCodec[T, B]) : T = {
    get(key) match {
      case Some(value : B @unchecked) if value.code == codec.code ⇒ codec.read(value)
      case Some(value : BSONValue) ⇒ throw new IllegalArgumentException(
        s"Expected type ${codec.typeName} for key '$key' but got type ${value.code.typeName}")
      case None ⇒ throw new NoSuchElementException(key)
    }
  }

  def getAsObject[T](key : String)(implicit codec : BSONCodec[T, BSONObject]) : T = getAs[T, BSONObject](key)(codec)
  def getAsString(key : String) : String = getAs[String, BSONString](key)
  def getAsInt(key : String) : Int = getAs[Int, BSONInteger](key)
  def getAsDouble(key : String) : Double = getAs[Double, BSONDouble](key)
  def getAsDate(key : String) : Date = getAs[Date, BSONDate](key)

  override def empty : BSONObject = BSONObject.empty

  override def seq : Map[String, BSONValue] = iterator.toMap

  def code = ObjectCode

  def toMap : Map[String, BSONValue] = iterator.toMap

  override def toSeq : Seq[(String, BSONValue)] = iterator.toSeq

  def value : Map[String, BSONValue] = iterator.toMap

  def compact : BSONObject = BSONObject(buffer.compact)

  override def toString() : String = {
    val s = new StringBuilder
    val itr = iterator
    if (itr.isEmpty) {
      s.append("{}")
    } else {
      s.append("{ ")
      for ((k, v) ← itr) {
        s.append(k).append("->").append(v).append(", ")
      }
      s.setLength(s.length - 2)
      s.append(" }")
    }
    s.toString()
  }
}

object BSONObject {

  object empty extends BSONObject(ByteString.empty)

  def apply(data : Map[String, Any]) : BSONObject = from(data.toSeq)
  def apply(data : (String, Any)*) : BSONObject = from(data.toSeq)

  def apply[T](key : String, value : T)(implicit codec : BSONCodec[T, BSONObject]) : BSONObject = {
    val bldr = BSONBuilder()
    bldr.obj(key, BSONObject.from[T](value))
    bldr.result()
  }

  def from(data : Seq[(String, Any)]) : BSONObject = {
    val bldr = BSONBuilder()
    bldr.append(data)
    bldr.result()
  }

  def from[T](data : T)(implicit codec : BSONCodec[T, BSONObject]) : BSONObject = codec.write(data)

  def newBuilder = BSONBuilder()
}

case class BSONArray private[bson] (buffer : ByteString) extends BSONDocument {
  def code = ArrayCode
  def value : Iterator[BSONValue] = iterator
  def compact : BSONArray = BSONArray(buffer.compact)

  def iterator : Iterator[BSONValue] = {
    val docItr = new DocumentIterator(buffer.iterator)
    docItr.map { case (key, value) ⇒ value }
  }

  override def toString : String = {
    val s = new StringBuilder
    val itr = iterator
    if (itr.isEmpty) {
      s.append("[]")
    } else {
      s.append("[ ")
      for (v ← itr) {
        s.append(v.toString).append(", ")
      }
      s.setLength(s.length - 2)
      s.append(" ]")
    }
    s.toString()
  }
}

object BSONArray {
  def apply(data : Any, data2 : Any*) : BSONArray = {
    val bldr = BSONBuilder()
    bldr.array(Seq(data) ++ data2)
    new BSONArray(bldr.buffer.result())
  }
  def apply(data : Seq[Any]) : BSONArray = {
    val bldr = BSONBuilder()
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

  override def toString = "BSONBinary(" + value.toString() + ")"

  def subtype : BinarySubtype = { BinarySubtype(buffer.iterator.drop(4).getByte) }
}

object BSONBinary {
  def apply(array : Array[Byte], subtype : BinarySubtype) : BSONBinary = {
    val bldr = BSONBuilder()
    bldr.binary(array, subtype)
    new BSONBinary(bldr.buffer.result())
  }
  def apply(array : ByteString, subtype : BinarySubtype) : BSONBinary = {
    val bldr = BSONBuilder()
    bldr.binary(array, subtype)
    new BSONBinary(bldr.buffer.result())
  }
}

case object BSONUndefined extends BSONValue {
  val buffer = ByteString.empty
  def code = UndefinedCode
  def value : Unit = {}
  override def toString = "BSONUndefined"
}

case class BSONObjectID private[bson] (buffer : ByteString) extends BSONValue {
  def code = ObjectIDCode
  def value : Array[Byte] = { buffer.iterator.getBytes(12) }
  override def toString = "BSONObjectId(" + value + ")"
}

object BSONObjectID {
  def apply(bytes : Array[Byte]) : BSONObjectID = {
    val bldr = BSONBuilder()
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
    val bldr = BSONBuilder()
    bldr.boolean(value)
    new BSONBoolean(bldr.buffer.result())
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
    val bldr = BSONBuilder()
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
    val bldr = BSONBuilder()
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
