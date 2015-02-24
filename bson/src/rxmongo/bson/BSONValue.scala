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
import scala.collection.{ mutable, Map, MapLike, immutable }
import scala.util.matching.Regex
import scala.language.implicitConversions

trait BSONValue {
  def code : TypeCode
  def value : Any

  def typeName : String = code.typeName

  def buffer : ByteString

  override def toString = value.toString

}

object BSONValue {
  implicit val byteOrder = ByteOrder.LITTLE_ENDIAN

  def apply(code : TypeCode, bytes : ByteString) : BSONValue = {
    code match {
      case IntegerCode ⇒ BSONInteger(bytes)
      case LongCode ⇒ BSONLong(bytes)
      case DoubleCode ⇒ BSONDouble(bytes)
      case StringCode ⇒ BSONString(bytes)
      case ObjectCode ⇒ BSONObject(bytes)
      case ArrayCode ⇒ BSONArray(bytes)
      case BinaryCode ⇒ BSONBinary(bytes)
      case ObjectIDCode ⇒ BSONObjectID(bytes)
      case BooleanCode ⇒ BSONBoolean(bytes)
      case DateCode ⇒ BSONDate(bytes)
      case NullCode ⇒ BSONNull
      case RegexCode ⇒ BSONRegex(bytes)
      case JavaScriptCode ⇒ BSONJsCode(bytes)
      case ScopedJSCode ⇒ BSONScopedJsCode(bytes)
      case DBPointerCode ⇒ BSONDBPointer(bytes)
      case TimestampCode ⇒ BSONTimestamp(bytes)
      case UndefinedCode ⇒ BSONUndefined
      case SymbolCode ⇒ BSONSymbol(bytes)
      case x : TypeCode ⇒ throw new NoSuchElementException(s"Unrecognized: $x")
    }

  }
}

abstract class BSONDocument extends BSONValue {

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

    lazy val _size : Int = {
      var result = 0
      val i = itr.clone()
      while (i.hasNext) {
        val code = i.getByte
        i.skipCStr
        advance(i, code)
        result = result + 1
      }
      result
    }

    override def size : Int = _size

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

case class BSONDouble private[bson] (value : Double) extends BSONValue {
  final val code : TypeCode = DoubleCode

  def buffer : ByteString = {
    val buffer = ByteString.newBuilder
    buffer.putDouble(value)(ByteOrder.LITTLE_ENDIAN)
    buffer.result()
  }
}

object BSONDouble {
  def apply(buff : ByteString) : BSONDouble = {
    new BSONDouble(buff.iterator.getDouble) { override val buffer : ByteString = buff }
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
    bldr.result
  }

  private[rxmongo] def byteIterator = buffer.iterator

  def iterator : Iterator[(String, BSONValue)] = {
    new DocumentIterator(buffer.iterator)
  }

  override def foreach[U](f : ((String, BSONValue)) ⇒ U) : Unit = {
    iterator.foreach[U](f)
  }

  override def size : Int = iterator.size

  def get(keyToFind : String) : Option[BSONValue] = {
    iterator.find {
      case (key, value) ⇒ key == keyToFind
    } map {
      case (key, value) ⇒ value
    }
  }

  def getTypeCode(keyToFind : String) : TypeCode = {
    iterator.find {
      case (key, value) ⇒ key == keyToFind
    } map {
      case (key, value) ⇒ value.code
    } match {
      case Some(tc) ⇒ tc
      case None     ⇒ NotACode
    }
  }

  def getObject(keyToFind : String) : Option[BSONObject] = {
    iterator.find {
      case (key, value) ⇒ value.code == ObjectCode && key == keyToFind
    } map {
      case (key, value) ⇒ value.asInstanceOf[BSONObject]
    }
  }

  def getObj(keyToFind : String) : BSONObject = {
    getObject(keyToFind) match {
      case Some(obj) ⇒ obj
      case _ ⇒ throw new NoSuchElementException(keyToFind)
    }
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
        s"Expected type ${codec.typeName} for key '$key' but got type ${value.typeName}")
      case None ⇒ throw new NoSuchElementException(key)
    }
  }

  def getAsObject[T](key : String)(implicit codec : BSONCodec[T, BSONObject]) : T = {
    getAs[T, BSONObject](key)(codec)
  }

  def getAsSeq[T, B <: BSONValue](key : String)(implicit codec : BSONCodec[T, B]) : Iterator[T] = {
    get(key) match {
      case Some(v : BSONValue) if v.code == ArrayCode ⇒ value.asInstanceOf[BSONArray].as[T, B]
      case Some(v : BSONValue) ⇒ throw new IllegalArgumentException(
        s"Expected type BSONArray for key '$key' but got type ${v.typeName}"
      )
      case None ⇒ throw new NoSuchElementException(key)
    }
  }

  def getAsMap[T, B <: BSONValue](key : String)(implicit codec : BSONCodec[T, B]) : immutable.Map[String, T] = {
    iterator.map {
      case (k, v : B @unchecked) if v.code == codec.code ⇒ k -> codec.read(v)
      case (k, v) ⇒
        throw new IllegalArgumentException(
          s"Expected type ${codec.typeName} for key '$k' but got type ${v.typeName}")
    }.toMap
  }

  def getAsString(key : String) : String = getAs[String, BSONString](key)
  def getAsInt(key : String) : Int = getAs[Int, BSONInteger](key)
  def getAsLong(key : String) : Long = getAs[Long, BSONLong](key)
  def getAsDouble(key : String) : Double = getAs[Double, BSONDouble](key)
  def getAsDate(key : String) : Date = getAs[Date, BSONDate](key)
  def getAsBoolean(key : String) : Boolean = getAs[Boolean, BSONBoolean](key)
  def getAsArray[T, B <: BSONValue](key : String)(implicit codec : BSONCodec[T, B]) : Seq[T] = {
    getAsSeq[T, B](key).toSeq
  }

  def getOptionalObject[T](key : String)(implicit codec : BSONCodec[T, BSONObject]) : Option[T] = {
    try { Some(getAs[T, BSONObject](key)(codec)) } catch { case x : Exception ⇒ None }
  }
  def getOptionalArray[T, B <: BSONValue](key : String)(implicit codec : BSONCodec[T, B]) : Option[Seq[T]] = {
    try { Some(getAsArray[T, B](key)(codec)) } catch { case x : Exception ⇒ None }
  }
  def getOptionalString(key : String) : Option[String] = {
    try { Some(getAs[String, BSONString](key)) } catch { case x : Exception ⇒ None }
  }
  def getOptionalInt(key : String) : Option[Int] = {
    try { Some(getAs[Int, BSONInteger](key)) } catch { case x : Exception ⇒ None }
  }
  def getOptionalDouble(key : String) : Option[Double] = {
    try { Some(getAs[Double, BSONDouble](key)) } catch { case x : Exception ⇒ None }
  }
  def getOptionalDate(key : String) : Option[Date] = {
    try { Some(getAs[Date, BSONDate](key)) } catch { case x : Exception ⇒ None }
  }
  def getOptionalBoolean(key : String) : Option[Boolean] = {
    try { Some(getAs[Boolean, BSONBoolean](key)) } catch { case x : Exception ⇒ None }
  }

  def matches(other : BSONObject) : Boolean = {
    for ((k, v) ← other) {
      get(k) match {
        case Some(value) ⇒ if (value != v) return false
        case None ⇒ return false
      }
    }
    true
  }

  override def empty : BSONObject = BSONObject.empty

  override def seq : Map[String, BSONValue] = iterator.toMap

  def code = ObjectCode

  def toMap : Map[String, BSONValue] = iterator.toMap

  def toAnyMap : Map[String, Any] = iterator.toMap.map { case (k, v) ⇒ k -> v.value }

  override def toSeq : Seq[(String, BSONValue)] = iterator.toSeq

  def value : Map[String, BSONValue] = iterator.toMap

  def compact : BSONObject = BSONObject(buffer.compact)

  def to[T](implicit codec : BSONCodec[T, BSONObject]) : T = codec.read(this)

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

  def newBuilder = BSONBuilder()

  object empty extends BSONObject(BSONBuilder().toByteString)

  def apply() : BSONObject = empty

  def apply(data : Map[String, Any]) : BSONObject = from(data.toSeq)
  def apply(data : (String, Any)*) : BSONObject = from(data.toSeq)

  def apply[T](key : String, value : T)(implicit codec : BSONCodec[T, BSONObject]) : BSONObject = {
    val bldr = BSONBuilder()
    bldr.obj(key, BSONObject(value))
    bldr.result
  }

  def apply[T](value : T)(implicit codec : BSONCodec[T, BSONObject]) : BSONObject = codec.write(value)

  def from(data : Seq[(String, Any)]) : BSONObject = {
    val bldr = BSONBuilder()
    bldr.append(data)
    bldr.result
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

  def as[T, B <: BSONValue](implicit codec : BSONCodec[T, B]) : Iterator[T] = {
    iterator.map {
      case v : BSONValue if v.code == codec.code ⇒ codec.read(v.asInstanceOf[B])
      case v : BSONValue ⇒ throw new IllegalArgumentException(
        s"Expected type ${codec.typeName} but got type ${v.typeName}"
      )
    }
  }
}

object BSONArray {

  object empty extends BSONArray(BSONBuilder().toByteString)

  def apply() : BSONArray = empty

  def apply(objs : BSONObject*) : BSONArray = {
    val arrayBuilder = BSONBuilder()
    objs.zipWithIndex.foreach {
      case (v, i) ⇒
        arrayBuilder.append(i.toString, v)
    }
    BSONArray(arrayBuilder.toByteString)
  }

  def apply(data : Array[Any]) : BSONArray = {
    val bldr = BSONBuilder()
    bldr.array(data)
    new BSONArray(bldr.buffer.result())
  }

  def apply[T, B <: BSONValue](data : Iterable[T])(implicit codec : BSONCodec[T, B]) : BSONArray = {
    val bldr = BSONBuilder()
    bldr.codecArray(data)
    new BSONArray(bldr.buffer.result())
  }

  def apply[T, B <: BSONValue](data1 : T, data : T*)(implicit codec : BSONCodec[T, B]) : BSONArray = {
    val bldr = BSONBuilder()
    bldr.codecArray[T, B](Seq(data1) ++ data)
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
  def value : Date = {
    new Date(buffer.iterator.getLong)
  }
  def toLong : Long = { buffer.iterator.getLong }
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
