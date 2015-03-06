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

import akka.util.{ ByteStringBuilder, ByteIterator, ByteString }

import scala.annotation.switch
import scala.collection._
import scala.language.implicitConversions
import scala.language.postfixOps

trait BSONValue {
  def code : TypeCode
  def value : Any
  def typeName : String = code.typeName
  private[bson] def addTo(bldr : ByteStringBuilder) : Unit
  def toByteString : ByteString = {
    val bldr = ByteString.newBuilder
    this.addTo(bldr)
    bldr.result()
  }
  def pair : (Byte, ByteIterator) = code.code → toByteString.iterator
  def buffer : ByteString = toByteString
  override def toString = value.toString
}

object BSONValue {
  implicit val byteOrder = ByteOrder.LITTLE_ENDIAN

  def apply(code : TypeCode, byteItr : ByteIterator) : BSONValue = apply(code.code, byteItr)
  def apply(code : TypeCode, bytes : ByteString) : BSONValue = apply(code.code, bytes.iterator)
  def apply(pair : (Byte, ByteIterator)) : BSONValue = apply(pair._1, pair._2)
  def apply(byte : Byte, byteItr : ByteIterator) : BSONValue = {
    (byte : @switch) match {
      case 1  ⇒ BSONDouble(byteItr)
      case 2  ⇒ BSONString(byteItr)
      case 3  ⇒ BSONObject(byteItr)
      case 4  ⇒ BSONArray(byteItr)
      case 5  ⇒ BSONBinary(byteItr)
      case 6  ⇒ BSONUndefined
      case 7  ⇒ BSONObjectID(byteItr)
      case 8  ⇒ BSONBoolean(byteItr)
      case 9  ⇒ BSONDate(byteItr)
      case 10 ⇒ BSONNull
      case 11 ⇒ BSONRegex(byteItr)
      case 12 ⇒ BSONDBPointer(byteItr)
      case 13 ⇒ BSONJsCode(byteItr)
      case 14 ⇒ BSONSymbol(byteItr)
      case 15 ⇒ BSONScopedJsCode(byteItr)
      case 16 ⇒ BSONInteger(byteItr)
      case 17 ⇒ BSONTimestamp(byteItr)
      case 18 ⇒ BSONLong(byteItr)
      case _  ⇒ throw new NoSuchElementException("Unrecognized BSON TypeCode")
    }
  }
}

case class BSONDouble( final val value : Double) extends BSONValue {
  final val code : TypeCode = DoubleCode
  override def addTo(b : ByteStringBuilder) : Unit = {
    b.putDouble(value)
  }
}

object BSONDouble {
  def apply(itr : ByteIterator) : BSONDouble = new BSONDouble(itr.getDouble)
  def apply(buff : ByteString) : BSONDouble = BSONDouble(buff.iterator)
}

case class BSONString( final val value : String) extends BSONValue {
  final val code = StringCode
  def addTo(b : ByteStringBuilder) : Unit = {
    b.putStr(value)
  }
  override def toString = value
}

object BSONString {
  private[bson] def apply(itr : ByteIterator) : BSONString = new BSONString(itr.getStr)
  private[bson] def apply(buffer : ByteString) : BSONString = BSONString(buffer.iterator)
}

case class BSONObject private[bson] ( final val doc : BSONDocument) extends BSONValue
  with MapLike[String, BSONValue, BSONObject] with Map[String, BSONValue] {
  final val code : TypeCode = ObjectCode

  final def value : Map[String, BSONValue] = toMap

  def toMap : Map[String, BSONValue] = doc.map {
    case (k, (b, bi)) ⇒
      k -> BSONValue(b, bi.clone())
  }

  private[bson] def addTo(bldr : ByteStringBuilder) : Unit = {
    bldr ++= doc.toByteString
  }

  override def equals(other : Any) : Boolean = {
    other match {
      case that : BSONObject ⇒ this.doc.equals(that.doc)
      case _ ⇒ false
    }
  }

  def get(key : String) : Option[BSONValue] = doc.get(key).map { case (b, bi) ⇒ BSONValue(b, bi.clone()) }

  def +[B1 >: BSONValue](kv : (String, B1)) : BSONObject = {
    val pair = kv._1 → kv._2.asInstanceOf[BSONValue].pair
    BSONObject(doc + pair)
  }

  def -(key : String) : BSONObject = BSONObject(doc.data - key)

  def iterator : Iterator[(String, BSONValue)] = {
    new Iterator[(String, BSONValue)] {
      val i = doc.iterator
      def hasNext : Boolean = i.hasNext
      def next() : (String, BSONValue) = {
        val (key, (b, bi)) = i.next()
        key → BSONValue(b, bi.clone())
      }
    }
  }

  override def size : Int = iterator.size

  /*  def filter(filt : (String,BSONValue) ⇒ Boolean) : BSONObject = {
    BSONObject(value.filter { case (key,(typeCode,itr)) ⇒
      filt(key,BSONValue(typeCode,itr))
    })
  }
*/
  override def contains(key : String) : Boolean = doc.contains(key)

  def getTypeCode(keyToFind : String) : TypeCode = {
    doc.get(keyToFind) match {
      case Some(v) ⇒ TypeCode(v._1)
      case None    ⇒ NotACode
    }
  }

  def getObject(keyToFind : String) : Option[BSONObject] = {
    get(keyToFind) map { case (v) ⇒ v.asInstanceOf[BSONObject] }
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
      case Some(v : BSONValue) if v.code == ArrayCode ⇒ doc.asInstanceOf[BSONArray].as[T, B]
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

  def toAnyMap : Map[String, Any] = toMap.map { case (k, v) ⇒ k -> v.value }

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

  override protected[this] def newBuilder : mutable.Builder[(String, BSONValue), BSONObject] = BSONBuilder()

  override def empty : BSONObject = BSONObject.empty
}

object BSONObject {

  def newBuilder = BSONBuilder()

  object empty extends BSONObject(BSONDocument.empty)

  private[bson] def apply(buffer : ByteString) : BSONObject = BSONObject(buffer.iterator)
  private[bson] def apply(itr : ByteIterator) : BSONObject = new BSONObject(BSONDocument.apply(itr))

  def apply() : BSONObject = empty

  def apply(data : (String, Any)*) : BSONObject = from(data.toSeq)

  def apply(data : Map[String, Any]) : BSONObject = from(data.toSeq)

  def apply[T](key : String, value : T)(implicit codec : BSONCodec[T, BSONObject]) : BSONObject = {
    val bldr = BSONBuilder()
    bldr.obj(key, BSONObject(value))
    bldr.toBSONObject
  }

  def apply[T](value : T)(implicit codec : BSONCodec[T, BSONObject]) : BSONObject = codec.write(value)

  def from(data : Seq[(String, Any)]) : BSONObject = {
    val bldr = BSONBuilder()
    bldr.append(data)
    bldr.toBSONObject
  }

}

case class BSONArray private[bson] ( final val doc : BSONDocument) extends BSONValue
  with SeqLike[BSONValue, BSONArray] {
  final val code : TypeCode = ArrayCode
  final def value : Seq[BSONValue] = seq

  override def equals(other : Any) : Boolean = {
    other match {
      case that : BSONArray ⇒ this.doc.equals(that.doc)
      case _ ⇒ false
    }
  }

  private[bson] def addTo(bldr : ByteStringBuilder) : Unit = {
    bldr ++= doc.toByteString
  }

  def length : Int = doc.size

  def iterator : Iterator[BSONValue] = {
    new Iterator[BSONValue] {
      val i = doc.iterator
      def hasNext : Boolean = i.hasNext
      def next() : BSONValue = {
        val (key, (b, bi)) = i.next()
        BSONValue(b, bi)
      }
    }
  }

  def apply(index : Int) = {
    doc.get(index.toString) match {
      case Some((typeCode, byteIterator)) ⇒ BSONValue(typeCode, byteIterator)
      case None ⇒ throw new NoSuchElementException(index.toString)
    }
  }

  protected[this] def newBuilder : mutable.Builder[BSONValue, BSONArray] = ???

  def seq : Seq[BSONValue] = doc.values.map { case (b, bi) ⇒ BSONValue(b, bi) } toSeq

  override def toString() : String = {
    val s = new StringBuilder
    val itr = iterator
    if (itr.isEmpty) {
      s.append("[]")
    } else {
      s.append("[ ")
      for (v ← itr) {
        s.append(v.toString()).append(", ")
      }
      s.setLength(s.length - 2)
      s.append(" ]")
    }
    s.toString()
  }

  def asArray : Array[BSONValue] = doc.values.map { case (b, bi) ⇒ BSONValue(b, bi) } toArray

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

  object empty extends BSONArray(BSONDocument.empty)

  def apply(buffer : ByteString) : BSONArray = BSONArray(buffer.iterator)
  def apply(itr : ByteIterator) : BSONArray = new BSONArray(BSONDocument(itr))
  def apply(values : Iterator[BSONValue]) : BSONArray = {
    val pairs : Iterator[(String, (Byte, ByteIterator))] = values.zipWithIndex.map {
      case (v, i) ⇒ i.toString → (v.code.code → v.toByteString.iterator)
    }
    new BSONArray(BSONDocument(pairs.toMap))
  }

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
    BSONArray(bldr.toByteString)
  }

  def apply[T, B <: BSONValue](data : Iterable[T])(implicit codec : BSONCodec[T, B]) : BSONArray = {
    val bldr = BSONBuilder()
    bldr.codecArray(data)
    BSONArray(bldr.buffer.result())
  }

  def apply[T, B <: BSONValue](data1 : T, data : T*)(implicit codec : BSONCodec[T, B]) : BSONArray = {
    val bldr = BSONBuilder()
    val values = Seq(data1) ++ data
    values.zipWithIndex.foreach {
      case (v, i) ⇒ bldr.append(i.toString, codec.write(v))
    }
    bldr.toByteString
    BSONArray(bldr.toByteString)
  }
}

case class BSONBinary( final val value : (BinarySubtype, Array[Byte])) extends BSONValue {
  final val code = BinaryCode
  def array : Array[Byte] = value._2
  def subtype : BinarySubtype = value._1
  def addTo(b : ByteStringBuilder) : Unit = {
    b.putInt(array.size)
    b.putByte(subtype.code)
    b.putBytes(array)
  }
  override def toString = "BSONBinary" + value.toString
  override def equals(other : Any) : Boolean = {
    if (!other.isInstanceOf[BSONBinary])
      return false
    val that = other.asInstanceOf[BSONBinary]
    that.subtype == this.subtype && java.util.Arrays.equals(that.array, this.array)
  }
}

object BSONBinary {
  private[bson] def apply(itr : ByteIterator) : BSONBinary = {
    val len = itr.getInt
    val st = BinarySubtype(itr.getByte)
    val value = itr.getBytes(len)
    BSONBinary(st → value)
  }
  private[bson] def apply(bs : ByteString) : BSONBinary = BSONBinary(bs.iterator)
  def apply(array : ByteString, st : BinarySubtype) : BSONBinary = BSONBinary(st → array.toArray[Byte])
  def apply(st : BinarySubtype, array : ByteString) : BSONBinary = BSONBinary(st → array.toArray[Byte])
  def apply(array : Array[Byte], st : BinarySubtype) : BSONBinary = BSONBinary(st → array)
  def apply(st : BinarySubtype, array : Array[Byte]) : BSONBinary = BSONBinary(st → array)
}

case object BSONUndefined extends BSONValue {
  final val code = UndefinedCode
  final val value : Unit = {}
  def addTo(b : ByteStringBuilder) : Unit = {
    // nothing to do
  }
  override def toString = "BSONUndefined"
}

case class BSONObjectID(value : Array[Byte]) extends BSONValue {
  final val code = ObjectIDCode
  def addTo(b : ByteStringBuilder) : Unit = {
    b.putBytes(value, 0, 12)
  }
  override def toString = "BSONObjectId(" + value + ")"
}

object BSONObjectID {
  private[bson] def apply(itr : ByteIterator) : BSONObjectID = new BSONObjectID(itr.getBytes(12))
  private[bson] def apply(buffer : ByteString) : BSONObjectID = BSONObjectID(buffer.iterator)
}

case class BSONBoolean( final val value : Boolean) extends BSONValue {
  final val code = BooleanCode
  def addTo(b : ByteStringBuilder) : Unit = {
    b.putByte(if (value) 1 else 0)
  }
}

object BSONBoolean {
  private[bson] def apply(itr : ByteIterator) : BSONBoolean = new BSONBoolean(if (itr.getByte == 0) false else true)
  private[bson] def apply(buffer : ByteString) : BSONBoolean = BSONBoolean(buffer.iterator)
}

case class BSONDate( final val value : Date) extends BSONValue {
  final val code = DateCode
  def toLong : Long = value.getTime
  def getTime : Long = value.getTime
  def addTo(b : ByteStringBuilder) : Unit = {
    b.putLong(toLong)
  }
}

object BSONDate {
  private[bson] def apply(itr : ByteIterator) : BSONDate = new BSONDate(new Date(itr.getLong))
  private[bson] def apply(buffer : ByteString) : BSONDate = BSONDate(buffer.iterator)
  def apply(d : Long) : BSONDate = BSONDate(new Date(d))
}

case object BSONNull extends BSONValue {
  final val code = NullCode
  final val value : Unit = {}
  def addTo(b : ByteStringBuilder) : Unit = {
    // nothing to do
  }
}

case class BSONRegex( final val value : Pattern) extends BSONValue {
  final val code = RegexCode
  def addTo(b : ByteStringBuilder) : Unit = {
    b.putRegex(value)
  }
  override def equals(other : Any) : Boolean = {
    if (!other.isInstanceOf[BSONRegex])
      return false
    val that = other.asInstanceOf[BSONRegex]
    this.code == that.code &&
      this.value.pattern.equals(that.value.pattern) &&
      (this.value.flags == that.value.flags)
  }
}

object BSONRegex {
  private[bson] def apply(itr : ByteIterator) : BSONRegex = new BSONRegex(itr.getRegex)
  private[bson] def apply(buffer : ByteString) : BSONRegex = BSONRegex(buffer.iterator)
}

case class BSONDBPointer( final val value : (String, Array[Byte])) extends BSONValue {
  def code = DBPointerCode
  def addTo(b : ByteStringBuilder) : Unit = {
    b.putStr(value._1)
    b.putBytes(value._2, 0, 12)
  }
}

object BSONDBPointer {
  private[bson] def apply(itr : ByteIterator) : BSONDBPointer = new BSONDBPointer(itr.getStr → itr.getBytes(12))
  private[bson] def apply(buffer : ByteString) : BSONDBPointer = BSONDBPointer(buffer.iterator)
  def apply(referent : String, objectID : Array[Byte]) : BSONDBPointer = new BSONDBPointer(referent → objectID)
}

case class BSONJsCode( final val value : String) extends BSONValue {
  final val code = JavaScriptCode
  def addTo(b : ByteStringBuilder) : Unit = {
    b.putStr(value)
  }
}

object BSONJsCode {
  private[bson] def apply(itr : ByteIterator) : BSONJsCode = new BSONJsCode(itr.getStr)
  private[bson] def apply(buffer : ByteString) : BSONJsCode = BSONJsCode(buffer.iterator)
}

case class BSONSymbol( final val value : String) extends BSONValue {
  final val code = SymbolCode
  def addTo(b : ByteStringBuilder) : Unit = {
    b.putStr(value)
  }
}

object BSONSymbol {
  private[bson] def apply(itr : ByteIterator) : BSONSymbol = new BSONSymbol(itr.getStr)
  private[bson] def apply(buffer : ByteString) : BSONSymbol = BSONSymbol(buffer.iterator)
}

case class BSONScopedJsCode( final val value : (String, BSONObject)) extends BSONValue {
  final val code = ScopedJSCode
  def jsCode : String = value._1
  def scope : BSONObject = value._2
  def addTo(b : ByteStringBuilder) : Unit = {
    val obj = scope.toByteString
    val len = jsCode.length + 9 + obj.length
    b.putInt(len)
    b.putStr(jsCode)
    b ++= obj
  }
}

object BSONScopedJsCode {
  private[bson] def apply(itr : ByteIterator) : BSONScopedJsCode = {
    val len = itr.getInt
    val code = itr.getStr
    val obj = itr.getObj
    new BSONScopedJsCode(code → obj)
  }
  private[bson] def apply(buffer : ByteString) : BSONScopedJsCode = BSONScopedJsCode(buffer.iterator)
  def apply(code : String, scope : BSONObject) : BSONScopedJsCode = BSONScopedJsCode(code → scope)
}

case class BSONInteger( final val value : Int) extends BSONValue {
  final val code = IntegerCode
  def addTo(b : ByteStringBuilder) : Unit = {
    b.putInt(value)
  }
}

object BSONInteger {
  private[bson] def apply(itr : ByteIterator) : BSONInteger = new BSONInteger(itr.getInt)
  private[bson] def apply(buffer : ByteString) : BSONInteger = BSONInteger(buffer.iterator)
}

case class BSONTimestamp( final val value : Long) extends BSONValue {
  final val code = TimestampCode
  def addTo(b : ByteStringBuilder) : Unit = {
    b.putLong(value)
  }
}

object BSONTimestamp {
  private[bson] def apply(itr : ByteIterator) : BSONTimestamp = new BSONTimestamp(itr.getLong)
  private[bson] def apply(buffer : ByteString) : BSONTimestamp = BSONTimestamp(buffer.iterator)
}

case class BSONLong( final val value : Long) extends BSONValue {
  final val code = LongCode
  def addTo(b : ByteStringBuilder) : Unit = {
    b.putLong(value)
  }
}

object BSONLong {
  private[bson] def apply(itr : ByteIterator) : BSONLong = new BSONLong(itr.getLong)
  private[bson] def apply(buffer : ByteString) : BSONLong = BSONLong(buffer.iterator)
}
