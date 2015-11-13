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
  override def equals(other : Any) : Boolean = {
    if (!other.isInstanceOf[BSONObjectID])
      return false
    val that = other.asInstanceOf[BSONObjectID]
    java.util.Arrays.equals(that.value, this.value)
  }
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
  final val value : Unit = ()
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
  override def equals(other : Any) : Boolean = {
    if (!other.isInstanceOf[BSONDBPointer])
      return false
    val that = other.asInstanceOf[BSONDBPointer]
    this.value._1 == that.value._1 &&
      java.util.Arrays.equals(that.value._2, this.value._2)
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
    val obj = itr.getObject
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
