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

import akka.util.{ ByteString, ByteStringBuilder }
import rxmongo.bson.BinarySubtype.UserDefinedBinary

import scala.collection.mutable
import scala.util.matching.Regex

/** Builder for BSON Object
  *
  * This uses the builder pattern to allow construction of a BSON.Object by using a ByteStringBuilder to construct
  * the corresponding ByteString and then instantiating BSON.Object with the immutable ByteString
  */
case class BSONBuilder() extends mutable.Builder[(String, Any), BSONObject] {
  implicit val byteOrder = ByteOrder.LITTLE_ENDIAN
  val buffer : ByteStringBuilder = ByteString.newBuilder

  def toByteString() : ByteString = {
    val content = buffer.result()
    val result = ByteString.newBuilder
    result.putInt(content.length + 5) // 4 for length, 1 for terminating 0 byte
    result ++= content
    result.putByte(0)
    result.result()
  }

  def toBSONObject : BSONObject = BSONObject(toByteString())

  override def result() : BSONObject = toBSONObject

  def +=(elem : (String, Any)) : this.type = {
    append(elem._1, elem._2)
    this
  }

  def clear() : Unit = buffer.clear()

  def double(key : String, value : Double) : BSONBuilder = {
    putPrefix(DoubleCode, key)
    buffer.putDouble(value)
    this
  }

  def string(key : String, value : String) : BSONBuilder = {
    putPrefix(StringCode, key)
    putStr(value)
  }

  def obj(key : String, value : BSONObject) : BSONBuilder = {
    putPrefix(ObjectCode, key)
    putObj(value)
    this
  }

  def obj(key : String, fields : Map[String, Any]) : BSONBuilder = {
    putPrefix(ObjectCode, key)
    obj(fields.toSeq)
  }

  def obj(key : String, field1 : (String, Any), fields : (String, Any)*) : BSONBuilder = {
    putPrefix(ObjectCode, key)
    obj(Seq(field1) ++ fields)
  }

  def array(key : String, values : Any*) : BSONBuilder = {
    putPrefix(ArrayCode, key)
    array(values)
  }

  def binary(key : String, blob : Array[Byte], subtype : BinarySubtype) : BSONBuilder = {
    putPrefix(BinaryCode, key)
    binary(blob, subtype)
  }

  def binary(key : String, blob : ByteString, subtype : BinarySubtype) : BSONBuilder = {
    putPrefix(BinaryCode, key)
    binary(blob, subtype)
  }

  def undefined(key : String) = {
    putPrefix(UndefinedCode, key)
  }

  def objectID(key : String, value : Array[Byte]) : BSONBuilder = {
    putPrefix(ObjectIDCode, key)
    objectID(value)
  }

  def boolean(key : String, value : Boolean) : BSONBuilder = {
    putPrefix(BooleanCode, key)
    boolean(value)
  }

  def utcDate(key : String, time : Long) : BSONBuilder = {
    putPrefix(DateCode, key)
    utcDate(time)
  }

  def nil(key : String) : BSONBuilder = {
    putPrefix(NullCode, key)
  }

  def regex(key : String, pattern : String, options : String = "") : BSONBuilder = {
    putPrefix(RegexCode, key)
    buffer.putRegex(pattern, options)
    this
  }

  def regex(key : String, regex : Regex) : BSONBuilder = {
    putPrefix(RegexCode, key)
    buffer.putRegex(regex)
    this
  }

  def dbPointer(key : String, referent : String, id : Array[Byte]) : BSONBuilder = {
    putPrefix(DBPointerCode, key)
    dbPointer(referent, id)
  }

  def jsCode(key : String, code : String) : BSONBuilder = {
    putPrefix(JavaScriptCode, key)
    putStr(code)
  }

  def symbol(key : String, symbol : String) : BSONBuilder = {
    putPrefix(SymbolCode, key)
    putStr(symbol)
  }

  def scopedJsCode(key : String, code : String, scope : BSONObject) : BSONBuilder = {
    putPrefix(ScopedJSCode, key)
    scopedJsCode(code, scope)
  }

  def integer(key : String, value : Int) : BSONBuilder = {
    putPrefix(IntegerCode, key)
    buffer.putInt(value)
    this
  }

  def timestamp(key : String, value : Long) : BSONBuilder = {
    putPrefix(TimestampCode, key)
    buffer.putLong(value)
    this
  }

  def long(key : String, value : Long) : BSONBuilder = {
    putPrefix(LongCode, key)
    buffer.putLong(value)
    this
  }

  def append(key : String, value : BSONValue) : BSONBuilder = {
    putPrefix(value.code, key)
    buffer ++= value.buffer
    this
  }

  private[bson] def append(key : String, anyVal : Any) : BSONBuilder = {
    anyVal match {
      case BSONNull ⇒ putPrefix(NullCode, key)
      case BSONUndefined ⇒ putPrefix(UndefinedCode, key)
      case v : BSONValue ⇒
        putPrefix(v.code, key); buffer ++= v.buffer
      case i : Int ⇒ integer(key, i)
      case l : Long ⇒ long(key, l)
      case d : Double ⇒ double(key, d)
      case f : Float ⇒ double(key, f)
      case b : Boolean ⇒ boolean(key, b)
      case s : Short ⇒ integer(key, s)
      case s : String ⇒ string(key, s)
      case d : Date ⇒ utcDate(key, d.getTime)
      case b : ByteString ⇒ binary(key, b, UserDefinedBinary)
      case r : Regex ⇒ regex(key, r)
      case m : Map[String, Any] @unchecked ⇒
        putPrefix(ObjectCode, key); obj(m.toSeq)
      case a : Array[Byte]   ⇒ binary(key, a, UserDefinedBinary)
      case i : Iterable[Any] ⇒ array(key, i.toSeq : _*)
      case x : Any ⇒
        throw RxMongoError(s"Unable to convert $x into a BSONValue")
    }
    this
  }

  private[bson] def append(fields : Iterable[(String, _)]) : BSONBuilder = {
    fields.foreach {
      case (key : String, value : Unit) ⇒ nil(key)
      case (key : String, value : Any) ⇒ this.append(key, value)
      case (key : String, null) ⇒ nil(key)
    }
    this
  }

  private[bson] def obj(fields : Iterable[(String, Any)]) : BSONBuilder = {
    val docBuilder = BSONBuilder()
    for ((key : String, any : Any) ← fields) {
      docBuilder.append(key, any)
    }
    buffer ++= docBuilder.toByteString
    this
  }

  private[bson] def obj(key : String, value : ByteString) : BSONBuilder = {
    putPrefix(ObjectCode, key)
    buffer ++= value
    this
  }

  private[bson] def array(values : Iterable[Any]) : BSONBuilder = {
    val arrayBuilder = BSONBuilder()
    values.zipWithIndex.foreach {
      case (v, i) ⇒
        arrayBuilder.append(i.toString, v)
    }
    buffer ++= arrayBuilder.toByteString()
    this
  }

  private[bson] def binary(blob : Array[Byte], subtype : BinarySubtype) : BSONBuilder = {
    buffer.
      putInt(blob.length).
      putByte(subtype.code).
      putBytes(blob)
    this
  }

  private[bson] def binary(blob : ByteString, subtype : BinarySubtype) : BSONBuilder = {
    buffer.
      putInt(blob.length).
      putByte(subtype.code).
      append(blob)
    this
  }

  private[bson] def objectID(value : Array[Byte]) : BSONBuilder = {
    require(value.length == 12, "ObjectID must be exactly 12 bytes")
    buffer.putBytes(value)
    this
  }

  private[bson] def boolean(value : Boolean) : BSONBuilder = {
    buffer.putByte(if (value) 1.toByte else 0.toByte)
    this
  }

  private[bson] def utcDate(time : Long) : BSONBuilder = {
    buffer.putLong(time)
    this
  }

  private[bson] def dbPointer(referent : String, id : Array[Byte]) : BSONBuilder = {
    require(id.length == 12, "ObjectID must be exactly 12 bytes")
    buffer.
      putStr(referent).
      putBytes(id)
    this
  }

  private[bson] def scopedJsCode(code : String, scope : BSONObject) : BSONBuilder = {
    val content = ByteString.newBuilder
    content.
      putStr(code).
      putObj(scope)
    val tmp = content.result()
    buffer.putInt(tmp.length + 4) // add four for the length field itself
    buffer ++= tmp
    this
  }

  private[bson] def putCStr(s : String) : BSONBuilder = {
    buffer.putCStr(s)
    this
  }

  private[bson] def putPrefix(code : TypeCode, key : String) : BSONBuilder = {
    buffer.
      putByte(code.code).
      putCStr(key)
    this
  }

  private[bson] def putStr(value : String) : BSONBuilder = {
    buffer.putStr(value)
    this
  }

  private[bson] def putObj(value : BSONObject) : BSONBuilder = {
    buffer.putDoc(value)
    this
  }

}
