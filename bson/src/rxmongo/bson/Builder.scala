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

import scala.util.matching.Regex

/** Builder for BSON Object
  *
  * This uses the builder pattern to allow construction of a BSON.Object by using a ByteStringBuilder to construct
  * the corresponding ByteString and then instantiating BSON.Object with the immutable ByteString
  */
case class Builder() {
  implicit val byteOrder = ByteOrder.LITTLE_ENDIAN
  val buffer : ByteStringBuilder = ByteString.newBuilder

  def result : ByteString = {
    val content = buffer.result()
    val result = ByteString.newBuilder
    result.putInt(content.length + 1)
    result ++= content
    result.putByte(0)
    result.result()
  }

  def bsonObj : BSONObject = BSONObject(result)

  def double(key : String, value : Double) : Builder = {
    putPrefix(DoubleCode, key)
    buffer.putDouble(value)
    this
  }

  def string(key : String, value : String) : Builder = {
    putPrefix(StringCode, key)
    putStr(value)
  }

  def obj(key : String, value : BSONObject) : Builder = {
    putPrefix(ObjectCode, key)
    putObj(value)
    this
  }

  def array(key : String, values : Iterable[BSONValue]) : Builder = {
    putPrefix(ArrayCode, key)
    array(values)
  }

  def binary(key : String, blob : Array[Byte], subtype : BinarySubtype) : Builder = {
    putPrefix(BinaryCode, key)
    binary(blob, subtype)
  }

  def binary(key : String, blob : ByteString, subtype : BinarySubtype) : Builder = {
    putPrefix(BinaryCode, key)
    binary(blob, subtype)
  }

  def undefined(key : String) = {
    putPrefix(UndefinedCode, key)
  }

  def objectID(key : String, value : Array[Byte]) : Builder = {
    putPrefix(ObjectIDCode, key)
    objectID(value)
  }

  def boolean(key : String, value : Boolean) : Builder = {
    putPrefix(BooleanCode, key)
    boolean(value)
  }

  def utcDate(key : String, time : Long) : Builder = {
    putPrefix(DateCode, key)
    utcDate(time)
  }

  def nil(key : String) : Builder = {
    putPrefix(NullCode, key)
  }

  def regex(key : String, pattern : String, options : String = "") : Builder = {
    putPrefix(RegexCode, key)
    buffer.putRegex(pattern, options)
    this
  }

  def regex(key : String, regex : Regex) : Builder = {
    putPrefix(RegexCode, key)
    buffer.putRegex(regex)
    this
  }

  def dbPointer(key : String, referent : String, id : Array[Byte]) : Builder = {
    putPrefix(DBPointerCode, key)
    dbPointer(referent, id)
  }

  def jsCode(key : String, code : String) : Builder = {
    putPrefix(JavaScriptCode, key)
    putStr(code)
  }

  def symbol(key : String, symbol : String) : Builder = {
    putPrefix(SymbolCode, key)
    putStr(symbol)
  }

  def scopedJsCode(key : String, code : String, scope : BSONObject) : Builder = {
    putPrefix(ScopedJSCode, key)
    scopedJsCode(code, scope)
  }

  def integer(key : String, value : Int) : Builder = {
    putPrefix(IntegerCode, key)
    buffer.putInt(value)
    this
  }

  def timestamp(key : String, value : Long) : Builder = {
    putPrefix(TimestampCode, key)
    buffer.putLong(value)
    this
  }

  def long(key : String, value : Long) : Builder = {
    putPrefix(LongCode, key)
    buffer.putLong(value)
    this
  }

  def value(key : String, value : BSONValue) : Builder = {
    putPrefix(value.code, key)
    buffer ++= value.buffer
    this
  }

  private[bson] def value(key : String, anyVal : Any) : Builder = {
    anyVal match {
      case BSONNull ⇒ putPrefix(NullCode, key)
      case BSONUndefined ⇒ putPrefix(UndefinedCode, key)
      case v : BSONValue ⇒
        putPrefix(v.code, key); buffer ++= v.buffer
      case i : Int     ⇒ integer(key, i)
      case l : Long    ⇒ long(key, l)
      case d : Double  ⇒ double(key, d)
      case f : Float   ⇒ double(key, f)
      case b : Boolean ⇒ boolean(key, b)
      case s : Short   ⇒ integer(key, s)
      case s : String  ⇒ string(key, s)
      case d : Date    ⇒ utcDate(key, d.getTime)
      case m : Map[String, Any] @unchecked ⇒ {
        putPrefix(ObjectCode, key)
        val docBuilder = Builder()
        for ((key : String, any) ← m) {
          docBuilder.value(key, any)
        }
        buffer ++= docBuilder.result
      }
      case b : ByteString ⇒ binary(key, b, UserDefinedBinary)
      case r : Regex ⇒ regex(key, r)
      case n : Unit ⇒ nil(key)
      case null ⇒ nil(key)
      case a : Array[Byte] ⇒ binary(key, a, UserDefinedBinary)
      case i : Iterable[Any] ⇒ {
        putPrefix(ArrayCode, key)
        val docBuilder = Builder()
        for ((xVal : Any, j : Int) ← i.zipWithIndex) {
          docBuilder.value(j.toString, xVal)
        }
        buffer ++= docBuilder.result
      }
      case x : Any ⇒
        throw RxMongoError(s"Unable to convert $x into a BSONValue")
    }
    this
  }

  private[bson] def array(values : Iterable[BSONValue]) : Builder = {
    val array = ByteString.newBuilder
    values.zipWithIndex.foreach {
      case (value, index) ⇒
        array.
          putByte(value.code.code).
          putCStr(index.toString)
        array ++= value.buffer
    }
    buffer.putInt(array.length + 1)
    buffer ++= array.result
    buffer.putByte(0)
    this
  }

  private[bson] def binary(blob : Array[Byte], subtype : BinarySubtype) : Builder = {
    buffer.
      putInt(blob.length).
      putByte(subtype.code).
      putBytes(blob)
    this
  }

  private[bson] def binary(blob : ByteString, subtype : BinarySubtype) : Builder = {
    buffer.
      putInt(blob.length).
      putByte(subtype.code).
      append(blob)
    this
  }

  private[bson] def objectID(value : Array[Byte]) : Builder = {
    require(value.length == 12, "ObjectID must be exactly 12 bytes")
    buffer.putBytes(value)
    this
  }

  private[bson] def boolean(value : Boolean) : Builder = {
    buffer.putByte(if (value) 1.toByte else 0.toByte)
    this
  }

  private[bson] def utcDate(time : Long) : Builder = {
    buffer.putLong(time)
    this
  }

  private[bson] def dbPointer(referent : String, id : Array[Byte]) : Builder = {
    require(id.length == 12, "ObjectID must be exactly 12 bytes")
    buffer.
      putStr(referent).
      putBytes(id)
    this
  }

  private[bson] def scopedJsCode(code : String, scope : BSONObject) : Builder = {
    val content = ByteString.newBuilder
    content.
      putStr(code).
      putObj(scope)
    val tmp = content.result()
    buffer.putInt(tmp.length)
    buffer ++= tmp
    this
  }

  private[bson] def putCStr(s : String) : Builder = {
    buffer.putCStr(s)
    this
  }

  private[bson] def putPrefix(code : TypeCode, key : String) : Builder = {
    buffer.
      putByte(code.code).
      putCStr(key)
    this
  }

  private[bson] def putStr(value : String) : Builder = {
    buffer.putStr(value)
    this
  }

  private[bson] def putObj(value : BSONObject) : Builder = {
    buffer.putDoc(value)
    this
  }
}
