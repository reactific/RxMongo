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

import akka.util.{ByteString, ByteStringBuilder}

/** Builder for BSON Object
  *
  * This uses the builder pattern to allow construction of a BSON.Object by using a ByteStringBuilder to construct
  * the corresponding ByteString and then instantiating BSON.Object with the immutable ByteString
  */
case class Builder() {
  implicit val byteOrder = ByteOrder.LITTLE_ENDIAN
  val buffer : ByteStringBuilder = ByteString.newBuilder

  def result : ByteString = finish(buffer)

  def double(key: String, value: Double) : Builder = {
    putPrefix(DoubleCode, key)
    buffer.putDouble(value)
    this
  }

  def string(key: String, value: String) : Builder = {
    putPrefix(StringCode, key)
    putStr(value)
  }

  def obj(key: String, value: Builder) : Builder = {
    putPrefix(ObjectCode, key)
    obj(value)
  }

  def array(key: String, values: Iterable[BSONValue]) : Builder = {
    putPrefix(ArrayCode, key)
    array(values)
  }

  def binary(key: String, blob: Array[Byte], subtype: BinarySubtype) : Builder = {
    putPrefix(BinaryCode, key)
    binary(blob, subtype)
  }

  def undefined(key:String) = {
    putPrefix(UndefinedCode, key)
  }

  def objectID(key: String, value: Array[Byte]) : Builder = {
    putPrefix(ObjectIDCode, key)
    objectID(value)
  }

  def boolean(key: String, value: Boolean) : Builder = {
    putPrefix(BooleanCode, key)
    boolean(value)
  }

  def utcDate(key: String, time: Long) : Builder = {
    putPrefix(DateCode, key)
    utcDate(time)
  }

  def nil(key: String) : Builder = {
    putPrefix(NullCode, key)
  }

  def regex(key: String, pattern: String, options: String = "") : Builder = {
    require(options.matches("i?l?m?s?u?x?"))
    putPrefix(RegexCode, key)
    regex(pattern, options)
  }

  def dbPointer(key: String, referent: String, id: Array[Byte]) : Builder = {
    putPrefix(DBPointerCode, key)
    dbPointer(referent, id)
  }

  def jsCode(key: String, code: String) : Builder = {
    putPrefix(JavaScriptCode, key)
    putStr(code)
  }

  def symbol(key: String, symbol: String) : Builder = {
    putPrefix(SymbolCode, key)
    putStr(symbol)
  }

  def scopedJsCode(key: String, code: String, scope: Builder) : Builder = {
    putPrefix(ScopedJSCode, key)
    scopedJsCode(code, scope)
  }

  def integer(key: String, value: Int) : Builder = {
    putPrefix(IntegerCode, key)
    buffer.putInt(value)
    this
  }

  def timestamp(key: String, value: Long) : Builder = {
    putPrefix(TimestampCode, key)
    buffer.putLong(value)
    this
  }

  def long(key: String, value: Long) : Builder = {
    putPrefix(LongCode, key)
    buffer.putLong(value)
    this
  }

  def value(key: String, value: BSONValue) : Builder = {
    putPrefix(value.code, key)
    buffer ++= value.buffer
    this
  }

  private[bson] def obj(value: Builder) : Builder = {
    putObj(value)
  }

  private[bson] def array(values: Iterable[BSONValue]) : Builder = {
    val array = ByteString.newBuilder
    values.zipWithIndex.foreach { case (value, index) =>
      array.putByte(value.code.code)
      putCStr(array, index.toString)
      array ++= value.buffer
    }
    buffer.putInt(array.length+1)
    buffer ++= array.result
    buffer.putByte(0)
    this
  }

  private[bson] def binary(blob: Array[Byte], subtype: BinarySubtype) : Builder = {
    buffer.putInt(blob.length)
    buffer.putByte(subtype.code)
    buffer.putBytes(blob)
    this
  }

  private[bson]  def objectID(value: Array[Byte]) : Builder = {
    require(value.length == 12)
    buffer.putBytes(value)
    this
  }

  private[bson] def boolean(value: Boolean) : Builder = {
    buffer.putByte(if(value) 1.toByte else 0.toByte)
    this
  }

  private[bson] def utcDate(time: Long) : Builder = {
    buffer.putLong(time)
    this
  }

  private[bson] def dbPointer(referent: String, id: Array[Byte]) : Builder = {
    require(id.length == 12)
    putStr(referent)
    buffer.putBytes(id)
    this
  }

  private[bson] def regex(pattern: String, options: String) : Builder = {
    putCStr(pattern)
    putCStr(options)
    this
  }

  private[bson] def scopedJsCode(code: String, scope: Builder) : Builder = {
    val content = ByteString.newBuilder
    putStr(content, code)
    putObj(content, scope)
    val tmp = content.result()
    buffer.putInt(tmp.length)
    buffer ++= tmp
    this
  }

  private[bson] def putCStr(bldr: ByteStringBuilder, s: String) : Builder = {
    val bytes = s.getBytes(utf8)
    for (by <- bytes if by == 0.toByte) {
      throw new IllegalArgumentException("UTF-8 encoding of BSON keys must not contain a 0 byte")
    }
    bldr.putBytes(bytes)
    bldr.putByte(0.toByte)
    this
  }

  private[bson] def putCStr(s: String) : Builder = {
    putCStr(this.buffer, s)
  }

  private[bson] def putPrefix(code: TypeCode, key: String) : Builder = {
    buffer.putByte(code.code)
    putCStr(key)
    this
  }

  private[bson] def putStr(bldr: ByteStringBuilder, value: String) : Builder = {
    val bytes = value.getBytes(utf8)
    val length = bytes.length + 1
    bldr.putInt(bytes.length + 1)
    bldr.putBytes(bytes)
    bldr.putByte(0.toByte)
    this
  }

  private[bson] def putStr(value: String) : Builder = putStr(buffer, value)

  private[bson] def finish(bldr: ByteStringBuilder) : ByteString = {
    val result = ByteString.newBuilder
    val content = bldr.result()
    result.putInt(content.length + 1)
    result ++= content
    result.putByte(0)
    result.result()
  }

  private[bson] def putObj(bldr: ByteStringBuilder, value: Builder) : Builder = {
    val content = finish(value.buffer)
    bldr ++= content
    this
  }

  private[bson] def putObj(value: Builder) : Builder = putObj(buffer, value)
}

