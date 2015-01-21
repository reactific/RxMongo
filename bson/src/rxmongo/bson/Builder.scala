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

  def result : ByteString = {
    val finisher = Builder()
    finisher.putObj(this)
    finisher.buffer.result()
  }

  def double(key: String, value: Double) : Builder = {
    putPrefix(DoubleCode, key)
    buffer.putDouble(value)
    this
  }

  def string(key: String, value: String) : Builder = {
    putPrefix(StringCode, key)
    putStr(value)
    this
  }

  def obj(key: String, value: Builder) : Builder = {
    putPrefix(ObjectCode, key)
    putObj(value)
  }

  def array(key: String, values: Iterable[BSONValue]) : Builder = {
    putPrefix(ArrayCode, key)
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

  def binary(key: String, blob: Array[Byte], kind: BinarySubtype) : Builder = {
    putPrefix(BinaryCode, key)
    buffer.putInt(blob.length)
    buffer.putByte(kind.code)
    buffer.putBytes(blob)
    this
  }

  def undefined(key:String) = {
    putPrefix(UndefinedCode, key)
  }

  def objectID(key: String, value: Array[Byte]) : Builder = {
    require(value.length == 12)
    putPrefix(ObjectIDCode, key)
    buffer.putBytes(value)
    this
  }

  def boolean(key: String, value: Boolean) : Builder = {
    putPrefix(BooleanCode, key)
    buffer.putByte(if(value) 1.toByte else 0.toByte)
    this
  }

  def utcDate(key: String, time: Long) : Builder = {
    putPrefix(DateCode, key)
    buffer.putLong(time)
    this
  }

  def nil(key: String) : Builder = {
    putPrefix(NullCode, key)
  }

  def regex(key: String, pattern: String, options: String = "") : Builder = {
    require(options.matches("i?l?m?s?u?x?"))
    putPrefix(RegexCode, key)
    putCStr(pattern)
    putCStr(options)
  }

  def dbPointer(key: String, referent: String, id: Array[Byte]) : Builder = {
    require(id.length == 12)
    putPrefix(DBPointerCode, key)
    putStr(referent)
    buffer.putBytes(id)
    this
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
    val content = ByteString.newBuilder
    putStr(content, code)
    putObj(content, scope)
    val tmp = content.result()
    buffer.putInt(tmp.length)
    buffer ++= tmp
    this
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

  protected def putCStr(bldr: ByteStringBuilder, s: String) : Builder = {
    val bytes = s.getBytes(utf8)
    for (by <- bytes if by == 0.toByte) {
      throw new IllegalArgumentException("UTF-8 encoding of BSON keys must not contain a 0 byte")
    }
    bldr.putBytes(bytes)
    bldr.putByte(0.toByte)
    this
  }

  protected def putCStr(s: String) : Builder = {
    putCStr(this.buffer, s)
  }

  protected def putPrefix(code: TypeCode, key: String) : Builder = {
    buffer.putByte(code.code)
    putCStr(key)
    this
  }

  protected def putStr(bldr: ByteStringBuilder, value: String) : Builder = {
    val bytes = value.getBytes(utf8)
    val length = bytes.length + 1
    bldr.putInt(bytes.length + 1)
    bldr.putBytes(bytes)
    bldr.putByte(0.toByte)
    this
  }

  protected def putStr(value: String) : Builder = putStr(buffer, value)

  protected def putObj(bldr: ByteStringBuilder, value: Builder) : Builder = {
    val result = value.buffer.result()
    bldr.putInt(result.length + 1)
    bldr ++= result
    bldr.putByte(0.toByte)
    this
  }

  protected def putObj(value: Builder) : Builder = putObj(buffer, value)
}
