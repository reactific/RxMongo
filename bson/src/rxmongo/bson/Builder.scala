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

  def double(key: String, value: Double) : this.type = {
    putPrefix(DoubleCode, key)
    buffer.putDouble(value)
    this
  }

  def string(key: String, value: String) : this.type = {
    putPrefix(StringCode, key)
    putStr(value)
    this
  }

  def obj(key: String, value: Builder) : this.type = {
    putPrefix(ObjectCode, key)
    putObj(value)
  }

  def array(key: String, value: Array[_]) = ???

  def binary(key: String, blob: Array[Byte], kind: BinarySubtype) : this.type = {
    putPrefix(BinaryCode, key)
    buffer.putInt(blob.length)
    buffer.putByte(kind.code)
    buffer.putBytes(blob)
    this
  }

  def undefined(key:String) = {
    putPrefix(UndefinedCode, key)
  }

  def objectID(key: String, value: Array[Byte]) : this.type = {
    require(value.length == 12)
    putPrefix(ObjectIDCode, key)
    buffer.putBytes(value)
    this
  }

  def boolean(key: String, value: Boolean) : this.type = {
    putPrefix(BooleanCode, key)
    buffer.putByte(if(value) 1.toByte else 0.toByte)
    this
  }

  def utcDate(key: String, time: Long) : this.type = {
    putPrefix(DateCode, key)
    buffer.putLong(time)
    this
  }

  def nil(key: String) : this.type = {
    putPrefix(NullCode, key)
  }

  def regex(key: String, pattern: String, options: String = "") : this.type = {
    require(options.matches("i?l?m?s?u?x?"))
    putPrefix(RegexCode, key)
    putCString(pattern)
    putCString(options)
  }

  def dbPointer(key: String, referent: String, id: Array[Byte]) : this.type = {
    require(id.length == 12)
    putPrefix(DBPointerCode, key)
    putStr(referent)
    buffer.putBytes(id)
    this
  }

  def jsCode(key: String, code: String) : this.type = {
    putPrefix(JavaScriptCode, key)
    putStr(code)
  }

  def symbol(key: String, symbol: String) : this.type = {
    putPrefix(SymbolCode, key)
    putStr(symbol)
  }

  def scopedJsCode(key: String, code: String, scope: this.type) : this.type = {
    putPrefix(ScopedJSCode, key)
    putStr(code)
    putObj(scope)
  }

  def integer(key: String, value: Int) : this.type = {
    putPrefix(IntegerCode, key)
    buffer.putInt(value)
    this
  }

  def timestamp(key: String, value: Long) : this.type = {
    putPrefix(TimestampCode, key)
    buffer.putLong(value)
    this
  }

  def long(key: String, value: Long) : this.type = {
    putPrefix(LongCode, key)
    buffer.putLong(value)
    this
  }

  protected def putCString(s: String) : this.type = {
    val bytes = s.getBytes(utf8)
    for (by <- bytes if by == 0.toByte) {
      throw new IllegalArgumentException("UTF-8 encoding of BSON keys must not contain a 0 byte")
    }
    buffer.putBytes(bytes)
    buffer.putByte(0.toByte)
    this
  }

  protected def putPrefix(code: TypeCode, key: String) : this.type = {
    buffer.putByte(code.code)
    putCString(key)
    this
  }

  protected def putStr(value: String) : this.type = {
    val bytes = value.getBytes(utf8)
    val length = bytes.length + 1
    buffer.putInt(bytes.length + 1)
    buffer.putBytes(bytes)
    buffer.putByte(0.toByte)
    this
  }

  protected def putObj(value: Builder) : this.type = {
    val result = value.buffer.result()
    buffer.putInt(result.length + 1)
    buffer ++= result
    buffer.putByte(0.toByte)
    this
  }
}
