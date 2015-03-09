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

import java.util.Date
import java.util.regex.Pattern

import akka.util.{ ByteStringBuilder, ByteString }
import rxmongo.bson.BinarySubtype.UserDefinedBinary

import scala.annotation.switch
import scala.util.{ Failure, Success, Try }

/** BSON Coder/Decoder (Codec) between BSONValue and type T
  *
  * RxMongo users can implement this trait as an implicit object so that their types, T, can be converted into
  * BSON and back. This makes using BSON transparent in most instances.
  *
  * @tparam T The type from which this Codec encodes and to which it decodes
  */
trait BSONCodec[T, B <: BSONValue] {

  def code : TypeCode

  def typeName : String = code.toString

  /** Convert BSONValue Into T
    *
    * @param value The BSONValue to be converted
    * @return A Try[T] that results from reading T from BSON
    */
  def read(value : B) : T

  /** Convert T into BSONValue
    *
    * @param value The value, T, to be written to BSON
    * @return A Try[BSONValue] resulting from writing T to BSON
    */
  def write(value : T) : B

  /** Convenience method to get an Option[T]
    * @param value BSONValue to be converted to T
    * @return None if the value cannot be read, Some[T] otherwise
    */
  def readOption(value : B) : Option[T] = Try {
    Option(read(value))
  } match {
    case Success(x) ⇒ x
    case Failure(x) ⇒ None
  }

  /** Convenience method to get an Option[BSONValue]
    *
    * @param value The value, T, to write to BSON
    * @return None if T cannot be written, Some[BSONValue] otherwise
    */
  def writeOption(value : T) : Option[B] = Try {
    Option(write(value))
  } match {
    case Success(x) ⇒ x
    case Failure(x) ⇒ None
  }

  def tryRead(value : B) : Try[T] = Try {
    Option(read(value)) match {
      case Some(x) ⇒ x
      case None    ⇒ throw new UnsupportedOperationException(s"Could not decode $value")
    }
  }

  def tryWrite(value : T) : Try[B] = Try {
    Option(write(value)) match {
      case Some(x) ⇒ x
      case None    ⇒ throw new UnsupportedOperationException(s"Could not encode $value")
    }
  }

}

object BSONCodec {

  implicit object ByteStringBuilderCodec extends BSONCodec[ByteStringBuilder, BSONObject] {
    def code : TypeCode = ObjectCode
    def write(value : ByteStringBuilder) : BSONObject = BSONObject(value.result())
    def read(value : BSONObject) : ByteStringBuilder = {
      val bldr = ByteString.newBuilder
      bldr.putObject(value)
      bldr
    }
  }

  implicit object ByteStringCodec extends BSONCodec[ByteString, BSONObject] {
    def code : TypeCode = ObjectCode
    def write(value : ByteString) : BSONObject = BSONObject(value)
    def read(value : BSONObject) : ByteString = {
      val bldr = ByteString.newBuilder
      bldr.putObject(value)
      bldr.result()
    }
  }

  implicit object StringCodec extends BSONCodec[String, BSONString] {
    def code = StringCode
    def read(value : BSONString) : String = value.value
    def write(value : String) : BSONString = BSONString(value)
  }

  implicit object IntCodec extends BSONCodec[Int, BSONInteger] {
    def code = IntegerCode
    def read(value : BSONInteger) : Int = value.value
    def write(value : Int) : BSONInteger = BSONInteger(value)
  }

  implicit object LongCodec extends BSONCodec[Long, BSONLong] {
    def code = LongCode
    def read(value : BSONLong) : Long = value.value
    def write(value : Long) : BSONLong = BSONLong(value)
  }

  implicit object DoubleCodec extends BSONCodec[Double, BSONDouble] {
    def code = DoubleCode
    def read(value : BSONDouble) : Double = value.value
    def write(value : Double) : BSONDouble = BSONDouble(value)
  }

  implicit object DateCodec extends BSONCodec[Date, BSONDate] {
    def code = DateCode
    def read(value : BSONDate) : Date = { value.value }
    def write(value : Date) : BSONDate = BSONDate(value)
  }

  implicit object BooleanCodec extends BSONCodec[Boolean, BSONBoolean] {
    def code = BooleanCode
    def read(value : BSONBoolean) : Boolean = { value.value }
    def write(value : Boolean) : BSONBoolean = BSONBoolean(value)
  }

  implicit object BSONObjectCodec extends BSONCodec[BSONObject, BSONObject] {
    def code = ObjectCode
    def read(value : BSONObject) : BSONObject = { value }
    def write(value : BSONObject) : BSONObject = { value }
  }

  implicit object BSONAnyCodec extends BSONCodec[Any, BSONValue] {
    def code = NotACode
    def read(value : BSONValue) : Any = {
      (value.code.code : @switch) match {
        case 1  ⇒ value.asInstanceOf[BSONDouble].value
        case 2  ⇒ value.asInstanceOf[BSONString].value
        case 3  ⇒ value.asInstanceOf[BSONObject].value
        case 4  ⇒ value.asInstanceOf[BSONArray].value
        case 5  ⇒ value.asInstanceOf[BSONBinary].value
        case 6  ⇒ Unit
        case 7  ⇒ value.asInstanceOf[BSONObjectID].value
        case 8  ⇒ value.asInstanceOf[BSONBoolean].value
        case 9  ⇒ value.asInstanceOf[BSONDate].value
        case 10 ⇒ null
        case 11 ⇒ value.asInstanceOf[BSONRegex].value
        case 12 ⇒ value.asInstanceOf[BSONDBPointer].value
        case 13 ⇒ value.asInstanceOf[BSONJsCode].value
        case 14 ⇒ value.asInstanceOf[BSONSymbol].value
        case 15 ⇒ value.asInstanceOf[BSONScopedJsCode].value
        case 16 ⇒ value.asInstanceOf[BSONInteger].value
        case 17 ⇒ value.asInstanceOf[BSONTimestamp].value
        case 18 ⇒ value.asInstanceOf[BSONLong].value
        case _ ⇒
          throw new NoSuchElementException(s"BSON TypeCode($code)")
      }
    }
    def write(value : Any) : BSONValue = {
      value match {
        case null ⇒ BSONNull
        case v : BSONValue ⇒ v
        case i : Int ⇒ BSONInteger(i)
        case l : Long ⇒ BSONLong(l)
        case d : Double ⇒ BSONDouble(d)
        case f : Float ⇒ BSONDouble(f)
        case b : Boolean ⇒ BSONBoolean(b)
        case s : Short ⇒ BSONInteger(s)
        case s : String ⇒ BSONString(s)
        case d : Date ⇒ BSONDate(d.getTime)
        case p : Pattern ⇒ BSONRegex(p)
        case b : ByteString ⇒ BSONBinary(b, UserDefinedBinary)
        case a : Array[Byte] ⇒ BSONBinary(a, UserDefinedBinary)
        case m : Map[String, Any] @unchecked ⇒ BSONObject(m)
        case i : Iterable[Any] ⇒ BSONArray(i.toSeq)
        case x : Any ⇒
          throw RxMongoError(s"Unable to convert $x into a BSONValue")
      }
    }
  }

  implicit object ArrayOfStringCodec extends BSONCodec[Array[String], BSONArray] {
    def code = ArrayCode
    def read(value : BSONArray) : Array[String] = {
      value.iterator.map { v ⇒ v.value.toString }.toArray
    }
    def write(value : Array[String]) : BSONArray = {
      BSONArray(value)
    }
  }

  implicit object ArrayOfByteCodec extends BSONCodec[Array[Byte], BSONBinary] {
    def code = BinaryCode
    def read(value : BSONBinary) : Array[Byte] = { value.value._2 }
    def write(value : Array[Byte]) : BSONBinary = {
      BSONBinary(value, UserDefinedBinary)
    }

  }
}
