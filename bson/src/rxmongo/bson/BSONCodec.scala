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

  implicit object ArrayOfStringCodec extends BSONCodec[Array[String], BSONArray] {
    def code = ArrayCode
    def read(value : BSONArray) : Array[String] = {
      value.iterator.map { v ⇒ v.value.toString }.toArray
    }
    def write(value : Array[String]) : BSONArray = {
      BSONArray(value.iterator.toSeq)
    }
  }
}
