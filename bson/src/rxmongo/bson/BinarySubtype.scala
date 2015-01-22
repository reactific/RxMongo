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

/** Binary fields specify their sub type  which we model with instances of this trait */
trait BinarySubtype {
  /** The code value for the binary subtype as required by the BSON specification */
  val code: Byte
}

object BinarySubtype {
  case object GenericBinary extends { val code = 0.toByte } with BinarySubtype
  case object FunctionBinary extends { val code = 1.toByte } with BinarySubtype
  case object DeprecatedGenericBinary extends { val code = 2.toByte } with BinarySubtype
  case object DeprecatedUUIDBinary extends { val code = 3.toByte } with BinarySubtype
  case object UUIDBinary extends { val code = 4.toByte } with BinarySubtype
  case object MD5SumBinary extends { val code = 5.toByte } with BinarySubtype
  case object UserDefinedBinary extends { val code: Byte = -128 } with BinarySubtype

  def apply(code: Byte) = code match {
    case 0    => GenericBinary
    case 1    => FunctionBinary
    case 2    => DeprecatedGenericBinary
    case 3    => DeprecatedUUIDBinary
    case 4    => UUIDBinary
    case 5    => MD5SumBinary
    case -128 => UserDefinedBinary
    case _    => throw new NoSuchElementException(s"BinarySubtype($code)")
  }
}
