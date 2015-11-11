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

import scala.annotation.switch

/** Base class of the case objects for BSON code values
  *
  * Byte code identifiers from here: [[http://docs.mongodb.org/manual/reference/bson-types/]]
  */
sealed trait TypeCode {
  val code : Byte
  val typeName : String
}

case object NotACode extends { val code : Byte = 0; val typeName = "Not_A_BSONValue" } with TypeCode
case object DoubleCode extends { val code : Byte = 1; val typeName = "BSONDouble" } with TypeCode
case object StringCode extends { val code : Byte = 2; val typeName = "BSONString" } with TypeCode
case object ObjectCode extends { val code : Byte = 3; val typeName = "BSONObject" } with TypeCode
case object ArrayCode extends { val code : Byte = 4; val typeName = "BSONArray" } with TypeCode
case object BinaryCode extends { val code : Byte = 5; val typeName = "BSONBinary" } with TypeCode
/** @deprecated */
case object UndefinedCode extends { val code : Byte = 6; val typeName = "BSONUndefinede" } with TypeCode
case object ObjectIDCode extends { val code : Byte = 7; val typeName = "BSONObjectID" } with TypeCode
case object BooleanCode extends { val code : Byte = 8; val typeName = "BSONBoolean" } with TypeCode
case object DateCode extends { val code : Byte = 9; val typeName = "BSONDate" } with TypeCode
case object NullCode extends { val code : Byte = 10; val typeName = "BSONNull" } with TypeCode
case object RegexCode extends { val code : Byte = 11; val typeName = "BSONRegex" } with TypeCode
/** @deprecated */
case object DBPointerCode extends { val code : Byte = 12; val typeName = "BSONDBPointer" } with TypeCode
case object JavaScriptCode extends { val code : Byte = 13; val typeName = "BSONJavaScript" } with TypeCode
/** @deprecated */
case object SymbolCode extends { val code : Byte = 14; val typeName = "BSONSymbol" } with TypeCode
case object ScopedJSCode extends { val code : Byte = 15; val typeName = "BSONScopedJSCode" } with TypeCode
case object IntegerCode extends { val code : Byte = 16; val typeName = "BSONInteger" } with TypeCode
case object TimestampCode extends { val code : Byte = 17; val typeName = "BSONTimestamp" } with TypeCode
case object LongCode extends { val code : Byte = 18; val typeName = "BSONLong" } with TypeCode
case object MinKey extends { val code : Byte = 255.toByte; val typeName = "MinKey" } with TypeCode
case object MaxKey extends { val code : Byte = 127; val typeName = "MaxKey" } with TypeCode

object TypeCode {
  def apply(code : Byte) : TypeCode = {
    (code : @switch) match {
      case 1   ⇒ DoubleCode
      case 2   ⇒ StringCode
      case 3   ⇒ ObjectCode
      case 4   ⇒ ArrayCode
      case 5   ⇒ BinaryCode
      case 6   ⇒ UndefinedCode
      case 7   ⇒ ObjectIDCode
      case 8   ⇒ BooleanCode
      case 9   ⇒ DateCode
      case 10  ⇒ NullCode
      case 11  ⇒ RegexCode
      case 12  ⇒ DBPointerCode
      case 13  ⇒ JavaScriptCode
      case 14  ⇒ SymbolCode
      case 15  ⇒ ScopedJSCode
      case 16  ⇒ IntegerCode
      case 17  ⇒ TimestampCode
      case 18  ⇒ LongCode
      case -1  ⇒ MinKey
      case 127 ⇒ MaxKey
      case _ ⇒
        throw new NoSuchElementException(s"BSON TypeCode($code)")
    }
  }
  def apply(v : BSONValue) : TypeCode = v.code
}
