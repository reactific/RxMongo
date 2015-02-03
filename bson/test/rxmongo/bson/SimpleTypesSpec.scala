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

import org.specs2.mutable.Specification
import rxmongo.bson.BinarySubtype._

class SimpleTypesSpec extends Specification {

  "BinarySubtype" should {
    "yield correct values for objects" in {
      GenericBinary.code must beEqualTo(0)
      FunctionBinary.code must beEqualTo(1)
      DeprecatedGenericBinary.code must beEqualTo(2)
      DeprecatedUUIDBinary.code must beEqualTo(3)
      UUIDBinary.code must beEqualTo(4)
      MD5SumBinary.code must beEqualTo(5)
      UserDefinedBinary.code must beEqualTo(-128)
    }

    "apply values correctly" in {
      BinarySubtype(0) must beEqualTo(GenericBinary)
      BinarySubtype(1) must beEqualTo(FunctionBinary)
      BinarySubtype(2) must beEqualTo(DeprecatedGenericBinary)
      BinarySubtype(3) must beEqualTo(DeprecatedUUIDBinary)
      BinarySubtype(4) must beEqualTo(UUIDBinary)
      BinarySubtype(5) must beEqualTo(MD5SumBinary)
      BinarySubtype(6) must throwA[NoSuchElementException]
      BinarySubtype(-128) must beEqualTo(UserDefinedBinary)
    }
  }

  "Delete" should {
    "construction from boolean expression" in {
      Delete("a" $ne "b", 1) must beEqualTo(Delete(BSONObject("a" → BSONObject("$ne" -> "b")), 1))
    }
    "construct from Query" in {
      Delete(Query("a" $ne "b"), 1) must beEqualTo(Delete(
        BSONObject("$query" → BSONObject("a" → BSONObject("$ne" -> "b"))), 1))
    }
    "produce correct BSONObject" in {
      Delete.Codec.write(Delete("a" $ne "b", 1)) must beEqualTo(
        BSONObject("q" → BSONObject("a" → BSONObject("$ne" -> "b")), "limit" → 1)
      )
    }
  }

  "TypeCode" should {
    "yield correct values for objects" in {
      NotACode.code must beEqualTo(0)
      DoubleCode.code must beEqualTo(1)
      StringCode.code must beEqualTo(2)
      ObjectCode.code must beEqualTo(3)
      ArrayCode.code must beEqualTo(4)
      BinaryCode.code must beEqualTo(5)
      UndefinedCode.code must beEqualTo(6)
      ObjectIDCode.code must beEqualTo(7)
      BooleanCode.code must beEqualTo(8)
      DateCode.code must beEqualTo(9)
      NullCode.code must beEqualTo(10)
      RegexCode.code must beEqualTo(11)
      DBPointerCode.code must beEqualTo(12)
      JavaScriptCode.code must beEqualTo(13)
      SymbolCode.code must beEqualTo(14)
      ScopedJSCode.code must beEqualTo(15)
      IntegerCode.code must beEqualTo(16)
      TimestampCode.code must beEqualTo(17)
      LongCode.code must beEqualTo(18)
      MinKey.code must beEqualTo(-1)
      MaxKey.code must beEqualTo(127)
    }
    "apply values correctly" in {
      TypeCode(1.toByte) must beEqualTo(DoubleCode)
      TypeCode(2.toByte) must beEqualTo(StringCode)
      TypeCode(3.toByte) must beEqualTo(ObjectCode)
      TypeCode(4.toByte) must beEqualTo(ArrayCode)
      TypeCode(5.toByte) must beEqualTo(BinaryCode)
      TypeCode(6.toByte) must beEqualTo(UndefinedCode)
      TypeCode(7.toByte) must beEqualTo(ObjectIDCode)
      TypeCode(8.toByte) must beEqualTo(BooleanCode)
      TypeCode(9.toByte) must beEqualTo(DateCode)
      TypeCode(10.toByte) must beEqualTo(NullCode)
      TypeCode(11.toByte) must beEqualTo(RegexCode)
      TypeCode(12.toByte) must beEqualTo(DBPointerCode)
      TypeCode(13.toByte) must beEqualTo(JavaScriptCode)
      TypeCode(14.toByte) must beEqualTo(SymbolCode)
      TypeCode(15.toByte) must beEqualTo(ScopedJSCode)
      TypeCode(16.toByte) must beEqualTo(IntegerCode)
      TypeCode(17.toByte) must beEqualTo(TimestampCode)
      TypeCode(18.toByte) must beEqualTo(LongCode)
      TypeCode(-1.toByte) must beEqualTo(MinKey)
      TypeCode(127.toByte) must beEqualTo(MaxKey)
      TypeCode(19.toByte) must throwA[NoSuchElementException]
      TypeCode(0.toByte) must throwA[NoSuchElementException]
    }
  }

  "Update" should {
    "construct from BooleanExpression" in {
      Update("a" $ne "b", "foo" $set "bar", upsert = true, multi = false) must beEqualTo(
        Update(BSONObject("a" → BSONObject("$ne" → "b")), BSONObject("$set" → BSONObject("foo" → "bar")), true, false)
      )
    }

    "construct from Query" in {
      Update(Query("a" $ne "b"), "foo" $set "bar", upsert = true, multi = false) must beEqualTo(
        Update(BSONObject("$query" → BSONObject("a" → BSONObject("$ne" → "b"))),
          BSONObject("$set" → BSONObject("foo" → "bar")), true, false)
      )
    }

    "produce correct BSONObject" in {
      Update.Codec.write(Update("a" $ne "b", "foo" $set "bar", upsert = true, multi = false)) must beEqualTo(
        BSONObject(
          "q" → BSONObject("a" → BSONObject("$ne" → "b")),
          "u" → BSONObject("$set" → BSONObject("foo" → "bar")),
          "upsert" → true,
          "multi" → false
        )
      )
    }
  }
}
