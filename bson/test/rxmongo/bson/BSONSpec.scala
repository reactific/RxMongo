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

import java.lang.management.ManagementFactory
import java.util.Date
import java.util.regex.Pattern

import com.reactific.hsp.Profiler

import org.specs2.mutable.Specification
import rxmongo.bson.BinarySubtype._

/** Test Suite For BSON object */
class BSONSpec extends Specification {

  sequential

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

  val ALL_FLAGS : Int = Pattern.CASE_INSENSITIVE |
    Pattern.MULTILINE |
    Pattern.DOTALL |
    Pattern.UNICODE_CHARACTER_CLASS |
    Pattern.UNICODE_CASE |
    Pattern.COMMENTS

  "BSON" should {
    "build and interpret reflectively" in {
      val startime = System.nanoTime()
      val bsonObject = Helper.makeObject
      val endtime = System.nanoTime
      val constructiontime = endtime - startime

      if (Helper.suitableForTimingTests)
        constructiontime must beLessThan(200000000L)

      val double = bsonObject.get("double")
      val string = bsonObject.get("string")
      val obj = bsonObject.get("obj")
      val array = bsonObject.get("array")
      val binary = bsonObject.get("binary")
      val undefined = bsonObject.get("undefined")
      val objectid = bsonObject.get("objectid")
      val boolean = bsonObject.get("boolean")
      val date = bsonObject.get("date")
      val nil = bsonObject.get("null")
      val regex = bsonObject.get("regex")
      val dbpointer = bsonObject.get("dbpointer")
      val jscode = bsonObject.get("jscode")
      val symbol = bsonObject.get("symbol")
      val scopedJsCode = bsonObject.get("scopedjscode")
      val integer = bsonObject.get("integer")
      val timestamp = bsonObject.get("timestamp")
      val long = bsonObject.get("long")

      double.isDefined must beTrue
      string.isDefined must beTrue
      obj.isDefined must beTrue
      array.isDefined must beTrue
      binary.isDefined must beTrue
      undefined.isDefined must beTrue
      objectid.isDefined must beTrue
      boolean.isDefined must beTrue
      date.isDefined must beTrue
      nil.isDefined must beTrue
      regex.isDefined must beTrue
      dbpointer.isDefined must beTrue
      jscode.isDefined must beTrue
      symbol.isDefined must beTrue
      scopedJsCode.isDefined must beTrue
      integer.isDefined must beTrue
      timestamp.isDefined must beTrue
      long.isDefined must beTrue

      double.get.value must beEqualTo(42.0D)
      string.get.value must beEqualTo("fourty-two")
      obj.get.value.asInstanceOf[Map[String, BSONValue]] must beEqualTo(
        Map("one" -> BSONDouble(84.0D), "two" -> BSONString("eighty-four")))
      array.get.asInstanceOf[BSONArray].seq must beEqualTo(
        Seq(BSONDouble(42.0D), BSONDouble(84.0D)))

      val pair = binary.get.value.asInstanceOf[(BinarySubtype, Array[Byte])]
      pair._1 must beEqualTo(UserDefinedBinary)
      pair._2 must beEqualTo(Helper.data)

      undefined.get.value.asInstanceOf[Unit] must beEqualTo({})
      objectid.get.value.asInstanceOf[Array[Byte]] must beEqualTo(Helper.data)
      boolean.get.value.asInstanceOf[Boolean] must beEqualTo(true)
      date.get.value.asInstanceOf[Date].getTime must beLessThan(System.currentTimeMillis)
      nil.get.value.asInstanceOf[Unit] must beEqualTo({})
      val r = regex.get.value.asInstanceOf[Pattern]
      r.pattern must beEqualTo("pattern")
      r.flags() must beEqualTo(ALL_FLAGS)

      val (referent, objid) = dbpointer.get.value.asInstanceOf[(String, Array[Byte])]
      referent must beEqualTo("referent")
      objid must beEqualTo(Helper.data)

      jscode.get.value.asInstanceOf[String] must beEqualTo("function(x) { return x + 1; };")
      symbol.get.value.asInstanceOf[String] must beEqualTo("symbol")

      val (code, scope) = scopedJsCode.get.value.asInstanceOf[(String, BSONObject)]
      code must beEqualTo("function(x)")
      scope must beEqualTo(Helper.anObject)

      integer.get.value.asInstanceOf[Int] must beEqualTo(42)
      timestamp.get.value.asInstanceOf[Long] must beEqualTo(42L)
      long.get.value.asInstanceOf[Long] must beEqualTo(42L)
    }
  }

  "BSON Builder" should {

    "build and compact a 100,000 object of 18 fields, quickly" in {
      val startTime = System.nanoTime()
      val obj = Helper.makeObject(10000, 1)
      val endTime = System.nanoTime
      val len = obj.buffer.length
      val constructionTime = endTime - startTime
      if (Helper.suitableForTimingTests) {
        constructionTime must beLessThan(2000000000L) // < 1 seconds for 10,000 nodes
      } else {
        skipped(": machine too busy for timing tests")
      }
      println("Construction:" + constructionTime)
      println(Profiler.format_one_item("makeObj"))
      success
    }
  }

  "BSONObject" should {
    "construct from a variety of Any values" in {
      val date = new Date()
      val regex = Pattern.compile("pattern", ALL_FLAGS)
      val map = Map("foo" -> 84, "bar" -> true, "roo" -> "fourty-two")
      val b = BSONObject(
        "double" -> 42.0D,
        "string" -> "fourty-two",
        "obj" -> Helper.anObject,
        "array" -> Helper.anArray,
        "map" -> map,
        "binary" -> Helper.data,
        "undefined" -> null,
        "boolean" -> true,
        "date" -> date,
        "regex" -> regex,
        "integer" -> 42,
        "long" -> 42L
      )
      b.get("double") must beEqualTo(Some(BSONDouble(42.0D)))
      b.get("string") must beEqualTo(Some(BSONString("fourty-two")))
      b.get("obj") must beEqualTo(Some(Helper.anObject))
      b.get("array") must beEqualTo(Some(Helper.anArrayBSON))
      b.get("map") must beEqualTo(Some(BSONObject(map)))
      b.get("binary") must beEqualTo(Some(BSONBinary(Helper.data, UserDefinedBinary)))
      b.get("undefined") must beEqualTo(Some(BSONNull))
      b.get("boolean") must beEqualTo(Some(BSONBoolean(value = true)))
      b.get("date") must beEqualTo(Some(BSONDate(date)))
      b.get("regex") must beEqualTo(Some(BSONRegex(regex)))
      b.get("integer") must beEqualTo(Some(BSONInteger(42)))
      b.get("long") must beEqualTo(Some(BSONLong(42L)))
    }
  }
}

object Helper {

  val data = Array[Byte](0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
  val anArray = Seq(42.0D, 84.0D)
  val anArraySeq = Seq(BSONDouble(42.0D), BSONDouble(84.0D))
  val anArrayBSON : BSONArray = BSONArray(anArray)
  val anObject = BSONObject("one" -> BSONDouble(84.0D), "two" -> BSONString("eighty-four"))
  val aTime = System.currentTimeMillis()

  def makeObj : BSONBuilder = Profiler.profile("makeObj") {
    val b = BSONBuilder().
      double("double", 42.0D).
      string("string", "fourty-two").
      obj("obj", anObject).
      array("array", anArrayBSON).
      binary("binary", data, UserDefinedBinary).
      undefined("undefined").
      objectID("objectid", data).
      boolean("boolean", value = true).
      date("date", aTime).
      nil("null").
      regex("regex", "pattern", "ilmsux").
      dbPointer("dbpointer", "referent", data).
      jsCode("jscode", "function(x) { return x + 1; };").
      symbol("symbol", "symbol").
      scopedJsCode("scopedjscode", "function(x)", anObject).
      integer("integer", 42).
      timestamp("timestamp", 42L).
      long("long", 42L)
    b
  }

  def makeObject : BSONObject = makeObj.toBSONObject

  def makeObject(width : Int, depth : Int) : BSONObject = {
    val bldr = makeObj
    if (depth > 0) {
      val kids = for (i ← 1 to width) yield {
        makeObject(width, depth - 1)
      }
      bldr.array("kids", kids.toSeq)
    }
    bldr.toBSONObject
  }

  val suitableForTimingTests : Boolean = {
    if (System.getenv("TRAVIS") != null)
      false
    else {
      val os = ManagementFactory.getOperatingSystemMXBean
      val processors = os.getAvailableProcessors
      val avg = os.getSystemLoadAverage
      avg < processors / 2
    }
  }
}
