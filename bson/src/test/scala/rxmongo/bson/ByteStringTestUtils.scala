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
import java.nio.ByteOrder
import java.nio.charset.StandardCharsets

import akka.util.{ ByteString, ByteStringBuilder }
import com.reactific.hsp.Profiler
import rxmongo.bson.BinarySubtype.UserDefinedBinary

trait ByteStringTestUtils {

  implicit val byteOrder = ByteOrder.LITTLE_ENDIAN

  def cstring(bldr : ByteStringBuilder, str : String) : ByteStringBuilder = {
    bldr.putBytes(str.getBytes(StandardCharsets.UTF_8)) // c string
    bldr.putByte(0) // termination of c string
    bldr
  }

  def string(bldr : ByteStringBuilder, str : String) : ByteStringBuilder = {
    val bytes = str.getBytes(StandardCharsets.UTF_8)
    bldr.putInt(bytes.length + 1)
    bldr.putBytes(bytes)
    bldr.putByte(0)
  }

  def field(bldr : ByteStringBuilder, code : Byte, fieldName : String) : ByteStringBuilder = {
    bldr.putByte(code) // code
    cstring(bldr, fieldName)
  }

  /** Preamble for a new object buffer and its first field
    *
    * @param len The length of the whole object
    * @param code The type code byte of the first field
    * @param fieldName the name of the first field
    * @return ByteStringBuilder with first field preamble constructed
    */
  def preamble(len : Int, code : Byte, fieldName : String) : ByteStringBuilder = {
    val bldr : ByteStringBuilder = ByteString.newBuilder
    bldr.putInt(len)
    field(bldr, code, fieldName)
  }

  val data = Array[Byte](0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
  val anArray = Seq(42.0D, 84.0D)
  val anArraySeq = Seq(BSONDouble(42.0D), BSONDouble(84.0D))
  val anArrayBSON : BSONArray = BSONArray(anArray)
  val anObject = BSONObject("one" -> BSONDouble(84.0D), "two" -> BSONString("eighty-four"))
  val aTime = System.currentTimeMillis()

  def makeAnObject(profiler : Profiler = Profiler) : BSONBuilder = profiler.profile("makeAnObject") {
    val b = BSONBuilder()
    b.double("double", 42.0D)
      .string("string", "fourty-two")
      .obj("obj", anObject)
      .array("array", anArrayBSON)
      .binary("binary", data, UserDefinedBinary)
      .undefined("undefined")
      .objectID("objectid", data)
      .boolean("boolean", value = true)
      .date("date", aTime)
      .nil("null")
      .regex("regex", "pattern", "ilmsux")
      .dbPointer("dbpointer", "referent", data)
      .jsCode("jscode", "function(x) { return x + 1; };")
      .symbol("symbol", "symbol")
      .scopedJsCode("scopedjscode", "function(x)", anObject)
      .integer("integer", 42)
      .timestamp("timestamp", 42L)
      .long("long", 42L)
    b
  }

  def makeObject() : BSONObject = makeObject(Profiler)

  def makeObject(profiler : Profiler) : BSONObject = {
    profiler.profile("makeObject") { makeAnObject(profiler).toBSONObject }
  }

  def makeObject(width : Int, depth : Int, profiler : Profiler = Profiler) : BSONObject = {
    val bldr = makeAnObject(profiler)
    if (depth > 0) {
      val kids = for (i ← 1 to width) yield {
        makeObject(width, depth - 1, profiler)
      }
      profiler.profile("append kids") { bldr.array("kids", kids) }
    }
    profiler.profile("toBSONObject") { bldr.toBSONObject }
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

  def timedTest(maxNanoSeconds : Double, name : String)(func : (Profiler) ⇒ Unit) : Profiler = {
    val p = new Profiler
    val r = p.profile(name) { func(p) }
    val (count, time) = Profiler.get_one_item(name)
    p.print_profile_summary(System.out)
    println()
    val limit = if (suitableForTimingTests) maxNanoSeconds else maxNanoSeconds*10
    if (time > limit) {
      throw new Exception(s"Test '$name' took ${time}ns which exceeded limit of ${maxNanoSeconds}ns")
    }
    p
  }

  def timedAndCountedTests(testName: String, expected: Map[String,(Long,Double)])(func : (Profiler) => Unit) : Profiler = {
    val p = new Profiler
    val r = p.profile(testName) { func(p) }
    p.print_profile_summary(System.out)
    println()
    for ( (name, (maxCount,maxNanoSeconds)) <- expected) {
      val (count, time) = Profiler.get_one_item(name)
      val limit = if (suitableForTimingTests) maxNanoSeconds else maxNanoSeconds*10
      if (time > limit) {
        throw new Exception(s"Test '$name' took ${time}ns which exceeded limit of ${limit}ns")
      }
    }
    p
  }
}
