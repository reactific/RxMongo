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

import BinarySubtype._
import org.specs2.mutable.Specification

/** Test Cases For BinarySubtype
  */
class BinarySubtypeSpec extends Specification {

  "BinarySubtype" should {
    "match encoded bytes correctly" in {
      BinarySubtype(0.toByte) must beEqualTo(GenericBinary)
      BinarySubtype(1.toByte) must beEqualTo(FunctionBinary)
      BinarySubtype(2.toByte) must beEqualTo(DeprecatedGenericBinary)
      BinarySubtype(3.toByte) must beEqualTo(DeprecatedUUIDBinary)
      BinarySubtype(4.toByte) must beEqualTo(UUIDBinary)
      BinarySubtype(5.toByte) must beEqualTo(MD5SumBinary)
      BinarySubtype(-128.toByte) must beEqualTo(UserDefinedBinary)
    }
    "not match unknown byte encodings" in {
      BinarySubtype(17.toByte) must throwA[NoSuchElementException]
    }
  }
}
