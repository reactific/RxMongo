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

import akka.util.{ByteStringBuilder, ByteString}

/** A Trait for things that provide BSON
  *
  * The RxMongo-BSON library aims to provide builders of ByteStrings rather than implement representations of BSON
  * as objects. A Provider then is something that assists in the construction of those ByteStrings either by
  * adding something to an existing builder or finalizing into a ByteString
  */
trait BSONConstructor {

  /** Add to a BSONBuilder.
    * Whatever this provides as BSON is added to the provided builder. The same, or a new BSONBuilder must be returned.
    * @param builder The build to which this object's provided BSON is added
    * @return The resulting BSONBuilder
    */
  def addTo(builder: ByteStringBuilder) : ByteStringBuilder

  /** Finalize and return ByteString.
    * The construction of the ByteString is finalized and a ByteString is returned.
    * @return
    */
  def toByteString : ByteString = {
    val bldr = ByteString.newBuilder
    this.addTo(bldr)
    bldr.result()
  }

  /** Convert To BSONObject.
    * Generate the ByteString and then use that to construct a BSONObject.
    * @return The BSONObject corresponding
    */
  def toBSONDocument : BSONDocument = BSONDocument(toByteString)
  def result : BSONDocument = toBSONDocument
}
