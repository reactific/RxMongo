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

package rxmongo.driver

import akka.util.ByteIterator
import rxmongo.bson._

case class WriteError(index : Int, code : Int, errmsg : String)

object WriteError {
  implicit object Codec extends Codec[WriteError] {
    override def write(value : WriteError, bldr : BSONBuilder) : BSONBuilder = {
      bldr.
        integer("index", value.index).
        integer("code", value.code).
        string("errmsg", value.errmsg)
    }
    override def read(itr : ByteIterator) : WriteError = {
      val value = BSONDocument(itr)
      WriteError(value.asInt("index"), value.asInt("code"), value.asString("errmsg"))
    }
  }
}

case class WriteConcernError(code : Int, errmsg : String)

object WriteConcernError {
  implicit object Codec extends Codec[WriteConcernError] {
    override def write(value : WriteConcernError, bldr : BSONBuilder) : BSONBuilder = {
      bldr.
        integer("code", value.code).
        string("errmsg", value.errmsg)
    }
    override def read(itr : ByteIterator) : WriteConcernError = {
      val value = BSONDocument(itr)
      WriteConcernError(value.asInt("code"), value.asString("errmsg"))
    }
  }
}

case class WriteResult private[rxmongo] (obj : BSONObject) {
  val ok : Int = obj.getAsInt("ok")
  val n : Int = obj.getAsInt("n")
  val writeErrors = obj.getOptionalArray[WriteError]("writeErrors")
  val writeConcernError = obj.getOptionalObject[WriteConcernError]("writeConcernError")
}
