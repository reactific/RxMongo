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

import rxmongo.bson._

case class WriteError(index : Int, code : Int, errmsg : String)

object WriteError {
  implicit object Codec extends BSONCodec[WriteError, BSONObject] {
    override def code : TypeCode = ObjectCode
    override def write(value : WriteError) : BSONObject = {
      BSONBuilder().
        integer("index", value.index).
        integer("code", value.code).
        string("errmsg", value.errmsg).
        result
    }
    override def read(value : BSONObject) : WriteError = {
      WriteError(value.getAsInt("index"), value.getAsInt("code"), value.getAsString("errmsg"))
    }
  }
}

case class WriteConcernError(code : Int, errmsg : String)

object WriteConcernError {
  implicit object Codec extends BSONCodec[WriteConcernError, BSONObject] {
    override def code : TypeCode = ObjectCode
    override def write(value : WriteConcernError) : BSONObject = {
      BSONBuilder().
        integer("code", value.code).
        string("errmsg", value.errmsg).
        result
    }
    override def read(value : BSONObject) : WriteConcernError = {
      WriteConcernError(value.getAsInt("code"), value.getAsString("errmsg"))
    }
  }
}

case class WriteResult private[rxmongo] (obj : BSONObject) {
  val ok : Int = obj.getAsInt("ok")
  val n : Int = obj.getAsInt("n")
  val writeErrors = obj.getOptionalArray[WriteError, BSONObject]("writeErrors")
  val writeConcernError = obj.getOptionalObject[WriteConcernError]("writeConcernError")
}
