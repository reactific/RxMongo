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

package rxmongo.messages.replies

import rxmongo.bson.{BSONInteger, BSONDocument, BSONBuilder, DocumentCodec}

/** Reply From The BuildInfo Command
  *
  * Prototype:
  * {{{
  *   {
        "version" : "<string>",
        "gitVersion" : "<string>",
        "sysInfo" : "<string>",
        "loaderFlags" : "<string>",
        "compilerFlags" : "<string>",
        "allocator" : "<string>",
        "versionArray" : [ <num>, <num>, <...> ],
        "javascriptEngine" : "<string>",
        "bits" : <num>,
        "debug" : <boolean>,
        "maxBsonObjectSize" : <num>,
        "ok" : <num>
      }
  * }}}
  */
case class BuildInfoReply(
  version: String,
  gitVersion: String,
  sysInfo: String,
  loaderFlags : String,
  compilerFlags : String,
  allocator : String,
  versionArray : Array[Int],
  javascriptEngine : String,
  bits: Int,
  debug : Boolean,
  maxBsonObjectSize : Int
)

object BuildInfoReply {
  implicit object BuildInfoCodec extends DocumentCodec[BuildInfoReply] {
    /** Write BuildInfo into BSONBulder
      *
      * @param value The value, T, to be written to BSON
      * @return A Try[BSONValue] resulting from writing T to BSON
      */
    override def write(value : BuildInfoReply, b : BSONBuilder) : BSONBuilder = {
      b.string("version", value.version)
      b.string("gitVersion", value.gitVersion)
      b.string("sysInfo", value.sysInfo)
      b.string("loaderFlags", value.loaderFlags)
      b.string("compilerFlags", value.compilerFlags)
      b.string("allocator", value.allocator)
      b.array("versionArray", value.versionArray)
      b.string("javascriptEngine", value.javascriptEngine)
      b.integer("bits", value.bits)
      b.boolean("debug", value.debug)
      b.integer("maxBsonObjectSize", value.maxBsonObjectSize)
      b
    }

    /** Convert BSONValue Into T
      *
      * @param value The BSONValue to be converted
      * @return A Try[T] that results from reading T from BSON
      */
    override def read(value : BSONDocument) : BuildInfoReply = {
      BuildInfoReply(
        value.asString("version"),
        value.asString("gitVersion"),
        value.asString("sysInfo"),
        value.asString("loaderFlags"),
        value.asString("compilerFlags"),
        value.asString("allocator"),
        value.asArray("versionArray").asArray.map { bv => bv.value.asInstanceOf[Integer].toInt },
        value.asString("javascriptEngine"),
        value.asInt("bits"),
        value.asBoolean("debug"),
        value.asInt("maxBsonObjectSize")
      )
    }
  }
}
