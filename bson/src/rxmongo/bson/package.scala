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

package rxmongo

import java.nio.ByteOrder
import java.nio.charset.Charset

import akka.util.{ ByteIterator, ByteStringBuilder }
import scala.language.implicitConversions

/** The bson package object.
  *
  * This just contains values that are used throughout the bson package.
  */
package object bson {

  case class RxMongoError(message : String, cause : Option[Throwable] = None) extends Exception {
    override def getMessage = message
    override def getCause = cause.orNull
  }

  // Everything in Mongo is Little Endian
  implicit val byteOrder = ByteOrder.LITTLE_ENDIAN

  val utf8 = Charset.forName("UTF-8")

  /** Maximum Document Size.
    *
    * Use Mongo's maximum doc size to ensure that we're having sane reading of length fields and we don't OOM
    * by trying to allocate all memory.
    * @see [[http://docs.mongodb.org/manual/reference/command/isMaster/#dbcmd.isMaster]]
    *
    */
  final val maxDocSize = 16 * 1024 * 1024

  /** Implicit Extensions to ByteStringBuilder
    *
    * This makes using a ByteStringBuilder with BSON easier by providing functions that build BSON data structures.
    *
    * @param bldr The builder we are extending
    */
  implicit class ByteStringBuilderPimpsClass(val bldr : ByteStringBuilder) extends ByteStringBuilderPimps

  /** Implicit Extensions To ByteIterator
    *
    * This makes it easier to use a ByteIterator with MongoDB. The extensions implement recognition of various
    * data types that BSON encoding requires.
    * @param itr The wrapped iterator
    */
  implicit class ByteIteratorPimpsClass(val itr : ByteIterator) extends ByteIteratorPimps

}
