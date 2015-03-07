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

import java.nio.ByteOrder

import akka.util.{ByteString, ByteStringBuilder, ByteIterator}

import scala.collection.mutable

class BSONDocumentBuilder(hint: Int) extends mutable.Builder[(String, (Byte,ByteIterator)), BSONDocument] {
  implicit val byteOrder = ByteOrder.LITTLE_ENDIAN
  val buffer : ByteStringBuilder = ByteString.newBuilder
  buffer.sizeHint(hint)

  def +=(elem : (String, (Byte, ByteIterator))) : BSONDocumentBuilder.this.type = ???

  def result() : BSONDocument = ???

  def clear() : Unit = ???
}

object BSONDocumentBuilder {
  def apply(hint: Int = 512) : BSONDocumentBuilder = new BSONDocumentBuilder(hint)
}
