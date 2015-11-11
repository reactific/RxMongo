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

package rxmongo.messages

import rxmongo.bson.{ BSONDocument, DocumentCodec, BSONBuilder }

/** A Sort Specification
  *
  * Description of thing
  */
case class Sort(fields : Seq[(String, Boolean)])

object Sort {
  val empty = Sort(Seq.empty[(String, Boolean)])

  implicit object Codec extends DocumentCodec[Sort] {
    def write(value : Sort, bldr : BSONBuilder) : BSONBuilder = {
      for ((key, ascending) ← value.fields) {
        bldr.integer(key, if (ascending) 1 else -1)
      }
      bldr
    }
    def read(doc : BSONDocument) : Sort = {
      Sort(doc.asSeqOf[Int].map { case (key, value) ⇒ key -> (if (value < 0) false else true) })
    }
  }
}
