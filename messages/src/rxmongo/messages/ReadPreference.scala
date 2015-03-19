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

sealed trait ReadPreference
case object PrimaryRP extends ReadPreference { override def toString = "primary" }
case object PrimaryPreferredRP extends ReadPreference { override def toString = "primaryPreferred" }
case object SecondaryRP extends ReadPreference { override def toString = "secondary" }
case object SecondaryPreferredRP extends ReadPreference { override def toString = "secondaryPreferred" }
case object NearestRP extends ReadPreference { override def toString = "nearest" }

object ReadPreference {
  def apply(str : String) : ReadPreference = {
    str match {
      case "primary" ⇒ PrimaryRP
      case "primaryPreferred" ⇒ PrimaryPreferredRP
      case "secondary" ⇒ SecondaryRP
      case "secondaryPreferred" ⇒ SecondaryPreferredRP
      case "nearest" ⇒ NearestRP
      case _ ⇒ PrimaryRP
    }
  }

  def tags(str : String) : Iterable[(String, String)] = {
    val parts = str.split(",")
    for (part ← parts if part.contains(":")) yield {
      val parts = part.split(":")
      parts(0) -> parts(1)
    }
  }
}
