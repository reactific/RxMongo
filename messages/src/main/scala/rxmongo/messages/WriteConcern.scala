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

import java.util.concurrent.TimeUnit

import rxmongo.bson._

import scala.concurrent.duration._

sealed trait WriteConcernKind
case object NoAcknowledgmentWC extends WriteConcernKind { override def toString = "-1" }
case object ErrorsOnlyWC extends WriteConcernKind { override def toString = "0" }
case object BasicAcknowledgmentWC extends WriteConcernKind { override def toString = "1" }
case object MajorityWC extends WriteConcernKind { override def toString = "majority" }
case class WaitForMembersWC(numMembers : Int) extends WriteConcernKind { override def toString = numMembers.toString }
case class MembersWithTagWC(tag : String) extends WriteConcernKind { override def toString = tag }

/** MongoDB Write Concern
  * This represents the WriteConcern values that are part of driver, client, database, collection
  * and write operation specifications. The Write Concern controls isolation and durability requirements for a write.
  * @see [[http://docs.mongodb.org/master/reference/write-concern/]]
  * @param kind How writes should be done.
  * @param timeout The timeout for the replica set (when WaitForMembersWC.numMbers > 1) to complete the write.
  * @param journal Whether journaling of the write must be finished before return (guarantees durability)
  */
case class WriteConcern(
  kind : WriteConcernKind,
  timeout : FiniteDuration,
  journal : Boolean) extends BSONProvider {
  def wrapAndTerminate = { WriteConcern.Codec.write(this) }
  override def toBSONObject = { BSONObject(WriteConcern.Codec.write(this)) }
}

object WriteConcern {
  val default = WriteConcern()

  private def int2WCK(i : Int) : WriteConcernKind = {
    i match {
      case -1 ⇒ NoAcknowledgmentWC
      case 0 ⇒ ErrorsOnlyWC
      case 1 ⇒ BasicAcknowledgmentWC
      case x : Int ⇒ WaitForMembersWC(x)
    }
  }

  private def str2WCK(s : String) : WriteConcernKind = {
    s match {
      case "majority" ⇒ MajorityWC
      case tag : String if tag.length > 0 ⇒ MembersWithTagWC(tag)
      case _ ⇒ BasicAcknowledgmentWC
    }
  }

  implicit object Codec extends DocumentCodec[WriteConcern] {
    val size : Int = +4 + 3 + 4 + 10 + 1 + 3
    def write(value : WriteConcern, bldr : BSONBuilder) : BSONBuilder = {
      bldr.sizeHint(bldr.length + size)
      value.kind match {
        case NoAcknowledgmentWC ⇒ bldr.integer("w", -1)
        case ErrorsOnlyWC ⇒ bldr.integer("w", 0)
        case BasicAcknowledgmentWC ⇒ bldr.integer("w", 1)
        case MajorityWC ⇒ bldr.string("w", "majority")
        case WaitForMembersWC(num) ⇒ bldr.integer("w", num)
        case MembersWithTagWC(tag) ⇒ bldr.string("w", tag)
        case k ⇒ throw new RxMongoError(s"Invalid WriteConcernKind: $k")
      }
      bldr.integer("wtimeout", value.timeout.toMillis.toInt)
      bldr.boolean("j", value.journal)
    }

    def read(doc : BSONDocument) : WriteConcern = {
      val kind = doc.get("w") match {
        case Some((tc, i)) ⇒
          TypeCode(tc) match {
            case StringCode ⇒ str2WCK(i.getStr)
            case IntegerCode ⇒ int2WCK(i.getInt)
            case c ⇒ throw new RxMongoError(s"Invalid TypeCode for WriteConcernKind: $c")
          }
        case _ ⇒ throw new NoSuchElementException("w")
      }
      WriteConcern(kind, Duration(doc.asInt("wtimeout"), TimeUnit.MILLISECONDS), doc.asBoolean("j"))
    }
  }

  def apply(str : String = "", timeout : FiniteDuration = 10.seconds, journal : Boolean = false) : WriteConcern = {
    val kind = str.trim match {
      case num : String if num.length > 0 && (num forall { ch ⇒ ch.isDigit | ch == '-' }) ⇒
        num.toInt match {
          case -1 ⇒ NoAcknowledgmentWC
          case 0 ⇒ ErrorsOnlyWC
          case 1 ⇒ BasicAcknowledgmentWC
          case x : Int ⇒ WaitForMembersWC(x)
        }
      case "majority" ⇒ MajorityWC
      case tag : String if tag.length > 0 ⇒ MembersWithTagWC(tag)
      case _ ⇒ BasicAcknowledgmentWC
    }
    WriteConcern(kind, timeout, journal)
  }

  def apply(wck : WriteConcernKind) : WriteConcern = WriteConcern(wck, 10.seconds, journal = false)

  def apply(wck : WriteConcernKind, timeout : FiniteDuration) : WriteConcern = WriteConcern(wck, timeout, journal = false)
}
