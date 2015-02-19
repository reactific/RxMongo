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
  def toByteString = { WriteConcern.Codec.write(this).buffer }
  override def toBSONObject = { WriteConcern.Codec.write(this) }
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

  implicit object Codec extends BSONCodec[WriteConcern, BSONObject] {
    def code : TypeCode = ObjectCode
    def write(value : WriteConcern) : BSONObject = {
      val b = BSONBuilder()
      value.kind match {
        case NoAcknowledgmentWC ⇒ b.integer("w", -1)
        case ErrorsOnlyWC ⇒ b.integer("w", 0)
        case BasicAcknowledgmentWC ⇒ b.integer("w", 1)
        case MajorityWC ⇒ b.string("w", "majority")
        case WaitForMembersWC(num) ⇒ b.integer("w", num)
        case MembersWithTagWC(tag) ⇒ b.string("w", tag)
        case k ⇒ throw new RxMongoError(s"Invalid WriteConcernKind: $k")
      }
      b.integer("wtimeout", value.timeout.toMillis.toInt)
      b.boolean("j", value.journal)
      b.result
    }
    def read(value : BSONObject) : WriteConcern = {
      val kind = {
        value.getTypeCode("w") match {
          case StringCode ⇒ str2WCK(value.getAsString("w"))
          case IntegerCode ⇒ int2WCK(value.getAsInt("w"))
          case c ⇒ throw new RxMongoError(s"Invalid TypeCode for WriteConcernKind: $c")
        }
      }
      WriteConcern(kind, Duration(value.getAsInt("wtimeout"), TimeUnit.MILLISECONDS), value.getAsBoolean("j"))
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
