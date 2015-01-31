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

import java.util.Date

import rxmongo.bson._

/** Represents The IsMasterCmd Response From Mongo
  *
  * The optional parameters will not occur if the response came from a server that is not part of a replica set.
  * @param ok Has value 1 if the content is "ok" or an error otherwise
  * @param ismaster True if the responding server is the primary, master or only server
  * @param maxBsonObjectSize Maximum size of an object the server will accept
  * @param maxMessageSizeBytes Maximum size of a message the server will accept
  * @param maxWriteBatchSize Maximum number of write messages that can be batched
  * @param localTime The local time of the server in ISODate UTC
  * @param maxWireVersion The maximum wire version the server will accept
  * @param minWireVersion The minimum wire version the server will accept
  * @param secondary
  * @param primary
  * @param setName
  * @param hosts
  * @param setVersion
  * @param me
  * @param passives
  * @param arbiters
  * @param arbiterOnly
  * @param passive
  * @param hidden
  * @param tags
  * @param electionId
  */
case class IsMasterReply(
  ok : Double = 0,
  ismaster : Boolean = false,
  maxBsonObjectSize : Int = 0,
  maxMessageSizeBytes : Int = 0,
  maxWriteBatchSize : Int = 0,
  localTime : Date = new Date(0),
  maxWireVersion : Int = 0,
  minWireVersion : Int = 0,
  secondary : Option[Boolean] = None,
  primary : Option[String] = None,
  setName : Option[String] = None,
  setVersion : Option[Int] = None,
  me : Option[String] = None,
  hosts : Option[Array[String]] = None,
  passives : Option[Array[String]] = None,
  arbiters : Option[Array[String]] = None,
  arbiterOnly : Option[Boolean] = None,
  passive : Option[Boolean] = None,
  hidden : Option[Boolean] = None,
  tags : Option[Map[String, String]] = None,
  electionId : Option[Long] = None)

object IsMasterReply {
  implicit object IsMasterResponseCodec extends BSONCodec[IsMasterReply, BSONObject] {
    override def code : TypeCode = ObjectCode

    /** Convert T into BSONValue
      *
      * @param value The value, T, to be written to BSON
      * @return A Try[BSONValue] resulting from writing T to BSON
      */
    override def write(value : IsMasterReply) : BSONObject = {
      val b = BSONBuilder()
      b.double("ok", value.ok)
      b.boolean("ismaster", value.ismaster)
      b.integer("maxBsonObjectSize", value.maxBsonObjectSize)
      b.integer("maxMessageSizeBytes", value.maxMessageSizeBytes)
      b.integer("maxWriteBatchSize", value.maxWriteBatchSize)
      b.date("localTime", value.localTime)
      b.integer("maxWireVersion", value.maxWireVersion)
      b.integer("minWireVersion", value.minWireVersion)
      value.secondary.map { v ⇒ b.boolean("secondary", v) }
      value.primary.map { v ⇒ b.string("primary", v) }
      value.setName.map { v ⇒ b.string("setName", v) }
      value.setVersion.map { v ⇒ b.integer("setVersion", v) }
      value.me.map { v ⇒ b.string("me", v) }
      value.hosts.map { v ⇒ b.array("hosts", v.toSeq : _*) }
      value.passives.map { v ⇒ b.array("passives", v.toSeq : _*) }
      value.arbiters.map { v ⇒ b.array("arbiters", v.toSeq : _*) }
      value.arbiterOnly.map { v ⇒ b.boolean("arbiterOnly", v) }
      value.passive.map { v ⇒ b.boolean("passive", v) }
      value.hidden.map { v ⇒ b.boolean("hidden", v) }
      value.tags.map { v ⇒ b.obj("tags", v) }
      value.electionId.map { v ⇒ b.long("electionId", v) }
      b.result
    }

    /** Convert BSONValue Into T
      *
      * @param value The BSONValue to be converted
      * @return A Try[T] that results from reading T from BSON
      */
    override def read(value : BSONObject) : IsMasterReply = {
      val map = value.toAnyMap
      IsMasterReply(
        map.getOrElse("ok", 0.0D).asInstanceOf[Double],
        map.getOrElse("ismaster", false).asInstanceOf[Boolean],
        map.getOrElse("maxBsonObjectSize", 0).asInstanceOf[Int],
        map.getOrElse("maxMessageSizeBytes", 0).asInstanceOf[Int],
        map.getOrElse("maxWriteBatchSize", 0).asInstanceOf[Int],
        map.getOrElse("localTime", new Date(0)).asInstanceOf[Date],
        map.getOrElse("maxWireVersion", 0).asInstanceOf[Int],
        map.getOrElse("minWireVersion", 0).asInstanceOf[Int],
        map.get("secondary").map { v ⇒ v.asInstanceOf[Boolean] },
        map.get("primary").map { v ⇒ v.asInstanceOf[String] },
        map.get("setName").map { v ⇒ v.asInstanceOf[String] },
        map.get("setVersion").map { v ⇒ v.asInstanceOf[Int] },
        map.get("me").map { v ⇒ v.asInstanceOf[String] },
        map.get("hosts").map { v ⇒ v.asInstanceOf[Array[String]] },
        map.get("passives").map { v ⇒ v.asInstanceOf[Array[String]] },
        map.get("arbiters").map { v ⇒ v.asInstanceOf[Array[String]] },
        map.get("arbiterOnly").map { v ⇒ v.asInstanceOf[Boolean] },
        map.get("passive").map { v ⇒ v.asInstanceOf[Boolean] },
        map.get("hidden").map { v ⇒ v.asInstanceOf[Boolean] },
        map.get("tags").map { v ⇒ v.asInstanceOf[Map[String, String]] },
        map.get("electionId").map { v ⇒ v.asInstanceOf[Int] }
      )
    }
  }
}
