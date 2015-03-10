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
  hosts : Option[Iterable[String]] = None,
  passives : Option[Iterable[String]] = None,
  arbiters : Option[Iterable[String]] = None,
  arbiterOnly : Option[Boolean] = None,
  passive : Option[Boolean] = None,
  hidden : Option[Boolean] = None,
  tags : Option[Map[String, String]] = None,
  electionId : Option[Long] = None)

object IsMasterReply {
  implicit object IsMasterReplyCodec extends DocumentCodec[IsMasterReply] {
    /** Convert T into BSONValue
      *
      * @param value The value, T, to be written to BSON
      * @return A Try[BSONValue] resulting from writing T to BSON
      */
    override def write(value : IsMasterReply, bldr : BSONBuilder) : BSONBuilder = {
      bldr.double("ok", value.ok)
      bldr.boolean("ismaster", value.ismaster)
      bldr.integer("maxBsonObjectSize", value.maxBsonObjectSize)
      bldr.integer("maxMessageSizeBytes", value.maxMessageSizeBytes)
      bldr.integer("maxWriteBatchSize", value.maxWriteBatchSize)
      bldr.date("localTime", value.localTime)
      bldr.integer("maxWireVersion", value.maxWireVersion)
      bldr.integer("minWireVersion", value.minWireVersion)
      value.secondary.map { v ⇒ bldr.boolean("secondary", v) }
      value.primary.map { v ⇒ bldr.string("primary", v) }
      value.setName.map { v ⇒ bldr.string("setName", v) }
      value.setVersion.map { v ⇒ bldr.integer("setVersion", v) }
      value.me.map { v ⇒ bldr.string("me", v) }
      value.hosts.map { v ⇒ bldr.array("hosts", v) }
      value.passives.map { v ⇒ bldr.array("passives", v) }
      value.arbiters.map { v ⇒ bldr.array("arbiters", v) }
      value.arbiterOnly.map { v ⇒ bldr.boolean("arbiterOnly", v) }
      value.passive.map { v ⇒ bldr.boolean("passive", v) }
      value.hidden.map { v ⇒ bldr.boolean("hidden", v) }
      value.tags.map { v ⇒ bldr.obj("tags", v) }
      value.electionId.map { v ⇒ bldr.long("electionId", v) }
      bldr
    }

    /** Convert BSONValue Into T
      *
      * @param value The BSONValue to be converted
      * @return A Try[T] that results from reading T from BSON
      */
    override def read(doc : BSONDocument) : IsMasterReply = {
      IsMasterReply(
        doc.asDoubleOrElse("ok", 0.0D),
        doc.asBooleanOrElse("ismaster", d = false),
        doc.asIntOrElse("maxBsonObjectSize", 0),
        doc.asIntOrElse("maxMessageSizeBytes", 0),
        doc.asIntOrElse("maxWriteBatchSize", 0),
        doc.asDateOrElse("localTime", new Date(0)),
        doc.asIntOrElse("maxWireVersion", 0),
        doc.asIntOrElse("minWireVersion", 0),
        doc.asOptionalBoolean("secondary"),
        doc.asOptionalString("primary"),
        doc.asOptionalString("setName"),
        doc.asOptionalInt("setVersion"),
        doc.asOptionalString("me"),
        doc.asOptionalArray[String]("hosts"),
        doc.asOptionalArray[String]("passives"),
        doc.asOptionalArray[String]("arbiters"),
        doc.asOptionalBoolean("arbiterOnly"),
        doc.asOptionalBoolean("passive"),
        doc.asOptionalBoolean("hidden"),
        doc.asOptionalMap[String]("tags"),
        doc.asOptionalLong("electionId")
      )
    }
  }
}
