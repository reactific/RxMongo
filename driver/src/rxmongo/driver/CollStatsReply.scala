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

import akka.util.{ ByteStringBuilder, ByteIterator }
import rxmongo.bson._

/** Reply From The CollStatsCmd
  * @param ns The namespace of the current collection, which follows the format [database].[collection].
  * @param count The number of objects or documents in this collection.
  * @param size The total size of all records in a collection. This value does not include the record header, which
  * is 16 bytes per record, but does include the record’s padding. Additionally size does not include the
  * size of any indexes associated with the collection, which the totalIndexSize field reports. The scale
  * argument affects this value.
  * @param avgObjSize The average size of an object in the collection. The scale argument does not affect this value.
  * @param storageSize The total amount of storage allocated to this collection for document storage. The scale
  *  argument affects this value. For mmapv1, storageSize will not decrease as you remove or
  *  shrink documents.
  * @param numExtents The total number of contiguously allocated data file regions. Only present when using
  * the mmapv1 storage engine.
  * @param nindexes The number of indexes on the collection. All collections have at least one index on the _id field.
  * @param lastExtentSize The size of the last extent allocated. The scale argument affects this value. Only present
  *     when using the mmapv1 storage engine.
  * @param userFlags Reports the flags on this collection set by the user. See the collMod command for more information
  * on setting user flags and usePowerOf2Sizes. Only appears when using the mmapv1 storage engine.
  * @param totalIndexSize The total size of all indexes. The scale argument affects this value.
  * @param indexSizes This field specifies the key and size of every existing index on the collection.
  * The scale argument affects this value.
  * @param capped This field will be “true” if the collection is capped. Not present if collection is not capped
  * @param max Shows the maximum number of documents that may be present in a capped collection. Not present if
  * collection is not capped.
  * @param maxSize Shows the maximum size of a capped collection. Not present if collection is not capped.
  * @param wiredTiger wiredTiger only appears when using the wiredTiger storage engine. This document contains data
  * reported directly by the WiredTiger engine and other data for internal diagnostic use.
  * @param indexDetails A document that reports data from the storage engine for each index in the collection. The
  *   fields in this document are the names of the indexes, while the values themselves are documents
  *   that contain statistics for the index provided by the storage engine. These statistics are for
  *   internal diagnostic use.
  */
case class CollStatsReply(
  ns : String,
  count : Int,
  size : Int,
  avgObjSize : Int,
  storageSize : Int,
  numExtents : Int,
  nindexes : Int,
  lastExtentSize : Int,
  systemFlags : Option[Int] = Some(1),
  userFlags : Option[Int] = Some(1),
  totalIndexSize : Int = 0,
  indexSizes : Map[String, Int] = Map.empty[String, Int],
  extents : Seq[ExtentInfo] = Seq.empty[ExtentInfo],
  ok : Double = 1.0,
  capped : Boolean = false,
  max : Option[Int] = None,
  maxSize : Option[Int] = None,
  wiredTiger : Option[BSONObject] = None,
  indexDetails : Option[BSONObject] = None)

object CollStatsReply {
  implicit object CollStatsReplyCodec extends Codec[CollStatsReply] {
    /** Convert T into BSONValue
      *
      * @param value The value, T, to be written to BSON
      * @return A Try[BSONValue] resulting from writing T to BSON
      */
    override def write(value : CollStatsReply, bldr : BSONBuilder) : BSONBuilder = {
      val b = BSONBuilder()
      b.string("ns", value.ns)
      b.integer("count", value.count)
      b.integer("size", value.size)
      b.integer("avgObjSize", value.avgObjSize)
      b.integer("storageSize", value.storageSize)
      b.integer("numExtents", value.numExtents)
      b.integer("nindexes", value.nindexes)
      b.integer("lastExtentSize", value.lastExtentSize)
      value.systemFlags.map { sf ⇒ b.integer("systemFlags", sf) }
      value.userFlags.map { uf ⇒ b.integer("userFlags", uf) }
      b.integer("totalIndexSize", value.totalIndexSize)
      b.obj("indexSizes", value.indexSizes)
      b.array("extents", value.extents)(ExtentInfo.Codec)
      b.double("ok", value.ok)
      if (value.capped)
        b.boolean("capped", value = true)
      value.max.map { max ⇒ b.integer("max", max) }
      value.maxSize.map { maxSize ⇒ b.integer("maxSize", maxSize) }
      value.wiredTiger.map { wt ⇒ b.obj("wiredTiger", wt) }
      value.indexDetails.map { id ⇒ b.obj("indexDetails", id) }
      b
    }

    /** Convert BSONValue Into T
      *
      * @param value The BSONValue to be converted
      * @return A Try[T] that results from reading T from BSON
      */
    override def read(itr : ByteIterator) : CollStatsReply = {
      val value = BSONDocument(itr)
      CollStatsReply(
        value.asString("ns"),
        value.asInt("count"),
        value.asInt("size"),
        value.asInt("avgObjSize"),
        value.asInt("storageSize"),
        value.asInt("numExtents"),
        value.asInt("nindexes"),
        value.asInt("lastExtentSize"),
        value.asOptionalInt("systemFlags"),
        value.asOptionalInt("userFlags"),
        value.asInt("totalIndexSize"),
        value.asMap[Int]("indexSizes"),
        value.asSeq[ExtentInfo]("extents")(ExtentInfo.Codec).toSeq,
        value.asDouble("ok"),
        if (value.contains("capped")) true else false,
        value.asOptionalInt("max"),
        value.asOptionalInt("maxSize"),
        value.asOptionalObject("wiredTiger"),
        value.asOptionalObject("indexDetails")
      )
    }
  }
}

case class ExtentInfo(len : Int, file : Int, offset : Int)

object ExtentInfo {
  implicit object Codec extends Codec[ExtentInfo] {
    override def read(itr : ByteIterator) : ExtentInfo = {
      val value = BSONDocument(itr)
      val obj = value.asMap[Int]("loc:")
      ExtentInfo(
        value.getOrElse("len", 0).asInstanceOf[Int],
        obj.getOrElse("file", 0),
        obj.getOrElse("offset", 0)
      )
    }
    override def write(value : ExtentInfo, bldr : BSONBuilder) : BSONBuilder = {
      bldr.integer("len", value.len)
      bldr.anyObj("loc:", Seq("file" → value.file, "offset" → value.offset))
      bldr
    }
  }
}
