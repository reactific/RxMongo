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

import akka.util.ByteString
import rxmongo.bson.{ BSONBuilder, BSONObject, BSONProvider }

/** Storage Engine Configuration */
sealed trait StorageEngineConfig extends BSONProvider {
  def name : String
  def doc : BSONObject
  def toByteString : ByteString = {
    BSONBuilder().obj(name, doc).toByteString
  }
}

/** WiredTiger Engine Configuration
  *
  * @param doc The WiredTiger configuration options as a document.
  */
case class WiredTigerConfig(doc : BSONObject) extends { val name : String = "WiredTiger" } with StorageEngineConfig

/** MMapV1 Engine Configuration
  *
  * @param doc The MMapV1 configuration options as a document.
  */
case class MMapV1Config(doc : BSONObject) extends { val name : String = "mmapv1" } with StorageEngineConfig

/** Options For Index Creation
  * @see [[http://docs.mongodb.org/v3.0/reference/method/db.collection.createIndex/#ensureindex-options]]
  * @param unique Creates a unique index so that the collection will not accept insertion of documents where the
  * index key or keys match an existing value in the index. Specify true to create a unique index.
  * The default value is false. The option is unavailable for hashed indexes.
  * @param sparse If true, the index only references documents with the specified field. These indexes use less
  * space but behave differently in some situations (particularly sorts). The default value is false.
  * 2dsphere indexes are sparse by default and ignore this option. For a compound index that includes
  * 2dsphere index key(s) along with keys of other types, only the 2dsphere index fields determine
  * whether the index references a document. 2d, geoHaystack, and text indexes behave similarly to
  * the 2dsphere indexes.
  * @param background Builds the index in the background so that building an index does not block other database
  *     activities. Specify true to build in the background. The default value is false.
  * @param name The name of the index. If unspecified, MongoDB generates an index name by concatenating the names of
  * the indexed fields and the sort order. Whether user specified or MongoDB generated, index names
  * including their full namespace (i.e. database.collection) cannot be longer than the Index Name Limit.
  * @param expireAfterSeconds Specifies a value, in seconds, as a TTL to control how long MongoDB retains documents in
  *             this collection. This applies only to TTL indexes.
  * @param storageEngine Allows users to specify configuration to the storage engine on a per-index basis when
  *        creating an index. Storage engine configuration specified when creating indexes are validated
  *        and logged to the oplog during replication to support replica sets with members that use
  *        different storage engines.
  * @param weights For Text indices, specifies the weights to assign fields in the document
  * @param default_language For Text indices, specifies the default language for the text
  * @param language_override For Text indices, specifies the field name that provides the language of the text
  * @param bits For 2d indices, the number of precision of the stored geohash value of the location data. The bits
  * value ranges from 1 to 32 inclusive. The default value is 26.
  * @param min For 2d indices, the lower inclusive boundary for the longitude and latitude values. The default
  * value is -180.0.
  * @param max For 2d indices, the upper inclusive boundary for the longitude and latitude values. The default
  * value is 180.0.
  * @param bucketSize For geoHaystack indexes, specify the number of units within which to group the location
  *     values; i.e. group in the same bucket those location values that are within the specified
  *     number of units to each other. The value must be greater than 0.
  */
case class IndexOptions(
  unique : Option[Boolean] = None,
  sparse : Option[Boolean] = None,
  background : Option[Boolean] = None,
  name : Option[String] = None,
  expireAfterSeconds : Option[Int] = None,
  storageEngine : Option[StorageEngineConfig] = None,
  weights : Seq[(String, Integer)] = Seq.empty[(String, Integer)],
  default_language : Option[String] = None,
  language_override : Option[String] = None,
  bits : Option[Int] = None,
  min : Option[Double] = None,
  max : Option[Double] = None,
  bucketSize : Option[Double] = None) extends BSONProvider {
  def toByteString : ByteString = {
    val b = BSONBuilder()
    unique.map { unique ⇒ b.boolean("unique", unique) }
    sparse.map { sparse ⇒ b.boolean("sparse", sparse) }
    background.map { background ⇒ b.boolean("background", background) }
    name.map { name ⇒ b.string("name", name) }
    expireAfterSeconds.map { expire ⇒ b.integer("expireAfterSeconds", expire) }
    storageEngine.map { se ⇒ b.obj("storageEngine", se.result) }
    if (weights.nonEmpty) {
      val w = BSONBuilder()
      for ((field, weight) ← weights) { w.integer(field, weight) }
      b.obj("weights", w.result)
    }
    default_language.map { default_language ⇒ b.string("default_language", default_language) }
    language_override.map { language_override ⇒ b.string("language_override", language_override) }
    bits.map { bits ⇒ b.integer("bits", bits) }
    min.map { min ⇒ b.double("min", min) }
    max.map { max ⇒ b.double("max", max) }
    bucketSize.map { bucketSize ⇒ b.double("bucketSize", bucketSize) }
    b.toByteString
  }
}

/** Abstract base class of Index case classes
  * This just implements the toByteString method in terms of the construct method making it easier for
  * subclasses to deliver their result.
  */
sealed trait IndexTrait extends BSONProvider {
  def construct : BSONBuilder = {
    BSONBuilder()
  }
  def name : String
  final def toByteString : ByteString = construct.toByteString
}

/** A Simple Single-Field Index
  * @see [[http://docs.mongodb.org/v3.0/core/index-single/]]
  * @see [[http://docs.mongodb.org/v3.0/reference/method/db.collection.createIndex/#db.collection.createIndex]]
  * @param fieldName The name of the field to index
  * @param ascending True for ascending values, false for descending values
  */
case class Index(
  fieldName : String,
  ascending : Boolean) extends IndexTrait {
  override def construct : BSONBuilder = {
    super.construct.integer(fieldName, if (ascending) 1 else -1)
  }
  def name = fieldName
}

/** A Compound Index
  * @see [[http://docs.mongodb.org/v3.0/core/index-compound/]]
  * @see [[http://docs.mongodb.org/v3.0/reference/method/db.collection.createIndex/#db.collection.createIndex]]
  * @param fields A mapping of field names to whether that field is ascending (true) or descending (false)
  */
case class CompoundIndex(
  fields : (String, Boolean)*) extends IndexTrait {
  require(fields.size <= 31, "Only up to 31 fields may comprise a compound index")
  override def construct : BSONBuilder = {
    val b = super.construct
    for ((field, ascending) ← fields) { b.integer(field, if (ascending) 1 else -1) }
    b
  }
  def name = fields.map { case (name, ascending) ⇒ name } mkString (".")
}

/** A Text Index
  * @see [[http://docs.mongodb.org/v3.0/core/index-text/]]
  * @param fields The string (or array of string) fields for the text index
  */
case class TextIndex(
  fields : String*) extends IndexTrait {
  override def construct : BSONBuilder = {
    val b = super.construct
    for (field ← fields) { b.string(field, "text") }
    b
  }
  def name = fields.mkString(".") + ".text"
}

/** A Hashed Index
  * @see [[http://docs.mongodb.org/v3.0/core/index-hashed/]]
  * @param field The name of the field for the hashed index
  */
case class HashedIndex(
  field : String) extends IndexTrait {
  override def construct : BSONBuilder = {
    val b = super.construct
    b.string(field, "hashed")
  }
  def name = field + ".hashed"
}

/** A 2d Index
  * @see [[http://docs.mongodb.org/v3.0/core/2d/]]
  * @param locationField The name of the field that contains the GeoJSON location information
  * @param otherFields Other fields to include in a compound index (optional)
  */
case class TwoDIndex(
  locationField : String,
  otherFields : (String, Boolean)*) extends IndexTrait {
  require(otherFields.size < 31, "Only up to 31 fields may comprise a compound index")
  override def construct : BSONBuilder = {
    val b = super.construct
    b.string(locationField, "2d")
    for ((field, ascending) ← otherFields) { b.integer(field, if (ascending) 1 else -1) }
    b
  }
  def name = locationField + ".2d"
}

/** A 2dsphere Index
  * @see [[http://docs.mongodb.org/v3.0/core/2dsphere/]]
  * @param prefixFields Non-2d fields that come before the location field
  * @param locationField The name of the field that contains the location information
  * @param suffixFields Non-2d fields that come after the locationf ield
  */
case class TwoDSphereIndex(
  prefixFields : Seq[(String, Boolean)] = Seq.empty[(String, Boolean)],
  locationField : String,
  suffixFields : Seq[(String, Boolean)] = Seq.empty[(String, Boolean)]) extends IndexTrait {
  require(prefixFields.size + suffixFields.size < 31, "Only up to 31 fields may comprise a compound index")

  override def construct : BSONBuilder = {
    val b = super.construct
    for ((field, ascending) ← prefixFields) { b.integer(field, if (ascending) 1 else -1) }
    b.string(locationField, "2dsphere")
    for ((field, ascending) ← suffixFields) { b.integer(field, if (ascending) 1 else -1) }
    b
  }
  def name = {
    (prefixFields.map { case (name, asc) ⇒ name } mkString ".") +
      s".$locationField." +
      (suffixFields.map { case (name, asc) ⇒ name } mkString ".") +
      ".2dsphere"
  }
}

/** A geoHaystack Index
  * @see [[http://docs.mongodb.org/v3.0/core/geohaystack/]]
  * @param locationField
  * @param otherFields
  */
case class GeoHaystackIndex(
  locationField : String,
  otherFields : (String, Boolean)*) extends IndexTrait {
  require(otherFields.size < 31, "Only up to 31 fields may comprise a compound index")

  override def construct : BSONBuilder = {
    val b = super.construct
    b.string(locationField, "geoHaystack")
    for ((field, ascending) ← otherFields) { b.integer(field, if (ascending) 1 else -1) }
    b
  }
  def name = locationField + "." + otherFields.map { case (name, asc) ⇒ name } mkString (".") + ".geoHaystack"
}
