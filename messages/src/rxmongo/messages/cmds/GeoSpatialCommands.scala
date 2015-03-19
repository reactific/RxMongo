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

package rxmongo.messages.cmds

import akka.util.ByteIterator
import rxmongo.bson._
import rxmongo.messages.Command
import rxmongo.messages.Query


sealed trait GeoJSON

object GeoJSON {

  val Point = "Point"
  val LineString = "LineString"
  val Polygon = "Polygon"
  val MultiPoint = "MultiPoint"
  val MultiPolygon = "MultiPolygon"
  val Collection = "GeometryCollection"

  implicit object Codec extends DocumentCodec[GeoJSON] {
    def write(value : GeoJSON, bldr: BSONBuilder) : BSONBuilder = {
      value match {
        case x: GeoPoint ⇒ GeoPoint.Codec.write(x, bldr)
        case x: GeoLineString ⇒ GeoLineString.Codec.write(x, bldr)
        case x: GeoPolygon ⇒ GeoPolygon.Codec.write(x, bldr)
        case x: GeoMultiPoint ⇒ GeoMultiPoint.Codec.write(x, bldr)
        case x: GeoMultiPolygon ⇒ GeoMultiPolygon.Codec.write(x, bldr)
        case x: GeometryCollection ⇒ GeometryCollection.Codec.write(x, bldr)
        case x ⇒ throw new NoSuchElementException(s"$x is not a GeoJSON")
      }
    }
    def read(doc : BSONDocument) : GeoJSON = {
      doc.asString("type") match {
        case Point ⇒ GeoPoint.Codec.read(doc)
        case LineString ⇒ GeoLineString.Codec.read(doc)
        case Polygon ⇒ GeoPolygon.Codec.read(doc)
        case MultiPoint ⇒ GeoMultiPoint.Codec.read(doc)
        case MultiPolygon ⇒ GeoMultiPolygon.Codec.read(doc)
        case Collection ⇒ GeometryCollection.Codec.read(doc)
      }
    }
  }
}

case class GeoPoint(long : Double, lat : Double) extends GeoJSON {
  def asArray : BSONArray = BSONArray(long, lat)
}

object GeoPoint {
  implicit object Codec extends DocumentCodec[GeoPoint] {
    def write(value : GeoPoint, bldr: BSONBuilder) : BSONBuilder = {
      bldr.string("type", GeoJSON.Point)
      bldr.array("coordinates", value.long, value.lat)
    }
    def read(doc : BSONDocument) : GeoPoint = {
      require(doc.asString("type") == GeoJSON.Point)
      val coords = doc.asSeq[Double]("coordinates")
      GeoPoint(coords(0), coords(1))
    }
  }

  implicit object SeqCodec extends Codec[Seq[GeoPoint]] {
    override val code = ArrayCode
    def write(value: Seq[GeoPoint], bldr: BSONBuilder) : BSONBuilder = {
      bldr.bldr.putArray[GeoPoint](value)(GeoPoint.Codec)
      bldr
    }
    def read(itr: ByteIterator) : Seq[GeoPoint] = {
      val i = new BSONIterator(itr)
      for ((k, (c,b)) ← i) yield {
        require(c == ObjectCode.code)
        GeoPoint.Codec.read(b)
      }
    }.toSeq
  }
}

case class GeoLineString(coordinates : Seq[GeoPoint]) extends GeoJSON

object GeoLineString {
  implicit object Codec extends DocumentCodec[GeoLineString] {
    def write(value: GeoLineString, bldr: BSONBuilder) : BSONBuilder = {
      bldr.string("type", GeoJSON.LineString)
      bldr.array("coordinates", value.coordinates)
    }
    def read(doc: BSONDocument) : GeoLineString = {
      require(doc.asString("type") == GeoJSON.LineString)
      val coords = doc.asSeq[GeoPoint]("coordinates")
      GeoLineString(coords)
    }
  }
}

case class GeoPolygon(coordinates : Seq[Seq[GeoPoint]]) extends GeoJSON {
  def asArray: BSONArray = {
    BSONArray ( coordinates map { segment ⇒ BSONArray( segment.map { p ⇒ p.asArray } ) } )
  }
}

object GeoPolygon {
  implicit object Codec extends DocumentCodec[GeoPolygon] {
    def write(value: GeoPolygon, bldr: BSONBuilder) : BSONBuilder = {
      bldr.string("type", GeoJSON.Polygon)
      bldr.array("coordinates", value.asArray)
    }
    def read(doc: BSONDocument) : GeoPolygon = {
      require(doc.asString("type") == GeoJSON.Polygon)
      GeoPolygon(doc.asSeq[Seq[GeoPoint]]("coordinates"))
    }
  }
  implicit object SeqCodec extends Codec[Seq[GeoPolygon]] {
    override val code = ArrayCode
    def write(value: Seq[GeoPolygon], bldr: BSONBuilder) : BSONBuilder = {
      bldr.bldr.putArray[GeoPolygon](value)(GeoPolygon.Codec)
      bldr
    }
    def read(itr: ByteIterator) : Seq[GeoPolygon] = {
      val i = new BSONIterator(itr)
      for ((k, (c,b)) ← i) yield {
        require(c == ObjectCode.code)
        GeoPolygon.Codec.read(b)
      }
    }.toSeq
  }
}

case class GeoMultiPoint(coordinates : Seq[GeoPoint]) extends GeoJSON

object GeoMultiPoint {
  implicit object Codec extends DocumentCodec[GeoMultiPoint] {
    def write(value: GeoMultiPoint, bldr: BSONBuilder) : BSONBuilder = {
      bldr.string("type", GeoJSON.MultiPoint)
      bldr.array[GeoPoint]("coordinates", value.coordinates)
    }
    def read(doc: BSONDocument) : GeoMultiPoint = {
      require(doc.asString("type") == GeoJSON.MultiPoint)
      GeoMultiPoint(doc.asSeq[GeoPoint]("coordinates"))
    }
  }
}

case class GeoMultiPolygon(coordinates : Seq[GeoPolygon]) extends GeoJSON

object GeoMultiPolygon {

  implicit object Codec extends DocumentCodec[GeoMultiPolygon] {
    def write(value: GeoMultiPolygon, bldr: BSONBuilder): BSONBuilder = {
      bldr.string("type", GeoJSON.MultiPolygon)
      bldr.array("coordinates", value.coordinates)
    }

    def read(doc: BSONDocument): GeoMultiPolygon = {
      require(doc.asString("type") == GeoJSON.MultiPolygon)
      GeoMultiPolygon(doc.asSeq[GeoPolygon]("coordinates"))
    }
  }
}

case class GeometryCollection(geometries : Seq[GeoJSON]) extends GeoJSON

object GeometryCollection {

  implicit object Codec extends DocumentCodec[GeometryCollection] {
    def write(value: GeometryCollection, bldr: BSONBuilder): BSONBuilder = {
      bldr.string("type", GeoJSON.Collection)
      bldr.array("geometries", value.geometries)
    }

    def read(doc: BSONDocument): GeometryCollection = {
      require(doc.asString("type") == GeoJSON.Collection)
      GeometryCollection(doc.asSeq[GeoJSON]("geometries"))
    }
  }
}

/** geoNear
  * Performs a geospatial query that returns the documents closest to a given point.
  * @see [[http://docs.mongodb.org/master/reference/command/geoNear/#dbcmd.geoNear]]
  * @param db The name of the database containing the collection to query.
  * @param coll The name of the collection to query
  * @param near The point for which to find the closest documents. If using a 2dsphere index, you can specify the point
  *         as either a GeoJSON point or legacy coordinate pair. If using a 2d index, specify the point as a legacy
  *         coordinate pair.
  * @param spherical Required if using a 2dsphere index. Determines how MongoDB calculates the distance.
  *              The default value is false. If true, then MongoDB uses spherical geometry to calculate distances
  *              in meters if the specified (near) point is a GeoJSON point and in radians if the specified (near)
  *              point is a legacy coordinate pair. If false, then MongoDB uses 2d planar geometry to calculate
  *              distance between points. If using a 2dsphere index, spherical must be true.
  * @param limit Optional. The maximum number of documents to return. The default value is 100. See also the num option.
  * @param minDistance Optional. The minimum distance from the center point that the documents must be. MongoDB filters
  *                the results to those documents that are at least the specified distance from the center point.
  *                Only available for use with 2dsphere index. Specify the distance in meters for GeoJSON data
  *                and in radians for legacy coordinate pairs.
  * @param maxDistance Optional. The maximum distance from the center point that the documents can be. MongoDB limits
  *                the results to those documents that fall within the specified distance from the center point.
  *                Specify the distance in meters for GeoJSON data and in radians for legacy coordinate pairs.
  * @param query Optional. Limits the results to the documents that match the query. The query syntax is the usual
  *          MongoDB read operation query syntax. You cannot specify a $near predicate in the query field of
  *          the geoNear command.
  * @param distanceMultiplier The factor to multiply all distances returned by the query. For example, use the
  *                       distanceMultiplier to convert radians, as returned by a spherical query, to kilometers
  *                       by multiplying by the radius of the Earth.
  * @param includeLocs Optional. If this is true, the query returns the location of the matching documents in the
  *                results. The default is false. This option is useful when a location field contains multiple
  *                locations. To specify a field within a subdocument, use dot notation.
  */
case class GeoNearCmd(
  db : String,
  coll : String,
  near : GeoJSON,
  spherical : Boolean,
  limit : Option[Int] = None,
  minDistance : Option[Double] = None,
  maxDistance : Option[Double] = None,
  query : Option[Query] = None,
  distanceMultiplier : Option[Double] = None,
  includeLocs : Option[Boolean] = None
) extends Command(db, {
  val b = BSONBuilder()
  b.string("geoNear", coll)
  b.obj("near", near)
  b.boolean("spherical", spherical)
  limit.map { l ⇒ b.integer("limit", l) }
  minDistance.map { md ⇒ b.double("minDistance", md) }
  maxDistance.map { md ⇒ b.double("maxDistance", md) }
  query.map { q ⇒ b.obj("query", q.result) }
  distanceMultiplier.map { dm ⇒ b.double("distanceMultiplier", dm) }
  includeLocs.map { il ⇒ b.boolean("includeLocs", il) }
  b.result
})

/** geoSearch
  * Performs a geospatial query that uses MongoDB’s haystack index functionality.
  * @see [[http://docs.mongodb.org/master/reference/command/geoSearch/]]
  * @param db The name of the database containing the collection to search
  * @param coll THe name of the collection to search
  * @param near The GeoJSON point around which to search
  * @param query The query that selects candidate objects to search (the haystack)
  * @param maxDistance The maximum distance result documents are allowed to be from the "near" point
  * @param limit The maximum number of documents to return, defaults to 50
  */
case class GeoSearchCmd(
  db : String,
  coll : String,
  near : GeoJSON,
  query : Query,
  maxDistance : Int,
  limit : Option[Int] = Some(50)
) extends Command(db, {
  val b = BSONBuilder()
  b.string("geoSearch", coll)
  b.obj("near", near)
  b.obj("search", query.result)
  b.integer("maxDistance", maxDistance)
  limit.map { l ⇒ b.integer("limit", l) }
  b.result
})
