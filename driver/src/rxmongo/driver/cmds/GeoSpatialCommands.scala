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

package rxmongo.driver.cmds

import akka.util.ByteString
import rxmongo.bson._
import rxmongo.driver.Command
/* TODO: Fix GeoSpatial commands codecs


sealed trait GeoJSON extends BSONProvider

case class GeoPoint(long : Double, lat : Double) extends GeoJSON {
  def toByteString : ByteString = {
    val b = BSONBuilder()
    b.string("type", "Point")
    b.array("coordinates", long, lat)
    b.toByteString
  }
  def asArray : BSONArray = BSONArray(long, lat)
}

object GeoPoint {
  implicit object Codec extends BSONCodec[GeoPoint, BSONObject] {
    def code : TypeCode = ObjectCode
    def write(value : GeoPoint) : BSONObject = { value.toBSONObject }
    def read(value : BSONObject) : GeoPoint = {
      val a = value.getAsArray[Double, BSONDouble]("coordinates")
      GeoPoint(a(0), a(1))
    }
  }
}

case class GeoLineString(positions : Seq[GeoPoint]) extends GeoJSON {
  def toByteString : ByteString = {
    val b = BSONBuilder()
    b.string("type", "LineString")
    b.array[GeoPoint, BSONObject]("coordinates", positions)
    b.toByteString
  }
}

case class GeoPolygon(linearRing : Seq[GeoPoint], holes : Seq[Seq[GeoPoint]]) extends GeoJSON {
  def toByteString : ByteString = {
    val b = BSONBuilder()
    b.string("type", "Polygon")
    b.array("coordinates", asArray)
    b.toByteString
  }
  def asArray : BSONArray = {
    val ring = BSONArray(linearRing.map { p ⇒ p.asArray })
    val h = holes.map { one ⇒ BSONArray(one.map { p ⇒ p.asArray }) }
    BSONArray(h :+ ring)
  }
}

object GeoPolygon {
  implicit object Codec extends BSONCodec[GeoPolygon, BSONObject]
}

case class GeoMultiPoint(positions : Seq[GeoPoint]) extends GeoJSON {
  def toByteString : ByteString = {
    val b = BSONBuilder()
    b.string("type", "MultiPoint")
    b.array[GeoPoint, BSONObject]("coordinates", positions)
    b.toByteString
  }
}

case class GeoMultiPolygon(polygons : Seq[GeoPolygon]) extends GeoJSON {
  def toByteString : ByteString = {
    val b = BSONBuilder()
    b.string("type", "MultiPolygon")
    b.array("coordinates", polygons)
    b.toByteString
  }
}

case class GeometryCollection(geometries : Seq[GeoJSON]) extends GeoJSON {
  def toByteString : ByteString = {
    val b = BSONBuilder()
    b.string("type", "GeometryCollection")
    val geos = geometries.map { g ⇒ g.result }
    b.array("geometries", geos)
    b.toByteString
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
  b.obj("near", near.result)
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
  b.obj("near", near.result)
  b.obj("search", query.result)
  b.integer("maxDistance", maxDistance)
  limit.map { l ⇒ b.integer("limit", l) }
  b.result
})
 */
