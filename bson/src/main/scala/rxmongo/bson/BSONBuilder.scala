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

package rxmongo.bson

import java.nio.ByteOrder
import java.util.Date
import java.util.regex.Pattern

import akka.util.{ ByteStringBuilder, ByteString }

import scala.collection.mutable

/** A Trait for things that provide BSON ByteStrings
  *
  */
trait BSONProvider {

  /** Finalize and return ByteString.
    * The construction of the ByteString is finalized and a ByteString is returned.
    * @return
    */
  def wrapAndTerminate : ByteString

  /** Convert To BSONObject.
    * Generate the ByteString and then use that to construct a BSONObject.
    * @return The BSONObject corresponding
    */
  def toBSONObject : BSONObject = BSONObject(wrapAndTerminate)

  /** Convert to BSONObject.
    * Generate teh ByteString and then use that to construct a BSONObject
    * @return The corresponding BSONObject
    */
  def result : BSONObject = toBSONObject
}

trait ByteStringBuilderWrapper extends BSONProvider {
  implicit val byteOrder = ByteOrder.LITTLE_ENDIAN

  val bldr : ByteStringBuilder

  def wrapAndTerminate : ByteString = {
    val content = bldr.result()
    val bsb = ByteString.newBuilder
    bsb.putInt(content.length + 5) // 4 for length, 1 for terminating 0 byte
    bsb ++= content
    bsb.putByte(0)
    bsb.result()
  }
}

/** A Trait for things that provide BSON
  *
  * The RxMongo-BSON library aims to provide builders of ByteStrings rather than implement representations of BSON
  * as objects. A Provider then is something that assists in the construction of those ByteStrings either by
  * adding something to an existing builder or finalizing into a ByteString
  */
trait BSONConstructor extends BSONProvider {

  /** Add to a BSONBuilder.
    * Whatever this provides as BSON is added to the provided builder. The same, or a new BSONBuilder must be returned.
    * @param builder The build to which this object's provided BSON is added
    * @return The resulting BSONBuilder
    */
  def addTo(builder : ByteStringBuilder) : ByteStringBuilder

  def build : ByteStringBuilder = {
    val bldr = ByteString.newBuilder
    addTo(bldr)
    bldr
  }

  /** Finalize and return ByteString.
    * The construction of the ByteString is finalized and a ByteString is returned.
    * @return
    */
  override def wrapAndTerminate : ByteString = { build.wrapAndTerminate }
}

/** Builder for BSON Object
  *
  * This uses the builder pattern to allow construction of a BSON.Object by using a ByteStringBuilder to construct
  * the corresponding ByteString and then instantiating BSON.Object with the immutable ByteString
  */
class BSONBuilder(val bldr : ByteStringBuilder = ByteString.newBuilder)
  extends ByteStringBuilderWrapper with mutable.Builder[(String, Any), BSONObject] {

  def +=(elem : (String, Any)) : this.type = {
    bldr.anyField(elem._1, elem._2)
    this
  }

  def clear() : Unit = bldr.clear()

  def length() : Int = bldr.length

  override def wrapAndTerminate : ByteString = { bldr.wrapAndTerminate }

  override def sizeHint(size : Int) = { bldr.sizeHint(size) }

  @inline def double(key : String, value : Double) : this.type = { bldr.double(key, value); this }
  @inline def string(key : String, value : String) : this.type = { bldr.string(key, value); this }

  @inline def obj(key : String, value : BSONObject) : this.type = { bldr.obj(key, value); this }
  @inline def obj(key : String, value : BSONBuilder) : this.type = { bldr.obj(key, value); this }
  @inline def obj[T](key : String, fieldKey : String, fieldValue : T)(implicit codec : Codec[T]) : this.type = {
    bldr.obj(key, fieldKey, fieldValue)
    this
  }

  @inline def obj[T](key : String, value : T)(implicit codec : Codec[T]) : this.type = { bldr.obj(key, value); this }
  @inline def obj(key : String, value : ByteString) : this.type = { bldr.obj(key, value); this }

  @inline def obj[T](key : String, fields : Map[String, T])(implicit codec : Codec[T]) : this.type = {
    bldr.obj(key, fields)
    this
  }

  @inline def anyObj(key : String, value : Iterable[(String, Any)]) : this.type = { bldr.anyObj(key, value); this }

  @inline def anyObj(key : String, field1 : (String, Any), fields : (String, Any)*) : this.type = {
    bldr.anyObj(key, Seq(field1) ++ fields)
    this
  }

  @inline def anyObj(key : String, fields : Map[String, Any]) : this.type = { bldr.anyObj(key, fields); this }

  @inline def array(key : String, value : BSONArray) : this.type = { bldr.array(key, value); this }

  @inline def array[T](key : String, values : Iterable[T])(implicit codec : Codec[T]) : this.type = {
    bldr.array[T](key, values)
    this
  }

  @inline def anyArray(key : String, values : Iterable[Any]) : this.type = { bldr.anyArray(key, values); this }

  @inline def array(key : String, v1 : Any, values : Any*) : this.type = { bldr.anyArray(key, v1 +: values); this }

  @inline def binary(key : String, blob : Array[Byte], subtype : BinarySubtype) : this.type = {
    bldr.binary(key, blob, subtype)
    this
  }

  @inline def binary(key : String, blob : Iterator[Byte], subtype : BinarySubtype) : this.type = {
    bldr.binary(key, blob, subtype)
    this
  }

  @inline def binary(key : String, blob : ByteString, subtype : BinarySubtype) : this.type = {
    bldr.binary(key, blob, subtype)
    this
  }

  @inline def undefined(key : String) : this.type = { bldr.undefined(key); this }

  @inline def objectID(key : String, value : Array[Byte]) : this.type = { bldr.objectID(key, value); this }

  @inline def boolean(key : String, value : Boolean) : this.type = { bldr.boolean(key, value); this }

  @inline def date(key : String, time : Date) : this.type = { bldr.date(key, time); this }

  @inline def date(key : String, time : Long) : this.type = { bldr.date(key, time); this }

  @inline def nil(key : String) : this.type = { bldr.nil(key); this }

  @inline def regex(key : String, pattern : String, options : String = "") : this.type = {
    bldr.regex(key, pattern, options)
    this
  }

  @inline def regex(key : String, regex : Pattern) : this.type = { bldr.regex(key, regex); this }

  @inline def dbPointer(key : String, referent : String, id : Array[Byte]) : this.type = {
    bldr.dbPointer(key, referent, id)
    this
  }

  @inline def jsCode(key : String, code : String) : this.type = { bldr.jsCode(key, code); this }

  @inline def symbol(key : String, symbol : String) : this.type = { bldr.symbol(key, symbol); this }

  @inline def scopedJsCode(key : String, code : String, scope : BSONObject) : this.type = {
    bldr.scopedJsCode(key, code, scope)
    this
  }

  @inline def integer(key : String, value : Int) : this.type = { bldr.integer(key, value); this }

  @inline def timestamp(key : String, value : Long) : this.type = { bldr.timestamp(key, value); this }

  @inline def long(key : String, value : Long) : this.type = { bldr.long(key, value); this }

  @inline def field[T](key : String, value : T)(implicit codec : Codec[T]) : this.type = {
    bldr.field[T](key, value)(codec)
    this
  }

  @inline def addFieldsOf[T](value : T)(implicit codec : Codec[T]) : this.type = {
    bldr.addFieldsOf[T](value)(codec)
    this
  }

  @inline def anyField(key : String, anyVal : Any) : this.type = { bldr.anyField(key, anyVal); this }

  @inline def put(value : BSONConstructor) : this.type = { bldr.put(value); this }

  @inline def arrayObj[T](key : String, arrayKey : String, values : Iterable[T])(implicit codec : Codec[T]) : this.type = {
    bldr.arrayObj[T](key, arrayKey, values)
    this
  }

  @inline def append(key : String, value : BSONValue) : this.type = { bldr.field(key, value); this }

}

object BSONBuilder {
  def apply() : BSONBuilder = new BSONBuilder()
  def apply(b : ByteStringBuilder) : BSONBuilder = new BSONBuilder(b)
}
