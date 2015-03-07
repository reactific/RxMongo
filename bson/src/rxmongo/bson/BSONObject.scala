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

import java.util.Date

import akka.util.{ByteIterator, ByteString, ByteStringBuilder}

import scala.collection._

case class BSONObject private[bson] ( final val doc : BSONDocument) extends BSONValue
with MapLike[String, BSONValue, BSONObject] with Map[String, BSONValue] {
  final val code : TypeCode = ObjectCode

  final def value : Map[String, BSONValue] = toMap

  def toMap : Map[String, BSONValue] = doc.map {
    case (k, (b, bi)) ⇒
      k -> BSONValue(b, bi.clone())
  }

  private[bson] def addTo(bldr : ByteStringBuilder) : Unit = {
    bldr ++= doc.toByteString
  }

  override def equals(other : Any) : Boolean = {
    other match {
      case that : BSONObject ⇒ this.doc.equals(that.doc)
      case _ ⇒ false
    }
  }

  def get(key : String) : Option[BSONValue] = doc.get(key).map { case (b, bi) ⇒ BSONValue(b, bi.clone()) }

  def +[B1 >: BSONValue](kv : (String, B1)) : BSONObject = {
    val pair = kv._1 → kv._2.asInstanceOf[BSONValue].pair
    BSONObject(doc + pair)
  }

  def -(key : String) : BSONObject = BSONObject(doc.data - key)

  def iterator : Iterator[(String, BSONValue)] = {
    new Iterator[(String, BSONValue)] {
      val i = doc.iterator
      def hasNext : Boolean = i.hasNext
      def next() : (String, BSONValue) = {
        val (key, (b, bi)) = i.next()
        key → BSONValue(b, bi.clone())
      }
    }
  }

  override def size : Int = iterator.size

  /*  def filter(filt : (String,BSONValue) ⇒ Boolean) : BSONObject = {
    BSONObject(value.filter { case (key,(typeCode,itr)) ⇒
      filt(key,BSONValue(typeCode,itr))
    })
  }
*/
  override def contains(key : String) : Boolean = doc.contains(key)

  def getTypeCode(keyToFind : String) : TypeCode = {
    doc.get(keyToFind) match {
      case Some(v) ⇒ TypeCode(v._1)
      case None    ⇒ NotACode
    }
  }

  def getObject(keyToFind : String) : Option[BSONObject] = {
    get(keyToFind) map { case (v) ⇒ v.asInstanceOf[BSONObject] }
  }

  def getObj(keyToFind : String) : BSONObject = {
    getObject(keyToFind) match {
      case Some(obj) ⇒ obj
      case _ ⇒ throw new NoSuchElementException(keyToFind)
    }
  }

  /** Get with conversion to another type
    *
    * Gets the BSONValue associated with the provided `key`, and converts it to `T` via the BSONCodec[T].
    *
    * @tparam T The type to which the call wishes the BSONValue to be decoded.
    * @param key The key to look up in the object to obtain the value
    * @return Success(None) if the key is not found, Success(Some(T)) if the key is found and converted successfully,
    * Failure(Throwable) if the conversion failed
    *
    * If there is no matching value, or the value could not be deserialized or converted, returns a `None`.
    */
  def getAs[T, B <: BSONValue](key : String)(implicit codec : BSONCodec[T, B]) : T = {
    get(key) match {
      case Some(value : B @unchecked) if value.code == codec.code ⇒ codec.read(value)
      case Some(value : BSONValue) ⇒ throw new IllegalArgumentException(
        s"Expected type ${codec.typeName} for key '$key' but got type ${value.typeName}")
      case None ⇒ throw new NoSuchElementException(key)
    }
  }

  def getAsObject[T](key : String)(implicit codec : BSONCodec[T, BSONObject]) : T = {
    getAs[T, BSONObject](key)(codec)
  }

  def getAsSeq[T, B <: BSONValue](key : String)(implicit codec : BSONCodec[T, B]) : Iterator[T] = {
    get(key) match {
      case Some(v : BSONValue) if v.code == ArrayCode ⇒ doc.asInstanceOf[BSONArray].as[T, B]
      case Some(v : BSONValue) ⇒ throw new IllegalArgumentException(
        s"Expected type BSONArray for key '$key' but got type ${v.typeName}"
      )
      case None ⇒ throw new NoSuchElementException(key)
    }
  }

  def getAsMap[T, B <: BSONValue](key : String)(implicit codec : BSONCodec[T, B]) : immutable.Map[String, T] = {
    iterator.map {
      case (k, v : B @unchecked) if v.code == codec.code ⇒ k -> codec.read(v)
      case (k, v) ⇒
        throw new IllegalArgumentException(
          s"Expected type ${codec.typeName} for key '$k' but got type ${v.typeName}")
    }.toMap
  }

  def getAsString(key : String) : String = getAs[String, BSONString](key)
  def getAsInt(key : String) : Int = getAs[Int, BSONInteger](key)
  def getAsLong(key : String) : Long = getAs[Long, BSONLong](key)
  def getAsDouble(key : String) : Double = getAs[Double, BSONDouble](key)
  def getAsDate(key : String) : Date = getAs[Date, BSONDate](key)
  def getAsBoolean(key : String) : Boolean = getAs[Boolean, BSONBoolean](key)
  def getAsArray[T, B <: BSONValue](key : String)(implicit codec : BSONCodec[T, B]) : Seq[T] = {
    getAsSeq[T, B](key).toSeq
  }

  def getOptionalObject[T](key : String)(implicit codec : BSONCodec[T, BSONObject]) : Option[T] = {
    try { Some(getAs[T, BSONObject](key)(codec)) } catch { case x : Exception ⇒ None }
  }
  def getOptionalArray[T, B <: BSONValue](key : String)(implicit codec : BSONCodec[T, B]) : Option[Seq[T]] = {
    try { Some(getAsArray[T, B](key)(codec)) } catch { case x : Exception ⇒ None }
  }
  def getOptionalString(key : String) : Option[String] = {
    try { Some(getAs[String, BSONString](key)) } catch { case x : Exception ⇒ None }
  }
  def getOptionalInt(key : String) : Option[Int] = {
    try { Some(getAs[Int, BSONInteger](key)) } catch { case x : Exception ⇒ None }
  }
  def getOptionalDouble(key : String) : Option[Double] = {
    try { Some(getAs[Double, BSONDouble](key)) } catch { case x : Exception ⇒ None }
  }
  def getOptionalDate(key : String) : Option[Date] = {
    try { Some(getAs[Date, BSONDate](key)) } catch { case x : Exception ⇒ None }
  }
  def getOptionalBoolean(key : String) : Option[Boolean] = {
    try { Some(getAs[Boolean, BSONBoolean](key)) } catch { case x : Exception ⇒ None }
  }

  def matches(other : BSONObject) : Boolean = {
    for ((k, v) ← other) {
      get(k) match {
        case Some(value) ⇒ if (value != v) return false
        case None ⇒ return false
      }
    }
    true
  }

  def toAnyMap : Map[String, Any] = toMap.map { case (k, v) ⇒ k -> v.value }

  def to[T](implicit codec : BSONCodec[T, BSONObject]) : T = codec.read(this)

  override def toString() : String = {
    val s = new StringBuilder
    val itr = iterator
    if (itr.isEmpty) {
      s.append("{}")
    } else {
      s.append("{ ")
      for ((k, v) ← itr) {
        s.append(k).append("->").append(v).append(", ")
      }
      s.setLength(s.length - 2)
      s.append(" }")
    }
    s.toString()
  }

  override protected[this] def newBuilder : mutable.Builder[(String, BSONValue), BSONObject] = BSONBuilder()

  override def empty : BSONObject = BSONObject.empty
}

object BSONObject {

  def newBuilder = BSONBuilder()

  object empty extends BSONObject(BSONDocument.empty)

  def apply(buffer : ByteString) : BSONObject = BSONObject(buffer.iterator)
  def apply(itr : ByteIterator) : BSONObject = new BSONObject(BSONDocument.apply(itr))

  def apply() : BSONObject = empty

  def apply(data : (String, Any)*) : BSONObject = from(data.toSeq)

  def apply(data : Map[String, Any]) : BSONObject = from(data.toSeq)

  def apply[T](key : String, value : T)(implicit codec : BSONCodec[T, BSONObject]) : BSONObject = {
    val bldr = BSONBuilder()
    bldr.obj(key, BSONObject(value))
    bldr.toBSONObject
  }

  def apply[T](value : T)(implicit codec : BSONCodec[T, BSONObject]) : BSONObject = codec.write(value)

  def from(data : Seq[(String, Any)]) : BSONObject = {
    val bldr = BSONBuilder()
    bldr.append(data)
    bldr.toBSONObject
  }

}
