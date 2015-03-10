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

import akka.util.{ ByteIterator, ByteString, ByteStringBuilder }

import scala.collection._

case class BSONObject private[bson] ( final val doc : BSONDocument) extends BSONValue
  with MapLike[String, BSONValue, BSONObject] with Map[String, BSONValue] {
  final val code : TypeCode = ObjectCode

  final def value : Map[String, BSONValue] = toMap

  def toMap : Map[String, BSONValue] = doc.map {
    case (k, (b, bi)) ⇒
      k -> BSONValue(b, bi.clone())
  }

  override def toByteString : ByteString = doc.toByteString

  private[bson] def addTo(bldr : ByteStringBuilder) : Unit = {
    bldr ++= doc.toByteString
  }

  override def equals(other : Any) : Boolean = {
    other match {
      case that : BSONObject ⇒ this.doc.equals(that.doc)
      case _ ⇒ false
    }
  }

  def get(key : String) : Option[BSONValue] = {
    doc.get(key).map {
      case (b, bi) ⇒
        BSONValue(b, bi.clone())
    }
  }

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

  def byteSize : Int = {
    doc.docItr match {
      case Some(itr) ⇒ itr.length
      case None ⇒ doc.iterator.foldLeft(0) { case (sum, (key, (bc, bi))) ⇒ sum + bi.length }

    }
  }

  def filter(filt : (String, BSONValue) ⇒ Boolean) : BSONObject = {
    BSONObject(doc.filter {
      case (key, (typeCode, itr)) ⇒
        filt(key, BSONValue(typeCode, itr))
    })
  }

  override def contains(key : String) : Boolean = doc.contains(key)

  def getTypeCode(keyToFind : String) : TypeCode = {
    doc.get(keyToFind) match {
      case Some(v) ⇒ TypeCode(v._1)
      case None    ⇒ NotACode
    }
  }

  def getOptionObj(keyToFind : String) : Option[BSONObject] = {
    get(keyToFind) map { case (v) ⇒ v.asInstanceOf[BSONObject] }
  }

  def getObj(keyToFind : String) : BSONObject = {
    getOptionObj(keyToFind) match {
      case Some(obj) ⇒ obj
      case _ ⇒ throw new NoSuchElementException(keyToFind)
    }
  }

  /** Get with conversion to another type
    *
    * Gets the BSONValue associated with the provided `key`, and converts it to `T` via the Codec[T].
    *
    * @tparam T The type to which the call wishes the BSONValue to be decoded.
    * @param key The key to look up in the object to obtain the value
    * @return Success(None) if the key is not found, Success(Some(T)) if the key is found and converted successfully,
    * Failure(Throwable) if the conversion failed
    *
    * If there is no matching value, or the value could not be deserialized or converted, returns a `None`.
    */
  def getAs[T](key : String)(implicit codec : Codec[T]) : T = {
    get(key) match {
      case Some(value : BSONValue @unchecked) if value.code == codec.code ⇒ codec.read(value)
      case Some(value : BSONValue) ⇒ throw new IllegalArgumentException(
        s"Expected type ${codec.typeName} for key '$key' but got type ${value.typeName}")
      case None ⇒ throw new NoSuchElementException(key)
    }
  }

  def getAsObject[T](key : String)(implicit codec : Codec[T]) : T = {
    getAs[T](key)(codec)
  }

  def getAsSeq[T](key : String)(implicit codec : Codec[T]) : Seq[T] = getAsIterator[T](key).toSeq

  def getAsIterator[T](key : String)(implicit codec : Codec[T]) : Iterator[T] = {
    get(key) match {
      case Some(v : BSONValue) if v.code == ArrayCode ⇒
        iterator.map {
          case v : BSONValue if v.code == codec.code ⇒ codec.read(v)
          case v : BSONValue ⇒ throw new IllegalArgumentException(
            s"Expected type ${codec.typeName} but got type ${v.typeName}"
          )
        }
      case Some(v : BSONValue) ⇒ throw new IllegalArgumentException(
        s"Expected type BSONArray for key '$key' but got type ${v.typeName}"
      )
      case None ⇒ throw new NoSuchElementException(key)
    }
  }

  def getAsMap[T](key : String)(implicit codec : Codec[T]) : immutable.Map[String, T] = {
    iterator.map {
      case (k, v : BSONValue @unchecked) if v.code == codec.code ⇒ k -> codec.read(v)
      case (k, v) ⇒
        throw new IllegalArgumentException(
          s"Expected type ${codec.typeName} for key '$k' but got type ${v.typeName}")
    }.toMap
  }

  def getAsString(key : String) : String = getAs[String](key)
  def getAsInt(key : String) : Int = getAs[Int](key)
  def getAsLong(key : String) : Long = getAs[Long](key)
  def getAsDouble(key : String) : Double = getAs[Double](key)
  def getAsDate(key : String) : Date = getAs[Date](key)
  def getAsBoolean(key : String) : Boolean = getAs[Boolean](key)

  def getOptionalObject[T](key : String)(implicit codec : Codec[T]) : Option[T] = {
    try { Some(getAs[T](key)(codec)) } catch { case x : Exception ⇒ None }
  }
  def getOptionalArray[T](key : String)(implicit codec : Codec[T]) : Option[Seq[T]] = {
    try { Some(getAsSeq[T](key)(codec)) } catch { case x : Exception ⇒ None }
  }
  def getOptionalString(key : String) : Option[String] = {
    try { Some(getAs[String](key)) } catch { case x : Exception ⇒ None }
  }
  def getOptionalInt(key : String) : Option[Int] = {
    try { Some(getAs[Int](key)) } catch { case x : Exception ⇒ None }
  }
  def getOptionalDouble(key : String) : Option[Double] = {
    try { Some(getAs[Double](key)) } catch { case x : Exception ⇒ None }
  }
  def getOptionalDate(key : String) : Option[Date] = {
    try { Some(getAs[Date](key)) } catch { case x : Exception ⇒ None }
  }
  def getOptionalBoolean(key : String) : Option[Boolean] = {
    try { Some(getAs[Boolean](key)) } catch { case x : Exception ⇒ None }
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

  def to[T](implicit codec : Codec[T]) : T = codec.read(this)

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

  def apply[T](key : String, value : T)(implicit codec : Codec[T]) : BSONObject = {
    val bldr = BSONBuilder()
    bldr.obj(key, BSONObject(value))
    bldr.toBSONObject
  }

  def apply[T](value : T)(implicit codec : Codec[T]) : BSONObject = {
    BSONObject(codec.write(value))
  }

  def from(key : String, value : Any) : BSONObject = {
    val bldr = BSONBuilder()
    bldr.anyField(key, value)
    bldr.toBSONObject
  }

  def from(data : Seq[(String, Any)]) : BSONObject = {
    val bldr = ByteString.newBuilder
    bldr.putAnyObject(data)
    BSONObject(bldr.result())
  }

}
