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

import akka.util.{ ByteIterator, ByteString, ByteStringBuilder }

import scala.collection._

case class BSONArray private[bson] ( final val doc : BSONDocument) extends BSONValue
  with SeqLike[BSONValue, BSONArray] {
  final val code : TypeCode = ArrayCode
  final def value : Seq[BSONValue] = seq

  override def equals(other : Any) : Boolean = {
    other match {
      case that : BSONArray ⇒ this.doc.equals(that.doc)
      case _ ⇒ false
    }
  }

  private[bson] def addTo(bldr : ByteStringBuilder) : Unit = {
    bldr ++= doc.toByteString
  }

  def length : Int = doc.size

  def iterator : Iterator[BSONValue] = {
    new Iterator[BSONValue] {
      val i = doc.iterator
      def hasNext : Boolean = i.hasNext
      def next() : BSONValue = {
        val (key, (b, bi)) = i.next()
        BSONValue(b, bi)
      }
    }
  }

  def apply(index : Int) = {
    doc.get(index.toString) match {
      case Some((typeCode, byteIterator)) ⇒ BSONValue(typeCode, byteIterator)
      case None ⇒ throw new NoSuchElementException(index.toString)
    }
  }

  protected[this] def newBuilder : mutable.Builder[BSONValue, BSONArray] = ???

  def seq : Seq[BSONValue] = doc.values.map { case (b, bi) ⇒ BSONValue(b, bi) }.toSeq

  override def toString() : String = {
    val s = new StringBuilder
    val itr = iterator
    if (itr.isEmpty) {
      s.append("[]")
    } else {
      s.append("[ ")
      for (v ← itr) {
        s.append(v.toString()).append(", ")
      }
      s.setLength(s.length - 2)
      s.append(" ]")
    }
    s.toString()
  }

  def asArray : Array[BSONValue] = doc.values.map { case (b, bi) ⇒ BSONValue(b, bi) }.toArray

  def as[T, B <: BSONValue](implicit codec : BSONCodec[T, B]) : Iterator[T] = {
    iterator.map {
      case v : BSONValue if v.code == codec.code ⇒ codec.read(v.asInstanceOf[B])
      case v : BSONValue ⇒ throw new IllegalArgumentException(
        s"Expected type ${codec.typeName} but got type ${v.typeName}"
      )
    }
  }

}

object BSONArray {

  object empty extends BSONArray(BSONDocument.empty)

  def apply(buffer : ByteString) : BSONArray = BSONArray(buffer.iterator)
  def apply(itr : ByteIterator) : BSONArray = new BSONArray(BSONDocument(itr))
  def apply(values : Iterator[BSONValue]) : BSONArray = {
    val pairs : Iterator[(String, (Byte, ByteIterator))] = values.zipWithIndex.map {
      case (v, i) ⇒ i.toString → (v.code.code → v.toByteString.iterator)
    }
    new BSONArray(BSONDocument(pairs.toMap))
  }

  def apply() : BSONArray = empty

  def apply(objs : BSONObject*) : BSONArray = {
    val arrayBuilder = BSONBuilder()
    objs.zipWithIndex.foreach {
      case (v, i) ⇒
        arrayBuilder.append(i.toString, v)
    }
    BSONArray(arrayBuilder.toByteString)
  }

  def apply(data : Array[Any]) : BSONArray = {
    val bldr = BSONBuilder()
    bldr.array(data)
    BSONArray(bldr.toByteString)
  }

  def apply[T, B <: BSONValue](data : Iterable[T])(implicit codec : BSONCodec[T, B]) : BSONArray = {
    val bldr = BSONBuilder()
    bldr.codecArray(data)
    BSONArray(bldr.buffer.result())
  }

  def apply[T, B <: BSONValue](data1 : T, data : T*)(implicit codec : BSONCodec[T, B]) : BSONArray = {
    val bldr = BSONBuilder()
    val values = Seq(data1) ++ data
    values.zipWithIndex.foreach {
      case (v, i) ⇒ bldr.append(i.toString, codec.write(v))
    }
    bldr.toByteString
    BSONArray(bldr.toByteString)
  }
}
