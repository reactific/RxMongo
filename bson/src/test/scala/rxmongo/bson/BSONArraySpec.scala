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

import java.time.Instant

import akka.util.ByteString
import org.specs2.mutable.Specification

/** Title Of Thing.
  *
  * Description of thing
  */
class BSONArraySpec extends Specification with ByteStringTestUtils {

  "BSONArray" should {
    "yield single empty object for empty construction" in {
      val anArray = BSONArray()
      anArray must beEqualTo(BSONArray.empty)
      val otherArray = BSONArray()
      otherArray must beEqualTo(BSONArray.empty)
      otherArray must beEqualTo(anArray)
    }
    "construct from an Array[Any]" in {
      val anArray = BSONArray.fromAny(Array(1,2.toShort, 3L, true, 5.0, "six", null, Instant.now()))
      anArray.length must beEqualTo(8)
      anArray(0).code must beEqualTo(IntegerCode)
      anArray(1).code must beEqualTo(IntegerCode)
      anArray(2).code must beEqualTo(LongCode)
      anArray(3).code must beEqualTo(BooleanCode)
      anArray(4).code must beEqualTo(DoubleCode)
      anArray(5).code must beEqualTo(StringCode)
      anArray(6).code must beEqualTo(NullCode)
      anArray(7).code must beEqualTo(DateCode)
    }

    "construct from an Iterator[Any]" in {
      val values = Seq[Any](1,2.toShort, 3L, true, 5.0, "six", null, Instant.now())
      val anArray = BSONArray.fromAny(values)
      anArray.length must beEqualTo(8)
      anArray(0).code must beEqualTo(IntegerCode)
      anArray(1).code must beEqualTo(IntegerCode)
      anArray(2).code must beEqualTo(LongCode)
      anArray(3).code must beEqualTo(BooleanCode)
      anArray(4).code must beEqualTo(DoubleCode)
      anArray(5).code must beEqualTo(StringCode)
      anArray(6).code must beEqualTo(NullCode)
      anArray(7).code must beEqualTo(DateCode)
    }

    "constructs from Seq[BSONValue]" in {
      val values = Seq[BSONValue](BSONInteger(1), BSONLong(2), BSONString("three"), BSONDouble(4), BSONBoolean(true))
      val anArray = BSONArray(values.iterator)
      anArray.length must beEqualTo(5)
      anArray(0).value must beEqualTo(1)
      anArray(1).value must beEqualTo(2L)
      anArray(2).value must beEqualTo("three")
      anArray(3).value must beEqualTo(4.0)
      anArray(4).value must beEqualTo(true)
    }

    "constructs from a list of BSONObject" in {
      val one = makeAnObject().toBSONObject
      val two = makeAnObject().toBSONObject
      val three = makeAnObject().toBSONObject
      val anArray = BSONArray(one, two, three)
      anArray.length must beEqualTo(3)
      anArray(0).value must beEqualTo(one)
      anArray(1).value must beEqualTo(two)
      anArray(2).value must beEqualTo(three)
    }

    "constructs from homogeneous data" in {
      val anArray = BSONArray(1,2,3,4,5)
      anArray.length must beEqualTo(5)
      anArray(0).value must beEqualTo(1)
      anArray(1).value must beEqualTo(2)
      anArray(2).value must beEqualTo(3)
      anArray(3).value must beEqualTo(4)
      anArray(4).value must beEqualTo(5)
    }

    "converts to an Array[BSONValue]" in {
      val a1 = BSONArray.fromAny(Array(1,2.toShort, 3L, true, 5.0, "six", null))
      val array = a1.asArray
      array.length must beEqualTo(7)
      array(0).value must beEqualTo(1)
      array(1).value must beEqualTo(2)
      array(2).value must beEqualTo(3L)
      array(3).value must beEqualTo(true)
      array(4).value must beEqualTo(5.0)
      array(5).value must beEqualTo("six")
      array(6).value must beEqualTo(())
    }

    "converts to a Seq" in {
      val seq = Seq(1,2)
      val anArray = BSONArray(seq)
      val seq2 = anArray.value.map { bv => bv.asInstanceOf[BSONInteger].value }
      seq.equals(seq2) must beTrue
    }

    "converts to a ByteString" in {
      val seq = Seq(1,2)
      val anArray = BSONArray(seq)
      val bs = anArray.toByteString
      val expected = {
        val bldr = ByteString.newBuilder
        bldr.putByte(16)
        bldr.putCStr("0")
        bldr.putInt(1)
        bldr.putByte(16)
        bldr.putCStr("1")
        bldr.putInt(2)
        bldr.wrapAndTerminate
      }
      bs must beEqualTo(expected)
    }

    "converts to a String" in {
      val a1 = BSONArray.fromAny(Array(1,2.toShort, 3L, true, 5.0, "six", null))
      val str = a1.toString()
      str must beEqualTo("[ 1, 2, 3, true, 5.0, six, () ]")
    }

    "equals works" in {
      val a1 = BSONArray.fromAny(Array(1,2.toShort, 3L, true, 5.0, "six", null, Instant.now()))
      val a2 = BSONArray.fromAny(Array(1,2.toShort, 3L, true, 5.0, "six", null, Instant.now()))
      a1.equals(a2) must beTrue
    }

    "not be equal to non BSONArray types" in {
      val anArray = BSONArray.fromAny(Array(1,2.toShort, 3L, true, 5.0, "six", null, Instant.now()))
      anArray.equals("nothing") must beFalse
    }
    "throw on invalid index" in {
      val anArray = BSONArray.fromAny(Array(1))
      anArray(0).isInstanceOf[BSONValue] must beTrue
      anArray(1) must throwA[NoSuchElementException]
    }
    "allow iteration with a Codec" in {
      val data = Seq[Long](1,2,3)
      val anArray = BSONArray(data)
      val longs = for (i <- anArray.as[Long]) yield { i }
      longs.toSeq must beEqualTo(data)
    }
    "throw on iterating a heterogeneous array with as[T]" in {
      val data = Seq(1, 2, "3")
      val anArray = BSONArray.fromAny(data)
      val iter = anArray.as[Int]
      iter.next must beEqualTo(1)
      iter.next must beEqualTo(2)
      iter.next must throwA[IllegalArgumentException]
    }
  }
}

/*

  override def toByteString : ByteString = doc.toByteString

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

  def as[T](implicit codec : Codec[T]) : Iterator[T] = {
    iterator.map {
      case v : BSONValue if v.code == codec.code ⇒ codec.read(v)
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

  def apply(objs : BSONObject*) : BSONArray = {
    val bldr = ByteString.newBuilder
    bldr.putAnyArray(objs)
    BSONArray(bldr.result())
  }

  def apply[T](data : Iterable[T])(implicit codec : Codec[T]) : BSONArray = {
    val bldr = ByteString.newBuilder
    bldr.putArray(data)
    BSONArray(bldr.result())
  }

  def apply[T](data1 : T, data : T*)(implicit codec : Codec[T]) : BSONArray = {
    val bldr = ByteString.newBuilder
    val values = data1 +: data
    bldr.putArray(values)
    BSONArray(bldr.result())
  }

  def fromAny(data : Array[Any]) : BSONArray = {
    val bldr = ByteString.newBuilder
    bldr.putAnyArray(data)
    BSONArray(bldr.result())
  }

  def fromAny(data : Iterable[Any]) : BSONArray = {
    val bldr = ByteString.newBuilder
    bldr.putAnyArray(data)
    BSONArray(bldr.result())
  }
}

 */
