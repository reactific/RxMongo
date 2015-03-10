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

import java.util
import java.util.Date
import java.util.regex.Pattern

import akka.util.{ ByteStringBuilder, ByteIterator, ByteString }
import rxmongo.bson.Codec._

import scala.annotation.switch
import scala.collection._

case class BSONDocument private[rxmongo] (
  private[bson] val data : Map[String, (Byte, ByteIterator)],
  docItr : Option[ByteIterator] = None) extends MapLike[String, (Byte, ByteIterator), BSONDocument] with Map[String, (Byte, ByteIterator)] {

  def get(key : String) : Option[(Byte, ByteIterator)] = data.get(key)

  def as[T](key : String, code : TypeCode, conv : (ByteIterator) ⇒ T) : T = {
    data.get(key) match {
      case Some((c, itr)) if c == code.code ⇒ conv(itr)
      case Some((c, itr)) ⇒ throw new IllegalArgumentException(s"Field '$key' has type ${TypeCode(c).typeName} not ${code.typeName}")
      case None ⇒ throw new NoSuchElementException(s"Field '$key' does not exist.")
    }
  }

  def asOption[T](key : String, code : TypeCode, conv : (ByteIterator) ⇒ T) : Option[T] = {
    data.get(key) map {
      case (cd, bi) ⇒
        if (cd != code.code)
          throw new IllegalArgumentException(s"Field '$key' has type ${TypeCode(cd).typeName} not ${code.typeName}")
        else
          conv(bi)
    }
  }

  def asOptionSeq[T](key : String, code : TypeCode, conv : (ByteIterator) ⇒ T) : Option[Seq[T]] = {
    data.get(key) map {
      case (cd, bi) ⇒
        if (cd != ArrayCode.code)
          throw new IllegalArgumentException(s"Field '$key' has type ${TypeCode(cd).typeName} not ${ArrayCode.typeName}")
        else {
          val doc = BSONDocument(bi)
          for ((k, (c, b)) ← BSONDocument(bi)) yield {
            if (c != code.code)
              throw new IllegalArgumentException(s"Field '$k' has type ${TypeCode(c).typeName} not ${code.typeName}")
            else
              conv(b)
          }
        }.toSeq
    }
  }

  def asDouble(key : String) : Double = as[Double](key, DoubleCode, { itr ⇒ itr.clone().getDouble })
  def asString(key : String) : String = as[String](key, StringCode, { itr ⇒ itr.clone().getStr })
  def asObject(key : String) : BSONObject = as[BSONObject](key, ObjectCode, { itr ⇒ itr.clone().getObject })
  def asArray(key : String) : BSONArray = as[BSONArray](key, ArrayCode, { itr ⇒ itr.clone().getArray })
  def asBinary(key : String) : (BinarySubtype, Array[Byte]) =
    as[(BinarySubtype, Array[Byte])](key, BinaryCode, { itr ⇒ itr.clone().getBinary })
  def asUndefined(key : String) : Unit = as[Unit](key, UndefinedCode, { itr ⇒ require(itr.isEmpty) })
  def asObjectID(key : String) : Array[Byte] = as[Array[Byte]](key, ObjectIDCode, { itr ⇒ itr.clone().getObjectID })
  def asBoolean(key : String) : Boolean = as[Boolean](key, BooleanCode, { itr ⇒ itr.clone().getByte != 0 })
  def asDate(key : String) : Date = as[Date](key, DateCode, { itr ⇒ new Date(itr.clone().getLong) })
  def asNull(key : String) : Unit = as[Unit](key, NullCode, { itr ⇒ require(itr.clone().isEmpty) })
  def asRegex(key : String) : Pattern = as[Pattern](key, RegexCode, { itr ⇒ itr.clone().getRegex })
  def asDBPointer(key : String) : (String, Array[Byte]) =
    as[(String, Array[Byte])](key, DBPointerCode, { itr ⇒ itr.clone().getDBPointer })
  def asJavaScript(key : String) : String = as[String](key, JavaScriptCode, { itr ⇒ itr.clone().getStr })
  def asSymbol(key : String) : String = as[String](key, SymbolCode, { itr ⇒ itr.clone().getStr })
  def asScopedJavaScript(key : String) : (String, BSONObject) =
    as[(String, BSONObject)](key, ScopedJSCode, { itr ⇒ itr.clone().getScopedJavaScript })
  def asInt(key : String) : Int = as[Int](key, IntegerCode, { itr ⇒ itr.clone().getInt })
  def asTimestamp(key : String) : Long = as[Long](key, TimestampCode, { itr ⇒ itr.clone().getLong })
  def asLong(key : String) : Long = as[Long](key, LongCode, { itr ⇒ itr.clone().getLong })

  def asObjectOfType[T](key : String)(implicit codec : Codec[T]) : T = {
    as[T](key, ObjectCode, { itr ⇒ codec.read(itr) })
  }

  def asArrayOfType[T](key : String)(implicit codec : Codec[T]) : T = {
    as[T](key, ArrayCode, { itr ⇒ codec.read(itr) })
  }

  def asMap[T](key : String)(implicit codec : Codec[T]) : immutable.Map[String, T] = {
    iterator.map {
      case (k, (bc, bi)) if bc == codec.code.code ⇒ k → codec.read(bi)
      case (k, (bc, bi)) ⇒
        throw new IllegalArgumentException(
          s"Expected type ${codec.typeName} for key '$k' but got type ${TypeCode(bc).typeName}")
    }.toMap
  }

  def asSeq[T](key : String)(implicit codec : Codec[T]) : Seq[T] = {
    asOptionSeq[T](key, codec.code, codec) match {
      case Some(x) ⇒ x
      case None    ⇒ Seq.empty[T]
    }
  }

  def asAnyMap : Map[String, Any] = {
    iterator.map {
      case (k, (bc, bi)) ⇒ k → BSONValue(bc, bi).value
    }.toMap
  }

  def asOptionalObject(key : String) : Option[BSONObject] = {
    asOption[BSONObject](key, ObjectCode, { bi ⇒ BSONObject(bi) })
  }

  def asOptionalObjectOfType[T](key : String)(implicit codec : Codec[T]) : Option[T] = {
    asOption[T](key, ObjectCode, codec)
  }

  def asOptionalArray[T](key : String)(implicit codec : Codec[T]) : Option[Seq[T]] = {
    asOptionSeq[T](key, ArrayCode, codec)
  }

  def asOptionalString(key : String) : Option[String] = { asOption[String](key, StringCode, StringCodec) }
  def asOptionalInt(key : String) : Option[Int] = { asOption[Int](key, IntegerCode, IntCodec) }
  def asOptionalDouble(key : String) : Option[Double] = { asOption[Double](key, DoubleCode, DoubleCodec) }
  def asOptionalDate(key : String) : Option[Date] = { asOption[Date](key, DateCode, DateCodec) }
  def asOptionalBoolean(key : String) : Option[Boolean] = { asOption[Boolean](key, BooleanCode, BooleanCodec) }

  def +[B1 >: (Byte, ByteIterator)](kv : (String, B1)) : BSONDocument = {
    val pair = kv._1 → kv._2.asInstanceOf[(Byte, ByteIterator)]
    BSONDocument(data.+(pair), None)
  }

  def iterator : Iterator[(String, (Byte, ByteIterator))] = data.iterator

  def -(key : String) : BSONDocument = {
    BSONDocument(data.-(key))
  }

  override def empty : BSONDocument = BSONDocument.empty

  override protected[this] def newBuilder : mutable.Builder[(String, (Byte, ByteIterator)), BSONDocument] = ???

  def addTo(b : BSONBuilder) : Unit = {
    for ((k, (typeCode, byteIterator)) ← data) {
      b.bldr.putPrefix(typeCode, k)
      b.bldr ++= byteIterator.clone()
    }
  }

  def addTo(b : ByteStringBuilder) : Unit = {
    for ((k, (typeCode, byteIterator)) ← data) {
      b.putPrefix(typeCode, k)
      b ++= byteIterator.clone()
    }
  }

  def toByteString : ByteString = {
    docItr match {
      case Some(itr) ⇒ itr.clone().toByteString
      case None ⇒
        val bldr = BSONBuilder()
        for ((k, (typeCode, byteIterator)) ← data) {
          bldr.bldr.putPrefix(typeCode, k)
          bldr.bldr ++= byteIterator.clone()
        }
        bldr.wrapAndTerminate
    }
  }

  def byteSize : Int = {
    docItr match {
      case Some(itr) ⇒ itr.length
      case None ⇒ iterator.foldLeft(0) { case (sum, (key, (bc, bi))) ⇒ sum + bi.len }
    }
  }

  override def equals(other : Any) : Boolean = other match {
    case that : BSONDocument ⇒
      (this eq that) ||
        (that canEqual this) &&
        (this.size == that.size) && {
          try {
            this forall {
              case (k, (tc1, bi1)) ⇒ that.get(k) match {
                case Some((tc2, bi2)) ⇒ tc1 == tc2 && util.Arrays.equals(bi1.clone().toArray, bi2.clone().toArray)
                case None ⇒ false
                case _ ⇒ false
              }
            }
          } catch {
            case ex : ClassCastException ⇒
              println("class cast "); false
          }
        }
    case _ ⇒
      false
  }
}

object BSONDocument {
  object empty extends BSONDocument(Map.empty[String, (Byte, ByteIterator)], None)

  def apply() : BSONDocument = empty

  def apply(fields : Map[String, BSONValue]) : BSONDocument = {
    val b = BSONBuilder()
    for ((k, v) ← fields) b.append(k, v)
    BSONDocument(b.wrapAndTerminate)
  }

  def apply(buffer : ByteString) : BSONDocument = BSONDocument(buffer.iterator)

  def apply(itr : ByteIterator) : BSONDocument = {
    val docItr = itr.clone()
    val byteLen = itr.getInt
    var code = itr.getByte
    var seq = Seq.empty[(String, (Byte, ByteIterator))]
    while (itr.hasNext && code != 0) {
      val key = itr.getCStr
      val save = itr.clone()
      val bi : ByteIterator = (code : @switch) match {
        case 1  ⇒ save.take(itr.skipDouble) // Double
        case 2  ⇒ save.take(itr.skipLength) // String
        case 3  ⇒ save.take(itr.skipDocument) // Object
        case 4  ⇒ save.take(itr.skipDocument) // Array
        case 5  ⇒ save.take(itr.skipLength + itr.skipByte) // Binary
        case 6  ⇒ save.take(0) // Undefined
        case 7  ⇒ save.take(itr.skipObjId) // ObjectID
        case 8  ⇒ save.take(itr.skipByte) // Boolean
        case 9  ⇒ save.take(itr.skipLong) // Date
        case 10 ⇒ save.take(0) // Null
        case 11 ⇒ save.take(itr.skipCStr + itr.skipCStr) // Regex
        case 12 ⇒ save.take(itr.skipLength + itr.skipObjId) // DBPointer
        case 13 ⇒ save.take(itr.skipLength) // JavaScript
        case 14 ⇒ save.take(itr.skipLength) // Symbol
        case 15 ⇒ save.take(itr.skipDocument) // Scoped JavaScript
        case 16 ⇒ save.take(itr.skipInt) // Integer
        case 17 ⇒ save.take(itr.skipLong) // Timestamp
        case 18 ⇒ save.take(itr.skipLong) // Long
        case _  ⇒ throw new NoSuchElementException("Unrecognized BSON Type Code")
      }
      seq = seq :+ (key -> (code → bi))
      code = itr.getByte
    }
    val map = seq.toMap
    new BSONDocument(map, Some(docItr))
  }
}
