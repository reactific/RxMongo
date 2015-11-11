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

import java.nio.charset.StandardCharsets
import java.util.Date
import java.util.regex.Pattern

import akka.util.{ ByteStringBuilder, ByteIterator, ByteString }
import rxmongo.bson.BinarySubtype.UserDefinedBinary

trait ByteStringBuilderPimps extends ByteStringBuilderWrapper {

  @inline private[rxmongo] def putCStr(s : String) : ByteStringBuilder = {
    val bytes = s.getBytes(StandardCharsets.UTF_8)
    bldr.putBytes(bytes)
    bldr.putByte(0)
  }

  @inline private[rxmongo] def putStr(value : String) : ByteStringBuilder = {
    val bytes = value.getBytes(StandardCharsets.UTF_8)
    bldr.putInt(bytes.length + 1)
    bldr.putBytes(bytes)
    bldr.putByte(0)
  }

  @inline private[rxmongo] def putPrefix(code : TypeCode, key : String) : ByteStringBuilder = {
    bldr.
      putByte(code.code).
      putCStr(key)
  }

  @inline private[rxmongo] def putPrefix(code : Byte, key : String) : ByteStringBuilder = {
    bldr.
      putByte(code).
      putCStr(key)
  }

  @inline private[rxmongo] def putDoc(value : BSONDocument) : ByteStringBuilder = {
    val buffer = value.toByteString
    require(bldr.length + buffer.length <= maxDocSize, s"Document size exceeds maximum of $maxDocSize")
    bldr ++= buffer
    bldr
  }

  @inline private[rxmongo] def putDoc(value : Option[BSONDocument]) : ByteStringBuilder = {
    value match {
      case Some(doc) ⇒ putDoc(doc)
      case None ⇒ bldr
    }
  }

  @inline private[rxmongo] def putDoc(value : BSONBuilder) : ByteStringBuilder = {
    bldr ++= value.wrapAndTerminate
    bldr
  }

  private[rxmongo] def putDocs(values : Iterable[BSONDocument]) : ByteStringBuilder = {
    for (doc ← values) { putDoc(doc) }
    bldr
  }

  @inline private[rxmongo] def putObject(value : BSONObject) : ByteStringBuilder = putDoc(value.doc)
  @inline private[rxmongo] def putObject(value : Option[BSONObject]) : ByteStringBuilder = putDoc(value.map { v ⇒ v.doc })
  @inline private[rxmongo] def putObjects(values : Iterable[BSONObject]) : ByteStringBuilder = putDocs(values.map { v ⇒ v.doc })

  private[rxmongo] def putObject[T](fields : Iterable[(String, T)])(implicit codec : Codec[T]) = {
    val docBuilder = ByteString.newBuilder
    for ((key : String, t : T @unchecked) ← fields) {
      docBuilder.field(key, t)
    }
    bldr ++= docBuilder.wrapAndTerminate
  }

  private[rxmongo] def putAnyObject(fields : Iterable[(String, Any)]) : ByteStringBuilder = {
    val docBuilder = ByteString.newBuilder
    for ((key : String, any : Any) ← fields) {
      docBuilder.anyField(key, any)
    }
    bldr ++= docBuilder.wrapAndTerminate
  }

  @inline private[rxmongo] def putArray(value : BSONArray) : ByteStringBuilder = putDoc(value.doc)
  @inline private[rxmongo] def putArray(value : Option[BSONArray]) : ByteStringBuilder = putDoc(value.map { v ⇒ v.doc })
  @inline private[rxmongo] def putArrays(values : Iterable[BSONArray]) : ByteStringBuilder = putDocs(values.map { v ⇒ v.doc })

  private[rxmongo] def putArray[T](elems : Iterable[T])(implicit codec : Codec[T]) : ByteStringBuilder = {
    val arrayBuilder = ByteString.newBuilder
    elems.zipWithIndex.foreach { (x : (T, Int)) ⇒ arrayBuilder.field(x._2.toString, x._1) }
    bldr ++= arrayBuilder.wrapAndTerminate
  }

  private[rxmongo] def putAnyArray(elems : Iterable[Any]) : ByteStringBuilder = {
    val arrayBuilder = ByteString.newBuilder
    elems.zipWithIndex.foreach {
      case (value, index) ⇒
        arrayBuilder.anyField(index.toString, value)
    }
    bldr ++= arrayBuilder.wrapAndTerminate
  }

  private[rxmongo] def putBinary(subtype : BinarySubtype, blob : Iterator[Byte]) : ByteStringBuilder = {
    bldr.
      putInt(blob.length).
      putByte(subtype.code)
    for (b ← blob) { bldr.putByte(b) }
    bldr
  }

  @inline private[rxmongo] def putBinary(subtype : BinarySubtype, blob : Array[Byte]) : ByteStringBuilder = {
    bldr.sizeHint(bldr.length + blob.length + 5)
    bldr.
      putInt(blob.length).
      putByte(subtype.code).
      putBytes(blob)
  }

  @inline private[rxmongo] def putBinary(subtype : BinarySubtype, blob : ByteString) : ByteStringBuilder = {
    bldr.sizeHint(bldr.length + blob.length + 5)
    bldr.
      putInt(blob.length).
      putByte(subtype.code).
      append(blob)
  }

  @inline private[rxmongo] def putObjectID(value : Array[Byte]) : ByteStringBuilder = {
    require(value.length == 12, "ObjectID must be exactly 12 bytes")
    bldr.putBytes(value)
  }

  @inline private[rxmongo] def putBoolean(value : Boolean) : ByteStringBuilder = {
    bldr.putByte(if (value) 1 else 0)
  }

  @inline private[rxmongo] def putRegex(pattern : String, options : String) : ByteStringBuilder = {
    putCStr(pattern)
    putCStr(options)
  }

  private[rxmongo] def putRegex(p : Pattern) : ByteStringBuilder = {
    val options : String = {
      //
      // 'i' for case insensitive matching,
      // 'l' to make \w, \W, etc. locale dependent,
      // 'm' for multiline matching,
      // 's' for dotall mode ('.' matches everything),
      // and 'u' to make \w, \W, etc. match unicode.
      // 'x' for verbose mode,
      val flags = p.flags()
      var str : String = ""
      if ((flags & Pattern.CASE_INSENSITIVE) != 0)
        str = str + "i"
      if ((flags & Pattern.MULTILINE) != 0)
        str = str + "m"
      if ((flags & Pattern.DOTALL) != 0)
        str = str + "s"
      if ((flags & Pattern.UNICODE_CHARACTER_CLASS) != 0)
        str = str + "u"
      if ((flags & Pattern.COMMENTS) != 0)
        str = str + "x"
      str
    }
    val pattern : String = {
      val s = p.pattern()
      val r = "^\\(\\?[idmsuxU]+\\)"
      s.replaceFirst(r, "")
    }
    putRegex(pattern, options)
  }

  private[rxmongo] def putDBPointer(referent : String, id : Array[Byte]) : ByteStringBuilder = {
    require(id.length == 12, "ObjectID must be exactly 12 bytes")
    bldr.
      putStr(referent).
      putBytes(id)
  }

  private[rxmongo] def putScopedJavaScript(code : String, scope : BSONObject) : ByteStringBuilder = {
    val content = ByteString.newBuilder
    content.
      putStr(code).
      putObject(scope)
    val tmp = content.result()
    bldr.putInt(tmp.length + 4) // add four for the length field itself
    bldr ++= tmp
  }

  private[rxmongo] def put[T](value : T)(implicit codec : Codec[T]) : ByteStringBuilder = {
    codec.write(value, bldr)
    bldr
  }

  @inline def double(key : String, value : Double) : ByteStringBuilder = {
    putPrefix(DoubleCode, key)
    bldr.putDouble(value)
  }

  @inline def string(key : String, value : String) : ByteStringBuilder = {
    putPrefix(StringCode, key)
    putStr(value)
  }

  @inline def obj(key : String, value : BSONObject) : ByteStringBuilder = {
    putPrefix(ObjectCode, key)
    putObject(value)
  }

  @inline def obj(key : String, value : BSONBuilder) : ByteStringBuilder = {
    putPrefix(ObjectCode, key)
    putDoc(value)
  }

  @inline def obj[T](key : String, fieldKey : String, fieldValue : T)(implicit codec : Codec[T]) : ByteStringBuilder = {
    obj(key, BSONBuilder().field(fieldKey, fieldValue))
  }

  @inline def obj[T](key : String, value : T)(implicit codec : Codec[T]) : ByteStringBuilder = {
    putPrefix(ObjectCode, key)
    bldr ++= codec.write(value)
  }

  @inline def obj[T](key : String, fields : Map[String, T])(implicit codec : Codec[T]) : ByteStringBuilder = {
    putPrefix(ObjectCode, key)
    putObject[T](fields)
  }

  @inline def obj(key : String, value : ByteString) : ByteStringBuilder = {
    putPrefix(ObjectCode, key)
    bldr ++= value
  }

  @inline def anyObj(key : String, value : Iterable[(String, Any)]) : ByteStringBuilder = {
    putPrefix(ObjectCode, key)
    putAnyObject(value)
  }

  @inline def anyObj(key : String, field1 : (String, Any), fields : (String, Any)*) : ByteStringBuilder = {
    anyObj(key, field1 +: fields.toSeq)
  }

  @inline def anyObj(key : String, fields : Map[String, Any]) : ByteStringBuilder = {
    bldr.anyObj(key, fields.toSeq)
  }

  @inline def array(key : String, value : BSONArray) : ByteStringBuilder = {
    putPrefix(ArrayCode, key)
    putArray(value)
  }

  @inline def array[T](key : String, values : Iterable[T])(implicit codec : Codec[T]) : ByteStringBuilder = {
    putPrefix(ArrayCode, key)
    val arrayBuilder = BSONBuilder()
    values.zipWithIndex.foreach {
      case (v, i) ⇒
        arrayBuilder.bldr.putPrefix(codec.code, i.toString)
        codec.write(v, arrayBuilder)
    }
    bldr ++= arrayBuilder.wrapAndTerminate
  }

  @inline def anyArray(key : String, values : Iterable[Any]) : ByteStringBuilder = {
    putPrefix(ArrayCode, key)
    putAnyArray(values)
  }

  @inline def array(key : String, v1 : Any, values : Any*) : ByteStringBuilder = {
    putPrefix(ArrayCode, key)
    putAnyArray(v1 +: values)
  }

  @inline def binary(key : String, blob : Array[Byte], subtype : BinarySubtype) : ByteStringBuilder = {
    putPrefix(BinaryCode, key)
    putBinary(subtype, blob)
  }

  @inline def binary(key : String, blob : Iterator[Byte], subtype : BinarySubtype) : ByteStringBuilder = {
    putPrefix(BinaryCode, key)
    putBinary(subtype, blob)
  }

  @inline def binary(key : String, blob : ByteString, subtype : BinarySubtype) : ByteStringBuilder = {
    putPrefix(BinaryCode, key)
    putBinary(subtype, blob)
  }

  @inline def undefined(key : String) : ByteStringBuilder = {
    putPrefix(UndefinedCode, key)
  }

  @inline def objectID(key : String, value : Array[Byte]) : ByteStringBuilder = {
    putPrefix(ObjectIDCode, key)
    putObjectID(value)
  }

  @inline def boolean(key : String, value : Boolean) : ByteStringBuilder = {
    putPrefix(BooleanCode, key)
    bldr.putByte(if (value) 1.toByte else 0.toByte)
  }

  @inline def date(key : String, time : Date) : ByteStringBuilder = {
    putPrefix(DateCode, key)
    bldr.putLong(time.getTime)
  }

  @inline def date(key : String, time : Long) : ByteStringBuilder = {
    putPrefix(DateCode, key)
    bldr.putLong(time)
  }

  @inline def nil(key : String) : ByteStringBuilder = {
    putPrefix(NullCode, key)
  }

  @inline def regex(key : String, pattern : String, options : String = "") : ByteStringBuilder = {
    putPrefix(RegexCode, key)
    bldr.putRegex(pattern, options)
  }

  @inline def regex(key : String, regex : Pattern) : ByteStringBuilder = {
    putPrefix(RegexCode, key)
    putRegex(regex)
  }

  @inline def dbPointer(key : String, referent : String, id : Array[Byte]) : ByteStringBuilder = {
    putPrefix(DBPointerCode, key)
    putDBPointer(referent, id)
  }

  @inline def jsCode(key : String, code : String) : ByteStringBuilder = {
    putPrefix(JavaScriptCode, key)
    putStr(code)
  }

  @inline def symbol(key : String, symbol : String) : ByteStringBuilder = {
    putPrefix(SymbolCode, key)
    putStr(symbol)
  }

  @inline def scopedJsCode(key : String, code : String, scope : BSONObject) : ByteStringBuilder = {
    putPrefix(ScopedJSCode, key)
    putScopedJavaScript(code, scope)
  }

  @inline def integer(key : String, value : Int) : ByteStringBuilder = {
    putPrefix(IntegerCode, key)
    bldr.putInt(value)
  }

  @inline def timestamp(key : String, value : Long) : ByteStringBuilder = {
    putPrefix(TimestampCode, key)
    bldr.putLong(value)
  }

  @inline def long(key : String, value : Long) : ByteStringBuilder = {
    putPrefix(LongCode, key)
    bldr.putLong(value)
  }

  def field[T](key : String, value : T)(implicit codec : Codec[T]) : ByteStringBuilder = {
    putPrefix(codec.code, key)
    codec.write(value, bldr)
    bldr
  }

  def addFieldsOf[T](value : T)(implicit codec : Codec[T]) : ByteStringBuilder = {
    val doc = BSONDocument(codec.write(value))
    for ((key, (code, itr)) ← doc) {
      putPrefix(code, key)
      bldr ++= itr
    }
    bldr
  }

  def anyField(key : String, anyVal : Any) : ByteStringBuilder = {
    anyVal match {
      case null ⇒
        putPrefix(NullCode, key)
      case BSONNull ⇒
        putPrefix(NullCode, key)
      case BSONUndefined ⇒
        putPrefix(UndefinedCode, key)
      case v : BSONValue ⇒
        putPrefix(v.code, key)
        bldr ++= v.toByteString
      case i : Int ⇒
        integer(key, i)
      case l : Long ⇒
        long(key, l)
      case d : Double ⇒
        double(key, d)
      case f : Float ⇒
        double(key, f)
      case b : Boolean ⇒
        boolean(key, b)
      case s : Short ⇒
        integer(key, s)
      case s : String ⇒
        string(key, s)
      case d : Date ⇒
        date(key, d.getTime)
      case b : ByteString ⇒
        binary(key, b, UserDefinedBinary)
      case p : Pattern ⇒
        regex(key, p)
      case (bs : BinarySubtype, ar : Array[Byte]) ⇒
        binary(key, ar, bs)
      case (r : String, ar : Array[Byte]) ⇒
        dbPointer(key, r, ar)
      case (c : String, s : BSONObject) ⇒
        scopedJsCode(key, c, s)
      case m : Map[String, Any] @unchecked ⇒
        anyObj(key, m)
      case a : Array[Byte] ⇒
        binary(key, a, UserDefinedBinary)
      case i : Iterable[Any] ⇒
        putPrefix(ArrayCode, key)
        putAnyArray(i.toSeq)
      case (b : Byte, bi : ByteIterator) ⇒
        putPrefix(b, key)
        bldr ++= bi.clone()
      case x : Any ⇒
        throw RxMongoError(s"Unable to convert $x into a BSONValue")
    }
    bldr
  }

  def put(value : BSONConstructor) : ByteStringBuilder = {
    value.addTo(bldr)
    bldr
  }

  def arrayObj[T](key : String, arrayKey : String, values : Iterable[T])(implicit codec : Codec[T]) : ByteStringBuilder = {
    bldr.putPrefix(ObjectCode, key)
    val b = BSONBuilder().array(arrayKey, values)
    bldr ++= b.wrapAndTerminate
  }

  def field(key : String, value : BSONValue) : ByteStringBuilder = {
    putPrefix(value.code, key)
    bldr ++= value.toByteString
  }

}
