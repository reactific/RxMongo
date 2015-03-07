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

package rxmongo

import java.nio.ByteOrder
import java.nio.charset.{ StandardCharsets, Charset }
import java.util.Date
import java.util.regex.Pattern

import akka.util.{ByteString, ByteIterator, ByteStringBuilder}

import scala.annotation.switch
import scala.language.implicitConversions

/** The bson package object.
  *
  * This just contains values that are used throughout the bson package.
  */
package object bson {

  case class RxMongoError(message : String, cause : Option[Throwable] = None) extends Exception {
    override def getMessage = message
    override def getCause = cause.orNull
  }

  // Everything in Mongo is Little Endian
  implicit val byteOrder = ByteOrder.LITTLE_ENDIAN

  val utf8 = Charset.forName("UTF-8")

  /** Maximum Document Size.
    *
    * Use Mongo's maximum doc size to ensure that we're having sane reading of length fields and we don't OOM
    * by trying to allocate all memory.
    * @see [[http://docs.mongodb.org/manual/reference/command/isMaster/#dbcmd.isMaster]]
    *
    */
  final val maxDocSize = 16 * 1024 * 1024

  /** Implicit Extensions to ByteStringBuilder
    *
    * This makes using a ByteStringBuilder with BSON easier by providing functions that build BSON data structures.
    *
    * @param bldr The builder we are extending
    */
  implicit class ByteStringBuilderPimps(bldr : ByteStringBuilder) {

    @inline private[rxmongo] def putCStr(s : String) : ByteStringBuilder = {
      val bytes = s.getBytes(StandardCharsets.UTF_8)
      bldr.putBytes(bytes)
      bldr.putByte(0)
    }

    @inline private[rxmongo] def putStr(value : String) : ByteStringBuilder = {
      val bytes = value.getBytes(utf8)
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
      bldr ++= value.result.buffer
      bldr
    }

    private[rxmongo] def putDocs(values : Iterable[BSONDocument]) : ByteStringBuilder = {
      for (doc ← values) { putDoc(doc) }
      bldr
    }

    @inline private[rxmongo] def putObject(value : BSONObject) : ByteStringBuilder = putDoc(value.doc)
    @inline private[rxmongo] def putObject(value : Option[BSONObject]) : ByteStringBuilder = putDoc(value.map { v ⇒ v.doc })
    @inline private[rxmongo] def putObjects(values : Iterable[BSONObject]) : ByteStringBuilder = putDocs(values.map { v ⇒ v.doc })

    @inline private[rxmongo] def putArray(value : BSONArray) : ByteStringBuilder = putDoc(value.doc)
    @inline private[rxmongo] def putArray(value : Option[BSONArray]) : ByteStringBuilder = putDoc(value.map { v ⇒ v.doc })
    @inline private[rxmongo] def putArrays(values : Iterable[BSONArray]) : ByteStringBuilder = putDocs(values.map { v ⇒ v.doc })

    @inline private[rxmongo] def putRegex(pattern : String, options : String) : ByteStringBuilder = {
      putCStr(pattern)
      putCStr(options)
    }

    private [rxmongo] def putRegex(p : Pattern) : ByteStringBuilder = {
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

    private[rxmongo] def scopedJsCode(code : String, scope : BSONObject) : ByteStringBuilder = {
      val content = ByteString.newBuilder
      content.
        putStr(code).
        putObject(scope)
      val tmp = content.result()
      bldr.putInt(tmp.length + 4) // add four for the length field itself
      bldr ++= tmp
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

    @inline def array(key : String, value : BSONArray) : ByteStringBuilder = {
      putPrefix(ArrayCode, key)
      putArray(value)
    }

    @inline def binary(key : String, blob : Array[Byte], subtype : BinarySubtype) : ByteStringBuilder = {
      putPrefix(BinaryCode, key)
      bldr.
        putInt(blob.length).
        putByte(subtype.code).
        putBytes(blob)
    }

    def binary(key : String, blob : Iterator[Byte], subtype : BinarySubtype) : ByteStringBuilder = {
      putPrefix(BinaryCode, key)
      bldr.sizeHint(bldr.length + blob.length + 5)
      bldr.
        putInt(blob.length).
        putByte(subtype.code)
      for (b ← blob) { bldr.putByte(b) }
      bldr
    }

    @inline def binary(key : String, blob : ByteString, subtype : BinarySubtype) : ByteStringBuilder = {
      putPrefix(BinaryCode, key)
      bldr.
        putInt(blob.length).
        putByte(subtype.code).
        append(blob)
    }

    @inline def undefined(key : String) : ByteStringBuilder = {
      putPrefix(UndefinedCode, key)
    }

    @inline def objectID(key : String, value : Array[Byte]) : ByteStringBuilder = {
      require(value.length == 12, "ObjectID must be exactly 12 bytes")
      putPrefix(ObjectIDCode, key)
      bldr.putBytes(value)
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
      bldr.putRegex(regex)
    }

    @inline def dbPointer(key : String, referent : String, id : Array[Byte]) : ByteStringBuilder = {
      require(id.length == 12, "ObjectID must be exactly 12 bytes")
      putPrefix(DBPointerCode, key)
      bldr.
        putStr(referent).
        putBytes(id)
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
      bldr.scopedJsCode(code, scope)
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

    def toByteString : ByteString = {
      val content = bldr.result()
      val bsb = ByteString.newBuilder
      bsb.putInt(content.length + 5) // 4 for length, 1 for terminating 0 byte
      bsb ++= content
      bsb.putByte(0)
      bsb.result()
    }
  }

  /** Implicit Extensions To ByteIterator
    *
    * This makes it easier to use a ByteIterator with MongoDB. The extensions implement recognition of various
    * data types that BSON encoding requires.
    * @param itr The wrapped iterator
    */
  implicit class ByteIteratorPimps(itr : ByteIterator) {

    def getCStr : String = {
      val s = itr.clone().takeWhile { p ⇒ p != 0 }
      itr.drop(s.len + 1)
      val buf = Array.ofDim[Byte](s.len)
      s.copyToArray(buf)
      new String(buf, utf8)
    }

    def getStr : String = {
      val len = itr.getInt - 1
      require(len < maxDocSize, s"Maximum string size is $maxDocSize bytes")
      val buf = Array.ofDim[Byte](len)
      itr.getBytes(buf)
      require(itr.getByte == 0.toByte, "Failed to read terminating null in String")
      new String(buf, utf8)
    }

    def getBytes(len : Int) : Array[Byte] = {
      val res = Array.ofDim[Byte](len)
      itr.getBytes(res)
      res
    }

    def getDoc : BSONDocument = {
      val save = itr.clone()
      val len = itr.getInt
      require(len < maxDocSize, s"Maximum object size is $maxDocSize bytes")
      itr.drop(len - 4)
      val bitr = save.slice(0, len)
      BSONDocument(bitr)
    }

    def getObject : BSONObject = BSONObject(getDoc)

    def getArray : BSONArray = BSONArray(getDoc)

    def getObjects(count : Int) : Seq[BSONObject] = {
      for (x ← 1 to count) yield { getObject }
    }

    def getBinary : (BinarySubtype, Array[Byte]) = {
      val len = itr.getInt
      val st = BinarySubtype(itr.getByte)
      val value = itr.getBytes(len)
      st → value
    }

    def getRegex : Pattern = {
      val pattern = itr.getCStr
      val options = itr.getCStr
      val flags = options.foldLeft(0) {
        case (flg, ch) ⇒
          (ch : @switch) match {
            case 'i' ⇒ flg | Pattern.CASE_INSENSITIVE
            case 'm' ⇒ flg | Pattern.MULTILINE
            case 's' ⇒ flg | Pattern.DOTALL
            case 'u' ⇒ flg | Pattern.UNICODE_CHARACTER_CLASS | Pattern.UNICODE_CASE
            case 'x' ⇒ flg | Pattern.COMMENTS
            case _   ⇒ flg
          }
      }
      Pattern.compile(pattern, flags)
    }

    def getDBPointer : (String, Array[Byte]) = {
      itr.getStr → itr.getBytes(12)
    }

    def getScopedJavaScript : (String, BSONObject) = {
      itr.getInt
      itr.getStr → itr.getObject
    }

    @inline def skipLength : Int = {
      val len = itr.getInt
      itr.drop(len)
      len + 4
    }

    @inline def skipCStr : Int = {
      var count = 0
      itr.dropWhile { ch ⇒ count = count + 1; ch != 0 }
      itr.drop(1)
      count
    }

    @inline def skipDocument : Int = {
      val len = itr.getInt
      itr.drop(len - 4)
      len
    }

    @inline def skipLong : Int = { itr.drop(8); 8 }
    @inline def skipDouble : Int = skipLong
    @inline def skipInt : Int = { itr.drop(4); 4 }
    @inline def skipObjId : Int = { itr.drop(12); 12 }
    @inline def skipByte : Int = { itr.drop(1); 1 }

  }

}
