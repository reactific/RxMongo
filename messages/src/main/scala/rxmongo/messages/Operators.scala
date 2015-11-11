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

package rxmongo.messages

import akka.util.ByteString
import rxmongo.bson._

import scala.util.matching.Regex

/** Expression */
class Expression extends BSONBuilder

class BooleanExpression() extends Expression
object BooleanExpression { def apply() : BooleanExpression = new BooleanExpression }

class BooleanFieldExpression(fieldName : String) extends BooleanExpression {

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/eq/]]
    * @param value
    * @return
    */
  def $eq[T](value : T)(implicit codec : Codec[T]) : BooleanExpression = { field[T](fieldName, value) }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/ne/]]
    * @param value
    * @return
    */
  def $ne[T](value : T)(implicit codec : Codec[T]) : BooleanExpression = { obj(fieldName, "$ne", value) }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/gt/]]
    * @param value
    * @return
    */
  def $gt[T](value : T)(implicit codec : Codec[T]) : BooleanExpression = { obj(fieldName, "$gt", value) }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/lt/]]
    * @param value
    * @return
    */
  def $lt[T](value : T)(implicit codec : Codec[T]) : BooleanExpression = { obj(fieldName, "$lt", value) }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/gte/]]
    * @param value
    * @return
    */
  def $gte[T](value : T)(implicit codec : Codec[T]) : BooleanExpression = { obj(fieldName, "$gte", value) }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/lte/]]
    * @param value
    * @return
    */
  def $lte[T](value : T)(implicit codec : Codec[T]) : BooleanExpression = { obj(fieldName, "$lte", value) }

  def $lte[T](values : Iterable[T])(implicit codec : Codec[T]) : BooleanExpression = {
    arrayObj[T](fieldName, "$lte", values)
  }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/in/]]
    * @param values
    * @return
    */
  def $in[T](values : T*)(implicit codec : Codec[T]) : BooleanExpression = {
    arrayObj[T](fieldName, "$in", values)
  }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/nin/]]
    * @param values
    * @return
    */
  def $nin[T](values : T*)(implicit codec : Codec[T]) : BooleanExpression = {
    arrayObj[T](fieldName, "$nin", values)
  }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/not/]]
    * @param values
    * @return
    */
  def $not(values : BooleanExpression) : BooleanExpression = { obj(fieldName, "$not", values.result) }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/exists/]]
    * @param value
    * @return
    */
  def $exists(value : Boolean) : BooleanExpression = { obj(fieldName, "$exists", value) }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/type/]]
    * @param code
    * @return
    */
  def $type(code : TypeCode) : BooleanExpression = { obj(fieldName, "$type", code.code.toInt) }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/mod/]]
    * @param divisor
    * @param remainder
    * @return
    */
  def $mod(divisor : Int, remainder : Int) = {
    arrayObj(fieldName, "$mod", Seq(divisor, remainder))(Codec.IntCodec)
  }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/mod/]]
    * @param divisor
    * @param remainder
    * @return
    */
  def $mod(divisor : Long, remainder : Long) = {
    arrayObj(fieldName, "$mod", Seq(divisor, remainder))(Codec.LongCodec)
  }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/regex/]]
    * @param pattern
    * @param options
    * @return
    */
  def $regex(pattern : String, options : String) = {
    anyObj(fieldName, Map("$regex" → s"/$pattern/", "$options" → s"'$options'"))
  }

  // TODO: Support conversion between Scala Regex and Perl Regex
  def $regex(regex : Regex) = ???

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/regex/]]
    * @param pattern
    * @return
    */
  def $regex(pattern : String) = { obj(fieldName, "$regex", s"/$pattern/") }

  // TODO: Support $all query modifier
  def $all = ???

  // TODO: Support $elemMatch query modifier
  def $elemMatch = ???

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/size/]]
    * @param size
    * @return
    */
  def $size(size : Int) = { anyObj(fieldName, Map("$size" → size)) }
}

object BooleanFieldExpression {
  def apply(fieldName : String) : BooleanFieldExpression = new BooleanFieldExpression(fieldName)
}

object $text {
  /** @see [[http://docs.mongodb.org/master/reference/operator/query/text/]]
    * @param search
    * @return
    */
  def apply(search : String) = {
    BooleanExpression().anyObj("$text", Map("$search" → search))
  }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/text/]]
    * @param search
    * @param language
    * @return
    */
  def $text(search : String, language : String) = {
    BooleanExpression().anyObj("$text", Map("$search" → search, "$language" → language))
  }
}

object $where {
  /** @see [[http://docs.mongodb.org/master/reference/operator/query/where/]]
    * @param javascript
    * @return
    */
  def apply(javascript : String) = {
    BooleanExpression().string("$where", javascript)
  }
}

class LogicalExpression(exp1 : BooleanExpression) extends BooleanExpression {
  /** @see [[http://docs.mongodb.org/master/reference/operator/query/and/]]
    * @param exp2
    * @return
    */
  def $and(exp2 : Expression) : BooleanExpression = { array("$and", exp1.result, exp2.result); this }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/or/]]
    * @param exp2
    * @return
    */
  def $or(exp2 : Expression) : BooleanExpression = { array("$or", exp1.result, exp2.result); this }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/nor/]]
    * @param exp2
    * @return
    */
  def $nor(exp2 : Expression) : BooleanExpression = { array("$nor", exp1.result, exp2.result); this }
}

object $and {
  /** @see [[[http://docs.mongodb.org/master/reference/operator/query/and/]]
    * @param expressions
    * @return
    */
  def apply(expressions : BooleanExpression*) : BooleanExpression = {
    val e = new BooleanExpression
    e.array("$and", expressions.map{ xe ⇒ xe.result })
    e
  }
}

object $or {
  /** @see [[[http://docs.mongodb.org/master/reference/operator/query/or/]]
    * @param expressions
    * @return
    */
  def apply(expressions : BooleanExpression*) : BooleanExpression = {
    BooleanExpression().array("$or", expressions.map { ex ⇒ ex.result })
  }
}

object $nor {
  /** @see [[[http://docs.mongodb.org/master/reference/operator/query/nor/]]
    * @param expressions
    * @return
    */
  def apply(expressions : BooleanExpression*) : BooleanExpression = {
    BooleanExpression().array("$nor", expressions.map { ex ⇒ ex.result })
  }
}

object $not extends BooleanExpression {
  /** @see [[[http://docs.mongodb.org/master/reference/operator/query/not/]]
    * @param expr
    * @return
    */
  def apply(expr : Expression with BooleanExpression) : BooleanExpression = {
    BooleanExpression().obj("$not", expr.result)
  }
}

class UpdateExpression extends Expression {
  def +(other : UpdateExpression) : UpdateExpression = {
    bldr ++= other.bldr.result
    this
  }

}

object UpdateExpression {
  def apply() = new UpdateExpression
  def apply(doc : BSONDocument) : UpdateExpression = {
    val result = UpdateExpression()
    doc.addTo(result)
    result
  }

  import rxmongo.bson._

  private[rxmongo] def apply(operator : String, value : BSONBuilder) : UpdateExpression = {
    UpdateExpression().obj(operator, value)
  }

  private[rxmongo] def apply[T](operator : String, field : String, value : T)(implicit codec : Codec[T]) : UpdateExpression = {
    val b = BSONBuilder()
    b.field(field, value)
    UpdateExpression().obj(operator, b.wrapAndTerminate)
  }

  private[rxmongo] def apply[T](operator : String, fields : Iterable[(String, T)])(implicit codec : Codec[T]) : UpdateExpression = {
    val b = BSONBuilder()
    for ((name, value) ← fields) {
      b.field(name, value)
    }
    UpdateExpression().obj(operator, b.wrapAndTerminate)
  }
}

object $inc {
  /** Increments the value of the field by the specified amount.
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/update/inc/#up._S_inc]]
    * @param pairs The field name and integer increment pairs
    * @return this
    */
  def apply(pairs : (String, AnyVal)*) : UpdateExpression = {
    val b = BSONBuilder()
    for ((field, value) ← pairs) {
      value match {
        case i : Int ⇒ b.integer(field, i)
        case l : Long ⇒ b.long(field, l)
        case d : Double ⇒ b.double(field, d)
        case f : Float ⇒ b.double(field, f)
        case s : Short ⇒ b.integer(field, s)
        case by : Byte ⇒ b.integer(field, by.toInt)
        case x ⇒ throw new IllegalArgumentException(s"Value $x is not suitable for $$inc operator")
      }
    }
    UpdateExpression("$inc", b)
  }

  def apply(field : String, value : Int) : UpdateExpression = {
    UpdateExpression("$inc", field, value)(Codec.IntCodec)
  }

  def apply(field : String, value : Long) : UpdateExpression = {
    UpdateExpression("$inc", field, value)(Codec.LongCodec)
  }

  def apply(field : String, value : Double) : UpdateExpression = {
    UpdateExpression("$inc", field, value)(Codec.DoubleCodec)
  }
}

object $mul {

  /** Multiplies the value of the field by the specified amount.
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/update/mul/]]
    * @param field The name of the field
    * @param value The value to multiple tye field by
    * @return this
    */
  def apply(field : String, value : Int) : UpdateExpression = {
    UpdateExpression("$mul", field, value)(Codec.IntCodec)
  }

  /** Multiplies the value of the field by the specified amount.
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/update/mul/]]
    * @param field The name of the field
    * @param value The value to multiple tye field by
    * @return this
    */
  def apply(field : String, value : Long) : UpdateExpression = {
    UpdateExpression("$mul", field, value)(Codec.LongCodec)
  }

  /** Multiplies the value of the field by the specified amount.
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/update/mul/]]
    * @param field The name of the field
    * @param value The value to multiple tye field by
    * @return this
    */
  def apply(field : String, value : Double) : UpdateExpression = {
    UpdateExpression("$mul", field, value)(Codec.DoubleCodec)
  }
}

object $rename {

  /** Renames a field.
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/update/rename/]]
    * @param fields The name and rename for the fields
    * @return this
    */
  def apply(fields : (String, String)*) : UpdateExpression = {
    val b = BSONBuilder()
    for ((field, renameTo) ← fields) {
      b.string(field, renameTo)
    }
    UpdateExpression("$rename", b)
  }

  def apply(field : String, newName : String) : UpdateExpression = {
    UpdateExpression("$rename", field, newName)(Codec.StringCodec)
  }
}

object $setOnInsert {
  /** Sets the value of a field if an update results in an insert of a document. Has no effect on update operations
    * that modify existing documents.
    *
    * @param value
    * @param codec
    * @tparam T
    * @return
    */
  def apply[T](fieldName : String, value : T)(implicit codec : Codec[T]) : UpdateExpression = {
    UpdateExpression("$setOnInsert", fieldName, value)(codec)
  }

  def apply[T](pairs : (String, T)*)(implicit codec : Codec[T]) : UpdateExpression = {
    UpdateExpression("$setOnInsert", pairs)(codec)
  }
}

object $set {
  /** Sets the value of a field in a document.
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/update/set/]]
    * @param fieldName Name of field to set
    * @param value Value to set field to
    * @param codec codec to use to turn value into BSONValue
    * @tparam T Scala type for the value
    * @return this
    */
  def apply[T](fieldName : String, value : T)(implicit codec : Codec[T]) : UpdateExpression = {
    UpdateExpression("$set", fieldName, value)(codec)
  }

  /** Sets the value of multiple fields in a document, of a given type
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/update/set/]]
    * @param fields The name/value pairs to set in the updated document
    * @param codec codec to use to turn value into BSONValue
    * @tparam T Scala type for the value
    * @return this
    */
  def apply[T](fields : (String, T)*)(implicit codec : Codec[T]) : UpdateExpression = {
    UpdateExpression("$set", fields)(codec)
  }

}

object $unset {

  /** Removes the specified fields from a document.
    *
    * @param fields The names of the fields to remove (unset) from the document
    * @return this
    */
  def apply(fields : String*) : UpdateExpression = {
    UpdateExpression("$unset", fields.map { s ⇒ s → "" })(Codec.StringCodec)
  }
}

object $min {
  /** Only updates the field if the specified value is less than the existing field value.
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/update/min/]]
    * @param field The name of the field to update
    * @param value The value to use for updating the field
    * @param codec The codec to use to conver the value to a BSONValue
    * @tparam T The scala type of the value
    * @return this
    */
  def apply[T](field : String, value : T)(implicit codec : Codec[T]) : UpdateExpression = {
    UpdateExpression("$min", field, value)(codec)
  }

  /** Only updates the field if the specified value is less than the existing field value.
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/update/min/]]
    * @param pairs The name/value pairs of the fields to update
    * @param codec The codec to use to conver the value to a BSONValue
    * @tparam T The scala type of the value
    * @return this
    */
  def apply[T](pairs : (String, T)*)(implicit codec : Codec[T]) : UpdateExpression = {
    UpdateExpression("$min", pairs)(codec)
  }
}

object $max {

  /** Only updates the field if the specified value is greater than the existing field value.
    *
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/update/max/]]
    * @param field The name of the field to update
    * @param value The value to use for updating the field
    * @param codec The codec to use to conver the value to a BSONValue
    * @tparam T The scala type of the value
    * @return this
    */
  def apply[T](field : String, value : T)(implicit codec : Codec[T]) : UpdateExpression = {
    UpdateExpression("$max", field, value)(codec)
  }

  /** Only updates the field if the specified value is greater than the existing field value.
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/update/max/]]
    * @param fields The name/value pairs of the fields to update
    * @param codec The codec to use to conver the value to a BSONValue
    * @tparam T The scala type of the value
    * @return this
    */
  def apply[T](fields : (String, T)*)(implicit codec : Codec[T]) : UpdateExpression = {
    UpdateExpression("$max", fields)
  }
}

object $currentDate {

  /** Sets the value of a field to current date, either as a Date or a Timestamp.
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/update/currentDate/]]
    * @param field THe name of the field to set
    * @param wantDate True to set as a Date value, false to set as a Timestamp value
    * @return this
    */
  def apply(field : String, wantDate : Boolean = false) : UpdateExpression = {
    UpdateExpression("$currentDate", field, wantDate)
  }

  /** Sets the value of a field to current date, either as a Date or a Timestamp.
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/update/currentDate/]]
    * @param fields The field/Boolean pairs to specify how to set the current date (or timestamp)
    * @return this
    */
  def apply(fields : (String, Boolean)*) : UpdateExpression = {
    UpdateExpression[Boolean]("$currentDate", fields)
  }
}

object $addToSet {

  /** Add an element to an array only if it does not already exist in the set.
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/update/addToSet/]]
    * @param field The name of the array field to which the element will be added
    * @param value The value to add to the array field
    * @param codec The codec to use to conver the value to a BSONValue
    * @tparam T The Scala type of the value
    * @return this
    */
  def apply[T](field : String, value : T)(implicit codec : Codec[T]) : UpdateExpression = {
    UpdateExpression("$addToSet", field, value)(codec)
  }

  /** Add elements to an array only if they do not already exist in the set.
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/update/addToSet/]]
    * @param field The name of the array field to which the element will be added
    * @param values The value to add to the array field
    * @param codec The codec to use to conver the value to a BSONValue
    * @tparam T The Scala type of the value
    * @return this
    */
  def apply[T](field : String, values : Seq[T])(implicit codec : Codec[T]) : UpdateExpression = {
    val bldr = ByteString.newBuilder
    bldr.putPrefix(ObjectCode, field)
    bldr.array("$each", values)(codec)
    UpdateExpression().obj("$addToSet", bldr.wrapAndTerminate)
  }
}

object $pop {

  /** Removes the first or last item of an array.
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/update/pop/]]
    * @param field Name of the field from which to pop a value
    * @param head True to pop from the head (lowest index) or false to pop from the tail (highest index)
    * @return Å new UpdateExpression containing the \$pop operator
    */
  def apply(field : String, head : Boolean = true) : UpdateExpression = {
    UpdateExpression().obj("$pop", field, if (head) -1 else 1)(Codec.IntCodec)
  }

  def head(field : String) = apply(field, head = true)

  def tail(field : String) = apply(field, head = false)

}

object $pullAll {

  /** Removes all matching values from an array.
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/update/pullAll/]]
    * @param field The name of the field from which the values will be pulled
    * @param values The values of the field that will be removed
    * @param codec A codec for translating the values
    * @tparam T The scala type of the values
    * @return A new UpdateExpression containing the \$pull operator
    */
  def apply[T](field : String, values : Seq[T])(implicit codec : Codec[T]) : UpdateExpression = {
    val b = BSONBuilder()
    b.array(field, values)(codec)
    UpdateExpression().obj("$pull", b)
  }
}

object $pull {

  /** Removes all array elements that match a specified query.
    *
    * @param field The name of the array field from which values should be pulled
    * @param value The value that should be pulled
    * @param codec A codec for translating the values
    * @tparam T The scala type of the values
    * @return A new UpdateExpression with the \$pull operator
    */
  def apply[T](field : String, value : T)(implicit codec : Codec[T]) : UpdateExpression = {
    UpdateExpression("$pull", field, value)(codec)
  }

  /** Removes all array elements that match a set of field/value pairs
    *
    * @param fields The values of the array fields that should be pulled from the document
    * @param codec A codec for translating the values
    * @tparam T The scala type of the values
    * @return A new UpdateExpression with the \$pull operator
    */
  def apply[T](fields : (String, T)*)(implicit codec : Codec[T]) : UpdateExpression = {
    UpdateExpression("$pull", fields)(codec)
  }

  /** Removes all array elements that match a query
    *
    * @param fields The values of the array fields that should be pulled from the document
    * @return A new UpdateExpression with the \$pull operator
    */
  def apply(fields : (String, Query)*) : UpdateExpression = {
    val b = BSONBuilder()
    for ((field, query) ← fields) {
      b.obj(field, query.result)
    }
    UpdateExpression("$pull", b)
  }
}

object $push {

  /** Adds an item to an array.
    *
    * @param fields The array fields and values to be pushed
    * @param codec The codec to convert the fields to bytes
    * @tparam T The type of the fields in the array
    * @return An UpdateExpression with the \$push operator
    */
  def apply[T](fields : (String, T)*)(implicit codec : Codec[T]) : UpdateExpression = {
    UpdateExpression("$push", fields)(codec)
  }

  def apply[T](fieldName : String, each : Seq[T], slice : Int = Int.MinValue,
    position : Int = -1, sort : Int = 0)(implicit codec : Codec[T]) : UpdateExpression = {
    val b2 = BSONBuilder()
    b2.array("$each", each)
    if (slice != Int.MinValue)
      b2.integer("$slice", slice)
    if (position >= 0)
      b2.integer("$position", position)
    if (position != 0)
      b2.integer("$sort", if (position < 0) -1 else 1)
    val b1 = BSONBuilder()
    b1.obj(fieldName, b2)
    val e = UpdateExpression()
    e.obj("$push", b1)
    e
  }
}

object $bit {

  private def bitwise(operator : String, fieldName : String, value : Int) : UpdateExpression = {
    val b = BSONBuilder()
    b.integer(operator, value)
    val b2 = BSONBuilder()
    b2.obj(fieldName, b)
    val e = UpdateExpression()
    e.obj("$bit", b2)
    e
  }

  def and(fieldName : String, value : Int) : UpdateExpression = bitwise("and", fieldName, value)

  def or(fieldName : String, value : Int) : UpdateExpression = bitwise("or", fieldName, value)

  def xor(fieldName : String, value : Int) : UpdateExpression = bitwise("xor", fieldName, value)
}
