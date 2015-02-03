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

import scala.util.matching.Regex

/** Expression */
class Expression extends BSONBuilder

class BooleanExpression extends Expression

class BooleanFieldExpression(fieldName : String) extends BooleanExpression {

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/eq/]]
    * @param value
    * @return
    */
  def $eq[T, B <: BSONValue](value : T)(implicit codec : BSONCodec[T, B]) : BooleanExpression = {
    appendAs[T, B](fieldName, value)
    this
  }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/ne/]]
    * @param value
    * @return
    */
  def $ne[T, B <: BSONValue](value : T)(implicit codec : BSONCodec[T, B]) : BooleanExpression = {
    obj(fieldName, Map("$ne" -> value))
    this
  }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/gt/]]
    * @param value
    * @return
    */
  def $gt[T, B <: BSONValue](value : T)(implicit codec : BSONCodec[T, B]) : BooleanExpression = {
    obj(fieldName, Map("$gt" -> value))
    this
  }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/lt/]]
    * @param value
    * @return
    */
  def $lt[T, B <: BSONValue](value : T)(implicit codec : BSONCodec[T, B]) : BooleanExpression = {
    obj(fieldName, Map("$lt" -> value))
    this
  }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/gte/]]
    * @param value
    * @return
    */
  def $gte[T, B <: BSONValue](value : T)(implicit codec : BSONCodec[T, B]) : BooleanExpression = {
    obj(fieldName, Map("$gte" -> value))
    this
  }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/lte/]]
    * @param value
    * @return
    */
  def $lte[T, B <: BSONValue](value : T)(implicit codec : BSONCodec[T, B]) : BooleanExpression = {
    obj(fieldName, Map("$lte" -> value))
    this
  }

  def $lte[T, B <: BSONValue](values : Iterable[T])(implicit codec : BSONCodec[T, B]) : BooleanExpression = {
    embeddedArray[T, B](fieldName, "$lte", values)
    this
  }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/in/]]
    * @param values
    * @return
    */
  def $in[T, B <: BSONValue](values : T*)(implicit codec : BSONCodec[T, B]) : BooleanExpression = {
    embeddedArray[T, B](fieldName, "$in", values)
    this
  }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/nin/]]
    * @param values
    * @return
    */
  def $nin[T, B <: BSONValue](values : T*)(implicit codec : BSONCodec[T, B]) : BooleanExpression = {
    embeddedArray[T, B](fieldName, "$nin", values)
    this
  }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/not/]]
    * @param values
    * @return
    */
  def $not(values : BooleanExpression) : BooleanExpression = { obj(fieldName, Map("$not" -> values.result)); this }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/exists/]]
    * @param value
    * @return
    */
  def $exists(value : Boolean) : BooleanExpression = { obj(fieldName, Map("$exists" -> value)); this }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/type/]]
    * @param code
    * @return
    */
  def $type(code : TypeCode) : BooleanExpression = { obj(fieldName, Map("$type" -> code.code.toInt)); this }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/mod/]]
    * @param divisor
    * @param remainder
    * @return
    */
  def $mod(divisor : Int, remainder : Int) = { obj(fieldName, Map("$mod" → Seq(divisor, remainder))) }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/mod/]]
    * @param divisor
    * @param remainder
    * @return
    */
  def $mod(divisor : Long, remainder : Long) = { obj(fieldName, Map("$mod" → Seq(divisor, remainder))) }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/regex/]]
    * @param pattern
    * @param options
    * @return
    */
  def $regex(pattern : String, options : String) = {
    obj(fieldName, Map("$regex" → s"/$pattern/", "$options" → s"'$options'"))
  }

  // TODO: Support conversion between Scala Regex and Perl Regex
  def $regex(regex : Regex) = ???

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/regex/]]
    * @param pattern
    * @return
    */
  def $regex(pattern : String) = { obj(fieldName, Map("$regex" → s"/$pattern/")) }

  // TODO: Support $all query modifier
  def $all = ???

  // TODO: Support $elemMatch query modifier
  def $elemMatch = ???

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/size/]]
    * @param size
    * @return
    */
  def $size(size : Int) = { obj(fieldName, Map("$size" → size)) }
}

object $text {
  /** @see [[http://docs.mongodb.org/master/reference/operator/query/text/]]
    * @param search
    * @return
    */
  def apply(search : String) = {
    val e = new BooleanExpression
    e.obj("$text", Map("$search" → search))
    e
  }

  /** @see [[http://docs.mongodb.org/master/reference/operator/query/text/]]
    * @param search
    * @param language
    * @return
    */
  def $text(search : String, language : String) = {
    val e = new BooleanExpression
    e.obj("$text", Map("$search" → search, "$language" → language))
    e
  }
}

object $where {
  /** @see [[http://docs.mongodb.org/master/reference/operator/query/where/]]
    * @param javascript
    * @return
    */
  def apply(javascript : String) = {
    val e = new BooleanExpression
    e.string("$where", javascript)
    e
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
    val e = new BooleanExpression
    e.array("$or", expressions.map { ex ⇒ ex.result })
    e
  }
}

object $nor {
  /** @see [[[http://docs.mongodb.org/master/reference/operator/query/nor/]]
    * @param expressions
    * @return
    */
  def apply(expressions : BooleanExpression*) : BooleanExpression = {
    val e = new BooleanExpression
    e.array("$nor", expressions.map { ex ⇒ ex.result })
    e
  }
}

object $not extends BooleanExpression {
  /** @see [[[http://docs.mongodb.org/master/reference/operator/query/not/]]
    * @param expr
    * @return
    */
  def apply(expr : Expression with BooleanExpression) : BooleanExpression = {
    val e = new BooleanExpression
    e.putPrefix(ObjectCode, "$not")
    e.obj(expr.result)
    e
  }
}

class UpdateExpression extends Expression {
  def +(other : UpdateExpression) : UpdateExpression = {
    buffer ++= other.buffer.result
    this
  }

}

object UpdateExpression {
  def apply() = new UpdateExpression

  private[rxmongo] def apply(operator : String, value : BSONBuilder) : UpdateExpression = {
    val e = UpdateExpression()
    e.putPrefix(ObjectCode, operator).
      putObj(value)
    e
  }

  private[rxmongo] def apply[T, B <: BSONValue](operator : String, field : String, value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = {
    val e = UpdateExpression()
    e.putPrefix(ObjectCode, operator)
    e.putObj(BSONBuilder().append(field, codec.write(value)))
    e
  }

  private[rxmongo] def apply[T, B <: BSONValue](operator : String, fields : Iterable[(String, T)])(implicit codec : BSONCodec[T, B]) : UpdateExpression = {
    val b = BSONBuilder()
    for ((name, value) ← fields) {
      b.append(name, codec.write(value))
    }
    val e = UpdateExpression()
    e.putPrefix(ObjectCode, operator)
    e.putObj(b)
    e
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
    UpdateExpression("$inc", field, value)
  }

  def apply(field : String, value : Long) : UpdateExpression = {
    UpdateExpression("$inc", field, value)
  }

  def apply(field : String, value : Double) : UpdateExpression = {
    UpdateExpression("$inc", field, value)
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
    UpdateExpression("$mul", field, value)
  }

  /** Multiplies the value of the field by the specified amount.
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/update/mul/]]
    * @param field The name of the field
    * @param value The value to multiple tye field by
    * @return this
    */
  def apply(field : String, value : Long) : UpdateExpression = {
    UpdateExpression("$mul", field, value)
  }

  /** Multiplies the value of the field by the specified amount.
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/update/mul/]]
    * @param field The name of the field
    * @param value The value to multiple tye field by
    * @return this
    */
  def apply(field : String, value : Double) : UpdateExpression = {
    UpdateExpression("$mul", field, value)
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
    UpdateExpression("$rename", field, newName)
  }
}

object $setOnInsert {
  /** Sets the value of a field if an update results in an insert of a document. Has no effect on update operations
    * that modify existing documents.
    *
    * @param value
    * @param codec
    * @tparam T
    * @tparam B
    * @return
    */
  def apply[T, B <: BSONValue](fieldName : String, value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = {
    UpdateExpression("$setOnInsert", fieldName, value)(codec)
  }

  def apply[T, B <: BSONValue](pairs : (String, T)*)(implicit codec : BSONCodec[T, B]) : UpdateExpression = {
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
    * @tparam B BSONType for the value
    * @return this
    */
  def apply[T, B <: BSONValue](fieldName : String, value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = {
    UpdateExpression("$set", fieldName, value)(codec)
  }

  /** Sets the value of multiple fields in a document, of a given type
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/update/set/]]
    * @param fields The name/value pairs to set in the updated document
    * @param codec codec to use to turn value into BSONValue
    * @tparam T Scala type for the value
    * @tparam B BSONType for the value
    * @return this
    */
  def apply[T, B <: BSONValue](fields : (String, T)*)(implicit codec : BSONCodec[T, B]) : UpdateExpression = {
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
    UpdateExpression("$unset", fields.map { s ⇒ s → "" })
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
    * @tparam B The BSONValue type of the value
    * @return this
    */
  def apply[T, B <: BSONValue](field : String, value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = {
    UpdateExpression("$min", field, value)(codec)
  }

  /** Only updates the field if the specified value is less than the existing field value.
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/update/min/]]
    * @param pairs The name/value pairs of the fields to update
    * @param codec The codec to use to conver the value to a BSONValue
    * @tparam T The scala type of the value
    * @tparam B The BSONValue type of the value
    * @return this
    */
  def apply[T, B <: BSONValue](pairs : (String, T)*)(implicit codec : BSONCodec[T, B]) : UpdateExpression = {
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
    * @tparam B The BSONValue type of the value
    * @return this
    */
  def apply[T, B <: BSONValue](field : String, value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = {
    UpdateExpression("$max", field, value)(codec)
  }

  /** Only updates the field if the specified value is greater than the existing field value.
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/update/max/]]
    * @param fields The name/value pairs of the fields to update
    * @param codec The codec to use to conver the value to a BSONValue
    * @tparam T The scala type of the value
    * @tparam B The BSONValue type of the value
    * @return this
    */
  def apply[T, B <: BSONValue](fields : (String, T)*)(implicit codec : BSONCodec[T, B]) : UpdateExpression = {
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
    UpdateExpression[Boolean, BSONBoolean]("$currentDate", fields)
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
    * @tparam B The BSONValue type of the value
    * @return this
    */
  def apply[T, B <: BSONValue](field : String, value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = {
    UpdateExpression("$addToSet", field, value)(codec)
  }

  /** Add elements to an array only if they do not already exist in the set.
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/update/addToSet/]]
    * @param field The name of the array field to which the element will be added
    * @param values The value to add to the array field
    * @param codec The codec to use to conver the value to a BSONValue
    * @tparam T The Scala type of the value
    * @tparam B The BSONValue type of the value
    * @return this
    */
  def apply[T, B <: BSONValue](field : String, values : Seq[T])(implicit codec : BSONCodec[T, B]) : UpdateExpression = {
    val e = UpdateExpression()
    e.putPrefix(ObjectCode, "$addToSet")
    val bldr = BSONBuilder()
    bldr.putPrefix(ObjectCode, field)
    bldr.array("$each", values)
    e.putObj(bldr)
    e
  }
}

object $pop {

  /** Removes the first or last item of an array.
    *
    * @see [[http://docs.mongodb.org/master/reference/operator/update/pop/]]
    * @param field Name of the field from which to pop a value
    * @param head True to pop from the head (lowest index) or false to pop from the tail (highest index)
    * @return Å new UpdateExpression containing the $pop operator
    */
  def apply(field : String, head : Boolean = true) : UpdateExpression = {
    val e = UpdateExpression()
    e.obj("$pop", field → (if (head) -1 else 1))
    e
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
    * @tparam B The BSONValue type of the values
    * @return A new UpdateExpression containing the $pull operator
    */
  def apply[T, B <: BSONValue](field : String, values : Seq[T])(implicit codec : BSONCodec[T, B]) : UpdateExpression = {
    val e = UpdateExpression()
    e.putPrefix(ObjectCode, "$pull")
    val b = BSONBuilder()
    b.array(field, values)(codec)
    e.putObj(b)
    e
  }
}

object $pull {

  /** Removes all array elements that match a specified query.
    *
    * @param field The name of the array field from which values should be pulled
    * @param value The value that should be pulled
    * @param codec A codec for translating the values
    * @tparam T The scala type of the values
    * @tparam B The BSONValue type of the values
    * @return A new UpdateExpression with the $pull operator
    */
  def apply[T, B <: BSONValue](field : String, value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = {
    UpdateExpression("$pull", field, value)(codec)
  }

  /** Removes all array elements that match a set of field/value pairs
    *
    * @param fields The values of the array fields that should be pulled from the document
    * @param codec A codec for translating the values
    * @tparam T The scala type of the values
    * @tparam B The BSONValue type of the values
    * @return A new UpdateExpression with the $pull operator
    */
  def apply[T, B <: BSONValue](fields : (String, T)*)(implicit codec : BSONCodec[T, B]) : UpdateExpression = {
    UpdateExpression("$pull", fields)(codec)
  }

  /** Removes all array elements that match a query
    *
    * @param fields The values of the array fields that should be pulled from the document
    * @return A new UpdateExpression with the $pull operator
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
    * @param codec
    * @tparam T
    * @tparam B
    * @return
    */
  def apply[T, B <: BSONValue](fields : (String, T)*)(implicit codec : BSONCodec[T, B]) : UpdateExpression = {
    UpdateExpression("$push", fields)(codec)
  }

  def apply[T, B <: BSONValue](fieldName : String, each : Seq[T], slice : Int = Int.MinValue,
    position : Int = -1, sort : Int = 0)(implicit codec : BSONCodec[T, B]) : UpdateExpression = {
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
