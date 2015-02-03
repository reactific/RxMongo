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

trait LiteralExpression extends Expression

class StringLiteral(str : String) extends LiteralExpression { buffer.putStr(str) }

class IntLiteral(int : Int) extends LiteralExpression { buffer.putInt(int) }

class DoubleLiteral(dbl : Double) extends LiteralExpression { buffer.putDouble(dbl) }

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

class UpdateExpression extends Expression

/** TODO: Finish UpdateFieldExpression implementation
  * @param fieldName The name of the field to which the operator applies
  */
class UpdateFieldExpression(fieldName : String) extends UpdateExpression {

  /** Increments the value of the field by the specified amount.
    *
    * @param value
    * @param codec
    * @tparam T
    * @tparam B
    * @return
    */
  def $inc[T, B <: BSONValue](value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = ???

  /** Multiplies the value of the field by the specified amount.
    *
    * @param value
    * @param codec
    * @tparam T
    * @tparam B
    * @return
    */
  def $mul[T, B <: BSONValue](value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = ???

  /** Renames a field.
    *
    * @param value
    * @param codec
    * @tparam T
    * @tparam B
    * @return
    */
  def $rename[T, B <: BSONValue](value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = ???

  /** Sets the value of a field if an update results in an insert of a document. Has no effect on update operations
    * that modify existing documents.
    *
    * @param value
    * @param codec
    * @tparam T
    * @tparam B
    * @return
    */
  def $setOnInsert[T, B <: BSONValue](value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = ???

  /** Sets the value of a field in a document.
    *
    * @param value
    * @param codec
    * @tparam T
    * @tparam B
    * @return
    */
  def $set[T, B <: BSONValue](value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = {
    obj("$set", fieldName → value)
    this
  }

  /** Removes the specified field from a document.
    *
    * @param value
    * @param codec
    * @tparam T
    * @tparam B
    * @return
    */
  def $unset[T, B <: BSONValue](value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = ???

  /** Only updates the field if the specified value is less than the existing field value.
    *
    * @param value
    * @param codec
    * @tparam T
    * @tparam B
    * @return
    */
  def $min[T, B <: BSONValue](value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = ???

  /** Only updates the field if the specified value is greater than the existing field value.
    *
    * @param value
    * @param codec
    * @tparam T
    * @tparam B
    * @return
    */
  def $max[T, B <: BSONValue](value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = ???

  /** Sets the value of a field to current date, either as a Date or a Timestamp.
    *
    * @param value
    * @param codec
    * @tparam T
    * @tparam B
    * @return
    */
  def $currentDate[T, B <: BSONValue](value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = ???

  /** Acts as a placeholder to update the first element that matches the query condition in an update.
    *
    * @param value
    * @param codec
    * @tparam T
    * @tparam B
    * @return
    */
  def $[T, B <: BSONValue](value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = ???

  /** Adds elements to an array only if they do not already exist in the set.
    *
    * @param value
    * @param codec
    * @tparam T
    * @tparam B
    * @return
    */
  def $addToSet[T, B <: BSONValue](value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = ???

  /** Removes the first or last item of an array.
    *
    * @param value
    * @param codec
    * @tparam T
    * @tparam B
    * @return
    */
  def $pop[T, B <: BSONValue](value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = ???

  /** Removes all matching values from an array.
    *
    * @param value
    * @param codec
    * @tparam T
    * @tparam B
    * @return
    */
  def $pullAll[T, B <: BSONValue](value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = ???

  /** Removes all array elements that match a specified query.
    *
    * @param value
    * @param codec
    * @tparam T
    * @tparam B
    * @return
    */
  def $pull[T, B <: BSONValue](value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = ???

  /** Deprecated. Adds several items to an array.
    *
    * @param value
    * @param codec
    * @tparam T
    * @tparam B
    * @return
    */
  def $pushAll[T, B <: BSONValue](value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = ???

  /** Adds an item to an array.
    *
    * @param value
    * @param codec
    * @tparam T
    * @tparam B
    * @return
    */
  def $push[T, B <: BSONValue](value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = ???

  /** Modifies the $push and $addToSet operators to append multiple items for array updates.
    *
    * @param value
    * @param codec
    * @tparam T
    * @tparam B
    * @return
    */

  def $each[T, B <: BSONValue](value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = ???

  /** Modifies the $push operator to limit the size of updated arrays.
    *
    * @param value
    * @param codec
    * @tparam T
    * @tparam B
    * @return
    */
  def $slice[T, B <: BSONValue](value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = ???

  /** Modifies the $push operator to reorder documents stored in an array.
    *
    * @param value
    * @param codec
    * @tparam T
    * @tparam B
    * @return
    */
  def $sort[T, B <: BSONValue](value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = ???

  /** Modifies the $push operator to specify the position in the array to add elements.
    *
    * @param value
    * @param codec
    * @tparam T
    * @tparam B
    * @return
    */
  def $position[T, B <: BSONValue](value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = ???

  /** Performs bitwise AND, OR, and XOR updates of integer values.
    *
    * @param value
    * @param codec
    * @tparam T
    * @tparam B
    * @return
    */
  def $bit[T, B <: BSONValue](value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = ???

  /** Modifies the behavior of a write operation to increase the isolation of the operation.
    *
    * @param value
    * @param codec
    * @tparam T
    * @tparam B
    * @return
    */
  def $isolated[T, B <: BSONValue](value : T)(implicit codec : BSONCodec[T, B]) : UpdateExpression = ???
}
