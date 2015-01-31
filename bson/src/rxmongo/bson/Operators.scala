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

/** Expression */
class Expression extends BSONBuilder

trait LiteralExpression extends Expression

class StringLiteral(str : String) extends LiteralExpression { buffer.putStr(str) }

class IntLiteral(int : Int) extends LiteralExpression { buffer.putInt(int) }

class DoubleLiteral(dbl : Double) extends LiteralExpression { buffer.putDouble(dbl) }

class BooleanExpression extends Expression

class BooleanFieldExpression(fieldName : String) extends BooleanExpression {

  /**
   * @see [[http://docs.mongodb.org/master/reference/operator/query/eq/]]
   * @param value
   * @return
   */
  def $eq(value : Any) : BooleanExpression = { append(fieldName, value); this }

  /**
   * @see [[http://docs.mongodb.org/master/reference/operator/query/ne/]]
   * @param value
   * @return
   */
  def $ne(value : Any) : BooleanExpression = { obj(fieldName, Map("$ne" -> value)); this }

  /**
   * @see [[http://docs.mongodb.org/master/reference/operator/query/gt/]]
   * @param value
   * @return
   */
  def $gt(value : Any) : BooleanExpression = { obj(fieldName, Map("$gt" -> value)); this }

  /**
   * @see [[http://docs.mongodb.org/master/reference/operator/query/lt/]]
   * @param value
   * @return
   */
  def $lt(value : Any) : BooleanExpression = { obj(fieldName, Map("$lt" -> value)); this }

  /**
   * @see [[http://docs.mongodb.org/master/reference/operator/query/gte/]]
   * @param value
   * @return
   */
  def $gte(value : Any) : BooleanExpression = { obj(fieldName, Map("$gte" -> value)); this }

  /**
   * @see [[http://docs.mongodb.org/master/reference/operator/query/lte/]]
   * @param value
   * @return
   */
  def $lte(value : Any) : BooleanExpression = { obj(fieldName, Map("$lte" -> value)); this }

  /**
   * @see [[http://docs.mongodb.org/master/reference/operator/query/in/]]
   * @param values
   * @return
   */
  def $in(values : Any*) : BooleanExpression = { obj(fieldName, Map("$in" -> BSONArray(values))); this }

  /**
   * @see [[http://docs.mongodb.org/master/reference/operator/query/nin/]]
   * @param values
   * @return
   */
  def $nin(values : Any*) : BooleanExpression = { obj(fieldName, Map("$nin" -> BSONArray(values))); this }

  /**
   * @see [[http://docs.mongodb.org/master/reference/operator/query/not/]]
   * @param values
   * @return
   */
  def $not(values : BooleanExpression) : BooleanExpression = { obj(fieldName, Map("$not" -> values.result)); this }

  /**
   * @see [[http://docs.mongodb.org/master/reference/operator/query/exists/]]
   * @param value
   * @return
   */
  def $exists(value : Boolean) : BooleanExpression = { obj(fieldName, Map("$exists" -> value)); this }

  /**
   * @see [[http://docs.mongodb.org/master/reference/operator/query/type/]]
   * @param code
   * @return
   */
  def $type(code : TypeCode) : BooleanExpression = { obj(fieldName, Map("$type" -> code.code.toInt)); this }

  /**
   * @see [[http://docs.mongodb.org/master/reference/operator/query/mod/]]
   * @param divisor
   * @param remainder
   * @return
   */
  def $mod(divisor: Int, remainder: Int) = { obj(fieldName, Map("$mod" → Seq(divisor, remainder))) }

  /**
   * @see [[http://docs.mongodb.org/master/reference/operator/query/mod/]]
   * @param divisor
   * @param remainder
   * @return
   */
  def $mod(divisor: Long, remainder: Long) = { obj(fieldName, Map("$mod" → Seq(divisor, remainder))) }

  /**
   * @see [[http://docs.mongodb.org/master/reference/operator/query/regex/]]
   * @param pattern
   * @param options
   * @return
   */
  def $regex(pattern: String, options: String) = {
    obj(fieldName, Map("$regex" →s"/$pattern/", "$options" → s"'$options'"))
  }

  /**
   * @see [[http://docs.mongodb.org/master/reference/operator/query/regex/]]
   * @param pattern
   * @return
   */
  def $regex(pattern: String) = { obj(fieldName, Map("$regex" → s"/$pattern/")) }

  def $all = ???
  def $elemMatch = ???

  /**
   * @see [[http://docs.mongodb.org/master/reference/operator/query/size/]]
   * @param size
   * @return
   */
  def $size(size: Int) = { obj(fieldName, Map("$size" → size)) }
}

object $text {
  /**
   * @see [[http://docs.mongodb.org/master/reference/operator/query/text/]]
   * @param search
   * @return
   */
  def apply(search : String) = {
    val e = new BooleanExpression
    e.obj("$text", Map("$search" → search))
    e
  }

  /**
   * @see [[http://docs.mongodb.org/master/reference/operator/query/text/]]
   * @param search
   * @param language
   * @return
   */
  def $text(search: String, language: String) = {
    val e = new BooleanExpression
    e.obj("$text", Map("$search" → search, "$language" → language))
    e
  }
}


object $where {
  /**
   * @see [[http://docs.mongodb.org/master/reference/operator/query/where/]]
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
  /**
   * @see [[http://docs.mongodb.org/master/reference/operator/query/and/]]
   * @param exp2
   * @return
   */
  def $and(exp2 : Expression) : BooleanExpression = { array("$and", exp1.result, exp2.result); this }

  /**
   * @see [[http://docs.mongodb.org/master/reference/operator/query/or/]]
   * @param exp2
   * @return
   */
  def $or(exp2 : Expression) : BooleanExpression = { array("$or", exp1.result, exp2.result); this }

  /**
   * @see [[http://docs.mongodb.org/master/reference/operator/query/nor/]]
   * @param exp2
   * @return
   */
  def $nor(exp2 : Expression) : BooleanExpression = { array("$nor", exp1.result, exp2.result); this }
}

object $and {
  /**
   * @see [[[http://docs.mongodb.org/master/reference/operator/query/and/]]
   * @param expressions
   * @return
   */
  def apply(expressions : BooleanExpression*) : BooleanExpression = {
    val e = new BooleanExpression
    e.array("$and", expressions.map{ xe ⇒ xe.result } : _*)
    e
  }
}

object $or {
  /**
   * @see [[[http://docs.mongodb.org/master/reference/operator/query/or/]]
   * @param expressions
   * @return
   */
  def apply(expressions: BooleanExpression*) : BooleanExpression = {
    val e = new BooleanExpression
    e.array("$or", expressions.map { ex ⇒ ex.result } : _*)
    e
  }
}

object $nor {
  /**
   * @see [[[http://docs.mongodb.org/master/reference/operator/query/nor/]]
   * @param expressions
   * @return
   */
  def apply(expressions: BooleanExpression*) : BooleanExpression = {
    val e = new BooleanExpression
    e.array("$nor", expressions.map { ex ⇒ ex.result } : _*)
    e
  }
}

object $not extends BooleanExpression {
  /**
   * @see [[[http://docs.mongodb.org/master/reference/operator/query/not/]]
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
