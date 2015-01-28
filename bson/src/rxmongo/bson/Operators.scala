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

import scala.language.implicitConversions

/** Expression */
class Expression extends BSONBuilder

trait LiteralExpression extends Expression

class StringLiteral(str : String) extends LiteralExpression { buffer.putStr(str) }

class IntLiteral(int : Int) extends LiteralExpression { buffer.putInt(int) }

class DoubleLiteral(dbl : Double) extends LiteralExpression { buffer.putDouble(dbl) }

class BooleanExpression extends Expression

class BooleanFieldExpression(fieldName : String) extends BooleanExpression {
  def $eq(value : Any) : BooleanExpression = { append(fieldName, value); this }
  def $ne(value : Any) : BooleanExpression = { obj(fieldName, Map("$ne" -> value)); this }
  def $gt(value : Any) : BooleanExpression = { obj(fieldName, Map("$gt" -> value)); this }
  def $lt(value : Any) : BooleanExpression = { obj(fieldName, Map("$lt" -> value)); this }
  def $in(values : Any*) : BooleanExpression = { obj(fieldName, Map("$in" -> BSONArray(values))); this }
  def $gte(value : Any) : BooleanExpression = { obj(fieldName, Map("$gte" -> value)); this }
  def $lte(value : Any) : BooleanExpression = { obj(fieldName, Map("$lte" -> value)); this }
  def $nin(values : Any*) : BooleanExpression = { obj(fieldName, Map("$nin" -> BSONArray(values))); this }
  def $not(values : BooleanExpression) : BooleanExpression = { obj(fieldName, Map("$not" -> values.result())); this }
  def $exists(value : Boolean) : BooleanExpression = { obj(fieldName, Map("$exists" -> value)); this }
  def $type(code : TypeCode) : BooleanExpression = { obj(fieldName, Map("$type" -> code.code.toInt)); this }
}

class LogicalExpression(exp1 : BooleanExpression) extends BooleanExpression {
  def $and(exp2 : Expression) : BooleanExpression = { array("$and", exp1.result(), exp2.result()); this }
  def $or(exp2 : Expression) : BooleanExpression = { array("$and", exp1.result(), exp2.result()); this }
  def $nor(exp2 : Expression) : BooleanExpression = { array("$nor", exp1.result(), exp2.result()); this }
}

object $and extends BooleanExpression {
  def apply(expressions : BooleanExpression*) : BooleanExpression = {
    val e = new BooleanExpression
    e.array("$and", expressions.map{ xe ⇒ xe.result() } : _*)
    e
  }
}

object $not extends BooleanExpression {
  def apply(expr : Expression with BooleanExpression) : BooleanExpression = {
    val e = new BooleanExpression
    e.putPrefix(ObjectCode, "$not")
    e.obj(expr.result())
    e
  }
}

object Expression {
  implicit def conditionalExpression(fieldName : String) : BooleanFieldExpression = new BooleanFieldExpression(fieldName)
  implicit def stringLiteral(str : String) : StringLiteral = new StringLiteral(str)
  implicit def intLiteral(int : Int) : IntLiteral = new IntLiteral(int)
  implicit def doubleLiteral(dbl : Double) : DoubleLiteral = new DoubleLiteral(dbl)
  implicit def logicalExpression(exp1 : BooleanExpression) : LogicalExpression = new LogicalExpression(exp1)
}
