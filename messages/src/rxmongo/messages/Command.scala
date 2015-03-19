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

import rxmongo.bson._

/** Generic command issued on a database
  * Commands are basically query commands issued to the "\$cmd" collection. The selector of the query forms the
  * content of the command and its parameters. Subclasses of this class form all the standard commands that
  * MongoDB knows to process from a driver.
  * @param db The name of the database towards which the command should be directed.
  * @param selector The content of the command.
  */
class Command(
  db : String,
  val selector : BSONObject) extends GenericQueryMessage {
  val fullCollectionName = s"$db.$$cmd"
  val options = QueryOptions.default
  val returnFieldsSelector : Option[BSONObject] = None
  override def appendTo(builder : StringBuilder) : StringBuilder = {
    super.appendTo(builder).
      append(",db=").append(db).
      append(",options=").append(options.withNumberToReturn(-1)).
      append(",selector=").append(selector).
      append(",returnFieldsSelector=").append(returnFieldsSelector)
  }
}

/** An Administrative Command
  * This is like a generic command but it is always directed towards the database named "admin".
  * @param query
  */
class AdminCommand(query : BSONObject) extends Command("admin", query)
