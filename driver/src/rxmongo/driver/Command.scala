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

package rxmongo.driver

import rxmongo.bson.BSONObject

class Command(db : String, val selector : BSONObject) extends GenericQueryMessage {
  val fullCollectionName = s"$db.$$cmd"
  val options = QueryOptions.default
  val returnFieldsSelector : Option[BSONObject] = None
}

class AdminCommand(query : BSONObject) extends Command("admin", query)

case class IsMasterCmd() extends Command("admin", BSONObject("isMaster" → 1))
case class ServerStatus() extends Command("admin", BSONObject("serverStatus" → 1))
case class GetLastErrorCmd(db : String) extends Command(db, BSONObject("getLastError" → 1))
case class DBStatsCmd(db : String) extends Command(db, BSONObject("dbStats" → 1))
case class CollStatsCmd(db : String, collection : String, scale : Int = 1024, verbose : Boolean = true)
  extends Command(db, BSONObject("collStats" → collection, "scale" → scale, "verbose" → verbose))
