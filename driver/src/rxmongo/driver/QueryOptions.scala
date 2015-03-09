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

import akka.util.{ ByteIterator, ByteStringBuilder }
import rxmongo.bson._

/** The Options For A Query
  *
  * @param numberToSkip The number of documents to skip. Sets the number of documents to omit - starting from the first
  * document in the resulting dataset - when returning the result of the query.
  * @param numberToReturn The number of documents to return in the first OP_REPLY batch. Limits the number of
  *         documents in the first OP_REPLY message to the query. However, the database will still
  *         establish a cursor and return the cursorID to the client if there are more results than
  *         numberToReturn. If the client driver offers ‘limit’ functionality (like the SQL LIMIT
  *         keyword), then it is up to the client driver to ensure that no more than the specified
  *         number of documents are returned to the calling application. If numberToReturn is 0, the
  *         db will use the default return size. If the number is negative, then the database will
  *         return that number and close the cursor. No further results for that query can be fetched.
  *         If numberToReturn is 1 the server will treat it as -1 (closing the cursor automatically).
  * @param tailableCursor Tailable means cursor is not closed when the last data is retrieved. Rather, the cursor
  *         marks the final object’s position. You can resume using the cursor later, from where it was
  *         located, if more data were received. Like any “latent cursor”, the cursor may become invalid
  *         at some point (CursorNotFound) – for example if the final object it references were deleted.
  * @param slaveOk Allow query of replica slave. Normally these return an error except for namespace “local”.
  * @param noCursorTimeout The server normally times out idle cursors after an inactivity period (10 minutes) to
  *          prevent excess memory use. Set this option to prevent that.
  * @param awaitData Use with TailableCursor. If we are at the end of the data, block for a while rather than
  *    returning no data. After a timeout period, we do return as normal.
  * @param exhaust Stream the data down full blast in multiple “more” packages, on the assumption that the client
  *  will fully read all data queried. Faster when you are pulling a lot of data and know you want to
  *  pull it all down. Note : the client is not allowed to not read all the data unless it closes
  *  the connection.
  * @param partial Get partial results from a mongos if some shards are down (instead of throwing an error)
  */
case class QueryOptions(
  numberToSkip : Int = 0,
  numberToReturn : Int = 1,
  tailableCursor : Boolean = false,
  slaveOk : Boolean = false,
  noCursorTimeout : Boolean = false,
  awaitData : Boolean = false,
  exhaust : Boolean = false,
  partial : Boolean = false) {

  def flags : Int = {
    0 |
      (if (tailableCursor) 2 else 0) |
      (if (slaveOk) 4 else 0) |
      // 8 (4th bit) is reserved for opLogReplay which is internal to MongoDB
      (if (noCursorTimeout) 16 else 0) |
      (if (awaitData) 32 else 0) |
      (if (exhaust) 64 else 0) |
      (if (partial) 128 else 0)
  }

  def withNumberToSkip(n : Int) = copy(numberToSkip = n)
  def withNumberToReturn(n : Int) = copy(numberToReturn = n)
  def withTailableCursor = copy(tailableCursor = true)
  def withSlaveOk = copy(slaveOk = true)
  def withNoCursorTimeout = copy(noCursorTimeout = true)
  def withAwaitData = copy(awaitData = true)
  def withExhaust = copy(exhaust = true)
  def withPartial = copy(partial = true)
}

object QueryOptions {
  val default = QueryOptions()

  def apply(skip : Int, ret : Int, flags : Int) : QueryOptions = {
    val tailableCursor = (flags & 2) != 0
    val slaveOk = (flags & 4) != 0
    val noCursorTimeout = (flags & 16) != 0
    val awaitData = (flags & 32) != 0
    val exhaust = (flags & 64) != 0
    val partial = (flags & 128) != 0
    QueryOptions(skip, ret, tailableCursor, slaveOk, noCursorTimeout, awaitData, exhaust, partial)
  }
}
