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

package rxmongo.client

import akka.pattern.ask
import akka.util.Timeout

import rxmongo.bson.{ RxMongoError, BSONObject }
import rxmongo.driver.{ ReplyMessage, Driver, QueryMessage }

import scala.concurrent.{ ExecutionContext, Future }

case class Collection(name : String, db : Database) {

  implicit val ec : ExecutionContext = db.client.driver.executionContext
  def namespace = db.namespace + "." + name

  def query(selector : (String, Any)*)(implicit to : Timeout = Driver.defaultTimeout) : Future[Seq[BSONObject]] = {
    val obj = BSONObject(selector : _*)
    val msg = QueryMessage(namespace, numberToSkip = 0, numberToReturn = 1, obj)
    db.client.connection.ask(msg) map {
      case reply : ReplyMessage ⇒
        if (reply.QueryFailure)
          throw new RxMongoError("Query Failure")
        else
          reply.documents.toSeq
    }
  }
}
