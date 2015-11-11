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

package rxmongo.examples

import akka.actor.ActorRef
import akka.pattern.ask

import com.typesafe.config.ConfigFactory

import rxmongo.driver._
import rxmongo.bson._
import rxmongo.messages.{ ReplyMessage, QueryMessage }

import scala.concurrent.ExecutionContext.Implicits.global

/** Basic Driver Example
  *
  * This example shows how to instantiate the driver[1], connect to a MongoDB instance using a
  * Mongodb URL [2], create a query message [3], send a query and get its response in a non-blocking
  * fashion [4], and decode the reply from MongoDB [5].
  */
object DriverBasics extends App {

  // You can specify your own timeout for asynchronous calls instead of using this default value from Driver.
  implicit val timeout = Driver.defaultTimeout

  // [1] Instantiate with a specific configuration and a specific name for the driver
  val driver = Driver(ConfigFactory.load("rxmongo"), "MyDriver")

  // [2] Make a connection to database "mydb" on the local host at the default port and set the
  // maxPoolSize configuration value to 19 so at most 19 channels to the server will be open. This
  // will yield a Future[ActorRef] in "conn". If the connection succeeds, conn will have a value
  // that can be used to talk to the members of a MongoDB replica set.
  val conn = driver.connect("mongodb://localhost/mydb?maxPoolSize=19")

  // [3] Create a query message to find the documents with the name "foo" in the mydb.mycoll namespace.
  // You can also create all of the other kinds of messages that MongoDB supports.
  val msg = QueryMessage("mydb.mycoll", BSONObject("name" -> "foo"))

  // [4] When the connection is successful, extract the ActorRef and send it the query using the ask pattern
  // which will asynchronously deliver a ReplyMessage when the response comes back from MongoDB.
  conn.map {
    case connection : ActorRef ⇒
      connection.ask(msg).map {
        case reply : ReplyMessage ⇒
          // [5] We got a reply from MongoDB, now we need to decipher it. The numberReturned field tells us how
          // many documents were returned by the query. If we got at least one, just print out the head document.
          if (reply.numberReturned > 0) {
            println("Document returned: " + reply.documents.head)
          } else if (reply.QueryFailure) {
            // In this case, there was something wrong with the query.
            println("Query Failed")
          } else {
            // Chances are if you run this program, you will get this output because you don't have a
            // database named "mydb.mycoll" and if you do, it probably doesn't have a document whose
            // name field is "foo".
            println("No results: " + reply)
          }
      } recover {
        case xcptn : Throwable ⇒
          // If the query fails for any reason, you can recover from it here.
          println("Error from MongoDB: " + xcptn)
      }
  } recover {
    case xcptn : Throwable ⇒
      // If the connection fails for any reason, you can recover from it here.
      println("Could not connect to MongoDB: " + xcptn)
  }
}
