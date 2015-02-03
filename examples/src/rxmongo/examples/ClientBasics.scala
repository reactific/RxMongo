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

import rxmongo.bson.{ Query, BSONObject }
import rxmongo.client.Client
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

/** Basic Client Usage Example
  *
  * This example shows how to use the Client interface to create the client [1], obtain a database from the client [2],
  * obtain a collection from the database [3], query the collection [4], obtain the results [5], and handle query
  * errors [6].
  */
object ClientBasics extends App {

  // [1] Make a connection to the mydb database on the local host
  val client = Client("mongodb://localhost/mydb")

  try {

    // [2] Obtain a database from the client
    val db = client.database("mydb")

    // [3] Obtain a collection from the database
    val coll = db.collection("mycoll")

    // [4] Obtain a cursor for finding the documents with field name set to "foo"
    val cursor = coll.find(Query("name" -> "foo"))

    // [5] When the results are ready, open them
    val future = cursor.map { crsr ⇒
      // [6] For each result
      while (crsr.hasNext) {
        crsr.next.map { results : BSONObject ⇒
          // [7] We got an answer to our query, print the results
          println("Results: " + results)
        } recover {
          case xcptn : Throwable ⇒
            // [8] We got an error in our query, print the error message
            println("Error in query: " + xcptn)
        }
      }
    }

    // [9] Wait for all the asynchronous processing to complete, within 5 seconds.
    Await.result(future, 5.seconds)
  } finally {
    // [10] Close the client and its driver, releasing background threads
    client.close()
  }
  // [11] Terminate the program.
  println(s"Finished.")
}
