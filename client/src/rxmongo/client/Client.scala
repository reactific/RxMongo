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

import akka.actor.ActorRef
import akka.util.Timeout
import com.typesafe.config.Config

import rxmongo.driver.{ Driver, MongoURI }

import scala.concurrent.Await
import scala.util.{ Failure, Success }

/** Primary Application Interface To RxMongo
  *
  * Each Client instance provides a separate interface to MongoDB. It can manage connections to multiple
  * replica sets and coordinate the activity between them. Your application must instantiate an RxMongoClient in order
  * to use RxMongo as all other object provided by RxMongo are accessed through this. You may instantiate multiple
  * RxMongoClient instances, but each will maintain its own set of connections and view of the databases. This may be
  * useful in testing but is unlikely to be useful in an application unless you wish to keep replica set traffic very
  * separate.
  */
case class Client(uri : MongoURI, config : Option[Config], name : String) {
  private[client] val driver = Driver(config, name)

  private implicit val connectionTimeout : Timeout = Driver.defaultTimeout

  private[client] val connection : ActorRef = {
    Await.result(driver.connect(uri, None)(connectionTimeout), connectionTimeout.duration)
  }

  def database(name : String) : Database = Database(name, this)
}

object Client {

  def apply(uri : String, config : Option[Config] = None, name : String = "RxMongo") = {
    MongoURI(uri) match {
      case Success(u) ⇒ new Client(u, config, name)
      case Failure(x) ⇒ throw x
    }
  }
}
