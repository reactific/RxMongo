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

import java.io.Closeable

import akka.actor.ActorRef
import akka.util.Timeout
import com.typesafe.config.Config

import rxmongo.driver.{ Driver, MongoURI }
import rxmongo.messages.{WriteConcern, Query}

import scala.concurrent.Await
import scala.util.{ Failure, Success }

/** Primary Application Interface To RxMongo
  *
  * Each Client instance provides a separate interface to MongoDB. It can manage connections to multiple
  * replica sets and coordinate the activity between them. Your application must instantiate an Client in order
  * to use RxMongo as all other objects provided by RxMongo are accessed through this one. You may instantiate multiple
  * Client instances, but each will maintain its own set of connections and view of the databases. This may be
  * useful in testing but is unlikely to be useful in an application unless you wish to keep replica set traffic
  * separate.
  *
  * The Client is a wrapper around the [[rxmongo.driver.Driver]] object that allow you to avoid the low level
  * interface of the Driver. Instead of dealing directly with connections to a MongoDB server, you deal with
  * abstractions like [[rxmongo.client.Database]], [[rxmongo.client.Collection]], [[Query]] and
  * [[rxmongo.client.Cursor]].
  */
case class Client(uri : MongoURI, config : Option[Config], name : String)(override implicit val timeout : Timeout, override implicit val writeConcern : WriteConcern)
  extends RxMongoComponent(Driver(config, name)) with Closeable {

  private[client] val connection : ActorRef = {
    Await.result(driver.connect(uri, None)(timeout), timeout.duration)
  }

  def close() = {
    driver.close()
  }

  /** Get a Database Object.
    *
    * Note that instantiation of this does not imply creation of the database. Databases are created on demand by
    * MongoDB. So, you can create numerous instances of Database without interacting with MongoDB at all. Once you do
    * something with the returned Database instance, however, you will be interacting with MongoDB.
    * @param name The name of the database
    * @return A lightweight interface to a MongoDB Database.
    */
  def database(name : String)(implicit to : Timeout = timeout, wc : WriteConcern = writeConcern) : Database = {
    Database(name, this)(to, wc)
  }

  /** Get a Collection Object.
    *
    * Note that instantiona of this does not imply creation of a collection. Collections are created on demand by
    * MongoDB. So, you can create numerous instances of Collection through this method without any interactions with
    * MongoDB at all. Once you do something with the returned Collection instance, however, you will be interacting
    * with MongoDB.
    * @param dbName The name of the database
    * @param collName The name of the collection
    * @return
    */
  def collection(dbName : String, collName : String)(implicit to : Timeout = timeout, wc : WriteConcern = writeConcern) : Collection = {
    Database(dbName, this)(to, wc).collection(collName)(to, wc)
  }

}

object Client {

  /** Client constructor.
    * Create a Client from a URI string, configuration options, and a name. The string uri is parsed into a MongoURI
    * and then the arguments are passed to the Client class's constructor. If the uri cannot be parsed then an
    * exception will be thrown
    * @param uri The uri specifying what to connect to and how
    * @param config rxmongo configuration options
    * @param name The name for this client
    * @return An instance of [[rxmongo.client.Client]] for interacting with MongoDB
    */
  def apply(uri : String, config : Option[Config] = None, name : String = "RxMongo")(implicit to : Timeout = Driver.defaultTimeout, wc : WriteConcern = WriteConcern.default) = {
    MongoURI(uri) match {
      case Success(u) ⇒ new Client(u, config, name)(to, wc)
      case Failure(x) ⇒ throw x
    }
  }
}
