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

import java.util.concurrent.atomic.AtomicLong

import akka.actor.{ ActorRef, ActorSystem }
import com.typesafe.config.Config

import scala.concurrent.duration._
import scala.util.{ Failure, Success }

/** RxMongo Driver
  *
  * This provides the driver for interacting with a single Mongo ReplicaSet. A given client can instantiate multiple
  * Drivers to
  * @param config
  */
class Driver(config : Option[Config] = None) {

  // RxMongo's ActorSystem is configured here. We use a separate one so it doesn't interfere with the application.
  val system = {
    import com.typesafe.config.ConfigFactory
    val cfg = config match { case Some(c) ⇒ c; case None ⇒ ConfigFactory.load() }
    ActorSystem("RxMongo", cfg.getConfig("rxmongo.akka"))
  }

  private val supervisorActor = system.actorOf(Supervisor.props(this), "Supervisor")

  def close(timeout : FiniteDuration = 0.seconds) = {
    // Tell the supervisor to close. It will shut down all the connections and then shut down the ActorSystem
    // as it is exiting.
    supervisorActor ! Supervisor.Terminate

    // When the actorSystem is shutdown, it means that supervisorActor has exited (run its postStop)
    // So, wait for that event.
    system.awaitTermination(timeout)
  }

  /** Creates a new MongoConnection from URI.
    *
    * @see [[http://docs.mongodb.org/manual/reference/connection-string/ the MongoDB URI documentation]]
    *
    * @param uri The MongoURI object that contains the connection details
    */
  def connect(uri : MongoURI, name : Option[String] = None) : ActorRef = {
    val props = Connection.props(uri)
    name match {
      case Some(nm) ⇒ system.actorOf(props, nm);
      case None     ⇒ system.actorOf(props, "Connection-" + Driver.nextCounter)
    }
  }

  /** Connect From A URI String
    *
    * Parses the URI string and uses it to establish a connection to Mongo
    *
    * @return Connection if successful
    *
    * @see [[http://docs.mongodb.org/manual/reference/connection-string/ the MongoDB URI documentation]]
    *
    * @param uri A string that will be parsed with [[rxmongo.driver.MongoURI.parse]]
    */
  def connect(uri : String) : ActorRef = {
    MongoURI.parse(uri) match {
      case Success(mongoURI) ⇒ connect(mongoURI)
      case Failure(xcptn)    ⇒ throw xcptn
    }
  }

}

object Driver {

  /** Creates a new MongoDriver with a new ActorSystem. */
  def apply() = new Driver

  def apply(config : Config) = new Driver(Some(config))

  private[driver] val _counter = new AtomicLong(0)
  private[driver] def nextCounter : Long = _counter.incrementAndGet()
}

case class RxMongoError(message : String, cause : Option[Throwable] = None) extends Exception {
  override def getMessage = message
  override def getCause = cause.orNull
}
