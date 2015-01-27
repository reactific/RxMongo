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

import java.io.Closeable
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicLong

import akka.actor.{ Terminated, Inbox, ActorRef, ActorSystem }
import akka.event.{ Logging, LoggingAdapter }
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.config.{ ConfigFactory, Config }
import rxmongo.driver.Supervisor.AddConnection

import scala.concurrent.{ Await, ExecutionContext, Future }
import scala.concurrent.duration._
import scala.util.{ Failure, Success }

/** RxMongo Driver
  *
  * This provides the driver for interacting with any group of Replica Sets. A given client can instantiate multiple
  * Drivers or (typically) just use a single instance. The Driver manages, through its Supervisor, a set of Connection
  * actors. A Connection here is not the same as a TCP network connection to a single MongoD or MongoS. In RxMongo, a
  * Connection is an actor that actively stays connected to a replica set, according to its configured values, even
  * through failover and failback. The user may connect in this way to as many replica sets as is needed.
  *
  * The Driver provides a very low level interface to MongoDB. It allows messages to be sent to a replica set and
  * responses returned. No higher level facilities for making interaction with the driver are provided. For that,
  * use the [[rxmongo.client.RxMongoClient]] class. To send messages to a replica set, you must send requests to
  * the Connection actor returned by the connect() methods.
  *
  * Each Driver contains an Akka ActorSystem that is used to manage the concurrent non-blocking communication with
  * MongoDB. Since a Driver is a [[java.io.Closeable]], it is important to `close()` the driver when you're done with
  * it so the Akka resources are released.
  *
  * @see [[rxmongo.driver.Connection]]
  * @param cfg An optional configuration with which to configure the RxMongo driver. It defaults to the configuration
  * included with the RxMongo library as a resource (rxmongo.conf)
  * @param name An optional name for this instance of the Driver. In situations where more than one driver is
  * instantiated, this may become necessary in order to distinguish the various driver's actors by name.
  */
case class Driver(cfg : Option[Config] = None, name : String = "RxMongo") extends Closeable {

  val config = cfg match { case Some(c) ⇒ c; case None ⇒ ConfigFactory.load("rxmongo") }

  // RxMongo's ActorSystem is configured here. We use a separate one so it doesn't interfere with the application.
  val system = ActorSystem("RxMongo", config.getConfig("rxmongo"))

  lazy val log : LoggingAdapter = Logging.getLogger(system, this.getClass)

  implicit val executionContext : ExecutionContext = system.dispatcher

  private var supervisorActor = system.actorOf(Supervisor.props(), Driver.actorName(name + "-Supervisor"))

  private val supervisorInbox = Inbox.create(system)

  supervisorInbox.watch(supervisorActor)

  private def supervisorHasTerminated(timeout : FiniteDuration = 0.seconds) = {
    try {
      supervisorInbox.receive(timeout) match {
        case Terminated(actor) ⇒ {
          if (actor == supervisorActor) {
            supervisorActor = null
            true
          } else {
            false
          }
        }
        case _ ⇒ false
      }
    } catch {
      case x : Throwable ⇒ false
    }
  }

  def close() : Unit = close(0.seconds)

  def close(timeout : FiniteDuration) = {
    // Tell the supervisor to close. It will shut down all the connections and then shut down the ActorSystem
    // as it is exiting.
    if (supervisorActor != null && !supervisorHasTerminated()) {
      supervisorActor ! Supervisor.Shutdown

      // Wait for the supervisor to shut down the RxMongo actors in an orderly fashion, hopefully within the callers
      // timeout. If not, we just force the ActorSystem to shut down. Either way, after this, the ActorSystem is down.
      if (!supervisorHasTerminated(timeout)) {
        log.warning(s"RxMongo Supervisor failed to terminate within $timeout, forcing ActorSystem shutdown")
      }
    }

    // Terminate the actor system
    system.terminate()

    // When the actorSystem is shutdown, it means that supervisorActor has exited (run its postStop). Even if that
    // hasn't happened, the shutdown() method is asynchronous and this close method is synchronous so make sure
    // we wait for the system to be actually down.
    Await.result(system.whenTerminated, timeout.toMillis match { case 0 ⇒ Duration.Inf; case _ ⇒ timeout })
    log.debug("RxMongo Closed")
  }

  def isClosed = supervisorActor == null || supervisorHasTerminated()

  /** Creates a new MongoConnection from URI.
    *
    * @see [[http://docs.mongodb.org/manual/reference/connection-string/ the MongoDB URI documentation]]
    *
    * @param uri The MongoURI object that contains the connection details
    */
  def connect(uri : MongoURI, name : Option[String])(implicit timeout : Timeout) : Future[ActorRef] = {
    require(!isClosed, "Cannot connect with closed RxMongo Driver")

    val final_name = name match {
      case Some(nm) ⇒ nm
      case None     ⇒ Driver.actorName("Connection-")
    }
    (supervisorActor ? AddConnection(uri, final_name)).map { x ⇒
      log.debug(s"Connection to '$final_name' acquired actor $x")
      x.asInstanceOf[ActorRef]
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
    * @param uri A string that will be parsed with [[rxmongo.driver.MongoURI.apply]]
    */
  def connect(uri : String, name : Option[String] = None)(implicit timeout : Timeout = Driver.defaultTimeout) : Future[ActorRef] = {
    MongoURI(uri) match {
      case Success(mongoURI) ⇒ connect(mongoURI, name)
      case Failure(xcptn)    ⇒ throw xcptn
    }
  }
}

object Driver {

  implicit val defaultTimeout : Timeout = Timeout(10000, TimeUnit.MILLISECONDS)

  /** Creates a new MongoDriver with a new ActorSystem. */
  def apply() = new Driver

  def apply(config : Config) = new Driver(Some(config))

  def apply(config : Config, name : String) = new Driver(Some(config), name)

  private[driver] val _counter = new AtomicLong(0)
  private[driver] def nextCounter : Long = _counter.incrementAndGet()
  /** A method to generate a unique name for each actor that meets Akka's naming requirements.
    *
    * @param name The name of the type of the actor
    * @return The unique name generated for the actor instance
    */
  private[driver] def actorName(name : String) : String = {
    name.replaceAll("[^A-Za-z0-9]", "-") + "-" + nextCounter
  }
}

