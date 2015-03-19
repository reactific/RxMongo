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

import java.net.{ Socket, InetSocketAddress, InetAddress }
import javax.net.SocketFactory

import akka.actor.{ ActorSystem, ActorRef }
import akka.pattern.ask

import org.specs2.execute.Result
import rxmongo.bson.BSONObject
import rxmongo.messages.{ReplyMessage, QueryMessage}

import scala.concurrent.{ Await }
import scala.concurrent.duration._

class DriverSpec extends AkkaTest(ActorSystem("DriverSpec")) {

  implicit val timeout = Driver.defaultTimeout

  def mongoTest(f : () ⇒ Result) : Result = {
    if (Helper.haveLocalMongo) {
      f()
    } else {
      skipped(": no local mongo")
    }
  }

  sequential
  "Driver" should {
    /*
        "mind its lifecycle" in {
          val driver = Driver(None, "Lifecycle")
          driver.close(1.second)
          driver.isClosed must beTrue
        }

        "make a simple connection" in mongoTest { () ⇒
          val driver = Driver(None, "Simple")
          val future = driver.connect("mongodb://localhost/")
          val conn = Await.result(future, 1.seconds)
          conn.isInstanceOf[ActorRef] must beTrue
          driver.close(500.millis)
          success
        }
    */

    "send an innocuous query" in mongoTest { () ⇒
      val driver = Driver(None, "Innocuous")
      val future = driver.connect("mongodb://localhost/")
      val conn = Await.result(future, Duration(1, "s"))
      conn.isInstanceOf[ActorRef] must beTrue
      val c = conn.asInstanceOf[ActorRef]
      val msg = QueryMessage("rxmongo.test", BSONObject("foo" -> 1))
      val future2 = c.ask(msg)
      val x = Await.result(future2, 1.seconds)
      driver.close(500.millis)
      x.isInstanceOf[ReplyMessage] must beTrue
    }
    /*
    "handle a CheckReplicaSet" in mongoTest { () ⇒
      val driver = Driver(None, "Innocuous")
      val future = driver.connect("mongodb://localhost/") map { conn : ActorRef ⇒
        conn ! Connection.CheckReplicaSet
      }
      Await.result(future, Duration(1, "s"))
      success
    }

    "return 0 for numConnections when first established" in {
      val driver = Driver(None, "numConnections=0")
      Await.result(driver.numConnections, 1.seconds) must beEqualTo(0)
    }

    "return 0 before a connection is established, 1 afterwards" in mongoTest { () ⇒
      val driver = Driver(None, "ConnectionCount")
      val before = driver.numConnections
      val future = driver.connect("mongodb://localhost/")
      val conn = Await.result(future, 1.second)
      conn.isInstanceOf[ActorRef] must beTrue
      val after = driver.numConnections
      val result = Await.result(Future.sequence(Seq(before, after)), 1.second)
      result must beEqualTo(Seq(0, 1))
      driver.close(1.second)
      success
    }
  */
  }
}

object Helper {

  lazy val haveLocalMongo : Boolean = {
    val addr = InetAddress.getLoopbackAddress
    val port = 27017
    val socketAddress = new InetSocketAddress(addr, port)
    try {
      val socket : Socket = SocketFactory.getDefault.createSocket
      socket.connect(socketAddress, 1000)
      socket.close()
      true
    } catch {
      case x : Throwable ⇒ false
    }
  }
}
