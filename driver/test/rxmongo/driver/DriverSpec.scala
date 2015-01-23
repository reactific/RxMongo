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

import akka.actor.ActorRef
import org.specs2.execute.Result
import org.specs2.mutable.Specification

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class DriverSpec extends Specification {

  def mongoTest(f : () ⇒ Result) : Result = {
    if (Helper.haveLocalMongo) {
      f()
    } else {
      skipped(": no local mongo")
    }
  }

  "Driver" should {
    "mind its lifecycle" in {
      val driver = Driver(None, Some("One"))
      driver.close(Duration(1, "seconds"))
      driver.system.isTerminated must beTrue
    }

    "make a simple connection" in mongoTest { () ⇒
      val driver = Driver(None, Some("Thing"))
      val future = driver.connect("mongodb://localhost/", Some("TestConnection"))
      val conn = Await.result(future, Duration(1, "s"))
      conn.isInstanceOf[ActorRef] must beTrue
      driver.close(Duration(500, "ms"))
      success
    }
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
