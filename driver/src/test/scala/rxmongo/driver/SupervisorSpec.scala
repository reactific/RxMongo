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

import akka.actor._
import akka.pattern.ask
import akka.testkit.TestProbe
import rxmongo.driver.Supervisor.NumConnectionsReply

import scala.concurrent.Await
import scala.concurrent.duration._

class SupervisorSpec extends AkkaTest(ActorSystem("SupervisorTest")) {

  sequential

  "Supervisor" should {
    "handle Shutdown" in {
      val s = system.actorOf(Supervisor.props())
      s ! Supervisor.Shutdown
      val probe = TestProbe()
      probe watch s
      within(500.millis) {
        probe.expectTerminated(s)
      }
      success
    }

    "return 0 for NumChannels at startup" in {
      val s = system.actorOf(Supervisor.props())
      val future = s.ask(Supervisor.NumConnections)(500.millis)
      val reply = Await.result(future, 500.millis)
      reply must beEqualTo(NumConnectionsReply(0))
    }

    "add a connection" in {
      val s = system.actorOf(Supervisor.props())
      val uri = MongoURI("mongodb://localhost/").get
      s ! Supervisor.AddConnection(uri, "MyConnection")
      expectMsgType[ActorRef](750.millis)
      success
    }
  }
}
