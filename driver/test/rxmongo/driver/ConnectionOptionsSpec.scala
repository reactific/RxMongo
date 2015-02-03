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

import java.util.concurrent.TimeUnit

import org.specs2.mutable.Specification

import scala.concurrent.duration.{Duration, FiniteDuration}

class ConnectionOptionsSpec extends Specification {

  "WriteConcern" should {
    "yield NoAcknowledgement for -1" in {
      WriteConcern("-1") must beEqualTo(WriteConcern(NoAcknowledgmentWC))
    }
    "yield ErrorsOnly for 0" in {
      WriteConcern(" 0") must beEqualTo(WriteConcern(ErrorsOnlyWC))
    }
    "yield BasicAcknowledgment for 1" in {
      WriteConcern("1 ") must beEqualTo(WriteConcern(BasicAcknowledgmentWC))
    }
    "yield WaitForMembers for > 1" in {
      WriteConcern(" 2 ") must beEqualTo(WriteConcern(WaitForMembersWC(2)))
    }
    "yield Majority for 'majority'" in {
      WriteConcern("majority") must beEqualTo(WriteConcern(MajorityWC))
    }
    "yield MembersWithTag for other strings" in {
      WriteConcern(" foo ") must beEqualTo(WriteConcern(MembersWithTagWC("foo")))
    }
    "yield BasicAck for empty string" in {
      WriteConcern("") must beEqualTo(WriteConcern(BasicAcknowledgmentWC))
    }
  }

  "ConnectionOptions" should {
    "validate connectTimeoutMS" in {
      ConnectionOptions(connectTimeoutMS = 3700000).validate should throwA[IllegalArgumentException]
    }
    "validate socketTimeoutMS" in {
      ConnectionOptions(socketTimeoutMS = -1).validate should throwA[IllegalArgumentException]
    }
    "validate maxPoolSize" in {
      ConnectionOptions(maxPoolSize = 0).validate should throwA[IllegalArgumentException]
    }
    "validate minPoolSize" in {
      ConnectionOptions(minPoolSize = 0).validate should throwA[IllegalArgumentException]
    }
    "validate maxPoolSize >= minPoolSize" in {
      ConnectionOptions(maxPoolSize = 9, minPoolSize = 10).validate should throwA[IllegalArgumentException]
    }
    "validate maxIdleTimeMS" in {
      ConnectionOptions(maxIdleTimeMS = 24 * 3600 * 1000 + 1).validate should throwA[IllegalArgumentException]
    }
    "validate wtimeoutMS" in {
      ConnectionOptions(writeConcern =
        WriteConcern(BasicAcknowledgmentWC, timeout = Duration(-1, TimeUnit.MILLISECONDS))).validate should
        throwA[IllegalArgumentException]
    }
    "validate rampupRate" in {
      ConnectionOptions(rampupRate = 2).validate should throwA[IllegalArgumentException]
    }
    "validate backoffThreshold" in {
      ConnectionOptions(backoffThreshold = 0.0).validate should throwA[IllegalArgumentException]
    }
    "validate backoffRate" in {
      ConnectionOptions(backoffRate = 0).validate should throwA[IllegalArgumentException]
    }
    "validate messagesPerResize" in {
      ConnectionOptions(messagesPerResize = -1).validate should throwA[IllegalArgumentException]
    }
    "validate channelReconnectPeriod" in {
      ConnectionOptions(channelReconnectPeriod = FiniteDuration(-1, "s")).validate should throwA[IllegalArgumentException]
    }
  }
}
