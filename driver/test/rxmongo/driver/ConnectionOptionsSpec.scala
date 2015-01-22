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

import org.specs2.mutable.Specification

class ConnectionOptionsSpec extends Specification {

  "WriteConcern" should {
    "yield NoAcknowledgement for -1" in {
      WriteConcern("-1") must beEqualTo(NoAcknowledgmentWC)
    }
    "yield ErrorsOnly for 0" in {
      WriteConcern(" 0") must beEqualTo(ErrorsOnlyWC)
    }
    "yield BasicAcknowledgment for 1" in {
      WriteConcern("1 ") must beEqualTo(BasicAcknowledgmentWC)
    }
    "yield WaitForMembers for > 1" in {
      WriteConcern(" 2 ") must beEqualTo(WaitForMembersWC(2))
    }
    "yield Majority for 'majority'" in {
      WriteConcern("majority") must beEqualTo(MajorityWC)
    }
    "yield MembersWithTag for other strings" in {
      WriteConcern(" foo ") must beEqualTo(MembersWithTagWC("foo"))
    }
    "yield BasicAck for empty string" in {
      WriteConcern("") must beEqualTo(BasicAcknowledgmentWC)
    }
  }

}
