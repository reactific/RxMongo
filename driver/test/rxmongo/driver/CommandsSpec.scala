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

import rxmongo.bson._
import rxmongo.driver.cmds.FindAndModifyCmd

class CommandsSpec extends Specification {

  sequential

  "Commands" should {
    "print themselves out" in {
      val cmd = new Command("db", BSONObject("cmd" → 1))
      cmd.toString.matches("Command\\(opcode=OP_QUERY,requestId=\\d+,requiresResponse=true,db=db,options=QueryOptions\\(0,1,false,false,false,false,false,false\\),selector=\\{ cmd->1 \\},returnFieldsSelector=None\\)") must beTrue
    }
    "print out case class subclasses" in {
      val cmd = FindAndModifyCmd("db", "coll", Some(Query("a" $eq "b")), Seq("a" → true), None, Some(true))
      cmd.toString.matches("FindAndModifyCmd\\(opcode=OP_QUERY,requestId=\\d+,requiresResponse=true,db=db,options=QueryOptions\\(0,1,false,false,false,false,false,false\\),selector=\\{ findAndModify->coll, query->\\{ \\$query->\\{ a->b \\} \\}, remove->true \\},returnFieldsSelector=None\\)")
    }
  }
}
