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

package rxmongo.examples

import java.util.concurrent.TimeUnit

import akka.pattern.ask
import akka.util.Timeout
import rxmongo.driver.Connection.CheckReplicaSet

import rxmongo.driver.{ ReplyMessage, DBStatsCmd, Driver }

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.StdIn
import scala.util.{ Failure, Success, Try }

/** Low Level Example RxMongo Program
  *
  * This program shows how to interact with Mongo replica sets and send other low-level commands to Mongo. To run this
  * simple program, pass your mongo URL as the argument to the program. It will connect, if possible, and give you
  * some low level options to choose from. These can then be used to look at things on your replica set via RxMongo.
  */
object LowLevel extends App {

  def getChoice : (Int, Seq[String]) = {
    println("\n1: Show DB Stats (dbName)")
    println("\n2: Check Replica Set ()")
    val line = StdIn.readLine("Your choice: ")
    if (line == null)
      return -1 -> Seq.empty
    Try {
      val parts = line.trim.split(" ")
      parts.head.toInt -> parts.tail
    } match {
      case Success((cmd : Int, args : Array[String])) ⇒ cmd -> args
      case Failure(x) ⇒ println("Error: " + x.getClass + ": " + x.getMessage); getChoice
    }
  }

  implicit val timeout : Timeout = Timeout(2, TimeUnit.SECONDS)
  val driver = Driver()
  val url = args.mkString("")
  val connection = Await.result(driver.connect(url), 1000.milliseconds)

  println("Connected to: " + url)

  var shouldExit : Boolean = false

  while (!shouldExit) {
    val (choice, options) = getChoice
    choice match {
      case 1  ⇒ showDBStats(options)
      case 2  ⇒ checkReplicaSet(options)
      case -1 ⇒ shouldExit = true
      case _  ⇒ println("Invalid choice, try again.")
    }
  }

  System.exit(0)

  def showDBStats(options : Seq[String]) : Unit = {
    if (options.size != 1)
      println("You must specify exactly one database name as an argument to getDBStats")
    else {
      println(s"getDBStats ${options(0)}")
      val result = (connection ? DBStatsCmd(options(0))).mapTo[ReplyMessage]
      result.onComplete {
        case (msg) ⇒ msg match {
          case Success(reply) ⇒ println("\n" + reply.toString)
          case Failure(xcptn) ⇒ println("\nFailed: " + xcptn.getClass.getName + ": " + xcptn.getMessage)
        }
      }
    }
  }

  def checkReplicaSet(options : Seq[String]) : Unit = {
    if (options.size != 0)
      println("checkReplicaSet does not require options")
    else {
      println("checkReplicaSet")
      connection ! CheckReplicaSet
    }
  }
}
