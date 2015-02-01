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

import rxmongo.driver._

import scala.collection.mutable
import scala.concurrent.{ Future, Await }
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
    println("\n1: Print Results (cmd #...)")
    println("2: Show DB Stats (dbName)")
    println("3: Check Replica Set ()")
    println("4: Show Server Status ()")
    println("5: Show Collection Status (db, collection)")
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
  var cmdId : Int = 0
  val cmdResults = mutable.Map.empty[Int, Future[String]]

  while (!shouldExit) {
    val (choice, options) = getChoice
    cmdId += 1
    Try {
      choice match {
        case 1  ⇒ printResults(options)
        case 2  ⇒ showDBStats(options)
        case 3  ⇒ checkReplicaSet(options)
        case 4  ⇒ showServerStatus(options)
        case 5  ⇒ showCollStats(options)
        case -1 ⇒ shouldExit = true
        case _  ⇒ println("Invalid choice, try again.")
      }
    } match {
      case Success(x) ⇒ //nothing to do
      case Failure(x) ⇒
        println(s"Error during command processing: ${x.getClass.getName}: ${x.getMessage}")
    }
  }

  System.exit(0)

  def printResults(options : Seq[String]) : Unit = {
    for (option ← options) {
      val id = option.toInt
      cmdResults.get(id) match {
        case Some(result) ⇒
          result.value match {
            case Some(Success(msg)) ⇒
              println(s"#$id: Succeeded: $msg")
            case Some(Failure(xcptn)) ⇒
              println(s"#$id: Failed: " + xcptn.getClass.getName + ": " + xcptn.getMessage)
            case None ⇒
              println(s"#$id: Incomplete")
          }
        case None ⇒
          println(s"#$id: Not found")
      }
    }
  }

  def showDBStats(options : Seq[String]) : Unit = {
    if (options.size != 1)
      println("You must specify exactly one database name as an argument to getDBStats")
    else {
      val result = (connection ? DBStatsCmd(options(0))).mapTo[ReplyMessage] map { reply ⇒
        reply.toString()
      }
      cmdResults.put(cmdId, result)
      println(s"Command #$cmdId (getDBStats ${options(0)}) issued.")
    }
  }

  def checkReplicaSet(options : Seq[String]) : Unit = {
    if (options.size != 0)
      println("checkReplicaSet does not require options")
    else {
      val result = (connection ? CheckReplicaSet).mapTo[IsMasterReply] map { reply ⇒
        reply.toString
      }
      cmdResults.put(cmdId, result)
      println(s"Command #$cmdId (checkReplicaSet) issued.")
    }
  }

  def showServerStatus(options : Seq[String]) : Unit = {
    if (options.size != 0)
      println("showServerStatus does not require options")
    else {
      val result = (connection ? ServerStatus()).mapTo[ReplyMessage] map { reply ⇒
        { for (doc ← reply.documents) yield { doc.toString() } }.mkString(", ")
      }
      cmdResults.put(cmdId, result)
      println(s"Command #$cmdId (showServerStatus) issued.")
    }
  }

  def showCollStats(options : Seq[String]) : Unit = {
    if (options.size != 2)
      println("showCollStats requires two arguments: database, collection")
    else {
      val result = (connection ? CollStatsCmd(options(0), options(1))).mapTo[ReplyMessage] map { reply ⇒
        { for (doc ← reply.documents) yield { doc.toString() } }.mkString(", ")
      }
      cmdResults.put(cmdId, result)
      println(s"Command #$cmdId (showCollStats) issued.")
    }
  }
}
