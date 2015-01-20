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

import sbt._
import sbt.Keys._
import scala.language.postfixOps

object Dependencies {

  val resolvers = Seq(
    "Typesafe repository snapshots" at "http://repo.typesafe.com/typesafe/snapshots/",
    "Typesafe repository releases" at "http://repo.typesafe.com/typesafe/releases/"
  )

  val akka_actor      = "com.typesafe.akka"   %% "akka-actor"                 % "2.3.8"
  val akka_streams    = "com.typesafe.akka"   %% "akka-stream-experimental"   % "1.0-M2"
  val grizzled_slf4j  = "org.clapper"         %% "grizzled-slf4j"             % "1.0.2"
  val logback_classic = "ch.qos.logback"       % "logback-classic"            % "1.1.2"      % "test"
  val specs           = "org.specs2"          %% "specs2-core"                % "2.3.11"     % "test"

  val common = Seq(specs, grizzled_slf4j, logback_classic)

  val bson = common ++ Seq( akka_actor )

  val client = common ++ Seq( akka_streams, akka_actor )
}
