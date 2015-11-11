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
import scala.language.postfixOps

object Dependencies {

  // Akka Stuff
  val akkaV = "2.3.12"
  val akkaStreamsV = "2.0-M1"
  val akka_actor      = "com.typesafe.akka"   %% "akka-actor"                   % akkaV
  val akka_slf4j      = "com.typesafe.akka"   %% "akka-slf4j"                   % akkaV
  val akka_testkit    = "com.typesafe.akka"   %% "akka-testkit"                 % akkaV
  val akka_streams    = "com.typesafe.akka"   %% "akka-stream-experimental"     % akkaStreamsV
  val akka_http_core  = "com.typesafe.akka"   %% "akka-http-core-experimental"  % akkaStreamsV
  val akka_http       = "com.typesafe.akka"   %% "akka-http-experimental"       % akkaStreamsV
  val helpers         = "com.reactific"       %% "helpers"                      % "0.1.0"
  val hsp             = "com.reactific"       %% "hotspot-profiler"             % "0.3.0"   % "test"
  val scala_compiler  = "org.scala-lang"      % "scala-compiler"                % "2.11.6"

  val common = Seq(helpers, hsp)

  val bson = common ++ Seq( akka_actor)

  val macros = common ++ Seq( scala_compiler )

  val messages = common

  val driver = common ++ Seq( akka_streams, akka_actor, akka_testkit, akka_slf4j, akka_http_core)

  val client = common ++ Seq( )

  val gridfs = common ++ Seq( )

  val examples = common ++ Seq( )
}
