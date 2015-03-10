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


object RxMongo extends Build {

  import BuildSettings._

  lazy val RxMongo =
    Project(BuildSettings.name, file("."),
      settings = buildSettings ++ Seq(
        libraryDependencies := Dependencies.examples
      )).
      dependsOn(examples, client, driver, messages, bson_deps).
      aggregate(bson, messages, driver, client, examples)

  lazy val gridfs =
    Project(s"${BuildSettings.name}-GridFS", file("./gridfs"),
      settings = buildSettings ++ Seq(
        libraryDependencies ++= Dependencies.gridfs
      )).
    dependsOn(client, bson_deps, driver)

  lazy val client =
    Project(s"${BuildSettings.name}-Client", file("./client"),
      settings = buildSettings ++ Seq(
        libraryDependencies ++= Dependencies.client
      )).
    dependsOn(bson_deps, driver)

  lazy val driver =
    Project(s"${BuildSettings.name}-Driver", file("./driver"),
      settings = buildSettings ++ Seq(
        libraryDependencies := Dependencies.driver
      )).
    dependsOn(bson_deps, messages)

  lazy val macros =
    Project(s"${BuildSettings.name}-Macros", file("./macros"),
      settings = buildSettings ++ Seq(
        libraryDependencies := Dependencies.macros
      )).
      dependsOn(bson_deps)

  lazy val messages =
    Project(s"${BuildSettings.name}-Messages", file("./messages"),
      settings = buildSettings ++ Seq(
        libraryDependencies := Dependencies.messages
      )).
    dependsOn(bson_deps, macros)

  lazy val bson =
    Project(s"${BuildSettings.name}-BSON", file("./bson"),
      settings = buildSettings ++ Seq(
        libraryDependencies ++= Dependencies.bson
      ))
  lazy val bson_deps = bson % "compile->compile;test->test"

  lazy val examples =
    Project(s"${BuildSettings.name}-Examples", file("./examples"),
      settings = buildSettings ++ Seq(
        libraryDependencies ++= Dependencies.examples
      )).
    dependsOn(client,driver)
}
