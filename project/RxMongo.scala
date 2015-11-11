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

import com.reactific.sbt.ProjectPlugin
import com.reactific.sbt.ProjectPlugin.autoImport._
import sbt._
import sbt.Keys._
import scoverage.ScoverageSbtPlugin
import scala.language.postfixOps


object RxMongo extends Build {

  val name = "RxMongo"

  val filter = { (ms: Seq[(File, String)]) =>
    ms filter {
      case (file, path) =>
        path != "logback.xml" && !path.startsWith("toignore") && !path.startsWith("samples")
    }
  }

  val classesIgnoredByScoverage : String = Seq[String](
    "rxmongo.examples",
    "<empty>" // Avoids warnings from scoverage
  ).mkString(";")

  val buildSettings: Seq[sbt.Def.Setting[_]] = Defaults.coreDefaultSettings ++
    Seq(
      organization := "com.reactific",
      copyrightYears := Seq(2015),
      copyrightHolder := "Reactific Software LLC",
      codePackage     := "com.reactific.slickery",
      titleForDocs    := "Reactific Slick Utilities",
      developerUrl    := url("http://www.reactific.com/"),
      ScoverageSbtPlugin.ScoverageKeys.coverageFailOnMinimum := true,
      ScoverageSbtPlugin.ScoverageKeys.coverageExcludedPackages := classesIgnoredByScoverage,
      mappings in(Compile, packageBin) ~= filter,
      mappings in(Compile, packageSrc) ~= filter,
      mappings in(Compile, packageDoc) ~= filter
    )


  lazy val root = sbt.Project(name, file("."))
    .enablePlugins(ProjectPlugin)
    .settings(buildSettings:_*)
    .settings(
      libraryDependencies ++= Dependencies.examples,
      ScoverageSbtPlugin.ScoverageKeys.coverageMinimum := 45
    ).
    dependsOn(examples, client, driver, messages, bson_deps).
    aggregate(bson, messages, driver, client, examples)

  lazy val gridfs = sbt.Project(s"$name-GridFS", file("./gridfs"))
    .enablePlugins(ProjectPlugin)
    .settings(buildSettings:_*)
    .settings(
      libraryDependencies ++= Dependencies.gridfs,
      ScoverageSbtPlugin.ScoverageKeys.coverageMinimum := 0
    )
    .dependsOn(client, bson_deps, driver)

  lazy val client = sbt.Project(s"$name-Client", file("./client"))
    .enablePlugins(ProjectPlugin)
    .settings(buildSettings:_*)
    .settings(
      libraryDependencies ++= Dependencies.client,
      ScoverageSbtPlugin.ScoverageKeys.coverageMinimum := 43
    )
    .dependsOn(bson_deps, driver)

  lazy val driver = sbt.Project(s"$name-Driver", file("./driver"))
    .enablePlugins(ProjectPlugin)
    .settings(buildSettings:_*)
    .settings(
      libraryDependencies ++= Dependencies.driver,
      ScoverageSbtPlugin.ScoverageKeys.coverageMinimum := 64
    )
    .dependsOn(bson_deps, messages)

/*  lazy val macros =
    Project(s"${BuildSettings.name}-Macros", file("./macros"),
      settings = buildSettings ++ Seq(
        libraryDependencies := Dependencies.macros
      )).
      dependsOn(bson_deps)
*/
  lazy val messages = sbt.Project(s"$name-Messages", file("./messages"))
    .enablePlugins(ProjectPlugin)
    .settings(buildSettings:_*)
    .settings(
      libraryDependencies ++= Dependencies.messages,
      ScoverageSbtPlugin.ScoverageKeys.coverageMinimum := 35
    )
    .dependsOn(bson_deps)

  lazy val bson = sbt.Project(s"$name-BSON", file("./bson"))
    .enablePlugins(ProjectPlugin)
    .settings(buildSettings:_*)
    .settings(
      libraryDependencies ++= Dependencies.bson,
      ScoverageSbtPlugin.ScoverageKeys.coverageMinimum := 55
    )
  lazy val bson_deps = bson % "compile->compile;test->test"

  lazy val examples = sbt.Project(s"$name-Examples", file("./examples"))
    .enablePlugins(ProjectPlugin)
    .settings(buildSettings:_*)
    .settings(
      libraryDependencies ++= Dependencies.examples,
      ScoverageSbtPlugin.ScoverageKeys.coverageMinimum := 0
    ).
    dependsOn(client,driver)
}
