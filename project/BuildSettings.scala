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

import sbt.Keys._
import sbt._
import scala.language.postfixOps

object BuildSettings {
  val name = "RxMongo"
  val rxmongo_version = "0.1.0-SNAPSHOT"

  val filter = { (ms: Seq[(File, String)]) =>
    ms filter {
      case (file, path) =>
        path != "logback.xml" && !path.startsWith("toignore") && !path.startsWith("samples")
    }
  }

  val buildSettings: Seq[sbt.Def.Setting[_]] = Defaults.coreDefaultSettings ++
    Seq(
      organization := "org.rxmongo",
      version := rxmongo_version,
      scalaVersion := "2.11.4",
      javaOptions in test ++= Seq("-Xmx512m", "-XX:MaxPermSize=512m"),
      scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation", "-target:jvm-1.7"),
      scalacOptions in(Compile, doc) ++=
        Seq("-feature", "-unchecked", "-deprecation", "-diagrams", "-implicits", "-skip-packages", "samples"),
      scalacOptions in(Compile, doc) ++= Opts.doc.title("RxMongo API"),
      scalacOptions in(Compile, doc) ++= Opts.doc.version(rxmongo_version),
      sourceDirectories in Compile := Seq(baseDirectory.value / "src"),
      sourceDirectories in Test := Seq(baseDirectory.value / "test"),
      unmanagedSourceDirectories in Compile := Seq(baseDirectory.value / "src"),
      unmanagedSourceDirectories in Test := Seq(baseDirectory.value / "test"),
      scalaSource in Compile := baseDirectory.value / "src",
      scalaSource in Test := baseDirectory.value / "test",
      javaSource in Compile := baseDirectory.value / "src",
      javaSource in Test := baseDirectory.value / "test",
      resourceDirectory in Compile := baseDirectory.value / "src/resources",
      resourceDirectory in Test := baseDirectory.value / "test/resources",
      fork in Test := false,
      parallelExecution in Test := false,
      logBuffered in Test := false,
      ivyScala := ivyScala.value map {_.copy(overrideScalaVersion = true)},
      shellPrompt := ShellPrompt.buildShellPrompt,
      mappings in(Compile, packageBin) ~= filter,
      mappings in(Compile, packageSrc) ~= filter,
      mappings in(Compile, packageDoc) ~= filter
    ) ++ Publish.settings ++ Docs.settings
}

// Shell prompt which show the current project,
// git branch and build version
object ShellPrompt {
  object devnull extends ProcessLogger {
    def info(s: => String) {}

    def error(s: => String) {}

    def buffer[T](f: => T): T = f
  }

  def currBranch = (
    ("git status -sb" lines_! devnull headOption)
      getOrElse "-" stripPrefix "## ")

  val buildShellPrompt = {
    (state: State) =>
    {
      val currProject = Project.extract(state).currentProject.id
      "%s:%s:%s> ".format(
        currProject, currBranch, BuildSettings.rxmongo_version)
    }
  }
}
