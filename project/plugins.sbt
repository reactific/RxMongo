// Comment to get more information during initialization
logLevel := Level.Info

// Scala Version for SBT compilation
scalaVersion := "2.10.4"

// Options for SBT compilation
scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xlint")

// Resolvers to use for finding source modules
resolvers += "jgit-repo" at "http://download.eclipse.org/jgit/maven"

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Sonatype respository" at "https://oss.sonatype.org/content/repositories/releases/"

addSbtPlugin("com.typesafe.sbt" % "sbt-scalariform" % "1.3.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.6.4")

addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.5.3")

addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.3.1")

addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "0.8.1")
