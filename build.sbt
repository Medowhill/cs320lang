ThisBuild / scalaVersion := "2.13.5"

ThisBuild / scalacOptions += "-feature"
ThisBuild / scalacOptions += "-deprecation"
ThisBuild / scalacOptions += "-Xlint:unused"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
