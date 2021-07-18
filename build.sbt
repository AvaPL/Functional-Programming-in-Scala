name := "FunctionalScala"
ThisBuild / scalacOptions ++= Seq("-deprecation")

version := "0.1"

scalaVersion := "2.13.5"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.5" % "test"
libraryDependencies += "org.scalamock" %% "scalamock" % "5.1.0" % Test