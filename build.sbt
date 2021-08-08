name := "FunctionalScala"
ThisBuild / scalacOptions ++= Seq("-deprecation")

version := "0.1"

scalaVersion := "2.13.5"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.9"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"
libraryDependencies += "org.scalamock" %% "scalamock" % "5.1.0" % Test
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-15" % "3.2.9.0" % "test"
