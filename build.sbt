version := "0.1"

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "in.darkempire",
      scalaVersion := "2.13.4"
    )),
    name := "scala-programs"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % Test
