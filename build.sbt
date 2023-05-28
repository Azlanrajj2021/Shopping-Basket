ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "Shop-Basket"
  )

libraryDependencies += ("org.scalactic" %% "scalactic" % "3.0.8")
libraryDependencies += ("org.scalatest" %% "scalatest" % "3.0.8")
libraryDependencies += ("org.scalacheck" %% "scalacheck" % "1.14.0")