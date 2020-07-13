import Dependencies._

ThisBuild / scalaVersion     := "2.13.1"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

val circeVersion = "0.12.3"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

libraryDependencies ++= Seq(
  "com.github.finagle" %% "finch-core" % "0.31.0",
  "com.github.finagle" %% "finch-circe" % "0.31.0",
)

lazy val root = (project in file("."))
  .settings(
    name := "meeting",
    libraryDependencies += scalaTest % Test,
    libraryDependencies +=  "org.scalaj" %% "scalaj-http" % "2.4.2",
    libraryDependencies += "com.typesafe.play" %% "play-json" % "2.8.1",
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.0",
    libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "2.22.0"

  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
