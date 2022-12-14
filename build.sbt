ThisBuild / organization := "com.example"
ThisBuild / scalaVersion := "3.2.1"

lazy val root = (project in file(".")).settings(
  name := "advent-of-code-2022",
  libraryDependencies ++= Seq(
    // "core" module - IO, IOApp, schedulers
    // This pulls in the kernel and std modules automatically.
    "org.typelevel" %% "cats-effect" % "3.4.1",
    // concurrency abstractions and primitives (Concurrent, Sync, Async etc.)
    "org.typelevel" %% "cats-effect-kernel" % "3.4.1",
    // standard "effect" library (Queues, Console, Random etc.)
    "org.typelevel" %% "cats-effect-std" % "3.4.1",
    //Streams!
    "co.fs2" %% "fs2-core" % "3.4.0",
    "co.fs2" %% "fs2-io" % "3.4.0",
    //Tests
    "org.typelevel" %% "cats-effect-testing-specs2" % "1.4.0" % Test,
    "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test,
    //Circe
    "io.circe" %% "circe-core" % "0.14.1",
    "io.circe" %% "circe-generic" % "0.14.1",
    "io.circe" %% "circe-parser" % "0.14.1",
  )
)
