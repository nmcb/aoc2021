lazy val root =
  project
    .in(file("."))
    .settings( scalaVersion := "3.7.0"
      , name                := "aoc-2021"
      , version             := "0.1.0"
      , libraryDependencies ++= Seq(
        "org.scalatest"  %% "scalatest"  % "3.2.19" % "test"
      )
    )

scalacOptions ++= Seq(
  "-encoding", "utf8",
  "-feature",
  "-language:implicitConversions",
  "-language:existentials",
  "-unchecked",
  "-Werror",
  "-deprecation",
  "-experimental"
)

Compile / run / fork := true
Compile / run / javaOptions ++= Seq("-Xmx8G", "-Xss1G", "-XX:+UseG1GC")

Test / fork := true
Test / javaOptions ++= Seq("-Xmx8G", "-Xss1G", "-XX:+UseG1GC")
