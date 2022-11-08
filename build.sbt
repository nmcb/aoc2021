lazy val root =
  project
    .in(file("."))
    .settings( scalaVersion := "3.2.1"
             , name         := "aoc-2021"
             , version      := "0.1.0"
             )
