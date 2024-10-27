lazy val root =
  project
    .in(file("."))
    .settings( scalaVersion := "3.5.2"
             , name         := "aoc-2021"
             , version      := "0.1.0"
             )
