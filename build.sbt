import Dependencies._

ThisBuild / scalaVersion        := "2.13.0"
ThisBuild / version             := "0.1.0"
ThisBuild / libraryDependencies ++= CompilerPlugins.all

lazy val root = (project in file("."))
  .settings(
    name := "recursion-schemes",
    libraryDependencies ++= Seq(
      catsCore,
      kittens,
      scalaTest % Test
    )
  )
