import sbt._

object Dependencies {

  val catsCore = "org.typelevel" %% "cats-core" % "2.0.0-M4"
  val kittens = "org.typelevel" %% "kittens" % "2.0.0-M1"
  val scalaTest = "org.scalatest" %% "scalatest" % "3.0.8"

  object CompilerPlugins {

    val kindProjector =
      compilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")

    val betterMonadicFor =
      compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

    val silencer = {
      val version = "1.4.1"
      Seq(
        compilerPlugin("com.github.ghik" %% "silencer-plugin" % version),
        "com.github.ghik" %% "silencer-lib" % version % Provided
      )
    }

    val all =
      Seq(
        kindProjector,
        betterMonadicFor
      ) ++ silencer
  }
}
