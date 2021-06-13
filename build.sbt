ThisBuild / baseVersion := "0.0"

ThisBuild / organization := "com.armanbilge"
ThisBuild / publishGithubUser := "armanbilge"
ThisBuild / publishFullName := "Arman Bilge"
ThisBuild / startYear := Some(2021)

mimaPreviousArtifacts := Set()

enablePlugins(SonatypeCiReleasePlugin)
ThisBuild / homepage := Some(url("https://github.com/armanbilge/van-cats"))
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/armanbilge/van-cats"),
    "git@github.com:armanbilge/van-cats.git"))
sonatypeCredentialHost := "s01.oss.sonatype.org"

val Scala213 = "2.13.6"
val Scala3 = "3.0.0"
ThisBuild / crossScalaVersions := Seq(Scala3, Scala213)

replaceCommandAlias(
  "ci",
  "; project /; headerCheckAll; scalafmtCheckAll; scalafmtSbtCheck; clean; testIfRelevant; mimaReportBinaryIssuesIfRelevant"
)
addCommandAlias("prePR", "; root/clean; +root/scalafmtAll; scalafmtSbt; +root/headerCreate")

val CatsVersion = "2.6.1"
val Specs2Version = "4.12.1"
val DisciplineVersion = "1.1.6"

lazy val root =
  project.aggregate(core).enablePlugins(NoPublishPlugin)

lazy val core = project
  .in(file("core"))
  .settings(
    name := "cheshire",
    sonatypeCredentialHost := "s01.oss.sonatype.org",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % CatsVersion,
      "dev.optics" %% "monocle-core" % "3.0.0-RC2",
      "org.typelevel" %% "cats-laws" % CatsVersion % Test,
      "org.typelevel" %% "discipline-specs2" % DisciplineVersion % Test,
      "org.scalacheck" %% "scalacheck" % "1.15.4" % Test,
      "com.github.alexarchambault" %% "scalacheck-shapeless_1.15" % "1.3.0" % Test,
      "org.specs2" %% "specs2-core" % Specs2Version % Test cross CrossVersion.for3Use2_13,
      "org.specs2" %% "specs2-scalacheck" % Specs2Version % Test cross CrossVersion.for3Use2_13
    )
  )
