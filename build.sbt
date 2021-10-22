ThisBuild / baseVersion := "0.0"

ThisBuild / organization := "com.armanbilge"
ThisBuild / publishGithubUser := "armanbilge"
ThisBuild / publishFullName := "Arman Bilge"
ThisBuild / startYear := Some(2021)

enablePlugins(SonatypeCiReleasePlugin)
ThisBuild / homepage := Some(url("https://github.com/armanbilge/cheshire"))
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/armanbilge/cheshire"),
    "git@github.com:armanbilge/cheshire.git"))

val Scala3 = "3.1.0"
ThisBuild / crossScalaVersions := Seq(Scala3)

replaceCommandAlias(
  "ci",
  "; project /; headerCheckAll; scalafmtCheckAll; scalafmtSbtCheck; clean; testIfRelevant; mimaReportBinaryIssuesIfRelevant"
)
addCommandAlias("prePR", "; root/clean; +root/scalafmtAll; scalafmtSbt; +root/headerCreate")

val AlgebraVersion = "2.2.3"
val CatsVersion = "2.6.1"
val CatsEffectVersion = "3.2.9"
val DisciplineVersion = "1.3.0"
val ScodecBitsVersion = "1.1.29"
val Specs2Version = "5.0.0-RC-15"
val DisciplineSpecs2Version = "1.2-7-e3ce260"

val commonSettings = Seq(
  scalacOptions ++= Seq("-new-syntax", "-indent", "-source:future"),
  sonatypeCredentialHost := "s01.oss.sonatype.org"
)

lazy val root =
  project
    .in(file("."))
    .aggregate(core.js, core.jvm, likelihood, likelihoodLaws)
    .enablePlugins(NoPublishPlugin)

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    name := "cheshire",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % CatsVersion,
      "org.typelevel" %%% "cats-laws" % CatsVersion % Test,
      "io.vasilev" %%% "discipline-specs2" % DisciplineSpecs2Version % Test,
      "org.specs2" %%% "specs2-core" % Specs2Version % Test,
      "org.specs2" %%% "specs2-scalacheck" % Specs2Version % Test
    )
  )
  .settings(commonSettings)

lazy val likelihood = project
  .in(file("likelihood"))
  .settings(
    name := "cheshire-likelihood",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % CatsVersion,
      "org.scodec" %%% "scodec-bits" % ScodecBitsVersion,
      "org.typelevel" %%% "cats-effect-kernel" % CatsEffectVersion
    )
  )
  .settings(commonSettings)
  .dependsOn(core.jvm)

lazy val likelihoodLaws = project
  .in(file("likelihood-laws"))
  .settings(
    name := "cheshire-likelihood-laws",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "algebra" % AlgebraVersion,
      "org.typelevel" %%% "cats-kernel-laws" % CatsVersion,
      "org.typelevel" %%% "discipline-core" % DisciplineVersion,
      "org.typelevel" %%% "cats-effect" % CatsEffectVersion % Test
    )
  )
  .settings(commonSettings)
  .dependsOn(likelihood)
