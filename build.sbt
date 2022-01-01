ThisBuild / baseVersion := "0.0"

ThisBuild / organization := "com.armanbilge"
ThisBuild / publishGithubUser := "armanbilge"
ThisBuild / publishFullName := "Arman Bilge"
ThisBuild / startYear := Some(2021)

enablePlugins(SonatypeCiReleasePlugin)
ThisBuild / spiewakCiReleaseSnapshots := true
ThisBuild / spiewakMainBranches := Seq("main")
ThisBuild / homepage := Some(url("https://github.com/armanbilge/cheshire"))
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/armanbilge/cheshire"),
    "git@github.com:armanbilge/cheshire.git"))
sonatypeCredentialHost := "s01.oss.sonatype.org"

val Scala3 = "3.1.0"
ThisBuild / crossScalaVersions := Seq(Scala3)

replaceCommandAlias(
  "ci",
  "; project /; headerCheckAll; scalafmtCheckAll; scalafmtSbtCheck; clean; testIfRelevant; mimaReportBinaryIssuesIfRelevant"
)
addCommandAlias("prePR", "; root/clean; +root/scalafmtAll; scalafmtSbt; +root/headerCreate")

val CatsVersion = "2.7.0"
val CatsEffectVersion = "3.3.3"
val DisciplineVersion = "1.4.0"
val RefinedVersion = "0.9.28"
val ScodecBitsVersion = "1.1.30"
val Specs2Version = "5.0.0-RC-22"
val DisciplineSpecs2Version = "2.0-44-19f6d7f"

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
      "org.typelevel" %%% "discipline-specs2" % DisciplineSpecs2Version % Test,
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
      "org.typelevel" %%% "algebra" % CatsVersion,
      "org.typelevel" %%% "cats-kernel-laws" % CatsVersion,
      "org.typelevel" %%% "discipline-core" % DisciplineVersion,
      "org.typelevel" %%% "cats-effect-laws" % CatsEffectVersion,
      "eu.timepit" %%% "refined-scalacheck" % RefinedVersion
    )
  )
  .settings(commonSettings)
  .dependsOn(likelihood)
