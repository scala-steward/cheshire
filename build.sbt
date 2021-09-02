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

val Scala3 = "3.0.2"
ThisBuild / crossScalaVersions := Seq(Scala3)

replaceCommandAlias(
  "ci",
  "; project /; headerCheckAll; scalafmtCheckAll; scalafmtSbtCheck; clean; testIfRelevant; mimaReportBinaryIssuesIfRelevant"
)
addCommandAlias("prePR", "; root/clean; +root/scalafmtAll; scalafmtSbt; +root/headerCreate")

val CatsVersion = "2.6.1"
val Specs2Version = "4.12.7"
val DisciplineSpecs2Version = "1.1.6"

val commonSettings = Seq(
  scalacOptions := Seq("-new-syntax", "-indent", "-source:future"),
  sonatypeCredentialHost := "s01.oss.sonatype.org"
)

lazy val root =
  project.in(file(".")).aggregate(core.js, core.jvm).enablePlugins(NoPublishPlugin)

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    name := "cheshire",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % CatsVersion,
      "org.typelevel" %% "cats-laws" % CatsVersion % Test,
      "org.typelevel" %% "discipline-specs2" % DisciplineSpecs2Version % Test,
      "org.specs2" %% "specs2-core" % Specs2Version % Test cross CrossVersion.for3Use2_13,
      ("org.specs2" %% "specs2-scalacheck" % Specs2Version % Test)
        .cross(CrossVersion.for3Use2_13)
        .exclude("org.scalacheck", "scalacheck_2.13")
    )
  )
  .settings(commonSettings)
