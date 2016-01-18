import sbt.Keys._
import sbtrelease.ReleasePlugin.autoImport._


lazy val commonSettings = Seq(
  scalaVersion in ThisBuild := "2.11.8",
  crossScalaVersions in ThisBuild := Seq("2.10.6", "2.11.8"), // soon "2.12.0-M4"
  releaseCrossBuild in ThisBuild := true,
  scalafmtConfig in ThisBuild := Some(file(".scalafmt")),
  git.remoteRepo in ThisBuild := "git@github.com:danielwegener/unluac-scala.git",
  organization := "com.github.danielwegener",
  licenses := Seq("MIT" -> url("https://opensource.org/licenses/MIT")),
  homepage := Some(url("https://github.com/danielwegener/unluac-scala")),
  scmInfo := Some(ScmInfo(url("https://github.com/danielwegener/unluac-scala"),
    connection = "scm:git:github.com/danielwegener/unluac-scala",
    devConnection = Some("scm:git:git@github.com:/danielwegener/unluac-scala"))),
  developers := List(
    Developer("dwegener", "Daniel Wegener", "daniel@wegener.me", url("https://github.com/danielwegener"))),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  }
)


lazy val `unluac-scala` = project.in(file(".")).
  enablePlugins(SiteScaladocPlugin).
  settings(commonSettings: _*).
  settings(ghpages.settings :_*).
  aggregate(`unluac-scala-jvm`, `unluac-scala-js`).
  settings(
    name := "unluac-scala",
    publish := {},
    publishLocal := {},
    siteMappings ++= Seq(
      (fullOptJS in (`unluac-scala-js`, Compile)).value.data -> "unluac-scala-opt.js",
      file((fullOptJS in (`unluac-scala-js`, Compile)).value.data.getAbsolutePath+".map") -> "unluac-scala-opt.js.map"
    )
  )

lazy val `unluac-scala-crossproject` = crossProject.in(file(".")).
  settings(commonSettings: _*).
  settings(
    name := "unluac-scala",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.0-M15" % "test",
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false },
    releasePublishArtifactsAction := PgpKeys.publishSigned.value
  ).
  jvmSettings(
    // Add JVM-specific settings here
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided"
  ).
  jsSettings(
    // Add JS-specific settings here
    // Link source maps
    scalacOptions ++= Seq(
        // Link source maps to github sources
        "-P:scalajs:mapSourceURI:" + file(".").toURI +
          "->https://raw.githubusercontent.com/danielwegener/unluac-scala/v" +
          version.value + "/"
      )
  )

lazy val `unluac-scala-jvm` = `unluac-scala-crossproject`.jvm
lazy val `unluac-scala-js` = `unluac-scala-crossproject`.js
