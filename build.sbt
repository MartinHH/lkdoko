name := "lkdoko"

version := "0.1"


val sharedSettings = Seq(
  scalaVersion := Versions.scalaVersion,
  scalacOptions ++= Seq(
    "-Xfatal-warnings",
    "-unchecked",
    "-Wconf:cat=deprecation:e",
    // needed for derivation (of Json typeclasses & of Arbitrary-instances):
    "-Xmax-inlines:80"
  ),
  libraryDependencies ++= Seq(
    "org.scalameta" %%% "munit",
    "org.scalameta" %%% "munit-scalacheck"
  ).map(_  % Versions.munitVersion % "test")
)

lazy val shared =
  (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file("shared"))
    .settings(sharedSettings)
    .settings(
      libraryDependencies ++= Seq(
        "io.circe" %%% "circe-core",
        "io.circe" %%% "circe-generic",
        "io.circe" %%% "circe-parser"
      ).map(_ % Versions.circeVersion)
    )

lazy val sharedJvm = shared.jvm
lazy val sharedJs = shared.js

lazy val client =
  project.in(file("client"))
    .enablePlugins(ScalaJSPlugin)
    .settings(sharedSettings: _*)
    .settings(
      scalaJSUseMainModuleInitializer := true,
      Compile / mainClass := Some("io.github.mahh.doko.client.Client"),
      libraryDependencies ++= Seq(
        "org.scala-js" %%% "scalajs-dom" % Versions.scalaJsDomVersion
      )
    )
    .dependsOn(sharedJs % "compile->compile;test->test")

lazy val logic =
  project.in(file("logic"))
    .settings(sharedSettings)
    .dependsOn(sharedJvm % "compile->compile;test->test")

lazy val server =
  project.in(file("server"))
    .settings(sharedSettings)
    .settings(
      libraryDependencies ++= Seq(
        ("com.typesafe.akka" %% "akka-stream-typed" % Versions.akkaVersion).cross(CrossVersion.for3Use2_13),
        ("com.typesafe.akka" %% "akka-http" % Versions.akkaHttpVersion).cross(CrossVersion.for3Use2_13),
        "ch.qos.logback" % "logback-classic" % Versions.logBackVersion
      ),
      Compile / resourceGenerators += Def.task {
        val f1 = (client / Compile / fastOptJS).value.data
        val f1SourceMap = f1.getParentFile / (f1.getName + ".map")
        Seq(f1, f1SourceMap)
      }.taskValue,
      watchSources ++= (client/ watchSources).value
    )
    .dependsOn(sharedJvm % "compile->compile;test->test", logic)
