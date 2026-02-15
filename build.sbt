name := "lkdoko"

version := "0.1"

val sharedSettings = Seq(
  scalaVersion := Versions.scalaVersion,
  scalacOptions ++= Seq(
    "-Werror",
    "-unchecked",
    "-Wconf:cat=deprecation:e",
    "-Wunused:all",
    // needed for derivation (of Json typeclasses & of Arbitrary-instances):
    "-Xmax-inlines:90"
  ),
  libraryDependencies ++= Seq(
    "org.scalameta" %%% "munit" % Versions.munitVersion,
    "org.scalameta" %%% "munit-scalacheck" % Versions.munitScalacheckVersion,
    "io.github.martinhh" %%% "scalacheck-derived" % Versions.scalacheckDerivedVersion
  ).map(_ % "test")
)

lazy val shared =
  (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file("shared"))
    .settings(sharedSettings)
    .settings(
      libraryDependencies ++= Seq(
        "org.typelevel" %%% "cats-core" % Versions.catsVersion
      )
    )

lazy val sharedCirce =
  (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file("shared-circe"))
    .settings(sharedSettings)
    .dependsOn(shared % "compile->compile;test->test")
    .settings(
      libraryDependencies ++= Seq(
        "io.circe" %%% "circe-core",
        "io.circe" %%% "circe-generic",
        "io.circe" %%% "circe-parser"
      ).map(_ % Versions.circeVersion)
    )

lazy val client =
  project
    .in(file("client"))
    .enablePlugins(ScalaJSPlugin)
    .settings(sharedSettings)
    .settings(
      scalaJSUseMainModuleInitializer := true,
      Compile / mainClass := Some("io.github.mahh.doko.client.Client"),
      libraryDependencies ++= Seq(
        "org.scala-js" %%% "scalajs-dom" % Versions.scalaJsDomVersion,
        "com.raquo" %%% "laminar" % Versions.laminarVersion,
        "dev.laminext" %%% "core" % Versions.laminextVersion,
        "dev.laminext" %%% "websocket" % Versions.laminextVersion,
        "dev.laminext" %%% "websocket-circe" % Versions.laminextVersion
      )
    )
    .dependsOn(shared.js % "compile->compile;test->test", sharedCirce.js)

lazy val logic =
  project
    .in(file("logic"))
    .settings(sharedSettings)
    .dependsOn(shared.jvm % "compile->compile;test->test")

// static resources that are shared by various server implementations
lazy val serverResources =
  project
    .in(file("server-resources"))
    .settings(
      scalaVersion := Versions.scalaVersion
    )

def serverProject(project: Project)(dependencies: Seq[ModuleID]): Project = {
  project
    .settings(sharedSettings)
    .settings(
      libraryDependencies ++= dependencies,
      Compile / resourceGenerators += Def.task {
        val f1 = (client / Compile / fastOptJS).value.data
        val f1SourceMap = f1.getParentFile / (f1.getName + ".map")
        Seq(f1, f1SourceMap)
      }.taskValue,
      watchSources ++= (client / watchSources).value
    )
    .dependsOn(
      shared.jvm % "compile->compile;test->test",
      sharedCirce.jvm,
      logic,
      serverResources
    )
}

lazy val pekkoServer =
  serverProject(project.in(file("pekko-server")))(
    Seq(
      "org.apache.pekko" %% "pekko-stream-typed" % Versions.pekkoVersion,
      "org.apache.pekko" %% "pekko-http" % Versions.pekkoHttpVersion,
      "ch.qos.logback" % "logback-classic" % Versions.logBackVersion
    )
  )

lazy val http4sServer =
  serverProject(project.in(file("http4s-server"))) {
    Seq(
      "org.http4s" %% "http4s-ember-server" % Versions.http4sVersion,
      "org.http4s" %% "http4s-circe" % Versions.http4sVersion,
      "org.http4s" %% "http4s-dsl" % Versions.http4sVersion,
      "com.outr" %% "scribe" % Versions.scribeVersion,
      "com.outr" %% "scribe-cats" % Versions.scribeVersion
    )
  }
