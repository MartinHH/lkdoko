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
  ).map(_ % Versions.munitVersion % "test")
)

lazy val shared =
  (crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("shared")))
    .settings(sharedSettings)
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
        "org.scala-js" %%% "scalajs-dom" % Versions.scalaJsDomVersion
      )
    )
    .dependsOn(shared.js % "compile->compile;test->test")

lazy val logic =
  (crossProject(JVMPlatform, NativePlatform).crossType(CrossType.Pure) in (file("logic")))
    .settings(sharedSettings)
    .dependsOn(shared % "compile->compile;test->test")

// static resources that are shared by various server implementations
lazy val serverResources =
  (crossProject(JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("server-resources")))
    .settings(
      scalaVersion := Versions.scalaVersion
    )

val serverSettings = sharedSettings ++
  Seq(
    Compile / resourceGenerators += Def.task {
      val f1 = (client / Compile / fastOptJS).value.data
      val f1SourceMap = f1.getParentFile / (f1.getName + ".map")
      Seq(f1, f1SourceMap)
    }.taskValue,
    watchSources ++= (client / watchSources).value
  )

lazy val akkaServer =
  project
    .in(file("akka-server"))
    .settings(
      serverSettings ++
        Seq(
          libraryDependencies ++= Seq(
            ("com.typesafe.akka" %% "akka-stream-typed" % Versions.akkaVersion)
              .cross(CrossVersion.for3Use2_13),
            ("com.typesafe.akka" %% "akka-http" % Versions.akkaHttpVersion)
              .cross(CrossVersion.for3Use2_13),
            "ch.qos.logback" % "logback-classic" % Versions.logBackVersion
          )
        )
    )
    .dependsOn(
      shared.jvm % "compile->compile;test->test",
      logic.jvm,
      serverResources.jvm
    )

lazy val http4sServer =
  (crossProject(JVMPlatform, NativePlatform)
    .crossType(CrossType.Full)
    .in(file("http4s-server")))
    .settings(
      serverSettings ++
        Seq(
          libraryDependencies ++= Seq(
            "org.http4s" %%% "http4s-ember-server" % Versions.http4sVersion,
            "org.http4s" %%% "http4s-circe" % Versions.http4sVersion,
            "org.http4s" %%% "http4s-dsl" % Versions.http4sVersion,
            "ch.qos.logback" % "logback-classic" % Versions.logBackVersion
          )
        )
    )
    .dependsOn(
      shared % "compile->compile;test->test",
      logic,
      serverResources
    )
    .nativeSettings(
      libraryDependencies ++= Seq(
        // runtime:
        "com.armanbilge" %%% "epollcat" % Versions.epollcat,
        // secure random for UUID.randomUUID():
        "com.github.lolgab" %%% "scala-native-crypto" % Versions.scalaNativeCrypto
      )
    )
