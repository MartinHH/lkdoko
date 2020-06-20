name := "lkdoko"

version := "0.1"


val sharedSettings = Seq(
  scalaVersion := Versions.scalaVersion,
  scalacOptions ++= Seq(
    "-Ymacro-annotations",
    "-Xfatal-warnings",
    "-Xlint:infer-any"
  )
)

lazy val shared =
  (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file("shared"))
    .settings(sharedSettings)
    .settings(
      libraryDependencies ++= Seq(
        "org.scalacheck" %%% "scalacheck" % Versions.scalacheckVersion % "test",
        "org.scalatest" %%% "scalatest" % Versions.scalaTestVersion % "test",
        "org.scalatestplus" %% "scalacheck-1-14" % Versions.scalaTestPlusVersion % "test",
        // so far, shapeless is only used to derive arbitraries -> test only
        "com.chuusai" %%% "shapeless" % Versions.shapelessVersion % "test"
      ),
      libraryDependencies ++= Seq(
        "io.circe" %%% "circe-core"
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
      mainClass in Compile := Some("io.github.mahh.doko.client.Client"),
      libraryDependencies ++= Seq(
        "org.scala-js" %%% "scalajs-dom" % Versions.scalaJsDomVersion
      ),
      // TODO: add cats explicitly here since (it is used via transitive dependency)
      libraryDependencies ++= Seq(
        "io.circe" %%% "circe-core",
        "io.circe" %%% "circe-generic",
        "io.circe" %%% "circe-parser"
      ).map(_ % Versions.circeVersion)
    )
    .dependsOn(sharedJs)

lazy val logic =
  project.in(file("logic"))
    .settings(sharedSettings)
    .settings(
      libraryDependencies ++= Seq(
        "org.scalacheck" %% "scalacheck" % Versions.scalacheckVersion % "test",
        "org.scalatest" %% "scalatest" % Versions.scalaTestVersion % "test",
        "org.scalatestplus" %% "scalacheck-1-14" % Versions.scalaTestPlusVersion % "test"
      )
    )
    .dependsOn(sharedJvm % "compile->compile;test->test")

lazy val server =
  project.in(file("server"))
    .settings(sharedSettings)
    .settings(
      libraryDependencies ++= Seq(
        "com.typesafe.akka" %% "akka-stream-typed" % Versions.akkaVersion,
        "com.typesafe.akka" %% "akka-http" % Versions.akkaHttpVersion,
        "ch.qos.logback" % "logback-classic" % Versions.logBackVersion,
        "org.scalacheck" %% "scalacheck" % Versions.scalacheckVersion % "test",
        "org.scalatest" %% "scalatest" % Versions.scalaTestVersion % "test",
        "org.scalatestplus" %% "scalacheck-1-14" % Versions.scalaTestPlusVersion % "test"
      ),
      libraryDependencies ++= Seq(
        "io.circe" %% "circe-core",
        "io.circe" %% "circe-generic",
        "io.circe" %% "circe-parser"
      ).map(_ % Versions.circeVersion),
      resourceGenerators in Compile += Def.task {
        val f1 = (fastOptJS in Compile in client).value.data
        val f1SourceMap = f1.getParentFile / (f1.getName + ".map")
        Seq(f1, f1SourceMap)
      }.taskValue,
      watchSources ++= (watchSources in client).value
    )
    .dependsOn(sharedJvm, logic)


