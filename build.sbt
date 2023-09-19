name := "ppse"

val breezeVersion = "2.0.1-RC2"
val circeVersion = "0.14.5"
val Scala3Version = "3.3.1"
val laminarVersion = "15.0.1"
val scalajsDomVersion = "2.0.0"
val endpoints4SVersion = "1.9.0"
val endpointCirceVersion = "2.3.0"

ThisBuild / organization := "org.openmole"
ThisBuild / version := "1.0-SNAPSHOT"

def excludeConflicting = Seq(
  excludeDependencies += ExclusionRule(organization = "org.typelevel", name = "cats-kernel_2.13"),
  excludeDependencies += ExclusionRule(organization = "org.scala-lang.modules", name ="scala-collection-compat_2.13")
)

lazy val ppse = Project("ppse", file("ppse")).settings (
  scalaVersion := Scala3Version,
  libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1",
  libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.9.1" cross CrossVersion.for3Use2_13,
  libraryDependencies += "org.openmole" %% "mgo" % "3.55",
  libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.1",
  libraryDependencies += "com.github.haifengl" % "smile-core" % "2.6.0",
  libraryDependencies += "com.edwardraff" % "JSAT" % "0.0.9",
  libraryDependencies += "com.outr" %% "scribe" % "3.12.0",
  libraryDependencies ++= Seq(
    "org.scalanlp" %% "breeze" % breezeVersion,
    "org.scalanlp" %% "breeze-natives" % breezeVersion
  ),
  libraryDependencies += "org.locationtech.jts" % "jts-core" % "1.18.1",
  libraryDependencies += "org.plotly-scala" %% "plotly-render" % "0.8.4" cross CrossVersion.for3Use2_13,
    scalacOptions ++= Seq("-Ymacro-annotations", "-language:postfixOps"),

  libraryDependencies ++= Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser"
  ).map(_ % circeVersion),

  excludeConflicting
)

lazy val visu = Project("visu", file("visu")).enablePlugins(ScalaJSPlugin).settings (
  scalaVersion := Scala3Version,
  libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.2.0",
  //scalaJSUseMainModuleInitializer := true,
  libraryDependencies ++= Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser"
  ).map(_ % circeVersion),
//  libraryDependencies += "org.plotly-scala" %% "plotly-render" % "0.8.4",
//  libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.9.1"
)

lazy val selfContained = Project("ppse-paper", file("ppse-paper")).settings (
  version := "1.0-SNAPSHOT",
  scalaVersion := Scala3Version,
  libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1",
  libraryDependencies += "com.edwardraff" % "JSAT" % "0.0.9",
  libraryDependencies ++= Seq(
    "org.scalanlp" %% "breeze" % breezeVersion cross CrossVersion.for3Use2_13,
    "org.scalanlp" %% "breeze-natives" % breezeVersion  cross CrossVersion.for3Use2_13
  )
)

//resolvers += Resolver.sonatypeRepo("snapshots")




//Global / resolvers += Resolver.sonatypeRepo("staging")


lazy val shared = project.in(file("visu/shared")) settings (
  scalaVersion := Scala3Version,
  libraryDependencies ++= Seq(
    "org.endpoints4s" %%% "algebra" % endpoints4SVersion,
    "org.endpoints4s" %%% "json-schema-circe" % endpointCirceVersion,
    "io.circe" %% "circe-generic" % circeVersion)
) enablePlugins (ScalaJSPlugin)

lazy val client = project.in(file("visu/client")) enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin, ScalablyTypedConverterPlugin) settings(
  scalaVersion := Scala3Version,
  scalaJSUseMainModuleInitializer := false,
  webpackBundlingMode := BundlingMode.LibraryAndApplication(),
  webpackNodeArgs := Seq("--openssl-legacy-provider"),

  libraryDependencies ++= Seq(
    "com.raquo" %%% "laminar" % laminarVersion,
//    "org.openmole.scaladget" %%% "tools" % scaladgetVersion,
//    "org.openmole.scaladget" %%% "svg" % scaladgetVersion,
//    "org.openmole.scaladget" %%% "bootstrapnative" % scaladgetVersion,
    "org.scala-js" %%% "scalajs-dom" % scalajsDomVersion,
    "org.openmole.endpoints4s" %%% "xhr-client" % "5.1.0+n"
  ),
  Compile / npmDependencies ++= Seq(
    "fabric" -> "5.3.0",
    "@types/fabric" -> "5.3.0",
    "@svgdotjs/svg.js" -> "3.2.0",
    "@svgdotjs/svg.panzoom.js" -> "2.1.2"
//"bootstrap.native" -> "5.0.7"
  ),
) dependsOn (shared)

lazy val server = project.in(file("visu/server")) settings(
  scalaVersion := Scala3Version,
  libraryDependencies ++= Seq(
//    "com.lihaoyi" %% "scalatags" % scalatagsVersion,
    "org.endpoints4s" %% "http4s-server" % "10.1.0",
    "org.http4s" %% "http4s-blaze-server" % "0.23.12",
    "io.circe" %% "circe-parser" % circeVersion,
//    "com.raquo" %%% "laminar" % laminarVersion

    //    "org.endpoints4s" %% "akka-http-server" % "6.1.0+n",
    //    "com.typesafe.akka" %% "akka-stream" % "2.6.18" //cross CrossVersion.for3Use2_13
  ),

  Compile / compile := {
    val jsBuild = (client / Compile / fullOptJS / webpack).value.head.data

    val demoTarget = target.value
    val demoResource = (client / Compile / resourceDirectory).value

    IO.copyFile(jsBuild, demoTarget / "webapp/js/demo.js")
    IO.copyFile((client / crossTarget).value / "scalajs-bundler/main/client-opt-bundle.js.map", demoTarget / "webapp/js/client-opt-bundle.js.map")
    IO.copyDirectory(demoResource, demoTarget)

    IO.copyDirectory((client / crossTarget).value / "scalajs-bundler/main/node_modules", demoTarget / "webapp/js/node_modules")

    (Compile / compile).value
  },

  run := Def.taskDyn {
    (Compile / run).toTask(" " + ((Compile / target).value.getParentFile / "data"))
  }.value,

  excludeConflicting
) dependsOn (shared, ppse)



