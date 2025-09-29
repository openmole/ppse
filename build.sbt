name := "ppse"

val breezeVersion = "2.1.0"
val circeVersion = "0.14.14"
val Scala3Version = "3.7.0"
val laminarVersion = "15.0.1"
val scalajsDomVersion = "2.0.0"
val endpoints4SVersion = "1.9.0"
val endpointCirceVersion = "2.3.0"
val betterFilesVersion = "3.9.2"

ThisBuild / organization := "org.openmole"
ThisBuild / version := "1.0-SNAPSHOT"
ThisBuild / resolvers ++= Resolver.sonatypeOssRepos("snapshots")
ThisBuild / resolvers += Resolver.mavenLocal

def excludeConflicting = Seq(
  excludeDependencies += ExclusionRule(organization = "org.typelevel", name = "cats-kernel_2.13"),
  excludeDependencies += ExclusionRule(organization = "org.scala-lang.modules", name ="scala-collection-compat_2.13")
)

lazy val ppse = project.in(file("ppse")).settings (
  scalaVersion := Scala3Version,
  resolvers += "jitpack" at "https://jitpack.io",
  libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1",
  libraryDependencies += "com.github.pathikrit" %% "better-files" % betterFilesVersion,
  libraryDependencies += "org.openmole" %% "mgo" % "3.66",
  libraryDependencies += "com.github.scopt" %% "scopt" % "4.1.0",
  libraryDependencies += "com.github.haifengl" % "smile-core" % "4.4.0",
  //libraryDependencies += "com.edwardraff" % "JSAT" % "0.0.9",

  libraryDependencies += "com.github.openmole" % "JSAT" % "52daf354b728051f22bdb429ca77f561f9791872",
  //libraryDependencies += "com.edwardraff" % "JSAT" % "0.1.0-SNAPSHOT",// from baseDirectory.value / "lib" / "JSAT-0.1.0-SNAPSHOT.jar",
  libraryDependencies += "com.outr" %% "scribe" % "3.17.0",
  libraryDependencies ++= Seq(
    "org.scalanlp" %% "breeze" % breezeVersion,
    "org.scalanlp" %% "breeze-natives" % breezeVersion
  ),
  libraryDependencies += "org.locationtech.jts" % "jts-core" % "1.20.0",
  libraryDependencies += "org.plotly-scala" %% "plotly-render" % "0.8.5" cross CrossVersion.for3Use2_13,
  libraryDependencies += "org.slf4j" % "slf4j-simple" % "2.0.17",
  scalacOptions ++= Seq("-Ymacro-annotations", "-language:postfixOps"),

  libraryDependencies ++= Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser"
  ).map(_ % circeVersion),

  excludeConflicting
)

lazy val flocking = Project("flocking-model", file("flocking-model")).settings(
  scalaVersion := Scala3Version,
  excludeConflicting
) dependsOn ppse

lazy val traffic = Project("traffic-model", file("traffic-model")).settings(
  scalaVersion := Scala3Version,
  excludeConflicting
) dependsOn ppse

lazy val visu = project.in(file("visu")).enablePlugins(ScalaJSPlugin).settings (
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

lazy val ppsePaper = Project("ppse-paper", file("ppse-paper")).settings (
  version := "1.0-SNAPSHOT",
  scalaVersion := Scala3Version,
  resolvers += "jitpack" at "https://jitpack.io",
  libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1",
  libraryDependencies += "com.github.haifengl" % "smile-core" % "4.4.0",
  libraryDependencies += "com.github.openmole" % "JSAT" % "52daf354b728051f22bdb429ca77f561f9791872",
  //libraryDependencies += "com.edwardraff" % "JSAT" % "0.0.9",
  //libraryDependencies += "com.edwardraff" % "JSAT" % "0.1.0-SNAPSHOT",// from baseDirectory.value / "lib" / "JSAT-0.1.0-SNAPSHOT.jar",
  libraryDependencies += "com.github.pathikrit" %% "better-files" % betterFilesVersion,
  libraryDependencies += "ch.epfl.lamp" %% "gears" % "0.2.0",
  libraryDependencies += "org.openmole" %% "mgo" % "3.66",
  excludeConflicting
)

lazy val plotPaper  = Project("plot-paper", file("plot-paper")).settings (
  version := "1.0-SNAPSHOT",
  scalaVersion := Scala3Version,
  libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1",
  libraryDependencies += "com.github.pathikrit" %% "better-files" % betterFilesVersion,
  libraryDependencies += "org.plotly-scala" %% "plotly-render" % "0.8.5" cross CrossVersion.for3Use2_13,
  excludeConflicting
)
//resolvers += Resolver.sonatypeRepo("snapshots")




//Global / resolvers += Resolver.sonatypeRepo("staging")


lazy val shared = project.in(file("visu/shared")) settings (
  scalaVersion := Scala3Version,
  libraryDependencies ++= Seq(
    "org.endpoints4s" %%% "algebra" % endpoints4SVersion,
    "org.endpoints4s" %%% "json-schema-circe" % endpointCirceVersion,
    "io.circe" %% "circe-generic" % circeVersion)
) enablePlugins ScalaJSPlugin

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
    //"@svgdotjs/svg.panzoom.js" -> "2.1.2"
    "@panzoom/panzoom" -> "4.5.1"
//"bootstrap.native" -> "5.0.7"
  ),
) dependsOn shared

lazy val server = project.in(file("visu/server")) settings(
  scalaVersion := Scala3Version,
  libraryDependencies ++= Seq(
//    "com.lihaoyi" %% "scalatags" % scalatagsVersion,
    "org.endpoints4s" %% "http4s-server" % "11.0.1",
    "org.http4s" %% "http4s-blaze-server" % "0.23.17",
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



