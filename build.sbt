
name := "ExprEvaluator"

version := "0.1"

scalaVersion := "2.11.6"

mainClass in (Compile, run) := Some("cinsk.Main")

// libraryDependencies += "org.apache.httpcomponents" % "httpclient" % "4.3.2"
// libraryDependencies += "junit" % "junit" % "4.11"
// libraryDependencies += "org.scala-tools.testing" % "specs_2.10" % "1.6.9"

libraryDependencies ++= Seq(
//    "com.github.scopt" %% "scopt" % "3.3.0",
//    "commons-io" % "commons-io" % "2.4",
//    "org.scala-lang" % "scala-reflect" % "2.11.6",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"
  )

resolvers += Resolver.sonatypeRepo("public")

// See https://github.com/sbt/sbt-buildinfo ---------------
//
// buildInfoSettings
// 
// sourceGenerators in Compile <+= buildInfo
// 
// buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion)
// 
// buildInfoPackage := "cinsk.hello"
