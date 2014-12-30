
name := "hello"

version := "0.1"

scalaVersion := "2.11.4"

mainClass in (Compile, run) := Some("cinsk.hello.Hello")

// libraryDependencies += "org.apache.httpcomponents" % "httpclient" % "4.3.2"
// libraryDependencies += "junit" % "junit" % "4.11"
// libraryDependencies += "org.scala-tools.testing" % "specs_2.10" % "1.6.9"

// See https://github.com/scopt/scopt
libraryDependencies += "com.github.scopt" %% "scopt" % "3.3.0"

resolvers += Resolver.sonatypeRepo("public")

// See https://github.com/sbt/sbt-buildinfo ---------------
buildInfoSettings

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion)

buildInfoPackage := "cinsk.hello"
