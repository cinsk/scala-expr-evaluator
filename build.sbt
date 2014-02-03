
name := "hello"

version := "1.0"

scalaVersion := "2.10.3"

mainClass in (Compile, run) := Some("Hello")

libraryDependencies += "org.apache.httpcomponents" % "httpclient" % "4.3.2"

libraryDependencies += "junit" % "junit" % "4.11"

libraryDependencies += "org.scala-tools.testing" % "specs_2.10" % "1.6.9"
