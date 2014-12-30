Introduction
============

This is the sample SBT project for the Scala programming language.


Build
=====

Using [sbt](http://www.scala-sbt.org/), you can compile the project
using `sbt compile`, and assemble the jar using `sbt assembly`.

Dependency
----------

Currently, this project uses two *sbt* plugins: *sbt-buildinfo* to
retrive the name and version string from the `build.sbt`, and
*sbt-assembly* to create fat JAR of the project with all of its
dependencies.

This project also has a dependency to *scopt*, a little command line
option parsing library.
