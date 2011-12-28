name := "typesearch"

version := "0.0001"

scalaVersion := "2.9.1"


//Add Akka for using serialization libs

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "se.scalablesolutions.akka" % "akka-actor" % "1.3-RC4"

libraryDependencies += "se.scalablesolutions.akka" % "akka-remote" % "1.3-RC4"


