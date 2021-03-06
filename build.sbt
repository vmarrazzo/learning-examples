
name := "concurrency-examples"

version := "1.0"

scalaVersion := "2.11.7"

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

fork in Test := false

// javaOptions in Test := Seq(	"-XX:+UnlockCommercialFeatures",
// 							"-XX:+FlightRecorder",
// 							"-XX:StartFlightRecording=defaultrecording=true",
// 							"-XX:FlightRecorderOptions=dumponexit=true,dumponexitpath=./target/")

//fork in run := true
//
//javaOptions in run := Seq(	"-XX:+UnlockCommercialFeatures",
// 				"-XX:+FlightRecorder", 
// 				"-XX:StartFlightRecording=defaultrecording=true",
// 				"-XX:FlightRecorderOptions=dumponexit=true,dumponexitpath=./target/" //,
//				//"-Xms512M", 
//				//"-Xmx3536M", 
//				//"-Xss1M" 
//				)

libraryDependencies += "commons-io" % "commons-io" % "2.4"

libraryDependencies += "org.scala-lang.modules" %% "scala-async" % "0.9.1"

libraryDependencies += "com.github.scala-blitz" %% "scala-blitz" % "1.2"

libraryDependencies += "com.netflix.rxjava" % "rxjava-scala" % "0.19.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "1.0.1"
            
libraryDependencies += "org.scala-stm" %% "scala-stm" % "0.7"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.2"

libraryDependencies += "com.typesafe.akka" %% "akka-remote" % "2.3.2"

libraryDependencies += "com.storm-enroute" %% "scalameter-core" % "0.6"

// only enable with 2.10.4
// libraryDependencies += "com.twitter" %% "finagle-http" % "6.2.0"

libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.0.6"

libraryDependencies += "com.typesafe.akka" %% "akka-stream-experimental" % "0.4"

libraryDependencies += "com.storm-enroute" %% "reactive-collections" % "0.5"

libraryDependencies += "org.scalatest" %% "scalatest" %	"2.2.5"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.4"

libraryDependencies ++= {
  val breezeV = "0.11.2"
  Seq(
  	"org.scalanlp" %% "breeze" % breezeV,
  	"org.scalanlp" %% "breeze-natives" % breezeV, 
    "org.scalanlp" %% "breeze-viz" % breezeV
    )
}
