/* UNCOMMENT FOR SCALA NATIVE
import scala.scalanative.build._
*/
name := "sl"
organization:="ro.upb.nrs"
version := "1.0.0"
scalaVersion := "2.13.8"

/* UNCOMMENT FOR SCALA NATIVE
enablePlugins(ScalaNativePlugin)
nativeConfig ~= { c=>
  c.withLTO(LTO.full)
    .withMode(Mode.releaseFull)
    .withGC(GC.commix)
}
//.withCompileOptions(c.compileOptions ++ Seq("-v"))
*/

publishTo := Some("Repsy Managed Repository" at "https://repo.repsy.io/mvn/sdcioc/nrs")
credentials += Credentials("Repsy Managed Repository", "repo.repsy.io", "sdcioc", "12#$qwER")


libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"

//scalates test dependencies
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % "test"

publishConfiguration := publishConfiguration.value.withOverwrite(true)
publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true)


parallelExecution in Test := false
