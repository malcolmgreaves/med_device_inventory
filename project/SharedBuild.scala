import sbt._
import Keys._

object SharedBuild {

  // // // // // // // // // //
  // //     Versions      // //
  // // // // // // // // // //

  lazy val dataTcV     = "0.0.0"
  lazy val sutilV      = "0.3.0"
  lazy val silencerV   = "0.5"
  lazy val raptureV    = "2.0.0-M7"
  lazy val timeV       = "2.16.0"
  lazy val scalaMacroV = "2.1.0"

  // // // // // // // // // //
  // //    Dependencies   // //
  // // // // // // // // // //

  lazy val testDeps = Seq(
    "org.scalatest" %% "scalatest" % "2.2.6" % Test
  )

  lazy val mainDeps = Seq( 
   "io.malcolmgreaves"      %% "s-util-fp"     % sutilV,
   "io.malcolmgreaves"      %% "data-tc-extra" % dataTcV,
   "com.github.nscala-time" %% "nscala-time"   % timeV,
   "com.propensive"         %% "rapture"       % raptureV,
   "com.github.ghik"        %% "silencer-lib"  % silencerV
  )

  // // // // // // // // // //
  // //      Plugins      // //
  // // // // // // // // // //

  lazy val scalaMacros =
    "org.scalamacros" % "paradise" % scalaMacroV cross CrossVersion.full

  lazy val warningSilencer = 
    "com.github.ghik" %% "silencer-plugin" % silencerV
 
}
