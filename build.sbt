// SPDX-License-Identifier: CERN-OHL-S-2.0
ThisBuild / scalaVersion := "2.13.12"

val spinalVersion = "1.15.1"

lazy val root = (project in file("."))
  .settings(
    name := "fpga-onu",
    Compile / scalaSource := baseDirectory.value / "common" / "hw" / "spinal",
    Compile / unmanagedSourceDirectories += baseDirectory.value / "agilex5" / "hw" / "spinal",   // vendor wrappers
    Test / scalaSource := baseDirectory.value / "common" / "hw" / "test",
    libraryDependencies ++= Seq(
      "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion,
      "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion,
      compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)
    ),
    fork := true
  )
