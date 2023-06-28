package composer.Platforms.ASIC

import chipsalliance.rocketchip.config.Field


//noinspection ScalaUnusedSymbol
object ProcessCorner extends Enumeration {
  val Typical, Fast, Slow = Value
  type ProcessCorner = Value
}

//noinspection ScalaUnusedSymbol
object ProcessTemp extends Enumeration {
  val C25, CM40, C125 = Value
  type ProcessTemp = Value
}

//noinspection ScalaUnusedSymbol
object ProcessVoltageThreshold extends Enumeration {
  val High, Regular, Low, SuperLow = Value
  type ProcessVoltageThreshold = Value
}

//noinspection ScalaUnusedSymbol
object ProcessOperatingConditions extends Enumeration {
  type ProcessOperatingConditions = Value
  val NormalVoltage, LowVoltage = Value
}

case object BuildSynthesisKey extends Field[Boolean]
