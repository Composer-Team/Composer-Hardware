package composer.Platforms.ASIC

object ProcessCorner extends Enumeration {
  val Typical, Fast, Slow = Value
  type ProcessCorner = Value
}

object ProcessTemp extends Enumeration {
  val C25, C0, CM40, C125 = Value
  type ProcessTemp = Value
}

object ProcessVoltageThreshold extends Enumeration {
  val High, Regular, Low, SuperLow = Value
  type ProcessVoltageThreshold = Value
}

object ProcessOperatingConditions extends Enumeration {
  type ProcessOperatingConditions = Value
  val NormalVoltage, LowVoltage = Value
}
