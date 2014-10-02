//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis

sealed abstract class PartialComparison {
  def isComparable: Boolean = false
  def isEquivalent: Boolean = false
  def precedes: Boolean = false
  def succeeds: Boolean = false

  def toFloat: Float
}

object PartialComparison {
  def apply(value: Float): PartialComparison = {
    if (value < 0.0f) Precedes
    else if (value > 0.0f) Succeeds
    else if (value == 0.0f) Equivalent
    else Incomparable
  }
}

object Incomparable extends PartialComparison {
  override def toFloat: Float = Float.NaN

  override def toString: String = "Incomparable"
}

sealed abstract class Comparison extends PartialComparison {
  override def isComparable: Boolean = true

  def toInt: Int
}

object Comparison {
  def apply(value: Int): Comparison = {
    if (value < 0) Precedes
    else if (value > 0) Succeeds
    else Equivalent
  }
}

object Equivalent extends Comparison {
  override def isEquivalent: Boolean = true

  override def toInt: Int = 0

  override def toFloat: Float = 0.0f

  override def toString: String = "Equivalent"
}

object Precedes extends Comparison {
  override def precedes: Boolean = true

  override def toInt: Int = -1

  override def toFloat: Float = -1.0f

  override def toString: String = "Precedes"
}

object Succeeds extends Comparison {
  override def succeeds: Boolean = true

  override def toInt: Int = 1

  override def toFloat: Float = 1.0f

  override def toString: String = "Succeeds"
}
