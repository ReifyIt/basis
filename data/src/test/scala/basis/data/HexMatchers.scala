//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis.util._
import org.scalatest.matchers._
import scala.Predef.augmentString

trait HexMatchers {
  def equalByte(right: Byte) = new Matcher[Byte] {
    override def apply(left: Byte) = MatchResult(
      left == right,
      f"0x$left%02X did not equal 0x$right%02X",
      f"0x$left%02X equaled 0x$right%02X"
    )
  }

  def equalShort(right: Short) = new Matcher[Short] {
    override def apply(left: Short) = MatchResult(
      left == right,
      f"0x$left%04X did not equal 0x$right%04X",
      f"0x$left%04X equaled 0x$right%04X"
    )
  }

  def equalInt(right: Int) = new Matcher[Int] {
    override def apply(left: Int) = MatchResult(
      left == right,
      f"0x$left%08X did not equal 0x$right%08X",
      f"0x$left%08X equaled 0x$right%08X"
    )
  }

  def equalLong(right: Long) = new Matcher[Long] {
    override def apply(left: Long) = MatchResult(
      left == right,
      f"0x$left%016X did not equal 0x$right%016X",
      f"0x$left%016X equaled 0x$right%016X"
    )
  }

  def equalFloat(right: Float) = new Matcher[Float] {
    override def apply(left: Float) = MatchResult(
      left.toRawIntBits == right.toRawIntBits,
      f"0x${left.toRawIntBits}%08X did not equal 0x${right.toRawIntBits}%08X",
      f"0x${left.toRawIntBits}%08X equaled 0x${right.toRawIntBits}%08X"
    )
  }

  def equalDouble(right: Double) = new Matcher[Double] {
    override def apply(left: Double) = MatchResult(
      left.toRawLongBits == right.toRawLongBits,
      f"0x${left.toRawLongBits}%016X did not equal 0x${right.toRawLongBits}%016X",
      f"0x${left.toRawLongBits}%016X equaled 0x${right.toRawLongBits}%016X"
    )
  }
}

object HexMatchers extends HexMatchers
