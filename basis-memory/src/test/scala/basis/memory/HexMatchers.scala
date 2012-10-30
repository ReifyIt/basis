/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

import java.lang.Float.floatToRawIntBits
import java.lang.Double.doubleToRawLongBits

import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult

trait HexMatchers {
  import Predef._
  
  def equalByte(right: Byte) = new Matcher[Byte] {
    def apply(left: Byte) = MatchResult(
      left == right,
      f"0x$left%02X did not equal 0x$right%02X",
      f"0x$left%02X equaled 0x$right%02X"
    )
  }
  
  def equalShort(right: Short) = new Matcher[Short] {
    def apply(left: Short) = MatchResult(
      left == right,
      f"0x$left%04X did not equal 0x$right%04X",
      f"0x$left%04X equaled 0x$right%04X"
    )
  }
  
  def equalInt(right: Int) = new Matcher[Int] {
    def apply(left: Int) = MatchResult(
      left == right,
      f"0x$left%08X did not equal 0x$right%08X",
      f"0x$left%08X equaled 0x$right%08X"
    )
  }
  
  def equalLong(right: Long) = new Matcher[Long] {
    def apply(left: Long) = MatchResult(
      left == right,
      f"0x$left%016X did not equal 0x$right%016X",
      f"0x$left%016X equaled 0x$right%016X"
    )
  }
  
  def equalFloat(right: Float) = new Matcher[Float] {
    def apply(left: Float) = MatchResult(
      floatToRawIntBits(left) == floatToRawIntBits(right),
      f"0x${floatToRawIntBits(left)}%08X did not equal 0x${floatToRawIntBits(right)}%08X",
      f"0x${floatToRawIntBits(left)}%08X equaled 0x${floatToRawIntBits(right)}%08X"
    )
  }
  
  def equalDouble(right: Double) = new Matcher[Double] {
    def apply(left: Double) = MatchResult(
      doubleToRawLongBits(left) == doubleToRawLongBits(right),
      f"0x${doubleToRawLongBits(left)}%016X did not equal 0x${doubleToRawLongBits(right)}%016X",
      f"0x${doubleToRawLongBits(left)}%016X equaled 0x${doubleToRawLongBits(right)}%016X"
    )
  }
}

object HexMatchers extends HexMatchers
