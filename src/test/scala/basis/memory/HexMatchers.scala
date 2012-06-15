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
  def equalByte(right: Byte) = new Matcher[Byte] {
    def apply(left: Byte) = MatchResult(
      left == right,
      "0x%02X did not equal 0x%02X".format(left, right),
      "0x%02X equaled 0x%02X".format(left, right)
    )
  }
  
  def equalShort(right: Short) = new Matcher[Short] {
    def apply(left: Short) = MatchResult(
      left == right,
      "0x%04X did not equal 0x%04X".format(left, right),
      "0x%04X equaled 0x%04X".format(left, right)
    )
  }
  
  def equalInt(right: Int) = new Matcher[Int] {
    def apply(left: Int) = MatchResult(
      left == right,
      "0x%08X did not equal 0x%08X".format(left, right),
      "0x%08X equaled 0x%08X".format(left, right)
    )
  }
  
  def equalLong(right: Long) = new Matcher[Long] {
    def apply(left: Long) = MatchResult(
      left == right,
      "0x%016X did not equal 0x%016X".format(left, right),
      "0x%016X equaled 0x%016X".format(left, right)
    )
  }
  
  def equalChar(right: Char) = new Matcher[Char] {
    def apply(left: Char) = MatchResult(
      left == right,
      "0x%04X did not equal 0x%04X".format(left, right),
      "0x%04X equaled 0x%04X".format(left, right)
    )
  }
  
  def equalFloat(right: Float) = new Matcher[Float] {
    def apply(left: Float) = MatchResult(
      floatToRawIntBits(left) == floatToRawIntBits(right),
      "0x%08X did not equal 0x%08X".format(floatToRawIntBits(left), floatToRawIntBits(right)),
      "0x%08X equaled 0x%08X".format(floatToRawIntBits(left), floatToRawIntBits(right))
    )
  }
  
  def equalDouble(right: Double) = new Matcher[Double] {
    def apply(left: Double) = MatchResult(
      doubleToRawLongBits(left) == doubleToRawLongBits(right),
      "0x%016X did not equal 0x%016X".format(doubleToRawLongBits(left), doubleToRawLongBits(right)),
      "0x%016X equaled 0x%016X".format(doubleToRawLongBits(left), doubleToRawLongBits(right))
    )
  }
}

object HexMatchers extends HexMatchers
