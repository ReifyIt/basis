/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class IntegerSpec extends FunSpec with ShouldMatchers {
  override def suiteName = "binary integer specification"
  
  describe("binary integer conversion") {
    it("should convert Long values") {
      Integer(0L).toLong  should equal (0L)
      Integer(1L).toLong  should equal (1L)
      Integer(-1L).toLong should equal (-1L)
      Integer(Long.MinValue).toLong should equal (Long.MinValue)
      Integer(Long.MaxValue).toLong should equal (Long.MaxValue)
    }
    
    it("should convert base-10 string values") {
      Integer("0").toString should equal ("0")
      Integer("1").toString should equal ("1")
      Integer("-1").toString should equal ("-1")
      Integer("10").toString should equal ("10")
      Integer("-10").toString should equal ("-10")
      Integer("123456789").toString should equal ("123456789")
      Integer("987654321").toString should equal ("987654321")
    }
  }
  
  describe("binary integer arithmetic") {
    it("(x /% y) should return (quotient, remainder)") {
      (Integer(13L) /% Integer(5L)) should equal ((Integer(2L), Integer(3L)))
    }
    
    it("(x / y) should return the quotient") {
      (Integer(13L) / Integer(5L)) should equal (Integer(2L))
    }
    
    it("(x % y) should return the remainder") {
      (Integer(13L) % Integer(5L)) should equal (Integer(3L))
    }
    
    it("(-1 >> 1) should equal -1") {
      (Integer(-1L) >> 1) should equal (Integer(-1L))
    }
    
    it("(-1 >>> 1) should equal 0") {
      (Integer(-1L) >>> 1) should equal (Integer.zero)
    }
    
    val xs = Seq(
      "0", "1", "-1",
      "5555555555555555",
      "AAAAAAAAAAAAAAAA",
      "7FFFFFFFFFFFFFFF",
      "-8000000000000000",
      "FEDCBA9876543210",
      "FEDEDCDCBCBABA9A98987876765654543432321210"
    )
    
    for (x <- xs map (Integer(_, 16))) {
      it should behave like ObeysIdentities(x)
    }
  }
  
  def ObeysIdentities(n: Integer) {
    describe("for n = "+ n.toString(16) +" (base 16)") {
      it("(n + 0) should equal n") {
        (n + Integer.zero) should equal (n)
      }
      
      it("(n - 0) should equal n") {
        (n - Integer.zero) should equal (n)
      }
      
      it("(n + k - k) should equal n") {
        val ks = Seq("1", "-1", "9223372036854775807", "-9223372036854775808")
        for (k <- ks map (Integer(_))) {
          (n + k - k) should equal (n)
        }
      }
      
      it("(n + n - n) should equal n") {
        (n + n - n) should equal (n)
      }
      
      it("(-(-n)) should equal n") {
        -(-n) should equal (n)
      }
      
      it("(n - n) should equal 0") {
        (n - n) should equal (Integer.zero)
      }
      
      it("(n * 1) should equal n") {
        (n * Integer.unit) should equal (n)
      }
      
      it("(n / 1) should equal n") {
        (n / Integer.unit) should equal (n)
      }
      
      it("(n * k / k) should equal n") {
        val ks = Seq("2", "-2", "9223372036854775807", "-9223372036854775808")
        for (k <- ks map (Integer(_))) {
          (n * k / k) should equal (n)
        }
      }
      
      it("(n * n / n) should equal n if n != 0") {
        if (n != Integer.zero) (n * n / n) should equal (n)
      }
      
      it("(sqrt(n * n)) should equal abs(n)") {
        (n * n).sqrt should equal (n.abs)
      }
      
      it("(n << k >> k) should equal n") {
        val ks = Seq(0, 1, 63, 64, 7919)
        for (k <- ks) {
          (n << k >> k) should equal (n)
        }
      }
      
      it("(n << k >>> k) should equal n") {
        val ks = Seq(0, 1, 63, 64, 7919)
        for (k <- ks) {
          (n << k >>> k) should equal (n)
        }
      }
      
      it("(n.scale(b, k).scale(b, -k)) should equal n if k >= 0") {
        val bs = Seq(2, 3, 10, 64)
        val ks = Seq(0, 1, 2, 10, 23)
        for (b <- bs; k <- ks) {
          n.scale(b, k).scale(b, -k) should equal (n)
        }
      }
    }
  }
}
