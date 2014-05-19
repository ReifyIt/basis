//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.util

import org.scalatest._
import scala.reflect._

class ArrowOpsSpec extends FlatSpec with Matchers {
  override def suiteName = "ArrowOps specification"

  "The ArrowToOps implicit conversion" should "add the -> macro to concrete types" in {
    withClue("(Byte, Int):")    ((0.toByte  -> 1) should equal ((0.toByte,  1)))
    withClue("(Short, Int):")   ((0.toShort -> 1) should equal ((0.toShort, 1)))
    withClue("(Char, Int):")    ((0.toChar  -> 1) should equal ((0.toChar,  1)))
    withClue("(Int, Int):")     ((0         -> 1) should equal ((0,         1)))
    withClue("(Long, Int):")    ((0L        -> 1) should equal ((0L,        1)))
    withClue("(Float, Int):")   ((0.0F      -> 1) should equal ((0.0F,      1)))
    withClue("(Double, Int):")  ((0.0       -> 1) should equal ((0.0,       1)))
    withClue("(Boolean, Int):") ((true      -> 1) should equal ((true,      1)))
    withClue("(Unit, Int):")    ((()        -> 1) should equal (((),        1)))
    withClue("(String, Int):")  ((""        -> 1) should equal (("",        1)))
  }

  it should "add the →  macro to concrete types" in {
    withClue("(Byte, Int):")    ((0.toByte  → 1) should equal ((0.toByte,  1)))
    withClue("(Short, Int):")   ((0.toShort → 1) should equal ((0.toShort, 1)))
    withClue("(Char, Int):")    ((0.toChar  → 1) should equal ((0.toChar,  1)))
    withClue("(Int, Int):")     ((0         → 1) should equal ((0,         1)))
    withClue("(Long, Int):")    ((0L        → 1) should equal ((0L,        1)))
    withClue("(Float, Int):")   ((0.0F      → 1) should equal ((0.0F,      1)))
    withClue("(Double, Int):")  ((0.0       → 1) should equal ((0.0,       1)))
    withClue("(Boolean, Int):") ((true      → 1) should equal ((true,      1)))
    withClue("(Unit, Int):")    ((()        → 1) should equal (((),        1)))
    withClue("(String, Int):")  ((""        → 1) should equal (("",        1)))
  }

  it should "add the -> macro to polymorphic types" in {
    def pair[A, B](x: A, y: B): (A, B) = x -> y
    pair(0, 1) should equal ((0, 1))
  }

  it should "add the →  macro to polymorphic types" in {
    def pair[A, B](x: A, y: B): (A, B) = x → y
    pair(0, 1) should equal ((0, 1))
  }

  "The -> macro" should "return specialized pairs" in {
    withClue("(Int, Int):")       ((0   -> 1)   shouldBe a (Manifest.classType[(Int, Int)]      ((0,   1  ).getClass)))
    withClue("(Int, Long):")      ((0   -> 1L)  shouldBe a (Manifest.classType[(Int, Long)]     ((0,   1L ).getClass)))
    withClue("(Int, Double):")    ((0   -> 1.0) shouldBe a (Manifest.classType[(Int, Double)]   ((0,   1.0).getClass)))
    withClue("(Long, Int):")      ((0L  -> 1)   shouldBe a (Manifest.classType[(Long, Int)]     ((0L,  1  ).getClass)))
    withClue("(Long, Long):")     ((0L  -> 1L)  shouldBe a (Manifest.classType[(Long, Long)]    ((0L,  1L ).getClass)))
    withClue("(Long, Double):")   ((0L  -> 1.0) shouldBe a (Manifest.classType[(Long, Double)]  ((0L,  1.0).getClass)))
    withClue("(Double, Int):")    ((0.0 -> 1)   shouldBe a (Manifest.classType[(Double, Int)]   ((0.0, 1  ).getClass)))
    withClue("(Double, Long):")   ((0.0 -> 1L)  shouldBe a (Manifest.classType[(Double, Long)]  ((0.0, 1L ).getClass)))
    withClue("(Double, Double):") ((0.0 -> 1.0) shouldBe a (Manifest.classType[(Double, Double)]((0.0, 1.0).getClass)))
  }

  it should "unwrap ArrowOps l-values" in {
    def pair[A, B](lhs: ArrowOps[A], rhs: B): (A, B) = lhs -> rhs
    pair(ArrowToOps(0), 1) should equal ((0, 1))
  }

  "The →  macro" should "return specialized pairs" in {
    withClue("(Int, Int):")       ((0   → 1)   shouldBe a (Manifest.classType[(Int, Int)]      ((0,   1  ).getClass)))
    withClue("(Int, Long):")      ((0   → 1L)  shouldBe a (Manifest.classType[(Int, Long)]     ((0,   1L ).getClass)))
    withClue("(Int, Double):")    ((0   → 1.0) shouldBe a (Manifest.classType[(Int, Double)]   ((0,   1.0).getClass)))
    withClue("(Long, Int):")      ((0L  → 1)   shouldBe a (Manifest.classType[(Long, Int)]     ((0L,  1  ).getClass)))
    withClue("(Long, Long):")     ((0L  → 1L)  shouldBe a (Manifest.classType[(Long, Long)]    ((0L,  1L ).getClass)))
    withClue("(Long, Double):")   ((0L  → 1.0) shouldBe a (Manifest.classType[(Long, Double)]  ((0L,  1.0).getClass)))
    withClue("(Double, Int):")    ((0.0 → 1)   shouldBe a (Manifest.classType[(Double, Int)]   ((0.0, 1  ).getClass)))
    withClue("(Double, Long):")   ((0.0 → 1L)  shouldBe a (Manifest.classType[(Double, Long)]  ((0.0, 1L ).getClass)))
    withClue("(Double, Double):") ((0.0 → 1.0) shouldBe a (Manifest.classType[(Double, Double)]((0.0, 1.0).getClass)))
  }

  it should "unwrap ArrowOps l-values" in {
    def pair[A, B](lhs: ArrowOps[A], rhs: B): (A, B) = lhs → rhs
    pair(ArrowToOps(0), 1) should equal ((0, 1))
  }
}
