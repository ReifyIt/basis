//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import basis.util._
import org.scalatest._

trait DeltaVariantBehaviors extends Matchers { this: FlatSpec =>
  def DeltaVariantImplementation(variant: DeltaVariant): Unit = {
    s"$variant delta transforms" should behave like DeltaTransforms(variant)
    s"$variant delta object states" should behave like DeltaObjectStates(variant)
  }

  def DeltaTransforms(variant: DeltaVariant): Unit = {
    import variant._

    it should "delta primitive forms" in {
      def delta(a: AnyForm, b: AnyForm): Unit = a delta b should equal (b)
      def deltaFrom(a: AnyForm): Unit = {
        delta(a, TextForm.empty)
        delta(a, DataForm.empty)
        delta(a, NumberForm(0))
        delta(a, DateForm.now)
        delta(a, TrueForm)
        delta(a, FalseForm)
        delta(a, NullForm)
        delta(a, NoForm)
      }
      deltaFrom(TextForm.empty)
      deltaFrom(DataForm.empty)
      deltaFrom(NumberForm(0))
      deltaFrom(DateForm.now)
      deltaFrom(TrueForm)
      deltaFrom(FalseForm)
      deltaFrom(NullForm)
      deltaFrom(NoForm)
    }

    it should "patch primitive forms" in {
      def patch(a: AnyForm, b: AnyForm): Unit = a patch b should equal (b)
      def patchFrom(a: AnyForm): Unit = {
        patch(a, TextForm.empty)
        patch(a, DataForm.empty)
        patch(a, NumberForm(0))
        patch(a, DateForm.now)
        patch(a, TrueForm)
        patch(a, FalseForm)
        patch(a, NullForm)
        patch(a, NoForm)
      }
      patchFrom(TextForm.empty)
      patchFrom(DataForm.empty)
      patchFrom(NumberForm(0))
      patchFrom(DateForm.now)
      patchFrom(TrueForm)
      patchFrom(FalseForm)
      patchFrom(NullForm)
      patchFrom(NoForm)
    }

    it should "delta objects with added fields" in {
      val x = ObjectForm("a" -> NumberForm(1), "b" -> NumberForm(2))
      val y = ObjectForm("a" -> NumberForm(1), "b" -> NumberForm(2), "c" -> NumberForm(3))
      x delta y should equal (ObjectDelta("c" -> NumberForm(3)))
    }

    it should "patch objects with added fields" in {
      val x = ObjectForm("a" -> NumberForm(1), "b" -> NumberForm(2))
      val y = ObjectForm("a" -> NumberForm(1), "b" -> NumberForm(2), "c" -> NumberForm(3))
      x patch (x delta y) should equal (y)
    }

    it should "delta objects with removed fields" in {
      val x = ObjectForm("a" -> NumberForm(1), "b" -> NumberForm(2), "c" -> NumberForm(3))
      val y = ObjectForm("a" -> NumberForm(1), "c" -> NumberForm(3))
      x delta y should equal (ObjectDelta("b" -> NoForm))
    }

    it should "patch objects with removed fields" in {
      val x = ObjectForm("a" -> NumberForm(1), "b" -> NumberForm(2), "c" -> NumberForm(3))
      val y = ObjectForm("a" -> NumberForm(1), "c" -> NumberForm(3))
      x patch (x delta y) should equal (y)
    }

    it should "delta objects with single differing fields" in {
      val x = ObjectForm("a" -> NumberForm(1), "b" -> NumberForm(2), "c" -> NumberForm(3))
      val y = ObjectForm("a" -> NumberForm(1), "b" -> TextForm("2"), "c" -> NumberForm(3))
      x delta y should equal (ObjectDelta("b" -> TextForm("2")))
      y delta x should equal (ObjectDelta("b" -> NumberForm(2)))
    }

    it should "patch objects with single differing fields" in {
      val x = ObjectForm("a" -> NumberForm(1), "b" -> NumberForm(2), "c" -> NumberForm(3))
      val y = ObjectForm("a" -> NumberForm(1), "b" -> TextForm("2"), "c" -> NumberForm(3))
      x patch (x delta y) should equal (y)
      y patch (y delta x) should equal (x)
    }

    it should "delta objects with multiple differing fields" in {
      val x = ObjectForm("a" -> NumberForm(1), "b" -> NumberForm(2), "c" -> NumberForm(3))
      val y = ObjectForm("a" -> TextForm("1"), "b" -> NumberForm(2), "c" -> TextForm("3"))
      x delta y should equal (ObjectDelta("a" -> TextForm("1"), "c" -> TextForm("3")))
      y delta x should equal (ObjectDelta("a" -> NumberForm(1), "c" -> NumberForm(3)))
    }

    it should "patch objects with multiple differing fields" in {
      val x = ObjectForm("a" -> NumberForm(1), "b" -> NumberForm(2), "c" -> NumberForm(3))
      val y = ObjectForm("a" -> TextForm("1"), "b" -> NumberForm(2), "c" -> TextForm("3"))
      x patch (x delta y) should equal (y)
      y patch (y delta x) should equal (x)
    }

    it should "delta objects with differing, added, and removed fields" in {
      val x = ObjectForm("a" -> NumberForm(1), "b" -> NumberForm(2), "c" -> NumberForm(3))
      val y = ObjectForm("b" -> NumberForm(2), "c" -> TextForm("3"), "d" -> NumberForm(4))
      x delta y should equal (ObjectDelta("a" -> NoForm, "c" -> TextForm("3"), "d" -> NumberForm(4)))
      y delta x should equal (ObjectDelta("a" -> NumberForm(1), "c" -> NumberForm(3), "d" -> NoForm))
    }

    it should "patch objects with differing, added, and removed fields" in {
      val x = ObjectForm("a" -> NumberForm(1), "b" -> NumberForm(2), "c" -> NumberForm(3))
      val y = ObjectForm("b" -> NumberForm(2), "c" -> TextForm("3"), "d" -> NumberForm(4))
      x patch (x delta y) should equal (y)
      y patch (y delta x) should equal (x)
    }

    it should "recursively delta nested objects" in {
      val x = ObjectForm("a" -> NumberForm(1), "b" -> ObjectForm("x" -> TrueForm, "y" -> FalseForm), "c" -> NumberForm(3))
      val y = ObjectForm("a" -> NumberForm(1), "b" -> ObjectForm("x" -> FalseForm, "y" -> FalseForm), "c" -> NumberForm(3))
      x delta y should equal (ObjectDelta("b" -> ObjectDelta("x" -> FalseForm)))
      y delta x should equal (ObjectDelta("b" -> ObjectDelta("x" -> TrueForm)))
    }

    it should "recursively patch nested objects" in {
      val x = ObjectForm("a" -> NumberForm(1), "b" -> ObjectForm("x" -> TrueForm, "y" -> FalseForm), "c" -> NumberForm(3))
      val y = ObjectForm("a" -> NumberForm(1), "b" -> ObjectForm("x" -> FalseForm, "y" -> FalseForm), "c" -> NumberForm(3))
      x patch (x delta y) should equal (y)
      y patch (y delta x) should equal (x)
    }

    it should "delta sets with added elements" in {
      val x = SetForm(NumberForm(1), NumberForm(2))
      val y = SetForm(NumberForm(1), NumberForm(2), NumberForm(3))
      x delta y should equal (SetDelta(additions = SetForm(NumberForm(3))))
    }

    it should "patch sets with added elements" in {
      val x = SetForm(NumberForm(1), NumberForm(2))
      val y = SetForm(NumberForm(1), NumberForm(2), NumberForm(3))
      x patch (x delta y) should equal (y)
    }

    it should "delta sets with removed elements" in {
      val x = SetForm(NumberForm(1), NumberForm(2), NumberForm(3))
      val y = SetForm(NumberForm(1), NumberForm(3))
      x delta y should equal (SetDelta(deletions = SetForm(NumberForm(2))))
    }

    it should "patch sets with removed elements" in {
      val x = SetForm(NumberForm(1), NumberForm(2), NumberForm(3))
      val y = SetForm(NumberForm(1), NumberForm(3))
      x patch (x delta y) should equal (y)
    }

    it should "delta sets with added and removed elements" in {
      val x = SetForm(NumberForm(1), NumberForm(2), NumberForm(3))
      val y = SetForm(NumberForm(2), NumberForm(3), NumberForm(4))
      x delta y should equal (SetDelta(deletions = SetForm(NumberForm(1)), additions = SetForm(NumberForm(4))))
      y delta x should equal (SetDelta(deletions = SetForm(NumberForm(4)), additions = SetForm(NumberForm(1))))
    }

    it should "patch sets with added and removed elements" in {
      val x = SetForm(NumberForm(1), NumberForm(2), NumberForm(3))
      val y = SetForm(NumberForm(2), NumberForm(3), NumberForm(4))
      x patch (x delta y) should equal (y)
      y patch (y delta x) should equal (x)
    }

    it should "delta object from primitives" in {
      val b = ObjectForm("x" -> NumberForm(1), "y" -> NumberForm(2))
      val c = NoForm.delta(b)
      val d = ObjectDelta("x" -> NumberForm(1), "y" -> NumberForm(2))
      withClue("isObjectDelta:") (c.isObjectDelta should be (true))
      c should equal (d)
    }

    it should "patch primitives with object deltas" in {
      val b = ObjectDelta("x" -> NumberForm(1), "y" -> NoForm, "z" -> NullForm)
      val c = ObjectForm("x" -> NumberForm(1), "z" -> NullForm)
      TextForm.empty patch b should equal (c)
      DataForm.empty patch b should equal (c)
      NumberForm(0) patch b should equal (c)
      DateForm.now patch b should equal (c)
      TrueForm patch b should equal (c)
      FalseForm patch b should equal (c)
      NullForm patch b should equal (c)
      NoForm patch b should equal (c)
    }

    it should "delta sets from primitives" in {
      val b = SetForm(TextForm("x"), TextForm("y"))
      val c = NoForm.delta(b)
      val d = SetDelta(additions = b)
      withClue("isSetDelta:") (c.isSetDelta should be (true))
      c should equal (d)
    }

    it should "patch primitives with set deltas" in {
      val c = SetForm(TextForm("x"), TextForm("z"))
      val d = SetDelta(SetForm(TextForm("y")), c)
      TextForm.empty patch d should equal (c)
      DataForm.empty patch d should equal (c)
      NumberForm(0) patch d should equal (c)
      DateForm.now patch d should equal (c)
      TrueForm patch d should equal (c)
      FalseForm patch d should equal (c)
      NullForm patch d should equal (c)
      NoForm patch d should equal (c)
    }

    it should "recursively create missing nested objects when patching primitives" in {
      val x = ObjectForm("a" -> NumberForm(1))
      val d = ObjectDelta("b" -> ObjectDelta("x" -> TrueForm, "y" -> FalseForm))
      val y = ObjectForm("a" -> NumberForm(1), "b" -> ObjectForm("x" -> TrueForm, "y" -> FalseForm))
      x patch d should equal (y)
    }
  }

  def DeltaObjectStates(variant: DeltaVariant): Unit = {
    import variant._

    it should "track added fields" in {
      var form = ObjectState("a" -> NumberForm(1))
      form += ("b", NumberForm(2))
      form.delta should equal (ObjectDelta("b" -> NumberForm(2)))
    }

    it should "track changed fields" in {
      var form = ObjectState("a" -> NumberForm(1), "b" -> NumberForm(2))
      form += ("b", TextForm("2"))
      form.delta should equal (ObjectDelta("b" -> TextForm("2")))
    }

    it should "track removed fields" in {
      var form = ObjectState("a" -> NumberForm(1), "b" -> NumberForm(2))
      form -= "b"
      form.delta should equal (ObjectDelta("b" -> NoForm))
    }

    it should "track multi-step changes" in {
      var form = ObjectState("a" -> NumberForm(1), "b" -> NumberForm(2))
      form += ("b", TextForm("2"))
      form += ("c", NumberForm(3))
      form.delta should equal (ObjectDelta("b" -> TextForm("2"), "c" -> NumberForm(3)))
    }

    it should "track transformations" in {
      var form = ObjectState("a" -> NumberForm(1), "b" -> NumberForm(2), "c" -> NumberForm(3))
      form = form.map {
        case (key, value: NumberForm) if value.toInt % 2 == 0 => key -> TextForm(value.toInt.toString)
        case field => field
      } (form.builder)
      form.delta should equal (ObjectDelta("b" -> TextForm("2")))
    }

    it should "merge changes" in {
      var x = ObjectState("a" -> NumberForm(1), "b" -> NumberForm(2), "c" -> NumberForm(3))
      var y = x
      x -= "a"
      x += ("b", TextForm("2"))
      x += ("c", NullForm)
      y += ("a", TextForm("1"))
      y -= "b"
      y += ("c", TextForm("3"))
      y += ("d", TextForm("4"))
      val z = x merge y
      z.state should equal (ObjectState("a" -> TextForm("1"), "c" -> TextForm("3"), "d" -> TextForm("4")))
      z.delta should equal (ObjectDelta("a" -> TextForm("1"), "b" -> NoForm, "c" -> TextForm("3"), "d" -> TextForm("4")))
    }

    it should "commit changes" in {
      val x = ObjectState("a" -> NumberForm(1), "b" -> NumberForm(2))
      val y = (x + ("b", TextForm("2"))).commit
      y.state should equal (ObjectForm("a" -> NumberForm(1), "b" -> TextForm("2")))
      y.delta should equal (ObjectDelta.empty)
    }

    it should "revert changes" in {
      val x = ObjectState("a" -> NumberForm(1), "b" -> NumberForm(2))
      val y = (x + ("b", TextForm("2"))).revert
      x should equal (y)
    }
  }
}
