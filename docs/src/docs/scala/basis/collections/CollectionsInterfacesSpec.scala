package basis.docs.collections

import basis.collections._
import org.scalatest._

//# IntRange_class
case class IntRange(lower: Int, upper: Int) extends Traverser[Int] {
  override def traverse(f: Int => Unit): Unit = {
    var i = lower
    while (i <= upper) { f(i); i += 1 }
  }
}
//# IntRange_class

class CollectionsInterfacesSpec extends WordSpec {
  import Matchers._

  override def suiteName = "Collections interfaces documentation specification"

  "IntRange should traverse consecutive integers" in {
    IntRange(1, 3).map(Predef.identity)(Seq.Builder) should equal (Seq(1, 2, 3))
  }
}
