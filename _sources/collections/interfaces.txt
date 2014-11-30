.. _collections_interfaces:

Collections Interfaces
======================

.. _collections_traversers:

Traversers
----------

At the root of the Basis collections hierarchy sits the humble :class:`Traverser`,
interface, which declares just a single method.

.. code-block:: scala

  trait Traverser[+A] extends Any with Family[Traverser[_]] {
    def traverse(f: A => Unit): Unit
  }

But don't let :class:`Traverser`'s minimal makeup mislead you–there's more here
than meets the eye:

.. code-block:: scala

  scala> import basis.collections._
  import basis.collections._

  scala> val xs = Traverser(1, 2, 3)
  xs: basis.collections.Traverser[Int] = List(1, 2, 3)

  scala> val ys = xs.map(_.toString)
  ys: basis.collections.Traverser[String] = List("1", "2", "3")

.. _collections_traversers_impl:

Implementing a Traverser
^^^^^^^^^^^^^^^^^^^^^^^^

To implement a new :class:`Traverser`, simply extend the interface and fill-in
its :meth:`traverse` method:

.. includecode:: /scala/basis/collections/CollectionsInterfacesSpec.scala
   :include: IntRange_class

:class:`Traverser` deliberately avoids declaring a traditional :meth:`foreach`
method.  This leaves the namespace open for macros to fill-in precisely
defined traversal semantics.  You don't have to care about this though; just
use :meth:`foreach`, and other collection operators, like you normally would:

.. code-block:: scala

  scala> val xs = IntRange(1, 3)
  xs: IntRange = IntRange(1,3)

  scala> xs foreach Predef.println
  1
  2
  3

  scala> val sum = xs reduce (_ + _)
  sum: Int = 6

  scala> val ys = xs map (2.0 * _)
  ys: basis.collections.Traverser[Double] = List(2.0, 4.0, 6.0)
