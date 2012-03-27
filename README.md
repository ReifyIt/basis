Basis
=====

Value collections
--------------------------

The `basis.collection` module contains collections that optionally store their elements by value. `RawSeq` and its descendants build value sequences when an implicit `Struct` typeclass is available, and reference sequences otherwise. Here's an example:

```scala
scala> import basis.collection._
import basis.collection._

scala> val xs = RawSeq(1, 2, 3, 4, 5) // create a sequence of Int values.
xs: basis.collection.RawSeq[Int] = ValBuffer(1, 2, 3, 4, 5) // stored by-value.

scala> val ys = xs map (_.toString) // map Ints to Strings.
ys: basis.collection.RawSeq[java.lang.String] = RefBuffer(1, 2, 3, 4, 5) // stored by reference.

scala> val zs = ys map (java.lang.Double.valueOf(_)) // map Strings to Doubles.
zs: basis.collection.RawSeq[java.lang.Double] = RefBuffer(1.0, 2.0, 3.0, 4.0, 5.0) // stored by value again.
```

Memory abstraction
------------------

The `basis.memory` module contains a low-level memory abstraction and value typeclasses. `Data` objects model byte-addressable memory regions and `Allocator` objects abstract over `Data` allocation. `Struct` typeclasses implement store-by-value semantics for Scala types.

Let's look at some simple uses of `Data`:

```scala
scala> val data = Data.alloc[Int](4) // allocate Data for 4 Int values.
data: basis.memory.Data = Block4LE(16) // little-endian data backed by an Int array.

scala> data.storeInt(0L, 0x76543210) // store an Int value to address 0.

scala> data.loadInt(0L).toHexString // load an Int value from address 0.
res1: String = 76543210

scala> data.loadByte(0L).toHexString // load the low byte of our Int value.
res2: String = 10 // the least significant byte was stored first.

scala> data.loadShort(2L).toHexString // load the high bytes of our Int value.
res3: String = 7654

scala> data.loadUnalignedShort(1L).toHexString // load the middle bytes of our Int value.
res4: String = 5432

scala> data.storeDouble(8L, 2.0) // store a Double value.

scala> data.loadLong(8L).toHexString // load the bytes of our Double value.
res6: String = 3ff0000000000000
```

Now let's see an example using structs; tuples have structs pre-defined for them:

```scala
scala> val data = Data.alloc[(Int, Double)](8)
data: basis.memory.Data = Block8LE(128) // little-endian data backed by a Long array.

scala> data.store(0L, (2, math.sqrt(2)))

scala> data.load[(Int, Double)](0L)
res1: (Int, Double) = (2,1.4142135623730951)

scala> Struct[(Int, Double)] // what's the value type of this kind of tuple?
res2: basis.memory.Struct[(Int, Double)] = Record2(PaddedInt, PaddedDouble)

scala> implicit val Row = Struct.Record2[Int, Double] // cache the value type.
Row: basis.memory.Struct.Record2[Int,Double] = Record2(PaddedInt, PaddedDouble)

scala> data.load(0L)(Row.field2) // load just the second field of the tuple.
res3: Double = 1.4142135623730951
```

Here's how to define a `Struct` for a case class (use :paste mode in the REPL).

```scala
case class Vector3(x: Float, y: Float, z: Float)
implicit object Vector3 extends CaseStruct3[Float, Float, Float, Vector3]
```

And here are some ways you can use it:

```scala
scala> val data = Data.alloc[Vector3](1024)
data: basis.memory.Data = Block4LE(12288)

scala> data.store(0L, Vector3(1.0F, 2.0F, 3.0F))

scala> data.load[Vector3](0L)
res1: Vector3 = Vector3(1.0,2.0,3.0)

scaka> data.load(0L)(Vector3.field3) // load just the z-coordinate.
res2: Float = 3.0

scala> data.store(24L, (Vector3(0.0F, 3.0F, 4.0F), 5.0)) // store a tuple of a vector and a norm.

scala> data.load[(Vector3, Double)](24L)
res4: (Vector3, Double) = (Vector3(0.0,3.0,4.0),5.0)
```
