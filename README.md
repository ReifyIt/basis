# Scala Basis

A set of _independent_ software packages that combine together to fill a space–this the Scala Basis framework strives to be. The name _Basis_ references the analogous concept from linear algebra. Like the _vectors_ of a basis, packages in this library aim for fundamental simplicity and independence. And like a _vector space_, the framework's usefulness derives from the combination of its orthogonal elements. Components are _designed_ together–not _tied_ together.

**Quick Overview**: [Algebra](#Algebra) – [Memory](#Memory) – [JSON](#JSON)

## Basis Components

### <a name="Algebra"/>\[[API](http://scalabasis.github.com/latest/api/#basis.algebra.package)\] [Algebra Basis](https://github.com/scalabasis/basis/wiki/Algebra-Basis)

Elegant math does not usually translate into elegant code. But this can change. The Algebra Basis library provides abstract algebraic interfaces with tightly coupled implementations. You can write generic mathematical code that performs well too. Here's a quick taste:

```scala
scala> def combine[V <: VectorSpace[_] with Singleton](a: V#Scalar, x: V#Vector, b: V#Scalar, y: V#Vector): V#Vector = a *: x + b *: y
combine: [V <: basis.algebra.VectorSpace[_] with Singleton](a: V#Scalar, x: V#Vector, b: V#Scalar, y: V#Vector)V#Vector

scala> combine[R2.type](3, R2(5, 7), 9, R2(0, 1)) // works with any vector space
res0: basis.algebra.binary64.R2.Vector = R2(15.0, 30.0)

scala> (R3 map R2)(1, 0, 0,  0, 1, 0) ⋅ (R2 map R3)(1, 0,  0, 1,  0, 0) // type safe matrix composition
res1: basis.algebra.binary64.RMxN[basis.algebra.binary64.R2.type,basis.algebra.binary64.R2.type]#Matrix = R2x2(1.0, 0.0,  0.0, 1.0)

scala> decimal.Real(2L).sqrt // arbitrary precision arithmetic (not a BigDecimal wrapper)
res2: basis.algebra.decimal.Real.Value = 1.414213562373095048801688724209698078569671875376948073176679738
```

To use the Algebra Basis library with Scala 2.10.0-M4 or later, add this to your SBT build definition:

```scala
libraryDependencies += "com.scalabasis" % "basis-algebra_2.10" % "0.0-SNAPSHOT"
```

### <a name="Memory"/>\[[API](http://scalabasis.github.com/latest/api/#basis.memory.package)\] [Memory Basis](https://github.com/scalabasis/basis/wiki/Memory-Basis)

Sometimes you need low-level memory access. The Memory Basis library enables this with little-to-no overhead. But beyond immitating C, Memory Basis lets you abstract over Struct types and Data implementations. This culminates in full-blown Scala collections that transparently store their elements by value. It works like this:

```scala
scala> val xs = RawSeq(1, 2, 3, 4, 5) // create a sequence of Int values.
xs: basis.memory.collection.RawSeq[Int] = ValBuffer(1, 2, 3, 4, 5) // stored by-value.

scala> val ys = xs map (_.toString) // map Ints to Strings.
ys: basis.memory.collection.RawSeq[java.lang.String] = RefBuffer(1, 2, 3, 4, 5) // stored by reference.

scala> val zs = ys map (_.toDouble) // map Strings to Doubles.
zs: basis.memory.collection.RawSeq[Double] = ValBuffer(1.0, 2.0, 3.0, 4.0, 5.0) // stored by value again.
```

To use the Memory Basis library with Scala 2.10.0-M4 or later, add this to your SBT build definition:

```scala
libraryDependencies += "com.scalabasis" % "basis-memory_2.10" % "0.0-SNAPSHOT"
```

### <a name="JSON"/>\[[API](http://scalabasis.github.com/latest/api/#basis.json.package)\] [JSON Basis](https://github.com/scalabasis/basis/wiki/JSON-Basis)

You can't hide from JSON; it's ubiquitous. But you can deal with it simply and elegantly. The JSON Basis library has a fast JSON parser. But why repeatedly parse templates at run-time when you can parse them once at compile-time? JSON Basis does this too. And it throws in jquery-style selectors to boot. Take a look:

```scala
def message(id: Int, content: String) = json""" {
  "id"      : $id,
  "content" : $content
} """
```

```scala
scala> val wall = JSArray(message(0, "Hello"), message(1, "First post"))
wall: basis.json.JSArray = [{"id":0,"content":"Hello"},{"id":1,"content":"First post"}]

scala> for (post <- wall \\ +JSObject if post("id") == json"1") yield message(1, "[$redacted]\n")
res0: basis.json.JSArray = [{"id":0,"content":"Hello"},{"id":1,"content":"[$redacted]\n"}]
```

To use the JSON Basis library with Scala 2.10.0-M4 or later, add this to your SBT build definition:

```scala
libraryDependencies += "com.scalabasis" % "basis-json_2.10" % "0.0-SNAPSHOT"
```
