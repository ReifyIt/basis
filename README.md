### Mission

This library intends to serve as a basis for building Scala application frameworks. The library focuses on providing well designed solutions to common problems. The motivation for this project stems from a belief that good performance and high level design aren't mutually exclusive goals. By its nature Scala casts doubt on long-lived dogmas of software design–principally among them the extant view that design doesn't matter. I believe that Scala is the catalyst of a coming renaissance in software design. Design is the compression of complexity into its fundamental state. Shepherded by design, Scala leads the way towards compressing the bloated mess that is today's software landscape into something much smaller and much simpler. And much more powerful.

### Design principles

These ideas guide the development of this library:

1. Components should be simple, composable, and performant. Power comes from composability. Composability requires simplicity. And simplicity begets composability and performance. It's a virtuous cycle.
2. Be bold in making design decisions. Many decisions are easy for the developer of a library to make but difficult for the library's users to make. Make it easy for users to do the right thing.
3. One size does not fit all. Making it easy for users to do the right thing doesn't require you to force them to do it your way.

The process of design involves balancing these principles so that they all apply. A first attempt at solving a problem often only satisfies one of these goals. It takes wisdom and diligence to arrive at a solution satisfying all three. I stake no claims to success in this endeavor. But I will always try.

### Core packages

The library so far includes the following packages:

- \[[API](http://scalabasis.github.com/latest/api/#basis.memory.package)\] [basis.memory](https://github.com/scalabasis/basis/wiki/basis.memory) – implements an abstract memory model with struct typeclasses and store-by-value collections.
- \[[API](http://scalabasis.github.com/latest/api/#basis.json.package)\] [basis.json](https://github.com/scalabasis/basis/wiki/basis.json) – supports compile-time JSON interpolation and pattern matching, and jquery-style tree selectors.
- \[[API](http://scalabasis.github.com/latest/api/#basis.algebra.package)\] [basis.algebra](https://github.com/scalabasis/basis/wiki/basis.algebra) – implements efficient, family-polymorphic mathematical structures.

### Key Features

#### Value collections

Application that deal with large sets of small values (images, vertex arrays, etc.) may benefit from [store-by-value](https://github.com/scalabasis/basis/wiki/basis.memory) collections. Combining the expressiveness of Scala collections with the memory footprint of primitive arrays takes little effort on your part.

```scala
scala> val xs = RawSeq(1, 2, 3, 4, 5) // create a sequence of Int values.
xs: basis.memory.collection.RawSeq[Int] = ValBuffer(1, 2, 3, 4, 5) // stored by-value.

scala> val ys = xs map (_.toString) // map Ints to Strings.
ys: basis.memory.collection.RawSeq[java.lang.String] = RefBuffer(1, 2, 3, 4, 5) // stored by reference.

scala> val zs = ys map (_.toDouble) // map Strings to Doubles.
zs: basis.memory.collection.RawSeq[Double] = ValBuffer(1.0, 2.0, 3.0, 4.0, 5.0) // stored by value again.
```

#### JSON interpolation

Interpolating JSON text makes it easy to construct JSON trees. And because parsing happens at compile-time you can have confidence that your application will always produce valid results. [Efficient bytecode](https://github.com/scalabasis/basis/wiki/basis.json#wiki-Bytecode_generation) replaces the interpolated text.

```scala
def message(id: Int, content: String) = json""" {
  "id"      : $id,
  "content" : $content
} """
```

JSON trees are immutable so you can safely share them and cache them. Yet you can still easily make targeted updates to [selections](https://github.com/scalabasis/basis/wiki/basis.json#wiki-Selections).

```scala
scala> val wall = JSArray(message(0, "Hello"), message(1, "First post"))
wall: basis.json.JSArray = [{"id":0,"content":"Hello"},{"id":1,"content":"First post"}]

scala> for (post <- wall \\ +JSObject if post("id") == json"1") yield message(1, "[$redacted]\n")
res0: basis.json.JSArray = [{"id":0,"content":"Hello"},{"id":1,"content":"[$redacted]\n"}]
```
