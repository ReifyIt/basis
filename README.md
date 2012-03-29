### Mission

This library intends to serve as a basis for building client application frameworks in Scala. The library focuses on providing well designed solutions to common problems. The motivation for this project stems from a belief that good performance and high level design aren't mutually exclusive goals. By its nature Scala casts doubt on long-lived dogmas of software design–principally among them the extant view that design doesn't matter. I believe that Scala is the catalyst of a coming renaissance in software design. Design is the compression of complexity into its fundamental state. Shepherded by design, Scala leads the way towards compressing the bloated mess that is today's software landscape into something much smaller and much simpler. And much more powerful.

### Design principles

These ideas guide the development of this library:

1. Components should be simple, composable, and performant. Power comes from composability. Composability requires simplicity. And simplicity helps composability and performance. It's a virtuous cycle.
2. Be bold in making design decisions. Many decisions are easy for the developer of a library to make but difficult for the library's users to make. Make it easy for users to do the right thing.
3. One size does not fit all. Making it easy for users to do the right thing doesn't require you to force them to do it your way.

The process of design involves balancing these principles so that they all apply. A first attempt at solving a problem often only satisfies one of these goals. It takes wisdom and diligence to arrive at a solution satisfying all three. I stake no claims to success in this endeavor. But I will always try.

### Core packages

The library so far includes the following packages. Each core package may be moved to a separate subproject in the future.

- \[[API](http://scalabasis.github.com/api/#basis.collection.package)\] [basis.collection](https://github.com/scalabasis/basis/wiki/basis.collection) – contains collections that optionally store their elements by value.
- \[[API](http://scalabasis.github.com/api/#basis.memory.package)\] [basis.memory](https://github.com/scalabasis/basis/wiki/basis.memory) – contains an abstract memory model with value typeclasses.

### Packages in development

- basis.algebra – contains abstractions over mathematical spaces with efficient implementations.
- basis.geometry – contains geometric abstractions and implementations.
- basis.signal – contains generic images supporting high-level operations.
- basis.graphics – contains a generic rendering and compositing pipeline eventually capable of hardware acceleration.

### Future packages

I hope to start on these packages in the near future. It might makes sense to form a new project around them.

- basis.typography – will contain a text layout engine.
- basis.input – will contain a user input framework.
- basis.ui – will contain user interface controls.
