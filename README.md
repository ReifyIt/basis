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
- \[[API](http://scalabasis.github.com/latest/api/#basis.algebra.package)\] [basis.algebra](https://github.com/scalabasis/basis/wiki/basis.algebra) – implements efficient, family-polymorphic mathematical structures.
- \[[API](http://scalabasis.github.com/latest/api/#basis.json.package)\] [basis.json](https://github.com/scalabasis/basis/wiki/basis.json) – supports compile-time [JSON](http://www.json.org/) string interpolation and pattern matching, and jquery-style tree selectors.
