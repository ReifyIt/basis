//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.memory

abstract class ProxyFramer extends Framer with ProxyWriter {
  protected override def underlying: Framer

  override def size: Long = underlying.size

  override def expect(count: Long): this.type = { underlying.expect(count); this }

  override def clear(): Unit = underlying.clear()
}
