//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import Predef.<:<

final class AllocatorOps[+Data](val __ : Allocator[Data]) extends AnyVal {
  def store[T](value: T)(implicit T: Struct[T], isStorer: Data <:< Storer): Data = {
    val data = __(T.size)
    T.store(data, 0L, value)
    data
  }
}
