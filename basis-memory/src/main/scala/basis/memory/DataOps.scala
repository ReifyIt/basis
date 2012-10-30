/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

/** `Data` extensions to efficiently load and store compound values.
  * 
  * @groupname  compound  Loading and storing compound values
  * @groupprio  compound  -2
  * 
  * @groupname  array     Loading and storing arrays of values
  * @groupprio  array     -1
  */
final class DataOps(self: Data) {
  /** Loads an instance from a data value.
    * 
    * @tparam T         the instance type to load.
    * @param  address   the aligned address to load.
    * @param  field     the implicit value type to load.
    * @return the loaded instance.
    * @group  compound
    */
  def load[T](address: Long)(implicit field: ValType[T]): T =
    macro DataMacros.load[T]
  
  /** Stores an instance as a data value.
    * 
    * @tparam T         the instance type to store.
    * @param  address   the aligned storage address.
    * @param  value     the instance to store.
    * @param  field     the implicit value type to store.
    * @group  compound
    */
  def store[T](address: Long, value: T)(implicit field: ValType[T]): Unit =
    macro DataMacros.store[T]
  
  /** Loads and unpacks a struct as two values.
    * @group compound */
  def load2[T1, T2, R](address: Long)
      (f: (T1, T2) => R)
      (implicit field1: ValType[T1], field2: ValType[T2]): R =
    macro DataMacros.load2[T1, T2, R]
  
  /** Packs and stores two values as a struct.
    * @group compound */
  def store2[T1, T2](address: Long)
      (value1: T1, value2: T2)
      (implicit field1: ValType[T1], field2: ValType[T2]): Unit =
    macro DataMacros.store2[T1, T2]
  
  /** Loads and unpacks a struct as three values.
    * @group compound */
  def load3[T1, T2, T3, R](address: Long)
      (f: (T1, T2, T3) => R)
      (implicit field1: ValType[T1], field2: ValType[T2],
                field3: ValType[T3]): R =
    macro DataMacros.load3[T1, T2, T3, R]
  
  /** Packs and stores three values as a struct.
    * @group compound */
  def store3[T1, T2, T3](address: Long)
      (value1: T1, value2: T2, value3: T3)
      (implicit field1: ValType[T1], field2: ValType[T2],
                field3: ValType[T3]): Unit =
    macro DataMacros.store3[T1, T2, T3]
  
  /** Loads and unpacks a struct as four values.
    * @group compound */
  def load4[T1, T2, T3, T4, R](address: Long)
      (f: (T1, T2, T3, T4) => R)
      (implicit field1: ValType[T1], field2: ValType[T2],
                field3: ValType[T3], field4: ValType[T4]): R =
    macro DataMacros.load4[T1, T2, T3, T4, R]
  
  /** Packs and stores four values as a struct.
    * @group compound */
  def store4[T1, T2, T3, T4](address: Long)
      (value1: T1, value2: T2, value3: T3, value4: T4)
      (implicit field1: ValType[T1], field2: ValType[T2],
                field3: ValType[T3], field4: ValType[T4]): Unit =
    macro DataMacros.store4[T1, T2, T3, T4]
  
  /** Loads a sequence of data values into a new array.
    * 
    * @tparam T         the instance type to load.
    * @param  address   the aligned address to load.
    * @param  count     the number of values to load.
    * @param  field     the implicit value type to load.
    * @param  tag       the reflective type of the array to load.
    * @return the loaded array of instance values.
    * @group  array
    */
  def loadArray[T]
      (address: Long, count: Int)
      (implicit field: ValType[T], tag: scala.reflect.ClassTag[T])
    : Array[T] = {
    val array = tag.newArray(count)
    copyToArray[T](address, array, 0, count)
    array
  }
  
  /** Copies a sequence of loaded data values to an array slice.
    * 
    * @tparam T         the instance type to load.
    * @param  address   the aligned address to load.
    * @param  array     the array to copy to.
    * @param  start     the lower bound of the array slice to copy to.
    * @param  count     the number of values to copy.
    * @param  field     the implicit value type to load.
    * @group  array
    */
  def copyToArray[T]
      (address: Long, array: Array[T], start: Int, count: Int)
      (implicit field: ValType[T]) {
    val end = start + count
    var p = address
    var i = start
    while (i < end) {
      array(i) = field.load(self, p)
      p += field.size
      i += 1
    }
  }
  
  /** Stores an array slice as a sequence of data values.
    * 
    * @tparam T         the instance type to store.
    * @param  address   the aligned storage address.
    * @param  array     the array to store from.
    * @param  start     the lower bound of the array slice to store from.
    * @param  count     the number of values to store.
    * @param  field     the implicit value type to store.
    * @group  array
    */
  def storeArray[T]
      (address: Long, array: Array[T], start: Int, count: Int)
      (implicit field: ValType[T]) {
    val end = start + count
    var p = address
    var i = start
    while (i < end) {
      field.store(self, p, array(i))
      p += field.size
      i += 1
    }
  }
}
