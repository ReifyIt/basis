/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

import scala.reflect.macros.Context

private[memory] object DataMacros {
  def load[R : c.WeakTypeTag]
      (c: Context { type PrefixType <: Data })
      (address: c.Expr[Long])
      (field: c.Expr[ValType[_]]): c.Expr[R] = {
    c.Expr {
      new DataMacros[c.type](c).loadField(field, c.prefix.tree, address.tree)
    } (c.weakTypeTag[R])
  }
  
  def store[T]
      (c: Context { type PrefixType <: Data })
      (address: c.Expr[Long], value: c.Expr[T])
      (field: c.Expr[ValType[_]]): c.Expr[Unit] = {
    c.Expr {
      new DataMacros[c.type](c).storeField(field, c.prefix.tree, address.tree, value.tree)
    } (c.WeakTypeTag.Unit)
  }
  
  def load2[T1, T2, R : c.WeakTypeTag]
      (c: Context { type PrefixType <: Data })
      (address: c.Expr[Long])
      (f: c.Expr[(T1, T2) => R])
      (field1: c.Expr[ValType[_]], field2: c.Expr[ValType[_]]): c.Expr[R] = {
    c.Expr {
      new DataMacros[c.type](c).load(
        field1 :: field2 :: Nil,
        c.prefix.tree, address.tree, f.tree)
    } (c.weakTypeTag[R])
  }
  
  def store2[T1, T2]
      (c: Context { type PrefixType <: Data })
      (address: c.Expr[Long])
      (value1: c.Expr[T1], value2: c.Expr[T2])
      (field1: c.Expr[ValType[_]], field2: c.Expr[ValType[_]]): c.Expr[Unit] = {
    c.Expr {
      new DataMacros[c.type](c).store(
        field1 :: field2 :: Nil,
        value1 :: value2 :: Nil,
        c.prefix.tree, address.tree)
    } (c.WeakTypeTag.Unit)
  }
  
  def load3[T1, T2, T3, R : c.WeakTypeTag]
      (c: Context { type PrefixType <: Data })
      (address: c.Expr[Long])
      (f: c.Expr[(T1, T2, T3) => R])
      (field1: c.Expr[ValType[_]], field2: c.Expr[ValType[_]],
       field3: c.Expr[ValType[_]]): c.Expr[R] = {
    c.Expr {
      new DataMacros[c.type](c).load(
        field1 :: field2 :: field3 :: Nil,
        c.prefix.tree, address.tree, f.tree)
    } (c.weakTypeTag[R])
  }
  
  def store3[T1, T2, T3]
      (c: Context { type PrefixType <: Data })
      (address: c.Expr[Long])
      (value1: c.Expr[T1], value2: c.Expr[T2],
       value3: c.Expr[T3])
      (field1: c.Expr[ValType[_]], field2: c.Expr[ValType[_]],
       field3: c.Expr[ValType[_]]): c.Expr[Unit] = {
    c.Expr {
      new DataMacros[c.type](c).store(
        field1 :: field2 :: field3 :: Nil,
        value1 :: value2 :: value3 :: Nil,
        c.prefix.tree, address.tree)
    } (c.WeakTypeTag.Unit)
  }
  
  def load4[T1, T2, T3, T4, R : c.WeakTypeTag]
      (c: Context { type PrefixType <: Data })
      (address: c.Expr[Long])
      (f: c.Expr[(T1, T2, T3, T4) => R])
      (field1: c.Expr[ValType[_]], field2: c.Expr[ValType[_]],
       field3: c.Expr[ValType[_]], field4: c.Expr[ValType[_]]): c.Expr[R] = {
    c.Expr {
      new DataMacros[c.type](c).load(
        field1 :: field2 :: field3 :: field4 :: Nil,
        c.prefix.tree, address.tree, f.tree)
    } (c.weakTypeTag[R])
  }
  
  def store4[T1, T2, T3, T4]
      (c: Context { type PrefixType <: Data })
      (address: c.Expr[Long])
      (value1: c.Expr[T1], value2: c.Expr[T2],
       value3: c.Expr[T3], value4: c.Expr[T4])
      (field1: c.Expr[ValType[_]], field2: c.Expr[ValType[_]],
       field3: c.Expr[ValType[_]], field4: c.Expr[ValType[_]]): c.Expr[Unit] = {
    c.Expr {
      new DataMacros[c.type](c).store(
        field1 :: field2 :: field3 :: field4 :: Nil,
        value1 :: value2 :: value3 :: value4 :: Nil,
        c.prefix.tree, address.tree)
    } (c.WeakTypeTag.Unit)
  }
}

private[memory] final class DataMacros[C <: Context](val context: C) {
  import context.{Expr, fresh, mirror}
  import universe._
  import ValType._
  
  val universe: context.universe.type = context.universe
  
  val PackedByteTpe = mirror.staticModule("basis.memory.ValType.PackedByte").moduleClass.asType.toType
  val PackedShortTpe = mirror.staticModule("basis.memory.ValType.PackedShort").moduleClass.asType.toType
  val PackedIntTpe = mirror.staticModule("basis.memory.ValType.PackedInt").moduleClass.asType.toType
  val PackedLongTpe = mirror.staticModule("basis.memory.ValType.PackedLong").moduleClass.asType.toType
  val PackedFloatTpe = mirror.staticModule("basis.memory.ValType.PackedFloat").moduleClass.asType.toType
  val PackedDoubleTpe = mirror.staticModule("basis.memory.ValType.PackedDouble").moduleClass.asType.toType
  val PackedBooleanTpe = mirror.staticModule("basis.memory.ValType.PackedBoolean").moduleClass.asType.toType
  val PaddedShortTpe = mirror.staticModule("basis.memory.ValType.PaddedShort").moduleClass.asType.toType
  val PaddedIntTpe = mirror.staticModule("basis.memory.ValType.PaddedInt").moduleClass.asType.toType
  val PaddedLongTpe = mirror.staticModule("basis.memory.ValType.PaddedLong").moduleClass.asType.toType
  val PaddedFloatTpe = mirror.staticModule("basis.memory.ValType.PaddedFloat").moduleClass.asType.toType
  val PaddedDoubleTpe = mirror.staticModule("basis.memory.ValType.PaddedDouble").moduleClass.asType.toType
  
  def load(fieldList: List[Expr[ValType[_]]], data: Tree, address: Tree, f: Tree): Tree = {
    val pointer = newTermName(fresh("pointer$"))
    var fields = fieldList
    val loads = List.newBuilder[Tree]
    loads += loadField(fields.head, data, address)
    var base = 0L
    while (!fields.tail.isEmpty) {
      val increment = (sizeOf(fields.head), alignOf(fields.tail.head)) match {
        case (Literal(Constant(offset: Long)), Literal(Constant(alignment: Long))) if base >= 0L =>
          val delta = align(base + offset, alignment) - base
          base += delta
          Apply(Select(Ident(pointer), "$plus"),
                Literal(Constant(delta)) :: Nil)
        case (offset, alignment) =>
          base = -1L
          Apply(Select(Ident(pointer), "$plus"),
                Apply(Select(Select(Select(Ident("basis"), "memory"), nme.PACKAGE), "align"),
                      Apply(Select(Apply(Select(Ident(pointer), "$minus"), address :: Nil),
                                   "$plus"), offset :: Nil) :: alignment :: Nil) :: Nil)
      }
      loads += Block(Assign(Ident(pointer), increment) :: Nil,
                     loadField(fields.tail.head, data, Ident(pointer)))
      fields = fields.tail
    }
    Block(ValDef(Modifiers(Flag.MUTABLE), pointer, TypeTree(), address) :: Nil,
          Apply(f, loads.result))
  }
  
  def store(fieldList: List[Expr[ValType[_]]], valueList: List[Expr[_]], data: Tree, address: Tree): Tree = {
    val pointer = newTermName(fresh("pointer$"))
    var fields = fieldList
    var values = valueList
    val stores = List.newBuilder[Tree]
    stores += storeField(fields.head, data, address, values.head.tree)
    var base = 0L
    while (!fields.tail.isEmpty && !values.tail.isEmpty) {
      val increment = (sizeOf(fields.head), alignOf(fields.tail.head)) match {
        case (Literal(Constant(offset: Long)), Literal(Constant(alignment: Long))) if base >= 0L =>
          val delta = align(base + offset, alignment) - base
          base += delta
          Apply(Select(Ident(pointer), "$plus"),
                Literal(Constant(delta)) :: Nil)
        case (offset, alignment) =>
          base = -1L
          Apply(Select(Ident(pointer), "$plus"),
                Apply(Select(Select(Select(Ident("basis"), "memory"), nme.PACKAGE), "align"),
                      Apply(Select(Apply(Select(Ident(pointer), "$minus"), address :: Nil),
                                   "$plus"), offset :: Nil) :: alignment :: Nil) :: Nil)
      }
      stores += Block(Assign(Ident(pointer), increment) :: Nil,
                      storeField(fields.tail.head, data, Ident(pointer), values.tail.head.tree))
      fields = fields.tail
      values = values.tail
    }
    Block(ValDef(Modifiers(Flag.MUTABLE), pointer, TypeTree(), address) :: stores.result,
          Literal(Constant(())))
  }
  
  def loadField(field: Expr[ValType[_]], data: Tree, address: Tree): Tree = {
    val fieldTpe = field.actualType
    if (fieldTpe =:= PackedByteTpe)
      Apply(Select(data, "loadByte"), address :: Nil)
    else if (fieldTpe =:= PackedShortTpe)
      Apply(Select(data, "loadUnalignedShort"), address :: Nil)
    else if (fieldTpe =:= PackedIntTpe)
      Apply(Select(data, "loadUnalignedInt"), address :: Nil)
    else if (fieldTpe =:= PackedLongTpe)
      Apply(Select(data, "loadUnalignedLong"), address :: Nil)
    else if (fieldTpe =:= PackedFloatTpe)
      Apply(Select(data, "loadUnalignedFloat"), address :: Nil)
    else if (fieldTpe =:= PackedDoubleTpe)
      Apply(Select(data, "loadUnalignedDouble"), address :: Nil)
    else if (fieldTpe =:= PackedBooleanTpe)
      Apply(Select(Apply(Select(data, "loadByte"), address :: Nil), "$eq$eq"),
            Literal(Constant(0)) :: Nil)
    else if (fieldTpe =:= PaddedShortTpe)
      Apply(Select(data, "loadShort"), address :: Nil)
    else if (fieldTpe =:= PaddedIntTpe)
      Apply(Select(data, "loadInt"), address :: Nil)
    else if (fieldTpe =:= PaddedLongTpe)
      Apply(Select(data, "loadLong"), address :: Nil)
    else if (fieldTpe =:= PaddedFloatTpe)
      Apply(Select(data, "loadFloat"), address :: Nil)
    else if (fieldTpe =:= PaddedDoubleTpe)
      Apply(Select(data, "loadDouble"), address :: Nil)
    else
      Apply(Select(field.tree, "load"), data :: address :: Nil)
  }
  
  def storeField(field: Expr[ValType[_]], data: Tree, address: Tree, value: Tree): Tree = {
    val fieldTpe = field.actualType
    if (fieldTpe =:= PackedByteTpe)
      Apply(Select(data, "storeByte"), address :: value :: Nil)
    else if (fieldTpe =:= PackedShortTpe)
      Apply(Select(data, "storeUnalignedShort"), address :: value :: Nil)
    else if (fieldTpe =:= PackedIntTpe)
      Apply(Select(data, "storeUnalignedInt"), address :: value :: Nil)
    else if (fieldTpe =:= PackedLongTpe)
      Apply(Select(data, "storeUnalignedLong"), address :: value :: Nil)
    else if (fieldTpe =:= PackedFloatTpe)
      Apply(Select(data, "storeUnalignedFloat"), address :: value :: Nil)
    else if (fieldTpe =:= PackedDoubleTpe)
      Apply(Select(data, "storeUnalignedDouble"), address :: value :: Nil)
    else if (fieldTpe =:= PackedBooleanTpe)
      Apply(Select(data, "storeByte"),
            address :: If(value, Literal(Constant(0.toByte)), Literal(Constant(-1.toByte))) :: Nil)
    else if (fieldTpe =:= PaddedShortTpe)
      Apply(Select(data, "storeShort"), address :: value :: Nil)
    else if (fieldTpe =:= PaddedIntTpe)
      Apply(Select(data, "storeInt"), address :: value :: Nil)
    else if (fieldTpe =:= PaddedLongTpe)
      Apply(Select(data, "storeLong"), address :: value :: Nil)
    else if (fieldTpe =:= PaddedFloatTpe)
      Apply(Select(data, "storeFloat"), address :: value :: Nil)
    else if (fieldTpe =:= PaddedDoubleTpe)
      Apply(Select(data, "storeDouble"), address :: value :: Nil)
    else
      Apply(Select(field.tree, "store"), data :: address :: value :: Nil)
  }
  
  def alignOf(field: Expr[ValType[_]]): Tree = {
    val fieldTpe = field.actualType
    if (fieldTpe =:= PackedByteTpe   ||
        fieldTpe =:= PackedShortTpe  ||
        fieldTpe =:= PackedIntTpe    ||
        fieldTpe =:= PackedLongTpe   ||
        fieldTpe =:= PackedFloatTpe  ||
        fieldTpe =:= PackedDoubleTpe ||
        fieldTpe =:= PackedBooleanTpe)
      Literal(Constant(1L))
    else if (fieldTpe =:= PaddedShortTpe)
      Literal(Constant(2L))
    else if (fieldTpe =:= PaddedIntTpe   ||
             fieldTpe =:= PaddedFloatTpe)
      Literal(Constant(4L))
    else if (fieldTpe =:= PaddedLongTpe  ||
             fieldTpe =:= PaddedDoubleTpe)
      Literal(Constant(8L))
    else
      Select(field.tree, "alignment")
  }
  
  def sizeOf(field: Expr[ValType[_]]): Tree = {
    val fieldTpe = field.actualType
    if (fieldTpe =:= PackedByteTpe ||
        fieldTpe =:= PackedBooleanTpe)
      Literal(Constant(1L))
    else if (fieldTpe =:= PackedShortTpe ||
             fieldTpe =:= PaddedShortTpe)
      Literal(Constant(2L))
    else if (fieldTpe =:= PackedIntTpe   ||
             fieldTpe =:= PaddedIntTpe   ||
             fieldTpe =:= PackedFloatTpe ||
             fieldTpe =:= PaddedFloatTpe)
      Literal(Constant(4L))
    else if (fieldTpe =:= PackedLongTpe   ||
             fieldTpe =:= PaddedLongTpe   ||
             fieldTpe =:= PackedDoubleTpe ||
             fieldTpe =:= PaddedDoubleTpe)
      Literal(Constant(8L))
    else
      Select(field.tree, "size")
  }
}
