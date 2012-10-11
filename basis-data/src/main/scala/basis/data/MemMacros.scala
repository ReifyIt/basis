/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.data

import scala._
import scala.reflect.macros.Context

private[data] object MemMacros {
  def load[T](c: Context)(address: c.Expr[Long])
      (field: c.Expr[ValType[_]]): c.Expr[T] = {
    import c.universe._
    val Apply(_, mem :: Nil) = c.prefix.tree
    c.Expr {
      new MemMacros[c.type](c).loadField(field, mem, address.tree)
    } (WeakTypeTag.Nothing)
  }
  
  def store[T](c: Context)(address: c.Expr[Long], value: c.Expr[T])
      (field: c.Expr[ValType[_]]): c.Expr[Unit] = {
    import c.universe._
    val Apply(_, mem :: Nil) = c.prefix.tree
    c.Expr {
      new MemMacros[c.type](c).storeField(field, mem, address.tree, value.tree)
    } (WeakTypeTag.Unit)
  }
  
  def load2[T1, T2, R](c: Context)(address: c.Expr[Long])
      (f: c.Expr[(T1, T2) => R])
      (field1: c.Expr[ValType[_]], field2: c.Expr[ValType[_]]): c.Expr[R] = {
    import c.universe._
    val Apply(_, mem :: Nil) = c.prefix.tree
    c.Expr {
      new MemMacros[c.type](c).load(
        field1 :: field2 :: Nil,
        mem, address.tree, f.tree)
    } (WeakTypeTag.Nothing)
  }
  
  def store2[T1, T2](c: Context)(address: c.Expr[Long])
      (value1: c.Expr[T1], value2: c.Expr[T2])
      (field1: c.Expr[ValType[_]], field2: c.Expr[ValType[_]]): c.Expr[Unit] = {
    import c.universe._
    val Apply(_, mem :: Nil) = c.prefix.tree
    c.Expr {
      new MemMacros[c.type](c).store(
        field1 :: field2 :: Nil,
        value1 :: value2 :: Nil,
        mem, address.tree)
    } (WeakTypeTag.Unit)
  }
  
  def load3[T1, T2, T3, R](c: Context)(address: c.Expr[Long])
      (f: c.Expr[(T1, T2, T3) => R])
      (field1: c.Expr[ValType[_]], field2: c.Expr[ValType[_]],
       field3: c.Expr[ValType[_]]): c.Expr[R] = {
    import c.universe._
    val Apply(_, mem :: Nil) = c.prefix.tree
    c.Expr {
      new MemMacros[c.type](c).load(
      field1 :: field2 :: field3 :: Nil,
      mem, address.tree, f.tree)
    } (WeakTypeTag.Nothing)
  }
  
  def store3[T1, T2, T3](c: Context)(address: c.Expr[Long])
      (value1: c.Expr[T1], value2: c.Expr[T2],
       value3: c.Expr[T3])
      (field1: c.Expr[ValType[_]], field2: c.Expr[ValType[_]],
       field3: c.Expr[ValType[_]]): c.Expr[Unit] = {
    import c.universe._
    val Apply(_, mem :: Nil) = c.prefix.tree
    c.Expr {
      new MemMacros[c.type](c).store(
        field1 :: field2 :: field3 :: Nil,
        value1 :: value2 :: value3 :: Nil,
        mem, address.tree)
    } (WeakTypeTag.Unit)
  }
  
  def load4[T1, T2, T3, T4, R](c: Context)(address: c.Expr[Long])
      (f: c.Expr[(T1, T2, T3, T4) => R])
      (field1: c.Expr[ValType[_]], field2: c.Expr[ValType[_]],
       field3: c.Expr[ValType[_]], field4: c.Expr[ValType[_]]): c.Expr[R] = {
    import c.universe._
    val Apply(_, mem :: Nil) = c.prefix.tree
    c.Expr {
      new MemMacros[c.type](c).load(
        field1 :: field2 :: field3 :: field4 :: Nil,
        mem, address.tree, f.tree)
    } (WeakTypeTag.Nothing)
  }
  
  def store4[T1, T2, T3, T4](c: Context)(address: c.Expr[Long])
      (value1: c.Expr[T1], value2: c.Expr[T2],
       value3: c.Expr[T3], value4: c.Expr[T4])
      (field1: c.Expr[ValType[_]], field2: c.Expr[ValType[_]],
       field3: c.Expr[ValType[_]], field4: c.Expr[ValType[_]]): c.Expr[Unit] = {
    import c.universe._
    val Apply(_, mem :: Nil) = c.prefix.tree
    c.Expr {
      new MemMacros[c.type](c).store(
        field1 :: field2 :: field3 :: field4 :: Nil,
        value1 :: value2 :: value3 :: value4 :: Nil,
        mem, address.tree)
    } (WeakTypeTag.Unit)
  }
}

private[data] class MemMacros[C <: Context](val context: C) {
  import context.universe._
  import ValType._
  
  val PackedByteTpe = rootMirror.staticModule("basis.data.ValType.PackedByte").moduleClass.asType.toType
  val PackedShortTpe = rootMirror.staticModule("basis.data.ValType.PackedShort").moduleClass.asType.toType
  val PackedIntTpe = rootMirror.staticModule("basis.data.ValType.PackedInt").moduleClass.asType.toType
  val PackedLongTpe = rootMirror.staticModule("basis.data.ValType.PackedLong").moduleClass.asType.toType
  val PackedFloatTpe = rootMirror.staticModule("basis.data.ValType.PackedFloat").moduleClass.asType.toType
  val PackedDoubleTpe = rootMirror.staticModule("basis.data.ValType.PackedDouble").moduleClass.asType.toType
  val PackedBooleanTpe = rootMirror.staticModule("basis.data.ValType.PackedBoolean").moduleClass.asType.toType
  val PaddedShortTpe = rootMirror.staticModule("basis.data.ValType.PaddedShort").moduleClass.asType.toType
  val PaddedIntTpe = rootMirror.staticModule("basis.data.ValType.PaddedInt").moduleClass.asType.toType
  val PaddedLongTpe = rootMirror.staticModule("basis.data.ValType.PaddedLong").moduleClass.asType.toType
  val PaddedFloatTpe = rootMirror.staticModule("basis.data.ValType.PaddedFloat").moduleClass.asType.toType
  val PaddedDoubleTpe = rootMirror.staticModule("basis.data.ValType.PaddedDouble").moduleClass.asType.toType
  
  def load(fieldList: List[Expr[ValType[_]]], mem: Tree, address: Tree, f: Tree): Tree = {
    val pointer = context.fresh(newTermName("pointer$"))
    var fields = fieldList
    val loads = List.newBuilder[Tree]
    loads += loadField(fields.head, mem, address)
    var base = 0L
    while (!fields.tail.isEmpty) {
      val increment = (sizeOf(fields.head), alignOf(fields.tail.head)) match {
        case (Literal(Constant(offset: Long)), Literal(Constant(alignment: Long))) if base >= 0L =>
          val delta = align(base + offset, alignment) - base
          base += delta
          Apply(Select(Ident(pointer), newTermName("$plus")),
                Literal(Constant(delta)) :: Nil)
        case (offset, alignment) =>
          base = -1L
          Apply(Select(Ident(pointer), newTermName("$plus")),
                Apply(Select(Select(Select(Ident("basis"), newTermName("data")), nme.PACKAGE), newTermName("align")),
                      Apply(Select(Apply(Select(Ident(pointer), newTermName("$minus")), address :: Nil),
                                   newTermName("$plus")), offset :: Nil) :: alignment :: Nil) :: Nil)
      }
      loads += Block(Assign(Ident(pointer), increment) :: Nil,
                     loadField(fields.tail.head, mem, Ident(pointer)))
      fields = fields.tail
    }
    Block(ValDef(Modifiers(Flag.MUTABLE), pointer, TypeTree(), address) :: Nil,
          Apply(f, loads.result))
  }
  
  def store(fieldList: List[Expr[ValType[_]]], valueList: List[Expr[_]], mem: Tree, address: Tree): Tree = {
    val pointer = context.fresh(newTermName("pointer$"))
    var fields = fieldList
    var values = valueList
    val stores = List.newBuilder[Tree]
    stores += storeField(fields.head, mem, address, values.head.tree)
    var base = 0L
    while (!fields.tail.isEmpty && !values.tail.isEmpty) {
      val increment = (sizeOf(fields.head), alignOf(fields.tail.head)) match {
        case (Literal(Constant(offset: Long)), Literal(Constant(alignment: Long))) if base >= 0L =>
          val delta = align(base + offset, alignment) - base
          base += delta
          Apply(Select(Ident(pointer), newTermName("$plus")),
                Literal(Constant(delta)) :: Nil)
        case (offset, alignment) =>
          base = -1L
          Apply(Select(Ident(pointer), newTermName("$plus")),
                Apply(Select(Select(Select(Ident("basis"), newTermName("data")), nme.PACKAGE), newTermName("align")),
                      Apply(Select(Apply(Select(Ident(pointer), newTermName("$minus")), address :: Nil),
                                   newTermName("$plus")), offset :: Nil) :: alignment :: Nil) :: Nil)
      }
      stores += Block(Assign(Ident(pointer), increment) :: Nil,
                      storeField(fields.tail.head, mem, Ident(pointer), values.tail.head.tree))
      fields = fields.tail
      values = values.tail
    }
    Block(ValDef(Modifiers(Flag.MUTABLE), pointer, TypeTree(), address) :: stores.result,
          Literal(Constant(())))
  }
  
  def loadField(field: Expr[ValType[_]], mem: Tree, address: Tree): Tree = {
    val fieldTpe = field.actualType
    if (fieldTpe =:= PackedByteTpe)
      Apply(Select(mem, newTermName("loadByte")), address :: Nil)
    else if (fieldTpe =:= PackedShortTpe)
      Apply(Select(mem, newTermName("loadUnalignedShort")), address :: Nil)
    else if (fieldTpe =:= PackedIntTpe)
      Apply(Select(mem, newTermName("loadUnalignedInt")), address :: Nil)
    else if (fieldTpe =:= PackedLongTpe)
      Apply(Select(mem, newTermName("loadUnalignedLong")), address :: Nil)
    else if (fieldTpe =:= PackedFloatTpe)
      Apply(Select(mem, newTermName("loadUnalignedFloat")), address :: Nil)
    else if (fieldTpe =:= PackedDoubleTpe)
      Apply(Select(mem, newTermName("loadUnalignedDouble")), address :: Nil)
    else if (fieldTpe =:= PackedBooleanTpe)
      Apply(Select(Apply(Select(mem, newTermName("loadByte")), address :: Nil),
                   newTermName("$eq$eq")),
            Literal(Constant(0)) :: Nil)
    else if (fieldTpe =:= PaddedShortTpe)
      Apply(Select(mem, newTermName("loadShort")), address :: Nil)
    else if (fieldTpe =:= PaddedIntTpe)
      Apply(Select(mem, newTermName("loadInt")), address :: Nil)
    else if (fieldTpe =:= PaddedLongTpe)
      Apply(Select(mem, newTermName("loadLong")), address :: Nil)
    else if (fieldTpe =:= PaddedFloatTpe)
      Apply(Select(mem, newTermName("loadFloat")), address :: Nil)
    else if (fieldTpe =:= PaddedDoubleTpe)
      Apply(Select(mem, newTermName("loadDouble")), address :: Nil)
    else
      Apply(Select(field.tree, newTermName("load")), mem :: address :: Nil)
  }
  
  def storeField(field: Expr[ValType[_]], mem: Tree, address: Tree, value: Tree): Tree = {
    val fieldTpe = field.actualType
    if (fieldTpe =:= PackedByteTpe)
      Apply(Select(mem, newTermName("storeByte")), address :: value :: Nil)
    else if (fieldTpe =:= PackedShortTpe)
      Apply(Select(mem, newTermName("storeUnalignedShort")), address :: value :: Nil)
    else if (fieldTpe =:= PackedIntTpe)
      Apply(Select(mem, newTermName("storeUnalignedInt")), address :: value :: Nil)
    else if (fieldTpe =:= PackedLongTpe)
      Apply(Select(mem, newTermName("storeUnalignedLong")), address :: value :: Nil)
    else if (fieldTpe =:= PackedFloatTpe)
      Apply(Select(mem, newTermName("storeUnalignedFloat")), address :: value :: Nil)
    else if (fieldTpe =:= PackedDoubleTpe)
      Apply(Select(mem, newTermName("storeUnalignedDouble")), address :: value :: Nil)
    else if (fieldTpe =:= PackedBooleanTpe)
      Apply(Select(mem, newTermName("storeByte")),
            address :: If(value, Literal(Constant(0.toByte)), Literal(Constant(-1.toByte))) :: Nil)
    else if (fieldTpe =:= PaddedShortTpe)
      Apply(Select(mem, newTermName("storeShort")), address :: value :: Nil)
    else if (fieldTpe =:= PaddedIntTpe)
      Apply(Select(mem, newTermName("storeInt")), address :: value :: Nil)
    else if (fieldTpe =:= PaddedLongTpe)
      Apply(Select(mem, newTermName("storeLong")), address :: value :: Nil)
    else if (fieldTpe =:= PaddedFloatTpe)
      Apply(Select(mem, newTermName("storeFloat")), address :: value :: Nil)
    else if (fieldTpe =:= PaddedDoubleTpe)
      Apply(Select(mem, newTermName("storeDouble")), address :: value :: Nil)
    else
      Apply(Select(field.tree, newTermName("store")), mem :: address :: value :: Nil)
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
      Select(field.tree, newTermName("alignment"))
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
      Select(field.tree, newTermName("size"))
  }
}
