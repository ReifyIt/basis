/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

import scala.reflect.makro.Context

private[memory] object DataMacros {
  def load[T](c: Context)(address: c.Expr[Long])
      (field: c.Expr[ValType[T]]): c.Expr[T] = {
    import c.universe._
    val Apply(_, data :: Nil) = c.prefix.tree
    c.Expr(new DataMacros[c.type](c).loadField(field, data, address.tree))
  }
  
  def store[T](c: Context)(address: c.Expr[Long], value: c.Expr[T])
      (field: c.Expr[ValType[T]]): c.Expr[Unit] = {
    import c.universe._
    val Apply(_, data :: Nil) = c.prefix.tree
    c.Expr(new DataMacros[c.type](c).storeField(field, data, address.tree, value.tree))
  }
  
  def load2[T1, T2, R](c: Context)(address: c.Expr[Long])
      (f: c.Expr[(T1, T2) => R])
      (field1: c.Expr[ValType[T1]], field2: c.Expr[ValType[T2]]): c.Expr[R] = {
    import c.universe._
    val Apply(_, data :: Nil) = c.prefix.tree
    c.Expr(new DataMacros[c.type](c).load(
      List(field1, field2),
      data, address.tree, f.tree))
  }
  
  def store2[T1, T2](c: Context)(address: c.Expr[Long])
      (value1: c.Expr[T1], value2: c.Expr[T2])
      (field1: c.Expr[ValType[T1]], field2: c.Expr[ValType[T2]]): c.Expr[Unit] = {
    import c.universe._
    val Apply(_, data :: Nil) = c.prefix.tree
    c.Expr(new DataMacros[c.type](c).store(
      List(field1, field2),
      List(value1, value2),
      data, address.tree))
  }
  
  def load3[T1, T2, T3, R](c: Context)(address: c.Expr[Long])
      (f: c.Expr[(T1, T2, T3) => R])
      (field1: c.Expr[ValType[T1]], field2: c.Expr[ValType[T2]],
       field3: c.Expr[ValType[T3]]): c.Expr[R] = {
    import c.universe._
    val Apply(_, data :: Nil) = c.prefix.tree
    c.Expr(new DataMacros[c.type](c).load(
      List(field1, field2, field3),
      data, address.tree, f.tree))
  }
  
  def store3[T1, T2, T3](c: Context)(address: c.Expr[Long])
      (value1: c.Expr[T1], value2: c.Expr[T2],
       value3: c.Expr[T3])
      (field1: c.Expr[ValType[T1]], field2: c.Expr[ValType[T2]],
       field3: c.Expr[ValType[T3]]): c.Expr[Unit] = {
    import c.universe._
    val Apply(_, data :: Nil) = c.prefix.tree
    c.Expr(new DataMacros[c.type](c).store(
      List(field1, field2, field3),
      List(value1, value2, value3),
      data, address.tree))
  }
  
  def load4[T1, T2, T3, T4, R](c: Context)(address: c.Expr[Long])
      (f: c.Expr[(T1, T2, T3, T4) => R])
      (field1: c.Expr[ValType[T1]], field2: c.Expr[ValType[T2]],
       field3: c.Expr[ValType[T3]], field4: c.Expr[ValType[T4]]): c.Expr[R] = {
    import c.universe._
    val Apply(_, data :: Nil) = c.prefix.tree
    c.Expr(new DataMacros[c.type](c).load(
      List(field1, field2, field3, field4),
      data, address.tree, f.tree))
  }
  
  def store4[T1, T2, T3, T4](c: Context)(address: c.Expr[Long])
      (value1: c.Expr[T1], value2: c.Expr[T2],
       value3: c.Expr[T3], value4: c.Expr[T4])
      (field1: c.Expr[ValType[T1]], field2: c.Expr[ValType[T2]],
       field3: c.Expr[ValType[T3]], field4: c.Expr[ValType[T4]]): c.Expr[Unit] = {
    import c.universe._
    val Apply(_, data :: Nil) = c.prefix.tree
    c.Expr(new DataMacros[c.type](c).store(
      List(field1, field2, field3, field4),
      List(value1, value2, value3, value4),
      data, address.tree))
  }
}

private[memory] class DataMacros[C <: Context](val context: C) {
  val universe: context.universe.type = context.universe
  val mirror: context.mirror.type = context.mirror
  
  import universe._
  import mirror._
  import ValType._
  
  val PackedByteTpe = ThisType(staticModule("basis.memory.ValType.PackedByte").moduleClass)
  val PackedShortTpe = ThisType(staticModule("basis.memory.ValType.PackedShort").moduleClass)
  val PackedIntTpe = ThisType(staticModule("basis.memory.ValType.PackedInt").moduleClass)
  val PackedLongTpe = ThisType(staticModule("basis.memory.ValType.PackedLong").moduleClass)
  val PackedCharTpe = ThisType(staticModule("basis.memory.ValType.PackedChar").moduleClass)
  val PackedFloatTpe = ThisType(staticModule("basis.memory.ValType.PackedFloat").moduleClass)
  val PackedDoubleTpe = ThisType(staticModule("basis.memory.ValType.PackedDouble").moduleClass)
  val PackedBooleanTpe = ThisType(staticModule("basis.memory.ValType.PackedBoolean").moduleClass)
  val PaddedShortTpe = ThisType(staticModule("basis.memory.ValType.PaddedShort").moduleClass)
  val PaddedIntTpe = ThisType(staticModule("basis.memory.ValType.PaddedInt").moduleClass)
  val PaddedLongTpe = ThisType(staticModule("basis.memory.ValType.PaddedLong").moduleClass)
  val PaddedCharTpe = ThisType(staticModule("basis.memory.ValType.PaddedChar").moduleClass)
  val PaddedFloatTpe = ThisType(staticModule("basis.memory.ValType.PaddedFloat").moduleClass)
  val PaddedDoubleTpe = ThisType(staticModule("basis.memory.ValType.PaddedDouble").moduleClass)
  
  def load(fieldList: List[Expr[ValType[_]]], data: Tree, address: Tree, f: Tree): Tree = {
    val pointer = context.fresh(newTermName("pointer$"))
    var fields = fieldList
    val loads = List.newBuilder[Tree]
    loads += loadField(fields.head, data, address)
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
                Apply(Select(Select(Select(Ident("basis"), newTermName("memory")), nme.PACKAGE), newTermName("align")),
                      Apply(Select(Apply(Select(Ident(pointer), newTermName("$minus")), address :: Nil),
                                   newTermName("$plus")), offset :: Nil) :: alignment :: Nil) :: Nil)
      }
      loads += Block(Assign(Ident(pointer), increment) :: Nil,
                     loadField(fields.tail.head, data, Ident(pointer)))
      fields = fields.tail
    }
    Block(ValDef(Modifiers(Flag.MUTABLE), pointer, TypeTree(), address) :: Nil,
          Apply(f, loads.result))
  }
  
  def store(fieldList: List[Expr[ValType[_]]], valueList: List[Expr[_]], data: Tree, address: Tree): Tree = {
    val pointer = context.fresh(newTermName("pointer$"))
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
          Apply(Select(Ident(pointer), newTermName("$plus")),
                Literal(Constant(delta)) :: Nil)
        case (offset, alignment) =>
          base = -1L
          Apply(Select(Ident(pointer), newTermName("$plus")),
                Apply(Select(Select(Select(Ident("basis"), newTermName("memory")), nme.PACKAGE), newTermName("align")),
                      Apply(Select(Apply(Select(Ident(pointer), newTermName("$minus")), address :: Nil),
                                   newTermName("$plus")), offset :: Nil) :: alignment :: Nil) :: Nil)
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
    val fieldTpe = field.actualTpe
    if (fieldTpe =:= PackedByteTpe)
      Apply(Select(data, newTermName("loadByte")), address :: Nil)
    else if (fieldTpe =:= PackedShortTpe)
      Apply(Select(data, newTermName("loadUnalignedShort")), address :: Nil)
    else if (fieldTpe =:= PackedIntTpe)
      Apply(Select(data, newTermName("loadUnalignedInt")), address :: Nil)
    else if (fieldTpe =:= PackedLongTpe)
      Apply(Select(data, newTermName("loadUnalignedLong")), address :: Nil)
    else if (fieldTpe =:= PackedCharTpe)
      Apply(Select(data, newTermName("loadUnalignedChar")), address :: Nil)
    else if (fieldTpe =:= PackedFloatTpe)
      Apply(Select(data, newTermName("loadUnalignedFloat")), address :: Nil)
    else if (fieldTpe =:= PackedDoubleTpe)
      Apply(Select(data, newTermName("loadUnalignedDouble")), address :: Nil)
    else if (fieldTpe =:= PackedBooleanTpe)
      Apply(Select(Apply(Select(data, newTermName("loadByte")), address :: Nil),
                   newTermName("$eq$eq")),
            Literal(Constant(0)) :: Nil)
    else if (fieldTpe =:= PaddedShortTpe)
      Apply(Select(data, newTermName("loadShort")), address :: Nil)
    else if (fieldTpe =:= PaddedIntTpe)
      Apply(Select(data, newTermName("loadInt")), address :: Nil)
    else if (fieldTpe =:= PaddedLongTpe)
      Apply(Select(data, newTermName("loadLong")), address :: Nil)
    else if (fieldTpe =:= PaddedCharTpe)
      Apply(Select(data, newTermName("loadChar")), address :: Nil)
    else if (fieldTpe =:= PaddedFloatTpe)
      Apply(Select(data, newTermName("loadFloat")), address :: Nil)
    else if (fieldTpe =:= PaddedDoubleTpe)
      Apply(Select(data, newTermName("loadDouble")), address :: Nil)
    else
      Apply(Select(field.tree, newTermName("load")), data :: address :: Nil)
  }
  
  def storeField(field: Expr[ValType[_]], data: Tree, address: Tree, value: Tree): Tree = {
    val fieldTpe = field.actualTpe
    if (fieldTpe =:= PackedByteTpe)
      Apply(Select(data, newTermName("storeByte")), address :: value :: Nil)
    else if (fieldTpe =:= PackedShortTpe)
      Apply(Select(data, newTermName("storeUnalignedShort")), address :: value :: Nil)
    else if (fieldTpe =:= PackedIntTpe)
      Apply(Select(data, newTermName("storeUnalignedInt")), address :: value :: Nil)
    else if (fieldTpe =:= PackedLongTpe)
      Apply(Select(data, newTermName("storeUnalignedLong")), address :: value :: Nil)
    else if (fieldTpe =:= PackedCharTpe)
      Apply(Select(data, newTermName("storeUnalignedChar")), address :: value :: Nil)
    else if (fieldTpe =:= PackedFloatTpe)
      Apply(Select(data, newTermName("storeUnalignedFloat")), address :: value :: Nil)
    else if (fieldTpe =:= PackedDoubleTpe)
      Apply(Select(data, newTermName("storeUnalignedDouble")), address :: value :: Nil)
    else if (fieldTpe =:= PackedBooleanTpe)
      Apply(Select(data, newTermName("storeByte")),
            address :: If(value, Literal(Constant(0.toByte)), Literal(Constant(-1.toByte))) :: Nil)
    else if (fieldTpe =:= PaddedShortTpe)
      Apply(Select(data, newTermName("storeShort")), address :: value :: Nil)
    else if (fieldTpe =:= PaddedIntTpe)
      Apply(Select(data, newTermName("storeInt")), address :: value :: Nil)
    else if (fieldTpe =:= PaddedLongTpe)
      Apply(Select(data, newTermName("storeLong")), address :: value :: Nil)
    else if (fieldTpe =:= PaddedCharTpe)
      Apply(Select(data, newTermName("storeChar")), address :: value :: Nil)
    else if (fieldTpe =:= PaddedFloatTpe)
      Apply(Select(data, newTermName("storeFloat")), address :: value :: Nil)
    else if (fieldTpe =:= PaddedDoubleTpe)
      Apply(Select(data, newTermName("storeDouble")), address :: value :: Nil)
    else
      Apply(Select(field.tree, newTermName("store")), data :: address :: value :: Nil)
  }
  
  def alignOf(field: Expr[ValType[_]]): Tree = {
    val fieldTpe = field.actualTpe
    if (fieldTpe =:= PackedByteTpe   ||
        fieldTpe =:= PackedShortTpe  ||
        fieldTpe =:= PackedIntTpe    ||
        fieldTpe =:= PackedLongTpe   ||
        fieldTpe =:= PackedCharTpe   ||
        fieldTpe =:= PackedFloatTpe  ||
        fieldTpe =:= PackedDoubleTpe ||
        fieldTpe =:= PackedBooleanTpe)
      Literal(Constant(1L))
    else if (fieldTpe =:= PaddedShortTpe ||
             fieldTpe =:= PaddedCharTpe)
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
    val fieldTpe = field.actualTpe
    if (fieldTpe =:= PackedByteTpe ||
        fieldTpe =:= PackedBooleanTpe)
      Literal(Constant(1L))
    else if (fieldTpe =:= PackedShortTpe ||
             fieldTpe =:= PaddedShortTpe ||
             fieldTpe =:= PackedCharTpe  ||
             fieldTpe =:= PaddedCharTpe)
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
