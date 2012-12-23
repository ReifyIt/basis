/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.memory

import scala.reflect.macros.Context

private[memory] object DataMacros {
  def load[R : c.WeakTypeTag]
      (c: Context { type PrefixType <: Data })
      (address: c.Expr[Long])
      (T: c.Expr[Struct[_]])
    : c.Expr[R] =
    c.Expr(
      new DataMacros[c.type](c).loadField(c.prefix.tree, address.tree)(T)
    )(c.weakTypeTag[R])
  
  def store[T]
      (c: Context { type PrefixType <: Data })
      (address: c.Expr[Long], value: c.Expr[T])
      (T: c.Expr[Struct[_]])
    : c.Expr[Unit] =
    c.Expr(
      new DataMacros[c.type](c).storeField(c.prefix.tree, address.tree, value.tree)(T)
    )(c.TypeTag.Unit)
  
  def load2[T1, T2, R : c.WeakTypeTag]
      (c: Context { type PrefixType <: Data })
      (address: c.Expr[Long])
      (f: c.Expr[(T1, T2) => R])
      (T1: c.Expr[Struct[_]], T2: c.Expr[Struct[_]])
    : c.Expr[R] =
    c.Expr(
      new DataMacros[c.type](c).load
        (c.prefix.tree, address.tree)(f.tree)
        (T1 :: T2 :: Nil)
    )(c.weakTypeTag[R])
  
  def store2[T1, T2]
      (c: Context { type PrefixType <: Data })
      (address: c.Expr[Long])
      (value1: c.Expr[T1], value2: c.Expr[T2])
      (T1: c.Expr[Struct[_]], T2: c.Expr[Struct[_]])
    : c.Expr[Unit] =
    c.Expr(
      new DataMacros[c.type](c).store
        (c.prefix.tree, address.tree)
        (value1.tree :: value2.tree :: Nil)
        (T1 :: T2 :: Nil)
    )(c.TypeTag.Unit)
  
  def load3[T1, T2, T3, R : c.WeakTypeTag]
      (c: Context { type PrefixType <: Data })
      (address: c.Expr[Long])
      (f: c.Expr[(T1, T2, T3) => R])
      (T1: c.Expr[Struct[_]], T2: c.Expr[Struct[_]], T3: c.Expr[Struct[_]])
    : c.Expr[R] =
    c.Expr(
      new DataMacros[c.type](c).load
        (c.prefix.tree, address.tree)(f.tree)
        (T1 :: T2 :: T3 :: Nil)
    )(c.weakTypeTag[R])
  
  def store3[T1, T2, T3]
      (c: Context { type PrefixType <: Data })
      (address: c.Expr[Long])
      (value1: c.Expr[T1], value2: c.Expr[T2], value3: c.Expr[T3])
      (T1: c.Expr[Struct[_]], T2: c.Expr[Struct[_]], T3: c.Expr[Struct[_]])
    : c.Expr[Unit] =
    c.Expr(
      new DataMacros[c.type](c).store
        (c.prefix.tree, address.tree)
        (value1.tree :: value2.tree :: value3.tree :: Nil)
        (T1 :: T2 :: T3 :: Nil)
    )(c.TypeTag.Unit)
  
  def load4[T1, T2, T3, T4, R : c.WeakTypeTag]
      (c: Context { type PrefixType <: Data })
      (address: c.Expr[Long])
      (f: c.Expr[(T1, T2, T3, T4) => R])
      (T1: c.Expr[Struct[_]], T2: c.Expr[Struct[_]],
       T3: c.Expr[Struct[_]], T4: c.Expr[Struct[_]])
    : c.Expr[R] =
    c.Expr(
      new DataMacros[c.type](c).load
        (c.prefix.tree, address.tree)(f.tree)
        (T1 :: T2 :: T3 :: T4 :: Nil)
    )(c.weakTypeTag[R])
  
  def store4[T1, T2, T3, T4]
      (c: Context { type PrefixType <: Data })
      (address: c.Expr[Long])
      (value1: c.Expr[T1], value2: c.Expr[T2],
       value3: c.Expr[T3], value4: c.Expr[T4])
      (T1: c.Expr[Struct[_]], T2: c.Expr[Struct[_]],
       T3: c.Expr[Struct[_]], T4: c.Expr[Struct[_]])
    : c.Expr[Unit] =
    c.Expr(
      new DataMacros[c.type](c).store
        (c.prefix.tree, address.tree)
        (value1.tree :: value2.tree :: value3.tree :: value4.tree :: Nil)
        (T1 :: T2 :: T3 :: T4 :: Nil)
    )(c.TypeTag.Unit)
}

private[memory] final class DataMacros[C <: Context](val context: C) {
  import context.{Expr, fresh, mirror}
  import universe._
  import Struct._
  
  val universe: context.universe.type = context.universe
  
  val PackedByteType = mirror.staticModule("basis.memory.Struct.PackedByte").moduleClass.asType.toType
  val PackedShortType = mirror.staticModule("basis.memory.Struct.PackedShort").moduleClass.asType.toType
  val PackedIntType = mirror.staticModule("basis.memory.Struct.PackedInt").moduleClass.asType.toType
  val PackedLongType = mirror.staticModule("basis.memory.Struct.PackedLong").moduleClass.asType.toType
  val PackedFloatType = mirror.staticModule("basis.memory.Struct.PackedFloat").moduleClass.asType.toType
  val PackedDoubleType = mirror.staticModule("basis.memory.Struct.PackedDouble").moduleClass.asType.toType
  val PackedBooleanType = mirror.staticModule("basis.memory.Struct.PackedBoolean").moduleClass.asType.toType
  val PaddedShortType = mirror.staticModule("basis.memory.Struct.PaddedShort").moduleClass.asType.toType
  val PaddedIntType = mirror.staticModule("basis.memory.Struct.PaddedInt").moduleClass.asType.toType
  val PaddedLongType = mirror.staticModule("basis.memory.Struct.PaddedLong").moduleClass.asType.toType
  val PaddedFloatType = mirror.staticModule("basis.memory.Struct.PaddedFloat").moduleClass.asType.toType
  val PaddedDoubleType = mirror.staticModule("basis.memory.Struct.PaddedDouble").moduleClass.asType.toType
  
  def load(data: Tree, address: Tree)(f: Tree)(fieldList: List[Expr[Struct[_]]]): Tree = {
    val pointer = newTermName(fresh("pointer$"))
    var fields = fieldList
    val loads = List.newBuilder[Tree]
    loads += loadField(data, Ident(pointer))(fields.head)
    var base = 0L
    while (!fields.tail.isEmpty) {
      val increment = (sizeOf(fields.head), alignOf(fields.tail.head)) match {
        case (Literal(Constant(offset: Long)), Literal(Constant(alignment: Long))) if base >= 0L =>
          val delta = align(base + offset, alignment) - base
          base += delta
          Apply(Select(Ident(pointer), "$plus"), Literal(Constant(delta)) :: Nil)
        case (offset, alignment) =>
          base = -1L
          Apply(
            Select(Ident(pointer), "$plus"),
            Apply(
              Select(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "memory"), nme.PACKAGE), "align"),
              Apply(Select(Apply(Select(Ident(pointer), "$minus"), address :: Nil), "$plus"), offset :: Nil) ::
              alignment :: Nil) :: Nil)
      }
      fields = fields.tail
      loads += Block(Assign(Ident(pointer), increment) :: Nil, loadField(data, Ident(pointer))(fields.head))
    }
    Block(ValDef(Modifiers(Flag.MUTABLE), pointer, TypeTree(), address) :: Nil, Apply(f, loads.result))
  }
  
  def store(data: Tree, address: Tree)(valueList: List[Tree])(fieldList: List[Expr[Struct[_]]]): Tree = {
    val pointer = newTermName(fresh("pointer$"))
    var values = valueList
    var fields = fieldList
    val stores = List.newBuilder[Tree]
    stores += storeField(data, Ident(pointer), values.head)(fields.head)
    var base = 0L
    while (!values.tail.isEmpty && !fields.tail.isEmpty) {
      val increment = (sizeOf(fields.head), alignOf(fields.tail.head)) match {
        case (Literal(Constant(offset: Long)), Literal(Constant(alignment: Long))) if base >= 0L =>
          val delta = align(base + offset, alignment) - base
          base += delta
          Apply(Select(Ident(pointer), "$plus"), Literal(Constant(delta)) :: Nil)
        case (offset, alignment) =>
          base = -1L
          Apply(
            Select(Ident(pointer), "$plus"),
            Apply(
              Select(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "memory"), nme.PACKAGE), "align"),
              Apply(Select(Apply(Select(Ident(pointer), "$minus"), address :: Nil), "$plus"), offset :: Nil) ::
              alignment :: Nil) :: Nil)
      }
      values = values.tail
      fields = fields.tail
      stores += Assign(Ident(pointer), increment)
      stores += storeField(data, Ident(pointer), values.head)(fields.head)
    }
    Block(ValDef(Modifiers(Flag.MUTABLE), pointer, TypeTree(), address) :: stores.result, EmptyTree)
  }
  
  def loadField(data: Tree, address: Tree)(field: Expr[Struct[_]]): Tree = {
    val fieldType = field.actualType
    if (fieldType =:= PackedByteType)
      Apply(Select(data, "loadByte"), address :: Nil)
    else if (fieldType =:= PackedShortType)
      Apply(Select(data, "loadUnalignedShort"), address :: Nil)
    else if (fieldType =:= PackedIntType)
      Apply(Select(data, "loadUnalignedInt"), address :: Nil)
    else if (fieldType =:= PackedLongType)
      Apply(Select(data, "loadUnalignedLong"), address :: Nil)
    else if (fieldType =:= PackedFloatType)
      Apply(Select(data, "loadUnalignedFloat"), address :: Nil)
    else if (fieldType =:= PackedDoubleType)
      Apply(Select(data, "loadUnalignedDouble"), address :: Nil)
    else if (fieldType =:= PackedBooleanType)
      Apply(
        Select(Apply(Select(data, "loadByte"), address :: Nil), "$eq$eq"),
        Literal(Constant(0)) :: Nil)
    else if (fieldType =:= PaddedShortType)
      Apply(Select(data, "loadShort"), address :: Nil)
    else if (fieldType =:= PaddedIntType)
      Apply(Select(data, "loadInt"), address :: Nil)
    else if (fieldType =:= PaddedLongType)
      Apply(Select(data, "loadLong"), address :: Nil)
    else if (fieldType =:= PaddedFloatType)
      Apply(Select(data, "loadFloat"), address :: Nil)
    else if (fieldType =:= PaddedDoubleType)
      Apply(Select(data, "loadDouble"), address :: Nil)
    else Apply(Select(field.tree, "load"), data :: address :: Nil)
  }
  
  def storeField(data: Tree, address: Tree, value: Tree)(field: Expr[Struct[_]]): Tree = {
    val fieldType = field.actualType
    if (fieldType =:= PackedByteType)
      Apply(Select(data, "storeByte"), address :: value :: Nil)
    else if (fieldType =:= PackedShortType)
      Apply(Select(data, "storeUnalignedShort"), address :: value :: Nil)
    else if (fieldType =:= PackedIntType)
      Apply(Select(data, "storeUnalignedInt"), address :: value :: Nil)
    else if (fieldType =:= PackedLongType)
      Apply(Select(data, "storeUnalignedLong"), address :: value :: Nil)
    else if (fieldType =:= PackedFloatType)
      Apply(Select(data, "storeUnalignedFloat"), address :: value :: Nil)
    else if (fieldType =:= PackedDoubleType)
      Apply(Select(data, "storeUnalignedDouble"), address :: value :: Nil)
    else if (fieldType =:= PackedBooleanType)
      Apply(
        Select(data, "storeByte"),
        address :: If(value, Literal(Constant(0.toByte)), Literal(Constant(-1.toByte))) :: Nil)
    else if (fieldType =:= PaddedShortType)
      Apply(Select(data, "storeShort"), address :: value :: Nil)
    else if (fieldType =:= PaddedIntType)
      Apply(Select(data, "storeInt"), address :: value :: Nil)
    else if (fieldType =:= PaddedLongType)
      Apply(Select(data, "storeLong"), address :: value :: Nil)
    else if (fieldType =:= PaddedFloatType)
      Apply(Select(data, "storeFloat"), address :: value :: Nil)
    else if (fieldType =:= PaddedDoubleType)
      Apply(Select(data, "storeDouble"), address :: value :: Nil)
    else Apply(Select(field.tree, "store"), data :: address :: value :: Nil)
  }
  
  def alignOf(field: Expr[Struct[_]]): Tree = {
    val fieldType = field.actualType
    if (fieldType =:= PackedByteType   ||
        fieldType =:= PackedShortType  ||
        fieldType =:= PackedIntType    ||
        fieldType =:= PackedLongType   ||
        fieldType =:= PackedFloatType  ||
        fieldType =:= PackedDoubleType ||
        fieldType =:= PackedBooleanType)
      Literal(Constant(1L))
    else if (fieldType =:= PaddedShortType)
      Literal(Constant(2L))
    else if (fieldType =:= PaddedIntType   ||
             fieldType =:= PaddedFloatType)
      Literal(Constant(4L))
    else if (fieldType =:= PaddedLongType  ||
             fieldType =:= PaddedDoubleType)
      Literal(Constant(8L))
    else Select(field.tree, "alignment")
  }
  
  def sizeOf(field: Expr[Struct[_]]): Tree = {
    val fieldType = field.actualType
    if (fieldType =:= PackedByteType ||
        fieldType =:= PackedBooleanType)
      Literal(Constant(1L))
    else if (fieldType =:= PackedShortType ||
             fieldType =:= PaddedShortType)
      Literal(Constant(2L))
    else if (fieldType =:= PackedIntType   ||
             fieldType =:= PaddedIntType   ||
             fieldType =:= PackedFloatType ||
             fieldType =:= PaddedFloatType)
      Literal(Constant(4L))
    else if (fieldType =:= PackedLongType   ||
             fieldType =:= PaddedLongType   ||
             fieldType =:= PackedDoubleType ||
             fieldType =:= PaddedDoubleType)
      Literal(Constant(8L))
    else Select(field.tree, "size")
  }
}
