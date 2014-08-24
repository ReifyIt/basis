//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.proto

import basis.collections.immutable.{ List => _, Nil => _, _ }
import scala.annotation._
import scala.reflect.macros._

private[proto] class ProtobufMacros(val c: blackbox.Context) {
  import c.{ Expr, mirror, prefix, Tree, WeakTypeTag }
  import c.internal._
  import c.universe._

  def aggregateFields: Expr[HashTrieMap[Long, Protobuf.Field[_]]] = {
    val ProtobufModule = mirror.staticModule("basis.proto.Protobuf").typeSignature
    val FieldTpe = typeRef(ProtobufModule, ProtobufModule.member(TypeName("Field")), WildcardType :: Nil)
    @tailrec def aggregate(members: List[Symbol], fields: List[Tree]): List[Tree] =
      if (members.isEmpty) fields
      else if (members.head.isMethod) {
        val method = members.head.asMethod
        if (method.paramLists.isEmpty && !method.isSynthetic && method.returnType <:< FieldTpe)
          aggregate(members.tail, q"{ val field = $prefix.$method; (field.key, field) }" :: fields)
        else aggregate(members.tail, fields)
      }
      else aggregate(members.tail, fields)
    val fields = aggregate(prefix.actualType.members.sorted, Nil)
    implicit val fieldsTag =
      WeakTypeTag[HashTrieMap[Long, Protobuf.Field[_]]](
        typeRef(NoPrefix, mirror.staticClass("basis.collections.immutable.HashTrieMap"), definitions.LongTpe :: FieldTpe :: Nil))
    Expr[HashTrieMap[Long, Protobuf.Field[_]]](q"_root_.basis.collections.immutable.HashTrieMap[${definitions.LongTpe}, $FieldTpe](..$fields)")
  }
}
