/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.util.MurmurHash._

trait CoordinateVector[V <: CoordinateVector[V, S], S <: RingElement[S]]
  extends Equals with GeneralVector[V, S] {
  
  def Space: CoordinateModule {
    type Vector = V
    type Scalar = S
  }
  
  def coord(i: Int): S
  
  def dimension: Int = Space.dimension
  
  def + (that: V): V = {
    if (dimension != that.dimension)
      throw new DimensionException(Space.toString +" + "+ that.Space.toString)
    val coords = new Array[AnyRef](dimension)
    var i = 0
    while (i < dimension) {
      coords(i) = coord(i) + that.coord(i)
      i += 1
    }
    Space(wrapRefArray(coords).asInstanceOf[Seq[S]])
  }
  
  def unary_- : V = {
    val coords = new Array[AnyRef](dimension)
    var i = 0
    while (i < dimension) {
      coords(i) = -coord(i)
      i += 1
    }
    Space(wrapRefArray(coords).asInstanceOf[Seq[S]])
  }
  
  def - (that: V): V = {
    if (dimension != that.dimension)
      throw new DimensionException(Space.toString +" - "+ that.Space.toString)
    val coords = new Array[AnyRef](dimension)
    var i = 0
    while (i < dimension) {
      coords(i) = coord(i) - that.coord(i)
      i += 1
    }
    Space(wrapRefArray(coords).asInstanceOf[Seq[S]])
  }
  
  def :* (scalar: S): V = {
    val coords = new Array[AnyRef](dimension)
    var i = 0
    while (i < dimension) {
      coords(i) = coord(i) * scalar
      i += 1
    }
    Space(wrapRefArray(coords).asInstanceOf[Seq[S]])
  }
  
  def *: (scalar: S): V = {
    val coords = new Array[AnyRef](dimension)
    var i = 0
    while (i < dimension) {
      coords(i) = scalar * coord(i)
      i += 1
    }
    Space(wrapRefArray(coords).asInstanceOf[Seq[S]])
  }
  
  def canEqual(other: Any): Boolean =
    other.isInstanceOf[CoordinateVector[_, _]]
  
  override def equals(other: Any): Boolean = other match {
    case that: CoordinateVector[_, _] =>
      var equal = that.canEqual(this) && dimension == that.dimension
      var i = 0
      while (i < dimension && equal) {
        equal = coord(i).equals(that.coord(i))
        i += 1
      }
      equal
    case _ => false
  }
  
  override def hashCode: Int = {
    var h = 855447217
    var i = 0
    while (i < dimension) {
      mix(h, coord(i))
      i += 1
    }
    mash(h)
  }
  
  override def toString: String = {
    val s = new StringBuilder(Space.toString)
    s.append('(')
    if (0 < dimension) s.append(coord(0))
    var i = 1
    while (i < dimension) {
      s.append(", ").append(coord(i))
      i += 1
    }
    s.append(')')
    s.toString
  }
}
