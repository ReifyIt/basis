/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait VectorSpace[S <: Ring with Singleton] extends LinearSpace[S] {
  trait Element extends Any with super.Element {
    protected def Vector: VectorSpace.this.type = VectorSpace.this
    
    def N: Int = Vector.N
    
    def apply(i: Int): Scalar
    
    override def + (that: Vector): Vector = {
      if (N != that.N) throw new DimensionException
      val coords = new Array[AnyRef](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = (this(i) + that(i)).asInstanceOf[AnyRef]
        i += 1
      }
      Vector(wrapRefArray(coords).asInstanceOf[Seq[Scalar]])
    }
    
    override def unary_- : Vector = {
      val coords = new Array[AnyRef](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = (-this(i)).asInstanceOf[AnyRef]
        i += 1
      }
      Vector(wrapRefArray(coords).asInstanceOf[Seq[Scalar]])
    }
    
    override def - (that: Vector): Vector = {
      if (N != that.N) throw new DimensionException
      val coords = new Array[AnyRef](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = (this(i) - that(i)).asInstanceOf[AnyRef]
        i += 1
      }
      Vector(wrapRefArray(coords).asInstanceOf[Seq[Scalar]])
    }
    
    override def :* (scalar: Scalar): Vector = {
      val coords = new Array[AnyRef](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = (this(i) * scalar).asInstanceOf[AnyRef]
        i += 1
      }
      Vector(wrapRefArray(coords).asInstanceOf[Seq[Scalar]])
    }
    
    override def *: (scalar: Scalar): Vector = {
      val coords = new Array[AnyRef](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = (scalar * this(i)).asInstanceOf[AnyRef]
        i += 1
      }
      Vector(wrapRefArray(coords).asInstanceOf[Seq[Scalar]])
    }
    
    def ⋅ (that: Vector): Scalar = {
      if (N != that.N) throw new DimensionException
      var s = Scalar.zero
      var i = 0
      while (i < N) {
        s += this(i) * that(i)
        i += 1
      }
      s
    }
    
    override def equals(other: Any): Boolean = other match {
      case that: Element =>
        var equal = N == that.N
        var i = 0
        while (i < N && equal) {
          equal = this(i).equals(that(i))
          i += 1
        }
        equal
      case _ => false
    }
    
    override def hashCode: Int = {
      import basis.util.MurmurHash._
      var h = -1736520349
      var i = 0
      while (i < N) {
        h = mix(h, this(i))
        i += 1
      }
      mash(h)
    }
    
    override def toString: String = {
      val s = new StringBuilder(Vector.toString)
      s.append('(')
      if (N > 0) {
        s.append(this(0))
        var i = 1
        while (i < N) {
          s.append(", ").append(this(i))
          i += 1
        }
      }
      s.append(')')
      s.toString
    }
  }
  
  override type Vector <: Element
  
  def N: Int
  
  def apply(coords: TraversableOnce[Scalar]): Vector
  
  override def zero: Vector = {
    val z = Scalar.zero.asInstanceOf[AnyRef]
    val coords = new Array[AnyRef](N)
    var i = 0
    while (i < coords.length) {
      coords(i) = z
      i += 1
    }
    apply(wrapRefArray(coords).asInstanceOf[Seq[Scalar]])
  }
}
