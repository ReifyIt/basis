/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.util.MurmurHash._

class F[S <: Field[S]](val dimension: Int)(field: ScalarSpace[S])
  extends VectorSpace { FN =>
  
  type Scalar = S
  
  final class Vector(private val coordinates: Array[AnyRef])
    extends basis.algebra.Vector[Vector, Scalar] {
    
    if (dimension != FN.dimension)
      throw new DimensionException(dimension.toString)
    
    def dimension: Int = coordinates.length
    
    def apply(n: Int): Scalar = coordinates(n).asInstanceOf[Scalar]
    
    def + (that: Vector): Vector = {
      if (dimension != that.dimension) throw new DimensionException
      val coordinates = new Array[AnyRef](dimension)
      var i = 0
      while (i < dimension) {
        coordinates(i) = this(i) + that(i)
        i += 1
      }
      new Vector(coordinates)
    }
    
    def unary_- : Vector = {
      val coordinates = new Array[AnyRef](dimension)
      var i = 0
      while (i < dimension) {
        coordinates(i) = -this(i)
        i += 1
      }
      new Vector(coordinates)
    }
    
    def - (that: Vector): Vector = {
      if (dimension != that.dimension) throw new DimensionException
      val coordinates = new Array[AnyRef](dimension)
      var i = 0
      while (i < dimension) {
        coordinates(i) = this(i) - that(i)
        i += 1
      }
      new Vector(coordinates)
    }
    
    def :* (scalar: Scalar): Vector = {
      val coordinates = new Array[AnyRef](dimension)
      var i = 0
      while (i < dimension) {
        coordinates(i) = this(i) * scalar
        i += 1
      }
      new Vector(coordinates)
    }
    
    def *: (scalar: Scalar): Vector = {
      val coordinates = new Array[AnyRef](dimension)
      var i = 0
      while (i < dimension) {
        coordinates(i) = scalar * this(i)
        i += 1
      }
      new Vector(coordinates)
    }
    
    def / (scalar: Scalar): Vector = {
      val coordinates = new Array[AnyRef](dimension)
      var i = 0
      while (i < dimension) {
        coordinates(i) = this(i) / scalar
        i += 1
      }
      new Vector(coordinates)
    }
    
    override def equals(other: Any): Boolean = other match {
      case that: Vector =>
        var equal = dimension == that.dimension
        var i = 0
        while (i < dimension && equal) {
          equal = this(i).equals(that(i))
          i += 1
        }
        equal
      case _ => false
    }
    
    override def hashCode: Int = {
      var h = -906602559
      var i = 0
      while (i < dimension) {
        mix(h, this(i))
        i += 1
      }
      mash(h)
    }
    
    override def toString: String = {
      val s = new StringBuilder
      s.append('<')
      if (0 < dimension) s.append(this(0))
      var i = 1
      while (i < dimension) {
        s.append(", ").append(this(i))
        i += 1
      }
      s.append('>')
      s.toString
    }
    
    private[F] def toSeq: Seq[Scalar] =
      wrapRefArray(coordinates).asInstanceOf[Seq[Scalar]]
  }
  
  final class Matrix[FM <: F[Scalar] with Singleton](
      FM: FM, private val entries: Array[AnyRef])
    extends basis.algebra.Vector[Matrix[FM], Scalar] {
    
    private def ColumnSpace: FM = FM
    
    private def RowSpace: FN.type = FN
    
    def M: Int = FM.dimension
    
    def N: Int = FN.dimension
    
    if (entries.length != M * N)
      throw new DimensionException(entries.length.toString)
    
    def apply(i: Int, j: Int): Scalar = {
      if (i < 0 || i >= M || j < 0 || j >= N)
        throw new IndexOutOfBoundsException("row "+ i +", "+"column "+ j)
      entries(N * i + j).asInstanceOf[Scalar]
    }
    
    def apply(k: Int): Scalar = entries(k).asInstanceOf[Scalar]
    
    def column(j: Int): FM#Vector = {
      if (j < 0 || j >= N) throw new IndexOutOfBoundsException("column "+ j)
      val coordinates = new Array[AnyRef](M)
      var i = 0
      var m = j
      while (i < M) {
        coordinates(i) = this(m)
        i += 1
        m += N
      }
      new FM.Vector(coordinates)
    }
    
    def row(i: Int): FN.Vector = {
      if (i < 0 || i >= M) throw new IndexOutOfBoundsException("row "+ i)
      val coordinates = new Array[AnyRef](N)
      var j = 0
      var n = N * i
      while (j < N) {
        coordinates(j) = this(n)
        j += 1
        n += 1
      }
      new FN.Vector(coordinates)
    }
    
    def + (that: Matrix[FM]): Matrix[FM] = {
      if (M != that.M || N != that.N) throw new DimensionException
      val entries = new Array[AnyRef](this.entries.length)
      var k = 0
      while (k < entries.length) {
        entries(k) = this(k) + that(k)
        k += 1
      }
      new Matrix(FM, entries)
    }
    
    def unary_- : Matrix[FM] = {
      val entries = new Array[AnyRef](this.entries.length)
      var k = 0
      while (k < entries.length) {
        entries(k) = -this(k)
        k += 1
      }
      new Matrix(FM, entries)
    }
    
    def - (that: Matrix[FM]): Matrix[FM] = {
      if (M != that.M || N != that.N) throw new DimensionException
      val entries = new Array[AnyRef](this.entries.length)
      var k = 0
      while (k < entries.length) {
        entries(k) = this(k) - that(k)
        k += 1
      }
      new Matrix(FM, entries)
    }
    
    def :* (scalar: Scalar): Matrix[FM] = {
      val entries = new Array[AnyRef](this.entries.length)
      var k = 0
      while (k < entries.length) {
        entries(k) = this(k) * scalar
        k += 1
      }
      new Matrix(FM, entries)
    }
    
    def *: (scalar: Scalar): Matrix[FM] = {
      val entries = new Array[AnyRef](this.entries.length)
      var k = 0
      while (k < entries.length) {
        entries(k) = scalar * this(k)
        k += 1
      }
      new Matrix(FM, entries)
    }
    
    def / (scalar: Scalar): Matrix[FM] = {
      val entries = new Array[AnyRef](this.entries.length)
      var k = 0
      while (k < entries.length) {
        entries(k) = this(k) / scalar
        k += 1
      }
      new Matrix(FM, entries)
    }
    
    def :* (column: FN.Vector): FM#Vector = {
      if (N != column.dimension) throw new DimensionException
      val coordinates = new Array[AnyRef](M)
      var i = 0
      var i0 = 0
      while (i < M) {
        var x = Scalar.zero
        var n = i0
        var j = 0
        while (j < N) {
          x += this(n) * column(j)
          n += 1
          j += 1
        }
        coordinates(i) = x
        i += 1
        i0 += N
      }
      new FM.Vector(coordinates)
    }
    
    def *: (row: FM#Vector): FN.Vector = {
      if (row.dimension != M) throw new DimensionException
      val coordinates = new Array[AnyRef](N)
      var j = 0
      while (j < N) {
        var x = Scalar.zero
        var n = j
        var i = 0
        while (i < M) {
          x += row(i) * this(n)
          n += N
          i += 1
        }
        coordinates(j) = x
        j += 1
      }
      new FN.Vector(coordinates)
    }
    
    type Map[FP <: F[Scalar] with Singleton] = FN.Matrix[FP]
    
    def * (that: FP.Matrix[FN.type] forSome { val FP: F[Scalar] }): that.Map[FM] = {
      if (N != that.M) throw new DimensionException
      val P = that.N
      val entries = new Array[AnyRef](M * P)
      var k = 0
      var i = 0
      var i0 = 0
      while (i < M) {
        var j = 0
        while (j < P) {
          var x = Scalar.zero
          var m = i0
          var n = j
          var d = 0
          while (d < N) {
            x += this(m) * that(n)
            m += 1
            n += P
            d += 1
          }
          entries(k) = x
          k += 1
          j += 1
        }
        i += 1
        i0 += N
      }
      val FP = that.RowSpace
      new FP.Matrix(FM, entries)
    }
    
    def inverse: Option[FM#Matrix[FN.type]] = None
    
    def transpose: FM#Matrix[FN.type] = {
      val entries = new Array[AnyRef](this.entries.length)
      var k = 0
      var j = 0
      while (j < N) {
        var n = j
        var i = 0
        while (i < M) {
          entries(k) = this(n)
          n += N
          k += 1
          i += 1
        }
        j += 1
      }
      new FM.Matrix[FN.type](FN, entries)
    }
    
    override def equals(other: Any): Boolean = other match {
      case that: Matrix[_] =>
        var equal = M == that.M && N == that.N
        var k = 0
        while (k < entries.length && equal) {
          equal = this(k).equals(that(k))
          k += 1
        }
        equal
      case _ => false
    }
    
    override def hashCode: Int = {
      var h = -1167454657
      var k = 0
      while (k < entries.length) {
        mix(h, this(k))
        k += 1
      }
      mash(h)
    }
    
    override def toString: String = {
      val s = new StringBuilder
      s.append("[<")
      if (0 < entries.length) s.append(this(0))
      var k = 1
      var j = 1
      var i = 0
      while (i < M) {
        while (j < N) {
          if (j != 0) s.append(", ") else s.append(">, <")
          s.append(this(k))
          k += 1
          j += 1
        }
        j = 0
        i += 1
      }
      s.append(">]")
      s.toString
    }
  }
  
  class Morphism[FM <: F[Scalar] with Singleton](FM: FM) extends VectorSpace {
    type Scalar = FN.Scalar
    type Vector = Matrix
    type ColumnVector = FM#Vector
    type RowVector = FN.Vector
    type Matrix = FN.Matrix[FM]
    
    val Scalar = FN.Scalar
    
    def M: Int = FM.dimension
    
    def N: Int = FN.dimension
    
    def dimension: Int = M * N
    
    lazy val zero: Matrix = {
      val entries = new Array[AnyRef](dimension)
      var k = 0
      while (k < dimension) {
        entries(k) = Scalar.zero
        k += 1
      }
      new Matrix(FM, entries)
    }
    
    def apply(entries: Scalar*): Matrix =
      new Matrix(FM, entries.toArray[AnyRef])
    
    def map(FP: F[Scalar]): Morphism[FP.type] = new Morphism[FP.type](FP)
    
    override def toString: String = "<"+ FM +" => "+ FN +">"
  }
  
  class Endomorphism extends Morphism[FN.type](FN) {
    lazy val identity: Matrix = {
      val entries = new Array[AnyRef](dimension)
      var k = 0
      var j = 0
      var i = 0
      while (i < M) {
        while (j < N) {
          entries(k) = if (i == j) Scalar.unit else Scalar.zero
          k += 1
          j += 1
        }
        i += 1 
        j = 0
      }
      new Matrix(FN, entries)
    }
  }
  
  val Scalar = field: ScalarSpace[Scalar]
  
  lazy val Matrix = new Endomorphism
  
  lazy val zero: Vector = {
    val coordinates = new Array[AnyRef](dimension)
    var i = 0
    while (i < dimension) {
      coordinates(i) = Scalar.zero
      i += 1
    }
    new Vector(coordinates)
  }
  
  def apply(coordinates: Scalar*): Vector =
    new Vector(coordinates.toArray[AnyRef])
  
  def unapplySeq(vector: Vector): Option[Seq[Scalar]] =
    if (vector.dimension == dimension) Some(vector.toSeq) else None
  
  def map(FM: F[Scalar]): Morphism[FM.type] = new Morphism[FM.type](FM)
  
  override def toString: String = "F"+"("+ dimension +")"+"("+ Scalar +")"
}
