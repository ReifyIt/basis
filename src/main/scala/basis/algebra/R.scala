/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.util.MurmurHash._

class R(val dimension: Int) extends VectorSpace { RN =>
  type Scalar = Real
  
  final class Vector(private val coordinates: Array[Double])
    extends RealVector[Vector] {
    
    if (coordinates.length != R.this.dimension)
      throw new DimensionException(coordinates.length.toString)
    
    def dimension: Int = coordinates.length
    
    def apply(n: Int): Double = coordinates(n)
    
    def + (that: Vector): Vector = {
      if (dimension != that.dimension) throw new DimensionException
      val coordinates = new Array[Double](dimension)
      var i = 0
      while (i < dimension) {
        coordinates(i) = this(i) + that(i)
        i += 1
      }
      new Vector(coordinates)
    }
    
    def unary_- : Vector = {
      val coordinates = new Array[Double](dimension)
      var i = 0
      while (i < dimension) {
        coordinates(i) = -this(i)
        i += 1
      }
      new Vector(coordinates)
    }
    
    def - (that: Vector): Vector = {
      if (dimension != that.dimension) throw new DimensionException
      val coordinates = new Array[Double](dimension)
      var i = 0
      while (i < dimension) {
        coordinates(i) = this(i) - that(i)
        i += 1
      }
      new Vector(coordinates)
    }
    
    def :* (scalar: Double): Vector = {
      val coordinates = new Array[Double](dimension)
      var i = 0
      while (i < dimension) {
        coordinates(i) = this(i) * scalar
        i += 1
      }
      new Vector(coordinates)
    }
    
    def *: (scalar: Double): Vector = this :* scalar
    
    def / (scalar: Double): Vector = {
      val coordinates = new Array[Double](dimension)
      var i = 0
      while (i < dimension) {
        coordinates(i) = this(i) / scalar
        i += 1
      }
      new Vector(coordinates)
    }
    
    def ⋅ (that: Vector): Double = {
      if (dimension != that.dimension) throw new DimensionException
      var x = 0.0
      var i = 0
      while (i < dimension) {
        x += this(i) * that(i)
        i += 1
      }
      x
    }
    
    def length: Double = {
      var x = 0.0
      var i = 0
      while (i < dimension) {
        x += this(i) * this(i)
        i += 1
      }
      math.sqrt(x)
    }
    
    override def equals(other: Any): Boolean = other match {
      case that: Vector =>
        var equal = dimension == that.dimension
        var i = 0
        while (i < dimension && equal) {
          equal = this(i) == that(i)
          i += 1
        }
        equal
      case _ => false
    }
    
    override def hashCode: Int = {
      var h = 112329823
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
    
    private[R] def toSeq: Seq[Double] = coordinates
  }
  
  final class Matrix[RM <: R with Singleton](
      RM: RM, private val entries: Array[Double])
    extends RealVector[Matrix[RM]] {
    
    private def ColumnSpace: RM = RM
    
    private def RowSpace: RN.type = RN
    
    def M: Int = RM.dimension
    
    def N: Int = RN.dimension
    
    if (entries.length != M * N)
      throw new DimensionException(entries.length.toString)
    
    def apply(i: Int, j: Int): Double = {
      if (i < 0 || i >= M || j < 0 || j >= N)
        throw new IndexOutOfBoundsException("row "+ i +", "+"column "+ j)
      entries(N * i + j)
    }
    
    def apply(k: Int): Double = entries(k)
    
    def column(j: Int): RM#Vector = {
      if (j < 0 || j >= N) throw new IndexOutOfBoundsException("column "+ j)
      val coordinates = new Array[Double](M)
      var i = 0
      var m = j
      while (i < M) {
        coordinates(i) = this(m)
        i += 1
        m += N
      }
      new RM.Vector(coordinates)
    }
    
    def row(i: Int): RN.Vector = {
      if (i < 0 || i >= M) throw new IndexOutOfBoundsException("row "+ i)
      val coordinates = new Array[Double](N)
      var j = 0
      var n = N * i
      while (j < N) {
        coordinates(j) = this(n)
        j += 1
        n += 1
      }
      new RN.Vector(coordinates)
    }
    
    def + (that: Matrix[RM]): Matrix[RM] = {
      if (M != that.M || N != that.N) throw new DimensionException
      val entries = new Array[Double](this.entries.length)
      var k = 0
      while (k < entries.length) {
        entries(k) = this(k) + that(k)
        k += 1
      }
      new Matrix(RM, entries)
    }
    
    def unary_- : Matrix[RM] = {
      val entries = new Array[Double](this.entries.length)
      var k = 0
      while (k < entries.length) {
        entries(k) = -this(k)
        k += 1
      }
      new Matrix(RM, entries)
    }
    
    def - (that: Matrix[RM]): Matrix[RM] = {
      if (M != that.M || N != that.N) throw new DimensionException
      val entries = new Array[Double](this.entries.length)
      var k = 0
      while (k < entries.length) {
        entries(k) = this(k) - that(k)
        k += 1
      }
      new Matrix(RM, entries)
    }
    
    def :* (scalar: Double): Matrix[RM] = {
      val entries = new Array[Double](this.entries.length)
      var k = 0
      while (k < entries.length) {
        entries(k) = this(k) * scalar
        k += 1
      }
      new Matrix(RM, entries)
    }
    
    def *: (scalar: Double): Matrix[RM] = this :* scalar
    
    def / (scalar: Double): Matrix[RM] = {
      val entries = new Array[Double](this.entries.length)
      var k = 0
      while (k < entries.length) {
        entries(k) = this(k) / scalar
        k += 1
      }
      new Matrix(RM, entries)
    }
    
    def :* (column: RN.Vector): RM#Vector = {
      if (N != column.dimension) throw new DimensionException
      val coordinates = new Array[Double](M)
      var i = 0
      var i0 = 0
      while (i < M) {
        var x = 0.0
        var n = i0
        var j = 0
        while (j < N) {
          x = this(n) * column(j)
          n += 1
          j += 1
        }
        coordinates(i) = x
        i += 1
        i0 += N
      }
      new RM.Vector(coordinates)
    }
    
    def *: (row: RM#Vector): RN.Vector = {
      if (row.dimension != M) throw new DimensionException
      val coordinates = new Array[Double](N)
      var j = 0
      while (j < N) {
        var x = 0.0
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
      new RN.Vector(coordinates)
    }
    
    type Map[RP <: R with Singleton] = RN.Matrix[RP]
    
    def * (that: RP.Matrix[RN.type] forSome { val RP: R }): that.Map[RM] = {
      if (N != that.M) throw new DimensionException
      val P = that.N
      val entries = new Array[Double](M * P)
      var k = 0
      var i = 0
      var i0 = 0
      while (i < M) {
        var j = 0
        while (j < P) {
          var x = 0.0
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
        i0 += N
        i += 1
      }
      val RP = that.RowSpace
      new RP.Matrix(RM, entries)
    }
    
    def inverse: Option[RM#Matrix[RN.type]] = None
    
    def transpose: RM#Matrix[RN.type] = {
      val entries = new Array[Double](this.entries.length)
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
      new RM.Matrix[RN.type](RN, entries)
    }
    
    override def equals(other: Any): Boolean = other match {
      case that: Matrix[_] =>
        var equal = M == that.M && N == that.N
        var k = 0
        while (k < entries.length && equal) {
          equal = this(k) == that(k)
          k += 1
        }
        equal
      case _ => false
    }
    
    override def hashCode: Int = {
      var h = -1728567117
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
      var i = 0
      var j = 1
      while (i < M) {
        while (j < N) {
          if (j != 0) s.append(", ") else s.append(">, <")
          s.append(this(k))
          k += 1
          j += 1
        }
        i += 1
        j = 0
      }
      s.append(">]")
      s.toString
    }
  }
  
  class Morphism[RM <: R with Singleton](RM: RM) extends VectorSpace {
    type Scalar = Real
    type Vector = Matrix
    type ColumnVector = RM#Vector
    type RowVector = RN.Vector
    type Matrix = RN.Matrix[RM]
    
    val Scalar = Real
    
    def M: Int = RM.dimension
    
    def N: Int = RN.dimension
    
    def dimension: Int = M * N
    
    lazy val zero: Matrix = new Matrix(RM, new Array[Double](dimension))
    
    def apply(entries: Double*): Matrix =
      new Matrix(RM, entries.toArray[Double])
    
    def map(RP: R): Morphism[RP.type] = new Morphism[RP.type](RP)
    
    override def toString: String = "<"+ RM +" => "+ RN +">"
  }
  
  class Endomorphism extends Morphism[RN.type](RN) {
    lazy val identity: Matrix = {
      val entries = new Array[Double](dimension)
      var i = 0
      var i0 = 0
      while (i < M) {
        entries(i0 + i) = 1.0
        i0 += N
        i += 1
      }
      new Matrix(RN, entries)
    }
  }
  
  val Scalar = Real
  
  lazy val Matrix = new Endomorphism
  
  lazy val zero: Vector = new Vector(new Array[Double](dimension))
  
  def apply(coordinates: Double*): Vector =
    new Vector(coordinates.toArray[Double])
  
  def unapply(vector: Vector): Option[Seq[Double]] =
    if (vector.dimension == dimension) Some(vector.toSeq) else None
  
  def map(RM: R): Morphism[RM.type] = new Morphism[RM.type](RM)
  
  override def toString: String = "R"+"("+ dimension +")"
}
