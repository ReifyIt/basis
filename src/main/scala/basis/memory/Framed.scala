/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

/** An embeddable memory frame. Framed types define an `alignment`, an `offset`,
  * and a `size`. The `framed` method relocates the frame to a new offset,
  * resizes the frame to a new size, and aligns the frame to a new alignment.
  * 
  * @author Chris Sachs
  * 
  * @tparam F   The projected type of the frame.
  */
trait Framed[+F] {
  /** The type of `framed` projections. */
  protected type Frame = F
  
  /** Returns the power-of-two alignment of this frame. */
  def alignment: Int
  
  /** Returns the number of leading padding bytes in this frame.
    * The frame's alignment must evenly divide its padded offset. */
  def offset: Int
  
  /** Returns the size in bytes of this frame. The frame's alignmemt
    * must venly divide its size. */
  def size: Int
  
  /** Returns this frame projected into a different frame.
    * 
    * @param  offset      the preferred offset of the new frame.
    * @param  size        the preferred size of the new frame.
    * @param  alignment   the preferred alignment of the new frame.
    * @return the projected frame.
    */
  def framed(offset: Int, size: Int, alignment: Int): Frame
}
