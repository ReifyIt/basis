/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.dispatch

import basis.control._

/** A future result receiver.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * 
  * @groupprio  Evaluating  1
  * @groupprio  Relaying    2
  * @groupprio  Composing   3
  * @groupprio  Recovering  4
  */
trait Relay[+A] {
  /** Returns `true` if this relay is set with a value or exception,
    * otherwise returns `false` if this relay is not yet set.
    * @group Evaluating */
  def isSet: Boolean
  
  /** Applies a function to the received result.
    * @group Relaying */
  def run[U](f: Try[A] => U): Unit
  
  /** Sets a latch with the received result.
    * @group Relaying */
  def forward[B >: A](that: Latch[B]): Unit
  
  /** Applies a function to a received value; does nothing when this relay
    * receives an exception.
    * @group Relaying */
  def foreach[U](f: A => U): Unit
  
  /** Relays the received result after applying the result to a partial
    * function, if defined.
    * @group Relaying */
  def andThen[U](q: PartialFunction[Try[A], U]): Relay[A]
  
  /** Relays a partial function applied to a received value, if defined,
    * otherwise relays a trap.
    * @group Composing */
  def choose[B](q: PartialFunction[A, B]): Relay[B]
  
  /** Relays a function applied to a received value, otherwise relays
    * a produced or consumed exception.
    * @group Composing */
  def map[B](f: A => B): Relay[B]
  
  /** Forwards a relay function applied to a received value, otherwise relays
    * a produced or consumed exception.
    * @group Composing */
  def flatMap[B](f: A => Relay[B]): Relay[B]
  
  /** Relays a function applied to a received exception, if defined,
    * otherwise relays a produced or consumed exception.
    * @group Recovering */
  def recover[B >: A](q: PartialFunction[Throwable, B]): Relay[B]
  
  /** Forwards a relay function applied to a received exception, if defined,
    * otherwise relays a produced or consumed exception.
    * @group Recovering */
  def recoverWith[B >: A](q: PartialFunction[Throwable, Relay[B]]): Relay[B]
  
  /** Relays a received value, if the value satisfies a predicate,
    * otherwise relays a trap.
    * @group Composing */
  def filter(p: A => Boolean): Relay[A]
  
  /** Relays a received value, if the value satisfies a predicate,
    * otherwise relays a trap; equivalent to `filter`.
    * @group Composing */
  def withFilter(p: A => Boolean): Relay[A]
  
  /** Relays the pair of values received by this and another future,
    * otherwise relays a produced or consumed exception.
    * @group Composing */
  def zip[B](that: Relay[B]): Relay[(A, B)]
}