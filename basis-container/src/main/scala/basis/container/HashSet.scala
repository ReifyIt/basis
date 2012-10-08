/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis._
import basis.util._

final class HashSet[A] private (
    private val nodeMap: Int,
    private val itemMap: Int,
    private val slots: scala.Array[AnyRef])
  extends Set[A] {
  
  override type Self = HashSet[A]
  
  override def iterator: Iterator[A] = ???
  
  override def contains(element: A): Boolean =
    contains(element, element.hashCode)
  
  override def + (element: A): HashSet[A] =
    updated(element, element.hashCode, 0)
  
  override def - (element: A): HashSet[A] =
    removed(element, element.hashCode)
  
  private def contains(item: A, hash: Int): Boolean = {
    if (nodeMap != 0) { // test for non-empty hash trie
      val n = 1 << (hash & 0x1F) // convert low 5 hash bits to an index bit
      val i = (nodeMap & (n - 1)).bitCount // take hamming weight of n-bit node map as array index
      if ((itemMap & n) != 0) // test if slot contains an item
        slots(i).equals(item)
      else if ((nodeMap & n) != 0) // test if slot contains a subset
        slots(i).asInstanceOf[HashSet[A]].contains(item, hash >>> 5)
      else false
    }
    else { // search collision array
      for (i <- 0 until slots.length) // linearly search array for item
        if (slots(i).equals(item)) return true;
      false
    }
  }
  
  private def updated(item: A, hash: Int, shift: Int): HashSet[A] = {
    val n = 1 << ((hash >>> shift) & 0x1F) // convert shifted low 5 hash bits to an index bit
    if (slots.length == 0) { // test for empty set
      val newSlots = new scala.Array[AnyRef](1)
      newSlots(0) = item.asInstanceOf[AnyRef]
      new HashSet[A](n, n, newSlots)
    }
    else if (nodeMap != 0) { // test for non-empty hash trie
      val i = (nodeMap & (n - 1)).bitCount // take hamming weight of n-bit node map as array index
      if ((nodeMap & n) != 0) { // test if slot already contains a node
        if ((itemMap & n) != 0) { // test if slot contains an item
          val node = slots(i)
          if (node.equals(item)) this // return unmodified sets
          else { // merge new item with existing item
            val newNode = resolve(item.asInstanceOf[AnyRef], hash >>> (shift + 5), node, node.hashCode >>> (shift + 5))
            val newSlots = slots.clone
            newSlots(i) = newNode
            new HashSet[A](nodeMap, itemMap ^ n, newSlots)
          }
        }
        else { // update existing subset with new item
          val node = slots(i).asInstanceOf[HashSet[A]]
          val newNode = node.updated(item, hash, shift + 5)
          if (node eq newNode) this // return unmodified sets
          else { // replace existing subset with updated subset
            val newSlots = slots.clone
            newSlots(i) = newNode
            new HashSet[A](nodeMap, itemMap, newSlots)
          }
        }
      }
      else { // add new item to trie
        val newSlots = new scala.Array[AnyRef](slots.length + 1)
        java.lang.System.arraycopy(slots, 0, newSlots, 0, i)
        newSlots(i) = item.asInstanceOf[AnyRef]
        java.lang.System.arraycopy(slots, i, newSlots, i + 1, slots.length - i)
        new HashSet[A](nodeMap | n, itemMap | n, newSlots)
      }
    }
    else { // merge new item with collision array
      val node = slots(0)
      val nodeHash = node.hashCode
      if (hash != nodeHash) // test for hash collision
        resolve(item.asInstanceOf[AnyRef], hash >>> shift, this, nodeHash >>> shift)
      else { // add item to collision array
        val newSlots = new scala.Array[AnyRef](slots.length + 1)
        java.lang.System.arraycopy(slots, 0, newSlots, 0, slots.length)
        newSlots(slots.length) = item.asInstanceOf[AnyRef]
        new HashSet(0, 0, newSlots)
      }
    }
  }
  
  private def resolve(itemA: AnyRef, hashA: Int, itemB: AnyRef, hashB: Int): HashSet[A] = {
    if (hashA == hashB) { // test for hash collision
      val newSlots = new scala.Array[AnyRef](2)
      newSlots(0) = itemA
      newSlots(1) = itemB
      new HashSet[A](0, 0, newSlots)
    }
    else { // create new set with items
      val bitA = 1 << (hashA & 0x1F)
      val bitB = 1 << (hashB & 0x1F)
      val nodeMap = bitA | bitB
      if (bitA == bitB) { // test if low 5 hash bits collide
        val newNode = resolve(itemA, hashA >>> 5, itemB, hashB >>> 5)
        val newSlots = new scala.Array[AnyRef](1)
        newSlots(0) = newNode
        new HashSet[A](nodeMap, nodeMap, newSlots)
      }
      else { // no collision
        val newSlots = new scala.Array[AnyRef](2)
        if (((bitA - 1) & bitB) == 0) {
          newSlots(0) = itemA
          newSlots(1) = itemB
        }
        else {
          newSlots(0) = itemB
          newSlots(1) = itemA
        }
        new HashSet[A](nodeMap, nodeMap, newSlots)
      }
    }
  }
  
  private def removed(item: A, hash: Int): HashSet[A] = {
    if (nodeMap != 0) { // test for non-empty hash trie
      val n = 1 << (hash & 0x1F) // convert low 5 hash bits to an index bit
      if ((nodeMap & n) != 0) { // test if slot contains a node
        val i = (nodeMap & (n - 1)).bitCount // take hamming weight of n-bit node map as array index
        if ((itemMap & n) != 0) { // test if slot contains an item
          val node = slots(i)
          if (node.equals(item)) { // remove item from trie
            val newSlots = new scala.Array[AnyRef](slots.length - 1)
            java.lang.System.arraycopy(slots, 0, newSlots, 0, i)
            java.lang.System.arraycopy(slots, i + 1, newSlots, i, newSlots.length - i)
            new HashSet[A](nodeMap & ~n, itemMap & ~n, newSlots)
          }
          else this // item not in set
        }
        else { // slot contains a subset
          val node = slots(i).asInstanceOf[HashSet[A]]
          val newNode = node.removed(item, hash >>> 5) // remove item from subset
          if (node eq newNode) this // return unmodified sets
          else if (node.slots.length == 0) { // remove empty subsets
            val newSlots = new scala.Array[AnyRef](slots.length - 1)
            java.lang.System.arraycopy(slots, 0, newSlots, 0, i)
            java.lang.System.arraycopy(slots, i + 1, newSlots, i, newSlots.length - i)
            new HashSet[A](nodeMap ^ n, itemMap, newSlots)
          }
          else if (node.slots.length == 1) { // lift unary subsets
            val newSlots = slots.clone
            newSlots(i) = newNode.slots(0)
            new HashSet[A](nodeMap, itemMap | n, newSlots)
          }
          else { // replace existing subset with updated subset
            val newSlots = slots.clone
            newSlots(i) = newNode
            new HashSet[A](nodeMap, itemMap, newSlots)
          }
        }
      }
      else this // item not in set
    }
    else { // search collision array
      for (i <- 0 until slots.length) { // linearly search array for item
        if (slots(i).equals(item)) { // remove found item from collision array
          val newSlots = new scala.Array[AnyRef](slots.length - 1)
          java.lang.System.arraycopy(slots, 0, newSlots, 0, i)
          java.lang.System.arraycopy(slots, i + 1, newSlots, i, newSlots.length - i)
          return new HashSet[A](0, 0, newSlots)
        }
      }
      this // item not in set
    }
  }
}
