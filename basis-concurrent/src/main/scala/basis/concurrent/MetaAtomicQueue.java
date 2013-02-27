/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.concurrent;

final class MetaAtomicQueue {
  private MetaAtomicQueue() {}
  
  static final sun.misc.Unsafe Unsafe;
  
  static final long QueueOffset;
  
  static {
    try {
      java.lang.reflect.Field theUnsafeField = sun.misc.Unsafe.class.getDeclaredField("theUnsafe");
      theUnsafeField.setAccessible(true);
      Unsafe = (sun.misc.Unsafe)theUnsafeField.get(null);
      
      Class<AtomicQueue> AtomicQueueClass = AtomicQueue.class;
      QueueOffset = Unsafe.objectFieldOffset(AtomicQueueClass.getDeclaredField("queue"));
    }
    catch (Exception e) {
      throw new Error(e);
    }
  }
}
