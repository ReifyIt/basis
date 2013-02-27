/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.concurrent;

final class MetaAtomicStack {
  private MetaAtomicStack() {}
  
  static final sun.misc.Unsafe Unsafe;
  
  static final long StackOffset;
  
  static {
    try {
      java.lang.reflect.Field theUnsafeField = sun.misc.Unsafe.class.getDeclaredField("theUnsafe");
      theUnsafeField.setAccessible(true);
      Unsafe = (sun.misc.Unsafe)theUnsafeField.get(null);
      
      Class<AtomicStack> AtomicStackClass = AtomicStack.class;
      StackOffset = Unsafe.objectFieldOffset(AtomicStackClass.getDeclaredField("stack"));
    }
    catch (Exception e) {
      throw new Error(e);
    }
  }
}
