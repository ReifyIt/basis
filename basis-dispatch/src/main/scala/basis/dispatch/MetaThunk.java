/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.dispatch;

final class MetaThunk {
  private MetaThunk() {}
  
  static final sun.misc.Unsafe Unsafe;
  static final long StateOffset;
  
  static {
    try {
      java.lang.reflect.Field theUnsafeField = sun.misc.Unsafe.class.getDeclaredField("theUnsafe");
      theUnsafeField.setAccessible(true);
      Unsafe = (sun.misc.Unsafe)theUnsafeField.get(null);
      Class<Thunk> ThunkClass = Thunk.class;
      StateOffset = Unsafe.objectFieldOffset(ThunkClass.getDeclaredField("state"));
    }
    catch (Exception e) {
      throw new Error(e);
    }
  }
  
  static final class MetaGroup {
    private MetaGroup() {}
    
    static final long StoreOffset;
    
    static {
      try {
        Class<Thunk.Group> ThunkGroupClass = Thunk.Group.class;
        StoreOffset = Unsafe.objectFieldOffset(ThunkGroupClass.getDeclaredField("store"));
      }
      catch (Exception e) {
        throw new Error(e);
      }
    }
  }
}
