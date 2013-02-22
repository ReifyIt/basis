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
  
  static final class MetaJoinAll {
    private MetaJoinAll() {}
    
    static final long BatchOffset;
    
    static {
      try {
        Class<Thunk.JoinAll> ThunkJoinAllClass = Thunk.JoinAll.class;
        BatchOffset = Unsafe.objectFieldOffset(ThunkJoinAllClass.getDeclaredField("batch"));
      }
      catch (Exception e) {
        throw new Error(e);
      }
    }
  }
  
  static final class MetaJoinAny {
    private MetaJoinAny() {}
    
    static final long CountOffset;
    
    static {
      try {
        Class<Thunk.JoinAny> ThunkJoinAnyClass = Thunk.JoinAny.class;
        CountOffset = Unsafe.objectFieldOffset(ThunkJoinAnyClass.getDeclaredField("count"));
      }
      catch (Exception e) {
        throw new Error(e);
      }
    }
  }
  
  static final class MetaJoinFirst {
    private MetaJoinFirst() {}
    
    static final long CountOffset;
    
    static {
      try {
        Class<Thunk.JoinFirst> ThunkJoinFirstClass = Thunk.JoinFirst.class;
        CountOffset = Unsafe.objectFieldOffset(ThunkJoinFirstClass.getDeclaredField("count"));
      }
      catch (Exception e) {
        throw new Error(e);
      }
    }
  }
  
  static final class MetaReduce {
    private MetaReduce() {}
    
    static final long ReadyOffset;
    static final long CountOffset;
    
    static {
      try {
        Class<Thunk.Reduce> ThunkReduceClass = Thunk.Reduce.class;
        ReadyOffset = Unsafe.objectFieldOffset(ThunkReduceClass.getDeclaredField("ready"));
        CountOffset = Unsafe.objectFieldOffset(ThunkReduceClass.getDeclaredField("count"));
      }
      catch (Exception e) {
        throw new Error(e);
      }
    }
  }
}
