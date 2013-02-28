/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.dispatch.process;

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
  
  static final class MetaForAll {
    private MetaForAll() {}
    
    static final long CountOffset;
    
    static {
      try {
        Class<Thunk.ForAll> ThunkForAllClass = Thunk.ForAll.class;
        CountOffset = Unsafe.objectFieldOffset(ThunkForAllClass.getDeclaredField("count"));
      }
      catch (Exception e) {
        throw new Error(e);
      }
    }
  }
  
  static final class MetaExists {
    private MetaExists() {}
    
    static final long CountOffset;
    
    static {
      try {
        Class<Thunk.Exists> ThunkExistsClass = Thunk.Exists.class;
        CountOffset = Unsafe.objectFieldOffset(ThunkExistsClass.getDeclaredField("count"));
      }
      catch (Exception e) {
        throw new Error(e);
      }
    }
  }
  
  static final class MetaCount {
    private MetaCount() {}
    
    static final long TotalOffset;
    static final long CountOffset;
    
    static {
      try {
        Class<Thunk.Count> ThunkCountClass = Thunk.Count.class;
        TotalOffset = Unsafe.objectFieldOffset(ThunkCountClass.getDeclaredField("total"));
        CountOffset = Unsafe.objectFieldOffset(ThunkCountClass.getDeclaredField("count"));
      }
      catch (Exception e) {
        throw new Error(e);
      }
    }
  }
}
