/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.dispatch;

final class MetaAsync {
  private MetaAsync() {}
  
  static final sun.misc.Unsafe Unsafe;
  static final long WorkersOffset;
  static final long WorkingOffset;
  static final long BlockedOffset;
  static final long WaitingOffset;
  static final long QueueOffset;
  static final long NextUIDOffset;
  
  static {
    try {
      java.lang.reflect.Field theUnsafeField = sun.misc.Unsafe.class.getDeclaredField("theUnsafe");
      theUnsafeField.setAccessible(true);
      Unsafe = (sun.misc.Unsafe)theUnsafeField.get(null);
      Class<Async> AsyncClass = Async.class;
      WorkersOffset = Unsafe.objectFieldOffset(AsyncClass.getDeclaredField("workers"));
      WorkingOffset = Unsafe.objectFieldOffset(AsyncClass.getDeclaredField("working"));
      BlockedOffset = Unsafe.objectFieldOffset(AsyncClass.getDeclaredField("blocked"));
      WaitingOffset = Unsafe.objectFieldOffset(AsyncClass.getDeclaredField("waiting"));
      QueueOffset   = Unsafe.objectFieldOffset(AsyncClass.getDeclaredField("queue"));
      NextUIDOffset = Unsafe.objectFieldOffset(AsyncClass.getDeclaredField("nextUID"));
    }
    catch (Exception e) {
      throw new Error(e);
    }
  }
  
  static final class MetaWorkerThread {
    private MetaWorkerThread() {}
    
    static final long QueueOffset;
    
    static {
      try {
        Class<Async.WorkerThread> AsyncWorkerThreadClass = Async.WorkerThread.class;
        QueueOffset = Unsafe.objectFieldOffset(AsyncWorkerThreadClass.getDeclaredField("queue"));
      }
      catch (Exception e) {
        throw new Error(e);
      }
    }
  }
}
