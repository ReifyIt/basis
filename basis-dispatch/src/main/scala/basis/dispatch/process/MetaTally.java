/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.dispatch.process;

final class MetaTally {
  private MetaTally() {}
  
  static final sun.misc.Unsafe Unsafe;
  static final long JoinAllBatchOffset;
  static final long JoinAnyCountOffset;
  static final long JoinFirstCountOffset;
  static final long ReduceReadyOffset;
  static final long ReduceCountOffset;
  static final long ForAllCountOffset;
  static final long ExistsCountOffset;
  static final long CountTotalOffset;
  static final long CountCountOffset;
  
  static {
    try {
      java.lang.reflect.Field theUnsafeField = sun.misc.Unsafe.class.getDeclaredField("theUnsafe");
      theUnsafeField.setAccessible(true);
      Unsafe = (sun.misc.Unsafe)theUnsafeField.get(null);
      
      Class<Tally.JoinAll> JoinAllClass = Tally.JoinAll.class;
      JoinAllBatchOffset = Unsafe.objectFieldOffset(JoinAllClass.getDeclaredField("batch"));
      
      Class<Tally.JoinAny> JoinAnyClass = Tally.JoinAny.class;
      JoinAnyCountOffset = Unsafe.objectFieldOffset(JoinAnyClass.getDeclaredField("count"));
      
      Class<Tally.JoinFirst> JoinFirstClass = Tally.JoinFirst.class;
      JoinFirstCountOffset = Unsafe.objectFieldOffset(JoinFirstClass.getDeclaredField("count"));
      
      Class<Tally.Reduce> ReduceClass = Tally.Reduce.class;
      ReduceReadyOffset = Unsafe.objectFieldOffset(ReduceClass.getDeclaredField("ready"));
      ReduceCountOffset = Unsafe.objectFieldOffset(ReduceClass.getDeclaredField("count"));
      
      Class<Tally.ForAll> ForAllClass = Tally.ForAll.class;
      ForAllCountOffset = Unsafe.objectFieldOffset(ForAllClass.getDeclaredField("count"));
      
      Class<Tally.Exists> ExistsClass = Tally.Exists.class;
      ExistsCountOffset = Unsafe.objectFieldOffset(ExistsClass.getDeclaredField("count"));
      
      Class<Tally.Count> CountClass = Tally.Count.class;
      CountTotalOffset = Unsafe.objectFieldOffset(CountClass.getDeclaredField("total"));
      CountCountOffset = Unsafe.objectFieldOffset(CountClass.getDeclaredField("count"));
    }
    catch (Exception e) {
      throw new Error(e);
    }
  }
}
