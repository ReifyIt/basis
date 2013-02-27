/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.dispatch.process;

final class MetaPrime {
  private MetaPrime() {}
  
  static final sun.misc.Unsafe Unsafe;
  
  static final long RouteControlOffset;
  
  static {
    try {
      java.lang.reflect.Field theUnsafeField = sun.misc.Unsafe.class.getDeclaredField("theUnsafe");
      theUnsafeField.setAccessible(true);
      Unsafe = (sun.misc.Unsafe)theUnsafeField.get(null);
      
      Class<PrimeRoute.Route> RouteClass = PrimeRoute.Route.class;
      RouteControlOffset = Unsafe.objectFieldOffset(RouteClass.getDeclaredField("control"));
    }
    catch (Exception e) {
      throw new Error(e);
    }
  }
}
