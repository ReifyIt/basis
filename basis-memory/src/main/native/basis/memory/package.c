/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

#include "basis/memory/package.h"

jint basis_memory_NativeData_onLoad(JNIEnv *env);

jint basis_memory_package_onLoad(JNIEnv *env) {
  if (basis_memory_NativeData_onLoad(env) != JNI_OK) return JNI_ERR;
  
  return JNI_OK;
}

void basis_memory_package_onUnload(JNIEnv *env) {
  
}
