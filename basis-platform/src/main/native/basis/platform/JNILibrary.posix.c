/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

#include "basis/platform/package.h"
#include "basis/platform/posix/package.h"

JNIEXPORT jint JNICALL JNI_OnLoad(JavaVM *jvm, void *reserved) {
  JNIEnv *env;
  if ((*jvm)->GetEnv(jvm, (void **)&env, JNI_VERSION_1_4) != JNI_OK) return JNI_ERR;
  
  if (basis_platform_package_onLoad(env) != JNI_OK) return JNI_ERR;
  if (basis_platform_posix_package_onLoad(env) != JNI_OK) return JNI_ERR;
  
  return JNI_VERSION_1_4;
}

JNIEXPORT void JNICALL JNI_OnUnload(JavaVM *jvm, void *reserved) {
  JNIEnv *env;
  if ((*jvm)->GetEnv(jvm, (void **)&env, JNI_VERSION_1_4) != JNI_OK) return;
  
  basis_platform_posix_package_onUnload(env);
  basis_platform_package_onUnload(env);
}
