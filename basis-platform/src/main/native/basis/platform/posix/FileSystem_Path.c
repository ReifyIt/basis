/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

#include "basis/platform/posix/package.h"

jclass basis_platform_posix_FileSystem$Path;
jmethodID basis_platform_posix_FileSystem$Path_toString;

jint basis_platform_posix_FileSystem$Path_onLoad(JNIEnv *env) {
  jclass PathClass;
  if ((PathClass = (*env)->FindClass(env, "basis/platform/posix/FileSystem$Path")) == NULL) return JNI_ERR;
  
  basis_platform_posix_FileSystem$Path = (*env)->NewGlobalRef(env, PathClass);
  if ((basis_platform_posix_FileSystem$Path_toString = (*env)->GetMethodID(env, PathClass, "toString", "()Ljava/lang/String;")) == NULL) return JNI_ERR;
  
  (*env)->DeleteLocalRef(env, PathClass);
  return JNI_OK;
}

void basis_platform_posix_FileSystem$Path_onUnload(JNIEnv *env) {
  (*env)->DeleteGlobalRef(env, basis_platform_posix_FileSystem$Path);
}
