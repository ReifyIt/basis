/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

#include "basis/platform/package.h"
#include "basis/platform/posix/package.h"

#include <sys/mman.h>

jclass basis_platform_posix_FileSystem$FileData;
jmethodID basis_platform_posix_FileSystem$FileData_new;

static void JNICALL finalize(JNIEnv *env, jobject self);

static const int FileDataMethodCount = 1;
static const JNINativeMethod FileDataMethods[FileDataMethodCount] = {
  {"finalize", "()V", finalize}
};

jint basis_platform_posix_FileSystem$FileData_onLoad(JNIEnv *env) {
  jclass FileDataClass;
  if ((FileDataClass = (*env)->FindClass(env, "basis/platform/posix/FileSystem$FileData")) == NULL) return JNI_ERR;
  
  basis_platform_posix_FileSystem$FileData = (*env)->NewGlobalRef(env, FileDataClass);
  if ((basis_platform_posix_FileSystem$FileData_new = (*env)->GetMethodID(env, FileDataClass, "<init>", "(JJ)V")) == NULL) return JNI_ERR;
  if ((*env)->RegisterNatives(env, FileDataClass, FileDataMethods, FileDataMethodCount) != JNI_OK) return JNI_ERR;
  
  (*env)->DeleteLocalRef(env, FileDataClass);
  return JNI_OK;
}

void basis_platform_posix_FileSystem$FileData_onUnload(JNIEnv *env) {
  (*env)->DeleteGlobalRef(env, basis_platform_posix_FileSystem$FileData);
}

static void JNICALL finalize(JNIEnv *env, jobject self) {
  void *region = (void *)(*env)->GetLongField(env, self, basis_platform_NativeData_base);
  size_t length = (size_t)(*env)->GetLongField(env, self, basis_platform_NativeData_size);
  munmap(region, length);
}
