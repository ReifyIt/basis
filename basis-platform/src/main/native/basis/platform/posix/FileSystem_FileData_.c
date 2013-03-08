/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

#include "basis/platform/posix/package.h"

#include <errno.h>
#include <string.h>
#include <sys/mman.h>

static jobject JNICALL FileData$_map(JNIEnv *env, jobject self, jlong address, jlong length, jint prot, jint flags, jobject file, jlong offset);

static const int FileData$MethodCount = 1;
static const JNINativeMethod FileData$Methods[FileData$MethodCount] = {
  {"map", "(JJIILbasis/platform/posix/FileSystem$File;J)Lbasis/platform/posix/FileSystem$FileData;", FileData$_map}
};

jint basis_platform_posix_FileSystem$FileData$_onLoad(JNIEnv *env) {
  jclass FileData$Class;
  if ((FileData$Class = (*env)->FindClass(env, "basis/platform/posix/FileSystem$FileData$")) == NULL) return JNI_ERR;
  
  if ((*env)->RegisterNatives(env, FileData$Class, FileData$Methods, FileData$MethodCount) != JNI_OK) return JNI_ERR;
  
  (*env)->DeleteLocalRef(env, FileData$Class);
  return JNI_OK;
}

static void throwNewIOException(JNIEnv *env, const char *message) {
  jclass IOExceptionClass;
  if ((IOExceptionClass = (*env)->FindClass(env, "java/io/IOException")) == NULL) return;
  (*env)->ThrowNew(env, IOExceptionClass, message);
  (*env)->DeleteLocalRef(env, IOExceptionClass);
}

static jobject JNICALL FileData$_map(JNIEnv *env, jobject self, jlong address, jlong length, jint prot, jint flags, jobject file, jlong offset) {
  int fd = (*env)->GetIntField(env, file, basis_platform_posix_FileSystem$File_fd);
  
  void *region;
  if ((region = mmap((void *)address, (size_t)length, prot, flags, fd, (off_t)offset)) == MAP_FAILED) {
    throwNewIOException(env, strerror(errno));
    return NULL;
  }
  
  return (*env)->NewObject(env, basis_platform_posix_FileSystem$FileData, basis_platform_posix_FileSystem$FileData_new, (jlong)region, length);
}
