/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

#include "basis/platform/posix/package.h"

#include <errno.h>
#include <fcntl.h>
#include <string.h>

static jobject JNICALL File$_open(JNIEnv *env, jobject self, jobject path, jint flags, jint mode);

static const int File$MethodCount = 1;
static const JNINativeMethod File$Methods[File$MethodCount] = {
  {"open", "(Lbasis/platform/posix/FileSystem$Path;II)Lbasis/platform/posix/FileSystem$File;", File$_open}
};

jint basis_platform_posix_FileSystem$File$_onLoad(JNIEnv *env) {
  jclass File$Class;
  if ((File$Class = (*env)->FindClass(env, "basis/platform/posix/FileSystem$File$")) == NULL) return JNI_ERR;
  
  if ((*env)->RegisterNatives(env, File$Class, File$Methods, File$MethodCount) != JNI_OK) return JNI_ERR;
  
  (*env)->DeleteLocalRef(env, File$Class);
  return JNI_OK;
}

static void throwNewIOException(JNIEnv *env, const char *message) {
  jclass IOExceptionClass;
  if ((IOExceptionClass = (*env)->FindClass(env, "java/io/IOException")) == NULL) return;
  (*env)->ThrowNew(env, IOExceptionClass, message);
  (*env)->DeleteLocalRef(env, IOExceptionClass);
}

static jobject JNICALL File$_open(JNIEnv *env, jobject self, jobject path, jint flags, jint mode) {
  jstring pathString = (*env)->CallObjectMethod(env, path, basis_platform_posix_FileSystem$Path_toString);
  if ((*env)->ExceptionCheck(env)) return NULL;
  const char *pathChars = (*env)->GetStringUTFChars(env, pathString, NULL);
  
  int fd = open(pathChars, flags, mode);
  
  (*env)->ReleaseStringUTFChars(env, pathString, pathChars);
  (*env)->DeleteLocalRef(env, pathString);
  
  if (fd < 0) {
    throwNewIOException(env, strerror(errno));
    return NULL;
  }
  
  return (*env)->NewObject(env, basis_platform_posix_FileSystem$File, basis_platform_posix_FileSystem$File_new, fd);
}
