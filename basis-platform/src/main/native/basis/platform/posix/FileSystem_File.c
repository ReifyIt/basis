/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

#include "basis/platform/posix/package.h"

#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>

jclass basis_platform_posix_FileSystem$File;
jmethodID basis_platform_posix_FileSystem$File_new;
jfieldID basis_platform_posix_FileSystem$File_fd;

static void JNICALL File_close(JNIEnv *env, jobject self);
static jlong JNICALL File_size(JNIEnv *env, jobject self);
static void JNICALL File_setSize(JNIEnv *env, jobject self, jlong newSize);

static const int FileMethodCount = 3;
static const JNINativeMethod FileMethods[FileMethodCount] = {
  {"close", "()V", File_close},
  {"size", "()J", File_size},
  {"size_$eq", "(J)V", File_setSize}
};

jint basis_platform_posix_FileSystem$File_onLoad(JNIEnv *env) {
  jclass FileClass;
  if ((FileClass = (*env)->FindClass(env, "basis/platform/posix/FileSystem$File")) == NULL) return JNI_ERR;
  
  basis_platform_posix_FileSystem$File = (*env)->NewGlobalRef(env, FileClass);
  if ((basis_platform_posix_FileSystem$File_new = (*env)->GetMethodID(env, FileClass, "<init>", "(I)V")) == NULL) return JNI_ERR;
  if ((basis_platform_posix_FileSystem$File_fd = (*env)->GetFieldID(env, FileClass, "fd", "I")) == NULL) return JNI_ERR;
  if ((*env)->RegisterNatives(env, FileClass, FileMethods, FileMethodCount) != JNI_OK) return JNI_ERR;
  
  (*env)->DeleteLocalRef(env, FileClass);
  return JNI_OK;
}

void basis_platform_posix_FileSystem$File_onUnload(JNIEnv *env) {
  (*env)->DeleteGlobalRef(env, basis_platform_posix_FileSystem$File);
}

static void throwNewIOException(JNIEnv *env, const char *message) {
  jclass IOExceptionClass;
  if ((IOExceptionClass = (*env)->FindClass(env, "java/io/IOException")) == NULL) return;
  (*env)->ThrowNew(env, IOExceptionClass, message);
  (*env)->DeleteLocalRef(env, IOExceptionClass);
}

static void JNICALL File_close(JNIEnv *env, jobject self) {
  int fd = (*env)->GetIntField(env, self, basis_platform_posix_FileSystem$File_fd);
  if (fd < 0) return;
  
  (*env)->SetIntField(env, self, basis_platform_posix_FileSystem$File_fd, -1);
  if (close(fd) < 0) throwNewIOException(env, strerror(errno));
}

static jlong JNICALL File_size(JNIEnv *env, jobject self) {
  int fd = (*env)->GetIntField(env, self, basis_platform_posix_FileSystem$File_fd);
  if (fd < 0) {
    throwNewIOException(env, "invalid file descriptor");
    return 0;
  }
  
  struct stat info;
  if (fstat(fd, &info) < 0) {
    throwNewIOException(env, strerror(errno));
    return 0;
  }
  
  return info.st_size;
}

static void JNICALL File_setSize(JNIEnv *env, jobject self, jlong newSize) {
  int fd = (*env)->GetIntField(env, self, basis_platform_posix_FileSystem$File_fd);
  if (fd < 0) {
    throwNewIOException(env, "invalid file descriptor");
    return;
  }
  
  if (ftruncate(fd, (off_t)newSize) < 0) {
    throwNewIOException(env, strerror(errno));
    return;
  }
}
