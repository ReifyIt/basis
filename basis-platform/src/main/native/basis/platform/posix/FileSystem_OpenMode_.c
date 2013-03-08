/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

#include "basis/platform/posix/package.h"

#include <fcntl.h>

static jint setIntConst(JNIEnv *env, jclass cls, jobject self, const char *name, jint value) {
  jfieldID field;
  if ((field = (*env)->GetFieldID(env, cls, name, "I")) == NULL) return JNI_ERR;
  (*env)->SetIntField(env, self, field, value);
  return JNI_OK;
}

jint basis_platform_posix_FileSystem$OpenMode$_onLoad(JNIEnv *env) {
  jclass OpenMode$Class;
  jfieldID MODULE$Field;
  if ((OpenMode$Class = (*env)->FindClass(env, "basis/platform/posix/FileSystem$OpenMode$")) == NULL) return JNI_ERR;
  if ((MODULE$Field = (*env)->GetStaticFieldID(env, OpenMode$Class, "MODULE$", "Lbasis/platform/posix/FileSystem$OpenMode$;")) == NULL) return JNI_ERR;
  jobject OpenMode$ = (*env)->GetStaticObjectField(env, OpenMode$Class, MODULE$Field);
  
  if (setIntConst(env, OpenMode$Class, OpenMode$, "O_RDONLY", O_RDONLY) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, OpenMode$Class, OpenMode$, "O_WRONLY", O_WRONLY) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, OpenMode$Class, OpenMode$, "O_RDWR", O_RDWR) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, OpenMode$Class, OpenMode$, "O_CREAT", O_CREAT) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, OpenMode$Class, OpenMode$, "O_EXCL", O_EXCL) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, OpenMode$Class, OpenMode$, "O_NOCTTY", O_NOCTTY) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, OpenMode$Class, OpenMode$, "O_TRUNC", O_TRUNC) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, OpenMode$Class, OpenMode$, "O_APPEND", O_APPEND) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, OpenMode$Class, OpenMode$, "O_DSYNC", O_DSYNC) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, OpenMode$Class, OpenMode$, "O_NONBLOCK", O_NONBLOCK) != JNI_OK) return JNI_ERR;
  #ifdef O_RSYNC
  if (setIntConst(env, OpenMode$Class, OpenMode$, "O_RSYNC", O_RSYNC) != JNI_OK) return JNI_ERR;
  #endif
  #ifdef O_SYNC
  if (setIntConst(env, OpenMode$Class, OpenMode$, "O_SYNC", O_SYNC) != JNI_OK) return JNI_ERR;
  #endif
  
  (*env)->DeleteLocalRef(env, OpenMode$);
  (*env)->DeleteLocalRef(env, OpenMode$Class);
  return JNI_OK;
}
