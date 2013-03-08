/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

#include "basis/platform/posix/package.h"

#include <sys/stat.h>

static jint setIntConst(JNIEnv *env, jclass cls, jobject self, const char *name, jint value) {
  jfieldID field;
  if ((field = (*env)->GetFieldID(env, cls, name, "I")) == NULL) return JNI_ERR;
  (*env)->SetIntField(env, self, field, value);
  return JNI_OK;
}

jint basis_platform_posix_FileSystem$FileMode$_onLoad(JNIEnv *env) {
  jclass FileMode$Class;
  jfieldID MODULE$Field;
  if ((FileMode$Class = (*env)->FindClass(env, "basis/platform/posix/FileSystem$FileMode$")) == NULL) return JNI_ERR;
  if ((MODULE$Field = (*env)->GetStaticFieldID(env, FileMode$Class, "MODULE$", "Lbasis/platform/posix/FileSystem$FileMode$;")) == NULL) return JNI_ERR;
  jobject FileMode$ = (*env)->GetStaticObjectField(env, FileMode$Class, MODULE$Field);
  
  if (setIntConst(env, FileMode$Class, FileMode$, "S_IFBLK", S_IFBLK) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, FileMode$Class, FileMode$, "S_IFCHR", S_IFCHR) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, FileMode$Class, FileMode$, "S_IFIFO", S_IFIFO) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, FileMode$Class, FileMode$, "S_IFREG", S_IFREG) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, FileMode$Class, FileMode$, "S_IFDIR", S_IFDIR) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, FileMode$Class, FileMode$, "S_IFLNK", S_IFLNK) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, FileMode$Class, FileMode$, "S_IFSOCK", S_IFSOCK) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, FileMode$Class, FileMode$, "S_IRUSR", S_IRUSR) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, FileMode$Class, FileMode$, "S_IWUSR", S_IWUSR) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, FileMode$Class, FileMode$, "S_IXUSR", S_IXUSR) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, FileMode$Class, FileMode$, "S_IRGRP", S_IRGRP) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, FileMode$Class, FileMode$, "S_IWGRP", S_IWGRP) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, FileMode$Class, FileMode$, "S_IXGRP", S_IXGRP) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, FileMode$Class, FileMode$, "S_IROTH", S_IROTH) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, FileMode$Class, FileMode$, "S_IWOTH", S_IWOTH) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, FileMode$Class, FileMode$, "S_IXOTH", S_IXOTH) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, FileMode$Class, FileMode$, "S_ISUID", S_ISUID) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, FileMode$Class, FileMode$, "S_ISGID", S_ISGID) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, FileMode$Class, FileMode$, "S_ISVTX", S_ISVTX) != JNI_OK) return JNI_ERR;
  
  (*env)->DeleteLocalRef(env, FileMode$);
  (*env)->DeleteLocalRef(env, FileMode$Class);
  return JNI_OK;
}
