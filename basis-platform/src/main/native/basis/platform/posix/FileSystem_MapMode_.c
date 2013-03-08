/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

#include "basis/platform/posix/package.h"

#include <sys/mman.h>

static jint setIntConst(JNIEnv *env, jclass cls, jobject self, const char *name, jint value) {
  jfieldID field;
  if ((field = (*env)->GetFieldID(env, cls, name, "I")) == NULL) return JNI_ERR;
  (*env)->SetIntField(env, self, field, value);
  return JNI_OK;
}

jint basis_platform_posix_FileSystem$MapMode$_onLoad(JNIEnv *env) {
  jclass MapMode$Class;
  jfieldID MODULE$Field;
  if ((MapMode$Class = (*env)->FindClass(env, "basis/platform/posix/FileSystem$MapMode$")) == NULL) return JNI_ERR;
  if ((MODULE$Field = (*env)->GetStaticFieldID(env, MapMode$Class, "MODULE$", "Lbasis/platform/posix/FileSystem$MapMode$;")) == NULL) return JNI_ERR;
  jobject MapMode$ = (*env)->GetStaticObjectField(env, MapMode$Class, MODULE$Field);
  
  if (setIntConst(env, MapMode$Class, MapMode$, "MAP_SHARED", MAP_SHARED) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, MapMode$Class, MapMode$, "MAP_PRIVATE", MAP_PRIVATE) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, MapMode$Class, MapMode$, "MAP_FIXED", MAP_FIXED) != JNI_OK) return JNI_ERR;
  
  (*env)->DeleteLocalRef(env, MapMode$);
  (*env)->DeleteLocalRef(env, MapMode$Class);
  return JNI_OK;
}
