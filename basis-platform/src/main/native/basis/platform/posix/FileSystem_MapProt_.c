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

jint basis_platform_posix_FileSystem$MapProt$_onLoad(JNIEnv *env) {
  jclass MapProt$Class;
  jfieldID MODULE$Field;
  if ((MapProt$Class = (*env)->FindClass(env, "basis/platform/posix/FileSystem$MapProt$")) == NULL) return JNI_ERR;
  if ((MODULE$Field = (*env)->GetStaticFieldID(env, MapProt$Class, "MODULE$", "Lbasis/platform/posix/FileSystem$MapProt$;")) == NULL) return JNI_ERR;
  jobject MapProt$ = (*env)->GetStaticObjectField(env, MapProt$Class, MODULE$Field);
  
  if (setIntConst(env, MapProt$Class, MapProt$, "PROT_READ", PROT_READ) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, MapProt$Class, MapProt$, "PROT_WRITE", PROT_WRITE) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, MapProt$Class, MapProt$, "PROT_EXEC", PROT_EXEC) != JNI_OK) return JNI_ERR;
  if (setIntConst(env, MapProt$Class, MapProt$, "PROT_NONE", PROT_NONE) != JNI_OK) return JNI_ERR;
  
  (*env)->DeleteLocalRef(env, MapProt$);
  (*env)->DeleteLocalRef(env, MapProt$Class);
  return JNI_OK;
}
