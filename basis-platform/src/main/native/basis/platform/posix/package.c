/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

#include "basis/platform/posix/package.h"

extern jint basis_platform_posix_FileSystem$Path_onLoad(JNIEnv *env);
extern jint basis_platform_posix_FileSystem$File_onLoad(JNIEnv *env);
extern jint basis_platform_posix_FileSystem$File$_onLoad(JNIEnv *env);
extern jint basis_platform_posix_FileSystem$FileData_onLoad(JNIEnv *env);
extern jint basis_platform_posix_FileSystem$FileData$_onLoad(JNIEnv *env);
extern jint basis_platform_posix_FileSystem$FileMode$_onLoad(JNIEnv *env);
extern jint basis_platform_posix_FileSystem$OpenMode$_onLoad(JNIEnv *env);
extern jint basis_platform_posix_FileSystem$MapMode$_onLoad(JNIEnv *env);
extern jint basis_platform_posix_FileSystem$MapProt$_onLoad(JNIEnv *env);

extern void basis_platform_posix_FileSystem$FileData_onUnload(JNIEnv *env);
extern void basis_platform_posix_FileSystem$File_onUnload(JNIEnv *env);
extern void basis_platform_posix_FileSystem$Path_onUnload(JNIEnv *env);

jint basis_platform_posix_package_onLoad(JNIEnv *env) {
  if (basis_platform_posix_FileSystem$Path_onLoad(env) != JNI_OK) return JNI_ERR;
  if (basis_platform_posix_FileSystem$File_onLoad(env) != JNI_OK) return JNI_ERR;
  if (basis_platform_posix_FileSystem$File$_onLoad(env) != JNI_OK) return JNI_ERR;
  if (basis_platform_posix_FileSystem$FileData_onLoad(env) != JNI_OK) return JNI_ERR;
  if (basis_platform_posix_FileSystem$FileData$_onLoad(env) != JNI_OK) return JNI_ERR;
  if (basis_platform_posix_FileSystem$FileMode$_onLoad(env) != JNI_OK) return JNI_ERR;
  if (basis_platform_posix_FileSystem$OpenMode$_onLoad(env) != JNI_OK) return JNI_ERR;
  if (basis_platform_posix_FileSystem$MapMode$_onLoad(env) != JNI_OK) return JNI_ERR;
  if (basis_platform_posix_FileSystem$MapProt$_onLoad(env) != JNI_OK) return JNI_ERR;
  
  return JNI_OK;
}

void basis_platform_posix_package_onUnload(JNIEnv *env) {
  basis_platform_posix_FileSystem$FileData_onUnload(env);
  basis_platform_posix_FileSystem$File_onUnload(env);
  basis_platform_posix_FileSystem$Path_onUnload(env);
}
