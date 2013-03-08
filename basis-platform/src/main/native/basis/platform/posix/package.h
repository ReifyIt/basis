/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

#ifndef BASIS_PLATFORM_POSIX_PACKAGE_H
#define BASIS_PLATFORM_POSIX_PACKAGE_H

#include <jni.h>

extern jclass basis_platform_posix_FileSystem$Path;
extern jmethodID basis_platform_posix_FileSystem$Path_toString;

extern jclass basis_platform_posix_FileSystem$File;
extern jmethodID basis_platform_posix_FileSystem$File_new;
extern jfieldID basis_platform_posix_FileSystem$File_fd;

extern jclass basis_platform_posix_FileSystem$FileData;
extern jmethodID basis_platform_posix_FileSystem$FileData_new;

jint basis_platform_posix_package_onLoad(JNIEnv *env);
void basis_platform_posix_package_onUnload(JNIEnv *env);

#endif /* BASIS_PLATFORM_POSIX_PACKAGE_H */
