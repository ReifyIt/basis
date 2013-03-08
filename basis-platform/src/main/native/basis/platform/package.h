/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

#ifndef BASIS_PLATFORM_PACKAGE_H
#define BASIS_PLATFORM_PACKAGE_H

#include <jni.h>

extern jfieldID basis_platform_NativeData_base;
extern jfieldID basis_platform_NativeData_size;

jint basis_platform_package_onLoad(JNIEnv *env);
void basis_platform_package_onUnload(JNIEnv *env);

#endif /* BASIS_PLATFORM_PACKAGE_H */
