/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

#ifndef basis_memory_package_h
#define basis_memory_package_h

#include <jni.h>

jint basis_memory_package_onLoad(JNIEnv *env);
void basis_memory_package_onUnload(JNIEnv *env);

#endif /* basis_memory_package_h */
