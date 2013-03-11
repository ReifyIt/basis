/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

#include "basis/platform/package.h"

#include <string.h>

jfieldID basis_platform_NativeData_base;
jfieldID basis_platform_NativeData_size;

static jint JNICALL unit(JNIEnv *env, jobject self);
static jbyte JNICALL loadByte(JNIEnv *env, jobject self, jlong address);
static void JNICALL storeByte(JNIEnv *env, jobject self, jlong address, jbyte value);
static jshort JNICALL loadShort(JNIEnv *env, jobject self, jlong address);
static void JNICALL storeShort(JNIEnv *env, jobject self, jlong address, jshort value);
static jint JNICALL loadInt(JNIEnv *env, jobject self, jlong address);
static void JNICALL storeInt(JNIEnv *env, jobject self, jlong address, jint value);
static jlong JNICALL loadLong(JNIEnv *env, jobject self, jlong address);
static void JNICALL storeLong(JNIEnv *env, jobject self, jlong address, jlong value);
static jfloat JNICALL loadFloat(JNIEnv *env, jobject self, jlong address);
static void JNICALL storeFloat(JNIEnv *env, jobject self, jlong address, jfloat value);
static jdouble JNICALL loadDouble(JNIEnv *env, jobject self, jlong address);
static void JNICALL storeDouble(JNIEnv *env, jobject self, jlong address, jdouble value);
static jshort JNICALL loadUnalignedShort(JNIEnv *env, jobject self, jlong address);
static void JNICALL storeUnalignedShort(JNIEnv *env, jobject self, jlong address, jshort value);
static jint JNICALL loadUnalignedInt(JNIEnv *env, jobject self, jlong address);
static void JNICALL storeUnalignedInt(JNIEnv *env, jobject self, jlong address, jint value);
static jlong JNICALL loadUnalignedLong(JNIEnv *env, jobject self, jlong address);
static void JNICALL storeUnalignedLong(JNIEnv *env, jobject self, jlong address, jlong value);
static jfloat JNICALL loadUnalignedFloat(JNIEnv *env, jobject self, jlong address);
static void JNICALL storeUnalignedFloat(JNIEnv *env, jobject self, jlong address, jfloat value);
static jdouble JNICALL loadUnalignedDouble(JNIEnv *env, jobject self, jlong address);
static void JNICALL storeUnalignedDouble(JNIEnv *env, jobject self, jlong address, jdouble value);
static jbyte JNICALL loadVolatileByte(JNIEnv *env, jobject self, jlong address);
static void JNICALL storeVolatileByte(JNIEnv *env, jobject self, jlong address, jbyte value);
static jshort JNICALL loadVolatileShort(JNIEnv *env, jobject self, jlong address);
static void JNICALL storeVolatileShort(JNIEnv *env, jobject self, jlong address, jshort value);
static jint JNICALL loadVolatileInt(JNIEnv *env, jobject self, jlong address);
static void JNICALL storeVolatileInt(JNIEnv *env, jobject self, jlong address, jint value);
static jlong JNICALL loadVolatileLong(JNIEnv *env, jobject self, jlong address);
static void JNICALL storeVolatileLong(JNIEnv *env, jobject self, jlong address, jlong value);
static jfloat JNICALL loadVolatileFloat(JNIEnv *env, jobject self, jlong address);
static void JNICALL storeVolatileFloat(JNIEnv *env, jobject self, jlong address, jfloat value);
static jdouble JNICALL loadVolatileDouble(JNIEnv *env, jobject self, jlong address);
static void JNICALL storeVolatileDouble(JNIEnv *env, jobject self, jlong address, jdouble value);
static jboolean JNICALL compareAndSwapInt(JNIEnv *env, jobject self, jlong address, jint expected, jint value);
static jboolean JNICALL compareAndSwapLong(JNIEnv *env, jobject self, jlong address, jlong expected, jlong value);
static jboolean JNICALL compareAndSwapFloat(JNIEnv *env, jobject self, jlong address, jfloat expected, jfloat value);
static jboolean JNICALL compareAndSwapDouble(JNIEnv *env, jobject self, jlong address, jdouble expected, jdouble value);
static void JNICALL move(JNIEnv *env, jobject self, jlong fromAddress, jlong toAddress, jlong size);
static void JNICALL clear(JNIEnv *env, jobject self, jlong fromAddress, jlong untilAddress);

static const int methodCount = 41;
static const JNINativeMethod methods[methodCount] = {
  {"unit", "()I", unit},
  {"loadByte", "(J)B", loadByte},
  {"storeByte", "(JB)V", storeByte},
  {"loadShort", "(J)S", loadShort},
  {"storeShort", "(JS)V", storeShort},
  {"loadInt", "(J)I", loadInt},
  {"storeInt", "(JI)V", storeInt},
  {"loadLong", "(J)J", loadLong},
  {"storeLong", "(JJ)V", storeLong},
  {"loadFloat", "(J)F", loadFloat},
  {"storeFloat", "(JF)V", storeFloat},
  {"loadDouble", "(J)D", loadDouble},
  {"storeDouble", "(JD)V", storeDouble},
  {"loadUnalignedShort", "(J)S", loadUnalignedShort},
  {"storeUnalignedShort", "(JS)V", storeUnalignedShort},
  {"loadUnalignedInt", "(J)I", loadUnalignedInt},
  {"storeUnalignedInt", "(JI)V", storeUnalignedInt},
  {"loadUnalignedLong", "(J)J", loadUnalignedLong},
  {"storeUnalignedLong", "(JJ)V", storeUnalignedLong},
  {"loadUnalignedFloat", "(J)F", loadUnalignedFloat},
  {"storeUnalignedFloat", "(JF)V", storeUnalignedFloat},
  {"loadUnalignedDouble", "(J)D", loadUnalignedDouble},
  {"storeUnalignedDouble", "(JD)V", storeUnalignedDouble},
  {"loadVolatileByte", "(J)B", loadVolatileByte},
  {"storeVolatileByte", "(JB)V", storeVolatileByte},
  {"loadVolatileShort", "(J)S", loadVolatileShort},
  {"storeVolatileShort", "(JS)V", storeVolatileShort},
  {"loadVolatileInt", "(J)I", loadVolatileInt},
  {"storeVolatileInt", "(JI)V", storeVolatileInt},
  {"loadVolatileLong", "(J)J", loadVolatileLong},
  {"storeVolatileLong", "(JJ)V", storeVolatileLong},
  {"loadVolatileFloat", "(J)F", loadVolatileFloat},
  {"storeVolatileFloat", "(JF)V", storeVolatileFloat},
  {"loadVolatileDouble", "(J)D", loadVolatileDouble},
  {"storeVolatileDouble", "(JD)V", storeVolatileDouble},
  {"compareAndSwapInt", "(JII)Z", compareAndSwapInt},
  {"compareAndSwapLong", "(JJJ)Z", compareAndSwapLong},
  {"compareAndSwapFloat", "(JFF)Z", compareAndSwapFloat},
  {"compareAndSwapDouble", "(JDD)Z", compareAndSwapDouble},
  {"move", "(JJJ)V", move},
  {"clear", "(JJ)V", clear}
};

jint basis_platform_NativeData_onLoad(JNIEnv *env) {
  jclass NativeDataClass;
  if ((NativeDataClass = (*env)->FindClass(env, "basis/platform/NativeData")) == NULL) return JNI_ERR;
  
  if ((basis_platform_NativeData_base = (*env)->GetFieldID(env, NativeDataClass, "base", "J")) == NULL) return JNI_ERR;
  if ((basis_platform_NativeData_size = (*env)->GetFieldID(env, NativeDataClass, "size", "J")) == NULL) return JNI_ERR;
  if ((*env)->RegisterNatives(env, NativeDataClass, methods, methodCount) != JNI_OK) return JNI_ERR;
  
  (*env)->DeleteLocalRef(env, NativeDataClass);
  return JNI_OK;
}

static jint unit(JNIEnv *env, jobject self) {
  return (jint)sizeof(void *);
}

static jint checkAddress(JNIEnv *env, jobject self, jlong address) {
  jlong size = (*env)->GetLongField(env, self, basis_platform_NativeData_size);
  if (address < 0 || address >= size) {
    jclass IndexOutOfBoundsExceptionClass;
    if ((IndexOutOfBoundsExceptionClass = (*env)->FindClass(env, "java/lang/IndexOutOfBoundsException")) == NULL) return JNI_ERR;
    jint status = (*env)->ThrowNew(env, IndexOutOfBoundsExceptionClass, "address out of bounds");
    (*env)->DeleteLocalRef(env, IndexOutOfBoundsExceptionClass);
    return status;
  }
  return JNI_OK;
}

static jint checkAddressRange(JNIEnv *env, jobject self, jlong address, jlong length) {
  jlong size = (*env)->GetLongField(env, self, basis_platform_NativeData_size);
  if (address < 0 || address + length > size) {
    jclass IndexOutOfBoundsExceptionClass;
    if ((IndexOutOfBoundsExceptionClass = (*env)->FindClass(env, "java/lang/IndexOutOfBoundsException")) == NULL) return JNI_ERR;
    jint status = (*env)->ThrowNew(env, IndexOutOfBoundsExceptionClass, "address out of bounds");
    (*env)->DeleteLocalRef(env, IndexOutOfBoundsExceptionClass);
    return status;
  }
  return JNI_OK;
}

static jint checkAddressBounds(JNIEnv *env, jobject self, jlong lowerAddress, jlong upperAddress) {
  jlong size = (*env)->GetLongField(env, self, basis_platform_NativeData_size);
  if (lowerAddress > upperAddress || lowerAddress < 0 || lowerAddress >= size || upperAddress < 0 || upperAddress > size) {
    jclass IndexOutOfBoundsExceptionClass;
    if ((IndexOutOfBoundsExceptionClass = (*env)->FindClass(env, "java/lang/IndexOutOfBoundsException")) == NULL) return JNI_ERR;
    jint status = (*env)->ThrowNew(env, IndexOutOfBoundsExceptionClass, "address out of bounds");
    (*env)->DeleteLocalRef(env, IndexOutOfBoundsExceptionClass);
    return status;
  }
  return JNI_OK;
}

static jbyte JNICALL loadByte(JNIEnv *env, jobject self, jlong address) {
  if (checkAddress(env, self, address) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  return *(jbyte *)((char *)base + (size_t)address);
}

static void JNICALL storeByte(JNIEnv *env, jobject self, jlong address, jbyte value) {
  if (checkAddress(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  *(jbyte *)((char *)base + (size_t)address) = value;
}

static jshort JNICALL loadShort(JNIEnv *env, jobject self, jlong address) {
  if (checkAddress(env, self, address) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  return *(jshort *)((char *)base + (size_t)(address & -2));
}

static void JNICALL storeShort(JNIEnv *env, jobject self, jlong address, jshort value) {
  if (checkAddress(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  *(jshort *)((char *)base + (size_t)(address & -2)) = value;
}

static jint JNICALL loadInt(JNIEnv *env, jobject self, jlong address) {
  if (checkAddress(env, self, address) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  return *(jint *)((char *)base + (size_t)(address & -4));
}

static void JNICALL storeInt(JNIEnv *env, jobject self, jlong address, jint value) {
  if (checkAddress(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  *(jint *)((char *)base + (size_t)(address & -4)) = value;
}

static jlong JNICALL loadLong(JNIEnv *env, jobject self, jlong address) {
  if (checkAddress(env, self, address) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  return *(jlong *)((char *)base + (size_t)(address & -8));
}

static void JNICALL storeLong(JNIEnv *env, jobject self, jlong address, jlong value) {
  if (checkAddress(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  *(jlong *)((char *)base + (size_t)(address & -8)) = value;
}

static jfloat JNICALL loadFloat(JNIEnv *env, jobject self, jlong address) {
  if (checkAddress(env, self, address) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  return *(jfloat *)((char *)base + (size_t)(address & -4));
}

static void JNICALL storeFloat(JNIEnv *env, jobject self, jlong address, jfloat value) {
  if (checkAddress(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  *(jfloat *)((char *)base + (size_t)(address & -4)) = value;
}

static jdouble JNICALL loadDouble(JNIEnv *env, jobject self, jlong address) {
  if (checkAddress(env, self, address) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  return *(jdouble *)((char *)base + (size_t)(address & -8));
}

static void JNICALL storeDouble(JNIEnv *env, jobject self, jlong address, jdouble value) {
  if (checkAddress(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  *(jdouble *)((char *)base + (size_t)(address & -8)) = value;
}

static jshort JNICALL loadUnalignedShort(JNIEnv *env, jobject self, jlong address) {
  if (checkAddressRange(env, self, address, 2) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  return *(jshort *)((char *)base + (size_t)address);
}

static void JNICALL storeUnalignedShort(JNIEnv *env, jobject self, jlong address, jshort value) {
  if (checkAddressRange(env, self, address, 2) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  *(jshort *)((char *)base + (size_t)address) = value;
}

static jint JNICALL loadUnalignedInt(JNIEnv *env, jobject self, jlong address) {
  if (checkAddressRange(env, self, address, 4) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  return *(jint *)((char *)base + (size_t)address);
}

static void JNICALL storeUnalignedInt(JNIEnv *env, jobject self, jlong address, jint value) {
  if (checkAddressRange(env, self, address, 4) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  *(jint *)((char *)base + (size_t)address) = value;
}

static jlong JNICALL loadUnalignedLong(JNIEnv *env, jobject self, jlong address) {
  if (checkAddressRange(env, self, address, 8) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  return *(jlong *)((char *)base + (size_t)address);
}

static void JNICALL storeUnalignedLong(JNIEnv *env, jobject self, jlong address, jlong value) {
  if (checkAddressRange(env, self, address, 8) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  *(jlong *)((char *)base + (size_t)address) = value;
}

static jfloat JNICALL loadUnalignedFloat(JNIEnv *env, jobject self, jlong address) {
  if (checkAddressRange(env, self, address, 4) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  return *(jfloat *)((char *)base + (size_t)address);
}

static void JNICALL storeUnalignedFloat(JNIEnv *env, jobject self, jlong address, jfloat value) {
  if (checkAddressRange(env, self, address, 4) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  *(jfloat *)((char *)base + (size_t)address) = value;
}

static jdouble JNICALL loadUnalignedDouble(JNIEnv *env, jobject self, jlong address) {
  if (checkAddressRange(env, self, address, 8) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  return *(jdouble *)((char *)base + (size_t)address);
}

static void JNICALL storeUnalignedDouble(JNIEnv *env, jobject self, jlong address, jdouble value) {
  if (checkAddressRange(env, self, address, 8) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  *(jdouble *)((char *)base + (size_t)address) = value;
}

static jbyte JNICALL loadVolatileByte(JNIEnv *env, jobject self, jlong address) {
  if (checkAddress(env, self, address) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  __sync_synchronize();
  return *(volatile jbyte *)((char *)base + (size_t)address);
}

static void JNICALL storeVolatileByte(JNIEnv *env, jobject self, jlong address, jbyte value) {
  if (checkAddress(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  __sync_synchronize();
  *(volatile jbyte *)((char *)base + (size_t)address) = value;
}

static jshort JNICALL loadVolatileShort(JNIEnv *env, jobject self, jlong address) {
  if (checkAddress(env, self, address) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  __sync_synchronize();
  return *(volatile jshort *)((char *)base + (size_t)(address & -2));
}

static void JNICALL storeVolatileShort(JNIEnv *env, jobject self, jlong address, jshort value) {
  if (checkAddress(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  __sync_synchronize();
  *(volatile jshort *)((char *)base + (size_t)(address & -2)) = value;
}

static jint JNICALL loadVolatileInt(JNIEnv *env, jobject self, jlong address) {
  if (checkAddress(env, self, address) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  __sync_synchronize();
  return *(volatile jint *)((char *)base + (size_t)(address & -4));
}

static void JNICALL storeVolatileInt(JNIEnv *env, jobject self, jlong address, jint value) {
  if (checkAddress(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  __sync_synchronize();
  *(volatile jint *)((char *)base + (size_t)(address & -4)) = value;
}

static jlong JNICALL loadVolatileLong(JNIEnv *env, jobject self, jlong address) {
  if (checkAddress(env, self, address) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  __sync_synchronize();
  return *(volatile jlong *)((char *)base + (size_t)(address & -8));
}

static void JNICALL storeVolatileLong(JNIEnv *env, jobject self, jlong address, jlong value) {
  if (checkAddress(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  __sync_synchronize();
  *(volatile jlong *)((char *)base + (size_t)(address & -8)) = value;
}

static jfloat JNICALL loadVolatileFloat(JNIEnv *env, jobject self, jlong address) {
  if (checkAddress(env, self, address) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  __sync_synchronize();
  return *(volatile jfloat *)((char *)base + (size_t)(address & -4));
}

static void JNICALL storeVolatileFloat(JNIEnv *env, jobject self, jlong address, jfloat value) {
  if (checkAddress(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  __sync_synchronize();
  *(volatile jfloat *)((char *)base + (size_t)(address & -4)) = value;
}

static jdouble JNICALL loadVolatileDouble(JNIEnv *env, jobject self, jlong address) {
  if (checkAddress(env, self, address) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  __sync_synchronize();
  return *(volatile jdouble *)((char *)base + (size_t)(address & -8));
}

static void JNICALL storeVolatileDouble(JNIEnv *env, jobject self, jlong address, jdouble value) {
  if (checkAddress(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  __sync_synchronize();
  *(volatile jdouble *)((char *)base + (size_t)(address & -8)) = value;
}

static jboolean JNICALL compareAndSwapInt(JNIEnv *env, jobject self, jlong address, jint expected, jint value) {
  if (checkAddress(env, self, address) != JNI_OK) return JNI_FALSE;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  return __sync_bool_compare_and_swap((jint *)((char *)base + (size_t)(address & -4)), expected, value) ? JNI_TRUE : JNI_FALSE;
}

static jboolean JNICALL compareAndSwapLong(JNIEnv *env, jobject self, jlong address, jlong expected, jlong value) {
  if (checkAddress(env, self, address) != JNI_OK) return JNI_FALSE;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  return __sync_bool_compare_and_swap((jlong *)((char *)base + (size_t)(address & -8)), expected, value) ? JNI_TRUE : JNI_FALSE;
}

static jboolean JNICALL compareAndSwapFloat(JNIEnv *env, jobject self, jlong address, jfloat expected, jfloat value) {
  if (checkAddress(env, self, address) != JNI_OK) return JNI_FALSE;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  return __sync_bool_compare_and_swap((jint *)((char *)base + (size_t)(address & -4)), *(jint *)&expected, *(jint *)&value) ? JNI_TRUE : JNI_FALSE;
}

static jboolean JNICALL compareAndSwapDouble(JNIEnv *env, jobject self, jlong address, jdouble expected, jdouble value) {
  if (checkAddress(env, self, address) != JNI_OK) return JNI_FALSE;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  return __sync_bool_compare_and_swap((jlong *)((char *)base + (size_t)(address & -8)), *(jlong *)&expected, *(jlong *)&value) ? JNI_TRUE : JNI_FALSE;
}

static void JNICALL move(JNIEnv *env, jobject self, jlong fromAddress, jlong toAddress, jlong size) {
  if (checkAddressRange(env, self, fromAddress, size) != JNI_OK) return;
  if (checkAddressRange(env, self, toAddress, size) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  memmove((char *)base + (size_t)toAddress, (char *)base + (size_t)fromAddress, (size_t)size);
}

static void JNICALL clear(JNIEnv *env, jobject self, jlong fromAddress, jlong untilAddress) {
  if (checkAddressBounds(env, self, fromAddress, untilAddress) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, basis_platform_NativeData_base);
  memset((char *)base + (size_t)fromAddress, 0, (size_t)(untilAddress - fromAddress));
}
