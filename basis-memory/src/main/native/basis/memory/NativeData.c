/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

#include "basis/memory/package.h"

static jfieldID baseField;
static jfieldID sizeField;
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

static const int methodCount = 34;
static const JNINativeMethod methods[methodCount] = {
  {"loadByte", "(L)B", loadByte},
  {"storeByte", "(LB)V", storeByte},
  {"loadShort", "(L)S", loadShort},
  {"storeShort", "(LS)V", storeShort},
  {"loadInt", "(L)I", loadInt},
  {"storeInt", "(LI)V", storeInt},
  {"loadLong", "(L)L", loadLong},
  {"storeLong", "(LL)V", storeLong},
  {"loadFloat", "(L)F", loadFloat},
  {"storeFloat", "(LF)V", storeFloat},
  {"loadDouble", "(L)D", loadDouble},
  {"storeDouble", "(LD)V", storeDouble},
  {"loadUnalignedShort", "(L)S", loadUnalignedShort},
  {"storeUnalignedShort", "(LS)V", storeUnalignedShort},
  {"loadUnalignedInt", "(L)I", loadUnalignedInt},
  {"storeUnalignedInt", "(LI)V", storeUnalignedInt},
  {"loadUnalignedLong", "(L)L", loadUnalignedLong},
  {"storeUnalignedLong", "(LL)V", storeUnalignedLong},
  {"loadUnalignedFloat", "(L)F", loadUnalignedFloat},
  {"storeUnalignedFloat", "(LF)V", storeUnalignedFloat},
  {"loadUnalignedDouble", "(L)D", loadUnalignedDouble},
  {"storeUnalignedDouble", "(LD)V", storeUnalignedDouble},
  {"loadVolatileByte", "(L)B", loadVolatileByte},
  {"storeVolatileByte", "(LB)V", storeVolatileByte},
  {"loadVolatileShort", "(L)S", loadVolatileShort},
  {"storeVolatileShort", "(LS)V", storeVolatileShort},
  {"loadVolatileInt", "(L)I", loadVolatileInt},
  {"storeVolatileInt", "(LI)V", storeVolatileInt},
  {"loadVolatileLong", "(L)L", loadVolatileLong},
  {"storeVolatileLong", "(LL)V", storeVolatileLong},
  {"loadVolatileFloat", "(L)F", loadVolatileFloat},
  {"storeVolatileFloat", "(LF)V", storeVolatileFloat},
  {"loadVolatileDouble", "(L)D", loadVolatileDouble},
  {"storeVolatileDouble", "(LD)V", storeVolatileDouble}
};

jint basis_memory_NativeData_onLoad(JNIEnv *env) {
  jclass NativeDataClass;
  if ((NativeDataClass = (*env)->FindClass(env, "basis/memory/NativeData")) == NULL) return JNI_ERR;
  
  if ((baseField = (*env)->GetFieldID(env, NativeDataClass, "base", "J")) == NULL) return JNI_ERR;
  if ((sizeField = (*env)->GetFieldID(env, NativeDataClass, "size", "J")) == NULL) return JNI_ERR;
  if ((*env)->RegisterNatives(env, NativeDataClass, methods, methodCount) != JNI_OK) return JNI_ERR;
  
  (*env)->DeleteLocalRef(env, NativeDataClass);
  return JNI_OK;
}

static jint checkAddress(JNIEnv *env, jobject self, jlong address) {
  jlong size = (*env)->GetLongField(env, self, sizeField);
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
  jlong size = (*env)->GetLongField(env, self, sizeField);
  if (address < 0 || address + length > size) {
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
  jlong base = (*env)->GetLongField(env, self, baseField);
  return *(jbyte *)((char *)base + (size_t)address);
}

static void JNICALL storeByte(JNIEnv *env, jobject self, jlong address, jbyte value) {
  if (checkAddress(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, baseField);
  *(jbyte *)((char *)base + (size_t)address) = value;
}

static jshort JNICALL loadShort(JNIEnv *env, jobject self, jlong address) {
  if (checkAddress(env, self, address) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, baseField);
  return *(jshort *)((char *)base + (size_t)(address & -2));
}

static void JNICALL storeShort(JNIEnv *env, jobject self, jlong address, jshort value) {
  if (checkAddress(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, baseField);
  *(jshort *)((char *)base + (size_t)(address & -2)) = value;
}

static jint JNICALL loadInt(JNIEnv *env, jobject self, jlong address) {
  if (checkAddress(env, self, address) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, baseField);
  return *(jint *)((char *)base + (size_t)(address & -4));
}

static void JNICALL storeInt(JNIEnv *env, jobject self, jlong address, jint value) {
  if (checkAddress(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, baseField);
  *(jint *)((char *)base + (size_t)(address & -4)) = value;
}

static jlong JNICALL loadLong(JNIEnv *env, jobject self, jlong address) {
  if (checkAddress(env, self, address) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, baseField);
  return *(jlong *)((char *)base + (size_t)(address & -8));
}

static void JNICALL storeLong(JNIEnv *env, jobject self, jlong address, jlong value) {
  if (checkAddress(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, baseField);
  *(jlong *)((char *)base + (size_t)(address & -8)) = value;
}

static jfloat JNICALL loadFloat(JNIEnv *env, jobject self, jlong address) {
  if (checkAddress(env, self, address) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, baseField);
  return *(jfloat *)((char *)base + (size_t)(address & -4));
}

static void JNICALL storeFloat(JNIEnv *env, jobject self, jlong address, jfloat value) {
  if (checkAddress(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, baseField);
  *(jfloat *)((char *)base + (size_t)(address & -4)) = value;
}

static jdouble JNICALL loadDouble(JNIEnv *env, jobject self, jlong address) {
  if (checkAddress(env, self, address) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, baseField);
  return *(jdouble *)((char *)base + (size_t)(address & -8));
}

static void JNICALL storeDouble(JNIEnv *env, jobject self, jlong address, jdouble value) {
  if (checkAddress(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, baseField);
  *(jdouble *)((char *)base + (size_t)(address & -8)) = value;
}

static jshort JNICALL loadUnalignedShort(JNIEnv *env, jobject self, jlong address) {
  if (checkAddressRange(env, self, address, 2) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, baseField);
  return *(jshort *)((char *)base + (size_t)address);
}

static void JNICALL storeUnalignedShort(JNIEnv *env, jobject self, jlong address, jshort value) {
  if (checkAddressRange(env, self, address, 2) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, baseField);
  *(jshort *)((char *)base + (size_t)address) = value;
}

static jint JNICALL loadUnalignedInt(JNIEnv *env, jobject self, jlong address) {
  if (checkAddressRange(env, self, address, 4) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, baseField);
  return *(jint *)((char *)base + (size_t)address);
}

static void JNICALL storeUnalignedInt(JNIEnv *env, jobject self, jlong address, jint value) {
  if (checkAddressRange(env, self, address, 4) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, baseField);
  *(jint *)((char *)base + (size_t)address) = value;
}

static jlong JNICALL loadUnalignedLong(JNIEnv *env, jobject self, jlong address) {
  if (checkAddressRange(env, self, address, 8) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, baseField);
  return *(jlong *)((char *)base + (size_t)address);
}

static void JNICALL storeUnalignedLong(JNIEnv *env, jobject self, jlong address, jlong value) {
  if (checkAddressRange(env, self, address, 8) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, baseField);
  *(jlong *)((char *)base + (size_t)address) = value;
}

static jfloat JNICALL loadUnalignedFloat(JNIEnv *env, jobject self, jlong address) {
  if (checkAddressRange(env, self, address, 4) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, baseField);
  return *(jfloat *)((char *)base + (size_t)address);
}

static void JNICALL storeUnalignedFloat(JNIEnv *env, jobject self, jlong address, jfloat value) {
  if (checkAddressRange(env, self, address, 4) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, baseField);
  *(jfloat *)((char *)base + (size_t)address) = value;
}

static jdouble JNICALL loadUnalignedDouble(JNIEnv *env, jobject self, jlong address) {
  if (checkAddressRange(env, self, address, 8) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, baseField);
  return *(jdouble *)((char *)base + (size_t)address);
}

static void JNICALL storeUnalignedDouble(JNIEnv *env, jobject self, jlong address, jdouble value) {
  if (checkAddressRange(env, self, address, 8) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, baseField);
  *(jdouble *)((char *)base + (size_t)address) = value;
}

static jbyte JNICALL loadVolatileByte(JNIEnv *env, jobject self, jlong address) {
  if (checkAddress(env, self, address) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, baseField);
  __sync_synchronize();
  return *(volatile jbyte *)((char *)base + (size_t)address);
}

static void JNICALL storeVolatileByte(JNIEnv *env, jobject self, jlong address, jbyte value) {
  if (checkAddress(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, baseField);
  __sync_synchronize();
  *(volatile jbyte *)((char *)base + (size_t)address) = value;
}

static jshort JNICALL loadVolatileShort(JNIEnv *env, jobject self, jlong address) {
  if (checkAddress(env, self, address) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, baseField);
  __sync_synchronize();
  return *(volatile jshort *)((char *)base + (size_t)(address & -2));
}

static void JNICALL storeVolatileShort(JNIEnv *env, jobject self, jlong address, jshort value) {
  if (checkAddress(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, baseField);
  __sync_synchronize();
  *(volatile jshort *)((char *)base + (size_t)(address & -2)) = value;
}

static jint JNICALL loadVolatileInt(JNIEnv *env, jobject self, jlong address) {
  if (checkAddress(env, self, address) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, baseField);
  __sync_synchronize();
  return *(volatile jint *)((char *)base + (size_t)(address & -4));
}

static void JNICALL storeVolatileInt(JNIEnv *env, jobject self, jlong address, jint value) {
  if (checkAddress(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, baseField);
  __sync_synchronize();
  *(volatile jint *)((char *)base + (size_t)(address & -4)) = value;
}

static jlong JNICALL loadVolatileLong(JNIEnv *env, jobject self, jlong address) {
  if (checkAddress(env, self, address) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, baseField);
  __sync_synchronize();
  return *(volatile jlong *)((char *)base + (size_t)(address & -8));
}

static void JNICALL storeVolatileLong(JNIEnv *env, jobject self, jlong address, jlong value) {
  if (checkAddress(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, baseField);
  __sync_synchronize();
  *(volatile jlong *)((char *)base + (size_t)(address & -8)) = value;
}

static jfloat JNICALL loadVolatileFloat(JNIEnv *env, jobject self, jlong address) {
  if (checkAddress(env, self, address) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, baseField);
  __sync_synchronize();
  return *(volatile jfloat *)((char *)base + (size_t)(address & -4));
}

static void JNICALL storeVolatileFloat(JNIEnv *env, jobject self, jlong address, jfloat value) {
  if (checkAddress(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, baseField);
  __sync_synchronize();
  *(volatile jfloat *)((char *)base + (size_t)(address & -4)) = value;
}

static jdouble JNICALL loadVolatileDouble(JNIEnv *env, jobject self, jlong address) {
  if (checkAddress(env, self, address) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, baseField);
  __sync_synchronize();
  return *(volatile jdouble *)((char *)base + (size_t)(address & -8));
}

static void JNICALL storeVolatileDouble(JNIEnv *env, jobject self, jlong address, jdouble value) {
  if (checkAddress(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, baseField);
  __sync_synchronize();
  *(volatile jdouble *)((char *)base + (size_t)(address & -8)) = value;
}
