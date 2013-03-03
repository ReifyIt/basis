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

static const int methodCount = 12;
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
  {"storeDouble", "(LD)V", storeDouble}
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

static jint checkBounds(JNIEnv *env, jobject self, jlong address) {
  jlong size = (*env)->GetLongField(env, self, sizeField);
  if (address < 0 || address >= size) {
    jclass IndexOutOfBoundsExceptionClass;
    if ((IndexOutOfBoundsExceptionClass = (*env)->FindClass(env, "java/lang/IndexOutOfBoundsException")) == NULL) return JNI_ERR;
    jint status = (*env)->ThrowNew(env, IndexOutOfBoundsExceptionClass, "invalid address");
    (*env)->DeleteLocalRef(env, IndexOutOfBoundsExceptionClass);
    return status;
  }
  return JNI_OK;
}

static jbyte JNICALL loadByte(JNIEnv *env, jobject self, jlong address) {
  if (checkBounds(env, self, address) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, baseField);
  return *(jbyte *)((char *)base + (size_t)address);
}

static void JNICALL storeByte(JNIEnv *env, jobject self, jlong address, jbyte value) {
  if (checkBounds(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, baseField);
  *(jbyte *)((char *)base + (size_t)address) = value;
}

static jshort JNICALL loadShort(JNIEnv *env, jobject self, jlong address) {
  if (checkBounds(env, self, address) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, baseField);
  return *(jshort *)((char *)base + (size_t)(address & -2));
}

static void JNICALL storeShort(JNIEnv *env, jobject self, jlong address, jshort value) {
  if (checkBounds(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, baseField);
  *(jshort *)((char *)base + (size_t)(address & -2)) = value;
}

static jint JNICALL loadInt(JNIEnv *env, jobject self, jlong address) {
  if (checkBounds(env, self, address) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, baseField);
  return *(jint *)((char *)base + (size_t)(address & -4));
}

static void JNICALL storeInt(JNIEnv *env, jobject self, jlong address, jint value) {
  if (checkBounds(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, baseField);
  *(jint *)((char *)base + (size_t)(address & -4)) = value;
}

static jlong JNICALL loadLong(JNIEnv *env, jobject self, jlong address) {
  if (checkBounds(env, self, address) != JNI_OK) return 0;
  jlong base = (*env)->GetLongField(env, self, baseField);
  return *(jlong *)((char *)base + (size_t)(address & -8));
}

static void JNICALL storeLong(JNIEnv *env, jobject self, jlong address, jlong value) {
  if (checkBounds(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, baseField);
  *(jlong *)((char *)base + (size_t)(address & -8)) = value;
}

static jfloat JNICALL loadFloat(JNIEnv *env, jobject self, jlong address) {
  if (checkBounds(env, self, address) != JNI_OK) return 0.0;
  jlong base = (*env)->GetLongField(env, self, baseField);
  return *(jfloat *)((char *)base + (size_t)(address & -4));
}

static void JNICALL storeFloat(JNIEnv *env, jobject self, jlong address, jfloat value) {
  if (checkBounds(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, baseField);
  *(jfloat *)((char *)base + (size_t)(address & -4)) = value;
}

static jdouble JNICALL loadDouble(JNIEnv *env, jobject self, jlong address) {
  if (checkBounds(env, self, address) != JNI_OK) return 0.0;
  jlong base = (*env)->GetLongField(env, self, baseField);
  return *(jdouble *)((char *)base + (size_t)(address & -8));
}

static void JNICALL storeDouble(JNIEnv *env, jobject self, jlong address, jdouble value) {
  if (checkBounds(env, self, address) != JNI_OK) return;
  jlong base = (*env)->GetLongField(env, self, baseField);
  *(jdouble *)((char *)base + (size_t)(address & -8)) = value;
}
