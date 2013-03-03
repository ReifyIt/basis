export BASEDIR=$(CURDIR)

export BASIS_MEMORY_OBJECTS = \
  basis/memory/JNILibrary.o \
  basis/memory/package.o \
  basis/memory/NativeData.o

init:
  $(MAKE) -f project/macosx-universal.mk init

macosx-universal:
  $(MAKE) -f project/macosx-universal.mk
