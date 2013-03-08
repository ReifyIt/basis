CC = clang

CFLAGS += -O2 -Wall -arch x86_64 -arch i386 -I/System/Library/Frameworks/JavaVM.framework/Headers

LDFLAGS += -dynamiclib -framework JavaVM -arch x86_64 -arch i386

BASIS_PLATFORM_MACOSX_UNIVERSAL_OBJECTS = \
	basis/platform/JNILibrary.posix.o \
	$(BASIS_PLATFORM_OBJECTS) \
	$(BASIS_PLATFORM_POSIX_OBJECTS)

all: $(BASEDIR)/basis-platform/target/libbasis-platform.macosx-universal.jnilib

init:
	mkdir -p $(BASEDIR)/basis-platform/target/macosx-universal/
	mkdir -p $(BASEDIR)/basis-platform/target/macosx-universal/basis/platform
	mkdir -p $(BASEDIR)/basis-platform/target/macosx-universal/basis/platform/posix

$(BASEDIR)/basis-platform/target/macosx-universal/%.o : $(BASEDIR)/basis-platform/src/main/native/%.c
	$(CC) $(CFLAGS) -I$(BASEDIR)/basis-platform/src/main/native -c $< -o $@

$(BASEDIR)/basis-platform/target/libbasis-platform.macosx-universal.jnilib: $(addprefix $(BASEDIR)/basis-platform/target/macosx-universal/, $(BASIS_PLATFORM_MACOSX_UNIVERSAL_OBJECTS))
	$(CC) $(LDFLAGS) -o $@ $^
