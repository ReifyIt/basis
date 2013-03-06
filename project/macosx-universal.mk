CC = clang

CFLAGS += -O2 -Wall -arch x86_64 -arch i386 -I/System/Library/Frameworks/JavaVM.framework/Headers

LDFLAGS += -dynamiclib -framework JavaVM -arch x86_64 -arch i386

all: $(BASEDIR)/basis-memory/target/libbasis-memory.macosx-universal.jnilib

init:
	mkdir -p $(BASEDIR)/basis-memory/target/macosx-universal/
	mkdir -p $(BASEDIR)/basis-memory/target/macosx-universal/basis/memory

$(BASEDIR)/basis-memory/target/macosx-universal/%.o : $(BASEDIR)/basis-memory/src/main/native/%.c
	$(CC) $(CFLAGS) -I$(BASEDIR)/basis-memory/src/main/native -c $< -o $@

$(BASEDIR)/basis-memory/target/libbasis-memory.macosx-universal.jnilib: $(addprefix $(BASEDIR)/basis-memory/target/macosx-universal/, $(BASIS_MEMORY_OBJECTS))
	$(CC) $(LDFLAGS) -o $@ $^
