export BASEDIR=$(CURDIR)

export BASIS_PLATFORM_OBJECTS = \
	basis/platform/package.o \
	basis/platform/NativeData.o

export BASIS_PLATFORM_POSIX_OBJECTS = \
	basis/platform/posix/package.o \
	basis/platform/posix/FileSystem_File.o \
	basis/platform/posix/FileSystem_File_.o \
	basis/platform/posix/FileSystem_FileData.o \
	basis/platform/posix/FileSystem_FileData_.o \
	basis/platform/posix/FileSystem_FileMode_.o \
	basis/platform/posix/FileSystem_MapMode_.o \
	basis/platform/posix/FileSystem_MapProt_.o \
	basis/platform/posix/FileSystem_OpenMode_.o \
	basis/platform/posix/FileSystem_Path.o

init:
	$(MAKE) -f project/macosx-universal.mk init

macosx-universal:
	$(MAKE) -f project/macosx-universal.mk
