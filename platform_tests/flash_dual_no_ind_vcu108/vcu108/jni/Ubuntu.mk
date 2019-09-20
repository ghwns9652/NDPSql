
NDPDIR?=/home/chamdoo/FlashDriver_vcu/lower/vcu108
CONNECTALDIR?=$(NDPDIR)/platform/build_tools/connectal
DTOP?=$(NDPDIR)/platform_tests/flash_dual_no_ind_vcu108/vcu108

TOOLCHAIN?=
ifneq ($(TOOLCHAIN),)
CC=$(TOOLCHAIN)gcc
CXX=$(TOOLCHAIN)g++
endif
CFLAGS_COMMON = -g -I$(DTOP)/jni -I$(CONNECTALDIR) -I$(CONNECTALDIR)/cpp -I$(CONNECTALDIR)/lib/cpp   -Wall -Werror -I$(DTOP)/jni -I$(CONNECTALDIR) -I$(CONNECTALDIR)/cpp -I$(CONNECTALDIR)/lib/cpp  
CFLAGS = $(CFLAGS_COMMON)
CFLAGS += $(COMMONFLAGS)
CFLAGS2 = 

include $(DTOP)/Makefile.autotop
include $(CONNECTALDIR)/scripts/Makefile.connectal.application
SOURCES = $(NDPDIR)/platform_tests/flash_dual_no_ind_vcu108/vcu108_inf.cpp $(NDPDIR)/platform/build_tools/connectal/cpp/dmaManager.c $(NDPDIR)/platform/build_tools/connectal/cpp/platformMemory.cpp $(PORTAL_SRC_FILES)
SOURCES2 =  $(PORTAL_SRC_FILES)
XSOURCES = $(CONNECTALDIR)/cpp/XsimTop.cpp $(PORTAL_SRC_FILES)
LDLIBS :=    -lpthread

TARGETOBJ:=$(patsubst %.cpp,%.o,$(SOURCES))
TARGETOBJ:=$(patsubst %.c,%.o,$(TARGETOBJ))
	#$(addprefix ../../object/,$(TARGETOBJ))\

ubuntu.exe: $(SOURCES)
	$(CXX) $(CFLAGS) -o ubuntu.exe $(SOURCES) $(LDLIBS)
	$(Q)[ ! -f ../bin/mkTop.bin.gz ] || $(TOOLCHAIN)objcopy --add-section fpgadata=../bin/mkTop.bin.gz ubuntu.exe

connectal.so: $(SOURCES)
	$(Q)$(CXX) -shared -fpic $(CFLAGS) -o connectal.so $(SOURCES) $(LDLIBS)

connectal.a: $(TARGETOBJ)
	$(Q)$(AR) r $(@) $(TARGETOBJ)

.cpp.o: $(SOURCES)
	$(CXX) $(CFLAGS) $(CFLAGS_LOWER) -o $@ -c $< $(LDLIBS)

ubuntu.exe2: $(SOURCES2)
	$(Q)$(CXX) $(CFLAGS) $(CFLAGS2) -o ubuntu.exe2 $(SOURCES2) $(LDLIBS)

xsim: $(XSOURCES)
	$(CXX) $(CFLAGS) -o xsim $(XSOURCES)
