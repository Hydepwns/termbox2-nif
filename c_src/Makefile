##
# termbox2 NIF
#
# @file
# @version 0.4
# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>
# Based on the makefile from https://github.com/garlic0x1/termbox2-nif

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

PROJECT ?= $(notdir $(BASEDIR))
PROJECT := $(strip $(PROJECT))

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)])." -s init stop)
ERL_INTERFACE_INCLUDE_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, include)])." -s init stop)
ERL_INTERFACE_LIB_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, lib)])." -s init stop)

ERL_INCLUDE ?= $(shell  erl -noshell -eval "io:format(\"~ts/~s\", [filename:dirname(code:lib_dir()), \"usr/include/\"])." -s init stop) 
ERL_LIB ?= $(shell  erl -noshell -eval "io:format(\"~ts\", [code:lib_dir()])." -s init stop) 


# System type and C compiler/flags.

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	CC ?= cc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes -undefined dynamic_lookup -dynamiclib
	CXXFLAGS ?= -O3 -finline-functions -Wall
	LDFLAGS ?= -undefined dynamic_lookup -dynamiclib
	SO_EXTENSION ?= so
else ifeq ($(UNAME_SYS), FreeBSD)
	CC ?= cc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -finline-functions -Wall
	SO_EXTENSION ?= so
else ifeq ($(UNAME_SYS), Linux)
	CC ?= gcc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -finline-functions -Wall
	SO_EXTENSION ?= so
endif

C_SRC_DIR = $(CURDIR)
C_SRC_OUTPUT ?= $(CURDIR)/../priv/$(PROJECT).$(SO_EXTENSION)

TERMBOX2_SRC_DIR = $(C_SRC_DIR)/termbox2/src

CFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR) -I $(ERL_INCLUDE) -I $(TERMBOX2_SRC_DIR)
CXXFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR) -I $(TERMBOX2_SRC_DIR)

LDLIBS += -L $(ERL_INTERFACE_LIB_DIR) -L $(ERL_LIB) -lei 


# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

cpp_verbose_0 = @echo " CPP   " $(?F);
cpp_verbose = $(cpp_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

NIF_SOURCES := $(wildcard $(C_SRC_DIR)/*.c) $(wildcard $(C_SRC_DIR)/*.C) $(wildcard $(C_SRC_DIR)/*.cc) $(wildcard $(C_SRC_DIR)/*.cpp)
TERMBOX_SOURCES := $(wildcard $(TERMBOX2_SRC_DIR)/*.c) $(wildcard $(TERMBOX2_SRC_DIR)/*.C) $(wildcard $(TERMBOX2_SRC_DIR)/*.cc) $(wildcard $(TERMBOX2_SRC_DIR)/*.cpp)
SOURCES := $(NIF_SOURCES) $(TERMBOX_SOURCES)

OBJECTS = $(addsuffix .o, $(basename $(notdir $(SOURCES))))
OBJ_DIR = $(CURDIR)/obj
OBJECT_FILES = $(addprefix $(OBJ_DIR)/, $(OBJECTS))

COMPILE_C = $(c_verbose) $(CC) $(CFLAGS) $(CPPFLAGS) -c
COMPILE_CPP = $(cpp_verbose) $(CXX) $(CXXFLAGS) $(CPPFLAGS) -c 


$(C_SRC_OUTPUT): $(OBJECT_FILES)
	@mkdir -p $(BASEDIR)/priv/
	$(link_verbose) $(CC) $(CFLAGS) $(OBJECT_FILES) $(LDFLAGS) $(LDLIBS) -o $(C_SRC_OUTPUT)

$(OBJ_DIR)/%.o: $(C_SRC_DIR)/%.c
	@mkdir -p $(OBJ_DIR)
	$(COMPILE_C) $(OUTPUT_OPTION) $< -o $@

$(OBJ_DIR)/%.o: $(TERMBOX2_SRC_DIR)/%.c
	@mkdir -p $(OBJ_DIR)
	$(COMPILE_C) $(OUTPUT_OPTION) $< -o $@

# Add rules for C++ if needed, copying the pattern above

clean:
	@rm -f $(C_SRC_OUTPUT)
	@rm -f $(OBJECT_FILES)
	@rm -rf $(OBJ_DIR)
