##
# termbox2 NIF
#
# @file
# @version 0.4
# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>
# Based on the makefile from https://github.com/garlic0x1/termbox2-nif

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

PROJECT := termbox2_nif

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)])." -s init stop)
ERL_INTERFACE_INCLUDE_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, include)])." -s init stop)
ERL_INTERFACE_LIB_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, lib)])." -s init stop)

ERL_INCLUDE ?= $(shell  erl -noshell -eval "io:format(\"~ts/~s\", [filename:dirname(code:lib_dir()), \"usr/include/\"])." -s init stop) 
ERL_LIB ?= $(shell  erl -noshell -eval "io:format(\"~ts\", [code:lib_dir()])." -s init stop) 


# System type and C compiler/flags.

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	CC ?= cc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes -undefined dynamic_lookup -dynamiclib -fvisibility=default
	CXXFLAGS ?= -O3 -finline-functions -Wall
	LDFLAGS ?= -undefined dynamic_lookup -dynamiclib
	SO_EXTENSION := so
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
else ifeq ($(OS),Windows_NT)
	CC ?= gcc
	CFLAGS ?= -O2 -std=c99 -Wall -Wextra -Werror -D_WIN32
	SO_EXTENSION := dll
	LDFLAGS ?= -shared
	LDLIBS += -lkernel32
endif

C_SRC_DIR = $(CURDIR)
C_SRC_OUTPUT ?= $(CURDIR)/../priv/$(PROJECT).$(SO_EXTENSION)

CFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR) -I $(ERL_INCLUDE) -I $(C_SRC_DIR)/termbox2
CXXFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR) -I $(C_SRC_DIR)/termbox2

LDLIBS += -L $(ERL_INTERFACE_LIB_DIR) -L $(ERL_LIB) -lei 


# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

cpp_verbose_0 = @echo " CPP   " $(?F);
cpp_verbose = $(cpp_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

NIF_SOURCES := $(wildcard $(C_SRC_DIR)/*.c) $(wildcard $(C_SRC_DIR)/*.C) $(wildcard $(C_SRC_DIR)/*.cc) $(wildcard $(C_SRC_DIR)/*.cpp)
TERMBOX_SOURCES :=
SOURCES := $(NIF_SOURCES) $(TERMBOX_SOURCES)

OBJECTS = $(addsuffix .o, $(basename $(notdir $(SOURCES))))
OBJ_DIR = $(CURDIR)/obj
OBJECT_FILES = $(addprefix $(OBJ_DIR)/, $(OBJECTS))

COMPILE_C = $(c_verbose) $(CC) $(CFLAGS) $(CPPFLAGS) -c
COMPILE_CPP = $(cpp_verbose) $(CXX) $(CXXFLAGS) $(CPPFLAGS) -c 


# --- Auto-fetch termbox2 source if missing ---
TERMBOX2_DIR := $(C_SRC_DIR)/termbox2
TERMBOX2_REPO := https://github.com/termbox/termbox2.git

.PHONY: termbox2_fetch
termbox2_fetch:
	@if [ ! -f "$(TERMBOX2_DIR)/termbox2.h" ]; then \
	  echo "[INFO] termbox2 source not found, cloning..."; \
	  git clone --depth 1 $(TERMBOX2_REPO) $(TERMBOX2_DIR); \
	else \
	  echo "[INFO] termbox2 source found."; \
	fi

# Ensure termbox2_fetch runs before building anything
$(C_SRC_OUTPUT): termbox2_fetch
$(OBJECT_FILES): termbox2_fetch

$(C_SRC_OUTPUT): $(OBJECT_FILES)
	@mkdir -p $(BASEDIR)/priv/
	$(link_verbose) $(CC) $(CFLAGS) $(OBJECT_FILES) $(LDFLAGS) $(LDLIBS) -o $(BASEDIR)/priv/$(PROJECT).so

$(OBJ_DIR)/%.o: $(C_SRC_DIR)/%.c
	@mkdir -p $(OBJ_DIR)
	$(COMPILE_C) $(OUTPUT_OPTION) $< -o $@

# No termbox2 .c files to compile, header-only

clean:
	@rm -f $(CURDIR)/../priv/$(PROJECT).so
	@rm -f $(OBJECT_FILES)
	@rm -rf $(OBJ_DIR)
