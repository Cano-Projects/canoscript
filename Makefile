CC=gcc
CFLAGS=-Wall -Wextra -pedantic -ggdb2
DEFINES=
INCLUDES=
LIBS=

SRCDIR=src
BUILDDIR=build

ifeq ($(BUILD_TYPE), DEBUG)
CFLAGS += -g -ggdb2
endif

SRC=$(wildcard $(SRCDIR)/*.c)
OBJ=$(patsubst $(SRCDIR)/%.c, $(BUILDDIR)/%.o, $(SRC))

BINARYNAME=main
BINARY=$(BINARYNAME)

.PHONY: all clean destroy

all: $(BINARY)

$(BINARY): $(OBJ)
	$(CC) $(CFLAGS) $(INCLUDES) $(OBJ) -o $(BUILDDIR)/$(BINARY) $(LIBS)

$(BUILDDIR)/%.o: $(SRCDIR)/%.c
	@ mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(DEFINES) $(INCLUDES) -c $< -o $@

clean:
	rm -rf $(BINARY)
	rm -rf $(OBJ)

destroy:
	rm -rf $(BUILDDIR)
