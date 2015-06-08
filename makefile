###  -*- mode:makefile ; buffer-read-only:t -*-

### Makefile.
###
### Copyright (C) 2015 Alin C Soare
###
### This file is part of the Little Computer.
###
### This program is free software: you can redistribute it and/or modify
### it under the terms of the GNU General Public License as published by
### the Free Software Foundation, either version 3 of the License, or
### (at your option) any later version.
###
### This program is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.
###
### You should have received a copy of the GNU General Public License
### along with this program.  If not, see <http://www.gnu.org/licenses/>.


VERSION=0.2

RACKET=racket
ASMDIR=./asm/
BINDIR=./bin/
HWDIR=./hw/

tmp:=$(shell mktemp)

ASMOBJS=$(wildcard ${ASMDIR}/*.asm)
HACKOBJS = $(patsubst ${ASMDIR}/%.asm, %.hack, ${ASMOBJS})

ifneq ($(wildcard $(BINDIR)),${BINDIR})
$(info creating the directory ${BINDIR})
$(shell mkdir -p ${BINDIR})
endif

ifneq ($(wildcard $(HWDIR)),${HWDIR})
$(info creating the directory ${HWDIR})
$(shell mkdir -p ${HWDIR})
endif

all:
	@echo hwc hack sim

hwc:
	@${RACKET} hwc.rkt

sim:

ifndef HACK
	@echo To execute the hack program PROG do so:
	@echo "\tmake sim HACK=PROG"
	@echo PROG can be one of
	@echo ${patsubst %.hack, %, ${HACKOBJS}} \
	| sed 's. .\n.g' \
	| xargs -I@ sed '/DESCRIPTION:\(.*\)/ !d; s_[^:]*\(.*\)_\t'@'\1_; 3q' ./asm/@.asm \
	| column -s: -t
else
	${RACKET} sim.rkt ${BINDIR}/${HACK}.hack
endif

clean:
	@echo removing ./bin ./hw
	@rm -rf ./bin ./hw

gnu: clean
	tar c *.rkt makefile README asm/*.asm hdl-hack/*.HDL > $(tmp)
	bzip2 --best $(tmp)
	mv --backup=numbered $(tmp).bz2 little-computer.$(VERSION).tar.bz2

%.hack:
	@echo Compile $*
	@>/dev/null ${RACKET} assembler.rkt ${ASMDIR}/$*.asm ${BINDIR}/

hack: ${HACKOBJS}

rebuild: clean
	@make --silent hwc
	@make --silent hack
	@make --silent sim

