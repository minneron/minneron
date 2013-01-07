# directory paths, relative to this directory:
XPL       = ./lib/xpl/code
GEN	  = ./.gen
PPU	  = $(GEN)
EXE	  = $(GEN)
FCL = ~/ver/fpc/packages

# ROOT should be path back to this directory from GEN
ROOT      = ../

# compiler paths
FPC       = fpc -gl -B -Fu$(GEN) -Fu$(XPL) -Fi$(XPL) -Fu./lib -FE$(GEN)
FCL-PAS = $(FCL)/fcl-passrc

#------------------------------------------------------

targets:
	@echo 'available targets:'
	@echo '--------------------------'
	@echo 'make test  -> run all tests'
	@echo 'make run   -> run minneron'
	@echo

run:
	$(FPC) -Mobjfpc mn.pas
	mv $(GEN)/mn .
	./mn mn.pas

test: lib/xpl
	cd ./lib/xpl; make test

build : init obtangle

init : lib/xpl
	@mkdir -p $(GEN)
	@rm -f $(GEN)/library $(GEN)/retroImage

# you don't want to run 'git submodule update' every time you run
# the tests, because git will shove any changes you've made off
# to a side branch.
lib/xpl:
	@git submodule init
	@git submodule update



obtangle:
	@$(FPC) obtangle.pas

test:

