# directory paths, relative to this directory:
XPL       = ./lib/xpl/code
GEN	  = ./.gen
PPU	  = $(GEN)
EXE	  = $(GEN)
FCL = ~/ver/fpc/packages

# ROOT should be path back to this directory from GEN
ROOT      = ../

# compiler paths
FPC       = fpc -gl -B -Fu$(XPL) -Fi$(XPL) -FE$(GEN)
FCL-PAS = $(FCL)/fcl-passrc

#------------------------------------------------------

targets:
	@echo 'available targets:'
	@echo '--------------------------'
	@echo 'make test  -> run all tests'
	@echo 'make run   -> run minneron'
	@echo

run:
	$(FPC) -Mobjfpc -Fu./.gen mn.pas  && ./mn mn.pas

build : init obtangle

init :
	@mkdir -p $(GEN)
	@rm -f $(GEN)/library $(GEN)/retroImage
	@git submodule init
	@git submodule update
	@ln -s $(RETROPATH)/library $(GEN)/library
	@ln -n $(RETROPATH)/retroImage $(GEN)/retroImage


obtangle:
	@$(FPC) obtangle.pas

test:

