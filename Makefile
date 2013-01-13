# directory paths, relative to this directory:
XPL       = ~/x/code
GEN	  = ./.gen
PPU	  = $(GEN)
EXE	  = $(GEN)
FCL = ~/ver/fpc/packages

# ROOT should be path back to this directory from GEN
ROOT      = ../

# compiler paths
FPC       = fpc -gl -B -Fu$(GEN) -Fu$(XPL) -Fi$(XPL) -Fu./lib -FE$(GEN)
FCL-PAS = $(FCL)/fcl-passrc
TANGLE    = ./etc/tangle.el

#------------------------------------------------------

targets:
	@echo 'available targets:'
	@echo '--------------------------'
	@echo 'make test  -> run all tests'
	@echo 'make min   -> build ./min'
	@echo 'make run   -> build and run'
	@echo

min: .tangled
	$(FPC) -Mobjfpc $(GEN)/min.pas
	mv $(GEN)/min .$

run: min
	./min hello.min

.tangled: pr.min.org st.min.org pk.min.org
	cat pr.min.org st.min.org pk.min.org > .tangled
	$(TANGLE) .tangled

clean:
	delp $(GEN)
	rm -f min .tangled

test: #clean .tangled
	@echo 'no tests yet'


build : init

init : lib/xpl/Makefile
	@mkdir -p $(GEN)
	@rm -f $(GEN)/library $(GEN)/retroImage

# you don't want to run 'git submodule update' every time you run
# the tests, because git will shove any changes you've made off
# to a side branch.
lib/xpl/Makefile:
	@git submodule init
	@git submodule update


#obtangle:
#	@$(FPC) obtangle.pas

