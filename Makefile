# directory paths, relative to this directory:
XPL       = ~/x/code
IMP       = ~/i
GEN	  = ./.gen
PPU	  = $(GEN)
EXE	  = $(GEN)
FCL = ~/ver/fpc/packages

# ROOT should be path back to this directory from GEN
ROOT      = ../

# compiler paths
FPC_PATH  = fpc
RTL       = -Fu~/f/rtl/units/x86_64-linux
FPC       = $(FPC_PATH) $(RTL) -gl -B -Fu$(GEN) -Fi$(GEN) \
            -Fu$(IMP) \
            -Fu$(XPL) -Fi$(XPL) -Fu./lib -FE$(GEN) -O-
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

min: *.pas
	$(FPC) -Mobjfpc min.pas
	mv $(GEN)/min .$

run: min
	./min hello.min

.tangled: pr.min.org st.min.org pk.min.org
	cat pr.min.org st.min.org pk.min.org > .tangled
	$(TANGLE) .tangled

clean:
	delp $(GEN)
	rm -f min .tangled

$(GEN)/run-tests.pas:
	cd $(GEN); ln -F -s $(XPL)/../test/run-tests.pas

test: always min $(GEN)/run-tests.pas
	@cd test; python $(XPL)/../test/gen-tests.py ../$(GEN)
	$(FPC) -Mobjfpc -vn -gl -B $(GEN)/run-tests.pas -Fu./test -otest-min
	$(GEN)/test-min

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

always: .PHONY
