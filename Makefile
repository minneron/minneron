#-- minneron makefile ---------------------------------

targets:
	@echo 'available targets:'
	@echo '--------------------------'
	@echo 'make test  -> run all tests'
	@echo 'make min   -> build ./min'
	@echo 'make run   -> build and run'
	@echo

#------------------------------------------------------

# directory paths (relative to this directory):
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
            -Fu$(IMP) -dDEBUG \
            -Fu$(XPL) -Fi$(XPL) -Fu./lib -FE$(GEN)
FCL-PAS = $(FCL)/fcl-passrc
TANGLE    = ./etc/tangle.el

# ucomment this to enable tracing on the tests
# TRACE     = --trace


#-- core rules ---

clean:
	delp $(GEN)
	rm -f min .tangled

min: *.pas
	$(FPC) min.pas
	mv $(GEN)/min .$

run: min
	./min hello.min

tok: tok.pas minsql
	$(FPC) tok.pas
	$(GEN)/tok

dboutln: minsql
	$(FPC) dboutln.pas
	$(GEN)/dboutln

build : init

test: always min minsql $(GEN)/run-tests.pas
	@cd test; python $(XPL)/../test/gen-tests.py ../$(GEN)
	$(FPC) -vn -B $(GEN)/run-tests.pas -Fu./test -otest-min
	$(GEN)/test-min $(TRACE)

#-- helper rules --

init : lib/xpl/Makefile
	@mkdir -p $(GEN)
	@rm -f $(GEN)/library $(GEN)/retroImage

.tangled: pr.min.org st.min.org pk.min.org
	cat pr.min.org st.min.org pk.min.org > .tangled
	$(TANGLE) .tangled

minsql: $(GEN)/min-sql2pas.inc
$(GEN)/min-sql2pas.inc: sql/min.sql sql2pas.py
	python3 sql2pas.py sql/min.sql > $(GEN)/min-sql2pas.inc


$(GEN)/run-tests.pas:
	cd $(GEN); ln -F -s $(XPL)/../test/run-tests.pas


# you don't want to run 'git submodule update' every time you run
# the tests, because git will shove any changes you've made off
# to a side branch.
lib/xpl/Makefile:
	@git submodule init
	@git submodule update

always: .PHONY
