.PHONY:	all test

SRC := src
EXE := .exe
MAIN := main
MAINPY := main_py
DUNECLEAN := dune clean
DUNEBUILD := dune build
DUNEROOT := --root $(SRC)

all:
	$(DUNEBUILD) $(DUNEROOT)
	ln -fs $(SRC)/_build/default/tainter/$(MAIN)$(EXE) $(MAIN)$(EXE)
	ln -fs $(SRC)/_build/default/tainter/$(MAINPY)$(EXE) $(MAINPY)$(EXE)

main:
	$(DUNEBUILD) $(DUNEROOT) $(MAIN)

py:
	$(DUNEBUILD) $(DUNEROOT) $(MAINPY)$(EXE)

ln_main:
	ln -fs $(SRC)/_build/default/tainter/$(MAIN)$(EXE) $(MAIN)$(EXE)

ln_py:
	ln -fs $(SRC)/_build/default/tainter/$(MAINPY)$(EXE) $(MAINPY)$(EXE)

test:
	$(DUNEBUILD) $(DUNEROOT) --force --display short @test

clean:
	$(DUNECLEAN) $(DUNEROOT)
	rm -f $(MAIN)$(EXE)
	rm -f $(MAINPY)$(EXE)
