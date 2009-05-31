all: gattaca.hs
	ghc --make -O2 -fvia-C -optc-O2 -funbox-strict-fields -fwarn-missing-signatures -o gattaca gattaca.hs

profiled: gattaca.hs
	ghc --make -O2 -o gattaca gattaca.hs -prof -auto-all -caf-all -fforce-recomp

clean:
	@rm -f gattaca gattaca.o gattaca.hi gattaca.hp gattaca.prof
