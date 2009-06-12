all: sophie.hs
	ghc --make -O2 -funbox-strict-fields -fwarn-missing-signatures -o sophie sophie.hs

via-c: sophie.hs
	ghc --make -O2 -fvia-C -optc-O2 -funbox-strict-fields -fwarn-missing-signatures -o sophie sophie.hs

profiled: sophie.hs
	ghc --make -O2 -o sophie sophie.hs -prof -auto-all -caf-all -fforce-recomp

clean:
	@rm -f sophie sophie.o sophie.hi sophie.hp sophie.prof
