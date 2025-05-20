all: jlc ../lib/runtime.bc

%: %.hs *.hs AbsJavalette.hs
	ghc -o $* --make $<

AbsJavalette.hs: Javalette.cf
	bnfc Javalette.cf
	happy -gca ParJavalette.y
	alex -g LexJavalette.x

../lib/runtime.bc:
	llvm-as ../lib/runtime.ll

.PHONY: package
package: all clean-nobnfc clean-unneeded
	cd ..; tar -czvf partX-n.tar.gz doc lib src jlc

# Removes all generated files except for BNFC generated
.PHONY: clean-nobnfc
clean-nobnfc:
	rm -f jlc *.{hi,o}
	rm -f TypeChecker/*.{hi,o}
	rm -f Compiler/*.{hi,o}
	rm -f ../lib/runtime.bc

# Removes all BNFC generated files
.PHONY: clean-bnfc
clean-bnfc: clean-unneeded
	rm -f AbsJavalette.* ErrM.* LexJavalette.* ParJavalette.* PrintJavalette.*

# Removes all unneeded for compilation BNFC generated files
.PHONY: clean-unneeded
clean-unneeded:
	rm -f DocJavalette.* SkelJavalette.* TestJavalette.* TestJavalette

.PHONY: clean
clean: clean-nobnfc clean-bnfc clean-unneeded
