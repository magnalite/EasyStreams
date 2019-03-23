
output:
	alex --outfile=src/Tokens.hs src/Tokens.x
	(cd src; ghc Main.hs;)
	./src/Main < ExampleProgram.ezs

clean:
	rm src/Tokens.hs
	rm src/*.o 
	rm src/*.hi
