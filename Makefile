
output:
	alex --outfile=src/Tokens.hs src/Tokens.x
	(cd src; ghc --make Main.hs;)
	./src/Main ExampleProgram.spl < ExampleInput.txt

clean:
	rm src/Tokens.hs
	rm src/*.o 
	rm src/*.hi
