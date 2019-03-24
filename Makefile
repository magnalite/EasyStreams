
output: src/Tokens.hs
	(cd src; ghc Main.hs;)
	./src/Main problem1.spl < ExampleInput.txt

src/Tokens.hs: src/Tokens.x
	alex --outfile=src/Tokens.hs src/Tokens.x

clean:
	rm src/Tokens.hs
	rm src/*.o 
	rm src/*.hi
