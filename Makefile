
output: src/Tokens.hs
	(cd src; ghc -o myinterpreter Main.hs;)

src/Tokens.hs: src/Tokens.x
	alex --outfile=src/Tokens.hs src/Tokens.x

test:
	./src/myinterpreter tests/problem1.spl < tests/input1.txt
	./src/myinterpreter tests/problem2.spl < tests/input2.txt
	./src/myinterpreter tests/problem3.spl < tests/input3.txt
	./src/myinterpreter tests/problem4.spl < tests/input4.txt
	./src/myinterpreter tests/problem5.spl < tests/input5.txt

clean:
	rm src/Tokens.hs
	rm src/*.o 
	rm src/*.hi
