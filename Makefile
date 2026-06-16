
all: 
	cabal build

run_test:
	./dist-newstyle/build/x86_64-linux/ghc-9.6.7/Interpreter-book-take3-0.1.0.0/x/Interpreter-book-take3/build/Interpreter-book-take3/Interpreter-book-take3 test.lox
