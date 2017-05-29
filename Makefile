build:
	ghc --make catan.hs && ./catan

clean:
	rm *.hi *.o catan 
