EXE:= ChaosGame

install:
	ghc ChaosGame.hs
	./$(EXE)

clean:
	rm *.o *.hi $(EXE)
