EXE:= Run

install:
	ghc ChaosGame.hs -o $(EXE)
	./$(EXE)

clean:
	rm *.o *.hi $(EXE)
