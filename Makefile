all: src/main.hs
	hastec -o dist/main.js src/main.hs
	hastec -o dist/water_sim/main.js src/water_sim/Main.hs

clean:
	rm -r Main.jsmod src/main src/main.o src/main.hi dist/main.js 
