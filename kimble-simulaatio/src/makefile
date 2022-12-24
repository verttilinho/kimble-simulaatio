OBJS = parametrit.o noppa.o pelityyli.o vuorovaikutus_funktiot.o siirrot.o liike_testit.o pelaaja.o peli_toiminnot.o tietojen_tallennus.o peli.o

%.o: %.f90
	gfortran -c -O3 $< -o $@

kimblesim: $(OBJS)
	gfortran -o ../run/kimblesim $(OBJS)
