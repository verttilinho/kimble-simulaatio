#! /bin/bash
rm *.o
rm *.mod
gfortran -c parametrit.f90
gfortran -c -O3 noppa.f90
gfortran -c -O3 pelityyli.f90
gfortran -c -O3 vuorovaikutus_funktiot.f90
gfortran -c -O3 siirrot.f90
gfortran -c -O3 liike_testit.f90

gfortran -c -O3 pelaaja.f90
gfortran -c -O3 peli_toiminnot.f90
gfortran -c -O3 tietojen_tallennus.f90
#gfortran -c testi_liike_testit.f90
gfortran -c -O3 peli.f90
#gfortran pelaaja_testit.f90
#gfortran testi_liike_testit.f90 tietojen_tallennus.f90 peli_toiminnot.f90 pelaaja.f90 liike_testit.f90 siirrot.f90 vuorovaikutus_funktiot.f90 pelityyli.f90 noppa.f90 parametrit.f90
gfortran -o ../run/a.out peli.f90 tietojen_tallennus.f90 peli_toiminnot.f90 pelaaja.f90 siirrot.f90 liike_testit.f90 vuorovaikutus_funktiot.f90 pelityyli.f90 noppa.f90 parametrit.f90