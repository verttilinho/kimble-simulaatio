# Kimble-simulaatio (ENGLISH BELOW)
Fortanilla tehty Kimble -simulaatio. Simulaatiossa testataan erilaisten peritaktiikoiden toimivuutta. Tieteellinen laskenta II -kurssin loppuprojekti. Simulaation toimivuudesta ei ole takeita. Ainakin se pelailee pelejä, eli se saattaa toimia!


# Ajaminen

Ohjelma on valmiiksi koostettu (eng. compiled) kansioon run. Menemällä
kansioon run ja syöttämällä komennon ./a.out taktiikka 1.dat taktiikka 2.dat
taktiikka 3.dat taktiikka 4.dat asetukset.dat

Asetukset.dat sisältää kolme kokonaislukua. Ensimmäinen on simuloitavien
pelien lukumäärä. Toinen luku kertoo arvotaanko pelaajien järjestys ennen uu-
den pelin alkua (1 = kyllä). Viimeinen luku kertoo kuinka monesta pelityylistä
tehdään satunnainen jokaisella pelikeralla. 1 tarkoittaa, että ensimmäiseksi an-
nettu pelityyli pysyy samana ja muut arvotaan jokaisella pelikerralla uudestaan.
2 tarkoittaa, että kaikki pelityylit arvotaan jokaisella iteraatiokerralla.
Taktiikka i.dat (missä i = 1, 2, 3, 4) sisältää reaalilukujaväliltä [0, 100]. Lu-
vut kuvaavat datatyypin pelityyli alkiota. Ne luetaan järjestyksessä: persoona,
uusi1, uusi12, uusi3, aja, syo , maaliin, etene (ei kuus) , etene kuus, etene toisen
lahto, vaara, vaara (ei liiku), ja markov.

Ohjelma tuottaa neljä tiedostoa Pelitaktiikka_1_tulokset.txt, Pelitaktiikka_2_tulokset.txt,
Pelitaktiikka_3_tulokset.txt, Pelitaktiikka_4_tulokset.txt, mitkä sisältävät jokaisen
pelitaktiikan sijoitusjakaumafrekvenssit.

Tulokset voi visualisoida ohjelmalla tulosten visualisointi.py (ajo: python3
tulosten visualisointi.py)

# Koostaminen
./kooste.sh



# Simulaation idea

Simulaation ideana on testata erilaisten pelitaktiikoiden välisiä eroja, ja selvittää: Onko pelaajan valitsemalla taktiikalla merkitystä? Lisäksi voidaan testata esimerkiksi aloittajan vaikutusta sijoitukseen.

Simulaatio ottaa huomioon erilaisia mahdollisia pelitilanteita ja antaa niille painoarvon, jonka avulla valitaan paras mahdollinen peliliike. 

Pelitaktiikan tulokset tallennetaan, jotta niitä voidaan analysoida myöhemmin.

Simulaatiota on myös mahdollista ajaa täysin satunnaisilla pelitaktiikoilla


# Jatkokehitys

Kehitetään algoritmi, joka etsii optimaalisen pelistrategian. Lisäksi lisätään lisää tilanteita, joita simulaatio ottaa huomioon.


# ENGLISH (WORKING PROCESS)

Kimble simulation made with Fortran. This was a course project for Scientific Computing II course. There is no guarantees that the simulation works. At least it completes the games, so my hopes are still alive.

# Idea

Make a simulation which compares different game strategies in Kimble. Also it can compare how much advantage the starter has if any.

The simulation considers several different game situtations and looks at how much "player" values the different moves and decides the move based on the strategy. 

# What next?

Make an algortihm which finds optimal way of playing Kimble. Also add more game situations which the simulation considers.
