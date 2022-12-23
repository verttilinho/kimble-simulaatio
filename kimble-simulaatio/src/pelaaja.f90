module pelaajat
use pelityylit
use liike_testit
use siirrot
implicit none

contains

 ! Käydään läpi pelaajan nappulat ja katsotaan pääseeksse maaliinn
! Tee pisteytys logiikka ! Käydään läpi pelaajan n ! Käydään läpi pelaajan nappulat ja katsotaan pääseeksse maaliinnppulat ja katsotaan pääseeksse maaliinn pelityylille

! Anna eri piste arvo 


! Pelaaja muuttaa laudan tilannetta (eli pelaaja ottaa sisäänsä laudan tämänhetkisen tilanteen)
! Pelaaja ottaa sisäänsä heitetyn silmäluvun, ja käy läpi kaikki mahdolliset peliliikkeet
! Tämän jälkeen pelaaja päättä ! Käydään läpi pelaajan nappulat ja katsotaan pääseeksse maaliinnä pelitaktiikkansa perusteella, mitä pelaaja tekee
! Koska pelaajien alkujärjestys on satunnainen tarvitaan indeksi "kuka", jotta pelaaja tietää omat nappulansa 

subroutine mahdolliset_liikkeet(liikkeet, silmaluku, lauta, kuka, nappula)
    implicit none
    integer :: i, j, silmaluku, kuka, kentalla, nappula
    integer, dimension(4,4) :: lauta
    
    ! Pelityylissä on kaikki mahdolliset liikkeet
    ! Nyt lisätään jokaisen liikkeen kohdalle 1 tai 0 riippuen
    ! voiko nappula tehdä kyseistä liikettä
    type(pelityyli) :: liikkeet

    ! Katsotaan syökö nappula omaa nappulaa
    ! Jos ei niin tehdään testit

    if (oma_syo(silmaluku, lauta, kuka, nappula) == 0) then
    ! Aletaan käymään mahdollisia liikkeitä läpi
               
        call uusi(silmaluku, lauta, liikkeet, kuka, nappula)

        call maaliin(silmaluku, lauta, liikkeet, kuka, nappula)

        call syo(silmaluku, lauta, liikkeet, kuka, nappula)

        call etene_ei_kuus(silmaluku, lauta, liikkeet, kuka, nappula)

        call etene_kuus(silmaluku, lauta, liikkeet, kuka, nappula)

        call etene_toisen_lahto(silmaluku, lauta, liikkeet,kuka, nappula)

        !call aja(silmaluku, lauta, liikkeet, kuka, nappula)

        call vaara(silmaluku, lauta, liikkeet, kuka, nappula)

        call vaara_ei_liiku(lauta, liikkeet, kuka, nappula)

        call markov_nautinto(silmaluku, lauta, liikkeet, kuka, nappula)
    end if
    
end subroutine




! Funktio, joka katsoo mikä siirto on paras
function valitse_siirto(pisteet, pelitaktiikka, voitko_pelata, lauta, kuka, silmaluku) result(valittu_nappula)
    implicit none
    real(rk) :: voitko_pelata
    type(pisteytys), dimension(4) :: pisteet
    type(pelityyli) :: pelitaktiikka
    integer :: nappula, valittu_nappula, kuka, silmaluku
    integer, dimension(4,4) :: lauta
    ! Jos valittu nappula pysyy nollassa, ei voida siirtää
    valittu_nappula = 0
    ! Mahdolliset valinnat
    
    if (voitko_pelata /= 0) then

        do nappula = 1, 4
            if (silmaluku /= 6 .and. lauta(kuka, nappula) == 0) then
                    cycle
            else
                if (lauta(kuka, nappula) /= -1) then
                    valittu_nappula = nappula
                    exit
                end if
            end if
        end do
        
        do nappula = 1, 4
            
            ! Jos oot maalissa sua ei voi valita
            if (lauta(kuka, nappula) /= -1) then
                ! Jos et heittänyt kutosta, himassa olevilla ei väliä
                if (silmaluku /= 6 .and. lauta(kuka, nappula) == 0) then
                    cycle
                else
                    ! Objektiivisesti paras
                    if  (pisteet(valittu_nappula)%hyoty < pisteet(nappula)%hyoty .and. &
                        pisteet(valittu_nappula)%riski >= pisteet(nappula)%riski) then
                        
                        valittu_nappula = nappula

                    else if  (pisteet(valittu_nappula)%hyoty <= pisteet(nappula)%hyoty .and. &
                        pisteet(valittu_nappula)%riski > pisteet(nappula)%riski) then
                        
                    
                    
                    ! Persoonallisuudesta riippuva tulos
                    else if (pisteet(valittu_nappula)%hyoty < pisteet(nappula)%hyoty .and. &
                            pisteet(valittu_nappula)%riski < pisteet(nappula)%riski) then
                        if (pisteet(valittu_nappula)%hyoty == 0) then
                            valittu_nappula = nappula

                        ! Jos on rohkelikko otetaan iso hyöty ja iso riski
                        else if (pelitaktiikka%persoona == 1) then 
                            valittu_nappula = nappula
                        end if
                    
                    !Jos saman_arvoiset (tai oon unohtanut jonkun tapauksen)
                    else if(pisteet(valittu_nappula)%hyoty == pisteet(nappula)%hyoty .and. &
                    (pisteet(valittu_nappula)%riski == pisteet(nappula)%riski)) then
                    
                        ! TÄHÄN PITÄÄ TEHDÄ YMPÄRI MENO tee tästä lähempänä maalia
                        
                        if (etaisyys_koti(lauta, kuka, valittu_nappula) > etaisyys_koti(lauta, kuka, nappula)) then

                            valittu_nappula  = nappula
                        end if

                    

                    ! Jos nappula kentällä
                    else if (lauta(kuka, nappula) /= 0) then
                        valittu_nappula = nappula

                    end if
                end if
            end if
        end do
    end if
end function


 subroutine pelaaja(silmaluku, lauta, pelitaktiikka, kuka)
    implicit none
    real(rk) :: voitko_pelata
    integer, dimension(4,4) :: lauta
    integer :: nappula, kuka, silmaluku, valittu_nappula
    real(rk), dimension(2) :: hyoty_lista
    type(pelityyli) :: pelitaktiikka
    type(pelityyli), dimension(4) :: liikkeet
    type(pisteytys), dimension(4) :: pisteet

    ! Alustetaan (ei olla katsottu lautaa niin ei voi tietää mitä tehdään) 
    liikkeet = liikkeet_alustus()

    ! Oletetaan että nappulaa ei voi liikuttaa
    voitko_pelata = 0
    
    ! Katsotaan mitä pelaajan jokainen nappula voi tehdä
    do nappula = 1, 4
        ! Jos nappula on maalissa ei sille enää tehdä mitään
        if (lauta(kuka, nappula) /= -1) then
            call mahdolliset_liikkeet(liikkeet(nappula), silmaluku, lauta, kuka, nappula)
            
            ! Ehdot voitko siirtää
            if (lauta(kuka, nappula) /= 0 .or. (silmaluku == 6 .and. lauta(kuka, nappula) /= -1)) then
                     voitko_pelata = 1   
            end if

        end if
    end do

    ! Tee lista jossa vainl mahdolliset liikkeet
    ! 0 jos ei voida tehdä, pelitaktiikan mukainen piste,
    ! jos voidaan
    ! Tän ois voinu varmaan kirjoittaa lyhyemmin, en osannut


    do nappula = 1, 4
        liikkeet(nappula)%uusi_0                = pelitaktiikka%uusi_0 * liikkeet(nappula)%uusi_0
        liikkeet(nappula)%uusi_1                = pelitaktiikka%uusi_1 * liikkeet(nappula)%uusi_1
        liikkeet(nappula)%uusi_2                = pelitaktiikka%uusi_2 * liikkeet(nappula)%uusi_2
        liikkeet(nappula)%uusi_3                = pelitaktiikka%uusi_3 * liikkeet(nappula)%uusi_3
        liikkeet(nappula)%syo                   = pelitaktiikka%syo * liikkeet(nappula)%syo
        liikkeet(nappula)%maaliin               = pelitaktiikka%maaliin * liikkeet(nappula)%maaliin
        liikkeet(nappula)%aja                   = pelitaktiikka%aja * liikkeet(nappula)%aja
        liikkeet(nappula)%etene_ei_kuus         = pelitaktiikka%etene_ei_kuus * liikkeet(nappula)%etene_ei_kuus
        liikkeet(nappula)%etene_kuus            = pelitaktiikka%etene_kuus * liikkeet(nappula)%etene_kuus
        
        liikkeet(nappula)%etene_toisen_lahto    = pelitaktiikka%etene_toisen_lahto * liikkeet(nappula)%etene_toisen_lahto
        liikkeet(nappula)%vaara                 = pelitaktiikka%vaara * liikkeet(nappula)%vaara
        liikkeet(nappula)%vaara_ei_liiku        = pelitaktiikka%vaara_ei_liiku * liikkeet(nappula)%vaara_ei_liiku
        liikkeet(nappula)%markov                = pelitaktiikka%markov * liikkeet(nappula)%markov
    end do
    
    ! Katso mikä on paras vaihtoehto jokaiselle nappulalle (pelitaktiikka päättää)
    ! Katsotaan samalla voidaanko tehdä mitään
   
    
    ! Pelityyliin osa, joka määrittää miten painotetaan riski / hyöty
    do nappula = 1, 4
        ! Nappulan suurin hyöty ja riski
        pisteet(nappula)%hyoty = maksimi_hyoty(liikkeet(nappula))
        pisteet(nappula)%riski = riski(liikkeet(nappula))

        ! Jos nappula on menossa maaliin niin ei riskejä
        if (liikkeet(nappula)%maaliin /= 0) then
            pisteet(nappula)%riski = 0
        end if
    end do

    ! Katso mikä on paras
    

    valittu_nappula = valitse_siirto(pisteet, pelitaktiikka, voitko_pelata, lauta, kuka, silmaluku)
    ! print*, "Valittu nappula"
    ! ! Jos voidaan liikkua niin liikutaan
    ! print*, valittu_nappula
    if (valittu_nappula /=0) then
        if (lauta(kuka, valittu_nappula) == -1) then
            print*, "SEIS!"
            stop
        end if
        ! Suorita isoimman pisteen omaava liike
        call siirto(silmaluku, lauta, kuka, valittu_nappula)
        ! Syödyt nappulat
        call syotiinko(silmaluku, lauta, kuka, valittu_nappula)
    end if
end subroutine


end module pelaajat