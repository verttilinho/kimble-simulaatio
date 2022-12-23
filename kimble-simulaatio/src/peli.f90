program peli
use noppa
use peli_toiminnot
use pelaajat
use tietojen_tallennus


implicit none
integer, dimension(4,4) :: lauta, lauta_edellinen
integer ::  i,j, k, edellinen, luku, komentorivikomento_lkm, kuka, peli_kaynnissa, pelien_lkm, ios, sekoita, satunnaiset
integer, allocatable :: heitto_lkm(:)
type(pelaaja_tiedot), dimension(4) :: pelittelija, pelittelija_valmis
real(rk), dimension(6,6) :: todarit
character(len = 80) :: arg


! Luoteaan ajoasetukset
call ajo_asetukset(pelien_lkm, sekoita, satunnaiset)


! Alustetaan heittojen lukumäärä laskuri
allocate(heitto_lkm(pelien_lkm))
heitto_lkm = 0

! Alustetaan nopan painotus

todarit = todarijakauma()

! Otetaan satunaisesti nopan ensimmäine silmäluku
edellinen = noppa_alustus(6)

! Otetaan pelitaktiikat pelaajiin
 do kuka =1,4
    pelittelija(kuka) = pelaaja_tiedot_alustus(kuka)
 end do


lauta = 0

! Satunnaista aloitettu silmäluku
! edellinen = satunnaisest aloitettu silmäluku
edellinen  = noppa_alustus(6)
do k = 1, pelien_lkm
    ! Jos testataan yhden taktiikan toimimista satunnaisia taktiikoita vastaan
    if (satunnaiset == 1) then
        do kuka = 1, 4
        ! Pidetään ensimmäinen samana
            if (pelittelija(kuka)%id /= 1) then
                call satunnainen(pelittelija(kuka)%pelitaktiikka)
            end if
        end do
    end if

    ! Jos arvotaan täysin satunnaiset
    if (satunnaiset == 2) then
        do kuka = 1, 4
            call satunnainen(pelittelija(kuka)%pelitaktiikka)
        end do
    end if

    ! Arvotaanko aloittajat
    if (sekoita == 1) then
        call aloittajan_arpoja(pelittelija)
    end if

    peli_kaynnissa = 1
    do while(peli_kaynnissa == 1) 
        
        ! Kierros
        kuka = 0
        do while(kuka<4)
            ! Lisätään uusi heitto
            heitto_lkm(k) = heitto_lkm(k) +1
            lauta_edellinen = lauta
            kuka = kuka+1
            ! print*, "Kuka"
            ! print*, kuka
            luku = silmaluku(edellinen, todarit)
            ! print*, "Noppa"
            ! print*, luku
            call pelaaja(luku, lauta, pelittelija(kuka)%pelitaktiikka, kuka)

            call sijoitustarkastaja(lauta, kuka, pelittelija(kuka)%sija_nykyinen)

               
               
            peli_kaynnissa = peli_loppu(lauta, kuka, pelittelija(kuka)%sija_nykyinen)

            ! Jos heität kutosen ja et ole maalissa, ni saat uuden vuoron
            if (luku==6 .and. pelittelija(kuka)%sija_nykyinen == 0) then
                kuka = kuka - 1
                edellinen = silmaluku(edellinen, todarit)
            
            ! muuten vain päivitetään edellinen luku
            
            else
                edellinen = silmaluku(edellinen, todarit)
            end if
            
                
            ! Peli päättyi
            if (peli_kaynnissa == 0) then
                exit
            end if

    
        end do

    end do


    ! Päivitellään sijat ja valmistellaan uusi peli
    do j = 1,4
        ! Sihteeri kirjaa sijoitukset ylös kullekin pelaajalle
        call sihteeri(pelittelija(j)%sija_nykyinen, pelittelija(j))
        ! Alustetaan pelaajien nykyinen_sija seuraavaan peliin
        pelittelija(j)%sija_nykyinen = 0
    end do
    ! Alustetaan lauta
     lauta = 0
end do




! Järjestetään pelitaktiikat alkuperäiseen järjestykseen
! jos tarve

if (sekoita == 1) then
    pelittelija_valmis = jarjestys(pelittelija)
else
    pelittelija_valmis = pelittelija
end if
! Tulostetaan jakaumat
call tulostus(pelittelija_valmis, pelien_lkm, heitto_lkm)

! Tallennetaan nämä jakaumat vielä erillisiin tiedostoihin

call tallennus(pelittelija_valmis)





end program peli