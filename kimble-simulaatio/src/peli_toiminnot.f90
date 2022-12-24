module peli_toiminnot
use parametrit
use pelityylit
use noppa
implicit none



type pelaaja_tiedot
    type(pelityyli) :: pelitaktiikka
    integer :: id, sija_1, sija_2, sija_3, sija_4, sija_nykyinen
end type

!alustetaan laskuri pelattujen pelien määrälle. Tätä
!käytetään voittojen tallentamisessa
real :: pelatut_pelit = 0
integer :: voitot(4) = 0



contains

! Ajoasetukset


subroutine ajo_asetukset(pelien_lkm, sekoita, satunnaiset)
    implicit none
    integer :: pelien_lkm, sekoita, satunnaiset, ios, j, i
    character(len=80) :: tiedosto, arg
    integer, allocatable :: rivi(:)
    


    if (command_argument_count()/=5) then
       call get_command_argument(0,arg)
       write(0,'(/,a,/)') 'Käyttö: '//trim(arg)// " taktiikka1.dat, taktiikka2.dat, taktiikka3.dat, taktiikka4.dat, asetukset.dat"
       stop
    end if

    call get_command_argument(5,tiedosto) 
    open(unit=1,file=tiedosto,iostat=ios) !löytyykö

    if (ios/=0) then
        print '(a,a)','*** Virhe tiedoston avaamisessa ',trim(tiedosto)
        stop
    end if

    ! Tehää laskuri että osataan lukea
    i = 0
    do
    read(1, *, iostat= ios) !Ja sitte luetaa
    ! ettei tuu mitään epämukavaa   
        if (ios/=0) exit
        i = i+ 1
    end do
    ! Mennää takas tiedoston alkuun
    rewind(1)
    
    ! vähän tilaa
    allocate(rivi(i))

    ! Tallennetaa rivit muistii
    do j = 1, size(rivi)
        read(1, *, iostat= ios) rivi(j)
        if (ios /=0) then
		    write(0,'(/,a,/)') "Ajoasetusten tulee olla kokonaislukuja (pelien_lkm, sekoita, satunnaiset)"
            write(0,'(/,a,/)') "Keskeytetään ohjelma"		
		    stop
        end if
    end do
    
    close(1)

    


    pelien_lkm = rivi(1)
    if (pelien_lkm <= 0) then
        write(0,'(/,a,/)') "Pelienlukumäärä on positiivinen kokonaisluku."
        write(0,'(/,a,/)') "Keskeytetään ohjelma."
        stop
    end if
    sekoita = rivi(2)

    satunnaiset = rivi(3)

    print*, 'Pelien lukumäärä', pelien_lkm
    print*, "Arvotaanko aloittaja (1 = kyllä) ", sekoita
    print*, "Pelataanko satunnaisilla pelaajilla (1 = 1 pelaaja ei ole satunnainen, 2 = kaikki satunnaisia) ", satunnaiset    
end subroutine



! Aloittajan arpoja

subroutine aloittajan_arpoja(pelaajat)
    implicit none
    type(pelaaja_tiedot) :: paikan_pitaja
    type(pelaaja_tiedot), dimension(4) :: pelaajat
    integer :: i, j

! Tehdään hyväksi todettu Fisher-Yates shuffle
    do i = 0,3
        j = noppa_alustus(4-i)
        paikan_pitaja = pelaajat(j)
        pelaajat(j) = pelaajat(4-i)
        pelaajat(4-i) = paikan_pitaja
    end do 
end subroutine


! Pelaaja sai kaikki nappulat maaliin

! Alustetaan pelaajan tiedot

function pelaaja_tiedot_alustus(kuka) result(pelittelija)
    implicit none
    integer :: kuka
    type(pelaaja_tiedot) :: pelittelija
    pelittelija%pelitaktiikka = pelitaktiikka_alustus(kuka)
    pelittelija%id = kuka
    pelittelija%sija_1 = 0
    pelittelija%sija_2 = 0
    pelittelija%sija_3 = 0
    pelittelija%sija_4 = 0
    pelittelija%sija_nykyinen = 0
end function

function pelaaja_valmis(lauta, kuka) result(tulos)
    implicit none
    integer, dimension(4,4) :: lauta
    integer :: kuka, tulos
    ! 0 jos peli kesken
    tulos = 0

    ! 1 jos kaikki maalissa. Kun kaikki maalissa
    ! jokainenn alkio on indeksillä -1
    if (sum(lauta(kuka, :)) == -4) then
        tulos = 1
    end if

end function




! Sijoitustarkistaja
subroutine sijoitustarkastaja(lauta, kuka, sijoitus)
    implicit none
    integer :: i, kuka, muut_maalissa, sijoitus
    integer, dimension(4,4) :: lauta
    

    ! Tarkistetaan, että pelaaja ei ole päässyt aiemmalla vuorolla
    ! maaliin

    if (sijoitus == 0) then
        ! Jos pääsit maaliin kaikilla
        if (pelaaja_valmis(lauta, kuka) == 1) then
            sijoitus = 1
            ! katsotaan monesko olit
            do i = 1,4
            ! katsotaan onko muut maalissa
                if (i /= kuka) then
                    sijoitus = sijoitus + pelaaja_valmis(lauta, i)
                end if
            end do
        
        ! Tarkistetaan vielä ootko neljäs
        else
            muut_maalissa = 0
            do i = 1,4
            ! katsotaan onko muut maalissa
                if (i /= kuka) then
                    muut_maalissa = muut_maalissa + pelaaja_valmis(lauta, i)
                end if
            end do
            ! Jos muut maalissa, sä hävisit
            if (muut_maalissa == 3) then
                sijoitus = 4
            
            ! Pelaaja ei päässyt maaliin
            else 
                sijoitus = 0
            end if
        end if
   end if
end subroutine


! Tarkistetaan loppuiko peli
function peli_loppu(lauta, kuka, sijoitus) result(kaynnissa)
    implicit none
    integer :: kuka, kaynnissa, sijoitus 
    integer, dimension(4,4) :: lauta

    kaynnissa = 1
    
    if (sijoitus == 4) then
        kaynnissa = 0
    end if

end function


! Sihteeri tallentaa sijoituksen 
subroutine sihteeri(sijoitus, pelittelija)
    implicit none
    type(pelaaja_tiedot) :: pelittelija
    integer :: sijoitus
    !jos ei olla vielä pelattu yhtäkään peliä, sihteeri alustaa
    !voittajat.txt tiedoston
    if (pelatut_pelit == 0) then
        call voittotiedoston_alustus("voittajat.txt")
    end if
    !koska sihteeriä kutsutaan neljästi, siksi lisätään 0.25
    pelatut_pelit=pelatut_pelit+0.25
    if (sijoitus == 1) then
        pelittelija%sija_1 = pelittelija%sija_1 + 1
        voitot(pelittelija%id) = voitot(pelittelija%id) + 1
        !"aina välillä" tallennetaan tämä voitot vektori tiedostoon:
        !lukua 100 voi muokata, jos näyttää siltä että tallennetaan liikaa dataa.
        if (modulo(nint(pelatut_pelit),500)==0) then
            call voittojen_tallennus(voitot)
        end if
    else if (sijoitus == 2) then
        pelittelija%sija_2 = pelittelija%sija_2 + 1
    
    else if (sijoitus == 3) then
        pelittelija%sija_3 = pelittelija%sija_3 + 1

    else if (sijoitus == 4) then
        pelittelija%sija_4 = pelittelija%sija_4 + 1
    end if

end subroutine



!aliohjelma, jolla tallennetaan voittolista tiedostoon
subroutine voittojen_tallennus(lista)
    implicit none
    integer, intent(in) :: lista(4)
    character(len=13)   :: tiedosto = "voittajat.txt"
    

    open(unit=1,file=tiedosto)
    write(1,*) lista

end subroutine

subroutine voittotiedoston_alustus(tiedosto)
    implicit none
    character(len=13),intent(in) :: tiedosto
    integer                      :: alku(4) = 0

    open(unit=1, file=tiedosto)

    write(1,*) "        pelaaja1 pelaaja2 pelaaja3 pelaaja4"
    write(1,*) alku
end subroutine


end module