module pelityylit
use parametrit
implicit none
integer, parameter :: hyoty_lkm=9, riski_lkm=4


! Tehään sanakirja
type pelityyli

    ! Pelaajan persoonallisuus (nössö vai rohkelikko)
    real(rk)    :: persoona ! 1
   
    ! Hyödyt
      real(rk)  :: uusi_0, uusi_1, uusi_2, uusi_3 ! 5 
      real(rk)  :: aja ! 6
      real(rk)  :: syo   ! 7  
      real(rk)  :: maaliin ! 8
      real(rk)  :: etene_ei_kuus ! 9 
      real(rk)  :: etene_kuus ! 10
      ! Riskit
      real(rk)  :: etene_toisen_lahto ! 11
      real(rk)  :: vaara ! 12
      real(rk)  :: vaara_ei_liiku ! 13
      real(rk)  :: markov ! 14
end type

type pisteytys
    real(rk) :: hyoty
    real(rk) :: riski
end type



contains


! Alustetaan mahdolliset liikkeet, joita pelaaja
! voi tehdä vuorossa
function liikkeet_alustus() result(alustus)
    implicit none
    type(pelityyli) :: alustus
    alustus = pelityyli( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

end function

! Lue käyttäjän antama lista ja kasaa siitä pelityyli
function pelitaktiikka_alustus(monesko) result(pelitaktiikka)
    implicit none
    type(pelityyli) :: pelitaktiikka
    integer :: i,ios, j, monesko
    character(len=80) :: tiedosto
    real(rk), allocatable :: rivi(:)


    call get_command_argument(monesko,tiedosto) 
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

        if (ios /= 0) then
            write(0,'(/,a,/)') "Pelitaktiikka koostuu reaaliluvuisa väliltä [0,100]. Pelitaktiikassa on parametrejä: "
            print*, hyoty_lkm +riski_lkm
            write(0,'(/,a,/)') "Keskeytetään ohjelma"
            stop
        end if
        
        if (rivi(j) > 100 .or. rivi(j) < 0) then   
            print*, "Arvojen pitää olla 0 ja 100 välillä"
            stop
        end if
    end do

    close(1)

    ! Halutaan lukea tiedostosta 13 real(rk) arvoa ja asettaa
    ! ne oikeaan kohtaan pelitaktiikka datatyyppiä

    pelitaktiikka = pelityyli(rivi(1), 100, rivi(2), rivi(3), rivi(4), rivi(5), &
    rivi(6), rivi(7), rivi(8), rivi(9), rivi(10), rivi(11), rivi(12), rivi(13))

end function
! Kerätään "hyötyä" tuottavat liikkeet

function lista_posi(liikkeet) result(liikkeet_lista)
    implicit none
    type(pelityyli) :: liikkeet
    
    real(rk), allocatable :: liikkeet_lista(:)

    allocate(liikkeet_lista(hyoty_lkm))

    liikkeet_lista(1) = liikkeet%uusi_0 
    liikkeet_lista(2) = liikkeet%uusi_1 
    liikkeet_lista(3) = liikkeet%uusi_2 
    liikkeet_lista(4) = liikkeet%uusi_3
    liikkeet_lista(5) = liikkeet%aja
    liikkeet_lista(6) = liikkeet%syo
    liikkeet_lista(7) = liikkeet%maaliin    
    liikkeet_lista(8) = liikkeet%etene_ei_kuus  
    liikkeet_lista(9) = liikkeet%etene_kuus

end function


! Kerätään riskin huomioivat

function lista_negis(liikkeet) result(liikkeet_lista)
    implicit none
    type(pelityyli) :: liikkeet
    real(rk), allocatable :: liikkeet_lista(:)

    allocate(liikkeet_lista(riski_lkm))

    liikkeet_lista(1) = liikkeet%etene_toisen_lahto
    ! Pelityyliin osa, joka määrittää miten painotetaan riski / hyöty1) = liikkeet%etene_toisen_lahto    
    liikkeet_lista(2) = liikkeet%vaara   
    liikkeet_lista(3) = liikkeet%vaara_ei_liiku
    liikkeet_lista(4) = liikkeet%markov 
end function

! Katsotaan saatava hyöty

function maksimi_hyoty(liikkeet) result(pisteet)
    implicit none
    real(rk) :: pisteet

    type(pelityyli) :: liikkeet
    real(rk), allocatable :: liikkeet_lista(:)

    allocate(liikkeet_lista(hyoty_lkm))

    liikkeet_lista = lista_posi(liikkeet)
    pisteet = maxval(liikkeet_lista)
end function

! Katsotaan riski

function riski(liikkeet) result(pisteet)
    implicit none
    real(rk) :: pisteet
    type(pelityyli) :: liikkeet
    real(rk), allocatable :: liikkeet_lista(:)

    allocate(liikkeet_lista(riski_lkm))
    liikkeet_lista = lista_negis(liikkeet)
    pisteet = maxval(liikkeet_lista)
end function


! Luo satunnaisen pelityylin

subroutine satunnainen(pelittelija)
    implicit none
    type(pelityyli) :: pelittelija
    real(rk), allocatable :: luku(:)
    integer :: i

    allocate(luku(riski_lkm+hyoty_lkm+1))
    
    do i = 1, (riski_lkm+hyoty_lkm+1)
        call random_number(luku(i))
    end do
   
    

    if (luku(1) >= 0.5) then
        pelittelija%persoona = 1
    else 
        pelittelija%persoona = 0

    end if

        pelittelija%uusi_0                = 100*luku(2) ! 1 
        pelittelija%uusi_1                = 100*luku(3) ! 2
        pelittelija%uusi_2                = 100*luku(4)  !3
        pelittelija%uusi_3                = 100*luku(5) ! 4
        pelittelija%syo                   = 100*luku(6) ! 5 
        pelittelija%maaliin               = 100*luku(7) ! 6
        pelittelija%etene_ei_kuus         = 100*luku(8) ! 7
        pelittelija%etene_kuus            = 100*luku(9) ! 8
        pelittelija%aja                   = 100*luku(10)
        
        pelittelija%etene_toisen_lahto    = 100*luku(11) ! 9
        pelittelija%vaara                 = 100*luku(12) ! 10
        pelittelija%vaara_ei_liiku        = 100*luku(13) ! 11
        pelittelija%markov                = 100*luku(14) ! 12
  
    
end subroutine


end module

