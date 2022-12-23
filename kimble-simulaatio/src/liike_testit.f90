module liike_testit
use siirrot
implicit none

contains

! LISÄÄ KAIKKIIN ETTÄ PITÄÄ OLLA LAUDALLA

! Liiketestit: Voiko asian tehdä nappulalla 



subroutine uusi(silmaluku, lauta, liikkeet, kuka, nappula)
    implicit none
    integer :: silmaluku, kentalla, kuka, nappula
    integer, dimension(4,4) :: lauta
    type(pelityyli) :: liikkeet

    kentalla = count(lauta(kuka,:) /= 0 .and. lauta(kuka,:) /= -1)
    ! Jos himassa
    if (lauta(kuka, nappula) == 0) then
        if (silmaluku == 6) then
            if (kentalla == 0) then
                liikkeet%uusi_0 = 1
            end if 

            if (kentalla == 1) then
                liikkeet%uusi_1 = 1
            end if 

            if (kentalla == 2) then
                liikkeet%uusi_2 = 1
            end if             

            if (kentalla == 3) then
                liikkeet%uusi_3 = 1
            end if    
        end if         
    end if 
end subroutine


subroutine maaliin(silmaluku, lauta, liikkeet, kuka, nappula)
    implicit none    
    integer :: silmaluku, kuka, nappula
    type(pelityyli) :: liikkeet
    integer, dimension(4,4) :: lauta
    integer, dimension(3) :: maali
 
    ! Onks nappula laudalla

    if (lauta(kuka, nappula) /= 0 .and. lauta(kuka, nappula) /= -1) then

        ! Tarkistetaan mennäänkö maaliin
        maali = maalin_paikka(kuka)
        
        ! Oikea vyöhyke ja maaliin
        if ((lauta(kuka, nappula) + silmaluku) >= maali(1) .and. &
            lauta(kuka, nappula) >= maali(2) .and. &
            lauta(kuka, nappula) <= maali(3)) then
        ! Alle kolme maalissa
            if (kuinka_monta_maalissa(lauta, kuka) /= 3) then
                liikkeet%maaliin = 1
                

        ! Jos kolme maalissa, vika kimpoilee
            else 
                ! Jos pääset tasaluvulla, niin mene maaliin
                if ((lauta(kuka, nappula) + silmaluku) == maali(1)) then
                    liikkeet%maaliin = 1
                end if
            end if
        end if 
    end if
end subroutine


subroutine syo(silmaluku, lauta, liikkeet, kuka, nappula)
    implicit none
    integer :: silmaluku, kuka, i, j, nappula
    type(pelityyli) :: liikkeet
    integer, dimension(4,4) :: lauta
    
    ! Nyt käydään läpi voiko pelaaja syödä nappuloita

    do i = 1, 4

    ! Jos ei oma nappula niin katotaan voidaanks syödä
        if (i /= kuka) then
            do j = 1, 4
                if ((lauta(kuka, nappula) + silmaluku) == lauta(i, j)) then
                    liikkeet%syo = 1
                end if
            end do!use vuorovaikutus_funktiot
        end if
    end do
    
end subroutine 


subroutine etene_ei_kuus(silmaluku, lauta, liikkeet, kuka, nappula)
    implicit none
    integer :: silmaluku, kuka, nappula, i
    type(pelityyli) :: liikkeet, varmistus
    integer, dimension(4,4) :: lauta

    
    if (silmaluku /= 6) then
            
        ! Onks nappula laudalla

        if (lauta(kuka, nappula) /= 0 .and. lauta(kuka, nappula) /= -1) then
            liikkeet%etene_ei_kuus = 1
        end if

    end if
end subroutine 

! Voidaanko edetä (heitetty kutonen)
subroutine etene_kuus(silmaluku, lauta, liikkeet, kuka, nappula)
    implicit none
    integer :: silmaluku, kuka, nappula
    type(pelityyli) :: liikkeet
    integer, dimension(4,4) :: lauta
    
    if (silmaluku == 6) then
        ! Nappula ei ole himassa
    
        if (lauta(kuka, nappula) /= 0 .and. lauta(kuka, nappula) /= -1) then
                    liikkeet%etene_kuus = 1        
        end if
    end if
    
end subroutine 

! Voidaanko edetä (ei heitetty kutosta)
subroutine etene_toisen_lahto(silmaluku, lauta, liikkeet, kuka, nappula)
    implicit none
    integer :: silmaluku, kuka, nappula
    type(pelityyli) :: liikkeet
    integer, dimension(4,4) :: lauta
    
    ! Onks nappula laudalla

    if (lauta(kuka, nappula) /= 0 .and. lauta(kuka, nappula) /= -1) then
        
        ! Omaan lähtöön eteneminen ei haittaa (tälloin pääsee maaliin)
        
        if (kuka == 1) then
            if (lauta(kuka, nappula) + silmaluku == 8) then
                liikkeet%etene_toisen_lahto = 1
            end if

            if (lauta(kuka, nappula) + silmaluku == 15) then
                liikkeet%etene_toisen_lahto = 1
            end if

            if (lauta(kuka, nappula) + silmaluku == 22) then
                liikkeet%etene_toisen_lahto = 1
            end if
        else if (kuka == 2) then
            if (lauta(kuka, nappula) + silmaluku == 29) then
                liikkeet%etene_toisen_lahto = 1
            end if

            if (lauta(kuka, nappula) + silmaluku == 15) then
                liikkeet%etene_toisen_lahto = 1
            end if

            if (lauta(kuka, nappula) + silmaluku == 22) then
                liikkeet%etene_toisen_lahto = 1        
            end if  

        else if (kuka == 3) then
            if (lauta(kuka, nappula) + silmaluku == 29) then
                liikkeet%etene_toisen_lahto = 1
            end if

            if (lauta(kuka, nappula) + silmaluku == 8) then
                liikkeet%etene_toisen_lahto = 1
            end if

            if (lauta(kuka, nappula) + silmaluku == 22) then
                liikkeet%etene_toisen_lahto = 1  
            end if      

        else 
            if (lauta(kuka, nappula) + silmaluku == 29) then
                liikkeet%etene_toisen_lahto = 1
            end if

            if (lauta(kuka, nappula) + silmaluku == 8) then
                liikkeet%etene_toisen_lahto = 1
            end if

            if (lauta(kuka, nappula) + silmaluku == 15) then
                liikkeet%etene_toisen_lahto = 1
            end if
        end if
    end if


end subroutine 

subroutine aja(silmaluku, lauta,  liikkeet, kuka, nappula)
    implicit none
    integer :: silmaluku, kuka, nappula, i, j
    type(pelityyli) :: liikkeet
    integer, dimension(4,4) :: lauta
    
    ! Jos pääset alle kuuden päähän toisesta 
    ! se on ajamista
    
    ! Onks nappula laudalla

    if (lauta(kuka, nappula) /= 0 .and. lauta(kuka, nappula) /= -1) then
        do i = 1, 4
        ! Jos ei oma nappula niin katotaan voidaanks syödä
            if (i /= kuka) then
                do j = 1, 4
                    if (uusi_paikka(silmaluku, lauta(kuka, nappula)) < lauta(i, j) .and. &
                    etaisyys(lauta(kuka, nappula), lauta(i, j)) <= 6) then
                        liikkeet%aja = 1
                    end if
                end do
            end if
        end do
    end if
end subroutine 

! Jos ollaan menossa paikkaan jossa vastustajan nappula
! on <=6 päässä niin ollaan vaarassa. Tähän ei haluta
! palautetaan 1 jos ei olla vaarassa
subroutine vaara(silmaluku, lauta,  liikkeet,kuka, nappula)
    implicit none
    integer :: silmaluku, kuka, nappula, i, j
    type(pelityyli) :: liikkeet
    integer, dimension(4,4) :: lauta

    ! Ei olla vaarassa jos ei olla laudalla
    if (lauta(kuka, nappula) /= 0 .and. lauta(kuka, nappula) /= -1) then
        do i = 1, 4
            ! Omaa nappulaa ei syödä
            if (i /= kuka) then
                do j = 1, 4
                    ! Ei olla vaarassa jos vastustajan nappula ei ole laudalla
                    if (lauta(i, j) /= 0 .and. lauta(i, j) /= -1) then
                        if ((uusi_paikka(silmaluku, lauta(kuka, nappula)) > lauta(i, j)) .and. &
                            etaisyys(uusi_paikka(silmaluku, lauta(kuka, nappula)), lauta(i,j)) <= 6) then
                            ! Mitä enemmän nappuloita, sitä pahempi tilanne
                            liikkeet%vaara =  liikkeet%vaara + 1
                        end if
                    end if
                end do
            end if
        end do
    end if
end subroutine 


! ! Jos uhataan kun ollaan paikallaan, mutta ei uhkaa jos liikutaan
subroutine vaara_ei_liiku(lauta, liikkeet, kuka, nappula)
    implicit none
    integer ::  kuka, nappula, i, j
    type(pelityyli) :: liikkeet
    integer, dimension(4,4) :: lauta

    ! Ei olla vaarassa jos ei olla laudalla
    if (lauta(kuka, nappula) /= 0 .and. lauta(kuka, nappula) /= -1) then
        do i = 1, 4
            if (i /= kuka) then
                do j = 1, 4
                    ! Ei olla vaarassa jos vastustajan nappula ei ole laudalla
                    if (lauta(i, j) /= 0 .and. lauta(i, j) /= -1) then
                        if (lauta(kuka, nappula) > lauta(i, j) .and. &
                            etaisyys(lauta(kuka, nappula), lauta(i,j)) <= 6) then
                            ! Mitä enemmän nappuloita, sitä pahempi tilanne
                            liikkeet%vaara_ei_liiku = liikkeet%vaara_ei_liiku + 1
                        end if
                    end if
                end do
            end if
        end do
    end if
end subroutine


subroutine markov_nautinto(silmaluku, lauta,  liikkeet, kuka,nappula)
    implicit none
    integer :: silmaluku, kuka, nappula, i
    type(pelityyli) :: liikkeet
    integer, dimension(4,4) :: lauta

    ! Miten katsotaan kuka on seuraava
    if (kuka /= 4) then
       
        ! Jos seuraava nappula syö 7-silmaluku (vastakkainen)
        do i = 1, 4
            if (uusi_paikka(silmaluku,lauta(kuka, nappula)) == uusi_paikka(7-silmaluku,lauta(kuka+1, i))) then
                liikkeet%markov = 1
            end if
        end do

    else
        do i = 1,4
            if (uusi_paikka(silmaluku,lauta(kuka, nappula)) == uusi_paikka(7-silmaluku,lauta(1, i))) then
                liikkeet%markov = 1
            end if
        end do
    end if
   
end subroutine 


end module