module siirrot
use vuorovaikutus_funktiot
implicit none

! Tehdään siirtoja

! Jos pelaaja saa uuden nappulan

contains

    subroutine laudalle(silmaluku, lauta, kuka, nappula) 
        implicit none
        integer :: silmaluku, kuka, nappula
        integer, dimension(4,4) :: lauta
        ! Teoreettisesti jos ei olla laudalla niin otetaan uusi
        ! ja silmäluvun tulisi olla 6. Hyvä kuitenkin varmistaa
        if (silmaluku == 6) then
            ! Aloitus ruutu riippuu siitä kuka olet
            if (kuka == 1) then
                lauta(kuka, nappula) = 1
        

            else if (kuka == 2) then
                lauta(kuka, nappula) = 8

            else if (kuka == 3) then
                lauta(kuka, nappula) = 15

            else if (kuka == 4) then
                lauta(kuka, nappula) = 22

            end if
        end if
    end subroutine

    

    ! liikutetaan nappulaa
    subroutine liiku(silmaluku, lauta, kuka, nappula) 
        implicit none
        integer :: silmaluku, kuka, nappula
        integer, dimension(4,4) :: lauta
        integer, dimension(3) :: maali
        
        ! Tarkistetaan mennäänkö maaliin
        maali = maalin_paikka(kuka)
        
        ! Oikea vyöhyke ja maaliin
        if ((lauta(kuka, nappula) + silmaluku) >= maali(1) .and. &
            lauta(kuka, nappula) >= maali(2) .and. &
            lauta(kuka, nappula) <= maali(3)) then
        
        ! Alle kolme maalissa
            if (kuinka_monta_maalissa(lauta, kuka) /= 3) then
                lauta(kuka, nappula) = -1

        ! Jos kolme maalissa, vika kimpoilee
            else 
                ! Jos pääset tasaluvulla, niin mene maaliin
                if ((lauta(kuka, nappula) + silmaluku) == maali(1)) then
                    lauta(kuka, nappula) = -1
                ! Jos menee yli, kimpoile
                else
                    lauta(kuka, nappula) = kimpoilu(silmaluku, lauta, kuka, nappula)
                end if
            end if
        
        ! Jos ei olla maalissa niin liikutaan
        ! 28 viimeinen paikka laudalla, sen jälkeen tulee
        ! paikka 1
        else
            if (lauta(kuka, nappula) + silmaluku < 29) then
                lauta(kuka, nappula) = lauta(kuka, nappula) + silmaluku
        
            else
                lauta(kuka, nappula) = lauta(kuka, nappula) + silmaluku - 28
            end if
        end if

    end subroutine


    subroutine siirto(silmaluku, lauta, kuka, nappula)
        implicit none
        integer :: silmaluku, kuka, nappula
        integer, dimension(4,4) :: lauta

        ! Ollaanko laudalla
        if (lauta(kuka, nappula) == 0) then
            call laudalle(silmaluku, lauta, kuka, nappula) 

        ! Jos ei heitetty uutta niin liikutaan kentällä
        else
            call liiku(silmaluku, lauta, kuka, nappula)

        end if

    end subroutine

    subroutine syotiinko(silmaluku, lauta, kuka, nappula)
        implicit none
        integer :: silmaluku, kuka, nappula, i, j
        integer, dimension(4,4) :: lauta

        ! Jos päädytään toisen pelaajan nappulan paikalle, 
        ! palautetaan vastustajan nappula kotiin eli indeksille nolla

        ! Maalissa ei syödä:D, eikä kotona
        if (lauta(kuka, nappula) /= -1 .and. lauta(kuka, nappula) /= 0) then

            ! käydään muiden nappulat läpi ja katotaan syötiinkö
            ! omaa ei voi syödä
            do i = 1, 4
                if (kuka /= i) then
                    do j = 1, 4
                        if (lauta(kuka, nappula) == lauta(i, j)) then
                            lauta(i, j) = 0
                        end if
                    end do
                end if
            end do
        end if
    end subroutine

end module