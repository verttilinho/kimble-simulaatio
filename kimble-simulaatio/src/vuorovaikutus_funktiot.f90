module vuorovaikutus_funktiot
use parametrit
use pelityylit
implicit none

contains

function uusi_paikka(silmaluku, paikka)
    implicit none
    integer :: silmaluku, uusi_paikka, paikka
    ! 28 viimeinen paikka laudalla, sen jälkeen tulee
    ! paikka 1

    if (paikka + silmaluku < 29) then
        uusi_paikka = paikka + silmaluku
    
    else
        uusi_paikka = paikka + silmaluku - 28
    end if

end function


function etaisyys_koti(lauta, kuka, nappula) result(etaisyys)
    implicit none
    integer, dimension(4,4) :: lauta
    integer :: kuka, nappula, etaisyys
    if (kuka /= 1) then
        if (lauta(kuka, nappula) >= 7*(kuka-1)+1) then
            etaisyys = 28 - (lauta(kuka, nappula)-(7*(kuka-1)+1))

        else 
            etaisyys = 7*(kuka-1)+1-lauta(kuka, nappula)
        end if
    else
        etaisyys = 29-lauta(kuka, nappula)
    end if
end function




function etaisyys(paikka1, paikka2)
    implicit none 
    integer :: paikka1, paikka2, etaisyys
    
    ! Kahden nappulan välinen etäisyys

    ! Laudan symmetrian vuoksi (ympyrä)
    if (abs(paikka1 - paikka2) <= 14) then
        etaisyys = abs(paikka1 - paikka2)

    else 
        etaisyys = 28 - abs(paikka1 - paikka2)
    end if

end function


function oma_syo(silmaluku, lauta, kuka, nappula) result(tulos)
    implicit none
    ! Tulos = 1 syödään oma, tulos = 0, ei syödä
    integer :: i, tulos, silmaluku, kuka, nappula
    integer, dimension(4,4) :: lauta

    do i = 1, 4
        ! Nappula ei kotipesässä tai maalissa
        if (lauta(kuka, nappula) /= 0 .and. lauta(kuka, nappula) /= -1) then
            if ((lauta(kuka, nappula) + silmaluku) == lauta(kuka, i)) then
                tulos = 1
            end if
        ! Jos ollaan himassa ja heitetään kutonen
        else if (lauta(kuka, nappula)==0) then
            if (silmaluku == 6) then
                ! aloitus paikka riippuu siitä kuka oot 
                if (lauta(kuka, i) == (7*(kuka-1) + 1)) then
                    tulos = 1
                end if
            end if
        end if
    end do


end function


function maalin_paikka(kuka)
        implicit none
        integer :: kuka
        integer, dimension(3) :: maalin_paikka
        

        ! maalit on eri kohdissa
        if (kuka == 1) then
            maalin_paikka(1) = 29
            maalin_paikka(2) = 23
            maalin_paikka(3) = 28
        else
            maalin_paikka = 7*(kuka-1) + 1 
             ! Alaraja vyöhykkeelle josta voi mennä maaliin
            maalin_paikka(2) = 7*(kuka-1) - 5
            ! Yläraja vyöhykkeelle, josta voi mennä maaliin
            maalin_paikka(3) = 7*(kuka-1)
        end if

    
    end function

    ! Kuinka monta maalissa
    function kuinka_monta_maalissa(lauta, kuka) result(laskuri)
        implicit none
        integer :: kuka, laskuri, nappula
        integer, dimension(4,4) :: lauta

        laskuri = 0

        do nappula = 1,4
            if (lauta(kuka, nappula)==-1)then
                laskuri = laskuri +1
            end if
        end do
    end function

    ! Viimeinen nappula kimpoilee
    function kimpoilu(silmaluku, lauta, kuka, nappula) result(paikka)
        implicit none
        integer:: kuka, nappula, silmaluku, paikka, uusi_silmaluku
        integer, dimension(4,4) :: lauta
        integer, dimension(3) :: maali
    
        maali = maalin_paikka(kuka)
        uusi_silmaluku = etaisyys_koti(lauta, kuka, nappula) - silmaluku 
        paikka = maali(1) + uusi_silmaluku
    end function

end module