module tietojen_tallennus
use peli_toiminnot  
implicit none


contains

function jarjestys(pelittelija) result(pelittelija_valmis)
    implicit none 
    integer :: kuka
    type(pelaaja_tiedot), dimension(4) :: pelittelija, pelittelija_valmis


    ! Järjestetään pelittelijat ensin alkuperäiseen järjestykseen

    do kuka = 1, 4
        if (pelittelija(kuka)%id == 1) then
            pelittelija_valmis(1) = pelittelija(kuka)

        else if (pelittelija(kuka)%id == 2) then
            pelittelija_valmis(2) = pelittelija(kuka)

        else if (pelittelija(kuka)%id == 3) then
            pelittelija_valmis(3) = pelittelija(kuka)

        else if (pelittelija(kuka)%id == 4) then
            pelittelija_valmis(4) = pelittelija(kuka)
        end if
    end do

end function

subroutine tulostus(pelittelija, n, heitto_lkm)
    implicit none
    integer :: n
    integer, dimension(n) :: heitto_lkm
    type(pelaaja_tiedot), dimension(4):: pelittelija, pelittelija_valmis
    character(len=80) :: muotoilu
    
    ! Keskimääräiset heitot pelissä
    print*, "Heittojen keskimääräinen lukumäärä pelissä:", sum(heitto_lkm)/real(n)  
    !print*, heitto_lkm

    muotoilu = "(x,a,x,f0.1,x,'%')"
    print*, "Ensimmäisen taktiikan sijoitusjakauma prosentteina"
    print muotoilu, "1:", 100*pelittelija(1)%sija_1/real(n)
    print muotoilu,  "2:", 100*pelittelija(1)%sija_2/real(n)
    print muotoilu,  "3:", 100*pelittelija(1)%sija_3/real(n)
    print muotoilu,  "4:", 100*pelittelija(1)%sija_4/real(n)

    print*, "Toisen taktiikan sijoitusjakauma prosentteina"
    
    print muotoilu, "1:", 100*pelittelija(2)%sija_1/real(n)
    print muotoilu,  "2:", 100*pelittelija(2)%sija_2/real(n)
    print muotoilu,  "3:", 100*pelittelija(2)%sija_3/real(n)
    print muotoilu,  "4:", 100*pelittelija(2)%sija_4/real(n)



    print*, "Kolmannen taktiikan sijoitusjakauma prosentteina"
    
    print muotoilu, "1:", 100*pelittelija(3)%sija_1/real(n)
    print muotoilu,  "2:", 100*pelittelija(3)%sija_2/real(n)
    print muotoilu,  "3:", 100*pelittelija(3)%sija_3/real(n)
    print muotoilu,  "4:", 100*pelittelija(3)%sija_4/real(n)

    print*, "Neljännen taktiikan sijoitusjakauma prosentteina"

    print muotoilu, "1:", 100*pelittelija(4)%sija_1/real(n)
    print muotoilu,  "2:", 100*pelittelija(4)%sija_2/real(n)
    print muotoilu,  "3:", 100*pelittelija(4)%sija_3/real(n)
    print muotoilu,  "4:", 100*pelittelija(4)%sija_4/real(n)


end subroutine


subroutine tallennus(pelittelija)
    implicit none
    integer :: i, ios
    character(len=80), dimension(4) :: nimet
    type(pelaaja_tiedot), dimension(4) :: pelittelija

    nimet = ["Pelitaktiikka_1_tulokset.txt", "Pelitaktiikka_2_tulokset.txt",&
             "Pelitaktiikka_3_tulokset.txt", "Pelitaktiikka_4_tulokset.txt"] 

    ! Tehdään tiedostoja
    do i=1,4
        open(unit=1,file=nimet(i),iostat=ios)

    !   kirjotellaa sijoitukset
        write(1,*) pelittelija(i)%sija_1
        write(1,*) pelittelija(i)%sija_2
        write(1,*) pelittelija(i)%sija_3
        write(1,*) pelittelija(i)%sija_4

        close(unit=1,status='keep')
    end do
end subroutine   

end module