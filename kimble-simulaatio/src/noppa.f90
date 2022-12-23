module noppa
use parametrit
implicit none


contains 

   ! Luetaan fort.10 tiedosto. Sarakkevektoreissa on todennäköisyys 
   ! kullekin luvulle. Esim. Ensimmäinen sarake kuvaa luvun 1 jakaumaa
    function todarijakauma() result(taulukko)
        integer :: ios, i
        real(rk) :: satunnaisluku, raja
        real(rk), dimension(6) :: rajat
        real(rk), dimension(6,6) :: taulukko

        read(10, *) taulukko

    end function


    ! Tehään n-sivuinen noppa
    function noppa_alustus(n) result(tulos)
        integer ::i, tulos, n
        real(rk) :: satunnaisluku
        real(rk), dimension(6) :: rajat


        ! Otetaan satunnainen luku väliltä (0,1)
        call random_number(satunnaisluku)
    
        ! Jateaan väli (0,1) tasaisiin väleihin väleihin (kuuteen väliin)

        rajat = 0.0 ! alustetaan toi lista 
        do i = 1,n
            rajat(i) = 1.0/n*i
        
        end do

        !Katsotaan mile välille satunnaisluku osuu ja annetaan silmäluku.
        !Käydään läpi rajat alhaalta ylöspäin ja katsotaan mille välille jäädään
        
        tulos = 0

        do i = 1,n
            ! Lisätään silmälukuun aina seuraava
            tulos = tulos + 1
            ! Jos rajat paukkuu niin lopetetaan, koska tälloin silmäluku on halutulla
            ! välillä
            if (satunnaisluku < rajat(i)) then
                exit
            end if
        end do


        ! Katsotaan mile välille satunnaisluku osuu ja annetaan silmäluku.
        ! Käydään läpi rajat alhaalta ylöspäin ja katsotaan mille välille jäädään
        
        tulos = 0

        do i = 1,n
            ! Lisätään silmälukuun aina seuraava
            tulos = tulos + 1
            ! Jos rajat paukkuu niin lopetetaan, koska tälloin silmäluku on halutulla
            ! välillä
            if (satunnaisluku < rajat(i)) then
                exit
            end if
        end do


    end function


    function silmaluku(edellinen, todarit) result(tulos)
        implicit none

        integer :: edellinen, i, tulos
        real(rk) :: satunnaisluku, raja
        real(rk), dimension(6) :: rajat
        real(rk), dimension(6,6) :: todarit

        ! Luetaan todennäköisyysjakaumat
        
        !tulos = todarit(1, 1)  

        ! Satunnaisluku väliltä (0,1)
        call random_number(satunnaisluku)
        
        

        ! Jateaan väli (0,1) todennäköisyys jakauman antamiin väleihi


        raja = 0.0
        rajat = 0.0 ! alustetaan toi lista
        do i = 1,6
            if  (i == 1) then
                raja = todarit(edellinen, i)
            else 
                raja = todarit(edellinen, i) + raja
            end if

            rajat(i) = raja
        end do

        !Katsotaan mile välille satunnaisluku osuu ja annetaan silmäluku.
        !Käydään läpi rajat alhaalta ylöspäin ja katsotaan mille välille jäädään
        
        tulos = 0

        do i = 1,6
            ! Lisätään silmälukuun aina seuraava
            tulos = tulos + 1
            ! Jos rajat paukkuu niin lopetetaan, koska tälloin silmäluku on halutulla
            ! välillä
            if (satunnaisluku < rajat(i)) then
                exit
            end if
        end do


        ! Katsotaan mile välille satunnaisluku osuu ja annetaan silmäluku.
        ! Käydään läpi rajat alhaalta ylöspäin ja katsotaan mille välille jäädään
        
        tulos = 0

        do i = 1,6
            ! Lisätään silmälukuun aina seuraava
            tulos = tulos + 1
            ! Jos rajat paukkuu niin lopetetaan, koska tälloin silmäluku on halutulla
            ! välillä
            if (satunnaisluku < rajat(i)) then
                exit
            end if
        end do

    end function






end module noppa