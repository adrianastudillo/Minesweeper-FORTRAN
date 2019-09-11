module modulos
    implicit none
    contains

    subroutine espaciar(esp)
        integer::esp,i
        do i=0,esp
            print*," "
        end do
    end subroutine

    subroutine titulo
        print*," _____   _   _   _____   _____       ___       ___  ___   _   __   _       ___   _____  "
        print*,"|  _  \ | | | | /  ___/ /  ___|     /   |     /   |/   | | | |  \ | |     /   | /  ___/ "
        print*,"| |_| | | | | | | |___  | |        / /| |    / /|   /| | | | |   \| |    / /| | | |___  "
        print*,"|  _  ( | | | | \___  \ | |       / /_| |   / / |__/ | | | | | |\   |   / /_| | \___  \ "
        print*,"| |_| | | |_| |  ___| | | |___   / /  | |  / /       | | | | | | \  |  / /  | |  ___| | "
        print*,"|_____/ \_____/ /_____/ \_____| /_/   |_| /_/        |_| |_| |_|  \_| /_/   |_| /_____/ "
        print*," By  Adrian Astudillo"
        print*," "
        print*," "
    end subroutine

    subroutine dificultad(dmen)
        integer,intent(out)::dmen
        integer::dif
        dmen=0

        do while (dmen==0)


        print*,"====SELECCIONE LA DIFICULTAD==="
        print*,"====       1:FACIL          ==="
        print*,"====       2:DIFICIL        ==="
        print*,"==============================="
        print"(A,$)"," ====       DIFICULTAD:"
            read*, dif

            if (dif==1) then
                dmen=8
            elseif (dif==2) then
                dmen=16
            end if
        enddo

    end subroutine

    subroutine tablero(set,dmen)
        character(1)::set(:,:)
        integer::i,dmen
         if (dmen==8) then
                print*,"     1   2   3   4   5   6   7   8  "
            do i=1,dmen

                print*,"   +   +   +   +   +   +   +   +   +"
                print"(I3,A,A2,7A4)",i," |",set(i,:)


            end do
                print*,"   +   +   +   +   +   +   +   +   +"
        end if
         if (dmen==16) then
                print*,"     1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  "
            do i=1,dmen

                print*,"   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +"
                print"(I3,A,A2,15A4)",i," |",set(i,:)


            end do
                print*,"   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +"


        end if
    end subroutine

    subroutine generar_bombas(cellstatus,dmen)
        integer::cellstatus(:,:)
        integer::i,j,dmen,timearray(3),k
        real::rndm

        call itime(timeArray)
      k = rand ( timeArray(1)+timeArray(2)+timeArray(3) )

        do i=1,dmen
            do j=1,dmen


            rndm=(floor(100.*(rand())))

                if((rndm)<(10.*dmen/7.3)) then
                    cellstatus(i,j)=9
                else
                    cellstatus(i,j)=0
                end if

            end do
        end do

    end subroutine

    subroutine tablero_cell(cellstatus,dmen)
        integer::cellstatus(:,:)
        integer::i,dmen
         if (dmen==8) then
                print*,"     1   2   3   4   5   6   7   8  "
            do i=1,dmen

                print*,"   +   +   +   +   +   +   +   +   +"
                print"(I3,A,I2,7I4)",i," |",cellstatus(i,:)


            end do
                print*,"   +   +   +   +   +   +   +   +   +"
            end if
         if (dmen==16) then
                print*,"     1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  "
            do i=1,dmen

                print*,"   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +"
                print"(I3,A,I2,15I4)",i," |",cellstatus(i,:)


            end do
                print*,"   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +"

        end if
    end subroutine

    subroutine generar_casillas(cellstatus,dmen)
        integer::cellstatus(:,:)
        integer::i,j,dmen

        do i=2,dmen-1
            do j=2,dmen-1
                if (cellstatus(i,j)>=9) then
                    cellstatus(i+1,j)=(cellstatus(i+1,j))+1
                    cellstatus(i-1,j)=(cellstatus(i-1,j))+1
                    cellstatus(i,j+1)=(cellstatus(i,j+1))+1
                    cellstatus(i,j-1)=(cellstatus(i,j-1))+1
                    cellstatus(i+1,j+1)=(cellstatus(i+1,j+1))+1
                    cellstatus(i+1,j-1)=(cellstatus(i+1,j-1))+1
                    cellstatus(i-1,j+1)=(cellstatus(i-1,j+1))+1
                    cellstatus(i-1,j-1)=(cellstatus(i-1,j-1))+1
                end if
            end do
        end do

        do i=1,dmen
            do j=1,dmen
                if (cellstatus(i,j)>9) then
                    cellstatus(i,j)=9
                end if
            enddo
        enddo

        do i=2,dmen-1
            j=1
                if (cellstatus(i,j)>=9) then
                    cellstatus(i+1,j)=(cellstatus(i+1,j))+1
                    cellstatus(i-1,j)=(cellstatus(i-1,j))+1
                    cellstatus(i,j+1)=(cellstatus(i,j+1))+1
                    cellstatus(i+1,j+1)=(cellstatus(i+1,j+1))+1
                    cellstatus(i-1,j+1)=(cellstatus(i-1,j+1))+1
                end if
        end do

        do i=1,dmen
            do j=1,dmen
                if (cellstatus(i,j)>9) then
                    cellstatus(i,j)=9
                end if
            enddo
        enddo

        do i=2,dmen-1
            j=dmen
                if (cellstatus(i,j)>=9) then
                    cellstatus(i+1,j)=(cellstatus(i+1,j))+1
                    cellstatus(i-1,j)=(cellstatus(i-1,j))+1
                    cellstatus(i,j-1)=(cellstatus(i,j-1))+1
                    cellstatus(i+1,j-1)=(cellstatus(i+1,j-1))+1
                    cellstatus(i-1,j-1)=(cellstatus(i-1,j-1))+1
                end if
        end do
        do i=1,dmen
            do j=1,dmen
                if (cellstatus(i,j)>9) then
                    cellstatus(i,j)=9
                end if
            enddo
        enddo
        do j=2,dmen-1
            i=1
                if (cellstatus(i,j)>=9) then
                    cellstatus(i+1,j)=(cellstatus(i+1,j))+1
                    cellstatus(i,j+1)=(cellstatus(i,j+1))+1
                    cellstatus(i,j-1)=(cellstatus(i,j-1))+1
                    cellstatus(i+1,j+1)=(cellstatus(i+1,j+1))+1
                    cellstatus(i+1,j-1)=(cellstatus(i+1,j-1))+1
                end if
        end do
        do i=1,dmen
            do j=1,dmen
                if (cellstatus(i,j)>9) then
                    cellstatus(i,j)=9
                end if
            enddo
        enddo
        do j=2,dmen-1
            i=dmen
                if (cellstatus(i,j)>=9) then
                    cellstatus(i-1,j)=(cellstatus(i-1,j))+1
                    cellstatus(i,j+1)=(cellstatus(i,j+1))+1
                    cellstatus(i,j-1)=(cellstatus(i,j-1))+1
                    cellstatus(i-1,j+1)=(cellstatus(i-1,j+1))+1
                    cellstatus(i-1,j-1)=(cellstatus(i-1,j-1))+1
                end if
        end do
        do i=1,dmen
            do j=1,dmen
                if (cellstatus(i,j)>9) then
                    cellstatus(i,j)=9
                end if
            enddo
        enddo

        if (cellstatus(1,1)>=9) then
            i=1
            j=1
            cellstatus(i+1,j+1)=(cellstatus(i+1,j+1))+1
            cellstatus(i,j+1)=(cellstatus(i,j+1))+1
            cellstatus(i+1,j)=(cellstatus(i+1,j))+1
        end if

        do i=1,dmen
            do j=1,dmen
                if (cellstatus(i,j)>9) then
                    cellstatus(i,j)=9
                end if
            enddo
        enddo

        if (cellstatus(1,dmen)>=9) then
            i=1
            j=dmen
            cellstatus(i+1,j-1)=(cellstatus(i+1,j-1))+1
            cellstatus(i,j-1)=(cellstatus(i,j-1))+1
            cellstatus(i+1,j)=(cellstatus(i+1,j))+1
        end if

        do i=1,dmen
            do j=1,dmen
                if (cellstatus(i,j)>9) then
                    cellstatus(i,j)=9
                end if
            enddo
        enddo

        if (cellstatus(dmen,1)>=9) then
            i=dmen
            j=1
            cellstatus(i-1,j+1)=(cellstatus(i-1,j+1))+1
            cellstatus(i,j+1)=(cellstatus(i,j+1))+1
            cellstatus(i-1,j)=(cellstatus(i-1,j))+1
        end if

        do i=1,dmen
            do j=1,dmen
                if (cellstatus(i,j)>9) then
                    cellstatus(i,j)=9
                end if
            enddo
        enddo

        if (cellstatus(dmen,dmen)>=9) then
            i=dmen
            j=dmen
            cellstatus(i-1,j-1)=(cellstatus(i-1,j-1))+1
            cellstatus(i,j-1)=(cellstatus(i,j-1))+1
            cellstatus(i-1,j)=(cellstatus(i-1,j))+1
        end if

        do i=1,dmen
            do j=1,dmen
                if (cellstatus(i,j)>9) then
                    cellstatus(i,j)=9
                end if
            enddo
        enddo

    end subroutine

    subroutine eleccion(choice)
        integer::choice,h
        choice=0
        do while (choice==0)
            print*,"==============="
            print*,"1: Descubrir"
            print*,"2: Marcar/Desmarcar"
            print*,"==============="
            print"(A,$)","Elegir:"
                read*,h
            if (h==1) then
                choice=1
            elseif (h==2) then
                choice=2
            elseif (h==1998) then
                choice=1998
            elseif (h==0987) then
                choice=0987
            end if

            end do


    end subroutine

    subroutine descubrir(set,cellstatus,dmen,gameover)
        integer::xpos,ypos,i,j,dmen,gameover
        real::n
        character(1)::set(:,:)
        integer::cellstatus(:,:)
        xpos=0
        ypos=0
    do while((xpos<1).OR.(xpos>dmen).OR.(ypos<1).OR.(ypos>dmen))
        print*,"==========="
        print"(A,$)","Columna:"
                read*,ypos
        print"(A,$)","Fila:"
                read*,xpos
     enddo


                        if (cellstatus(xpos,ypos)==0) then
                    set(xpos,ypos)=" "

            do i=1,dmen*400
                call random_number(n)
                if ((floor(10.*n))<=2.5) then
                    if (cellstatus(xpos+1,ypos)==0) then
                        if((xpos+1)<=dmen) then
                        xpos=xpos+1
                        set(xpos,ypos)=" "
                        endif
                    end if
                else if ((floor(10.*n))<=5.) then
                    if (cellstatus(xpos-1,ypos)==0) then
                        if((xpos-1)>=1) then
                        xpos=xpos-1
                        set(xpos,ypos)=" "
                        endif
                    end if
                else if ((floor(10.*n))<=7.5) then
                    if (cellstatus(xpos,ypos+1)==0) then
                        if((ypos+1)<=dmen) then
                        ypos=ypos+1
                        set(xpos,ypos)=" "
                        endif
                    end if
                else if ((floor(10.*n))<=10.) then
                    if (cellstatus(xpos,ypos-1)==0) then
                        if((ypos-1)>=1) then
                        ypos=ypos-1
                        set(xpos,ypos)=" "
                        endif
                    end if
                endif
            enddo


            do i=1,dmen
                do j=1,dmen
                    if (cellstatus(i,j)/=0) then
                        !if ((set(i+1,j)==" ").OR.(set(i,j+1)==" ").OR.(set(i-1,j)==" ").OR.(set(i,j-1)==" ").OR.&
                        !(set(i+1,j+1)==" ").OR.(set(i-1,j+1)==" ").OR.(set(i+1,j-1)==" ").OR.(set(i-1,j-1)==" ")) then
                        if (((set(i+1,j)==" ").AND.(i+1<=dmen)).OR.&
                        ((set(i,j+1)==" ").AND.(j+1<=dmen)).OR.&
                        ((set(i-1,j)==" ").AND.(i-1>=1)).OR.&
                        ((set(i,j-1)==" ").AND.(j-1>=1)).OR.&
                        ((set(i+1,j+1)==" ").AND.(i+1<=dmen).AND.(j+1<=dmen)).OR.&
                        ((set(i+1,j-1)==" ").AND.(i+1<=dmen).AND.(j-1>=1)).OR.&
                        ((set(i-1,j+1)==" ").AND.(i-1>=1).AND.(j+1<=dmen)).OR.&
                        ((set(i-1,j-1)==" ").AND.(i-1>=1).AND.(j-1>=1)))then
                             if (cellstatus(i,j)==1) then
                                set(i,j)="1"
                            elseif (cellstatus(i,j)==2) then
                                set(i,j)="2"
                            elseif (cellstatus(i,j)==3) then
                                set(i,j)="3"
                            elseif (cellstatus(i,j)==4) then
                                set(i,j)="4"
                            elseif (cellstatus(i,j)==5) then
                                set(i,j)="5"
                            elseif (cellstatus(i,j)==6) then
                                set(i,j)="6"
                            elseif (cellstatus(i,j)==7) then
                                set(i,j)="7"
                            elseif (cellstatus(i,j)==8) then
                                set(i,j)="8"
                            end if
                        end if
                    end if
                end do
            end do


                elseif (cellstatus(xpos,ypos)==1) then
                    set(xpos,ypos)="1"
                elseif (cellstatus(xpos,ypos)==2) then
                    set(xpos,ypos)="2"
                elseif (cellstatus(xpos,ypos)==3) then
                    set(xpos,ypos)="3"
                elseif (cellstatus(xpos,ypos)==4) then
                    set(xpos,ypos)="4"
                elseif (cellstatus(xpos,ypos)==5) then
                    set(xpos,ypos)="5"
                elseif (cellstatus(xpos,ypos)==6) then
                    set(xpos,ypos)="6"
                elseif (cellstatus(xpos,ypos)==7) then
                    set(xpos,ypos)="7"
                elseif (cellstatus(xpos,ypos)==8) then
                    set(xpos,ypos)="8"
                elseif (cellstatus(xpos,ypos)==9) then
                    set(xpos,ypos)="X"
                    gameover=1
                end if


    end subroutine

    subroutine game_over(turn)
        integer::turn
print*,' _____   ___  ___  ___ _____   _____  _   _ ___________ '
print*,'|  __ \ / _ \ |  \/  ||  ___| |  _  || | | |  ___| ___ \'
print*,'| |  \// /_\ \| .  . || |__   | | | || | | | |__ | |_/ /'
print*,'| | __ |  _  || |\/| ||  __|  | | | || | | |  __||    / '
print*,'| |_\ \| | | || |  | || |___  \ \_/ /\ \_/ / |___| |\ \ '
print*,' \____/\_| |_/\_|  |_/\____/   \___/  \___/\____/\_| \_|'
print"(A,I3)","                     TURNO",turn

    end subroutine

    subroutine desvelar_todo(set,cellstatus,dmen)
            integer::xpos,ypos,seguir,i,j,dmen
        character(1)::set(:,:)
        integer::cellstatus(:,:)
    do xpos=1,dmen
        do ypos=1,dmen
                if (cellstatus(xpos,ypos)==0) then
                    set(xpos,ypos)=" "
                    seguir=1
                    do i=1,dmen
                        do j=1,dmen

                            if (cellstatus(i,j)==0) then
                                set(i,j)=" "
                            end if

                        end do
                    end do
                elseif (cellstatus(xpos,ypos)==1) then
                    set(xpos,ypos)="1"
                elseif (cellstatus(xpos,ypos)==2) then
                    set(xpos,ypos)="2"
                elseif (cellstatus(xpos,ypos)==3) then
                    set(xpos,ypos)="3"
                elseif (cellstatus(xpos,ypos)==4) then
                    set(xpos,ypos)="4"
                elseif (cellstatus(xpos,ypos)==5) then
                    set(xpos,ypos)="5"
                elseif (cellstatus(xpos,ypos)==6) then
                    set(xpos,ypos)="6"
                elseif (cellstatus(xpos,ypos)==7) then
                    set(xpos,ypos)="7"
                elseif (cellstatus(xpos,ypos)==8) then
                    set(xpos,ypos)="8"
                elseif (cellstatus(xpos,ypos)==9) then
                    set(xpos,ypos)="X"

                end if
            enddo
        enddo


    end subroutine

    subroutine desvelar_bombas(set,cellstatus,dmen)
            integer::xpos,ypos,dmen
        character(1)::set(:,:)
        integer::cellstatus(:,:)
    do xpos=1,dmen
        do ypos=1,dmen

                if (cellstatus(xpos,ypos)==9) then
                    set(xpos,ypos)="X"

                end if
            enddo
        enddo


    end subroutine

    subroutine marcar(flags,set,cellstatus,dmen,wingame,bombnum)
        integer::dmen,wingame,xpos=0,ypos=0,i,j,flagsnum=0,bombnum,cont,marca
        integer::flags(:,:),cellstatus(:,:)
        character(1)::set(:,:)
        xpos=0
        ypos=0
    do while((xpos<1).OR.(xpos>dmen).OR.(ypos<1).OR.(ypos>dmen))
        print*,"==========="
        print"(A,$)","Columna:"
                read*,ypos
        print"(A,$)","Fila:"
                read*,xpos
     enddo

        if (flags(xpos,ypos)==0) then
            flagsnum=flagsnum+1
            set(xpos,ypos)="P"
            marca=1

        else
            flagsnum=flagsnum-1
            set(xpos,ypos)=char(219)
            marca=0
        end if

        flags(xpos,ypos)=marca

        if (bombnum==flagsnum) then
                cont=0
                do i=1,dmen
                do j=1,dmen
                    if ((flags(i,j)==1).AND.(cellstatus(i,j)==9)) then
                        cont=cont+1
                    end if
                end do
                end do
        end if

        if (cont==bombnum) then
            wingame=1
        end if




    end subroutine

    subroutine contarbombas(cellstatus,bombnum,dmen)
        integer::bombnum,i,j,dmen
        integer::cellstatus(:,:)
        bombnum=0
            do i=1,dmen
            do j=1,dmen
                if(cellstatus(i,j)==9) then
                    bombnum=bombnum+1
                end if
            end do
        end do

    end subroutine

    subroutine endrestart(restart)
        integer::restart
        character(1)::input
        print*,"¿Jugar de nuevo? (Y/N)"
            read*,input
                if ((input=="n").OR.(input=="N")) then
                    restart=0
                end if

    end subroutine

    subroutine winprint(turn,score)
        integer::turn,score
  print*,"       _    _           _____      _____          _   _          _____   ____  "
  print*,"      | |  | |   /\    / ____|    / ____|   /\   | \ | |   /\   |  __ \ / __ \ "
  print*,"      | |__| |  /  \  | (___     | |  __   /  \  |  \| |  /  \  | |  | | |  | |"
  print*,"      |  __  | / /\ \  \___ \    | | |_ | / /\ \ | . ` | / /\ \ | |  | | |  | |"
  print*,"      | |  | |/ ____ \ ____) |   | |__| |/ ____ \| |\  |/ ____ \| |__| | |__| |"
  print*,"      |_|  |_/_/    \_\_____/     \_____/_/    \_\_| \_/_/    \_\_____/ \____/ "
  print"(A,I3,A,I5)","                              TURNO:",turn,"       SCORE:",score
    end subroutine

    subroutine descubrir_primera(set,cellstatus,dmen,xpos,ypos)
        integer::xpos,ypos,i,j,dmen
        real::n
        character(1)::set(:,:)
        integer::cellstatus(:,:)

        if (cellstatus(xpos,ypos)==0) then
                    set(xpos,ypos)=" "

            do i=1,dmen*400
                call random_number(n)
                if ((floor(10.*n))<=2.5) then
                    if (cellstatus(xpos+1,ypos)==0) then
                        if((xpos+1)<=dmen) then
                        xpos=xpos+1
                        set(xpos,ypos)=" "
                        endif
                    end if
                else if ((floor(10.*n))<=5.) then
                    if (cellstatus(xpos-1,ypos)==0) then
                        if((xpos-1)>=1) then
                        xpos=xpos-1
                        set(xpos,ypos)=" "
                        endif
                    end if
                else if ((floor(10.*n))<=7.5) then
                    if (cellstatus(xpos,ypos+1)==0) then
                        if((ypos+1)<=dmen) then
                        ypos=ypos+1
                        set(xpos,ypos)=" "
                        endif
                    end if
                else if ((floor(10.*n))<=10.) then
                    if (cellstatus(xpos,ypos-1)==0) then
                        if((ypos-1)>=1) then
                        ypos=ypos-1
                        set(xpos,ypos)=" "
                        endif
                    end if
                endif
            enddo

            do i=1,dmen
                do j=1,dmen
                    if (cellstatus(i,j)/=0) then
                        !if ((set(i+1,j)==" ").OR.(set(i,j+1)==" ").OR.(set(i-1,j)==" ").OR.(set(i,j-1)==" ").OR.&
                        !(set(i+1,j+1)==" ").OR.(set(i-1,j+1)==" ").OR.(set(i+1,j-1)==" ").OR.(set(i-1,j-1)==" ")) then
                        if (((set(i+1,j)==" ").AND.(i+1<=dmen)).OR.&
                        ((set(i,j+1)==" ").AND.(j+1<=dmen)).OR.&
                        ((set(i-1,j)==" ").AND.(i-1>=1)).OR.&
                        ((set(i,j-1)==" ").AND.(j-1>=1)).OR.&
                        ((set(i+1,j+1)==" ").AND.(i+1<=dmen).AND.(j+1<=dmen)).OR.&
                        ((set(i+1,j-1)==" ").AND.(i+1<=dmen).AND.(j-1>=1)).OR.&
                        ((set(i-1,j+1)==" ").AND.(i-1>=1).AND.(j+1<=dmen)).OR.&
                        ((set(i-1,j-1)==" ").AND.(i-1>=1).AND.(j-1>=1)))then
                             if (cellstatus(i,j)==1) then
                                set(i,j)="1"
                            elseif (cellstatus(i,j)==2) then
                                set(i,j)="2"
                            elseif (cellstatus(i,j)==3) then
                                set(i,j)="3"
                            elseif (cellstatus(i,j)==4) then
                                set(i,j)="4"
                            elseif (cellstatus(i,j)==5) then
                                set(i,j)="5"
                            elseif (cellstatus(i,j)==6) then
                                set(i,j)="6"
                            elseif (cellstatus(i,j)==7) then
                                set(i,j)="7"
                            elseif (cellstatus(i,j)==8) then
                                set(i,j)="8"
                            end if
                        end if
                    end if
                end do
            end do





                elseif (cellstatus(xpos,ypos)==1) then
                    set(xpos,ypos)="1"
                elseif (cellstatus(xpos,ypos)==2) then
                    set(xpos,ypos)="2"
                elseif (cellstatus(xpos,ypos)==3) then
                    set(xpos,ypos)="3"
                elseif (cellstatus(xpos,ypos)==4) then
                    set(xpos,ypos)="4"
                elseif (cellstatus(xpos,ypos)==5) then
                    set(xpos,ypos)="5"
                elseif (cellstatus(xpos,ypos)==6) then
                    set(xpos,ypos)="6"
                elseif (cellstatus(xpos,ypos)==7) then
                    set(xpos,ypos)="7"
                elseif (cellstatus(xpos,ypos)==8) then
                    set(xpos,ypos)="8"

                end if


    end subroutine

end module
