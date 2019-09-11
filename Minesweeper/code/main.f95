program buscaminas
    use modulos
    implicit none
    integer::dmen,choice=0,gameover=0,wingame=0,bombnum,restart=1,firstturn,xpos1=0,ypos1=0,turn,score,highscore


!=======MATRICES=============================================
    character(1),allocatable::set(:,:)
    integer,allocatable::cellstatus(:,:),flags(:,:)
!============================================================



    do while(restart==1)

        call titulo                 !Print del rótulo de inicio
        call dificultad(dmen)       !Elección de la dificultad y asignacion de dimensiones en funcion de esta
        allocate(set(dmen,dmen));allocate(cellstatus(dmen,dmen));allocate(flags(dmen,dmen))

        turn=0
        gameover=0
        wingame=0                               !Reset de variables
        choice=0
        firstturn=1
        xpos1=0
        ypos1=0

        if (dmen==8) then
            open(12,file='hs8.txt')
            read(12,*),highscore
        elseif (dmen==16) then                      !Lectura del Highscore previo
            open(12,file='hs16.txt')
            read(12,*),highscore
        end if

        call generar_bombas(cellstatus,dmen)        !Generación aleatoria de bombas

        set=char(219)                               !Creación del tablero con casillas bloqueadas
        flags=0                                     !Inicializacion de la matriz de banderas sin ninguna bandera




    do while(gameover==0)                           !Sucesión de la partida

        turn=turn+1
        call espaciar(4)
        print"(A60,I3)","TURNO:",turn
        call tablero(set,dmen)                      !Elementos fijos en pantalla
        call espaciar(1)

        if (turn==1) then                           !Excepciones durante el primer turno

            print*,"==========================="
            print*,"MARCA todas las bombas"
            print*,"con una bandera para ganar"         !Tutorial
            print*,"==========================="
            call espaciar(1)

        end if

        call eleccion(choice)                       !El jugador decide qué hacer

            if (choice==1) then

                    if (turn==1) then                  !Excepciones durante el primer turno

                    do while((xpos1<1).OR.(xpos1>dmen).OR.(ypos1<1).OR.(ypos1>dmen))  !Desbloqueo de casilla con "comodín" (No se puede destapar una bomba en el primer turno)
                        print*,"==========="
                        print"(A,$)","Columna:"
                            read*,ypos1
                        print"(A,$)","Fila:"
                            read*,xpos1
                    enddo
                        cellstatus(xpos1,ypos1)=0
                        call generar_casillas(cellstatus,dmen)          !Asigacion de los numeros para cada casilla en funcion de las bombas adyacentes
                        call descubrir_primera(set,cellstatus,dmen,xpos1,ypos1)     !Primer desbloqueo de casilla que elimina cualquier bomba que se destape
                        call contarbombas(cellstatus,bombnum,dmen)              !El numero de bombas se guarda en una variable
                        score=dmen*100+12*bombnum                                  !Se inicializa la puntuacion de la que se ira restando, en funcion de la dificultad
                        firstturn=0                                                !y el numero de bombas que se hayan generado

                endif

                if (turn/=1) then  !transcurso normal de turnos

                    call descubrir(set,cellstatus,dmen,gameover)
                    score=score-16                              !cada turno que le lleve al jugador terminar la partida resta puntuacion

                endif


            elseif (choice==2) then                 !el jugador elige poner una bandera

                if (turn==1) then                       !si por algun motivo decide poner una bandera en el primer turno, pierde el comodín, pues se generan las casillas igualmente
                    call generar_bombas(cellstatus,dmen)
                    call generar_casillas(cellstatus,dmen)
                end if

                call marcar(flags,set,cellstatus,dmen,wingame,bombnum)

            elseif (choice==1998) then

                call tablero_cell(cellstatus,dmen)
                call espaciar(5)
                                                                !Trucos del desarrollador
            elseif (choice==0987) then

                wingame=1

            end if

        if (wingame==1) exit

    enddo

    do while (gameover==1)          !Proceso de perder la partida

        call espaciar(50)
        call desvelar_bombas(set,cellstatus,dmen)
        call game_over(turn)
        call espaciar(2)
        call tablero(set,dmen)
        call espaciar (2)
        call endrestart(restart)
        gameover=0

        deallocate(set);deallocate(cellstatus);deallocate(flags)
        close(12)
    enddo

    do while (wingame==1)           !Proceso de ganar la partida

        call espaciar(50)
        call winprint(turn,score)

        if (score>highscore) then           !Actualización del highscore si procede

            close(12,status='delete')
            print*,"                              ========== NUEVO RECORD ==========="
            highscore=score

            if (dmen==8) then
                open(13,file='hs8.txt')
                write(13,*),highscore
                close(13)
            elseif (dmen==16) then
                open(13,file='hs16.txt')
                write(13,*),highscore
                close(13)
            endif

        end if

        close(12)
        call espaciar(1)
        call tablero(set,dmen)
        call espaciar(1)
        call endrestart(restart)            !Pregunta si el jugador quiere jugar de nuevo


        wingame=0
        deallocate(set);deallocate(cellstatus);deallocate(flags)                !Se limpian las dimensiones asignadas

    end do

    end do
                                                            !FIN DEL PROGRAMA

end program
