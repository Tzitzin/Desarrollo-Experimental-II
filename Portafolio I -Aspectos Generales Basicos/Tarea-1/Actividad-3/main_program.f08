!=========================================================================|
! Saturday, January 19, 2019  
! Portafolio 1                                     
! Tarea 1 - Actividad 3                                            
! Descripción: Este programa genera un arreglo de N**2 partículas locali-
!              zadas en una celda cuadrada de lado L tal que el origen 
!              (0,0) se ubica en (L/2, L/2).
! Por WyL Félix
!=========================================================================|
    ! Descripción de variables
    ! i, j, y k : contadores.
    ! L, N      : longitud del lado de la celda y número de partículas.
    ! X(N), Y(N): coordenada (X o Y) de la N-ésima partícula.
    ! dx, dy    : incluyen información acerca de la separación entre par-
    !             tículas.

PROGRAM particle_2D
    IMPLICIT NONE
        REAL :: L, dx, dy
        INTEGER :: N, i, j, k
        REAL, ALLOCATABLE :: X(:), Y(:)

!-------------------- Output data into a file --------------------------S1|
        open (unit = 10, file = "data_file_2D.dat", status = 'unknown')
!-----------------------------------------------------------------------E1|

WRITE(*,*) 'Introduzca el numero de particulas'
READ (*,*) N
WRITE(*,*) 'Introduzca la longitud del lado de la celda '
READ (*,*) L

!-------------------- Preliminaries ------------------------------------S2|
    ! Asignar dimensión de los vectores X(N), Y(N): 1xN
        allocate(X(N))
        allocate(Y(N))
    ! Separación (dx para N par y dy para N impar)
        dx=L/N
        dy=L/(N-1)
    ! Un contador
        k=N-1
!-----------------------------------------------------------------------E2|

!-----------------------------------------------------------------------S3|
! If N is EVEN    
!-----------------------------------------------------------------------E3|
IF(MOD(N,2)==0) THEN
    ! Coordenadas X(N)
        DO i=0, k
            X(i+1)=(1/2.0-(N/2.0-i))*dx
    ! Coordendas Y(N)
            Y(i+1)=(1/2.0-(N/2.0-i))*dx
        END DO
            WRITE(*,*) 'La configuracion se ha almacenado en el archivo'
            WRITE(*,*) "'data_file_2D.dat'"

!-----------------------------------------------------------------------S4|
! If N is ODD
!-----------------------------------------------------------------------E4|
    ELSE
    ! Coordenadas X(N)
        DO i=0, k
            X(i+1)=(i-(k/2.0))*dy
    ! Coordenadas Y(N)
            Y(i+1)=(i-(k/2.0))*dy
        END DO
            WRITE(*,*) 'La configuracion se ha almacenado en el archivo'
            WRITE(*,*) "'data_file_2D.dat'"
END IF

!Escribir arreglo dos-dimensional en el archivo "data_file_2D.dat"
  DO i=1, N
    DO j=1, N
        WRITE(10,*) X(i), Y(j)
    END DO
  END DO

!Cerrar y terminar
CLOSE(10)
END PROGRAM particle_2D