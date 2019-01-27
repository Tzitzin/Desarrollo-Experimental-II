!=========================================================================|
! Saturday, January 19, 2019  
! Portafolio 1                                     
! Tarea 1 - Actividad 2                                              
! Descripción: Este programa genera un arreglo de N partículas localizadas
!              sobre un segmento de recta de longitud L tal que el origen
!              se ubica en L/2 y las partículas con etiqueta par se en-
!              cuentran a la derecha de L/2 y las impares a la izquierda.
! Por WyL Félix
!=========================================================================|
    ! Descripción de variables
    ! i         : contador.
    ! L, N      : longitud del lado del segmento de recta y número de par-
    !             tículas.
    ! X(N)      : coordenada X de la N-ésima partícula.
    ! dx        : incluye información acerca de la separación entre par-
    !             tículas.

PROGRAM particle_1D
    IMPLICIT NONE
        REAL :: L, dx
        INTEGER :: i, N
        REAL, ALLOCATABLE :: X(:)

!-------------------- Output data into a file --------------------------S1|
        open (unit = 10, file = "data_file_1D.dat", status = 'unknown')
!-----------------------------------------------------------------------E1|

WRITE(*,*) 'Introduzca el numero de particulas'
READ (*,*) N
WRITE(*,*) 'Introduzca la longitud del segmento de recta'
READ (*,*) L

!-------------------- Preliminaries ------------------------------------S2|
    !Asignar dimensión del vector 1xN
        allocate(X(N))
    !Separación del cero (L/2)
        dx=L/N
!-----------------------------------------------------------------------E2|

!-----------------------------------------------------------------------S3|
! If N is even [the Nth particle is located to the right of L/2 (or zero)]     
!-----------------------------------------------------------------------E3|
IF(MOD(N,2)==0) THEN
        X(1)=-dx/2.0
        X(2)=+dx/2.0
            DO i=1, (N-2)/2
        !Partículas con etiqueta PAR
                X(2*i+2)=X(2)+i*dx
        !Partículas con etiqueta IMPAR
                X(2*i+1)=X(1)-i*dx
            END DO
        WRITE(*,*) 'La configuracion se ha almacenado en el archivo'
        WRITE(*,*) "'data_file_1D.dat'"

!-----------------------------------------------------------------------S4|
! If N is odd [the Nth particle is located to the left of L/2 (or zero)]
!-----------------------------------------------------------------------E4|
    ELSE
!       X(1)=-dx/2.0
        X(1)=0.0
            DO i=1, (N-1)/2
        !Partículas con etiqueta PAR
                X(2*i)=X(1)+i*dx
        !Partículas con etiqueta IMPAR
                X(2*i+1)=X(1)-i*dx
            END DO
        WRITE(*,*) 'La configuracion se ha almacenado en el archivo'
        WRITE(*,*) "'data_file_1D.dat'"
END IF

!Escribir arreglo unidimensional en el archivo "data_file_1D.dat"
  DO i=1, N
     WRITE(10,*) i,  X(i), 0.0
  END DO

!Cerrar y terminar
CLOSE(1)
END PROGRAM particle_1D