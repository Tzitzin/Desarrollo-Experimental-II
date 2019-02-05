SUBROUTINE confi_in_CR(N, dx, dy)
IMPLICIT NONE
        REAL, INTENT(IN)        :: dx, dy
        INTEGER, INTENT(IN)     :: N
        INTEGER                 :: k, i, j, t
        REAL, ALLOCATABLE       :: X(:), Y(:), Z(:)


!-------------------- Output data into a file ------------------------------|
        open (unit = 10, file = "data_file.dat", status = 'unknown')
!---------------------------------------------------------------------------|

! Asignar dimensión de los vectores X(N), Y(N) y Z(N):
        allocate(X(N))
        allocate(Y(N))
        allocate(Z(N))

! Un contador
    k=N-1

!---------------------------------------------------------------------------|
! If N is EVEN    
!---------------------------------------------------------------------------|

IF(MOD(N,2)==0) THEN
    ! Coordenadas X(N)
        DO i=0, k
            X(i+1)=(1/2.0-(N/2.0-i))*dx
    ! Coordenadas Y(N)
            Y(i+1)=(1/2.0-(N/2.0-i))*dx
    ! Coordenadas Z(N)
            Z(i+1)=(1/2.0-(N/2.0-i))*dx
        END DO
WRITE(*,*) "La configuracion se ha almacenado en el archivo 'data_file.dat'"

!---------------------------------------------------------------------------|
! If N is ODD
!---------------------------------------------------------------------------|

    ELSE
    ! Coordenadas X(N)
        DO i=0, k
            X(i+1)=(i-(k/2.0))*dy
    ! Coordenadas Y(N)
            Y(i+1)=(i-(k/2.0))*dy
    ! Coordenadas Z(N)
            Z(i+1)=(i-(k/2.0))*dy
        END DO
WRITE(*,*) "La configuracion se ha almacenado en el archivo 'data_file.dat'"

END IF

!Escribir arreglo tres-dimensional en el archivo "data_file.dat" -----------|
    DO i=1, N
        DO j=1, N
            DO t=1, N
                WRITE(10,*) X(i), Y(j), Z(t)
            END DO 
        END DO
    END DO

! Cierres ------------------------------------------------------------------|
CLOSE(10)


END SUBROUTINE confi_in_CR