!================================================================================|
! Thursday, January 31, 2019  
! Movimiento arbitrario de las N partículas para un sistema de discos du-
! ros alejados de las fronteras (en bulto).
!================================================================================|

PROGRAM CODIGO4
    IMPLICIT NONE
        REAL    :: AA, BOXL, DENS, RCUT
        INTEGER :: N
        REAL, ALLOCATABLE   :: X(:), Y(:), Z(:)
        REAL, PARAMETER     :: PI = 3.1415927 


! Lectura de datos de entrada ---------------------------------------------------|
    WRITE(*,*) 'Introduzca el numero de particulas (N)'
        READ (*,*) N
    WRITE(*,*) 'Introduzca la densidad reducida (n*)'
        READ (*,*) DENS

! Allocate positions ------------------------------------------------------------|
        allocate(X(N))
        allocate(Y(N))
        allocate(Z(N))

! Cálculos preeliminares --------------------------------------------------------|
    AA=1.0/3.0
    BOXL=((1.0*N)/DENS)**AA
    RCUT=BOXL/2.0

! Escribir datos de entrada en pantalla -----------------------------------------|
    WRITE(*,*) '----------------------------------------------------'
    WRITE(*,*) 'Numero de particulas    N =',  N
    WRITE(*,*) 'La densidad reducida    n*=',  DENS
    WRITE(*,*) 'Longitud de la celda    L*=',  BOXL
    WRITE(*,*) '----------------------------------------------------'

! Configuración inicial aleatoria sin traslapes: ejecución en subrutina ---------|
CALL CONFIGINI(BOXL, N)

! Finalizar programa -----------------------------------------------------------|
END PROGRAM CODIGO4