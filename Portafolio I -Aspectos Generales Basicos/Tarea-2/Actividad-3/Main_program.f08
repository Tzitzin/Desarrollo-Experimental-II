!================================================================================|
! Thursday, January 31, 2019  
! Movimiento arbitrario de las N partículas para un sistema de discos du-
! ros alejados de las fronteras (en bulto).
!================================================================================|

PROGRAM CODIGO4
    IMPLICIT NONE
        REAL    :: AA, BOXL, PHIT, RCUT, N_STAR
        INTEGER :: N
        REAL, ALLOCATABLE   :: X(:), Y(:)
        REAL, PARAMETER     :: PI = 3.1415927 


! Lectura de datos de entrada ---------------------------------------------------|
    WRITE(*,*) 'Introduzca el numero de particulas (N)'
        READ (*,*) N
    WRITE(*,*) 'Introduzca la fraccion en area (PHI)'
        READ (*,*) PHIT

! Allocate positions ------------------------------------------------------------|
        allocate(X(N))
        allocate(Y(N))

! Cálculos preeliminares --------------------------------------------------------|
    AA=1.0/2.0
    N_STAR=4*PHIT/PI
    BOXL=((1.0*N)/N_STAR)**AA
    RCUT=BOXL/2.0

! Escribir datos de entrada en pantalla -----------------------------------------|
    WRITE(*,*) '-----------------------------------------------------------------'
    WRITE(*,*) 'Numero de particulas          N =',  N
    WRITE(*,*) 'La concentracion en area es PHI =',  PHIT
    WRITE(*,*) 'Longitud de la celda         L* =',  BOXL
    WRITE(*,*) 'El rango de la celda es         =', '[', -BOXL/2, ',' ,BOXL/2, ']' 
    WRITE(*,*) '-----------------------------------------------------------------'

! Configuración inicial aleatoria sin traslapes: ejecución en subrutina ---------|
CALL CONFIGINI(BOXL, N)

! Finalizar programa -----------------------------------------------------------|
END PROGRAM CODIGO4