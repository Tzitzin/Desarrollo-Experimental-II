!================================================================================|
! Thursday, January 31, 2019  
! Movimiento arbitrario de las N partículas para un sistema de discos duros ale-
! jados de las fronteras (en bulto).
!================================================================================|

PROGRAM CA_2D
    IMPLICIT NONE
        REAL    :: AA, BOXL, RCUT, DENS
        INTEGER :: N
        REAL, ALLOCATABLE   :: X(:), Y(:)
        REAL, PARAMETER     :: PI = 3.1415927 


! Lectura de datos de entrada ---------------------------------------------------|
    WRITE(*,*) 'Introduzca el numero de particulas (N)'
        READ (*,*) N
    WRITE(*,*) 'Introduzca la densidad reducida (DENS)'
        READ (*,*) DENS

! Allocate positions ------------------------------------------------------------|
        allocate(X(N))
        allocate(Y(N))

! Cálculos preeliminares --------------------------------------------------------|
    AA=1.0/2.0
    BOXL=((1.0*N)/DENS)**AA
    RCUT=BOXL/2.0

! Escribir datos de entrada en pantalla -----------------------------------------|
    WRITE(*,*) '-----------------------------------------------------------------'
    WRITE(*,*) 'Numero de particulas          N =',  N
    WRITE(*,*) 'La densidad critica es     DENS =',  DENS
    WRITE(*,*) 'Longitud de la celda         L* =',  BOXL
    WRITE(*,*) 'El rango de la celda es         =', '[', -BOXL/2, ',' ,BOXL/2, ']' 
    WRITE(*,*) '-----------------------------------------------------------------'

! Configuración inicial aleatoria sin traslapes: ejecución en subrutina ---------|
CALL CONFIGINI(N, BOXL, X, Y)

! Información
WRITE(*,*) 'El programa ha terminado de manera exitosa. Se ha generado el archivo'
WRITE(*,*) '"confi_in.dat" con la informacion calculada, cuyo formato es:'
WRITE(*,*) '-----------------------'
WRITE(*,*) '   I   |   X   |   Y   '
WRITE(*,*) '-----------------------'

! Finalizar programa -----------------------------------------------------------|
END PROGRAM CA_2D
