!================================================================================|
! Thursday, January 31, 2019  
! Movimiento arbitrario de las N partículas para un sistema de esferas duras ale-
! jadas de las fronteras (en bulto).
!================================================================================|

PROGRAM CA_3D
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
CALL CONFIGINI(N, BOXL, X, Y, Z)

! Información

WRITE(*,*) 'El programa ha terminado de manera exitosa. Se ha generado el archivo'
WRITE(*,*) '"confi_in.dat" con la informacion calculada, cuyo formato es:'
WRITE(*,*) '-------------------------------'
WRITE(*,*) '   I   |   X   |   Y   |   Z   '
WRITE(*,*) '-------------------------------'

! Finalizar programa ------------------------------------------------------------|
END PROGRAM CA_3D
