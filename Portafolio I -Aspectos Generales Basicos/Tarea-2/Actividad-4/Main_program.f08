!=============================================================================|
! Monday, February 04, 2019  
! Portafolio 1                                     
! Tarea 1 - Actividad 3                                            
! Descripción: Este programa genera un arreglo de N**3 partículas locali-
!              zadas en una celda cubica de lado BOXL tal que el origen 
!              (0,0) se ubica en (BOXL/2, BOXL/2).
! Por WyL Félix
!=============================================================================|
    ! Descripción de variables
    ! BOXL, DENS     : Longitud del lado de la celda y densidad reducida.
    ! SIGMA          : Diametro de las particulas. Variable para reducir. 
    ! X(N), Y(N) Z(N): Coordenada (X, Y y Z) de la N-ésima partícula.
    ! dx, dy         : Incluyen información acerca de la separación entre par-
    !                  tículas.

PROGRAM particle_3D
    IMPLICIT NONE
        REAL :: dx, dy, DENS, BOXL, A, SIGMA
        INTEGER :: N
        REAL, ALLOCATABLE :: X(:), Y(:), Z(:)

! Lectura de datos de entrada ------------------------------------------------|        
WRITE(*,*) 'Introduzca el numero de particulas'
    READ (*,*) N
WRITE(*,*) 'Introduzca la densidad reducida'
    READ (*,*) DENS


! Algunos cálculos -----------------------------------------------------------|
    A=1.0/3.0
    BOXL=((1.0*(N**3))/DENS)**A
    SIGMA=1.0

! Escritura de datos en pantalla ---------------------------------------------|
    WRITE(*,*) '--------------------------------------------------------'
    WRITE(*,*) 'El numero total de particulas es N =', N**3
    WRITE(*,*) 'La longitud de la celda es      L* =', BOXL
    WRITE(*,*) 'La densidad reducida es         n* =', DENS
    WRITE(*,*) '--------------------------------------------------------'

!-------------------- Preliminaries ------------------------------------------|
    ! Asignar dimensión de los vectores X(N), Y(N) y Z(N):
        allocate(X(N))
        allocate(Y(N))
        allocate(Z(N))
    ! Separación (dx para N par y dy para N impar)
        dx=BOXL/N
        dy=BOXL/(N-1)
! ----------------------------------------------------------------------------|

! Longitud mínima de la celda ------------------------------------------------|
    IF(dx<=SIGMA) THEN
        WRITE(*,*) 'No se puede generar la configuracion de', N**3, 'par-'
        WRITE(*,*) 'ticulas de diametro', SIGMA
    END IF

Call confi_in_CR(N, dx, dy)



END PROGRAM particle_3D
