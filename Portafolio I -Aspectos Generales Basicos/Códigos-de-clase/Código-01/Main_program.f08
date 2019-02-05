!================================================================================|
! Thursday, January 31, 2019  
! Movimiento arbitrario de las N partículas para un sistema de discos du-
! ros alejados de las fronteras (en bulto).
! Se incorporan condiciones periodicas e instrucciones de configuraciones.
! Además, se incorpora lo relativo a IPRINT, ISAVE, arreglos de posiciones
! y seguimiento de una partícula trazadora.
!================================================================================|

PROGRAM CODIGO4
    IMPLICIT NONE
        REAL    :: AA, BOXL, RCUT, PHIT, DRMAX, N_STAR, RA, RU, ISEED, JSEED
        REAL    :: KSEED, RUA
        INTEGER :: KI2, N, NN2, NP, NSTEP, IPRINT, ISAVE, ISTEP
        INTEGER :: I, J, K
        REAL, ALLOCATABLE   :: X(:), Y(:), CX(:,:), CY(:,:)
        REAL, PARAMETER     :: PI = 3.1415927 

!-------------------- Output data into a file -----------------------------------|
        OPEN (12, FILE = 'confi_fi.dat',    STATUS = 'unknown') 
        OPEN (13, FILE = 'trazadora.dat',   STATUS = 'unknown')
!        OPEN (14, FILE = 'revision.dat',    STATUS = 'unknown')
! 12: Configuración final
! 13: Historia de partícula arbitraria
! 14: Archivo genérico para revisiones
!--------------------------------------------------------------------------------|

! Lectura de datos de entrada ---------------------------------------------------|
    WRITE(*,*) 'Introduzca el numero total de configuraciones deseadas (NSTEP)'
        READ (*,*) NSTEP
    WRITE(*,*) 'Monitoreo por pantalla: Frecuencia de impresion (IPRINT)'
        READ (*,*) IPRINT
    WRITE(*,*) 'Frecuencia de muestreo (ISAVE)'
        READ (*,*) ISAVE
    WRITE(*,*) 'Introduzca el numero de particulas (N)'
        READ (*,*) N
    WRITE(*,*) 'Introduzca el tamano del ensemble (NN2)'
        READ (*,*) NN2
    WRITE(*,*) 'Introduzca la fraccion en area total (PHI)'
        READ (*,*) PHIT

! Allocate positions ------------------------------------------------------------|
        allocate(X(N))
        allocate(Y(N))
        allocate(CX(N,NN2))
        allocate(CY(N,NN2))

! Máximo desplazamiento por coordenada ------------------------------------------|
    DRMAX = 0.1

! Cálculos preeliminares --------------------------------------------------------|
    AA=1.0/2.0
    N_STAR=4*PHIT/PI
    BOXL=((1.0*N)/N_STAR)**AA
    RCUT=BOXL/2.0
    KI2=0

! Escribir datos de entrada en pantalla -----------------------------------------|
    WRITE(*,*) '----------------------------------------------------'
    WRITE(*,*) 'Numero de particulas N :',  N
    WRITE(*,*) 'Tamano del ensemble NN2:',  NN2
    WRITE(*,*) 'Configuraciones totales:',  NSTEP
    WRITE(*,*) 'Frecuencia de impresion:',  IPRINT
    WRITE(*,*) 'Frecuencia de muestreo :',  ISAVE
    WRITE(*,*) 'Fraccion en area total :',  PHIT
    WRITE(*,*) 'Longitud de la celda   :',  BOXL
    WRITE(*,*) '----------------------------------------------------'

! Configuración inicial aleatoria sin traslapes: ejecución en subrutina ---------|
CALL CONFIGINI(BOXL, N)


! °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°|
! Con lo anterior construimos una configuración inicial arbitraria, ahora debemos
! construir nuevas configuraciones moviendo partículas.
! ¿Cuántas partículas se han de mover? TODAS (N partículas).
! ANOTACIÓN: El programa funciona correctamente hasta este punto. 
! °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°|


! Moviendo las partículas -------------------------------------------------------|
    WRITE(*,*) 'Introduzca dos numeros reales arbitrarios (semillas)'
        READ(*,*) ISEED, JSEED

! Una semilla para seleccionar arbitrariamente a "una" partícula como trazadora -|
    WRITE(*,*) 'Introduzca un numero real arbitrario (semilla) para seleccionar'
    WRITE(*,*) 'una particula'
        READ(*,*) KSEED

! Generadora de NÚMEROS ALEATORIOS ----------------------------------------------|
Call init_random_seed()

! ¿Qué partícula seguiremos? la NP ----------------------------------------------|
    Call random_number(KSEED)
        RUA=KSEED
            NP=NINT(RUA*N)
                IF(NP==0) THEN
                    NP=1
                END IF
    WRITE(*,*) '----------------------------------------------------'
    WRITE(*,*) 'Seguiremos la particula:', NP
    WRITE(*,*) '----------------------------------------------------'

! °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°|
! Hemos "seleccionado" qué particula seguir
! °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°|


! Movimiento arbitrario aleatorio    
    DO ISTEP = 1, NSTEP
        DO I = 1, N
            Call random_number(ISEED)
            Call random_number(JSEED)
                RA=2.0*ISEED
                RU=2.0*JSEED
                    X(I) = X(I) + (RA - 1.0)*DRMAX
                    Y(I) = Y(I) + (RU - 1.0)*DRMAX

! Condiciones periodicas: ninguna partícula queda fuera de la celda 
                    X(I) = X(I)-BOXL*ANINT(X(I)/BOXL)
                    Y(I) = Y(I)-BOXL*ANINT(Y(I)/BOXL)


                    
! °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°|
! Hemos movido las partículas de la configuración inicial. 
! DESCRIPCIÓN: El movimiento es aleatorio y se incluyen condiciones periódicas
! °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°|



! Seguimiento de la NP particula -----------------------------------------------|
            IF(I==NP) THEN
                WRITE(13,*) X(I), Y(I)
            END IF
        END DO

! Verificando si se requiere escribir información de ejecución -----------------|
            IF (MOD(ISTEP, IPRINT) == 0) THEN
                WRITE(*,*) ISTEP
            END IF 

! Verificando si debe almacenar configuraciones de equilibrio ------------------|
            IF (MOD(ISTEP, ISAVE) == 0) THEN
                KI2=KI2+1
! Construir los arreglos para las configuraciones seleccionadas ----------------|
                DO K=1, N
                    CX(K,KI2)=X(K)
                    CY(K,KI2)=Y(K)
                END DO
            END IF
    END DO

! Configuración final ----------------------------------------------------------|
    DO J = 1 , N
        WRITE(12,*) J, X(J), Y(J)
    END DO

! Cierres ----------------------------------------------------------------------|
    CLOSE(12)
    CLOSE(13)
    CLOSE(14)

! Finalizar programa -----------------------------------------------------------|
END PROGRAM CODIGO4