!====================================================================================|
! Thursday, January 31, 2019  
!   Movimiento arbitrario de N partículas para un sistema de discos duros en bulto.
!   Se incluyen condiciones periódicas e información de ejecución. Además, se incor-
!   pora lo relativo a IPRINT, ISAVE, arreglos de posiciones y seguimiento de una
!   partícula "trazadora".
!                                                           UNIDADES REDUCIDAS: SIGMA
! DESCRIPCIÓN DE VARIABLES, TOTAL = 23
!   Contador    : I, J, K, ISTEP.
!   Físicas     : 
!                       N = Número de partículas.
!                    BOXL = Longitud de la celda, por lado, L*
!                  N_STAR = Densidad reducida n*.
!                    PHIT = Fracción en área Ф.
!                     NN2 = Tamaño del ensemble.
!                   NSTEP = Número de configuraciones.
!                   DRMAX = Máximo desplazamiento (en cada paso) de cada partícula.
!            X, Y, CX, CY = Contienen información de las coordenadas y configuraciones
!                           del sistema.
!
!   Otras       : 
!                      NP = Qué partícula seguir.
!                  IPRINT = Frecuencia de impresión en pantalla (estado de cálculo).
!                   ISAVE = Para guardar configuraciones (¿Cada cuánto?).
!                      AA = Exponente.
!                     KI2 = Auxiliar de ISAVE.
!
!   Semillas    : ISEED, JSEED y KSEED para X, Y y seleccionar NP, respectivamente.    
!====================================================================================|

PROGRAM CODIGO4
    IMPLICIT NONE
        REAL                :: AA, BOXL, PHIT, DRMAX, N_STAR, ISEED, JSEED, KSEED
        INTEGER             :: I, J, K, KI2, N, NN2, NP, NSTEP, IPRINT, ISAVE, ISTEP
        REAL, ALLOCATABLE   :: X(:), Y(:), CX(:,:), CY(:,:)
        REAL, PARAMETER     :: PI = 3.1415927 

!-------------------- Output data into a file ---------------------------------------|
        OPEN (12, FILE = '02confi_fi.dat',    STATUS = 'unknown') 
        OPEN (13, FILE = '03trazadora.dat',   STATUS = 'unknown')
        OPEN (14, FILE = '04Information.dat', STATUS = 'unknown')
! 12: Configuración final.
! 13: Historia de la partícula arbitraria NP.
! 14: Información sobre la simulación.
!------------------------------------------------------------------------------------|

! Lectura de datos de entrada -------------------------------------------------------|
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
    WRITE(*,*) 'Introduzca el maximo desplazamiento de cada particula (DRMAX)'
        READ (*,*) DRMAX

! Asignar posiciones ----------------------------------------------------------------|
    allocate(X(N))
    allocate(Y(N))
    allocate(CX(N,NN2))
    allocate(CY(N,NN2))

! Cálculos preeliminares ------------------------------------------------------------|
    AA=1.0/2.0                      
    N_STAR=4*PHIT/PI                
    BOXL=((1.0*N)/N_STAR)**AA                       
    KI2=0                           

! Escribir datos de entrada en pantalla ---------------------------------------------|
    WRITE(*,*) '-----------------------------------------------------------------'
    WRITE(*,*) 'VERIFICA QUE HAYAS REGISTRADO LOS DATOS CORRECTOS:'
    WRITE(*,*) 'Numero de particulas            N = ',  N
    WRITE(*,*) 'Tamano del ensemble           NN2 = ',  NN2
    WRITE(*,*) 'Configuraciones totales     NSTEP = ',  NSTEP
    WRITE(*,*) 'Frecuencia de impresion    IPRINT = ',  IPRINT
    WRITE(*,*) 'Frecuencia de muestreo      ISAVE = ',  ISAVE
    WRITE(*,*) 'Fraccion en area total       PHIT = ',  PHIT
    WRITE(*,*) 'Longitud de la celda           L* = ',  BOXL
    WRITE(*,*) 'El rango de X, Y es [-L*/2, L*/2] = ', '[',-BOXL/2, ',' , BOXL/2, ']'
    WRITE(*,*) '---------------------------------------------------------------------'


! Configuración inicial aleatoria sin traslapes: ejecución en subrutina -------------|
CALL CONFIGINI(N, BOXL, X, Y)


! °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°|
! Con lo anterior construimos una configuración inicial arbitraria, ahora debemos    !
! construir nuevas configuraciones moviendo partículas.                              !
! ¿Cuántas partículas se han de mover? TODAS (N partículas).                         ! 
! °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°|


! Semillas necesarias ---------------------------------------------------------------|
    WRITE(*,*) 'Introduzca tres numeros reales arbitrarios (semillas)'
        READ(*,*) ISEED, JSEED, KSEED

! Generador de NÚMEROS ALEATORIOS ---------------------------------------------------|
    Call init_random_seed()

! ¿Qué partícula seguiremos? la NP --------------------------------------------------|
    Call random_number(KSEED)
            NP=NINT(KSEED*N)
                IF(NP==0) THEN
                    NP=1
                END IF
    WRITE(*,*) '------------------------------------------'
    WRITE(*,*) 'Seguiremos la particula:', NP
    WRITE(*,*) '------------------------------------------'


! °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°|
! Hemos "seleccionado" qué particula seguir.
! °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°|


! Movimiento arbitrario aleatorio    
    DO ISTEP = 1, NSTEP
        DO I = 1, N
            Call init_random_seed()
            Call random_number(ISEED)
            Call random_number(JSEED)
                    X(I) = X(I) + (2.0*ISEED - 1.0)*DRMAX
                    Y(I) = Y(I) + (2.0*JSEED - 1.0)*DRMAX

! Condiciones periódicas: ninguna partícula queda fuera de la celda 
                    X(I) = X(I)-BOXL*NINT(X(I)/BOXL)
                    Y(I) = Y(I)-BOXL*NINT(Y(I)/BOXL)


                    
! °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°|
! Hemos movido las partículas de la configuración inicial. 
! DESCRIPCIÓN: El movimiento es aleatorio y se incluyen condiciones periódicas.
! °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°|



! Seguimiento de la NP particula ---------------------------------------------------|
            IF(I==NP) THEN
                WRITE(13,*) X(I), Y(I)
            END IF
        END DO

! Verificando si se requiere escribir información de ejecución ---------------------|
            IF (MOD(ISTEP, IPRINT) == 0) THEN
                WRITE(*,*) 'Progreso:', ISTEP
            END IF 

! Verificando si debe almacenar configuraciones de equilibrio ----------------------|
            IF (MOD(ISTEP, ISAVE) == 0) THEN
                KI2=KI2+1
! Construir los arreglos para las configuraciones seleccionadas --------------------|
                DO K=1, N
                    CX(K,KI2)=X(K)
                    CY(K,KI2)=Y(K)
                END DO
            END IF
    END DO

! Configuración final --------------------------------------------------------------|
    DO J = 1 , N
        WRITE(12,*) J, X(J), Y(J)
    END DO

! Información adicional final (en pantalla) ----------------------------------------|
    WRITE(*,*) '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
    WRITE(*,*) 'El proceso ha terminado de manera exitosa. Se han generado los si-'
    WRITE(*,*) 'guientes archivos:'
    WRITE(*,*) '   Configuracion inicial                   : ', '01confi_in.dat'
    WRITE(*,*) '   Configuracion final                     : ', '02confi_fi.dat'
    WRITE(*,*) '   Seguimiento de la particula', NP, ': ', '03trazadora.dat' 
    WRITE(*,*) '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'

! Cierre de archivos ---------------------------------------------------------------|
    CLOSE(12)
    CLOSE(13)
    CLOSE(14)

! Finalizar programa ---------------------------------------------------------------|
END PROGRAM CODIGO4
