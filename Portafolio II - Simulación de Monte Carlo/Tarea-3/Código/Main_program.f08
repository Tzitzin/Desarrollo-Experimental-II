!=====================================================================================================|
! Thursday, January 31, 2019  
!   Movimiento arbitrario de N partículas para un sistema de discos duros en bulto.
!   Se incluyen condiciones periódicas e información de ejecución. 
!                                                               UNIDADES REDUCIDAS  : SIGMA - BETA
!                                                               MODELO DE POTENCIAL : DISCOS DUROS (HD)
! DESCRIPCIÓN DE VARIABLES
!   Contador    : I, J, K, ISTEP.
!   Físicas     : 
!                       N = Número de partículas.
!                    BOXL = Longitud de la celda, por lado, L*
!                    DENS = Densidad reducida n*.
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
!   Semillas    : ISEED, JSEED y LSEED para X, Y y criterio de aceptación o rechazo, respectivamente.    
!=====================================================================================================|
PROGRAM CODIGO4
    IMPLICIT NONE
        REAL(KIND=8)                :: BOXL, DRMAX, ISEED, JSEED, KSEED, LSEED, VOLD, VNEW, RATIO
        REAL(KIND=8)                :: RCUT, VLRC, VI, V, DENS, XIOLD, YIOLD, XINEW, YINEW, DELTV
        REAL(KIND=8)                :: ACATMA, ACM, VN, ACEPRATIO
        REAL(KIND=8), ALLOCATABLE   :: X(:), Y(:), CX(:,:), CY(:,:)
        INTEGER                     :: I, J, K, KI2, N, NN2, NSTEP, IPRINT, ISAVE, ISTEP, NENER
        INTEGER                     :: IRATIO, NP

        
! APERTURA DE ARCHIVOS -----------------------------------------------------------------|
        OPEN (12, FILE = '02confi_fi.dat',      STATUS = 'unknown') 
        OPEN (13, FILE = '03trazadora.dat',     STATUS = 'unknown')
        OPEN (14, FILE = '04termalizacion.dat', STATUS = 'unknown')
        OPEN (15, FILE = '05Informacion.dat',   STATUS = 'unknown')


! LECTURA DE DATOS DE ENTRADA ----------------------------------------------------------|
    WRITE(*,*) 'Introduzca el numero de particulas (N)'
        READ (*,*) N
    WRITE(*,*) 'Introduzca el numero total de configuraciones deseadas (NSTEP)'
        READ (*,*) NSTEP
    WRITE(*,*) 'Seleccione configuracion en equilibrio (NENER)'
        READ (*,*) NENER
    WRITE(*,*) 'Monitoreo por pantalla: Frecuencia de impresion (IPRINT)'
        READ (*,*) IPRINT
    WRITE(*,*) 'Frecuencia de muestreo (ISAVE)'
        READ (*,*) ISAVE
    WRITE(*,*) 'Introduzca la frecuencia de correcion de paso (IRATIO)'
        READ (*,*) IRATIO
    WRITE(*,*) 'Introduzca el tamano del ensemble (NN2)'
        READ (*,*) NN2
    WRITE(*,*) 'Introduzca la densidad reducida'
        READ (*,*) DENS
    WRITE(*,*) 'Introduzca el desplazamiento maximo (DRMAX) por particula'
        READ (*,*) DRMAX
    WRITE(*,*) 'Establezca el criterio de aceptacion (RATIO)'
        READ (*,*) ACEPRATIO
! --------------------------------------------------------------------------------------|


! ASIGNAR DIMENSIONES ------------------------------------------------------------------|
    allocate(X(N))
    allocate(Y(N))
    allocate(CX(N,NN2))
    allocate(CY(N,NN2))


! VALORES Y CÁLCULOS PREELIMINARES -----------------------------------------------------|                                
    BOXL=((1.0*N)/DENS)**(1.0/2.0)   
    RCUT=BOXL/2.0                   
    KI2=0        
    ACATMA=0.0
    ACM=0.0
!   Semillas necesarias 
    ISEED=7865.239
    JSEED=2344.564
    KSEED=5435.285
    LSEED=4334.363


! ESCRIBIR DATOS CAPTURADOS Y CALCULADOS EN PANTALLA -----------------------------------|
    WRITE(15,*) 'DATOS DE LA SIMULACION:'
    WRITE(15,*) '   Numero de particulas            N = ',  N
    WRITE(15,*) '   Configuraciones totales     NSTEP = ',  NSTEP
    WRITE(15,*) '   Configuracion de equilibrio NENER = ',  NENER
    WRITE(15,*) '   Frecuencia de muestreo      ISAVE = ',  ISAVE
    WRITE(15,*) '   Correcion de paso          IRATIO = ',  IRATIO
    WRITE(15,*) '   Tamano del ensemble           NN2 = ',  NN2
    WRITE(15,*) '   Densidad reducida              n* = ',  DENS
    WRITE(15,*) '   Desplazamiento max. inicial DRMAX = ',  DRMAX
    WRITE(15,*) '   Criterio de aceptacion  ACEPRATIO = ',  ACEPRATIO
    WRITE(15,*) '   Longitud de la celda           L* = ',  BOXL


! GENERAR CONFIGURACIÓN INICIAL --------------------------------------------------------|
    IF(DENS<=0.75) THEN
        CALL CONFIGINI(N, BOXL, X, Y)
    ELSE
        CALL CONFINI_REGULAR_2D(N, BOXL, X, Y)
    END IF


! ASPECTOS DE ENERGÍA: -----------------------------------------------------------------|
!   Corrección de largo alcance (HD, modificar en otro caso)
        VLRC = 0.0
!   Cálculo de la energía de la configuración inicial:
        CALL SUMUP (N, RCUT, BOXL, V, X, Y)
        VI=V+VLRC
            WRITE(*,*) '------------------------------------------------------------'
            WRITE(*,*) 'Energia de la configuracion inicial:', VI
            WRITE(15,*)'Energia de la configuracion inicial:', VI
            WRITE(*,*) '------------------------------------------------------------'
            WRITE(*,*) '      ISTEP |   RATIO    |       DRMAX       |        VN  '


! GENERADOR DE NÚMEROS ALEATORIOS ------------------------------------------------------|
    Call init_random_seed()
    

! SEGUIMIENTO DE LA PARTÍCULA NP -------------------------------------------------------|
    Call random_number(KSEED)
            NP=NINT(KSEED*N)
                IF(NP==0) THEN
                    NP=1
                END IF


! MOVIMIENTO DE LAS PARTÍCULAS DE LA CONFIGURACIÓN INICIAL -----------------------------|       
    DO ISTEP = 1, NSTEP
        DO I = 1, N
! Números aleatorios utilizando semillas:        
            Call random_number(ISEED)
            Call random_number(JSEED)
            Call random_number(LSEED)
                XIOLD=X(I)
                YIOLD=Y(I)
!   Cálculo de energía de la configuración actual: 
                CALL ENERGY(N, BOXL, XIOLD, YIOLD, I, RCUT, VOLD, X, Y)
!   Cálculo de las nuevas posiciones de cada partícula:
                XINEW = XIOLD + (2.0*ISEED - 1.0)*DRMAX
                YINEW = YIOLD + (2.0*JSEED - 1.0)*DRMAX
!   Aplicando condiciones periódicas: 
                XINEW = XINEW-BOXL*ANINT(XINEW/BOXL)
                YINEW = YINEW-BOXL*ANINT(YINEW/BOXL)
!   Cálculo de energía de la configuración nueva:            
                CALL ENERGY(N, BOXL, XINEW, YINEW, I, RCUT, VNEW, X, Y)


! SEGUIMIENTO DE LA PARTÍCULA NP -------------------------------------------------------|
            IF(I == NP) THEN
                WRITE(13,*) X(I), Y(I)
            END IF


! ALGORITMO METROPOLIS: CRITERIO DE ACEPTACIÓN O RECHAZO DEL MOVIMIENTO ----------------|
            DELTV = VNEW - VOLD 
                IF(DELTV < 75.0) THEN 
                    IF(DELTV <= 0.0) THEN 
                        V = V + DELTV
                        X(I) = XINEW
                        Y(I) = YINEW 
                        ACATMA = ACATMA + 1.0
                    ELSE IF (EXP(-DELTV) > LSEED ) THEN 
                        V = V + DELTV
                        X(I) = XINEW
                        Y(I) = YINEW 
                        ACATMA = ACATMA + 1.0
                    END IF
                END IF 
            ACM = ACM + 1.0
        END DO


! CÁLCULO Y ALMACENAMIENTO EN 14 DE LA ENERGÍA POR PARTÍCULA (TERMALIZACIÓN) -----------|
        VN = (V + VLRC)/(1.0*N)
        WRITE(14,*) ISTEP, VN


! ¿SE DEBE AJUSTAR EL DESPLAZAMIENTO MÁXIMO DRMAX? -------------------------------------|
        IF(MOD(ISTEP, IRATIO) == 0) THEN
            RATIO = ACATMA/(1.0*(N*IRATIO))
                IF(RATIO > ACEPRATIO) THEN 
                    DRMAX = DRMAX*1.05
                ELSE 
                    DRMAX = DRMAX*0.95
                END IF
            ACATMA = 0.0
        END IF


! VERIFICAR SI SE DEBE ESCRIBIR INFORMACIÓN DE EJECUCIÓN EN PANTALLA -------------------|
        IF (MOD(ISTEP, IPRINT) == 0) THEN
            WRITE(*,*) ISTEP, SNGL(RATIO), SNGL(DRMAX), VN
        END IF 


! VERIFICAR SI SE DEBE ALMACENAR CONFIGURACIONES DE EQUILIBRIO -------------------------|
        IF (MOD(ISTEP, ISAVE) == 0 .AND. ISTEP > NENER) THEN
            KI2=KI2+1
!   Construir arreglos para las configuraciones correspondientes:
            DO K=1, N
                CX(K,KI2)=X(K)
                CY(K,KI2)=Y(K)
            END DO
        END IF
    END DO


! CÁLCULO DE LA FUNCIÓN DE DISTRIBUCIÓN RADIAL g(r):
    WRITE(*,*) 'Estamos calculando puntos sobre la funcion de distribucion radial g(r)'
    CALL GDR(N, BOXL, DENS, NN2, CX, CY, KI2)
    WRITE(*,*) 'El proceso ha terminado de manera exitosa.'
    
    
! ALMACENAR CONFIGURACIÓN FINAL EN 12 --------------------------------------------------|
    DO J = 1, N
        WRITE(12,*) X(J), Y(J)
    END DO


! INFORMACIÓN SOBRE ALMACENAMIENTO DE LA INFORMACIÓN -----------------------------------|
    WRITE(*,*) '----------------------------------------------------------------------'
    WRITE(*,*) 'Se han generado los siguientes archivos:'
    WRITE(*,*) '   Configuracion inicial                   : ', '01confi_in.dat'
    WRITE(*,*) '   Configuracion final                     : ', '02confi_fi.dat'
    WRITE(*,*) '   Seguimiento de la particula', NP, ': ', '03trazadora.dat' 
    WRITE(*,*) '   Para curva de termalizacion             : ', '04termalizacion.dat'
    WRITE(*,*) '   Informacion                             : ', '05informacion.dat'
    WRITE(*,*) '   Funcion de distribucion radial g(r)     : ', '06g(r).dat'
    WRITE(*,*) '   n*, g(1+) y presion en g(1+)            : ', '07presion.dat'
    WRITE(*,*) '----------------------------------------------------------------------'


! CERRAR ARCHIVOS ----------------------------------------------------------------------|
    CLOSE(12)
    CLOSE(13)
    CLOSE(14)
    CLOSE(15)


! FIN DEL PROGRAMA ---------------------------------------------------------------------|
END PROGRAM CODIGO4