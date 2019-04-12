!=====================================================================================================================|
! Thursday, April 11, 2019  
!   Estudio sobre el Modelo de Potencial Doble Gaussiano Desplazado (DGM). 
!   Sistema en 3D: X, Y y Z. 
!                                                                               UNIDADES REDUCIDAS: SIGMA-BETA (σ, β) 
! DESCRIPCIÓN DE VARIABLES
!   Contador        : I, J, K, ISTEP.
!   Sin categoría   :
!                       N = Número de partículas.
!                    BOXL = Longitud del cubo, por lado, L*
!                    RCUT = Radio de corte
!                    DENS = Densidad reducida n*.
!                     NN2 = Tamaño del ensemble.
!                   NSTEP = Número de configuraciones.
!                   DRMAX = Máximo desplazamiento (en cada paso) de cada partícula.
!     X, Y, Z, CX, CY, CZ = Contienen información de las coordenadas y configuraciones del sistema.
!              VOLD, VNEW = Energía "antigua" y energía "nueva".
!              POLD, PNEW = Presión "antigua" y presión "nueva".
!            DELTV, DELTP = Diferencias de energía y presión, respectivamente.
!              VLRC, PLRC = Correcciones de largo alcance a la energía y la presión.
!                  IRATIO = Frecuencia de corrección de paso.
!               ACEPRATIO = Razón de aceptación de movimientos.
!                      NP = Qué partícula seguir.
!                  IPRINT = Frecuencia de impresión en pantalla (estado de cálculo).
!                   ISAVE = Para guardar configuraciones (¿Cada cuánto?).
!                     KI2 = Auxiliar de ISAVE.
!   Modelo: DGM
!                      TS = Temperatura reducida.
!                   KAPPA = Kappa reducida.
!                  PMEDIA = Presión media (por partícula).
!                 PRESIÓN = Auxiliar para el cálculo de PMEDIA.
!
!   Semillas    : ISEED, JSEED, KSEED, LSEED     
!====================================================================================================================|
PROGRAM CODIGO4
    IMPLICIT NONE
        REAL(KIND=8)                :: BOXL, DRMAX, ISEED, JSEED, KSEED, LSEED, VOLD, VNEW
        REAL(KIND=8)                :: RATIO, RCUT, VLRC, VI, V, DENS, XIOLD, YIOLD
        REAL(KIND=8)                :: XINEW, YINEW, DELTV, PLRC, POLD, PNEW, PI, DELTP
        REAL(KIND=8)                :: ACATMA, ACM, VN, ACEPRATIO, KAPPA, ZIOLD, ZINEW, QSEED
        REAL(KIND=8)                :: TS, P, PN, PMEDIA
        REAL(KIND=8), ALLOCATABLE   :: X(:), Y(:), Z(:)
        REAL(KIND=8), ALLOCATABLE   :: CX(:,:), CY(:,:), CZ(:,:)
        INTEGER                     :: I, J, K, KI2, N, NN2, NSTEP, IPRINT, TYPE
        INTEGER                     :: IRATIO, NP, ISAVE, ISTEP, NENER, PRESION


        
! APERTURA DE ARCHIVOS ----------------------------------------------------------------------------------------------|
        OPEN (12, FILE = '02confi_fi.dat',      STATUS = 'unknown') 
        OPEN (13, FILE = '03trazadora.dat',     STATUS = 'unknown')
        OPEN (14, FILE = '04termalizacion.dat', STATUS = 'unknown')
        OPEN (15, FILE = '05Informacion.dat',   STATUS = 'unknown')



! LECTURA DE DATOS DE ENTRADA ---------------------------------------------------------------------------------------|
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
    WRITE(*,*) 'Introduzca el valor de T*'
        READ (*,*) TS
    WRITE(*,*) 'Introduzca el valor de k*'
        READ (*,*) KAPPA
    WRITE(*,*) 'Selecciona un tipo de configuracion inicial'
    WRITE(*,*) '    1. Regular'
    WRITE(*,*) '    2. Aleatoria'
        READ (*,*) TYPE



! ASIGNAR DIMENSIONES -----------------------------------------------------------------------------------------------|
    allocate(X(N))
    allocate(Y(N))
    allocate(Z(N))
    allocate(CX(N,NN2))
    allocate(CY(N,NN2))
    allocate(CZ(N,NN2))



! VALORES Y CÁLCULOS PREELIMINARES ----------------------------------------------------------------------------------|                                
    BOXL=((1.0*N)/DENS)**(1.0/3.0)   
    RCUT=BOXL/2.0                   
    KI2=0        
    ACATMA=0.0
    ACM=0.0
    PRESION = 0
    PMEDIA = 0.0
!   Semillas necesarias 
    ISEED = 7865.239
    JSEED = 2344.564
    KSEED = 5435.285
    LSEED = 4334.363
    QSEED = 9724.686


! ESCRIBIR DATOS CAPTURADOS Y CALCULADOS EN PANTALLA ----------------------------------------------------------------|
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
    WRITE(15,*) '   Temperatura reducida           T* = ',  TS
    WRITE(15,*) '   Valor de                       K* = ',  KAPPA


! GENERAR CONFIGURACIÓN INICIAL ------------------------------------------------------------------------------------|
    IF(TYPE == 1) THEN
        CALL CONFINI_REGULAR_3D(N, BOXL, X, Y, Z)
    ELSE
        CALL CONFIGINI(N, BOXL, X, Y, Z)
    END IF



! ASPECTOS DE ENERGÍA: ---------------------------------------------------------------------------------------------|
!   Corrección de largo alcance (de ser necesario, modificar para cada conjunto de parámetros)
        VLRC = 0.0
        PLRC = 0.0
!   Cálculo de la energía de la configuración inicial:
        CALL SUMUP (N, RCUT, BOXL, DENS, TS, KAPPA, V, P, X, Y, Z)
        VI = V + VLRC
        PI = P + PLRC
            WRITE(*,*)  '------------------------------------------------------------'
            WRITE(*,*)  'Energia de la configuracion inicial:', VI
            WRITE(15,*) 'Energia de la configuracion inicial:', VI
            WRITE(*,*)  'Presion de la configuracion inicial:', PI
            WRITE(15,*) 'Presion de la configuracion inicial:', PI
            WRITE(*,*)  '------------------------------------------------------------'
            WRITE(*,*)  '      ISTEP |   RATIO    |       DRMAX       |        VN               |         PN'



! GENERADOR DE NÚMEROS ALEATORIOS ----------------------------------------------------------------------------------|
    Call init_random_seed()
    


! SEGUIMIENTO DE LA PARTÍCULA NP -----------------------------------------------------------------------------------|
    Call random_number(KSEED)
            NP=NINT(KSEED*N)
                IF(NP==0) THEN
                    NP=1
                END IF



! MOVIMIENTO DE LAS PARTÍCULAS DE LA CONFIGURACIÓN INICIAL ---------------------------------------------------------|       
    DO ISTEP = 1, NSTEP
        DO I = 1, N
                XIOLD = X(I)
                YIOLD = Y(I)
                ZIOLD = Z(I)
!   Cálculo de energía de la configuración actual: 
                CALL ENERGY(N, BOXL, DENS, KAPPA, TS, XIOLD, YIOLD, ZIOLD, I, RCUT, VOLD, POLD, X, Y, Z)
!   Cálculo de las nuevas posiciones de cada partícula:
                Call random_number(ISEED)
                XINEW = XIOLD + (2.0*ISEED - 1.0)*DRMAX
                Call random_number(JSEED)
                YINEW = YIOLD + (2.0*JSEED - 1.0)*DRMAX
                Call random_number(QSEED)
                ZINEW = ZIOLD + (2.0*QSEED - 1.0)*DRMAX
!   Aplicando condiciones periódicas: 
                XINEW = XINEW - BOXL*ANINT(XINEW/BOXL)
                YINEW = YINEW - BOXL*ANINT(YINEW/BOXL)
                ZINEW = ZINEW - BOXL*ANINT(ZINEW/BOXL)
!   Cálculo de energía de la configuración nueva:            
                CALL ENERGY(N, BOXL, DENS, KAPPA, TS, XINEW, YINEW, ZINEW, I, RCUT, VNEW, PNEW, X, Y, Z)



! SEGUIMIENTO DE LA PARTÍCULA NP ------------------------------------------------------------------------------------|
            IF(I == NP) THEN
                WRITE(13,*) X(I), Y(I), Z(I)
            END IF



! ALGORITMO METROPOLIS: CRITERIO DE ACEPTACIÓN O RECHAZO DEL MOVIMIENTO ---------------------------------------------|
            DELTV = VNEW - VOLD
            DELTP = PNEW - POLD 
                IF(DELTV < 75.0) THEN 
                    IF(DELTV <= 0.0) THEN 
                        V = V + DELTV
                        P = P + DELTP
                        X(I) = XINEW
                        Y(I) = YINEW 
                        Z(I) = ZINEW
                        ACATMA = ACATMA + 1.0
                        Call random_number(LSEED)
                    ELSE IF (EXP(-DELTV) > LSEED ) THEN 
                        V = V + DELTV
                        P = P + DELTP
                        X(I) = XINEW
                        Y(I) = YINEW 
                        Z(I) = ZINEW
                        ACATMA = ACATMA + 1.0
                    END IF
                END IF 
            ACM = ACM + 1.0
        END DO



! CÁLCULO Y ALMACENAMIENTO EN 14 DE LA ENERGÍA Y PRESIÓN POR PARTÍCULA (TERMALIZACIÓN) ------------------------------|
        VN = (V + VLRC)/(1.0*N)
        PN = (P + PLRC)/(1.0*N)
        WRITE(14,*) ISTEP, VN, PN, V, P



! ¿SE DEBE AJUSTAR EL DESPLAZAMIENTO MÁXIMO DRMAX? ------------------------------------------------------------------|
        IF(MOD(ISTEP, IRATIO) == 0) THEN
            RATIO = ACATMA/(1.0*(N*IRATIO))
                IF(RATIO > ACEPRATIO) THEN 
                    DRMAX = DRMAX*1.05
                ELSE 
                    DRMAX = DRMAX*0.95
                END IF
            ACATMA = 0.0
        END IF



! VERIFICAR SI SE DEBE ESCRIBIR INFORMACIÓN DE EJECUCIÓN EN PANTALLA ------------------------------------------------|
        IF (MOD(ISTEP, IPRINT) == 0) THEN
            WRITE(*,*) ISTEP, SNGL(RATIO), SNGL(DRMAX), VN, PN
        END IF 



! VERIFICAR SI SE DEBE ALMACENAR CONFIGURACIONES DE EQUILIBRIO ------------------------------------------------------|
        IF (MOD(ISTEP, ISAVE) == 0 .AND. ISTEP > NENER) THEN
            KI2=KI2+1
!   Construir arreglos para las configuraciones correspondientes:
            DO K=1, N
                CX(K,KI2)=X(K)
                CY(K,KI2)=Y(K)
                CZ(K,KI2)=Z(K)
            END DO
            PMEDIA = PMEDIA + PN
            PRESION = PRESION + 1
        END IF
    END DO


! CÁLCULO DE LA PRESIÓN MEDIA ---------------------------------------------------------------------------------------|
    PMEDIA = PMEDIA/REAL(PRESION,8)
    WRITE(*,*) 'El valor de la presion promedio es P=', SNGL(PMEDIA)
    WRITE(15,*) 'El valor de la presion promedio es P=', PMEDIA  



! CÁLCULO DE LA FUNCIÓN DE DISTRIBUCIÓN RADIAL g(r): ----------------------------------------------------------------|
    WRITE(*,*) 'Estamos calculando puntos sobre la funcion de distribucion radial g(r)'
    CALL GDR(N, BOXL, DENS, NN2, CX, CY, CZ, KI2)
    WRITE(*,*) 'El proceso ha terminado de manera exitosa.'
    

    
! ALMACENAR CONFIGURACIÓN FINAL EN 12 -------------------------------------------------------------------------------|
    DO J = 1, N
        WRITE(12,*) X(J), Y(J), Z(J)
    END DO



! INFORMACIÓN SOBRE ALMACENAMIENTO DE LA INFORMACIÓN ----------------------------------------------------------------|
    WRITE(*,*) '----------------------------------------------------------------------'
    WRITE(*,*) 'Se han generado los siguientes archivos:'
    WRITE(*,*) '   Configuracion inicial                   : ', '01confi_in.dat'
    WRITE(*,*) '   Configuracion final                     : ', '02confi_fi.dat'
    WRITE(*,*) '   Seguimiento de la particula', NP, ': ', '03trazadora.dat' 
    WRITE(*,*) '   Para curva de termalizacion             : ', '04termalizacion.dat'
    WRITE(*,*) '   Informacion                             : ', '05informacion.dat'
    WRITE(*,*) '   Funcion de distribucion radial g(r)     : ', '06g(r).dat'
    WRITE(*,*) '----------------------------------------------------------------------'



! CERRAR ARCHIVOS ---------------------------------------------------------------------------------------------------|
    CLOSE(12)
    CLOSE(13)
    CLOSE(14)
    CLOSE(15)



! FIN DEL PROGRAMA --------------------------------------------------------------------------------------------------|
END PROGRAM CODIGO4