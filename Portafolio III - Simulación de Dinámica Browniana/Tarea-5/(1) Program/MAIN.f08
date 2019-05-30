!   =======================================================================================================
!   Monday, April 15, 2019
!   SIMULACIÓN DE DINÁMICA BROWNIANA
!       | MODELO DE POTENCIAL : YUKAWA
!       | UNIDADES REDUCIDAS  : SIGMA-BETA (σ, β)
!   Código adaptado por WyL Alberto Félix Guillen
!
!   DESCRIPCIÓN DE VARIABLES
!       Contadoor       :   
!                           I, J, K                   
!       Otras           :
!                           N       = Número de partículas
!                           DENS    = Densidad
!                           PHI     = Fracción en volumen
!                           BOXL    = Longitud (por lado) de la celda de simulación 
!                           RCUT    = Radio de corte (Mitad de la celda)
!                           NN1     =
!                           NN2     =
!                           NENER   = Configuración inicial de equilibrio
!                           NSTEP   = Número total de configuraciones
!                           DT      = Tiempo de paso
!                           A y ZK  = Parámetros del potencial (YUKAWA, u(r)=A*EXP[-z*(r-1)]/r)
!                           NFREC   = Frecuencia de almacenamiento de CX, CY y CZ
!                           NFREC2  = Frecuencia de almacenamiento de CXR, CYR y CZR
!                           VAR     = Varianza
!                           X, Y, Z = Coordenadas de las partículas
!   SUBRUTINAS NECESARIAS
!       1. Configuración inicial
!           a) Regular (con o sin marialuisa)
!           b) Aleatoria
!       2. FUERZAS: Cálculo de la fuerza de interacción de las partículas y la energía de la configuración
!       3. GDR:     Cálculo de la función de distribución radial g(r)
!       4. WDT:     Cálculo del desplazamiento cuadrátrico medio y coeficiente de difusión dependiente del 
!                   tiempo
!       5. ZRAN:    Generador de números aleatorios con distribución uniforme [0,1]
!       6. AZARG:   Generador de números aleatorios con distribución gaussiana (o normal) 
!   =======================================================================================================
PROGRAM DB
    IMPLICIT NONE 
        REAL(KIND=8)                    :: DENS, DT, PI, BOXL, RCUT, VAR, A, ZK
        REAL(KIND=8)                    :: AX, AY, AZ, ENPOT, PHI, KSEED
        REAL(KIND=8), ALLOCATABLE       :: X(:), Y(:), Z(:), FX(:), FY(:), FZ(:), XR(:), YR(:), ZR(:)
        REAL(KIND=8), ALLOCATABLE       :: CX(:,:), CY(:,:), CZ(:,:), CXR(:,:), CYR(:,:), CZR(:,:)
        INTEGER                         :: N, NN2, ISEED, ISEED1, ISEED2, ISEED3
        INTEGER                         :: NSTEP, NENER, NFREC, NFREC2, IPRINT, TYPE
        INTEGER                         :: I, J, K, L, NP
        INTEGER                         :: KI, KI2
        


!   APERTURA DE ARCHIVOS -------------------------------------------------|01|
    OPEN(10,    FILE = '05Informacion.dat',     STATUS = 'UNKNOWN')
    OPEN(11,    FILE = '02confi_fi.dat',        STATUS = 'UNKNOWN')
    OPEN(12,    FILE = '04termalizacion.dat',   STATUS = 'UNKNOWN')
    OPEN(13,    FILE = '03trazadora.dat',       STATUS = 'UNKNOWN')
!   ----------------------------------------------------------------------|01|
    
    

!   SOLICITUD DE DATOS DE ENTRADA -----------------------------------------------------------------|02|
    WRITE(*,*) 'Introduce el numero de particulas (N)'
        READ(*,*) N
    WRITE(*,*) 'Introduce la fraccion en volumen (PHI)'
        READ(*,*) PHI
    WRITE(*,*) 'Introduce el numero total de configuraciones (NSTEP)'
        READ(*,*) NSTEP
    WRITE(*,*) 'Introduce desde donde se guardaran las configuraciones de equilibrio (NENER)'
        READ(*,*) NENER
    WRITE(*,*) 'Introduce el tamano del ensemble (NN2)'
        READ(*,*) NN2
    WRITE(*,*) 'Introduce la frecuencia de almacenamiento de configuraciones (NFREC)'
        READ(*,*) NFREC
    WRITE(*,*) 'Introduce la frecuencia de almacenamiento para el calculo de W(t) (NFREC2)'
        READ(*,*) NFREC2
    WRITE(*,*) 'Introduce el valor del tiempo de paso (DT)'
        READ(*,*) DT
    WRITE(*,*) 'Monitoreo por pantalla: frecuencia de impresion (IPRINT)'
        READ(*,*) IPRINT
    WRITE(*,*) 'Selecciona el tipo de configuracion inicial'
    WRITE(*,*) '    1. Regular'
    WRITE(*,*) '    2. Aleatoria'
        READ(*,*) TYPE
!   -----------------------------------------------------------------------------------------------|02|



!   CÁLCULOS PREELIMINARES Y CONDICIONES INICIALES -----|03|
!   Condiciones iniciales
    KI = 0
    KI2 = 0
    L = 1
!   Algunos cálculos iniciales
    PI = 4.0*ATAN(1.0)
    DENS = (6.0E0*PHI)/PI
    BOXL = ((1.0*N)/DENS)**(1.0/3.0)
    RCUT = BOXL/2.0
    VAR = SQRT(2.0*DT)
!   Parámetros del potencial (YUKAWA)
    A = 500.0
    ZK = 0.149
    A = A*EXP(ZK)
    ISEED=7865
    ISEED1=2344
    ISEED2=4334
    ISEED3=9724
    KSEED = 5435.285    
!   Asignación de dimensiones
    allocate(X(N))
    allocate(Y(N))
    allocate(Z(N))
    allocate(XR(N))
    allocate(YR(N))
    allocate(ZR(N))
    allocate(FX(N))
    allocate(FY(N))
    allocate(FZ(N))
    allocate(CX(N,NN2))
    allocate(CY(N,NN2))
    allocate(CZ(N,NN2))
    allocate(CXR(N,NN2))
    allocate(CYR(N,NN2))
    allocate(CZR(N,NN2))
!   ----------------------------------------------------|03|




!   ESCRIBIR DATOS CAPTURADOS Y CALCULADOS EN UN ARCHIVO ---------------------|04|
    WRITE(10,*) 'DATOS DE LA SIMULACION:'
    WRITE(10,*) '   Numero de particulas            N = ',  N
    WRITE(10,*) '   Configuraciones totales     NSTEP = ',  NSTEP
    WRITE(10,*) '   Configuracion de equilibrio NENER = ',  NENER
    WRITE(10,*) '   Frecuencia de muestreo      NFREC = ',  NFREC
    WRITE(10,*) '   Frecuencia de muestreo     NFREC2 = ',  NFREC2
    WRITE(10,*) '   Tamano del ensemble           NN2 = ',  NN2
    WRITE(10,*) '   Densidad reducida              n* = ',  DENS
    WRITE(10,*) '   Tiempo de paso                 DT = ',  DT
    WRITE(10,*) '   Longitud de la celda           L* = ',  BOXL
    WRITE(10,*) '   Varianza                      VAR = ',  VAR
    WRITE(10,*) '   Tipo de configuracion        TYPE = ',  TYPE
!   --------------------------------------------------------------------------|04|



!   ASPECTOS DE LA CONFIGURACIÓN INICIAL -----------------------------------------------------|05|
!   Generar la configuración inicial elegida    
    IF(TYPE == 1) THEN
        CALL CONFINI_REGULAR_3D(N, BOXL, X, Y, Z)
    ELSE
        CALL CONFIGINI(N, BOXL, X, Y, Z)
    END IF
!   Calcular fuerza y energía en la configuración inicial
    CALL FUERZAS(L, N, BOXL, X, Y, Z, FX, FY, FZ, A, ZK, ENPOT)
    WRITE(*,*)  '-------------------------------------------------------------------------'
    WRITE(*,*)  'Energia de la configuracion inicial = ', ENPOT/REAL(N)
    WRITE(10,*) 'Energia de la configuracion inicial = ', ENPOT/REAL(N)
    WRITE(*,*)  '-------------------------------------------------------------------------'
    WRITE(*,*)  '      ISTEP |         ENERGIA'
!   ------------------------------------------------------------------------------------------|05|



!   SEGUIMIENTO DE LA PARTÍCULA NP --------------|--|
    Call random_number(KSEED)
            NP=NINT(KSEED*N)
                IF(NP==0) THEN
                    NP=1
                END IF
    WRITE(*,*) 'La particula trazadora es', NP
!   ---------------------------------------------|--|             



!   COMIENZA EL MOVIMIENTO SEGÚN: MOVIMIENTO BROWNIANO -----------------------------------------------|06|
!   Construcción de configuraciones (termina en línea)
    DO 20 L = 1, NSTEP
!   Movimiento de las partículas (termina en línea 139)
        DO 25 I = 1, N                                                
            CALL AZARG(ISEED1, AX)
            CALL AZARG(ISEED2, AY)
            CALL AZARG(ISEED3, AZ)
!   Algoritmo de Ermak
            X(I) = X(I) + FX(I)*DT + VAR*AX
            Y(I) = Y(I) + FY(I)*DT + VAR*AY
            Z(I) = Z(I) + FZ(I)*DT + VAR*AZ
                XR(I) = XR(I) + FX(I)*DT + VAR*AX
                YR(I) = YR(I) + FY(I)*DT + VAR*AY
                ZR(I) = ZR(I) + FZ(I)*DT + VAR*AZ
!   Condiciones periódicas
            X(I) = X(I) - BOXL*ANINT(X(I)/BOXL)
            Y(I) = Y(I) - BOXL*ANINT(Y(I)/BOXL)
            Z(I) = Z(I) - BOXL*ANINT(Z(I)/BOXL)
!   Seguimiento de la partícula NP
            IF(I == NP) THEN
                WRITE(13,*) X(I), Y(I), Z(I)
            END IF            
        25 CONTINUE 
!   Verificar si se debe almacenar la configuración (configuraciones de equilibrio) : CX, CY, CZ
        IF(MOD(L, NFREC) == 0 .AND. L > NENER) THEN
            KI = KI + 1
            DO 30 I = 1, N
                CX(I,KI) = X(I)
                CY(I,KI) = Y(I)
                CZ(I,KI) = Z(I)
            30 CONTINUE
        END IF
!   Verificar si se debe almacenar la configuración (configuraciones de equilibrio) : CXR, CYR, CZR
        IF(MOD(L, NFREC2) == 0 .AND. L > NENER) THEN 
            KI2 = KI2 + 1
            DO 35 I = 1, N
                CXR(I, KI2) = XR(I)
                CYR(I, KI2) = YR(I)
                CZR(I, KI2) = ZR(I)
            35 CONTINUE
        END IF
!   Cálculo de la fuerza y energía por partícula de la configuración actual
        CALL FUERZAS(L, N, BOXL, X, Y, Z, FX, FY, FZ, A, ZK, ENPOT)
!   Almacenamiento de la fuerza y energía por partícula de la configuración actual   
        WRITE(12,*) L, ENPOT/REAL(N) 
!   Verificar si se debe escribir información de ejecución en pantalla
        IF(MOD(L, IPRINT) == 0) THEN
            WRITE(*,*) L, '|', ENPOT/REAL(N)
        END IF
    20 CONTINUE
!   --------------------------------------------------------------------------------------------------|06|



!   ALMACENAR CONFIGURACIOÓN FINAL ----------------|07|
    DO L = 1, N
        WRITE(11,*) X(L), Y(L), Z(L)
    END DO
!   -----------------------------------------------|07|
    
    

!   CÁLCULO DE PROPIEDADES --------------------------------------------------------------|08|
    WRITE(*,*) 'Estamos calculando puntos sobre la funcion de distribucion radial g(r*)'
!   Cálculo de la función de distribución radial g(r*)
    CALL GDR(N, BOXL, DENS, NN2, CX, CY, CZ, KI)  
    WRITE(*,*) 'Estamos calculando las propiedades de autodifusion'  
!   Cálculo de W(t)
    CALL WDT(N, NN2, BOXL, DENS, CXR, CYR, CZR, KI2, DT, NFREC2)
    WRITE(*,*) 'El proceso ha terminado de manera exitosa.'
!   -------------------------------------------------------------------------------------|08|  



!   CERRAR ARCHIVOS -----|09|
    CLOSE(10)
    CLOSE(11)
    CLOSE(12)
!   ---------------------|09|



!   FIN DEL PROGRAMA ----|10|
END PROGRAM DB    


