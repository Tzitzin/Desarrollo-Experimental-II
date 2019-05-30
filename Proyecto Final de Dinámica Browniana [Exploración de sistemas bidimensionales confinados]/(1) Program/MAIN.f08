!   =======================================================================================================
!   FECHA DE CREACIÓN
!       Monday, April 15, 2019
!
!   INFORMACIÓN DEL PROGRAMA
!       | SIMULACIÓN DE DINÁMICA BROWNIANA EN 2D
!           > MODELO DE POTENCIAL : YUKAWA
!           > UNIDADES REDUCIDAS  : SIGMA-BETA (σ, β)
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
!                                PI = Valor de π
!   SUBRUTINAS NECESARIAS
!       1. Configuración inicial
!           a) Regular (con o sin marialuisa)
!           b) Aleatoria
!       2. FUERZAS: Cálculo de la fuerza de interacción de las partículas y la energía de la configuración.
!       3. GDR:     Cálculo de la función de distribución radial g(r).
!       4. WDT:     Cálculo del desplazamiento cuadrátrico medio y coeficiente de difusión dependiente del 
!                   tiempo.
!       5. ZRAN:    Generador de números aleatorios con distribución uniforme [0,1].
!       6. AZARG:   Generador de números aleatorios con distribución gaussiana (o normal).
!   =======================================================================================================
PROGRAM DB
    IMPLICIT NONE 
        REAL(KIND=8)                    :: DENS, DT, PI, BOXL, RCUT, VAR, A, ZK
        REAL(KIND=8)                    :: AX, AY, AZ, ENPOT, TIME, WT, DIF
        REAL(KIND=8)                    :: A1S, A2S, HSTAR, HCUT, PHI
        REAL(KIND=8), ALLOCATABLE       :: X(:), Y(:), Z(:), FX(:), FY(:), FZ(:), XR(:), YR(:), ZR(:)
        REAL(KIND=8), ALLOCATABLE       :: CX(:,:), CY(:,:), CZ(:,:), CXR(:,:), CYR(:,:), CZR(:,:)
        INTEGER                         :: N, NN2, ISEED, ISEED1, ISEED2, ISEED3
        INTEGER                         :: NSTEP, NENER, NFREC, NFREC2, IPRINT, TYPE
        INTEGER                         :: I, J, K, L
        INTEGER                         :: KI, KI2, SALIDA
        


!   APERTURA DE ARCHIVOS -------------------------------------------------|01|
    OPEN(10,    FILE = '05Informacion.dat',     STATUS = 'UNKNOWN')
    OPEN(11,    FILE = '02confi_fi.dat',        STATUS = 'UNKNOWN')
    OPEN(12,    FILE = '04termalizacion.dat',   STATUS = 'UNKNOWN')
!   ----------------------------------------------------------------------|01|
    
    

!   SOLICITUD DE DATOS DE ENTRADA -----------------------------------------------------------------|02|
    WRITE(*,*) 'Introduce el numero de particulas (N)'
        READ(*,*) N
    WRITE(*,*) 'Introduce la fraccion en area'
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
!   Parámetros del potencial de interacción partículas-pared    
    WRITE(*,*)  '-------------------------------------------------------------------------'
    WRITE(*,*) 'Introduce el valor de A1* (A1S)'
        READ(*,*) A1S
    WRITE(*,*) 'Introduce el valor de A2* (A2S)'
        READ(*,*) A2S
    WRITE(*,*) 'Introduce el valor del ancho (HSTAR)'
        READ(*,*) HSTAR
!   -----------------------------------------------------------------------------------------------|02|



!   CÁLCULOS PREELIMINARES Y CONDICIONES INICIALES -----|03|
!   Condiciones iniciales
        KI  = 0
        KI2 = 0
        L   = 1
        SALIDA = 0
!   Algunos cálculos iniciales
        PI   = 4.0*ATAN(1.0)
        DENS = (4.0E0*PHI)/PI
        BOXL = REAL(N/(DENS*HSTAR),8)
        RCUT = BOXL/2.0E0
        HCUT = HSTAR/2.0E0
        VAR  = SQRT(2.0*DT)
!   --------------- PARÁMETROS DE LOS POTENCIALES DE INTERACCIÓN ---------------   !    
!   Interacción tipo YUKAWA PARTÍCULA-PARTÍCULA
        A = 500.0
        ZK = 0.149
!   ----------------------------------------------------------------------------   !        
!   Semillas para números aleatorios        
        ISEED=7865
        ISEED1=2344
        ISEED2=4334
        ISEED3=9724
!   Asignación de dimensiones
        allocate(X(N))
        allocate(Y(N))
        allocate(XR(N))
        allocate(YR(N))
        allocate(FX(N))
        allocate(FY(N))
        allocate(CX(N,NN2))
        allocate(CY(N,NN2))
        allocate(CXR(N,NN2))
        allocate(CYR(N,NN2))
!   ----------------------------------------------------|03|




!   ESCRIBIR DATOS CAPTURADOS Y CALCULADOS EN UN ARCHIVO ---------------------|04|
    WRITE(10,*) 'DATOS DE LA SIMULACION:'
    WRITE(10,*) '   Numero de particulas            N = ',  N
    WRITE(10,*) '   Configuraciones totales     NSTEP = ',  NSTEP
    WRITE(10,*) '   Configuracion de equilibrio NENER = ',  NENER
    WRITE(10,*) '   Frecuencia de muestreo      NFREC = ',  NFREC
    WRITE(10,*) '   Frecuencia de muestreo     NFREC2 = ',  NFREC2
    WRITE(10,*) '   Tamano del ensemble           NN2 = ',  NN2
    WRITE(10,*) '   Fraccion en volumen           PHI = ',  PHI
    WRITE(10,*) '   Densidad reducida              n* = ',  DENS
    WRITE(10,*) '   Tiempo de paso                 DT = ',  DT
    WRITE(10,*) '   Longitud de la celda           L* = ',  BOXL
    WRITE(10,*) '   Varianza                      VAR = ',  VAR
    WRITE(10,*) '   Tipo de configuracion        TYPE = ',  TYPE
    WRITE(10,*) '   Valor del coeficiente          A* = ',  A
    WRITE(10,*) '   Valor del coeficiente          k* = ',  ZK
    WRITE(10,*) '   Valor del coeficiente         A1* = ',  A1S
    WRITE(10,*) '   Valor del coeficiente         A2* = ',  A2S
    WRITE(10,*) '   Valor del ancho                H* = ',  HSTAR
!   --------------------------------------------------------------------------|04|
!   Interacción tipo YUKAWA PARTÍCULA-PARTÍCULA
    A = A*EXP(ZK)
!   Interacción tipo YUKAWA PARED-PARTÍCULA   
    A1S = A1S*EXP(-ZK*HCUT)
    A2S = A2S*EXP(-ZK*HCUT)


!   ASPECTOS DE LA CONFIGURACIÓN INICIAL -----------------------------------------------------|05|
!   Generar la configuración inicial elegida    
    IF(TYPE == 1) THEN
        CALL CONFINI_REGULAR_3D(N, BOXL, HSTAR, X, Y)
    ELSE
        CALL CONFIGINI(N, BOXL, HSTAR, X, Y)
    END IF
!   Calcular fuerza y energía en la configuración inicial
    CALL FUERZAS(L, N, BOXL, X, Y, FX, FY, A, ZK, A1S, A2S, HSTAR, ENPOT)
    WRITE(*,*)  '-------------------------------------------------------------------------'
    WRITE(*,*)  'Energia de la configuracion inicial = ', ENPOT/REAL(N)
    WRITE(10,*) 'Energia de la configuracion inicial = ', ENPOT/REAL(N)
    WRITE(*,*)  '-------------------------------------------------------------------------'
    WRITE(*,*)  '      ISTEP |         ENERGIA'
!   ------------------------------------------------------------------------------------------|05|



!   COMIENZA EL MOVIMIENTO SEGÚN: MOVIMIENTO BROWNIANO -----------------------------------------------|06|
!   Construcción de configuraciones 
    DO L = 1, NSTEP
!   Movimiento de las partículas 
        DO I = 1, N                                                
            CALL AZARG(ISEED1, AX)
            CALL AZARG(ISEED2, AY)
!   Algoritmo de Ermak
            X(I) = X(I) + FX(I)*DT + VAR*AX
            Y(I) = Y(I) + FY(I)*DT + VAR*AY
                XR(I) = XR(I) + FX(I)*DT + VAR*AX
                YR(I) = YR(I) + FY(I)*DT + VAR*AY
!   Condiciones periódicas
!   Condición para aquellas partículas que se salen de la caja               
        IF(X(I)<-HCUT .OR. X(I)>HCUT) THEN 
            !X(I) = X(I) - BOXL*ANINT(X(I)/BOXL)
            !Call random_number(X(I))
            !X(I) = HSTAR*X(I) - HCUT
            !X(I) = X(I) - HSTAR*ANINT(X(I)/HSTAR)
            X(I) = 0.0
            SALIDA = SALIDA + 1 
        END IF
            Y(I) = Y(I) - BOXL*ANINT(Y(I)/BOXL)
        END DO
!   Verificar si se debe almacenar la configuración (configuraciones de equilibrio) : CX, CY, CZ
        IF(MOD(L, NFREC) == 0 .AND. L > NENER) THEN
            KI = KI + 1
            DO I = 1, N
                CX(I,KI) = X(I)
                CY(I,KI) = Y(I)
            END DO
        END IF
!   Verificar si se debe almacenar la configuración (configuraciones de equilibrio) : CXR, CYR, CZR
        IF(MOD(L, NFREC2) == 0 .AND. L > NENER) THEN 
            KI2 = KI2 + 1
            DO I = 1, N
                CXR(I, KI2) = XR(I)
                CYR(I, KI2) = YR(I)
            END DO
        END IF
!   Cálculo de la fuerza y energía por partícula de la configuración actual
        CALL FUERZAS(L, N, BOXL, X, Y, FX, FY, A, ZK, A1S, A2S, HSTAR, ENPOT)
!   Almacenamiento de la fuerza y energía por partícula de la configuración actual   
        WRITE(12,*) L, ENPOT/REAL(N) 
!   Verificar si se debe escribir información de ejecución en pantalla
        IF(MOD(L, IPRINT) == 0) THEN
            WRITE(*,*) L, '|', ENPOT/REAL(N)
        END IF
    END DO
    WRITE(*,*) '--------------------------------------------------------------------------'
    WRITE(*,*) 'Numero de veces que se salio una particula:', SALIDA
    WRITE(*,*) '--------------------------------------------------------------------------'
!   --------------------------------------------------------------------------------------------------|06|



!   ALMACENAR CONFIGURACIÓN FINAL -----------------|07|
    DO L = 1, N
        WRITE(11,*) X(L), Y(L)
    END DO
!   -----------------------------------------------|07|
    
    

!   CÁLCULO DE PROPIEDADES --------------------------------------------------------------|08|
!   Cálculo del perfil de concentración    
    WRITE(*,*) '--------------------------------------------------------------------------'
    WRITE(*,*) '1. Estamos calculando el perfil de concentracion rho(x)'
    WRITE(*,*) '--------------------------------------------------------------------------'
    CALL RHOX(N, BOXL, DENS, HSTAR, CX, NN2, KI2)
    WRITE(*,*) '--------------------------------------------------------------------------'
    WRITE(*,*) '2. Estamos calculando puntos sobre la funcion de distribucion radial g(y*)'
    WRITE(*,*) '--------------------------------------------------------------------------'
!   Cálculo de la función de distribución radial g(y*)
    CALL GDR(N, BOXL, DENS, HSTAR, NN2, CY, KI)  
    WRITE(*,*) '--------------------------------------------------------------------------'
    WRITE(*,*) '3. Estamos calculando las propiedades de autodifusion'  
    WRITE(*,*) '--------------------------------------------------------------------------'
!   Cálculo de W(t)    
    WRITE(*,*) 'Estamos calculando W(t) paralelo (en Y)'
    CALL WDT(N, NN2, BOXL, DENS, CYR, KI2, DT, NFREC2)  
    WRITE(*,*) 'Estamos calculando W(t) perpendicular (en X)'
    CALL WDTX(N, NN2, HSTAR, DENS, CXR, KI2, DT, NFREC2) 
    WRITE(*,*) '--------------------------------------------------------------------------'
    WRITE(*,*) '4. El proceso ha terminado de manera exitosa.'
    WRITE(*,*) '--------------------------------------------------------------------------'
!   -------------------------------------------------------------------------------------|08|  



!   CERRAR ARCHIVOS -----|09|
    CLOSE(10)
    CLOSE(11)
    CLOSE(12)
!   ---------------------|09|



!   FIN DEL PROGRAMA ----|10|
END PROGRAM DB    


