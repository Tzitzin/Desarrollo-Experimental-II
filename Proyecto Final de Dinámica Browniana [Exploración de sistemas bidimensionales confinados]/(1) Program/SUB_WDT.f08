!   ====================================================================================|
!   Wednesday, April 17, 2019  
!   
!   DESCRIPCIÓN GENERAL
!               Subrutina para calcular propiedades de autodifusión, desplazamiento cua-
!               drático medio y el coeficiente de disfusión dependiente del tiempo.
!   DESCRIPCIÓN DE VARIABLES
!               Entrada : 
!                       ISEED   = 
!               Salida  :
!                           X   = 
!   ====================================================================================|
SUBROUTINE WDT(N, NN2, BOXL, DENS, CAUX, KI2, DT, NFREC2)
    IMPLICIT NONE
        INTEGER                                         :: I, J, K, NTMAX
        REAL(KIND=8), INTENT(IN)                        :: BOXL, DENS, DT
        INTEGER, INTENT(IN)                             :: N, NFREC2, KI2, NN2
        REAL(KIND=8), DIMENSION(N,NN2), INTENT(IN)      :: CAUX
        REAL(KIND=8)                                    :: TIM, WTAUX, WT, TIME, DIF


!   APERTURA DE ARCHIVOS ----------------------------------------------------------------
    OPEN (16,   FILE = '08WDTY.dat',   STATUS = 'unknown')
    
    
!   VALORES Y CÁLCULOS PREELIMINARES ----------------------------------------------------
    TIM = REAL(NFREC2)*DT

!   CICLO PARA EL BARRIMIENTO TEMPORAL --------------------------------------------------
    DO I = 1, KI2 - 1
        NTMAX = KI2 - I
        WTAUX = 0.d0
        WT = 0.d0

!   CICLO PARA INICIAR EL BARRIMIENTO DE PARTÍCULAS -------------------------------------
        DO J = 1, N
!   CICLO PARA INICIAR EL AVANCE TEMPORAL -----------------------------------------------
            DO K = 1, NTMAX        
!                WTX = WTX + (CXR(J, I+K)-CXR(J,K))**2
                WTAUX = WTAUX + (CAUX(J,I+K)-CAUX(J,K))**2
            END DO
        END DO

        TIME = TIM*REAL(I)
        WT = (WTAUX)/REAL(NTMAX)/REAL(N)/2.D0 
        DIF = WT/TIME

        WRITE(16,*) TIME, WT, DIF
!       IF(TIME .GT. 100) GO TO 11      
        IF(MOD(I,500) == 0) THEN
            WRITE(*,*) I, 'de', KI2-1  
        END IF
    END DO


!   CERRAR ARCHIVOS ---------------------------------------------------------------------
    CLOSE(16)
    
    
!   REGRESAR AL PUNTO DONDE SE LLAMÓ LA SUBRUTINA ---------------------------------------
    RETURN


!   FIN DE LA SUBRUTINA -----------------------------------------------------------------
END SUBROUTINE WDT
    
