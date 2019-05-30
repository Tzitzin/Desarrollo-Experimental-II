! ============================================================================================
! Subrutina para calcular el perfil de concentración en X para una sistema dos-dimensional
! inhomogeneo a partir de la matriz de configuración CX.
! ============================================================================================

SUBROUTINE RHOX(N, BOXL, DENS, H, CX, NN2, KI2)
    IMPLICIT NONE
        INTEGER, INTENT(IN)                             :: N, NN2, KI2
        REAL(KIND=8), INTENT(IN)                        :: BOXL, H, DENS
        REAL(KIND=8), DIMENSION(N, NN2), INTENT(IN)     :: CX
        REAL(KIND=8)                                    :: DELTAR, XL0, C1, XL, XU, XT, C2, RX
        INTEGER, ALLOCATABLE                            :: NHIST(:)
        INTEGER                                         :: I, J, NBIN, MAXBIN

!   APERTURA DE ARCHIVOS ------------------------------------
    OPEN (20, FILE = '07rhox.dat',      STATUS = 'unknown')  
    
    
!   VALORES Y CÁLCULOS PREELIMINARES ---------
!   Ancho de la 'cinta'    
    DELTAR = 0.01E0
!   Número máximo de entradas del histograma:    
    MAXBIN = INT(H/DELTAR)
!   Dimensión del histograma:
    allocate(NHIST(MAXBIN))    
!   Iniciar conteo en cero cada entrada del histograma de MAXBIN componentes:
    DO I=1, MAXBIN
        NHIST(I) = 0
    END DO    

!   CÁLCULO DE LAS ENTRADAS DEL HISTOGRAMA
    DO I = 1, KI2
        DO J = 1, N 
            XL0 = CX(J,I) + H/2.0E0 
!   Cálculo del NBIN
            NBIN = INT(XL0/DELTAR) + 1
            IF(NBIN .LE. MAXBIN) THEN
                NHIST(NBIN) = NHIST(NBIN) + 1
            END IF
        END DO
    END DO


!   CÁLCULO DEL PERFIL DE CONCENTRACIÓN
    C1 = BOXL*DENS    
    DO NBIN = 1, MAXBIN
        XL = REAL(NBIN-1.0)*DELTAR
        XU = XL + DELTAR
        XT = XL + DELTAR/2.0E0
        C2 = C1*(XU-XL)
        RX = REAL(NHIST(NBIN))/REAL(KI2)/C2
        WRITE(20,*) XT, RX
    END DO


!   CERRAR ARCHIVOS
    CLOSE(20)
    
    
!   FIN DE LA SUBRUTINA
END SUBROUTINE RHOX    




