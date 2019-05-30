!   ====================================================================================|
!   Wednesday, April 17, 2019  
!   
!   DESCRIPCIÓN GENERAL
!               S
!   DESCRIPCIÓN DE VARIABLES
!               Entrada : 
!                       ISEED   = 
!               Salida  :
!                           X   =  
!   ====================================================================================|

SUBROUTINE FUERZAS(L, N, BOXL, X, Y, FX, FY, A, ZK, A1S, A2S, HSTAR, ENPOT)
    IMPLICIT NONE 
        REAL(KIND=8), INTENT(IN)                :: BOXL, A, ZK, A1S, A2S, HSTAR
        REAL(KIND=8), INTENT(OUT)               :: ENPOT
        REAL(KIND=8), DIMENSION(N), INTENT(IN)  :: X, Y
        REAL(KIND=8), DIMENSION(N), INTENT(OUT) :: FX, FY
        INTEGER, INTENT(IN)                     :: N, L
        REAL(KIND=8)                            :: RCUT, XIJ, YIJ, RIJ
        REAL(KIND=8)                            :: FXI, FYI, U, U2, FXIJ, FYIJ
        REAL(KIND=8)                            :: FP
        INTEGER                                 :: I, J 


!   APERTURA DE ARCHIVOS ---------------------------------------------------------------|
!        OPEN (14, FILE = '04termalizacion.dat', STATUS = 'unknown')

!   VALORES Y CÁLCULOS PREELIMINARES ---------------------------------------------------|
    RCUT = BOXL/2.0  
    ENPOT = 0.0
    !FPARED = 0.0
    
    
!   INICIAR EL VALOR DE LA FUERZA EN CERO ----------------------------------------------|
    DO I = 1, N
        FX(I) = 0.0
        FY(I) = 0.0
    END DO


!   CÁLCULO DE FUERZAS -----------------------------------------------------------------|
    DO 20 I = 1, N-1
        FXI = FX(I)
        FYI = FY(I)
            DO 25 J = I+1, N
!   Distancia (por componentes) entre la partícula I y la partícula J
                XIJ = X(I)-X(J)
                YIJ = Y(I)-Y(J)
!   Condición de imágen mínima
!                XIJ = XIJ - BOXL*DNINT(XIJ/BOXL)
                YIJ = YIJ - BOXL*DNINT(YIJ/BOXL)
!   Distancia entre la partícula I y la partícula J
                RIJ = SQRT(XIJ**2 + YIJ**2)
!   Verificar si hay traslapes entre partículas
                    IF(RIJ .LE. 1.D0) THEN
                        WRITE(*,*) 'Hay traslape entre', I, 'y', J
                    !   STOP
                    END IF
!   MODELO DE INTERACCIÓN
                IF(RIJ .LT. RCUT) THEN
                    U  = EXP(-ZK*RIJ)
                    U2 = A*U*(ZK*RIJ+1.D0)/RIJ**3
                    ENPOT = (A*U)/RIJ+ENPOT
!   Fuerza (por componentes) entre la partícula I y la partícula J
                    FXIJ = XIJ*U2
                    FYIJ = YIJ*U2

                    FXI = FXI + FXIJ
                    FYI = FYI + FYIJ

                    FX(J) = FX(J) - FXIJ
                    FY(J) = FY(J) - FYIJ
                END IF
            25 CONTINUE 
!   Interacción pared-partícula             
        FP = ZK*(A1S*EXP(-ZK*X(I)) - A2S*EXP(ZK*X(I)))
        FX(I) = FXI + FP
        FY(I) = FYI       



    20 CONTINUE 


!   ENERGÍA DE LA CONFIGURACIÓN (PARA OBSERVAR TERMALIZACIÓN) --------------------------|
!    WRITE(*,*) L, ENPOT/REAL(N)


!   CERRAR ARCHIVOS --------------------------------------------------------------------|
    !CLOSE(14)


!   REGRESAR AL PUNTO DONDE SE LLAMÓ A LA SUBRUTINA ------------------------------------|
    RETURN
    
    
!   FIN DE LA SUBRUTINA ----------------------------------------------------------------|
END SUBROUTINE     


