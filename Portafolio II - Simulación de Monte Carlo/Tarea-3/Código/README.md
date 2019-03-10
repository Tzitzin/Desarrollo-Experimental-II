# Cambios al código segun la actividad

**Actividad 1:** En el archivo Main_program.f08 sustituir las lineas


    IF(DENS<=0.75) THEN
        CALL CONFIGINI(N, BOXL, X, Y)
    ELSE
        CALL CONFINI_REGULAR_2D(N, BOXL, X, Y)
    END IF
    
    
por


        CALL CONFINI_REGULAR_2D(N, BOXL, X, Y)
        

**Actividad 2:** En el archivo SUB_Configini_aleatoria.f08 eliminar las lineas


     DO 9 J=1, I-1
         Xij=X(I)-X(J)
         Yij=Y(I)-Y(J)
         RO=Xij**2 + Yij**2 
            IF (RO<=SIGMA) THEN
                WRITE(*,*) 'Hay traslape para:', I, J
                GO TO 2
            END IF
     9 CONTINUE



