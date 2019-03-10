# Cambios al código segun la actividad

**Actividad 1:** En el archivo Main_program.f08 sustituir las lineas
    IF(DENS<=0.75) THEN
        CALL CONFIGINI(N, BOXL, X, Y)
    ELSE
        CALL CONFINI_REGULAR_2D(N, BOXL, X, Y)
    END IF
por
        CALL CONFINI_REGULAR_2D(N, BOXL, X, Y)
