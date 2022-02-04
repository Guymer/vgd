PROGRAM main
    USE ISO_FORTRAN_ENV
    USE mod_funcs
    USE mod_safe,           ONLY:   sub_allocate_array,                         &
                                    sub_load_array_from_BIN,                    &
                                    sub_save_array_as_PGM

    IMPLICIT NONE

    ! Declare parameters ...
    INTEGER(kind = INT64), PARAMETER                                            :: nx = 43200_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: ny = 21600_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: ringMax = 1048576_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: stepMax = 1073741824_INT64

    ! Declare variables ...
    INTEGER(kind = INT8), ALLOCATABLE, DIMENSION(:, :)                          :: used
    INTEGER(kind = INT16)                                                       :: z
    INTEGER(kind = INT16), ALLOCATABLE, DIMENSION(:, :)                         :: elev
    INTEGER(kind = INT64)                                                       :: iscale
    INTEGER(kind = INT64)                                                       :: ix
    INTEGER(kind = INT64)                                                       :: iy
    INTEGER(kind = INT64)                                                       :: ixNew
    INTEGER(kind = INT64)                                                       :: iyNew
    INTEGER(kind = INT64)                                                       :: ixOld
    INTEGER(kind = INT64)                                                       :: iyOld
    INTEGER(kind = INT64)                                                       :: nxScaled
    INTEGER(kind = INT64)                                                       :: nyScaled
    INTEGER(kind = INT64)                                                       :: ring
    INTEGER(kind = INT64)                                                       :: scale
    INTEGER(kind = INT64)                                                       :: step
    REAL(kind = REAL64), ALLOCATABLE, DIMENSION(:)                              :: x
    REAL(kind = REAL64), ALLOCATABLE, DIMENSION(:)                              :: y

    ! Declare FORTRAN variables ...
    CHARACTER(len = 256)                                                        :: errmsg
    CHARACTER(len = 256)                                                        :: dname
    CHARACTER(len = 256)                                                        :: fnameBIN
    CHARACTER(len = 256)                                                        :: fnameCSV
    CHARACTER(len = 256)                                                        :: fnamePGM
    LOGICAL                                                                     :: fexist
    INTEGER                                                                     :: errnum
    INTEGER                                                                     :: funit

    ! Loop over scales ...
    DO iscale = 0_INT64, 5_INT64
        ! Determine scale ...
        scale = 2_INT64 ** iscale                                               ! [km]

        ! Check scale ...
        IF(MOD(nx, scale) /= 0_INT64)THEN
            WRITE(fmt = '("ERROR: ", a, ".")', unit = ERROR_UNIT) '"nx" is not an integer multiple of "scale"'
            FLUSH(unit = ERROR_UNIT)
            STOP
        END IF
        IF(MOD(ny, scale) /= 0_INT64)THEN
            WRITE(fmt = '("ERROR: ", a, ".")', unit = ERROR_UNIT) '"ny" is not an integer multiple of "scale"'
            FLUSH(unit = ERROR_UNIT)
            STOP
        END IF

        ! Determine file name and skip if it is missing ...
        WRITE(fnameBIN, fmt = '("../data/scale=", i2.2, "km.bin")') scale
        INQUIRE(file = TRIM(fnameBIN), exist = fexist)
        IF(.NOT. fexist)THEN
            WRITE(fmt = '("Skipping ", i2, "km.")', unit = OUTPUT_UNIT) scale
            FLUSH(unit = OUTPUT_UNIT)
            CYCLE
        END IF

        WRITE(fmt = '("Vectorising ", i2, "km ...")', unit = OUTPUT_UNIT) scale
        FLUSH(unit = OUTPUT_UNIT)

        ! HACK
        IF(iscale /= 5_INT64)THEN
            CYCLE
        END IF

        ! Loop over elevations ...
        DO z = 2000_INT16, 9000_INT16, 500_INT16
            WRITE(fmt = '(" > Searching for elevation of ", i4, "m ...")', unit = OUTPUT_UNIT) z
            FLUSH(unit = OUTPUT_UNIT)

            ! Determine directory name and make it ...
            WRITE(dname, fmt = '("../data/scale=", i2.2, "km/elev=", i4.4, "m")') scale, z
            CALL EXECUTE_COMMAND_LINE(                                          &
                "mkdir -p " // TRIM(dname),                                     &
                  cmdmsg = errmsg,                                              &
                exitstat = errnum                                               &
            )
            IF(errnum /= 0)THEN
                WRITE(fmt = '("ERROR: ", a, ". ERRMSG = ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "mkdir failed", TRIM(errmsg), errnum
                FLUSH(unit = ERROR_UNIT)
                STOP
            END IF

            ! Create short-hands ...
            nxScaled = nx / scale                                               ! [px]
            nyScaled = ny / scale                                               ! [px]

            ! Allocate array and populate it ...
            CALL sub_allocate_array(x, "x", nxScaled + 1_INT64, .TRUE._INT8)
            DO ix = 0_INT64, nxScaled
                x(ix + 1_INT64) = 360.0e0_REAL64 * (REAL(ix, kind = REAL64) - 0.5e0_REAL64 * REAL(nxScaled, kind = REAL64)) / REAL(nxScaled, kind = REAL64) ! [°]
            END DO

            ! Allocate array and populate it ...
            CALL sub_allocate_array(y, "y", nyScaled + 1_INT64, .TRUE._INT8)
            DO iy = 0_INT64, nyScaled
                y(iy + 1_INT64) = 180.0e0_REAL64 * (0.5e0_REAL64 * REAL(nyScaled, kind = REAL64) - REAL(iy, kind = REAL64)) / REAL(nyScaled, kind = REAL64) ! [°]
            END DO

            ! Allocate array and populate it ...
            CALL sub_allocate_array(elev, "elev", nxScaled, nyScaled, .TRUE._INT8)
            CALL sub_load_array_from_BIN(elev, TRIM(fnameBIN))                  ! [m]

            ! Allocate array and initialize it to say that no pixels have been
            ! used so far  ...
            CALL sub_allocate_array(used, "used", nxScaled, nyScaled, .TRUE._INT8)
            used = 127_INT8

            ! Initialize counter ...
            ring = 0_INT64                                                      ! [#]

            ! Loop over x-axis ...
            DO ix = 2_INT64, nxScaled - 1_INT64
                ! Loop over y-axis ...
                DO iy = 2_INT64, nyScaled - 1_INT64
                    ! Skip this pixel if it is too low ...
                    IF(elev(ix, iy) < z)THEN
                        CYCLE
                    END IF

                    ! Skip this pixel if it has been used in a previous
                    ! LinearRing ...
                    IF(used(ix, iy) == 0_INT8)THEN
                        CYCLE
                    END IF

                    ! Skip this pixel if it is not a local top-left corner ...
                    IF(elev(ix - 1_INT64, iy) >= z)THEN
                        CYCLE
                    END IF
                    IF(elev(ix - 1_INT64, iy - 1_INT64) >= z)THEN
                        CYCLE
                    END IF
                    IF(elev(ix, iy - 1_INT64) >= z)THEN
                        CYCLE
                    END IF

                    ! Determine file name ...
                    WRITE(fnameCSV, fmt = '("../data/scale=", i2.2, "km/elev=", i4.4, "m/ring=", i6.6, ".csv")') scale, z, ring

                    ! Open CSV ...
                    OPEN(                                                       &
                         action = "WRITE",                                      &
                           file = TRIM(fnameCSV),                               &
                           form = "FORMATTED",                                  &
                          iomsg = errmsg,                                       &
                         iostat = errnum,                                       &
                        newunit = funit,                                        &
                           sign = "PLUS",                                       &
                         status = "REPLACE"                                     &
                    )
                    IF(errnum /= 0)THEN
                        WRITE(fmt = '("ERROR: ", a, ". ERRMSG = ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "failed to open CSV", TRIM(errmsg), errnum
                        FLUSH(unit = ERROR_UNIT)
                        STOP
                    END IF

                    ! Write header ...
                    WRITE(fmt = '(a)', unit = funit) "lon,lat"

                    ! **********************************************************

                    ! Set initial location ...
                    ixOld = ix                                                  ! [px]
                    iyOld = iy                                                  ! [px]
                    used(ixOld, iyOld) = 0_INT8
                    WRITE(fmt = '(f11.6, ",", f11.6)', unit = funit) x(ixOld), y(iyOld)

                    ! Go eastwards along the northern edge of this pixel ...
                    CALL sub_go_east(ixOld, iyOld, ixNew, iyNew)
                    WRITE(fmt = '(f11.6, ",", f11.6)', unit = funit) x(ixNew), y(iyNew)

                    ! Initialize counter ...
                    step = 0_INT64                                              ! [#]

                    ! Start infinite loop ...
                    DO
                        ! Increment counter and crash if too many steps have
                        ! been made ...
                        step = step + 1_INT64                                   ! [#]
                        IF(step >= stepMax)THEN
                            WRITE(fmt = '("ERROR: ", a, ".")', unit = ERROR_UNIT) "exceeded stepMax"
                            FLUSH(unit = ERROR_UNIT)
                            STOP
                        END IF

                        ! Stop looping if we are back at the start ...
                        IF(ixNew == ix .AND. iyNew == iy)THEN
                            WRITE(fmt = '("   > Ring ", i6, " got back to the start after ", i6, " steps.")', unit = OUTPUT_UNIT) ring, step
                            FLUSH(unit = OUTPUT_UNIT)
                            EXIT
                        END IF

                        WRITE(*, *) ixNew, nxScaled, iyNew, nyScaled

                        ! Check if we went north ...
                        IF(ixNew == ixOld .AND. iyNew == iyOld - 1_INT64)THEN
                            ! Check if we have hit the northern limit of Earth ...
                            IF(iyNew == 1_INT64)THEN
                                ! Move location ...
                                ixOld = ixNew                                   ! [px]
                                iyOld = iyNew                                   ! [px]

                                ! Go east ...
                                CALL sub_go_east(ixOld, iyOld, ixNew, iyNew)
                                CYCLE
                            END IF

                            ! Move location ...
                            ixOld = ixNew                                       ! [px]
                            iyOld = iyNew                                       ! [px]
                            used(ixOld, iyOld) = 0_INT8

                            ! Go northwards ...
                            CALL sub_going_north(ixOld, iyOld, elev, z, ixNew, iyNew)
                            WRITE(fmt = '(f11.6, ",", f11.6)', unit = funit) x(ixNew), y(iyNew)
                            CYCLE
                        END IF

                        ! Check if we went east ...
                        IF(ixNew == ixOld + 1_INT64 .AND. iyNew == iyOld)THEN
                            ! Check if we have hit the eastern limit of Earth ...
                            IF(ixNew == nxScaled + 1_INT64)THEN
                                ! Move location ...
                                ixOld = ixNew                                   ! [px]
                                iyOld = iyNew                                   ! [px]

                                ! Go south ...
                                CALL sub_go_south(ixOld, iyOld, ixNew, iyNew)
                                CYCLE
                            END IF

                            ! Move location ...
                            ixOld = ixNew                                       ! [px]
                            iyOld = iyNew                                       ! [px]
                            used(ixOld - 1_INT64, iyOld) = 0_INT8

                            ! Go eastwards ...
                            CALL sub_going_east(ixOld, iyOld, elev, z, ixNew, iyNew)
                            WRITE(fmt = '(f11.6, ",", f11.6)', unit = funit) x(ixNew), y(iyNew)
                            CYCLE
                        END IF

                        ! Check if we went south ...
                        IF(ixNew == ixOld .AND. iyNew == iyOld + 1_INT64)THEN
                            ! Check if we have hit the southern limit of Earth ...
                            IF(iyNew == nyScaled + 1_INT64)THEN
                                ! Move location ...
                                ixOld = ixNew                                   ! [px]
                                iyOld = iyNew                                   ! [px]

                                ! Go west ...
                                CALL sub_go_west(ixOld, iyOld, ixNew, iyNew)
                                CYCLE
                            END IF

                            ! Move location ...
                            ixOld = ixNew                                       ! [px]
                            iyOld = iyNew                                       ! [px]
                            used(ixOld - 1_INT64, iyOld - 1_INT64) = 0_INT8

                            ! Go southwards ...
                            CALL sub_going_south(ixOld, iyOld, elev, z, ixNew, iyNew)
                            WRITE(fmt = '(f11.6, ",", f11.6)', unit = funit) x(ixNew), y(iyNew)
                            CYCLE
                        END IF

                        ! Check if we went west ...
                        IF(ixNew == ixOld - 1_INT64 .AND. iyNew == iyOld)THEN
                            ! Check if we have hit the western limit of Earth ...
                            IF(ixNew == 1_INT64)THEN
                                ! Move location ...
                                ixOld = ixNew                                   ! [px]
                                iyOld = iyNew                                   ! [px]

                                ! Go north ...
                                CALL sub_go_north(ixOld, iyOld, ixNew, iyNew)
                                CYCLE
                            END IF

                            ! Move location ...
                            ixOld = ixNew                                       ! [px]
                            iyOld = iyNew                                       ! [px]
                            used(ixOld, iyOld - 1_INT64) = 0_INT8

                            ! Go westwards ...
                            CALL sub_going_west(ixOld, iyOld, elev, z, ixNew, iyNew)
                            WRITE(fmt = '(f11.6, ",", f11.6)', unit = funit) x(ixNew), y(iyNew)
                            CYCLE
                        END IF

                        ! Catch errors ...
                        WRITE(fmt = '("ERROR: ", a, ".")', unit = ERROR_UNIT) "did not go N/S/E/W"
                        FLUSH(unit = ERROR_UNIT)
                        STOP
                    END DO

                    ! **********************************************************

                    ! Close CSV ...
                    CLOSE(unit = funit)

                    ! Increment counter and crash if too many rings have been
                    ! made ...
                    ring = ring + 1_INT64                                       ! [#]
                    IF(ring >= ringMax)THEN
                        WRITE(fmt = '("ERROR: ", a, ".")', unit = ERROR_UNIT) "exceeded ringMax"
                        FLUSH(unit = ERROR_UNIT)
                        STOP
                    END IF
                END DO
            END DO

            ! Determine file name ...
            WRITE(fnamePGM, fmt = '("../data/scale=", i2.2, "km/elev=", i4.4, "m.pgm")') scale, z

            ! Save mask ...
            CALL sub_save_array_as_PGM(used, TRIM(fnamePGM))

            ! Clean up ...
            DEALLOCATE(x)
            DEALLOCATE(y)
            DEALLOCATE(elev)
            DEALLOCATE(used)
        END DO
    END DO
END PROGRAM main
