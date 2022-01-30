PROGRAM main
    USE ISO_FORTRAN_ENV
    USE mod_funcs
    USE mod_safe,           ONLY:   sub_allocate_array,                         &
                                    sub_load_array_from_BIN

    IMPLICIT NONE

    ! Declare parameters ...
    INTEGER(kind = INT16), PARAMETER                                            :: z = 5000_INT16
    INTEGER(kind = INT64), PARAMETER                                            :: nx = 43200_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: ny = 21600_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: stepMax = 1000_INT64

    ! Declare variables ...
    LOGICAL(kind = INT8), ALLOCATABLE, DIMENSION(:, :)                          :: used
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
    INTEGER(kind = INT64)                                                       :: scale
    INTEGER(kind = INT64)                                                       :: step
    REAL(kind = REAL64), ALLOCATABLE, DIMENSION(:)                              :: x
    REAL(kind = REAL64), ALLOCATABLE, DIMENSION(:)                              :: y

    ! Declare FORTRAN variables ...
    ! CHARACTER(len = 256)                                                        :: errmsg
    CHARACTER(len = 256)                                                        :: fname
    LOGICAL                                                                     :: fexist
    ! INTEGER(kind = INT32)                                                       :: errnum
    ! INTEGER(kind = INT32)                                                       :: funit

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
        WRITE(fname, fmt = '("../data/scale=", i2.2, "km.bin")') scale
        INQUIRE(file = TRIM(fname), exist = fexist)
        IF(.NOT. fexist)THEN
            WRITE(fmt = '("Skipping ", i2, "km.")', unit = OUTPUT_UNIT) scale
            FLUSH(unit = OUTPUT_UNIT)
            CYCLE
        END IF

        WRITE(fmt = '("Vectorising ", i2, "km ...")', unit = OUTPUT_UNIT) scale
        FLUSH(unit = OUTPUT_UNIT)

        IF(iscale /= 5_INT64)THEN
            CYCLE
        END IF

        ! Create short-hands ...
        nxScaled = nx / scale                                                   ! [px]
        nyScaled = ny / scale                                                   ! [px]

        ! Allocate array and populate it ...
        CALL sub_allocate_array(x, "x", nxScaled + 1_INT64, .TRUE._INT8)
        DO ix = 0_INT64, nxScaled
            x(ix + 1_INT64) = 360.0e0_REAL64 * REAL(ix - nxScaled / 2_INT64, kind = REAL64) / REAL(nxScaled, kind = REAL64) ! [°]
        END DO

        ! Allocate array and populate it ...
        CALL sub_allocate_array(y, "y", nyScaled + 1_INT64, .TRUE._INT8)
        DO iy = 0_INT64, nyScaled
            y(iy + 1_INT64) = 180.0e0_REAL64 * REAL(iy - nyScaled / 2_INT64, kind = REAL64) / REAL(nyScaled, kind = REAL64) ! [°]
        END DO

        ! Allocate array and populate it ...
        CALL sub_allocate_array(elev, "elev", nxScaled, nyScaled, .TRUE._INT8)
        CALL sub_load_array_from_BIN(elev, TRIM(fname))                         ! [m]

        ! Allocate array and initialize it to say that no pixels have been used
        ! so far  ...
        CALL sub_allocate_array(used, "used", nxScaled, nyScaled, .TRUE._INT8)
        used = .FALSE._INT8

        ! Loop over x-axis ...
        ! NOTE: Do not start in first column as I will do "ix - 1" in a second.
        DO ix = 2_INT64, nxScaled
            ! Loop over y-axis ...
            ! NOTE: Do not start in first row as I will do "iy - 1" in a second.
            DO iy = 2_INT64, nyScaled
                ! Skip this pixel if it is too low ...
                IF(elev(ix, iy) < z)THEN
                    CYCLE
                END IF

                ! Skip this pixel if it has been used in a previous LinearRing ...
                IF(used(ix, iy))THEN
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

                WRITE(*, *) "starting LinearRing at", x(ix), y(iy)

                ! ******************************************************************

                ixOld = ix
                iyOld = iy
                used(ixOld, iyOld) = .TRUE._INT8
                WRITE(*, *) "file", x(ixOld), y(iyOld)

                CALL sub_go_east(ixOld, iyOld, ixNew, iyNew)
                WRITE(*, *) "file", x(ixNew), y(iyNew)

                step = 0_INT64
                DO
                    step = step + 1_INT64
                    IF(step >= stepMax)THEN
                        WRITE(*, *) "ERROR: too manyScaled steps"
                        STOP
                    END IF

                    IF(ixNew == ix .AND. iyNew == iy)THEN
                        WRITE(*, *) "got back to start after", step, "steps"
                        EXIT
                    END IF

                    IF(ixNew == ixOld .AND. iyNew == iyOld + 1_INT64)THEN
                        ixOld = ixNew
                        iyOld = iyNew
                        CALL sub_going_north(ixOld, iyOld, elev, z, ixNew, iyNew)
                        used(ixNew, iyNew) = .TRUE._INT8
                        WRITE(*, *) "file", x(ixNew), y(iyNew)
                        CYCLE
                    END IF

                    IF(ixNew == ixOld + 1_INT64 .AND. iyNew == iyOld)THEN
                        ixOld = ixNew
                        iyOld = iyNew
                        CALL sub_going_east(ixOld, iyOld, elev, z, ixNew, iyNew)
                        used(ixNew, iyNew) = .TRUE._INT8
                        WRITE(*, *) "file", x(ixNew), y(iyNew)
                        CYCLE
                    END IF

                    IF(ixNew == ixOld .AND. iyNew == iyOld - 1_INT64)THEN
                        ixOld = ixNew
                        iyOld = iyNew
                        CALL sub_going_south(ixOld, iyOld, elev, z, ixNew, iyNew)
                        used(ixNew, iyNew) = .TRUE._INT8
                        WRITE(*, *) "file", x(ixNew), y(iyNew)
                        CYCLE
                    END IF

                    IF(ixNew == ixOld - 1_INT64 .AND. iyNew == iyOld)THEN
                        ixOld = ixNew
                        iyOld = iyNew
                        CALL sub_going_west(ixOld, iyOld, elev, z, ixNew, iyNew)
                        used(ixNew, iyNew) = .TRUE._INT8
                        WRITE(*, *) "file", x(ixNew), y(iyNew)
                        CYCLE
                    END IF

                    WRITE(*, *) "ERROR: should not get here"
                    STOP
                END DO

                WRITE(*, *) "finished LinearRing"
                STOP
            END DO
        END DO

        ! Clean up ...
        DEALLOCATE(x)
        DEALLOCATE(y)
        DEALLOCATE(elev)
        DEALLOCATE(used)
    END DO
END PROGRAM main
