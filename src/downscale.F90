PROGRAM main
    USE ISO_FORTRAN_ENV
    USE mod_funcs
    USE mod_safe,           ONLY:   sub_allocate_array,                         &
                                    sub_load_array_from_BIN,                    &
                                    sub_save_array_as_BIN

    IMPLICIT NONE

    ! Declare parameters ...
    ! NOTE: The largest scale factor that scales the data without a remainder,
    !       that is also a power of 2, is 32.
    ! NOTE: The highest value in the dataset is 8,752. With a scale factor of 32
    !       then the highest possible total value within a tile is 8,962,048.
    !       This exceeds the 16-bit integer limit (but is within the 32-bit
    !       integer limit).
    INTEGER(kind = INT64), PARAMETER                                            :: nx = 43200_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: ny = 21600_INT64

    ! Declare variables ...
    INTEGER(kind = INT16), ALLOCATABLE, DIMENSION(:, :)                         :: elev
    INTEGER(kind = INT16), ALLOCATABLE, DIMENSION(:, :)                         :: elevScaled
    INTEGER(kind = INT32), ALLOCATABLE, DIMENSION(:, :)                         :: elevTile
    INTEGER(kind = INT64)                                                       :: iscale
    INTEGER(kind = INT64)                                                       :: ix
    INTEGER(kind = INT64)                                                       :: iy
    INTEGER(kind = INT64)                                                       :: ixLo
    INTEGER(kind = INT64)                                                       :: iyLo
    INTEGER(kind = INT64)                                                       :: ixHi
    INTEGER(kind = INT64)                                                       :: iyHi
    INTEGER(kind = INT64)                                                       :: scale
    REAL(kind = REAL64)                                                         :: tileSize

    ! Declare FORTRAN variables ...
    CHARACTER(len = 256)                                                        :: fname
    LOGICAL                                                                     :: fexist

    ! Allocate (1.74 GiB) array and populate it ...
    CALL sub_allocate_array(elev, "elev", nx, ny, .TRUE._INT8)
    CALL sub_load_array_from_BIN(elev, "../data/scale=01km.bin")                ! [m]

    ! Loop over scales ...
    DO iscale = 1_INT64, 5_INT64
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

        ! Determine file name and skip if it exists ...
        WRITE(fname, fmt = '("../data/scale=", i2.2, "km.bin")') scale
        INQUIRE(file = fname, exist = fexist)
        IF(fexist)THEN
            WRITE(fmt = '("Skipping ", i2, "km.")', unit = OUTPUT_UNIT) scale
            FLUSH(unit = OUTPUT_UNIT)
            CYCLE
        END IF

        WRITE(fmt = '("Making ", i2, "km ...")', unit = OUTPUT_UNIT) scale
        FLUSH(unit = OUTPUT_UNIT)

        ! Create short-hand ...
        tileSize = REAL(scale * scale, kind = REAL64)

        ! Allocate arrays ...
        CALL sub_allocate_array(elevScaled, "elevScaled", nx / scale, ny / scale, .TRUE._INT8)
        CALL sub_allocate_array(elevTile, "elevTile", scale, scale, .TRUE._INT8)

        ! Loop over x-axis tiles ...
        DO ix = 1_INT64, nx / scale
            ! Find the extent of the tile ...
            ixlo = (ix - 1_INT64) * scale + 1_INT64
            ixhi =  ix            * scale

            ! Loop over y-axis tiles ...
            DO iy = 1_INT64, ny / scale
                ! Find the extent of the tile ...
                iylo = (iy - 1_INT64) * scale + 1_INT64
                iyhi =  iy            * scale

                ! Increase the precision of the data and assign it to the tile ...
                elevTile = INT(elev(ixlo:ixhi, iylo:iyhi), kind = INT32)        ! [m]

                ! Find the average elevation in this tile ...
                elevScaled(ix, iy) = INT(REAL(SUM(elevTile), kind = REAL64) / tileSize, kind = INT16)   ! [m]
            END DO
        END DO

        ! Clean up ...
        DEALLOCATE(elevTile)

        ! Save array ...
        CALL sub_save_array_as_BIN(elevScaled, TRIM(fname))

        ! Clean up ...
        DEALLOCATE(elevScaled)
    END DO
END PROGRAM main
