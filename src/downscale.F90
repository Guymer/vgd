PROGRAM main
    USE ISO_FORTRAN_ENV
    USE mod_safe,           ONLY:   sub_allocate_array,                         &
                                    sub_load_array_from_BIN,                    &
                                    sub_save_array_as_BIN,                      &
                                    sub_shrink_array

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
    INTEGER(kind = INT64)                                                       :: iscale
    INTEGER(kind = INT64)                                                       :: shrinkScale

    ! Declare FORTRAN variables ...
    CHARACTER(len = 256)                                                        :: fname
    LOGICAL                                                                     :: fexist

    ! NOTE: The arrays go:
    !       ( 1, 1) ... (nx, 1)
    !         ...         ...
    !       ( 1,ny) ... (nx,ny)

    ! Allocate (1.74 GiB) array and populate it ...
    CALL sub_allocate_array(elev, "elev", nx, ny, .TRUE._INT8)
    CALL sub_load_array_from_BIN(elev, "../data/scale=01km.bin")                ! [m]

    WRITE(fmt = '("For reference, the highest pixel in the un-scaled dataset is ", i4, "m ASL.")', unit = OUTPUT_UNIT) MAXVAL(elev)
    FLUSH(unit = OUTPUT_UNIT)

    ! Loop over scales ...
    DO iscale = 1_INT64, 5_INT64
        ! Determine scale ...
        shrinkScale = 2_INT64 ** iscale                                         ! [km]

        ! Determine file name and skip if it exists ...
        WRITE(fname, fmt = '("../data/scale=", i2.2, "km.bin")') shrinkScale
        INQUIRE(file = TRIM(fname), exist = fexist)
        IF(fexist)THEN
            WRITE(fmt = '("Skipping ", i2, "km.")', unit = OUTPUT_UNIT) shrinkScale
            FLUSH(unit = OUTPUT_UNIT)
            CYCLE
        END IF

        WRITE(fmt = '("Making ", i2, "km ...")', unit = OUTPUT_UNIT) shrinkScale
        FLUSH(unit = OUTPUT_UNIT)

        ! Allocate array and populate it ...
        CALL sub_shrink_array(                                                  &
                     nx = nx,                                                   &
                     ny = ny,                                                   &
                    arr = elev,                                                 &
            shrinkScale = shrinkScale,                                          &
            shrunkenArr = elevScaled                                            &
        )

        ! Save array ...
        CALL sub_save_array_as_BIN(elevScaled, TRIM(fname))

        ! Clean up ...
        DEALLOCATE(elevScaled)
    END DO
END PROGRAM main
