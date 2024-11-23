PROGRAM main
    ! NOTE: For documentation see:
    !         * https://support.hdfgroup.org/documentation/hdf5/latest/group___f_h5.html
    !         * https://support.hdfgroup.org/documentation/hdf5/latest/group___f_h5_f.html
    !         * https://support.hdfgroup.org/documentation/hdf5/latest/group___f_h5_g.html
    !         * https://support.hdfgroup.org/documentation/hdf5/latest/group___f_h5_l_t.html
    !         * https://support.hdfgroup.org/documentation/hdf5/latest/group___f_h5_t.html
    USE ISO_FORTRAN_ENV,    ONLY:   ERROR_UNIT,                                 &
                                    INT8,                                       &
                                    INT16,                                      &
                                    INT64,                                      &
                                    OUTPUT_UNIT,                                &
                                    REAL64
    USE mod_funcs
    USE mod_safe,           ONLY:   sub_allocate_array,                         &
                                    sub_load_array_from_BIN,                    &
                                    sub_save_array_as_PGM
    USE H5LIB,              ONLY:   H5CLOSE_F,                                  &
                                    H5OPEN_F
    USE H5F,                ONLY:   H5FCLOSE_F,                                 &
                                    H5FCREATE_F
    USE H5G,                ONLY:   H5GCLOSE_F,                                 &
                                    H5GCREATE_F
    USE H5LT,               ONLY:   H5LTMAKE_DATASET_F,                         &
                                    H5LTSET_ATTRIBUTE_INT_F
    USE H5T,                ONLY:   H5F_ACC_TRUNC_F,                            &
                                    H5T_IEEE_F64LE,                             &
                                    HID_T,                                      &
                                    HSIZE_T

    IMPLICIT NONE

    ! Declare parameters ...
    INTEGER(kind = INT64), PARAMETER                                            :: nx = 43200_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: ny = 21600_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: ringMax = 1048576_INT64              ! NOTE: As of 23/Nov/2024 the maximum actually written is 19,441.
    INTEGER(kind = INT64), PARAMETER                                            :: stepMax = 1048576_INT64              ! NOTE: As of 23/Nov/2024 the maximum actually written is 91,083.

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
    REAL(kind = REAL64), ALLOCATABLE, DIMENSION(:)                              :: lats
    REAL(kind = REAL64), ALLOCATABLE, DIMENSION(:)                              :: lons
    REAL(kind = REAL64), ALLOCATABLE, DIMENSION(:)                              :: x
    REAL(kind = REAL64), ALLOCATABLE, DIMENSION(:)                              :: y

    ! Declare FORTRAN variables ...
    CHARACTER(len = 256)                                                        :: errmsg
    CHARACTER(len = 256)                                                        :: dname
    CHARACTER(len = 256)                                                        :: fnameBIN
    CHARACTER(len = 256)                                                        :: fnameHDF
    CHARACTER(len = 256)                                                        :: fnamePGM
    LOGICAL                                                                     :: fexist
    INTEGER                                                                     :: errnum

    ! Declare HDF5 variables ...
    CHARACTER(len = 256)                                                        :: groupName
    INTEGER(kind = HID_T)                                                       :: gUnit
    INTEGER(kind = HID_T)                                                       :: hUnit

    ! NOTE: The arrays go:
    !       ( 1, 1) ... (nx, 1)
    !         ...         ...
    !       ( 1,ny) ... (nx,ny)

    ! Open HDF5 interface ...
    CALL H5OPEN_F(                                                              &
        error = errnum                                                          &
    )
    IF(errnum /= 0)THEN
        WRITE(fmt = '("ERROR: ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "H5OPEN_F() failed", errnum
        FLUSH(unit = ERROR_UNIT)
        STOP
    END IF

    ! Allocate arrays ...
    CALL sub_allocate_array(lats, "lats", stepMax, .TRUE._INT8)
    CALL sub_allocate_array(lons, "lons", stepMax, .TRUE._INT8)

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

        ! Skip this scale if the output would be too big ...
        IF(iscale < 2_INT64)THEN
            CYCLE
        END IF

        ! Loop over elevations ...
        ! NOTE: Rounded to the nearest integer, Mount Everest is 8,849m ASL.
        ! NOTE: The output of "downscale" reports that the highest pixel in the
        !       un-scaled dataset is 8,752m ASL.
        DO z = 250_INT16, 8750_INT16, 250_INT16
            ! Determine directory name and make it ...
            WRITE(dname, fmt = '("../atad/scale=", i2.2, "km")') scale
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

            ! Determine file names ...
            WRITE(fnameHDF, fmt = '("../atad/scale=", i2.2, "km/elev=", i4.4, "m.h5")') scale, z
            WRITE(fnamePGM, fmt = '("../atad/scale=", i2.2, "km/elev=", i4.4, "m.pgm")') scale, z

            ! Skip if the HDF5 exists ...
            INQUIRE(file = TRIM(fnameHDF), exist = fexist)
            IF(fexist)THEN
                WRITE(fmt = '(" > Skipping for elevation of ", i4, "m.")', unit = OUTPUT_UNIT) z
                FLUSH(unit = OUTPUT_UNIT)
                CYCLE
            END IF

            WRITE(fmt = '(" > Searching for elevation of ", i4, "m ...")', unit = OUTPUT_UNIT) z
            FLUSH(unit = OUTPUT_UNIT)

            ! Create HDF5 file ...
            CALL H5FCREATE_F(                                                   &
                access_flags = H5F_ACC_TRUNC_F,                                 &
                        name = TRIM(fnameHDF),                                  &
                     file_id = hUnit,                                           &
                      hdferr = errnum                                           &
            )
            IF(errnum /= 0)THEN
                WRITE(fmt = '("ERROR: ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "H5FCREATE_F() failed", errnum
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

            ! HACK: Make sure that none of the plateaus touch the edge of the
            !       map.
            elev( 1_INT64, :) = 0_INT16                                         ! [m]
            elev(nxScaled, :) = 0_INT16                                         ! [m]
            elev(:,  1_INT64) = 0_INT16                                         ! [m]
            elev(:, nyScaled) = 0_INT16                                         ! [m]

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

                    ! Create HDF5 group ...
                    WRITE(groupName, fmt = '("ring=", i6.6)') ring
                    CALL H5GCREATE_F(                                           &
                        grp_id = gUnit,                                         &
                        hdferr = errnum,                                        &
                        loc_id = hUnit,                                         &
                          name = TRIM(groupName)                                &
                    )
                    IF(errnum /= 0)THEN
                        WRITE(fmt = '("ERROR: ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "H5GCREATE_F() failed", errnum
                        FLUSH(unit = ERROR_UNIT)
                        STOP
                    END IF

                    ! **********************************************************

                    ! Initialize counter and arrays ...
                    step = 0_INT64                                              ! [#]
                    lats = 0.0e0_REAL64                                         ! [°]
                    lons = 0.0e0_REAL64                                         ! [°]

                    ! Set initial location, mark the pixel as being used,
                    ! increment counter and populate arrays ...
                    ixOld = ix                                                  ! [px]
                    iyOld = iy                                                  ! [px]
                    used(ixOld, iyOld) = 0_INT8
                    step = step + 1_INT64                                       ! [#]
                    lats(step) = y(iyOld)                                       ! [°]
                    lons(step) = x(ixOld)                                       ! [°]

                    ! Go eastwards along the northern edge of this pixel,
                    ! increment counter and populate arrays ...
                    CALL sub_go_east(ixOld, iyOld, ixNew, iyNew)
                    step = step + 1_INT64                                       ! [#]
                    lats(step) = y(iyNew)                                       ! [°]
                    lons(step) = x(ixNew)                                       ! [°]

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
                            ! Create HDF5 dataset ...
                            CALL H5LTMAKE_DATASET_F(                            &
                                      buf = lats,                               &
                                     dims = (/ INT(step, kind = HSIZE_T) /),    &
                                dset_name = "lats",                             &
                                  errcode = errnum,                             &
                                   loc_id = gUnit,                              &
                                     rank = 1,                                  &
                                  type_id = H5T_IEEE_F64LE                      &
                            )
                            IF(errnum /= 0)THEN
                                WRITE(fmt = '("ERROR: ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "H5LTMAKE_DATASET_F() failed", errnum
                                FLUSH(unit = ERROR_UNIT)
                                STOP
                            END IF

                            ! Create HDF5 dataset ...
                            CALL H5LTMAKE_DATASET_F(                            &
                                      buf = lons,                               &
                                     dims = (/ INT(step, kind = HSIZE_T) /),    &
                                dset_name = "lons",                             &
                                  errcode = errnum,                             &
                                   loc_id = gUnit,                              &
                                     rank = 1,                                  &
                                  type_id = H5T_IEEE_F64LE                      &
                            )
                            IF(errnum /= 0)THEN
                                WRITE(fmt = '("ERROR: ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "H5LTMAKE_DATASET_F() failed", errnum
                                FLUSH(unit = ERROR_UNIT)
                                STOP
                            END IF

                            WRITE(fmt = '("   > Ring ", i6, " got back to the start after ", i6, " steps.")', unit = OUTPUT_UNIT) ring, step
                            FLUSH(unit = OUTPUT_UNIT)
                            EXIT
                        END IF

                        ! Check if we went north ...
                        IF(ixNew == ixOld .AND. iyNew == iyOld - 1_INT64)THEN
                            ! Move location ...
                            ixOld = ixNew                                       ! [px]
                            iyOld = iyNew                                       ! [px]
                            used(ixOld, iyOld) = 0_INT8

                            ! Go northwards and populate arrays ...
                            CALL sub_going_north(ixOld, iyOld, elev, z, ixNew, iyNew)
                            lats(step) = y(iyNew)                               ! [°]
                            lons(step) = x(ixNew)                               ! [°]
                            CYCLE
                        END IF

                        ! Check if we went east ...
                        IF(ixNew == ixOld + 1_INT64 .AND. iyNew == iyOld)THEN
                            ! Move location ...
                            ixOld = ixNew                                       ! [px]
                            iyOld = iyNew                                       ! [px]
                            used(ixOld - 1_INT64, iyOld) = 0_INT8

                            ! Go eastwards and populate arrays ...
                            CALL sub_going_east(ixOld, iyOld, elev, z, ixNew, iyNew)
                            lats(step) = y(iyNew)                               ! [°]
                            lons(step) = x(ixNew)                               ! [°]
                            CYCLE
                        END IF

                        ! Check if we went south ...
                        IF(ixNew == ixOld .AND. iyNew == iyOld + 1_INT64)THEN
                            ! Move location ...
                            ixOld = ixNew                                       ! [px]
                            iyOld = iyNew                                       ! [px]
                            used(ixOld - 1_INT64, iyOld - 1_INT64) = 0_INT8

                            ! Go southwards and populate arrays ...
                            CALL sub_going_south(ixOld, iyOld, elev, z, ixNew, iyNew)
                            lats(step) = y(iyNew)                               ! [°]
                            lons(step) = x(ixNew)                               ! [°]
                            CYCLE
                        END IF

                        ! Check if we went west ...
                        IF(ixNew == ixOld - 1_INT64 .AND. iyNew == iyOld)THEN
                            ! Move location ...
                            ixOld = ixNew                                       ! [px]
                            iyOld = iyNew                                       ! [px]
                            used(ixOld, iyOld - 1_INT64) = 0_INT8

                            ! Go westwards and populate arrays ...
                            CALL sub_going_west(ixOld, iyOld, elev, z, ixNew, iyNew)
                            lats(step) = y(iyNew)                               ! [°]
                            lons(step) = x(ixNew)                               ! [°]
                            CYCLE
                        END IF

                        ! Catch errors ...
                        WRITE(fmt = '("ERROR: ", a, ".")', unit = ERROR_UNIT) "did not go N/S/E/W"
                        FLUSH(unit = ERROR_UNIT)
                        STOP
                    END DO

                    ! **********************************************************

                    ! Close HDF5 group ...
                    CALL H5GCLOSE_F(                                            &
                        grp_id = gUnit,                                         &
                        hdferr = errnum                                         &
                    )
                    IF(errnum /= 0)THEN
                        WRITE(fmt = '("ERROR: ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "H5GCLOSE_F() failed", errnum
                        FLUSH(unit = ERROR_UNIT)
                        STOP
                    END IF

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

            ! Save mask ...
            CALL sub_save_array_as_PGM(used, TRIM(fnamePGM))

            ! Clean up ...
            DEALLOCATE(x)
            DEALLOCATE(y)
            DEALLOCATE(elev)
            DEALLOCATE(used)

            ! Create HDF5 attribute ...
            CALL H5LTSET_ATTRIBUTE_INT_F(                                       &
                attr_name = "nRings",                                           &
                      buf = (/ INT(ring) /),                                    &
                  errcode = errnum,                                             &
                   loc_id = hUnit,                                              &
                 obj_name = "/",                                                &
                     size = 1_HSIZE_T                                           &
            )
            IF(errnum /= 0)THEN
                WRITE(fmt = '("ERROR: ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "H5LTSET_ATTRIBUTE_INT_F() failed", errnum
                FLUSH(unit = ERROR_UNIT)
                STOP
            END IF

            ! Close HDF5 file ...
            CALL H5FCLOSE_F(                                                    &
                file_id = hUnit,                                                &
                 hdferr = errnum                                                &
            )
            IF(errnum /= 0)THEN
                WRITE(fmt = '("ERROR: ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "H5FCLOSE_F() failed", errnum
                FLUSH(unit = ERROR_UNIT)
                STOP
            END IF
        END DO
    END DO

    ! Clean up ...
    DEALLOCATE(lats)
    DEALLOCATE(lons)

    ! Close HDF5 interface ...
    CALL H5CLOSE_F(                                                             &
        error = errnum                                                          &
    )
    IF(errnum /= 0)THEN
        WRITE(fmt = '("ERROR: ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "H5CLOSE_F() failed", errnum
        FLUSH(unit = ERROR_UNIT)
        STOP
    END IF
END PROGRAM main
