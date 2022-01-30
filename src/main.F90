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

    ! Declare variables ...
    LOGICAL(kind = INT8), ALLOCATABLE, DIMENSION(:, :)                          :: used
    INTEGER(kind = INT16), ALLOCATABLE, DIMENSION(:, :)                         :: elev
    INTEGER(kind = INT64)                                                       :: ix
    INTEGER(kind = INT64)                                                       :: iy
    INTEGER(kind = INT64)                                                       :: ixnew
    INTEGER(kind = INT64)                                                       :: iynew
    INTEGER(kind = INT64)                                                       :: ixold
    INTEGER(kind = INT64)                                                       :: iyold
    REAL(kind = REAL64), ALLOCATABLE, DIMENSION(:)                              :: x
    REAL(kind = REAL64), ALLOCATABLE, DIMENSION(:)                              :: y

    ! Declare FORTRAN variables ...
    ! CHARACTER(len = 256)                                                        :: errmsg
    ! INTEGER(kind = INT32)                                                       :: errnum
    ! INTEGER(kind = INT32)                                                       :: funit

    ! Allocate (337.51 KiB) array and populate it ...
    CALL sub_allocate_array(x, "x", nx + 1_INT64, .TRUE._INT8)
    DO ix = 0_INT64, nx
        x(ix + 1_INT64) = 360.0e0_REAL64 * REAL(ix - nx / 2_INT64, kind = REAL64) / REAL(nx, kind = REAL64) ! [°]
    END DO

    ! Allocate (168.76 KiB) array and populate it ...
    CALL sub_allocate_array(y, "y", ny + 1_INT64, .TRUE._INT8)
    DO iy = 0_INT64, ny
        y(iy + 1_INT64) = 180.0e0_REAL64 * REAL(iy - ny / 2_INT64, kind = REAL64) / REAL(ny, kind = REAL64) ! [°]
    END DO

    ! Allocate (1.74 GiB) array and populate it ...
    CALL sub_allocate_array(elev, "elev", nx, ny, .TRUE._INT8)
    CALL sub_load_array_from_BIN(elev, "../all10g.bin")                         ! [m]

    ! Allocate (889.89 MiB) array and initialize it to say that no pixels have
    ! been used so far  ...
    CALL sub_allocate_array(used, "used", nx, ny, .TRUE._INT8)
    used = .FALSE._INT8

    ! Loop over x-axis ...
    ! NOTE: Do not start in first column as I will do "ix - 1" in a second.
    DO ix = 2_INT64, nx
        ! Loop over y-axis ...
        ! NOTE: Do not start in first row as I will do "iy - 1" in a second.
        DO iy = 2_INT64, ny
            ! Skip this pixel if it is too low ...
            IF(elev(ix, iy) < z)THEN
                CYCLE
            END IF

            ! Skip this pixel if it has been used ...
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

            ixold = ix
            iyold = iy
            used(ixold, iyold) = .TRUE._INT8
            WRITE(*, *) "file", x(ixold), y(iyold)

            CALL sub_go_east(ixold, iyold, ixnew, iynew)
            WRITE(*, *) "file", x(ixnew), y(iynew)

            DO
                IF(ixnew == ix .AND. iynew == iy)THEN
                    WRITE(*, *) "got back to start"
                    EXIT
                END IF

                IF(ixnew == ixold .AND. iynew == iyold + 1_INT64)THEN
                    ixold = ixnew
                    iyold = iynew
                    CALL sub_going_north(ixold, iyold, elev, z, ixnew, iynew)
                    used(ixnew, iynew) = .TRUE._INT8
                    WRITE(*, *) "file", x(ixnew), y(iynew)
                    CYCLE
                END IF

                IF(ixnew == ixold + 1_INT64 .AND. iynew == iyold)THEN
                    ixold = ixnew
                    iyold = iynew
                    CALL sub_going_east(ixold, iyold, elev, z, ixnew, iynew)
                    used(ixnew, iynew) = .TRUE._INT8
                    WRITE(*, *) "file", x(ixnew), y(iynew)
                    CYCLE
                END IF

                IF(ixnew == ixold .AND. iynew == iyold - 1_INT64)THEN
                    ixold = ixnew
                    iyold = iynew
                    CALL sub_going_south(ixold, iyold, elev, z, ixnew, iynew)
                    used(ixnew, iynew) = .TRUE._INT8
                    WRITE(*, *) "file", x(ixnew), y(iynew)
                    CYCLE
                END IF

                IF(ixnew == ixold - 1_INT64 .AND. iynew == iyold)THEN
                    ixold = ixnew
                    iyold = iynew
                    CALL sub_going_west(ixold, iyold, elev, z, ixnew, iynew)
                    used(ixnew, iynew) = .TRUE._INT8
                    WRITE(*, *) "file", x(ixnew), y(iynew)
                    CYCLE
                END IF

                WRITE(*, *) "should not get here"
                STOP
            END DO

            WRITE(*, *) "finished first LinearRing"
            STOP
        END DO
    END DO
END PROGRAM main
