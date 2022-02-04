PURE SUBROUTINE sub_going_south(ixOld, iyOld, elev, z, ixNew, iyNew)
    USE ISO_FORTRAN_ENV

    IMPLICIT NONE

    ! Declare inputs ...
    INTEGER(kind = INT64), INTENT(in)                                           :: ixOld
    INTEGER(kind = INT64), INTENT(in)                                           :: iyOld
    INTEGER(kind = INT16), DIMENSION(:, :), INTENT(in)                          :: elev
    INTEGER(kind = INT16), INTENT(in)                                           :: z
    INTEGER(kind = INT64), INTENT(out)                                          :: ixNew
    INTEGER(kind = INT64), INTENT(out)                                          :: iyNew

    ! Check if we can go East ...
    IF(elev(ixOld - 1_INT64, iyOld) >= z .AND. elev(ixOld, iyOld) >= z)THEN
        CALL sub_go_east(ixOld, iyOld, ixNew, iyNew)
        RETURN
    END IF

    ! Check if we can go South ...
    IF(elev(ixOld - 1_INT64, iyOld) >= z)THEN
        CALL sub_go_south(ixOld, iyOld, ixNew, iyNew)
        RETURN
    END IF

    ! Assume that we can go West ...
    CALL sub_go_west(ixOld, iyOld, ixNew, iyNew)
END SUBROUTINE sub_going_south
