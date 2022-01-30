PURE SUBROUTINE sub_going_west(ixold, iyold, elev, z, ixnew, iynew)
    USE ISO_FORTRAN_ENV

    IMPLICIT NONE

    ! Declare inputs ...
    INTEGER(kind = INT64), INTENT(in)                                           :: ixold
    INTEGER(kind = INT64), INTENT(in)                                           :: iyold
    INTEGER(kind = INT16), DIMENSION(:, :), INTENT(in)                          :: elev
    INTEGER(kind = INT16), INTENT(in)                                           :: z
    INTEGER(kind = INT64), INTENT(out)                                          :: ixnew
    INTEGER(kind = INT64), INTENT(out)                                          :: iynew

    ! Check if we can go South ...
    IF(elev(ixold - 1_INT64, iyold + 1_INT64) >= z .AND. elev(ixold - 1_INT64, iyold) >= z)THEN
        CALL sub_go_south(ixold, iyold, ixnew, iynew)
        RETURN
    END IF

    ! Check if we can go West ...
    IF(elev(ixold - 1_INT64, iyold) >= z)THEN
        CALL sub_go_west(ixold, iyold, ixnew, iynew)
        RETURN
    END IF

    ! Assume that we can go North ...
    CALL sub_go_north(ixold, iyold, ixnew, iynew)
END SUBROUTINE sub_going_west
