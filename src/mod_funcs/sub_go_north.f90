PURE SUBROUTINE sub_go_north(ixold, iyold, ixnew, iynew)
    USE ISO_FORTRAN_ENV

    IMPLICIT NONE

    ! Declare inputs ...
    INTEGER(kind = INT64), INTENT(in)                                           :: ixold
    INTEGER(kind = INT64), INTENT(in)                                           :: iyold
    INTEGER(kind = INT64), INTENT(out)                                          :: ixnew
    INTEGER(kind = INT64), INTENT(out)                                          :: iynew

    ! Move ...
    ixnew = ixold
    iynew = iyold + 1_INT64
END SUBROUTINE sub_go_north
