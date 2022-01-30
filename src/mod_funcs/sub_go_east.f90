PURE SUBROUTINE sub_go_east(ixold, iyold, ixnew, iynew)
    USE ISO_FORTRAN_ENV

    IMPLICIT NONE

    ! Declare inputs ...
    INTEGER(kind = INT64), INTENT(in)                                           :: ixold
    INTEGER(kind = INT64), INTENT(in)                                           :: iyold
    INTEGER(kind = INT64), INTENT(out)                                          :: ixnew
    INTEGER(kind = INT64), INTENT(out)                                          :: iynew

    ! Move ...
    ixnew = ixold + 1_INT64
    iynew = iyold
END SUBROUTINE sub_go_east
