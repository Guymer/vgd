PURE SUBROUTINE sub_go_south(ixOld, iyOld, ixNew, iyNew)
    USE ISO_FORTRAN_ENV

    IMPLICIT NONE

    ! Declare inputs ...
    INTEGER(kind = INT64), INTENT(in)                                           :: ixOld
    INTEGER(kind = INT64), INTENT(in)                                           :: iyOld
    INTEGER(kind = INT64), INTENT(out)                                          :: ixNew
    INTEGER(kind = INT64), INTENT(out)                                          :: iyNew

    ! Move ...
    ixNew = ixOld
    iyNew = iyOld + 1_INT64
END SUBROUTINE sub_go_south
