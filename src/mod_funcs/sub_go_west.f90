PURE SUBROUTINE sub_go_west(ixOld, iyOld, ixNew, iyNew)
    USE ISO_FORTRAN_ENV

    IMPLICIT NONE

    ! Declare inputs ...
    INTEGER(kind = INT64), INTENT(in)                                           :: ixOld
    INTEGER(kind = INT64), INTENT(in)                                           :: iyOld
    INTEGER(kind = INT64), INTENT(out)                                          :: ixNew
    INTEGER(kind = INT64), INTENT(out)                                          :: iyNew

    ! Move ...
    ixNew = ixOld - 1_INT64
    iyNew = iyOld
END SUBROUTINE sub_go_west
