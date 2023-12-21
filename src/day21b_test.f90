module day21b_test
    use iso_fortran_env, only : int64
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day21b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day21_example.txt', 6)
        call assert_true (16 == result)
    end subroutine

    subroutine test_solve_input
        use day21b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day21_input.txt', 64)
        call assert_true (3660 == result)
    end subroutine

end module day21b_test
