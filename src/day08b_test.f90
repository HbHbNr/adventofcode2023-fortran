module day08b_test
    use iso_fortran_env, only : int64
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example3
        use day08b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day08_example3.txt')
        call assert_true (6 == result)
    end subroutine

    subroutine test_solve_input
        use day08b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day08_input.txt')
        call assert_true (8811050362409_int64 == result)
    end subroutine

end module day08b_test
