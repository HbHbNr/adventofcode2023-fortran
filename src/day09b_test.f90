module day09b_test
    use iso_fortran_env, only : int64
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day09b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day09_example.txt')
        call assert_true (114 == result)
    end subroutine

    subroutine test_solve_input
        use day09b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day09_input.txt')
        call assert_true (2008960228 == result)
    end subroutine

end module day09b_test
