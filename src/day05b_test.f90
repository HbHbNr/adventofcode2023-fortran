module day05b_test
    use iso_fortran_env, only : int64
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day05b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day05_example.txt')
        call assert_true (46 == result)
    end subroutine

    subroutine test_solve_input
        use day05b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day05_input.txt')
        call assert_true (11554135 == result)
    end subroutine

end module day05b_test
