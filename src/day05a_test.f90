module day05a_test
    use iso_fortran_env, only : int64
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day05a, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day05_example.txt')
        call assert_true (35 == result)
    end subroutine

    subroutine test_solve_input
        use day05a, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day05_input.txt')
        call assert_true (282277027 == result)
    end subroutine

end module day05a_test
