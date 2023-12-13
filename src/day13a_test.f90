module day13a_test
    use iso_fortran_env, only : int64
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day13a, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day13_example.txt')
        call assert_true (-1 == result)
    end subroutine

    subroutine test_solve_input
        use day13a, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day13_input.txt')
        call assert_true (-1 == result)
    end subroutine

end module day13a_test
