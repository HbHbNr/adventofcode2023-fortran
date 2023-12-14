module day14a_test
    use iso_fortran_env, only : int64
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day14a, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day14_example.txt')
        call assert_true (136 == result)
    end subroutine

    subroutine test_solve_input
        use day14a, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day14_input.txt')
        call assert_true (-1 == result)
    end subroutine

end module day14a_test
