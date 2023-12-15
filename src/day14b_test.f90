module day14b_test
    use iso_fortran_env, only : int64
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day14b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day14_example.txt')
        call assert_true (64 == result)
    end subroutine

    subroutine test_solve_input
        use day14b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day14_input.txt')
        call assert_true (103445 == result)
    end subroutine

end module day14b_test
