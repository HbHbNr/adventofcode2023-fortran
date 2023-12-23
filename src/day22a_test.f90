module day22a_test
    use iso_fortran_env, only : int64
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day22a, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day22_example.txt', 2)
        ! call assert_true (5 == result)
        call assert_true (-1 == result)
    end subroutine

    subroutine test_solve_input
        use day22a, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day22_input.txt', 9)
        call assert_true (-1 == result)
    end subroutine

end module day22a_test
