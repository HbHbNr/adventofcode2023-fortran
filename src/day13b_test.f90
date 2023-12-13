module day13b_test
    use iso_fortran_env, only : int64
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day13b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day13_example.txt')
        call assert_true (400 == result)
    end subroutine

    subroutine test_solve_input
        use day13b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day13_input.txt')
        call assert_true (40995 == result)
    end subroutine

end module day13b_test
