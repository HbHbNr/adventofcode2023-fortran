module day01a_test
    use iso_fortran_env, only : int64
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day01a, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day01_example.txt')
        call assert_true (142 == result)
    end subroutine

    subroutine test_solve_input
        use day01a, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day01_input.txt')
        call assert_true (53921 == result)
    end subroutine

end module day01a_test
