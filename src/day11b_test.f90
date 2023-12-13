module day11b_test
    use iso_fortran_env, only : int64
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day11b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day11_example.txt')
        call assert_true (82000210 == result)
    end subroutine

    subroutine test_solve_input
        use day11b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day11_input.txt')
        call assert_true (710674907809_int64 == result)
    end subroutine

end module day11b_test
