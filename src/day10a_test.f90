module day10a_test
    use iso_fortran_env, only : int64
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day10a, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day10_example.txt')
        call assert_true (4 == result)
    end subroutine

    subroutine test_solve_example2
        use day10a, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day10_example2.txt')
        call assert_true (8 == result)
    end subroutine

    subroutine test_solve_input
        use day10a, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day10_input.txt')
        call assert_true (6846 == result)
    end subroutine

end module day10a_test
