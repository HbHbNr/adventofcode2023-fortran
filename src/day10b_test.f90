module day10b_test
    use iso_fortran_env, only : int64
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day10b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day10_example.txt')
        call assert_true (1 == result)
    end subroutine

    subroutine test_solve_example2
        use day10b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day10_example2.txt')
        call assert_true (1 == result)
    end subroutine

    subroutine test_solve_example3
        use day10b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day10_example3.txt')
        call assert_true (4 == result)
    end subroutine

    subroutine test_solve_example4
        use day10b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day10_example4.txt')
        call assert_true (8 == result)
    end subroutine

    subroutine test_solve_example5
        use day10b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day10_example5.txt')
        call assert_true (10 == result)
    end subroutine

    subroutine test_solve_input
        use day10b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day10_input.txt')
        call assert_true (325 == result)
    end subroutine

end module day10b_test
