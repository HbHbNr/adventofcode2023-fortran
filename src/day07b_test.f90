module day07b_test
    use iso_fortran_env, only : int64
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day07b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day07_example.txt')
        call assert_true (5905 == result)
    end subroutine

    subroutine test_solve_input
        use day07b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day07_input.txt')
        call assert_true (249781879 == result)
    end subroutine

end module day07b_test
