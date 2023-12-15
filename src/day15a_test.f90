module day15a_test
    use iso_fortran_env, only : int64
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day15a, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day15_example.txt')
        call assert_true (1320 == result)
    end subroutine

    subroutine test_solve_input
        use day15a, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day15_input.txt')
        call assert_true (517965 == result)
    end subroutine

end module day15a_test
