module day15b_test
    use iso_fortran_env, only : int64
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day15b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day15_example.txt')
        call assert_true (145 == result)
    end subroutine

    subroutine test_solve_input
        use day15b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day15_input.txt')
        call assert_true (267372 == result)
    end subroutine

end module day15b_test
