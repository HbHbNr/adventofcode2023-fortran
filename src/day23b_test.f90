module day23b_test
    use iso_fortran_env, only : int64
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day23b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day23_example.txt')
        call assert_true (154 == result)
    end subroutine

    subroutine test_solve_input
        use day23b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day23_input.txt')
        call assert_true (6654 == result)
    end subroutine

end module day23b_test
