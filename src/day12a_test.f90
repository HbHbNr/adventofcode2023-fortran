module day12a_test
    use iso_fortran_env, only : int64
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day12a, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day12_example.txt')
        ! call assert_true (21 == result)
        call assert_true (-1 == result)
    end subroutine

    subroutine test_solve_input
        use day12a, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day12_input.txt')
        call assert_true (-1 == result)  ! 12097 is too high
    end subroutine

end module day12a_test
