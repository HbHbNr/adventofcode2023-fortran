module day07a_test
    use iso_fortran_env, only : int64
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day07a, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day07_example.txt')
        call assert_true (6440 == result)
    end subroutine

    subroutine test_solve_input
        use day07a, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day07_input.txt')
        call assert_true (251058093 == result)
    end subroutine

end module day07a_test
