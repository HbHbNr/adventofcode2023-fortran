module day18b_test
    use iso_fortran_env, only : int64
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day18b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day18_example.txt')
        call assert_true (952408144115_int64 == result)
    end subroutine

    subroutine test_solve_input
        use day18b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day18_input.txt')
        call assert_true (131431655002266_int64 == result)
    end subroutine

end module day18b_test
