module day06b_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use iso_fortran_env, only : int64
        use day06b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day06_example.txt')
        call assert_true (71503 == result)
    end subroutine

    subroutine test_solve_input
        use iso_fortran_env, only : int64
        use day06b, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day06_input.txt')
        call assert_true (23632299 == result)
    end subroutine

end module day06b_test
