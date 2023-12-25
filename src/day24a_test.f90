module day24a_test
    use iso_fortran_env, only : int64, real128
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day24a, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day24_example.txt', 7.0_real128, 27.0_real128)
        call assert_true (2 == result)
    end subroutine

    subroutine test_solve_input
        use day24a, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day24_input.txt', 200000000000000.0_real128, 400000000000000.0_real128)
        call assert_true (-1 == result)
    end subroutine

end module day24a_test
