module day05b_test
    use iso_fortran_env, only : int64
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day05b, only : solve
        implicit none

        integer(int64) :: result
        logical        :: isequal

        result = solve('../inputfiles/day05_example.txt')
        isequal = 46 == result
        call assert_equals (isequal, .true.)
    end subroutine

    subroutine test_solve_input
        use day05b, only : solve
        implicit none

        integer(int64) :: result
        logical        :: isequal

        result = solve('../inputfiles/day05_input.txt')
        isequal = 11554135 == result
        call assert_equals (isequal, .true.)
    end subroutine

end module day05b_test
