module day02a_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day02a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day02_example.txt')
        call assert_equals (8, result)
    end subroutine

    subroutine test_solve_input
        use day02a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day02_input.txt')
        call assert_equals (2795, result)
    end subroutine

end module day02a_test
