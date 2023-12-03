module day03b_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day03b, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day03_example.txt')
        call assert_equals (467835, result)
    end subroutine

    subroutine test_solve_input
        use day03b, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day03_input.txt')
        call assert_equals (84051670, result)
    end subroutine

end module day03b_test
