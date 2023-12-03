module day03a_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day03a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day03_example.txt')
        call assert_equals (4361, result)
    end subroutine

    subroutine test_solve_input
        use day03a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day03_input.txt')
        call assert_equals (532428, result)
    end subroutine

end module day03a_test
