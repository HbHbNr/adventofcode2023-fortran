module day04a_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day04a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day04_example.txt')
        call assert_equals (13, result)
    end subroutine

    subroutine test_solve_input
        use day04a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day04_input.txt')
        call assert_equals (15268, result)
    end subroutine

end module day04a_test
