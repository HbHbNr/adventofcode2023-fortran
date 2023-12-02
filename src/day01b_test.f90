module day01b_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day01b, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day01_example2.txt')
        call assert_equals (281, result)
    end subroutine

    subroutine test_solve_input
        use day01b, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day01_input.txt')
        call assert_equals (54676, result)
    end subroutine

end module day01b_test
