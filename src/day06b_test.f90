module day06b_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day06b, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day06_example.txt')
        call assert_equals (142, result)
    end subroutine

    subroutine test_solve_input
        use day06b, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day06_input.txt')
        call assert_equals (53921, result)
    end subroutine

end module day06b_test
