module day06a_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day06a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day06_example.txt')
        call assert_equals (288, result)
    end subroutine

    subroutine test_solve_input
        use day06a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day06_input.txt')
        call assert_equals (505494, result)
    end subroutine

end module day06a_test
