module day08a_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_solve_example
        use day08a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day08_example.txt')
        call assert_equals (2, result)
    end subroutine

    subroutine test_solve_example2
        use day08a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day08_example2.txt')
        call assert_equals (6, result)
    end subroutine

    subroutine test_solve_input
        use day08a, only : solve
        implicit none

        integer :: result

        result = solve('../inputfiles/day08_input.txt')
        call assert_equals (19637, result)
    end subroutine

end module day08a_test
