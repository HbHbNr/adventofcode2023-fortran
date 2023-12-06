program day06a_main
    use util, only : printresultline_integer
    use day06a, only : solve
    implicit none

    integer :: result

    ! result = solve('inputfiles/day06_example.txt')
    result = solve('inputfiles/day06_input.txt')
    call printresultline_integer('06a', result)
end program day06a_main
