program day01a_main
    use util, only : printresultline_integer
    use day01a, only : solve
    implicit none

    integer :: result

    ! result = solve('inputfiles/day01_example.txt')
    result = solve('inputfiles/day01_input.txt')
    call printresultline_integer('01a', result)
end program day01a_main
