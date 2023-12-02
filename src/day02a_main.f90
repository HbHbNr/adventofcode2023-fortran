program day02a_main
    use util, only : printresultline_integer
    use day02a, only : solve
    implicit none

    integer :: result

    result = solve('inputfiles/day02_example.txt')
    ! result = solve('inputfiles/day02_input.txt')
    call printresultline_integer('02a', result)
end program day02a_main
