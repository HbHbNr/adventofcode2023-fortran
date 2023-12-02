program day02b_main
    use util, only : printresultline_integer
    use day02b, only : solve
    implicit none

    integer :: result

    result = solve('inputfiles/day02_example.txt')
    ! result = solve('inputfiles/day02_input.txt')
    call printresultline_integer('02b', result)
end program day02b_main
