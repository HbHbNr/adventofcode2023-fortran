program day03a_main
    use util, only : printresultline_integer
    use day03a, only : solve
    implicit none

    integer :: result

    ! result = solve('inputfiles/day03_example.txt')
    result = solve('inputfiles/day03_input.txt')
    call printresultline_integer('03a', result)
end program day03a_main
