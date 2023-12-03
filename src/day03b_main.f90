program day03b_main
    use util, only : printresultline_integer
    use day03b, only : solve
    implicit none

    integer :: result

    ! result = solve('inputfiles/day03_example.txt')
    result = solve('inputfiles/day03_input.txt')
    call printresultline_integer('03b', result)
end program day03b_main
