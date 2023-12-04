program day04b_main
    use util, only : printresultline_integer
    use day04b, only : solve
    implicit none

    integer :: result

    ! result = solve('inputfiles/day04_example.txt')
    result = solve('inputfiles/day04_input.txt')
    call printresultline_integer('04b', result)
end program day04b_main
