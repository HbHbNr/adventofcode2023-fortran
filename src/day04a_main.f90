program day04a_main
    use util, only : printresultline_integer
    use day04a, only : solve
    implicit none

    integer :: result

    ! result = solve('inputfiles/day04_example.txt')
    result = solve('inputfiles/day04_input.txt')
    call printresultline_integer('04a', result)
end program day04a_main
