program day01b_main
    use util, only : printresultline_integer
    use day01b, only : solve
    implicit none

    integer :: result

    ! result = solve('inputfiles/day01_example2.txt')
    result = solve('inputfiles/day01_input.txt')
    call printresultline_integer('01b', result)
end program day01b_main
