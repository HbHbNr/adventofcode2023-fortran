program day07b_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day07b, only : solve
    implicit none

    integer(int64) :: result

    ! result = solve('inputfiles/day07_example.txt')
    result = solve('inputfiles/day07_input.txt')
    call printresultline_int64('07b', result)
end program day07b_main
