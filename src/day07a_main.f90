program day07a_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day07a, only : solve
    implicit none

    integer(int64) :: result

    ! result = solve('inputfiles/day07_example.txt')
    result = solve('inputfiles/day07_input.txt')
    call printresultline_int64('07a', result)
end program day07a_main
