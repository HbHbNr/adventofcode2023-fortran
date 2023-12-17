program day18a_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day18a, only : solve
    implicit none

    integer(int64) :: result

    result = solve('inputfiles/day18_example.txt')
    ! result = solve('inputfiles/day18_input.txt')
    call printresultline_int64('18a', result)
end program day18a_main
