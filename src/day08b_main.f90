program day08b_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day08b, only : solve
    implicit none

    integer(int64) :: result

    ! result = solve('inputfiles/day08_example3.txt')
    result = solve('inputfiles/day08_input.txt')
    call printresultline_int64('08b', result)
end program day08b_main
