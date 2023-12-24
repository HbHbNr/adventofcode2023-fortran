program day23b_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day23b, only : solve
    implicit none

    integer(int64) :: result

    ! result = solve('inputfiles/day23_example.txt')
    result = solve('inputfiles/day23_input.txt')
    call printresultline_int64('23b', result)
end program day23b_main
