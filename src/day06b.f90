!> Solution for https://adventofcode.com/2023/day/6 part b
module day06b
    use iso_fortran_env, only : int64
    use util, only : readinputfile_asstringarray, string_extract_integers
    implicit none
    private

    integer, parameter :: maxlinelength = 36
    integer, parameter :: code_0 = iachar('0')
    integer, parameter :: code_9 = iachar('9')

    public :: solve

contains

    function calc_solutions(lines) result(solutions)
        implicit none
        character(len=*), intent(in)  :: lines(:)
        integer(int64)                :: solutions
        integer(int64)                :: totaltime, totaldistance, firstsecond, lastsecond
        integer(int64)                :: second

        solutions = -1
        read (lines(1)(12:), *) totaltime
        read (lines(2)(12:), *) totaldistance
        print *, totaltime
        print *, totaldistance

        solutions = 0
        firstsecond = -1
        lastsecond = -1
        do second = 0, totaltime
            if ((totaltime - second) * second > totaldistance) then
                firstsecond = second
                print *, firstsecond, ' firstsecond'
                exit
            end if
        end do
        do second = totaltime, 0, -1
            if ((totaltime - second) * second > totaldistance) then
                lastsecond = second
                print *, lastsecond, ' lastsecond'
                exit
            end if
        end do
        solutions = lastsecond - firstsecond + 1
    end function calc_solutions

    integer(int64) function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        integer(int64)                :: solutions

        lines = readinputfile_asstringarray(filename, maxlinelength)
        solutions = calc_solutions(lines)

        solve = solutions
    end function

end module day06b
