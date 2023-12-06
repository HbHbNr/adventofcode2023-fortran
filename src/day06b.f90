!> Solution for https://adventofcode.com/2023/day/6 part b
module day06b
    use iso_fortran_env, only : int64
    use util, only : readinputfile_asstringarray, code_0, code_9
    implicit none
    private

    integer, parameter :: maxlinelength = 36

    public :: solve

contains

    function killspaces(line1) result(line2)
        implicit none

        character(len=*), intent(in)  :: line1
        character(len=:), allocatable :: line2
        integer                       :: index1, index2

        ! allocate buffer of same length and clear it
        line2 = line1
        ! allocate(character(len=len(line1)) :: line2)
        line2(:) = ''

        ! copy only digits
        index2 = 1
        do index1 = 1, len(line1)
            if (iachar(line1(index1:index1)) >= code_0) then
                if (iachar(line1(index1:index1)) <= code_9) then
                    line2(index2:index2) = line1(index1:index1)
                    index2 = index2 + 1
                end if
            end if
        end do
    end function

    function calc_solutions(lines) result(solutions)
        implicit none

        character(len=*), intent(in)  :: lines(:)
        integer(int64)                :: solutions
        character(len=:), allocatable :: line
        integer(int64)                :: totaltime, totaldistance, firstsecond, lastsecond
        integer(int64)                :: second

        solutions = -1

        line = lines(1)(12:)
        line = killspaces(line)
        read (line, *) totaltime

        line = lines(2)(12:)
        line = killspaces(line)
        read (line, *) totaldistance

        solutions = 0
        firstsecond = -1
        lastsecond = -1
        ! iterate forwards
        do second = 0, totaltime
            if ((totaltime - second) * second > totaldistance) then
                ! stop iterating at first second creating a better total distance
                firstsecond = second
                exit
            end if
        end do
        ! iterate backwards
        do second = totaltime, 0, -1
            if ((totaltime - second) * second > totaldistance) then
                ! stop iterating at first second creating a better total distance
                lastsecond = second
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
