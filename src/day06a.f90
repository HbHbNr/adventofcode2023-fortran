!> Solution for https://adventofcode.com/2023/day/6 part a
module day06a
    use util, only : readinputfile_asstringarray, string_extract_integers
    implicit none
    private

    integer, parameter :: maxlinelength = 36
    integer, parameter :: code_0 = iachar('0')
    integer, parameter :: code_9 = iachar('9')

    public :: solve

contains

    function calc_solutions(lines) result(margin)
        implicit none
        character(len=*), intent(in)  :: lines(:)
        integer                       :: margin
        character(len=:), allocatable :: line
        integer, allocatable          :: times(:), distances(:)
        integer                       :: i, second, solutions

        margin = 1
        call string_extract_integers(lines(1)(12:), times)
        call string_extract_integers(lines(2)(12:), distances)
        print *, times
        print *, distances
        do i = 1, size(times)
            solutions = 0
            do second = 0, times(i)
                ! print *, (times(i) - second) * second
                if ((times(i) - second) * second > distances(i)) then
                    solutions = solutions + 1
                end if
                ! print *, solutions, ' solutions'
            end do
            margin = margin * solutions
        end do
    end function calc_solutions

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        integer                       :: solutions

        lines = readinputfile_asstringarray(filename, maxlinelength)
        solutions = calc_solutions(lines)

        solve = solutions
    end function

end module day06a
