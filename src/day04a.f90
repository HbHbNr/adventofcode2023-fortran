!> Solution for https://adventofcode.com/2023/day/4 part a
module day04a
    use util, only : readinputfile_asstringarray
    implicit none
    private

    integer, parameter :: maxlinelength = 116
    integer, parameter :: code_0 = iachar('0')
    integer, parameter :: code_9 = iachar('9')

    public :: solve

contains

    function sum_line(winnumbers, mynumbers) result(line_points)
        implicit none

        integer, intent(in) :: winnumbers(:), mynumbers(:)
        integer             :: line_points
        integer             :: i, hits

        line_points = 0
        hits = 0
        do i = 1, size(mynumbers)
            if (findloc(winnumbers, mynumbers(i), 1) > 0) then
                hits = hits + 1
                ! print *, 'found', mynumbers(i)
            end if
        end do

        if (hits > 0) line_points = 2 ** (hits - 1)
        ! print *, 'points:', line_points
    end function

    function sum_cards(lines) result(total_points)
        implicit none

        character(len=*), intent(in)  :: lines(:)
        integer                       :: total_points
        character(len=:), allocatable :: line
        integer                       :: i, colonpos, barpos, winamount, myamount
        integer, allocatable          :: winnumbers(:), mynumbers(:)

        total_points = 0
        colonpos = index(lines(1), ':')
        barpos = index(lines(1), '|')
        winamount = (barpos - colonpos - 1) / 3
        myamount = (len_trim(lines(1)) - barpos) / 3
        ! print *, winamount, myamount
        allocate(winnumbers(winamount))
        allocate(mynumbers(myamount))
        do i = 1, size(lines)
            line = lines(i)
            read (line(colonpos+1:barpos-1), *) winnumbers
            read (line(barpos+1:), *) mynumbers
            ! print *, winnumbers
            ! print *, mynumbers
            total_points = total_points + sum_line(winnumbers, mynumbers)
        end do
    end function sum_cards

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        integer                       :: total_points

        lines = readinputfile_asstringarray(filename, maxlinelength)
        total_points = sum_cards(lines)

        solve = total_points
    end function

end module day04a
