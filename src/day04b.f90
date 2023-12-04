!> Solution for https://adventofcode.com/2023/day/4 part b
module day04b
    use util, only : readinputfile_asstringarray
    implicit none
    private

    integer, parameter :: maxlinelength = 116
    integer, parameter :: code_0 = iachar('0')
    integer, parameter :: code_9 = iachar('9')

    public :: solve

contains

    subroutine sum_line(winnumbers, mynumbers, hits)
        implicit none

        integer, intent(in)  :: winnumbers(:), mynumbers(:)
        integer, intent(out) :: hits
        integer              :: i

        hits = 0
        do i = 1, size(mynumbers)
            if (findloc(winnumbers, mynumbers(i), 1) > 0) then
                hits = hits + 1
            end if
        end do
    end subroutine

    function sum_cards(lines) result(total_cards)
        implicit none

        character(len=*), intent(in)  :: lines(:)
        integer                       :: total_cards
        character(len=:), allocatable :: line
        integer                       :: i, j, colonpos, barpos, winamount, myamount, hits, ignore
        integer, allocatable          :: winnumbers(:), mynumbers(:), cards(:)

        total_cards = 0
        allocate(cards(size(lines)), source=1)
        colonpos = index(lines(1), ':')
        barpos = index(lines(1), '|')
        winamount = (barpos - colonpos - 1) / 3
        myamount = (len_trim(lines(1)) - barpos) / 3
        allocate(winnumbers(winamount))
        allocate(mynumbers(myamount))
        do i = 1, size(lines)
            line = lines(i)
            read (line(colonpos+1:barpos-1), *) winnumbers
            read (line(barpos+1:), *) mynumbers
            call sum_line(winnumbers, mynumbers, hits)
            do j = i + 1, min(i + hits, size(cards))
                cards(j) = cards(j) + cards(i)
            end do
        end do
        total_cards = sum(cards)
    end function sum_cards

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        integer                       :: total_cards

        lines = readinputfile_asstringarray(filename, maxlinelength)
        total_cards = sum_cards(lines)

        solve = total_cards
    end function

end module day04b
