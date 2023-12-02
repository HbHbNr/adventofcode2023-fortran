!> Solution for https://adventofcode.com/2023/day/2 part a
module day02a
    use regex_module, only : regex
    use util, only : readinputfile_asstringarray
    implicit none
    private

    integer, parameter :: maxlinelength = 170
    integer, parameter :: red_cubes = 12
    integer, parameter :: green_cubes = 13
    integer, parameter :: blue_cubes = 14

    public :: solve

contains

    function is_possible_game(orgline) result(possible_game)
        implicit none

        character(len=*), intent(in) :: orgline
        logical                      :: possible_game
        ! type(regex_op)               :: re
        character(len=:), allocatable :: line
        character(len=*), parameter  :: re = '[0-9][0-9]? [rgb]'
        integer                      :: indx, length, amount
        character(len=1)             :: colour

        line = orgline

        possible_game = .false.
        indx = regex(line, re, length)
        do while (indx > 0)
            if (length == 3) then
                ! amount has only one digit
                read (line(indx:indx), *) amount
                colour = line(indx + 2:indx + 2)
            else
                ! amount has two digits
                read (line(indx:indx + 1), *) amount
                colour = line(indx + 3:indx + 3)
            end if

            select case(colour)
            case ('r')
                if (amount > red_cubes) return
            case ('g')
                if (amount > green_cubes) return
            case ('b')
                if (amount > blue_cubes) return
            end select

            line = line(indx + length:)
            indx = regex(line, re, length)
        end do
        possible_game = .true.
    end function

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        integer                       :: i, possible_games

        lines = readinputfile_asstringarray(filename, maxlinelength)

        possible_games = 0
        do i = 1, size(lines)
            if (is_possible_game(lines(i))) possible_games = possible_games + i
        end do

        solve = possible_games
    end function

end module day02a
