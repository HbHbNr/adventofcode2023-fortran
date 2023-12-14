!> Solution for https://adventofcode.com/2023/day/14 part a
module day14a
    use iso_fortran_env, only : int64
    use util, only : readinputfile_asstringarray, code_0, code_9
    implicit none
    private

    integer, parameter :: maxlinelength = 100
    integer, parameter :: empty_space = 0
    integer, parameter :: rock_round = 1
    integer, parameter :: rock_cube = 2
    character(len=*), parameter :: narrowmapoutput = '(10I1)'
    character(len=*), parameter :: widemapoutput = '(100I1)'

    public :: solve

contains

    function roll_rocks(map) result(total_load)
        implicit none

        integer, intent(inout) :: map(:,:)
        integer(int64)                :: total_load
        integer                       :: height, row, col, newrow, testrow

        total_load = 0
        height = size(map, 1)
        do row = 1, height
            do col = 1, size(map, 2)
                if (map(row, col) == empty_space) cycle
                if (map(row, col) == rock_cube) cycle
                ! round cube on this place
                ! print '(I4,A,I4)', row, '/', col
                if (row == 1) then
                    ! already max north
                    total_load = total_load + height
                    cycle
                end if
                ! not max north, so try to roll it
                newrow = row
                do testrow = row - 1, 1, -1
                    ! print '(A,I4,A,I4,A,I1)', '---------- ', testrow, '/', col, '=', map(testrow, col)
                    if (map(testrow, col) == empty_space) then
                        newrow = testrow
                    else
                        exit
                    end if
                end do
                if (newrow /= row) then
                    ! move rock to new position
                    ! print '(A,I4,A,I4,A,I4,A,I4)', 'move', row, '/', col, ' to ', newrow, '/', col
                    map(row, col) = empty_space
                    map(newrow, col) = rock_round
                end if
                total_load = total_load + (height - newrow + 1)
            end do
        end do
    end function roll_rocks

    function create_map(lines) result(map)
        implicit none

        character(len=*), intent(in) :: lines(:)
        integer                      :: row, col
        integer, allocatable         :: map(:,:)

        allocate(map(size(lines), len_trim(lines(1))))
        map = empty_space

        do row = 1, size(map, 1)
            do col = 1, size(map, 2)
                select case (lines(row)(col:col))
                case ('O')
                    map(row, col) = rock_round
                case ('#')
                    map(row, col) = rock_cube
                end select
            end do
        end do
    end function

    subroutine print_map(map)
        implicit none

        integer, intent(in) :: map(:,:)
        integer             :: row
        character(len=:), allocatable :: mapoutput

        if (size(map, 2) == 10) then
            mapoutput = narrowmapoutput
        else
            mapoutput = widemapoutput
        end if

        do row = 1, size(map, 1)
            print mapoutput, map(row, :)
        end do
    end subroutine

    integer(int64) function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        integer(int64)                :: total_load
        integer, allocatable          :: map(:,:)

        
        lines = readinputfile_asstringarray(filename, maxlinelength)
        map = create_map(lines)
        ! call print_map(map)
        ! print *

        total_load = roll_rocks(map)
        ! call print_map(map)

        solve = total_load
    end function

end module day14a
