!> Solution for https://adventofcode.com/2023/day/10 part a
module day10a
    use iso_fortran_env, only : int64
    use util, only : readinputfile_asstringarray, code_0, code_9
    implicit none
    private

    integer, parameter :: maxlinelength = 140
    integer, parameter :: right = 1
    integer, parameter :: top = 2
    integer, parameter :: left = 3
    integer, parameter :: bottom = 4
    ! list pipes which have an opening into the corresponding direction
    character(len=4), parameter :: pipe(4) = ['F-LS', 'L|JS', 'J-7S', '7|FS']
    ! offset is first row, second column
    integer, parameter :: offset(2, 4) = reshape([0, 1, -1, 0, 0, -1, 1, 0], [2, 4])
 
    public :: solve

contains

    function is_connected(pipe1, pipe2, direction) result(connects)
        implicit none

        character(len=1), intent(in) :: pipe1, pipe2
        integer, intent(in)          :: direction
        logical                      :: connects
        integer                      :: opposite_direction

        opposite_direction = direction + 2
        if (opposite_direction > 4) opposite_direction = opposite_direction - 4

        connects = index(pipe(direction), pipe1) > 0 .and. &
                   index(pipe(opposite_direction), pipe2) > 0
    end function

    function find_farthest_tile(lines, start) result(farthest_tile)
        implicit none

        character(len=*), intent(in) :: lines(:)
        integer, intent(inout)       :: start(2)
        integer(int64)               :: farthest_tile
        integer                      :: lastpos(2), curpos(2), nextpos(2), totalsteps, direction
        character(len=1)             :: curpipe, nextpipe
        logical                      :: walk

        totalsteps = 0
        lastpos = start
        curpos = start
        walk = .true.
        do while (walk)
            curpipe = lines(curpos(1))(curpos(2):curpos(2))
            ! print *, curpipe
            do direction = 1, 4
                nextpos = curpos + offset(:, direction)
                if (nextpos(1) < 1) cycle
                if (nextpos(1) > size(lines)) cycle
                if (nextpos(2) < 1) cycle
                if (nextpos(2) > len(lines(1))) cycle
                nextpipe = lines(nextpos(1))(nextpos(2):nextpos(2))
                ! print *, 'try: ', nextpipe
                if (ALL(nextpos == lastpos)) cycle
                if (is_connected(curpipe, nextpipe, direction)) exit
            end do
            ! print *, 'chosen: ', nextpipe
            totalsteps = totalsteps + 1
            lastpos = curpos
            curpos = nextpos
            ! stop when reaching start again
            walk = .not. ALL(curpos == start)
        end do

        farthest_tile = totalsteps / 2  ! totalsteps are always even
    end function find_farthest_tile

    subroutine find_start(lines, start)
        implicit none

        character(len=*), intent(in)  :: lines(:)
        integer, intent(inout)        :: start(2)
        integer                       :: row, col

        do row = 1, size(lines)
            col = index(lines(row), 'S')
            if (col > 0) then
                start(1) = row
                start(2) = col
            end if
        end do
    end subroutine

    integer(int64) function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        integer(int64)                :: farthest_tile
        integer                       :: start(2)

        lines = readinputfile_asstringarray(filename, maxlinelength)
        call find_start(lines, start)
        ! print *, 'start:', start
        farthest_tile = find_farthest_tile(lines, start)

        solve = farthest_tile
    end function

end module day10a
