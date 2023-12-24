!> Solution for https://adventofcode.com/2023/day/23 part a
module day23a
    use iso_fortran_env, only : int64
    use util, only : readinputfile_asstringarray
    implicit none
    private

    integer, parameter :: maxlinelength = 141
    character(len=1), parameter :: land_path = '.'
    character(len=1), parameter :: land_forest = '#'
    character(len=1), parameter :: land_slopes(4) = ['>', '^', '<', 'v']
    integer, parameter :: right = 1
    integer, parameter :: top = 2
    integer, parameter :: left = 3
    integer, parameter :: down = 4
    ! offset is first row, second column
    integer, parameter :: offset(2, 4) = reshape([0, 1, -1, 0, 0, -1, 1, 0], [2, 4])
    character(len=*), parameter :: narrowmapoutput = '(23A1)'
    character(len=*), parameter :: widemapoutput = '(141A1)'
    character(len=*), parameter :: visitnarrowmapoutput = '(23L1)'
    character(len=*), parameter :: visitwidemapoutput = '(141L1)'

    public :: solve

contains

    logical function valid_target(map, pos, direction)
        implicit none

        character(len=1), intent(in) :: map(:,:)
        integer, intent(in)          :: pos(2)
        integer, intent(in)          :: direction
        character(len=1)             :: land

        valid_target = .false.
        if (pos(1) >= 1) then
            if (pos(1) <= size(map, 1)) then
                if (pos(2) >= 1) then
                    if (pos(2) <= size(map, 2)) then
                        land = map(pos(1), pos(2))
                        if (land == land_path) then
                            ! target is a normal field
                            valid_target = .true.
                        else if (land == land_forest) then
                            ! target is a forest field
                            valid_target = .false.
                        else
                            ! target is one of the slope fields, so it must fit to be walkable
                            valid_target = (land == land_slopes(direction))
                        end if
                    end if
                end if
            end if
        end if
    end function


    function create_map(lines) result(map)
        implicit none

        character(len=*), intent(in)  :: lines(:)
        integer                       :: row, col
        character(len=1), allocatable :: map(:,:)

        allocate(map(size(lines), len(lines(1))))

        do row = 1, size(map, 1)
            do col = 1, size(map, 2)
                map(row, col) = lines(row)(col:col)
            end do
        end do
    end function

    recursive subroutine walk_land(map, curpos, visitmap, distance, longest_walk)
        implicit none

        character(len=1), intent(in)  :: map(:,:)
        integer, intent(in)           :: curpos(2)
        logical, intent(inout)        :: visitmap(:,:)
        integer(int64), intent(in)    :: distance
        integer(int64), intent(inout) :: longest_walk
        integer                       :: nextpos(2), direction

        ! print *, curpos

        if (curpos(1) == size(map, 1)) then
            if (curpos(2) == (size(map, 2) - 1)) then
                ! call print_visitmap(visitmap)
                ! print *, 'end of path:', distance
                longest_walk = max(longest_walk, distance)
                return
            end if
        end if

        do direction = right, down
            nextpos = curpos + offset(:, direction)
            if (valid_target(map, nextpos, direction)) then
                if (visitmap(nextpos(1), nextpos(2))) cycle  ! already visited
                visitmap(nextpos(1), nextpos(2)) = .true.
                call walk_land(map, nextpos, visitmap, distance + 1, longest_walk)
                visitmap(nextpos(1), nextpos(2)) = .false.
            end if
        end do
    end subroutine

    subroutine print_map(map)
        implicit none

        character(len=1), intent(in)  :: map(:,:)
        integer                       :: row
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

    subroutine print_visitmap(visitmap)
        implicit none

        logical, intent(in)           :: visitmap(:,:)
        integer                       :: row
        character(len=:), allocatable :: visitmapoutput

        if (size(visitmap, 2) == 10) then
            visitmapoutput = visitnarrowmapoutput
        else
            visitmapoutput = visitwidemapoutput
        end if

        do row = 1, size(visitmap, 1)
            print visitmapoutput, visitmap(row, :)
        end do
    end subroutine

    integer(int64) function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        integer(int64)                :: longest_walk
        character(len=1), allocatable :: map(:,:)
        logical, allocatable          :: visitmap(:,:)
        integer                       :: start(2)

        lines = readinputfile_asstringarray(filename, maxlinelength)
        map = create_map(lines)
        ! call print_map(map)
        start = [1, 2]
        allocate(visitmap(size(map, 1), size(map, 2)), source=.false.)
        visitmap(start(1), start(2)) = .true.

        longest_walk = 0
        call walk_land(map, start, visitmap, 0_int64, longest_walk)

        solve = longest_walk
    end function
end module day23a
