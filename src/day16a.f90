!> Solution for https://adventofcode.com/2023/day/16 part a
module day16a
    use iso_fortran_env, only : int64
    use util, only : readinputfile_asstringarray, code_0, code_9
    implicit none
    private

    integer, parameter :: maxlinelength = 110
    character(len=1), parameter :: empty_space = '.'
    integer, parameter :: right = 1
    integer, parameter :: top = 2
    integer, parameter :: left = 3
    integer, parameter :: down = 4
    ! offset is first row, second column
    integer, parameter :: offset(2, 4) = reshape([0, 1, -1, 0, 0, -1, 1, 0], [2, 4])
    integer, parameter :: offset_slash(2, 4) = reshape([-1, 0, 0, 1, 1, 0, 0, -1], [2, 4])
    integer, parameter :: offset_backslash(2, 4) = reshape([1, 0, 0, -1, -1, 0, 0, 1], [2, 4])
    integer, parameter :: direction_slash(4) = [top, right, down, left]
    integer, parameter :: direction_backslash(4) = [down, left, top, right]
    character(len=1), parameter :: forward_tile(4) = ['-', '|', '-', '|']
    character(len=*), parameter :: narrowmapoutput = '(10A1)'
    character(len=*), parameter :: widemapoutput = '(110A1)'
    character(len=*), parameter :: visitnarrowmapoutput = '(10Z1)'
    character(len=*), parameter :: visitwidemapoutput = '(110Z1)'

    public :: solve

contains

    logical function valid_target(map, pos)
        implicit none

        character(len=1), intent(in)  :: map(:,:)
        integer, intent(in)           :: pos(2)

        valid_target = .false.
        if (pos(1) >= 1) then
            if (pos(1) <= size(map, 1)) then
                if (pos(2) >= 1) then
                    if (pos(2) <= size(map, 2)) then
                        valid_target = .true.
                    end if
                end if
            end if
        end if
    end function

    recursive subroutine travel_beam(map, visitmap, energized_tiles_count, curpos, curdirection)
        implicit none

        character(len=1), intent(in)  :: map(:,:)
        integer, intent(inout)        :: visitmap(:,:)
        integer(int64), intent(inout) :: energized_tiles_count
        integer, intent(in)           :: curpos(2)
        integer, intent(in)           :: curdirection
        character(len=1)              :: tile
        integer                       :: marker, nextpos(2), nextdirection

        marker = 2 ** (curdirection - 1)
        if (IAND(visitmap(curpos(1), curpos(2)), marker) /= 0) then
            ! already been here in that direction
            return
        end if

        if (visitmap(curpos(1), curpos(2)) == 0) then
            ! never been here in any direction
           energized_tiles_count = energized_tiles_count + 1
        end if

        ! first time here in the current direction
        visitmap(curpos(1), curpos(2)) = visitmap(curpos(1), curpos(2)) + marker
        tile = map(curpos(1), curpos(2))
        if (tile == empty_space .or. tile == forward_tile(curdirection)) then
            ! move forward in the same direction
            nextpos = curpos + offset(:, curdirection)
            if (valid_target(map, nextpos)) then
                call travel_beam(map, visitmap, energized_tiles_count, nextpos, curdirection)
            end if
        else if (tile == '/') then
            nextpos = curpos + offset_slash(:, curdirection)
            if (valid_target(map, nextpos)) then
                nextdirection = direction_slash(curdirection)
                call travel_beam(map, visitmap, energized_tiles_count, nextpos, nextdirection)
            end if
        else if (tile == '\') then
            nextpos = curpos + offset_backslash(:, curdirection)
            if (valid_target(map, nextpos)) then
                nextdirection = direction_backslash(curdirection)
                call travel_beam(map, visitmap, energized_tiles_count, nextpos, nextdirection)
            end if
        else if (tile == '-') then
            ! current direction must be top or down at this point
            nextpos = curpos + offset(:, right)
            if (valid_target(map, nextpos)) then
                nextdirection = right
                call travel_beam(map, visitmap, energized_tiles_count, nextpos, nextdirection)
            end if
            nextpos = curpos + offset(:, left)
            if (valid_target(map, nextpos)) then
                nextdirection = left
                call travel_beam(map, visitmap, energized_tiles_count, nextpos, nextdirection)
            end if
        else if (tile == '|') then
            ! current direction must be right or left at this point
            nextpos = curpos + offset(:, top)
            if (valid_target(map, nextpos)) then
                nextdirection = top
                call travel_beam(map, visitmap, energized_tiles_count, nextpos, nextdirection)
            end if
            nextpos = curpos + offset(:, down)
            if (valid_target(map, nextpos)) then
                nextdirection = down
                call travel_beam(map, visitmap, energized_tiles_count, nextpos, nextdirection)
            end if
        end if
    end subroutine

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

        integer, intent(in)           :: visitmap(:,:)
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
        integer(int64)                :: energized_tiles_count
        character(len=1), allocatable :: map(:,:)
        integer, allocatable          :: visitmap(:,:)
        
        lines = readinputfile_asstringarray(filename, maxlinelength)
        map = create_map(lines)
        call print_map(map)
        allocate(visitmap(size(map, 1),size(map, 2)))
        visitmap = 0

        energized_tiles_count = 0
        call travel_beam(map, visitmap, energized_tiles_count, [1, 1], right)
        call print_visitmap(visitmap)

        solve = energized_tiles_count
    end function

end module day16a
