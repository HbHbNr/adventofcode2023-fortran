!> Solution for https://adventofcode.com/2023/day/21 part a
module day21a
    use iso_fortran_env, only : int64
    use util, only : readinputfile_asstringarray
    use class_intringbuffer, only : IntRingBuffer
    implicit none
    private

    integer, parameter :: maxlinelength = 131
    character(len=1), parameter :: garden_plot = '.'
    character(len=1), parameter :: garden_rock = '#'
    character(len=1), parameter :: garden_start = 'S'
    integer, parameter :: right = 1
    integer, parameter :: top = 2
    integer, parameter :: left = 3
    integer, parameter :: down = 4
    ! offset is first row, second column
    integer, parameter :: offset(2, 4) = reshape([0, 1, -1, 0, 0, -1, 1, 0], [2, 4])
    character(len=*), parameter :: narrowmapoutput = '(10A1)'
    character(len=*), parameter :: widemapoutput = '(131A1)'
    character(len=*), parameter :: visitnarrowmapoutput = '(10Z1)'
    character(len=*), parameter :: visitwidemapoutput = '(131Z1)'

    public :: solve

contains

    logical function valid_target(map, pos)
        implicit none

        character(len=1), intent(in) :: map(:,:)
        integer, intent(in)          :: pos(2)

        valid_target = .false.
        if (pos(1) >= 1) then
            if (pos(1) <= size(map, 1)) then
                if (pos(2) >= 1) then
                    if (pos(2) <= size(map, 2)) then
                        if (map(pos(1), pos(2)) == garden_plot) then
                            ! garden plot is a valid target; do not check if already visited
                            valid_target = .true.
                        end if
                    end if
                end if
            end if
        end if
    end function


    function create_map(lines, start) result(map)
        implicit none

        character(len=*), intent(in)  :: lines(:)
        integer, intent(out)          :: start(2)
        integer                       :: row, col
        character(len=1), allocatable :: map(:,:)

        allocate(map(size(lines), len(lines(1))))

        do row = 1, size(map, 1)
            do col = 1, size(map, 2)
                map(row, col) = lines(row)(col:col)
                if (map(row, col) == garden_start) start = [row, col]
            end do
        end do
    end function

    function walk_garden(map, start, steps) result(reachable_plots)
        implicit none

        character(len=1), intent(in) :: map(:,:)
        integer, intent(in)          :: start(2)
        integer, intent(in)          :: steps
        integer(int64)               :: reachable_plots
        integer, allocatable         :: visitmap(:,:)
        type(IntRingBuffer)          :: current_positions
        integer                      :: curpos(2), distance, nextpos(2), direction

        allocate(visitmap(size(map, 1),size(map, 2)))
        visitmap = 0
        ! call print_visitmap(visitmap)

        ! current_positions stores triplets: row, col, distance
        call current_positions%init(size(map, 1) * size(map, 2) / 2)

        ! start with start position in queue
        call current_positions%addLast(start(1))
        call current_positions%addLast(start(2))
        call current_positions%addLast(0)
        reachable_plots = 0
        do while (.not. current_positions%empty())
            ! call current_positions%print()

            curpos(1) = current_positions%removeFirst()
            curpos(2) = current_positions%removeFirst()
            distance = current_positions%removeFirst()
            ! print *, curpos, distance
            if (visitmap(curpos(1), curpos(2)) > 0) then
                ! we have been here already
                cycle
            end if

            visitmap(curpos(1), curpos(2)) = distance
            if (iand(distance, 1) == 0) then
                ! lowest bit not set, so distance is an even number
                reachable_plots = reachable_plots + 1
            end if
            if (distance < steps) then
                ! maximal distance not yet reached, so go one more step
                do direction = right, down
                    nextpos = curpos + offset(:, direction)
                    if (valid_target(map, nextpos)) then
                        call current_positions%addLast(nextpos(1))
                        call current_positions%addLast(nextpos(2))
                        call current_positions%addLast(distance + 1)
                    end if
                end do
            end if
            ! exit
        end do
        ! call print_visitmap(visitmap)
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

    integer(int64) function solve(filename, steps)
        implicit none

        character(len=*), intent(in)  :: filename
        integer, intent(in)           :: steps
        character(len=:), allocatable :: lines(:)
        integer(int64)                :: reachable_plots
        character(len=1), allocatable :: map(:,:)
        integer                       :: start(2)

        lines = readinputfile_asstringarray(filename, maxlinelength)
        map = create_map(lines, start)
        ! call print_map(map)
        ! print *, start

        reachable_plots = walk_garden(map, start, steps)
        solve = reachable_plots
    end function

end module day21a
