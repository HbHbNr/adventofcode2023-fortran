!> Solution for https://adventofcode.com/2023/day/21 part b
module day21b
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

    logical function valid_target(map, orgpos, width)
        implicit none

        character(len=1), intent(in) :: map(:,:)
        integer, intent(in)          :: orgpos(2)
        integer, intent(in)          :: width
        integer                      :: pos(2)

        pos = orgpos
        ! move coords to initial map
        if (pos(1) > width) pos(1) = modulo(pos(1), width)
        if (pos(2) > width) pos(2) = modulo(pos(2), width)

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
        integer                      :: width, repeat_factor

        width = size(map, 1)
        repeat_factor = ceiling((steps - 0.5 * width) / width)
        print *, 'steps:', steps, 'repeat_factor:', repeat_factor

        allocate(visitmap((2 * repeat_factor + 1) * width, (2 * repeat_factor + 1) * width))
        visitmap = 0
        ! call print_visitmap(visitmap)

        ! current_positions stores triplets: row, col, distance
        call current_positions%init(width * width / 2)

        ! start with start position in queue
        curpos = start
        curpos = curpos + (repeat_factor * width)
        print *, start, curpos
        call current_positions%addLast(curpos(1))
        call current_positions%addLast(curpos(2))
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
                    if (valid_target(map, nextpos, width)) then
                        call current_positions%addLast(nextpos(1))
                        call current_positions%addLast(nextpos(2))
                        call current_positions%addLast(distance + 1)
                    end if
                end do
            end if
            ! exit
        end do
        ! call print_visitmap(visitmap)
        print *, reachable_plots
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
        integer, intent(in)           :: steps(3)
        character(len=:), allocatable :: lines(:)
        integer(int64)                :: reachable_plots
        character(len=1), allocatable :: map(:,:)
        integer                       :: start(2), i

        lines = readinputfile_asstringarray(filename, maxlinelength)
        map = create_map(lines, start)

        ! replace start field with normal garden plot - why does it increase reachable plots by 1 (which is wrong)?
        ! map(start(1), start(2)) = garden_plot
        ! call print_map(map)
        ! print *, start

        do i = 1, size(steps)
            reachable_plots = walk_garden(map, start, steps(i))
        end do
        solve = reachable_plots
    end function

    ! ######################################################################################################
    ! Status: visitmap enlarged, map virtually enlarged (valid_target uses modulo); but "step is even" trick
    ! does not work for non-power-of-two step counts, therefore the whole visitmap does not work anymore
    !
    ! general idea for the solution is to count the reachable garden plots with 0.5*(width-1), 1.5*(width-1),
    ! and 2.5*(width-1) steps, and find the polynomial of degree two (because the counting happens inside the
    ! Euclidean plane)
end module day21b
