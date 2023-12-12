!> Solution for https://adventofcode.com/2023/day/10 part b
module day10b
    use iso_fortran_env, only : int64
    use util, only : readinputfile_asstringarray, code_0, code_9
    implicit none
    private

    integer, parameter :: maxlinelength = 140
    integer, parameter :: right = 1
    integer, parameter :: top = 2
    integer, parameter :: left = 3
    integer, parameter :: down = 4
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

    function find_loop_orientation(lines, visited, start) result(counter_clockwise_loop)
        implicit none

        character(len=*), intent(in) :: lines(:)
        logical, intent(inout)       :: visited(:,:)
        integer, intent(inout)       :: start(2)
        logical                      :: counter_clockwise_loop
        integer                      :: lastpos(2), curpos(2), nextpos(2)
        integer                      :: direction, lastdirection, directionchange
        integer                      :: counter_clockwise_turns_delta
        character(len=1)             :: curpipe, nextpipe
        logical                      :: walk

        lastpos = start
        curpos = start
        lastdirection = 0
        counter_clockwise_turns_delta = 0
        visited = .false.
        walk = .true.
        do while (walk)
            curpipe = lines(curpos(1))(curpos(2):curpos(2))
            visited(curpos(1), curpos(2)) = .true.
            do direction = 1, 4
                nextpos = curpos + offset(:, direction)
                if (nextpos(1) < 1) cycle
                if (nextpos(1) > size(lines)) cycle
                if (nextpos(2) < 1) cycle
                if (nextpos(2) > len(lines(1))) cycle

                nextpipe = lines(nextpos(1))(nextpos(2):nextpos(2))
                if (ALL(nextpos == lastpos)) cycle

                ! found the correct connection -> exit loop
                if (is_connected(curpipe, nextpipe, direction)) exit
            end do
            lastpos = curpos
            curpos = nextpos
            if (direction /= lastdirection) then
                if (lastdirection /= 0) then
                    ! positive direction change: counter-clockwise turn
                    ! negative direction change: clockwise turn
                    directionchange = direction - lastdirection
                    ! fix direction change from down to right
                    if (directionchange == -3) directionchange = 1
                    ! fix direction change from right to down
                    if (directionchange == 3) directionchange = -1
                    
                    counter_clockwise_turns_delta = counter_clockwise_turns_delta + directionchange
                end if
                lastdirection = direction
            end if
            ! stop when reaching start again
            walk = .not. ALL(curpos == start)
        end do

        ! positive delta of turns: counter-clockwise loop
        ! negative delta of turns: clockwise loop
        counter_clockwise_loop = counter_clockwise_turns_delta > 0
    end function

    function get_paintable(lines, visited, pos) result(pospaintable)
        implicit none

        character(len=*), intent(in) :: lines(:)
        logical, intent(in)          :: visited(:,:)
        integer, intent(in)          :: pos(2)
        logical                      :: pospaintable

        pospaintable = .false.

        ! get pipe only if inside of grid
        if (pos(1) >= 1) then
            if (pos(1) <= size(visited, 1)) then
                if (pos(2) >= 1) then
                    if (pos(2) <= size(visited, 2)) then
                        ! get pipe only if not visited
                        if (.not. visited(pos(1), pos(2))) then
                            ! get pipe only if not yet painted
                            if (lines(pos(1))(pos(2):pos(2)) /= 'I') then
                                pospaintable = .true.
                            end if
                        end if
                    end if
                end if
            end if
        end if
    end function

    recursive subroutine set_pipe_and_flood(lines, visited, pos, pospipe, painted)
        implicit none

        character(len=*), intent(inout) :: lines(:)
        logical, intent(in)             :: visited(:,:)
        integer, intent(in)             :: pos(2)
        character(len=1), intent(in)    :: pospipe
        integer, intent(inout)          :: painted
        integer                         :: direction, nextpos(2)

        ! paint requested pipe
        lines(pos(1))(pos(2):pos(2)) = pospipe
        painted = painted + 1

        ! start flooding
        do direction = 1, 4
            nextpos = pos + offset(:, direction)
            if (get_paintable(lines, visited, nextpos)) then
                call set_pipe_and_flood(lines, visited, nextpos, pospipe, painted)
            end if
        end do
    end subroutine

    function paint_neighbours(lines, visited, curpos, curpipe, direction, counter_clockwise_loop) result(painted)
        implicit none

        character(len=*), intent(inout) :: lines(:)
        logical, intent(in)             :: visited(:,:)
        integer, intent(in)             :: curpos(2)
        character(len=1), intent(in)    :: curpipe
        integer, intent(in)             :: direction
        logical, intent(in)             :: counter_clockwise_loop
        integer                         :: painted
        integer                         :: neighbouroffset_vert(2), neighbouroffset_hori(2), neighbourpos(2)
        logical                         :: neighbourpaintable

        painted = 0
        neighbouroffset_vert = 0
        neighbouroffset_hori = 0

        ! depending on the type of pipe, vertical neighbours or horizontal neighbours or both could be painted,
        ! but only if also direction and loop orientation fit

        if (curpipe == '-') then
            ! default: direction right and counter clockwise loop
            neighbouroffset_vert = [-1, 0]
            ! flip once or twice if not default:
            if (.not. counter_clockwise_loop) neighbouroffset_vert = neighbouroffset_vert * (-1)
            if (.not. direction == right) neighbouroffset_vert = neighbouroffset_vert * (-1)

        else if (curpipe == '|') then
            ! default: direction top and counter clockwise loop
            neighbouroffset_hori = [0, -1]
            ! flip once or twice if not default:
            if (.not. counter_clockwise_loop) neighbouroffset_hori = neighbouroffset_hori * (-1)
            if (.not. direction == top) neighbouroffset_hori = neighbouroffset_hori * (-1)

        else if (curpipe == 'F') then
            if ((direction == left .and. (counter_clockwise_loop .eqv. .false.)) &
                .or. &
                (direction == top  .and. (counter_clockwise_loop .eqv. .true.))) then
                neighbouroffset_vert = [-1, 0]
                neighbouroffset_hori = [0, -1]
            end if

        else if (curpipe == '7') then
            if ((direction == right .and. (counter_clockwise_loop .eqv. .true.)) &
                .or. &
                (direction == top   .and. (counter_clockwise_loop .eqv. .false.))) then
                neighbouroffset_vert = [-1, 0]
                neighbouroffset_hori = [0, 1]
            end if

        else if (curpipe == 'J') then
            if ((direction == right .and. (counter_clockwise_loop .eqv. .false.)) &
                .or. &
                (direction == down  .and. (counter_clockwise_loop .eqv. .true.))) then
                neighbouroffset_vert = [1, 0]
                neighbouroffset_hori = [0, 1]
            end if

        else if (curpipe == 'L') then
            if ((direction == left .and. (counter_clockwise_loop .eqv. .true.)) &
                .or. &
                (direction == down .and. (counter_clockwise_loop .eqv. .false.))) then
                neighbouroffset_vert = [1, 0]
                neighbouroffset_hori = [0, -1]
            end if
        end  if

        ! check if a potential vertical neighbour should be set
        if (ANY(neighbouroffset_vert /= 0)) then
            neighbourpos = curpos + neighbouroffset_vert
            neighbourpaintable = get_paintable(lines, visited, neighbourpos)
            if (neighbourpaintable) then
                ! neighbour is free and not yet visited, so it should be the start point of flooding
                call set_pipe_and_flood(lines, visited, neighbourpos, 'I', painted)
            end if
        end if

        ! check if a potential horizontal neighbour should be set
        if (ANY(neighbouroffset_hori /= 0)) then
            neighbourpos = curpos + neighbouroffset_hori
            neighbourpaintable = get_paintable(lines, visited, neighbourpos)
            if (neighbourpaintable) then
                ! neighbour is free and not yet visited, so it should be the start point of flooding
                call set_pipe_and_flood(lines, visited, neighbourpos, 'I', painted)
            end if
        end if
    end function

    function paint_adjacent_pipes(lines, visited, start, counter_clockwise_loop) result(painted)
        implicit none

        character(len=*), intent(inout) :: lines(:)
        logical, intent(in)             :: visited(:,:)
        integer, intent(inout)          :: start(2)
        logical, intent(in)             :: counter_clockwise_loop
        integer                         :: painted
        integer                         :: lastpos(2), curpos(2), nextpos(2)
        integer                         :: direction
        character(len=1)                :: curpipe, nextpipe
        logical                         :: walk

        lastpos = start
        curpos = start
        painted = 0        
        walk = .true.
        do while (walk)
            curpipe = lines(curpos(1))(curpos(2):curpos(2))
            ! new: paint corresponding neighbours
            if (curpipe /= 'S') then
                painted = painted + paint_neighbours(lines, visited, curpos, curpipe, direction, counter_clockwise_loop)
            end if

            do direction = 1, 4
                nextpos = curpos + offset(:, direction)
                if (nextpos(1) < 1) cycle
                if (nextpos(1) > size(lines)) cycle
                if (nextpos(2) < 1) cycle
                if (nextpos(2) > len(lines(1))) cycle

                nextpipe = lines(nextpos(1))(nextpos(2):nextpos(2))
                if (ALL(nextpos == lastpos)) cycle

                ! found the correct connection -> exit loop
                if (is_connected(curpipe, nextpipe, direction)) exit
            end do

            lastpos = curpos
            curpos = nextpos
            ! stop when reaching start again
            walk = .not. ALL(curpos == start)
        end do
    end function

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

    subroutine print_lines(lines)
        implicit none

        character(len=*), intent(in)  :: lines(:)
        integer                       :: row

        do row = 1, size(lines)
            print *, lines(row)
        end do
    end subroutine

    integer(int64) function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        logical                       :: counter_clockwise_loop
        integer                       :: start(2), painted
        logical, allocatable          :: visited(:,:)
        integer                       :: row, col

        lines = readinputfile_asstringarray(filename, maxlinelength)

        ! search start position
        call find_start(lines, start)

        ! walk pipeline for the first time, find the orientation of the loop (counter-clockwise or clockwise),
        ! and mark all visited pipes in the "visited" map
        allocate(visited(size(lines), len(lines(1))))
        counter_clockwise_loop = find_loop_orientation(lines, visited, start)

        ! remove junk pipes from the map
        do row = 1, size(lines)
            do col = 1, len(lines(1))
                if (.not. visited(row, col)) then
                    lines(row)(col:col) = '.'
                end if
            end do
        end do

        ! walk again and paint (and count) adjacent neighbours if they are on the correct side: left side if
        ! counter-clockwise loop, right side if clockwise loop
        painted = paint_adjacent_pipes(lines, visited, start, counter_clockwise_loop)

        solve = painted
    end function

end module day10b
