!> Solution for https://adventofcode.com/2023/day/17 part a
module day17a
    use iso_fortran_env, only : int64, int8
    use util, only : readinputfile_asintarray
    implicit none
    private

    integer, parameter :: maxlinelength = 141
    integer, parameter :: right = 1
    integer, parameter :: top = 2
    integer, parameter :: left = 3
    integer, parameter :: down = 4
    ! offset is first row, second column
    integer, parameter :: offset(2, 4) = reshape([0, 1, -1, 0, 0, -1, 1, 0], [2, 4])
    character(len=*), parameter :: narrowmapoutput = '(12I1)'
    character(len=*), parameter :: widemapoutput = '(141I1)'
    character(len=*), parameter :: visitnarrowmapoutput = '(12L1)'
    character(len=*), parameter :: visitwidemapoutput = '(141L1)'

    public :: solve

contains

    logical function valid_target(map, pos)
        implicit none

        integer(int8), intent(in) :: map(:,:)
        integer, intent(in)       :: pos(2)

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

    recursive subroutine move_crucible(map, visitmap, least_heat_loss, current_heat_loss, curpos, direction, direction_steps)
        implicit none

        integer(int8), intent(in)     :: map(:,:)
        logical, intent(inout)        :: visitmap(:,:)
        integer(int64), intent(inout) :: least_heat_loss
        integer(int64), intent(in)    :: current_heat_loss
        integer, intent(in)           :: curpos(2), direction, direction_steps
        integer(int64)                :: new_current_heat_loss
        integer                       :: nextdirection, nextpos(2)

        print *, curpos
        new_current_heat_loss = current_heat_loss + map(curpos(1), curpos(2))

        if (curpos(1) == size(map, 1)) then
            if (curpos(2) == size(map, 2)) then
                ! at target position
                least_heat_loss = min(least_heat_loss, new_current_heat_loss)
                print *, 'least_heat_loss at', curpos, ':', least_heat_loss
                visitmap(curpos(1), curpos(2)) = .true.
                call print_visitmap(visitmap)
                visitmap(curpos(1), curpos(2)) = .false.
                return
            end if
        end if

        ! not on the final city block
        visitmap(curpos(1), curpos(2)) = .true.
        do nextdirection = 1, 4
            ! if (nextdirection == direction) then
            !     if (direction_steps > 2) then
            !         ! not allowed to go into this direction any further
            !         cycle
            !     end if
            ! end if
            nextpos = curpos + offset(:, nextdirection)
            if (.not. valid_target(map, nextpos)) cycle
            ! print *, 'nextpos:', nextpos
            if (visitmap(nextpos(1), nextpos(2)) .eqv. .false.) then
                ! never been here
                if (direction == nextdirection) then
                    call move_crucible(map, visitmap, least_heat_loss, new_current_heat_loss, &
                                       nextpos, nextdirection, direction_steps + 1)
                else
                    call move_crucible(map, visitmap, least_heat_loss, new_current_heat_loss, &
                                       nextpos, nextdirection, 1)
                end if
            end if
        end do
        visitmap(curpos(1), curpos(2)) = .false.
    end subroutine

    subroutine print_map(map)
        implicit none

        integer(int8), intent(in)     :: map(:,:)
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
        integer(int8), allocatable    :: map(:,:)
        logical, allocatable          :: visitmap(:,:)
        integer(int64)                :: least_heat_loss

        map = readinputfile_asintarray(filename, maxlinelength)
        allocate(visitmap(size(map, 1),size(map, 2)), source=.false.)
        call print_map(map)
        call print_visitmap(visitmap)

        least_heat_loss = huge(least_heat_loss)
        call move_crucible(map, visitmap, least_heat_loss, -map(1,1) + 0_int64, [1, 1], 0, 0)

        ! solve = least_heat_loss
        solve = -1
    end function

end module day17a
