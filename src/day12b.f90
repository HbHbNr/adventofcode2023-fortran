!> Solution for https://adventofcode.com/2023/day/12 part b
module day12b
    use iso_fortran_env, only : int64
    use util, only : readinputfile_asstringarray, string_extract_integers
    implicit none
    private

    integer, parameter :: maxlinelength = 32

    public :: solve

contains

    function verify_group_sizes(damaged, group_sizes) result(fits)
        implicit none

        logical, intent(in)           :: damaged(:)
        integer, intent(in)           :: group_sizes(:)
        logical                       :: fits
        integer                       :: groupstarts, currentgroupsize, i
        logical                       :: gap

        fits = .false.

        ! iterate damaged springs
        groupstarts = 0
        currentgroupsize = 0
        gap = .true.
        do i = 1, size(damaged)
            if (damaged(i)) then
                if (gap) then
                    groupstarts = groupstarts + 1
                    if (groupstarts > size(group_sizes)) then
                        ! additional group found -> not verified
                        ! print *, 'groupstarts:', groupstarts, ' > groups:', size(group_sizes)
                        return
                    end if
                    currentgroupsize = 1
                    gap = .false.
                else
                    currentgroupsize = currentgroupsize + 1
                    if (currentgroupsize > group_sizes(groupstarts)) then
                        ! current group is too big -> not verified
                        ! print *, 'currentgroupsize:', currentgroupsize, ' > groupsize:', group_sizes(groupstarts)
                        return
                    end if
                end if
            else
                if (.not. gap) then
                    if (currentgroupsize < group_sizes(groupstarts)) then
                        ! current group is not big enough -> not verified
                        ! print *, 'currentgroupsize:', currentgroupsize, ' < groupsize:', group_sizes(groupstarts)
                        return
                    end if
                    gap = .true.
                end if
                ! gap after gap is just fine, not test needed
            end if
        end do
        if (groupstarts < size(group_sizes)) then
            ! not enough groups found -> not verified
            ! print *, 'groupstarts:', groupstarts, ' < groups:', size(group_sizes)
            return
        end if
        ! repeat check from beginning of a new gap:
        if (currentgroupsize < group_sizes(groupstarts)) then
            ! current group is not big enough -> not verified
            ! print *, 'currentgroupsize:', currentgroupsize, ' < groupsize:', group_sizes(groupstarts)
            return
        end if

        fits = .true.
    end function

    recursive subroutine fill_unknown_springs(id, damaged, unknown, unknown_count, group_sizes, total_arrangements)
        implicit none

        integer, intent(in)           :: id
        logical, intent(inout)        :: damaged(:), unknown(:)
        integer, intent(in)           :: unknown_count
        integer, intent(in)           :: group_sizes(:)
        integer(int64), intent(inout) :: total_arrangements
        integer                       :: first_unknown

        ! print *, 'fill_unknown_springs ', unknown_count, ' - ', damaged, ' - ', unknown
        if (unknown_count == 0) then
            if (verify_group_sizes(damaged, group_sizes)) then
                total_arrangements = total_arrangements + 1
                print *, id, ' verified: ', damaged, ':', group_sizes
            end if
            return
        end if

        first_unknown = findloc(unknown, .true., 1)
        unknown(first_unknown) = .false.
        damaged(first_unknown) = .false.
        call fill_unknown_springs(id, damaged, unknown, unknown_count - 1, group_sizes, total_arrangements)
        damaged(first_unknown) = .true.
        call fill_unknown_springs(id, damaged, unknown, unknown_count - 1, group_sizes, total_arrangements)
        unknown(first_unknown) = .true.
    end subroutine

    integer(int64) function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        character(len=:), allocatable :: line
        integer(int64)                :: total_arrangements
        integer                       :: row, split, i, springs, groups
        integer, allocatable          :: group_sizes1x(:), group_sizes(:)
        logical, allocatable          :: damaged(:), unknown(:)

        lines = readinputfile_asstringarray(filename, maxlinelength)
        total_arrangements = 0

        do row = 1, size(lines)
            line = lines(row)
            split = index(line, ' ')
            springs = split - 1

            allocate(damaged(5 * springs + 4), source=.false.)  ! separated by "?"
            allocate(unknown(5 * springs + 4), source=.false.)  ! separated by "?"
            do i = 1, split - 1
                if (line(i:i) == '#') then
                    damaged(i) = .true.
                else if (line(i:i) == '?') then
                    unknown(i) = .true.
                end if
            end do
            ! append four times the data read
            do i = 1, 4
                damaged(i*(springs+1)) = .false.  ! separated by "?"
                damaged(i*(springs+1)+1:(i+1)*(springs+1)-1) = damaged(1:springs)
                unknown(i*(springs+1)) = .true.   ! separated by "?"
                unknown(i*(springs+1)+1:(i+1)*(springs+1)-1) = unknown(1:springs)
            end do

            call string_extract_integers(line(split+1:len_trim(line)), group_sizes1x)
            groups = size(group_sizes1x)
            allocate(group_sizes(5 * groups))
            group_sizes(:groups) = group_sizes1x(:)
            ! append four times the data read
            do i = 1, 4
                group_sizes(i*groups+1:(i+1)*groups) = group_sizes1x(:)
            end do

            ! print *, line
            ! print *, damaged
            ! print *, unknown
            ! print *, group_sizes
            ! call fill_unknown_springs(row, damaged, unknown, count(unknown), group_sizes, total_arrangements)

            deallocate(damaged)
            deallocate(unknown)
            deallocate(group_sizes)
            if (row == 1) exit
        end do

        ! solve = total_arrangements
        solve = -1
    end function

end module day12b
