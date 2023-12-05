!> Solution for https://adventofcode.com/2023/day/5 part a
module day05a
    use iso_fortran_env, only : int64
    use util, only : readinputfile_asstringarray
    implicit none
    private

    integer, parameter :: maxlinelength = 208
    integer, parameter :: code_0 = iachar('0')
    integer, parameter :: code_9 = iachar('9')
    integer, parameter :: total_mappings = 7

    public :: solve

contains

    function extractseeds(line) result(seeds)
        implicit none

        character(len=*), intent(in) :: line
        integer(int64), allocatable  :: seeds(:)
        integer                      :: i, seedcount, laststart

        ! print *, line
        seedcount = 0
        laststart = 1
        do i = 2, len_trim(line)
            if (line(i:i) == ' ') seedcount = seedcount + 1
        end do
        seedcount = seedcount + 1
        ! print *, seedcount, ' seeds'

        allocate(seeds(seedcount))

        seedcount = 0
        laststart = 1
        do i = 2, len_trim(line)
            if (line(i:i) == ' ') then
                seedcount = seedcount + 1
                read (line(laststart:i+1), *) seeds(seedcount)
                laststart = i
            end if
        end do
        read (line(laststart:), *) seeds(size(seeds))

        print *, seeds
    end function

    function lookup(map, mapindexes, mapping, oldvalue) result(nextvalue)
        implicit none

        integer(int64), intent(in)   :: map(:,:)
        integer, intent(in)          :: mapindexes(:)
        integer, intent(in)          :: mapping
        integer(int64), intent(in)   :: oldvalue
        integer(int64)               :: nextvalue
        integer                      :: i, from, to

        from = mapindexes(mapping)
        if (mapping < total_mappings) then
            to = mapindexes(mapping + 1) - 3
        else
            to = size(map, 1)
        end if
        ! print *, from, to

        nextvalue = -1
        iloop: do i = from, to
            print *, map(i, 1), map(i, 2), map(i, 3)
            if (oldvalue >= map(i, 2)) then
                if (oldvalue <= map(i, 2) + map(i, 3) - 1) then
                    nextvalue = oldvalue - map(i, 2) + map(i, 1)
                    exit iloop
                end if
            end if
        end do iloop
        if(nextvalue == -1) then
            ! no mapping, value stays the same
            print *, 'did not find oldvalue', oldvalue, ', will keep it'
            nextvalue = oldvalue
            print *, 'copy', nextvalue
        else
            print *, 'found', nextvalue
        end if
    end function

    function scan_alamanac(lines) result(lowest_location_number)
        implicit none

        character(len=*), intent(in)  :: lines(:)
        integer(int64)                :: lowest_location_number
        character(len=:), allocatable :: line
        integer(int64), allocatable   :: seeds(:), map(:,:)
        integer                       :: i, j, digit, mapindexes(total_mappings), section
        integer(int64)                :: oldvalue, nextvalue
        integer                       :: seed, mapping

        ! extract the seeds from first line
        seeds = extractseeds(lines(1)(8:))

        ! lookups are the whole file, empty lines are marked with -1
        allocate(map(size(lines), 3))
        map(:,:) = -1
        mapindexes(:) = 0

        ! read sections
        section = 0
        do i = 2, size(lines)
            line = lines(i)
            if (line == '') then
                section = section + 1
                ! print *, 'new section', section
            else if (iachar(line(1:1)) > code_9) then
                mapindexes(section) = i + 1
                ! print *, section, ': "', line, '"', mapindexes(section)
            else
                ! lookup line
                ! print *, line
                read (line, *) map(i,1), map(i,2), map(i,3)
                ! print *, map(i,1), map(i,2), map(i,3)
            end if
        end do
        ! print *, map

        lowest_location_number = huge(map)
        do seed = 1, size(seeds)
            oldvalue = seeds(seed)
            print *, 'seed', seed, ':', oldvalue

            do mapping = 1, total_mappings
                nextvalue = lookup(map, mapindexes, mapping, oldvalue)
                oldvalue = nextvalue
            end do

            lowest_location_number = min(lowest_location_number, nextvalue)
            ! exit
        end do
    end function scan_alamanac

    integer(int64) function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        integer(int64)                :: lowest_location_number

        lines = readinputfile_asstringarray(filename, maxlinelength)
        lowest_location_number = scan_alamanac(lines)
        print *, 'lowest_location_number:', lowest_location_number

        solve = lowest_location_number
    end function

end module day05a
