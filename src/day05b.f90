!> Solution for https://adventofcode.com/2023/day/5 part b
module day05b
    use iso_fortran_env, only : int64
    use util, only : readinputfile_asstringarray, string_extract_int64s
    implicit none
    private

    integer, parameter :: maxlinelength = 208
    integer, parameter :: code_0 = iachar('0')
    integer, parameter :: code_9 = iachar('9')
    integer, parameter :: total_mappings = 7
    integer(int64), allocatable :: seeds(:), map(:,:)
    integer                     :: mapindexes(total_mappings, 2)

    public :: solve

contains

    function lookup(mapping, oldvalue) result(nextvalue)
        implicit none

        integer, intent(in)          :: mapping
        integer(int64), intent(in)   :: oldvalue
        integer(int64)               :: nextvalue
        integer                      :: i, from, to

        from = mapindexes(mapping, 1)
        to = mapindexes(mapping, 2)

        ! if not mapping is found, keep the old value
        nextvalue = oldvalue
        iloop: do i = from, to
            ! print *, map(i, 1), map(i, 2), map(i, 3)
            if (oldvalue >= map(i, 2)) then
                if (oldvalue <= map(i, 2) + map(i, 3) - 1) then
                    nextvalue = oldvalue - map(i, 2) + map(i, 1)
                    exit iloop
                end if
            end if
        end do iloop
    end function

    subroutine print_map()
        implicit none

        integer :: i

        do i = 1, size(map, 1)
            print *, map(i,:)
        end do
    end subroutine

    function scan_alamanac(lines) result(lowest_location_number)
        implicit none

        character(len=*), intent(in)  :: lines(:)
        integer(int64)                :: lowest_location_number
        character(len=:), allocatable :: line
        integer                       :: i, section
        integer(int64)                :: seed, oldvalue, nextvalue
        integer                       :: seedrangestart, mapping

        ! extract the seeds from first line
        call string_extract_int64s(lines(1)(8:), seeds)

        ! lookups are the whole file, first line and empty values are marked with -1
        allocate(map(size(lines), 3))
        map(1,:) = -1

        ! read sections
        section = 0
        do i = 2, size(lines)
            line = lines(i)
            if (line == '') then
                section = section + 1
                ! print *, 'new section', section
            else if (iachar(line(1:1)) > code_9) then
                mapindexes(section, 1) = i + 1
                ! print *, section, ': "', line, '"', mapindexes(section, 1)
            else
                ! lookup line
                ! print *, line
                read (line, *) map(i,1), map(i,2), map(i,3)
                ! print *, map(i,1), map(i,2), map(i,3)
            end if
        end do
        ! call print_map()

        ! calc end of each section
        do section = 1, total_mappings - 1
            mapindexes(section, 2) = mapindexes(section + 1, 1) - 3
        end do
        mapindexes(total_mappings, 2) = size(map, 1)

        lowest_location_number = huge(map)
        do seedrangestart = 1, size(seeds), 2
            do seed = seeds(seedrangestart), seeds(seedrangestart) + seeds(seedrangestart+1) - 1
                oldvalue = seed

                do mapping = 1, total_mappings
                    nextvalue = lookup(mapping, oldvalue)
                    oldvalue = nextvalue
                end do

                lowest_location_number = min(lowest_location_number, nextvalue)
            end do
        end do
    end function scan_alamanac

    integer(int64) function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        integer(int64)                :: lowest_location_number

        lines = readinputfile_asstringarray(filename, maxlinelength)
        lowest_location_number = scan_alamanac(lines)
        ! print *, 'lowest_location_number:', lowest_location_number

        solve = lowest_location_number
    end function

end module day05b
