!> Solution for https://adventofcode.com/2023/day/15 part b
module day15b
    use iso_fortran_env, only : int64
    use util, only : readinputfile_asline, code_0
    implicit none
    private

    integer, parameter :: max_boxes = 256
    integer, parameter :: max_labellength = 6
    integer, parameter :: max_lenses_per_box = 100
    integer, parameter :: max_steplength = max_labellength + 2  ! label + "=" + digit

    public :: solve

contains

    function hash(string)
        implicit none

        character(len=*), intent(in)  :: string
        integer                       :: i, hash, code

        hash = 0
        do i = 1, len(string)
            code = iachar(string(i:i))
            hash = hash + code
            hash = hash * 17
            hash = modulo(hash, 256)
        end do
    end function

    subroutine remove_lense(boxes_lenses_count, boxes_lenses_focallength, boxes_lenses_labels, label)
        implicit none

        integer, intent(inout)          :: boxes_lenses_count(0:max_boxes-1)
        character(len=max_labellength), intent(inout) :: boxes_lenses_labels(max_lenses_per_box, 0:max_boxes-1)
        integer, intent(inout)          :: boxes_lenses_focallength(max_lenses_per_box, 0:max_boxes-1)
        character(len=*), intent(in)    :: label
        integer                         :: boxnumber, lense

        boxnumber = hash(label)
        print *, 'remove lense labelled ', label, ' from box ', boxnumber
        lense = 0
        if (boxes_lenses_count(boxnumber) > 0) then
            lense = findloc(boxes_lenses_labels(:,boxnumber), label, 1)
        end if
        if (lense > 0) then
            print *, 'found lense labelled ', label, ' in box', boxnumber
        else
            print *, 'id not find lense labelled ', label, ' in box', boxnumber
        end if
    end subroutine

    subroutine add_lense(boxes_lenses_count, boxes_lenses_focallength, boxes_lenses_labels, label, focallength)
        implicit none

        integer, intent(inout)          :: boxes_lenses_count(0:max_boxes-1)
        character(len=max_labellength), intent(inout) :: boxes_lenses_labels(max_lenses_per_box, 0:max_boxes-1)
        integer, intent(inout)          :: boxes_lenses_focallength(max_lenses_per_box, 0:max_boxes-1)
        character(len=*), intent(in)    :: label
        integer, intent(in)             :: focallength
        integer                         :: boxnumber, lense

        boxnumber = hash(label)
        print '(3A, I1)', 'add lense labelled ', label, ' with focal length ', focallength, ' to box', boxnumber
    end subroutine

    integer(int64) function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: line
        integer(int64)                :: total_sum
        integer                       :: start, end, linelength
        integer, allocatable          :: boxes_lenses_count(:)
        character(len=max_labellength), allocatable :: boxes_lenses_labels(:,:)
        integer, allocatable          :: boxes_lenses_focallength(:,:)
        character(len=max_steplength) :: step
        integer                       :: len_trimmed, digit

        line = readinputfile_asline(filename)

        allocate(boxes_lenses_count(0:max_boxes-1))
        boxes_lenses_count = 0
        allocate(boxes_lenses_labels(max_lenses_per_box, 0:max_boxes-1))
        boxes_lenses_labels = ''
        allocate(boxes_lenses_focallength(max_lenses_per_box, 0:max_boxes-1))
        boxes_lenses_focallength = 0

        linelength = len(line)
        start = 1
        do end = 2, linelength
            if (line(end:end) == ',' .or. end == linelength) then
                if (line(end:end) == ',') then
                    step = line(start:end-1)
                else
                    step = line(start:end)
                end if
                print *, step
                len_trimmed = len_trim(step)
                if (step(len_trimmed:len_trimmed) == '-') then
                    step = step(:len_trimmed-1)
                    call remove_lense(boxes_lenses_count, boxes_lenses_focallength, boxes_lenses_labels, trim(step))
                else
                    digit = iachar(step(len_trimmed:len_trimmed)) - code_0
                    step = step(:len_trimmed-2)
                    call add_lense(boxes_lenses_count, boxes_lenses_focallength, boxes_lenses_labels, trim(step), digit)
                end if
                start = end + 1
                cycle
            end if
        end do
        step = line(start:)
        print *, step

        total_sum = 0
        solve = total_sum
    end function

end module day15b
