!> Solution for https://adventofcode.com/2023/day/15 part b
module day15b
    use iso_fortran_env, only : int64
    use util, only : readinputfile_asline, code_0
    implicit none
    private

    integer, parameter :: max_boxes = 256
    integer, parameter :: max_labellength = 6
    integer, parameter :: max_lenses_per_box = 10
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

    function calculate_focusing_power(boxes_lenses_count, boxes_lenses_focallength) result(total_focusing_power)
        implicit none

        integer, intent(in)            :: boxes_lenses_count(0:max_boxes-1)
        integer, intent(in)            :: boxes_lenses_focallength(max_lenses_per_box, 0:max_boxes-1)
        integer(int64)                 :: total_focusing_power
        integer(int64)                 :: focusing_power
        integer                        :: boxnumber, lense, lensecount

        total_focusing_power = 0
        do boxnumber = 0, max_boxes-1
            lensecount = boxes_lenses_count(boxnumber)
            if (lensecount > 0) then
                do lense = 1, lensecount
                    focusing_power = (boxnumber + 1) * lense * boxes_lenses_focallength(lense, boxnumber)
                    total_focusing_power = total_focusing_power + focusing_power
                end do
            end if
        end do
    end function

    subroutine print_box(boxes_lenses_focallength, boxes_lenses_labels, boxnumber)
        implicit none

        character(len=max_labellength), intent(in) :: boxes_lenses_labels(max_lenses_per_box, 0:max_boxes-1)
        integer, intent(in)            :: boxes_lenses_focallength(max_lenses_per_box, 0:max_boxes-1)
        integer, intent(in)            :: boxnumber

        write (*, '(I3, A)',  advance='no') boxnumber, ': '
        print *, boxes_lenses_labels(:, boxnumber)
        write (*, '(I3, A)',  advance='no') boxnumber, ': '
        print *, boxes_lenses_focallength(:, boxnumber)
    end subroutine

    subroutine remove_lense(boxes_lenses_count, boxes_lenses_focallength, boxes_lenses_labels, trimmedlabel)
        implicit none

        integer, intent(inout)         :: boxes_lenses_count(0:max_boxes-1)
        character(len=max_labellength), intent(inout) :: boxes_lenses_labels(max_lenses_per_box, 0:max_boxes-1)
        integer, intent(inout)         :: boxes_lenses_focallength(max_lenses_per_box, 0:max_boxes-1)
        character(len=*), intent(in)   :: trimmedlabel
        character(len=max_labellength) :: label
        integer                        :: boxnumber, lense, lensecount, i

        boxnumber = hash(trimmedlabel)
        label = trimmedlabel  ! extend trimmed label to correct length
        lense = 0
        lensecount = boxes_lenses_count(boxnumber)
        do i = 1, lensecount
            if (boxes_lenses_labels(i, boxnumber) == label) then
                lense = i
                exit
            end if
        end do
        if (lense > 0) then
            ! move rest of lenses one step towards the front
            boxes_lenses_labels(lense:lensecount-1, boxnumber) = boxes_lenses_labels(lense+1:lensecount, boxnumber)
            boxes_lenses_focallength(lense:lensecount-1, boxnumber) = boxes_lenses_focallength(lense+1:lensecount, boxnumber)
            ! overwrite old data at the old end
            boxes_lenses_labels(lensecount, boxnumber) = ''
            boxes_lenses_focallength(lensecount, boxnumber) = 0
            ! reduce number of lense in box by one
            boxes_lenses_count(boxnumber) = lensecount - 1    
        end if
    end subroutine

    subroutine put_lense(boxes_lenses_count, boxes_lenses_focallength, boxes_lenses_labels, trimmedlabel, focallength)
        implicit none

        integer, intent(inout)         :: boxes_lenses_count(0:max_boxes-1)
        character(len=max_labellength), intent(inout) :: boxes_lenses_labels(max_lenses_per_box, 0:max_boxes-1)
        integer, intent(inout)         :: boxes_lenses_focallength(max_lenses_per_box, 0:max_boxes-1)
        character(len=*), intent(in)   :: trimmedlabel
        integer, intent(in)            :: focallength
        character(len=max_labellength) :: label
        integer                        :: boxnumber, lense, lensecount, i

        boxnumber = hash(trimmedlabel)
        label = trimmedlabel  ! extend trimmed label to correct length
        lense = 0
        lensecount = boxes_lenses_count(boxnumber)
        do i = 1, lensecount
            if (boxes_lenses_labels(i, boxnumber) == label) then
                lense = i
                exit
            end if
        end do
        if (lense > 0) then
            ! replace labelled lense in place
            boxes_lenses_focallength(lense, boxnumber) = focallength
        else
            ! add labelled lense to the end
            lensecount = lensecount + 1
            boxes_lenses_count(boxnumber) = lensecount
            boxes_lenses_labels(lensecount, boxnumber) = label
            boxes_lenses_focallength(lensecount, boxnumber) = focallength
        end if
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

                len_trimmed = len_trim(step)
                if (step(len_trimmed:len_trimmed) == '-') then
                    step = step(:len_trimmed-1)
                    call remove_lense(boxes_lenses_count, boxes_lenses_focallength, boxes_lenses_labels, trim(step))
                else
                    digit = iachar(step(len_trimmed:len_trimmed)) - code_0
                    step = step(:len_trimmed-2)
                    call put_lense(boxes_lenses_count, boxes_lenses_focallength, boxes_lenses_labels, trim(step), digit)
                end if
                start = end + 1
                cycle
            end if
        end do

        total_sum = calculate_focusing_power(boxes_lenses_count, boxes_lenses_focallength)
        solve = total_sum
    end function

end module day15b
