!> Solution for https://adventofcode.com/2023/day/8 part a
module day08a
    use util, only : readinputfile_asline, readinputfile_asstringarray, code_0, code_9
    implicit none
    private

    integer, parameter :: maxlinelength = 16
    integer, parameter :: aaa = 4276545  ! 'AAA\0' in little endian
    integer, parameter :: zzz = 5921370  ! 'ZZZ\0' in little endian
    character(len=1)   :: nullchar = achar(0)

    public :: solve

contains

    function count_steps(firstline, lines) result(steps)
        implicit none

        character(len=*), intent(in)  :: firstline, lines(:)
        integer                       :: steps
        character(len=:), allocatable :: line
        logical, allocatable          :: goleft(:)
        integer, allocatable          :: targets(:,:)
        integer                       :: i, currentnode, currentchoice

        ! init orders and convert to boolean
        allocate(goleft(len(firstline)))
        do i = 1, len(firstline)
            goleft(i) = firstline(i:i) == 'L'
        end do

        ! init targets
        allocate(targets(2, zzz))
        do i = 3, size(lines)
            line = lines(i)
            ! make node names 4 bytes wide by placing a null character after each triple
            line(4:4) = nullchar
            line(11:11) = nullchar
            line(16:16) = nullchar
            ! convert node names to numbers
            currentnode = transfer(line(1:4), 1)
            targets(1,currentnode) = transfer(line(8:11), 1)
            targets(2,currentnode) = transfer(line(13:16), 1)
        end do

        ! walk
        steps = 0
        currentnode = aaa
        currentchoice = 1
        do while (currentnode /= zzz)
            steps = steps + 1
            if (goleft(currentchoice)) then
                currentnode = targets(1, currentnode)
            else
                currentnode = targets(2, currentnode)
            end if
            currentchoice = currentchoice + 1
            if (currentchoice > size(goleft)) currentchoice = 1
        end do
    end function count_steps

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: firstline, lines(:)
        integer                       :: steps

        firstline = readinputfile_asline(filename)
        lines = readinputfile_asstringarray(filename, maxlinelength)
        steps = count_steps(firstline, lines)

        solve = steps
    end function

end module day08a
