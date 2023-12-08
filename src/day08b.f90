!> Solution for https://adventofcode.com/2023/day/8 part b
module day08b
    use iso_fortran_env, only : int64
    use util, only : readinputfile_asline, readinputfile_asstringarray, lcm
    implicit none
    private

    integer, parameter :: maxlinelength = 16
    integer, parameter :: aaa = 4276545  ! 'AAA\0' in little endian
    integer, parameter :: zzz = 5921370  ! 'ZZZ\0' in little endian
    character(len=1), parameter :: nullchar = achar(0)
    integer, parameter :: code_z = iachar('Z')

    public :: solve

contains

    function count_steps(firstline, lines) result(steps)
        implicit none

        character(len=*), intent(in)  :: firstline, lines(:)
        integer(int64)                :: steps
        character(len=:), allocatable :: line
        logical, allocatable          :: goleft(:)
        integer, allocatable          :: targets(:,:)
        integer, allocatable          :: startnodes(:), exitnodes(:)
        integer(int64), allocatable   :: cycles(:)
        integer                       :: i, s, e, currentnode, currentchoice, startnodecount
        logical                       :: run

        ! init orders and convert to boolean
        allocate(goleft(len(firstline)))
        do i = 1, len(firstline)
            goleft(i) = firstline(i:i) == 'L'
        end do

        ! init targets
        allocate(targets(2, zzz))
        startnodecount = 0
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
            ! check if node is a start node
            if (line(3:3) == 'A') startnodecount = startnodecount + 1
        end do

        ! init startnodes
        allocate(startnodes(startnodecount))
        allocate(exitnodes(startnodecount))
        s = 0
        e = 0
        do i = 3, size(lines)
            line = lines(i)
            if (line(3:3) == 'A') then
                s = s + 1
                ! make node names 4 bytes wide by placing a null character after each triple
                line(4:4) = nullchar
                ! convert node names to numbers
                currentnode = transfer(line(1:4), 1)
                startnodes(s) = currentnode
            else if (line(3:3) == 'Z') then
                e = e + 1
                ! make node names 4 bytes wide by placing a null character after each triple
                line(4:4) = nullchar
                ! convert node names to numbers
                currentnode = transfer(line(1:4), 1)
                exitnodes(e) = currentnode
            end if
        end do

        ! walk from each start node individually to find the cycles
        allocate(cycles(startnodecount))
        do i = 1, startnodecount
            steps = 0
            currentnode = startnodes(i)
            currentchoice = 1
            run = .true.
            do while (run)
                steps = steps + 1
                ! print *, steps, transfer(currentnode, '    ')
                if (goleft(currentchoice)) then
                    currentnode = targets(1, currentnode)
                else
                    currentnode = targets(2, currentnode)
                end if

                ! increase choice index, start over if needed
                currentchoice = currentchoice + 1
                if (currentchoice > size(goleft)) currentchoice = 1

                ! check if new node is an exit node
                run = findloc(exitnodes, currentnode, 1) == 0
            end do
            cycles(i) = steps
        end do

        ! find least common multiple of all cycles
        steps = lcm(cycles)
    end function count_steps

    integer(int64) function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: firstline, lines(:)
        integer(int64)                :: steps
        integer(int64)                :: numbers(4) = [2, 3, 4, 5], thelcm

        firstline = readinputfile_asline(filename)
        lines = readinputfile_asstringarray(filename, maxlinelength)
        steps = count_steps(firstline, lines)
        ! thelcm = lcm(numbers)
        ! print *, thelcm
        solve = steps
    end function

end module day08b
