!> Solution for https://adventofcode.com/2023/day/20 part a
module day20a
    use iso_fortran_env, only : int64
    use util, only : readinputfile_asstringarray, code_0, code_9, code_lower_a
    implicit none
    private

    integer, parameter :: maxlinelength = 33
    integer, parameter :: max_modules = 2 ** (5 + 5)  ! 5 bits per char from aa to zz
    integer, parameter :: max_outputs = 7
    integer, parameter :: max_inputs = 10
    integer, parameter :: modtype_flipflop = iachar('%')
    integer, parameter :: modtype_conjunction = iachar('&')
    integer, parameter :: modtype_broadcaster = iachar('b')

    public :: name_to_id
    public :: id_to_name
    public :: solve

contains

    function name_to_id(name) result(id)
        implicit none

        character(len=2), intent(in) :: name
        integer                      :: id

        id = iachar(name(2:2)) - code_lower_a + 1
        if (name(1:1) /= ' ') then
            id = id + ishft(iachar(name(1:1)) - code_lower_a + 1, 5)
        end if
    end function

    function id_to_name(id) result(name)
        implicit none

        integer, intent(in) :: id
        character(len=2)    :: name

        name = ''
        if (id >= (2 ** 5)) then
            name(1:1) = achar(ishft(id, -5) + code_lower_a - 1)
        end if
        name(2:2) = achar(iand(id, 31) + code_lower_a - 1)
    end function

    subroutine read_output_names(orgline, num_outputs, output_names)
        implicit none

        character(len=*), intent(in)  :: orgline
        integer, intent(in)           :: num_outputs
        character(len=2), intent(out) :: output_names(max_outputs)
        integer                       :: comma, remaining_outputs
        character(len=2)              :: name
        character(len=:), allocatable :: line

        line = orgline
        ! print *, num_outputs, line

        output_names = ''
        remaining_outputs = num_outputs
        do while (remaining_outputs > 1)
            ! print *, '"', line, '"'
            comma = index(line, ',')
            ! print *, '"', line(:comma - 1), '"'
            read (line(:comma - 1), '(A)') name
            if(len_trim(name) == 1) then
                name(2:2) = name(1:1)
                name(1:1) = ' '
            end if
            ! print *, 'name: "', name, '"'
            output_names(num_outputs - remaining_outputs + 1) = name

            ! cut current name
            line = line(comma + 2:)
            remaining_outputs = remaining_outputs - 1
        end do
        ! at last output name
        ! print *, '"', line, '"'
        read (line, '(A2)') name
        if(len_trim(name) == 1) then
            name(2:2) = name(1:1)
            name(1:1) = ' '
        end if
        ! print *, 'name: "', name, '"'
        output_names(num_outputs) = name
    end subroutine

    subroutine read_wiring(lines, module_ids, modules, outputs, inputs)
        implicit none

        character(len=*), intent(in)      :: lines(:)
        integer, intent(out), allocatable :: module_ids(:)
        integer, intent(out), allocatable :: modules(:,:)
        integer, intent(out), allocatable :: outputs(:,:)
        integer, intent(out), allocatable :: inputs(:,:)
        character(len=:), allocatable     :: line
        integer                           :: i, j, id, modtype, num_outputs, &
                                             gt, output_id
        character(len=2)                  :: name, output_names(max_outputs)

        ! store overlook of all module ids
        allocate(module_ids(size(lines)), source=0)
        ! each module stores: id, modtype, number of outputs, number of inputs
        allocate(modules(1 + 1 + 1 + 1, max_modules), source=0)
        ! outputs list the outgoing connections
        allocate(outputs(max_outputs, max_modules), source=0)
        ! inputs list the imcoming connections
        allocate(inputs(max_inputs, max_modules), source=0)

        do i = 1, size(lines)
            line = lines(i)
            name = ''
            if (line(1:7) == 'broadca') then
                name = 'br'
                modtype = modtype_broadcaster
            else if(line(3:3) == '') then
                ! name has only one char
                name(2:2) = line(2:2)
                modtype = iachar(line(1:1))
            else
                ! name has two chars
                name = line(2:3)
                modtype = iachar(line(1:1))
            end if
            gt = index(line, '>')
            num_outputs = 1
            do j = gt + 2, len_trim(line)
                if (line(j:j) == ',') num_outputs = num_outputs + 1
            end do
            call read_output_names(line(gt+2:), num_outputs, output_names)

            id = name_to_id(name)
            module_ids(i) = id
            modules(1, id) = id
            modules(2, id) = modtype
            modules(3, id) = num_outputs
            ! print *, modules(:, id)
            do j = 1, num_outputs
                ! store output in sending module
                output_id = name_to_id(output_names(j))
                outputs(j, id) = output_id

                ! increment number of inputs in receiving module
                modules(4, output_id) = modules(4, output_id) + 1
                ! store current module as input in receiving module
                ! (even if it has not been created yet)
                inputs(modules(4, output_id), output_id) = id
            end do
        end do
    end subroutine

    integer(int64) function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        integer(int64)                :: total_pulses
        character(len=:), allocatable :: lines(:)
        integer, allocatable          :: module_ids(:), modules(:,:), &
                                         outputs(:,:), inputs(:,:)
        integer                       :: i, id

        lines = readinputfile_asstringarray(filename, maxlinelength)
        call read_wiring(lines, module_ids, modules, outputs, inputs)
        print *, module_ids
        do i = 1, size(module_ids)
            id = module_ids(i)
            print *, 'module: ', id_to_name(id)
            print *, modules(:, id)
            print *, outputs(:, id)
            print *, inputs(:, id)
        end do

        ! solve = total_pulses
        solve = -1
    end function

end module day20a
