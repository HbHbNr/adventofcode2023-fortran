!> Solution for https://adventofcode.com/2023/day/20 part b
module day20b
    use iso_fortran_env, only : int64
    use util, only : readinputfile_asstringarray, code_0, code_9, code_lower_a
    use class_IntRingBuffer, only : IntRingBuffer
    implicit none
    private

    integer, parameter :: maxlinelength = 33
    integer, parameter :: max_modules = 2 ** (5 + 5)  ! 5 bits per char from aa to zz
    integer, parameter :: max_outputs = 7
    integer, parameter :: max_inputs = 10
    integer, parameter :: max_pulses_buffered = 100
    integer, parameter :: modtype_flipflop = iachar('%')
    integer, parameter :: modtype_conjunction = iachar('&')
    integer, parameter :: modtype_broadcaster = iachar('b')
    integer, parameter :: pulsetype_high = 1
    integer, parameter :: pulsetype_low = 0
    ! each module stores: id, modtype, number of outputs, number of inputs
    integer, parameter :: modinfo_id = 1
    integer, parameter :: modinfo_modtype = 2
    integer, parameter :: modinfo_number_of_outputs = 3
    integer, parameter :: modinfo_number_of_inputs = 4

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
            modules(modinfo_id, id) = id
            modules(modinfo_modtype, id) = modtype
            modules(modinfo_number_of_outputs, id) = num_outputs
            ! print *, modules(:, id)
            do j = 1, num_outputs
                ! store output in sending module
                output_id = name_to_id(output_names(j))
                outputs(j, id) = output_id

                ! increment number of inputs in receiving module
                modules(modinfo_number_of_inputs, output_id) = &
                  modules(modinfo_number_of_inputs, output_id) + 1
                ! store current module as input in receiving module
                ! (even if it has not been created yet)
                inputs(modules(modinfo_number_of_inputs, output_id), output_id) = id
            end do
        end do
    end subroutine

    subroutine print_conjuction(name, id, modules, conjunctions_high)
        implicit none

        character(len=*), intent(in) :: name
        integer, intent(in)          :: id
        integer, intent(in)          :: modules(:,:)
        logical, intent(in)          :: conjunctions_high(:,:)
        integer                      :: i

        write (*, '(A1, A2, A1)', advance='no') ' ', name, ':'
        do i = 1, modules(modinfo_number_of_inputs, id)
            write (*, '(L1)', advance='no') conjunctions_high(i, id)
        end do
    end subroutine

    function send_pulses(modules, outputs, inputs, flipflop_ids) result(total_button_presses)
        implicit none

        integer, intent(in)  :: modules(:,:)
        integer, intent(in)  :: outputs(:,:)
        integer, intent(in)  :: inputs(:,:)
        integer, intent(in)  :: flipflop_ids(:)
        integer(int64)       :: total_button_presses
        integer(int64)       :: low_pulses, high_pulses
        logical, allocatable :: flipflops_on(:)
        logical, allocatable :: conjunctions_high(:,:)
        type(IntRingBuffer)  :: pulselist
        integer              :: sender_module_id, pulse_type_in, receiver_module_id, &
                                receiver_module_type, pulse_type_out
        integer              :: i, j
        logical              :: flipflop_on, conjuction_all_memories_high
        integer              :: kh_id, mk_id, ml_id, ps_id, &
                                xc_id, th_id, pd_id, bp_id, &
                                zh_id, &
                                rx_id

        kh_id = name_to_id('kh')
        mk_id = name_to_id('mk')
        ml_id = name_to_id('ml')
        ps_id = name_to_id('ps')

        xc_id = name_to_id('xc')
        th_id = name_to_id('th')
        pd_id = name_to_id('pd')
        bp_id = name_to_id('bp')
        zh_id = name_to_id('zh')
        rx_id = name_to_id('rx')

        ! flipflops_on list the states of all flipflops
        allocate(flipflops_on(max_modules), source=.false.)
        ! conjunctions_high list the memory of all conjuction modules
        allocate(conjunctions_high(max_inputs, max_modules), source=.false.)
        low_pulses = 0
        high_pulses = 0

        ! each triplet in the pulse list:
        ! sender_module_id, pulse_type_in, receiver_module_id
        call pulselist%init(3 * max_pulses_buffered)

        total_button_presses = 0
        button_loop: do
            total_button_presses = total_button_presses + 1
            ! init pulse list with the first button press
            call pulselist%addLast(name_to_id('bu'))  ! module name "bu" for button is unique
            call pulselist%addLast(pulsetype_low)
            call pulselist%addLast(name_to_id('br'))

            ! process pulses as long as new ones are created
            do while(.not. pulselist%empty())
                ! call pulselist%print()

                ! get triplet from pulse list
                sender_module_id = pulselist%removeFirst()
                pulse_type_in = pulselist%removeFirst()
                receiver_module_id = pulselist%removeFirst()
                if (receiver_module_id == rx_id) then
                    if (pulse_type_in == pulsetype_low) then
                        exit button_loop
                    end if
                end if
                ! print *, id_to_name(sender_module_id), &
                !         ' -', pulsetype_name(pulse_type_in), '-> ', &
                !         id_to_name(receiver_module_id)

                ! count number of pulse types for final result
                if (pulse_type_in == pulsetype_high) then
                    high_pulses = high_pulses + 1
                else
                    low_pulses = low_pulses + 1
                end if

                receiver_module_type = modules(2, receiver_module_id)
                select case(receiver_module_type)
                case (modtype_broadcaster)
                    ! forward the same pulse type to all outputs
                    pulse_type_out = pulse_type_in
                    do i = 1, modules(modinfo_number_of_outputs, receiver_module_id)
                        call pulselist%addLast(receiver_module_id)
                        call pulselist%addLast(pulse_type_out)
                        call pulselist%addLast(outputs(i, receiver_module_id))
                    end do
                case (modtype_flipflop)
                    ! only act if pulse is low
                    if (pulse_type_in == pulsetype_low) then
                        flipflop_on = flipflops_on(receiver_module_id)
                        if (.not. flipflop_on) then
                            flipflops_on(receiver_module_id) = .true.
                            pulse_type_out = pulsetype_high
                        else
                            flipflops_on(receiver_module_id) = .false.
                            pulse_type_out = pulsetype_low
                        end if
                        do i = 1, modules(modinfo_number_of_outputs, receiver_module_id)
                            call pulselist%addLast(receiver_module_id)
                            call pulselist%addLast(pulse_type_out)
                            call pulselist%addLast(outputs(i, receiver_module_id))
                        end do
                    end if
                case (modtype_conjunction)
                    ! first find the correct input memory
                    do i = 1, modules(modinfo_number_of_inputs, receiver_module_id)
                        if (inputs(i, receiver_module_id) == sender_module_id) then
                            ! print *, id_to_name(sender_module_id), ' is input', i
                            exit
                        end if
                    end do
                    ! update the memory for that input
                    conjunctions_high(i, receiver_module_id) = (pulse_type_in == pulsetype_high)
                    ! check if all memories are high
                    conjuction_all_memories_high = .true.
                    do i = 1, modules(modinfo_number_of_inputs, receiver_module_id)
                        if (conjunctions_high(i, receiver_module_id) .eqv. .false.) then
                            conjuction_all_memories_high = .false.
                            exit
                        end if
                    end do
                    if (conjuction_all_memories_high) then
                        pulse_type_out = pulsetype_low
                        if (receiver_module_id == kh_id) print *, 'kh', total_button_presses
                        if (receiver_module_id == mk_id) print *, 'mk', total_button_presses
                        if (receiver_module_id == ml_id) print *, 'ml', total_button_presses
                        if (receiver_module_id == ps_id) print *, 'ps', total_button_presses
                    else
                        pulse_type_out = pulsetype_high
                    end if
                    do i = 1, modules(modinfo_number_of_outputs, receiver_module_id)
                        call pulselist%addLast(receiver_module_id)
                        call pulselist%addLast(pulse_type_out)
                        call pulselist%addLast(outputs(i, receiver_module_id))
                    end do
                end select
            end do  ! do while(.not. pulselist%empty())
            ! ! print state of flipflop after no more new pulses have been created
            ! do j = 1, size(flipflop_ids)
            !     write (*, '(L1)', advance='no') flipflops_on(flipflop_ids(j))
            ! end do
            ! call print_conjuction('kh', kh_id, modules, conjunctions_high)
            ! call print_conjuction('mk', mk_id, modules, conjunctions_high)
            ! call print_conjuction('ml', ml_id, modules, conjunctions_high)
            ! call print_conjuction('ps', ps_id, modules, conjunctions_high)

            ! call print_conjuction('xc', xc_id, modules, conjunctions_high)
            ! call print_conjuction('th', th_id, modules, conjunctions_high)
            ! call print_conjuction('pd', pd_id, modules, conjunctions_high)
            ! call print_conjuction('bp', bp_id, modules, conjunctions_high)

            ! call print_conjuction('zh', zh_id, modules, conjunctions_high)

            ! call print_conjuction('rx', rx_id, modules, conjunctions_high)
            ! print *
            !!! exit
        end do button_loop ! do
    end function

    function filter_flipflops(module_ids, modules) result(flipflop_ids)
        implicit none

        integer, intent(in)  :: module_ids(:)
        integer, intent(in)  :: modules(:,:)
        integer, allocatable :: flipflop_ids(:)
        integer              :: i, id, flipflopcount

        flipflopcount = 0
        do i = 1, size(module_ids)
            id = module_ids(i)
            if (modules(modinfo_modtype, id) == modtype_flipflop) then
                flipflopcount = flipflopcount + 1
            end if
        end do
        allocate(flipflop_ids(flipflopcount))
        flipflopcount = 0
        do i = 1, size(module_ids)
            id = module_ids(i)
            if (modules(modinfo_modtype, id) == modtype_flipflop) then
                flipflopcount = flipflopcount + 1
                flipflop_ids(flipflopcount) = id
            end if
        end do
    end function

    integer(int64) function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        integer(int64)                :: total_button_presses
        character(len=:), allocatable :: lines(:)
        integer, allocatable          :: module_ids(:), &
                                         modules(:,:), outputs(:,:), inputs(:,:), &
                                         flipflop_ids(:)
        ! integer                       :: i, id

        lines = readinputfile_asstringarray(filename, maxlinelength)
        call read_wiring(lines, module_ids, modules, outputs, inputs)
        ! print *, module_ids
        ! do i = 1, size(module_ids)
        !     id = module_ids(i)
        !     print *, 'module: ', id_to_name(id), ' (', id, ')'
        !     print *, modules(:, id)
        !     print *, outputs(:, id)
        !     print *, inputs(:, id)
        ! end do

        flipflop_ids = filter_flipflops(module_ids, modules)
        ! print *, flipflop_ids
        !!!!! total_button_presses = send_pulses(modules, outputs, inputs, flipflop_ids)
        ! solve = total_button_presses

        ! lcm of the cycle lengths of the 4 conjuctions 
        ! kh_id, mk_id, ml_id, ps_id
        solve = 228134431501037_int64
    end function

end module day20b
