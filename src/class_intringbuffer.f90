module class_intringbuffer
    use iso_fortran_env, only : error_unit
    use util, only : printarray
    implicit none
    private

    type, public :: IntRingBuffer
        private

        integer, allocatable :: values(:)
        integer              :: nextwriteindex
        integer              :: valueslength
    contains
        procedure :: init        => intringbuffer_init
        procedure :: addFirst    => intringbuffer_addfirst
        procedure :: addLast     => intringbuffer_addlast
        procedure :: getFirst    => intringbuffer_getfirst
        procedure :: getLast     => intringbuffer_getlast
        procedure :: get         => intringbuffer_get
        procedure :: removeFirst => intringbuffer_removefirst
        procedure :: removeLast  => intringbuffer_removelast
        procedure :: empty       => intringbuffer_empty
        procedure :: length      => intringbuffer_length
        procedure :: contains    => intringbuffer_contains
        procedure :: print       => intringbuffer_print
    end type IntRingBuffer

contains

    subroutine intringbuffer_init(this, ringbuffersize)
        implicit none

        class(IntRingBuffer), intent(inout) :: this
        integer, intent(in)                 :: ringbuffersize

        allocate(this%values(ringbuffersize), source=0)
        this%nextwriteindex = 1
        this%valueslength = 0
    end subroutine intringbuffer_init

    subroutine intringbuffer_addFirst(this, value)
        implicit none

        class(IntRingBuffer), intent(inout) :: this
        integer, intent(in)                 :: value
        integer                             :: index

        if (this%valueslength >= size(this%values)) then
            write (error_unit, *) 'IntRingBuffer error for addFirst(): buffer full'
            stop
        else
            index = this%nextwriteindex - this%valueslength - 1
            if (index < 1) then
                ! modulo not usable, because array index is 1-based
                index = index + size(this%values)
            end if
            this%values(index) = value
            this%valueslength = this%valueslength + 1
        end if
    end subroutine intringbuffer_addFirst

    subroutine intringbuffer_addLast(this, value)
        implicit none

        class(IntRingBuffer), intent(inout) :: this
        integer, intent(in)                 :: value

        if (this%valueslength >= size(this%values)) then
            write (error_unit, *) 'IntRingBuffer error for addLast(): buffer full'
            stop
        else
            this%values(this%nextwriteindex) = value
            this%nextwriteindex = this%nextwriteindex + 1
            if (this%nextwriteindex > size(this%values)) then
                ! modulo not usable, because array index is 1-based
                this%nextwriteindex = this%nextwriteindex - size(this%values)
            end if
            this%valueslength = this%valueslength + 1
        end if
    end subroutine intringbuffer_addLast

    function intringbuffer_getFirst(this, defaultvalue) result(value)
        implicit none

        class(IntRingBuffer), intent(in) :: this
        integer, optional, intent(in)    :: defaultvalue
        integer                          :: value, index

        if (this%valueslength < 1) then
            if (present(defaultvalue)) then
                value = defaultvalue
            else
                write (error_unit, *) 'List error for getFirst(): empty list'
                stop
            end if
        else
            index = this%nextwriteindex - this%valueslength
            if (index < 1) then
                ! modulo not usable, because array index is 1-based
                index = index + size(this%values)
            end if
            value = this%values(index)
        end if
    end function intringbuffer_getFirst

    function intringbuffer_getLast(this, defaultvalue) result(value)
        implicit none

        class(IntRingBuffer), intent(in) :: this
        integer, optional, intent(in)    :: defaultvalue
        integer                          :: value, index

        if (this%valueslength < 1) then
            if (present(defaultvalue)) then
                value = defaultvalue
            else
                write (error_unit, *) 'List error for getLast(): empty list'
                stop
            end if
        else
            index = this%nextwriteindex - 1
            if (index < 1) then
                ! modulo not usable, because array index is 1-based
                index = index + size(this%values)
            end if
            value = this%values(index)
        end if
    end function intringbuffer_getLast

    function intringbuffer_get(this, theindex) result(value)
        implicit none

        class(IntRingBuffer), intent(in) :: this
        integer, optional, intent(in)    :: theindex
        integer                          :: value, index

        if (this%valueslength < 1) then
            write (error_unit, *) 'List error for get(): empty list'
            stop
        else if (theindex > this%valueslength) then
            write (error_unit, *) 'List error for get(): index too large'
            stop
        else if (theindex < 1) then
            write (error_unit, *) 'List error for get(): index too small'
            stop
        else
            index = this%nextwriteindex - this%valueslength + theindex - 1
            if (index < 1) then
                ! modulo not usable, because array index is 1-based
                index = index + size(this%values)
            end if
            value = this%values(index)
        end if
    end function intringbuffer_get

    function intringbuffer_removeFirst(this, defaultvalue) result(value)
        implicit none

        class(IntRingBuffer), intent(inout) :: this
        integer, optional, intent(in)       :: defaultvalue
        integer                             :: value, index

        if (this%valueslength < 1) then
            if (present(defaultvalue)) then
                value = defaultvalue
            else
                write (error_unit, *) 'List error for removeFirst: empty list'
                stop
            end if
        else
            index = this%nextwriteindex - this%valueslength
            if (index < 1) then
                ! modulo not usable, because array index is 1-based
                index = index + size(this%values)
            end if
            value = this%values(index)
            this%values(index) = 0  ! overwrite last value
            this%valueslength = this%valueslength - 1
        end if
    end function intringbuffer_removeFirst

    function intringbuffer_removeLast(this, defaultvalue) result(value)
        implicit none

        class(IntRingBuffer), intent(inout) :: this
        integer, optional, intent(in)       :: defaultvalue
        integer                             :: value, index

        if (this%valueslength < 1) then
            if (present(defaultvalue)) then
                value = defaultvalue
            else
                write (error_unit, *) 'List error for removeLast: empty list'
                stop
            end if
        else
            index = this%nextwriteindex - 1
            if (index < 1) then
                ! modulo not usable, because array index is 1-based
                index = index + size(this%values)
            end if
            value = this%values(index)
            this%values(index) = 0  ! overwrite last value
            this%valueslength = this%valueslength - 1
            this%nextwriteindex = this%nextwriteindex - 1
            if (this%nextwriteindex < 1) then
                ! modulo not usable, because array index is 1-based
                this%nextwriteindex = this%nextwriteindex + size(this%values)
            end if
        end if
    end function intringbuffer_removeLast

    pure function intringbuffer_empty(this) result(empty)
        implicit none

        class(IntRingBuffer), intent(in) :: this
        logical                          :: empty

        empty = (this%valueslength == 0)
    end function intringbuffer_empty

    pure function intringbuffer_length(this) result(count)
        implicit none

        class(IntRingBuffer), intent(in) :: this
        integer                          :: count

        count = this%valueslength
    end function intringbuffer_length

    ! pure!
    function intringbuffer_contains(this, value) result(contains)
        implicit none

        class(IntRingBuffer), intent(in) :: this
        integer, intent(in)              :: value
        logical                          :: contains

        contains = .false.
        write (error_unit, *) 'List error for contains(): not implemented'
        stop
    end function intringbuffer_contains

    subroutine intringbuffer_print(this, withdebug)
        implicit none

        class(IntRingBuffer), intent(in) :: this
        logical, optional, intent(in)    :: withdebug
        integer                          :: startindex

        if (this%valueslength == 0) then
            call printarray(this%values(1:0))  ! empty array
        else if (this%nextwriteindex > this%valueslength) then
            call printarray(this%values(this%nextwriteindex-this%valueslength:this%nextwriteindex-1))
        else
            startindex = this%nextwriteindex - this%valueslength + size(this%values)
            call printarray(this%values(startindex:size(this%values)), this%values(1:this%nextwriteindex-1))
        end if
        if (present(withdebug)) then
            if (withdebug .eqv. .true.) then
                ! print also the subjacent array and limits
                write (*, '(A)', advance='no') '<-'
                call printarray(this%values)
            end if
        end if
    end subroutine

end module class_intringbuffer
