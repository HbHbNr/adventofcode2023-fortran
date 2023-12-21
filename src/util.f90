!> Collection of utility functions
module util
    use iso_fortran_env, only : int32, int64
    implicit none

    private

    integer, parameter, public :: code_0 = iachar('0')
    integer, parameter, public :: code_9 = iachar('9')
    integer, parameter, public :: code_upper_a = iachar('A')
    integer, parameter, public :: code_upper_z = iachar('Z')
    integer, parameter, public :: code_lower_a = iachar('a')
    integer, parameter, public :: code_lower_z = iachar('z')

    public :: gausss_area_formular
    public :: lcm
    public :: printarray
    public :: printresultline_integer
    public :: printresultline_int64
    public :: printresultline_stringarray
    public :: printresultline
    public :: printioerror
    public :: readinputfile_asline
    public :: readinputfile_asstringarray
    public :: readinputfile_asintarray
    public :: string_extract_int64s
    public :: string_extract_integers

contains

    ! determine the area of a simple polygon whose vertices are described
    ! by their Cartesian coordinates in the plane
    function gausss_area_formular(coords) result(total_area)
        implicit none

        integer, intent(in) :: coords(:,:)
            !! Cartesian coordinates of the vertices, dimension 1 is 2, dimension 2 is the number of vertices;
            !! first part of the coordinate is y, second part of the coordinate is x
        integer(int64)      :: total_area
        integer(int64)      :: area
        integer             :: i, p1(2), p2(2), width, doubleheight

        total_area = 0
        p1 = coords(:, size(coords, 2))
        ! the last step leads back to the start
        do i = 1, size(coords, 2)
            p2 = coords(:, i)
            doubleheight = p1(1) + p2(1)
            width = p1(2) - p2(2)
            ! divide trapezoid by 2
            area = 1_int64 * doubleheight * width / 2
            total_area = total_area + area
            p1 = p2
        end do
    end function

    !> find least common multiplier of an array of int64 numbers
    integer(int64) function lcm(orgnumbers)
        implicit none

        integer(int64), intent(in)  :: orgnumbers(:)
        integer(int64), allocatable :: numbers(:)
        integer(int64)              :: increment, tmp
        integer                     :: largestloc, i, remaining, nextremaining
        logical                     :: run

        ! create copy of the original numbers
        numbers = orgnumbers

        ! find largest element and use it as increment
        largestloc = maxloc(numbers, 1)
        increment = numbers(largestloc)
        ! move largest to the end of the array
        if (largestloc /= size(numbers)) then
            tmp = numbers(size(numbers))
            numbers(size(numbers)) = numbers(largestloc)
            numbers(largestloc) = tmp
        end if
        nextremaining = size(numbers) - 1

        lcm = 0
        run = .true.
        do while (run)
            lcm = lcm + increment
            remaining = nextremaining
            do i = 1, remaining
                if (modulo(lcm, numbers(i)) == 0) then
                    nextremaining = nextremaining - 1
                    if (nextremaining > 0) then
                        increment = lcm
                        ! start from zero to test the same number again, because the numbers
                        ! could include duplicates
                        lcm = 0
                        ! swap current number to the current end
                        tmp = numbers(remaining)
                        numbers(remaining) = numbers(i)
                        numbers(i) = tmp
                        ! continue to run because there are more numbers
                        run = .true.
                    else
                        ! no more numbers, result calculated
                        run = .false.
                    end if
                    exit
                end if
            end do
            ! if none divides current lcm: next round
        end do
    end function

    !> print an array of integers, supports default integer type as well as integer(int64)
    subroutine printarray(array1, array2)
        implicit none

        class(*), intent(in)           :: array1(:)
        class(*), optional, intent(in) :: array2(:)
        character(len=20)              :: result
        integer                        :: i
        logical                        :: first

        first = .true.
        write (*, '(A)', advance='no') '['
        do i = lbound(array1, 1), ubound(array1, 1)
            if (first) then
                first = .false.
            else
                write (*, '(A)', advance='no') ','
            end if
            select type(array1)
                type is (integer(int32))
                    write(result, '(I20)') array1(i)
                    write (*, '(A)', advance='no') trim(adjustl(result))
                type is (integer(int64))
                    write(result, '(I20)') array1(i)
                    write (*, '(A)', advance='no') trim(adjustl(result))
                type is (character(len=*))
                    write (*, '(A)', advance='no') trim(adjustl(array1(i)))
                class default
                    print *, 'unsupported type in printarray()'
                    stop
            end select
        end do
        if (present(array2)) then
            do i = lbound(array2, 1), ubound(array2, 1)
                if (first) then
                    first = .false.
                else
                    write (*, '(A)', advance='no') ','
                end if
                select type(array2)
                    type is (integer(int32))
                        write(result, '(I20)') array2(i)
                        write (*, '(A)', advance='no') trim(adjustl(result))
                    type is (integer(int64))
                        write(result, '(I20)') array2(i)
                        write (*, '(A)', advance='no') trim(adjustl(result))
                    type is (character(len=*))
                        write (*, '(A)', advance='no') trim(adjustl(array2(i)))
                    class default
                        print *, 'unsupported type in printarray()'
                        stop
                end select
            end do
        end if
        write (*, '(A)', advance='yes') ']'
    end subroutine

    !> print a standard AOC result line, with an integer parameter
    subroutine printresultline_integer(day, intresult)
        implicit none

        character(len=*), intent(in) :: day
        integer(kind=4),  intent(in) :: intresult
        character(len=11)            :: result

        write(result, '(I11)') intresult
        call printresultline(day, result)
    end subroutine

    !> print a standard AOC result line, with an int64 parameter
    subroutine printresultline_int64(day, intresult)
        implicit none

        character(len=*), intent(in) :: day
        integer(int64),  intent(in)  :: intresult
        character(len=22)            :: result

        write(result, '(I22)') intresult
        call printresultline(day, result)
    end subroutine

    !> print a standard AOC result line, with an array of strings parameter
    subroutine printresultline_stringarray(day, stringsresult)
        implicit none

        character(len=*), intent(in) :: day
        character(len=*), intent(in) :: stringsresult(:)
        integer                      :: i

        call printresultline(day, '')
        do i = 1, size(stringsresult)
            print '(A)', stringsresult(i)
        end do
    end subroutine

    !> print a standard AOC result line, with a string parameter
    subroutine printresultline(day, result)
        implicit none

        character(len=*), intent(in) :: day
        character(len=*), intent(in) :: result

        print '(A, A, A, A)', 'Day ', day, ': ', adjustl(result)
    end subroutine

    !> print an I/O error message, and stop program on serious I/O error or by request
    subroutine printioerror(iostat, iomsg, alwaysstop)
        implicit none

        integer, intent(in)           :: iostat
        character(len=*), intent(in)  :: iomsg
        logical, intent(in), optional :: alwaysstop
        character(len=80)             :: iostatstr

        ! iostat:
        !   ==  0 -> no error
        !   == -1 -> end of file
        !    >  0 -> serious I/O error
        if (iostat > 0) then
            write (iostatstr, *) iostat
            print '(A, A, A, A, A)', 'I/O error: ', trim(adjustl(iostatstr)), ' (', trim(iomsg), ')'
            stop
        end if
        if (present(alwaysstop)) then
            if (alwaysstop .eqv. .true.) then
                stop
            end if
        end if
    end subroutine

    !> read the first (and maybe only) line of a file into a string
    function readinputfile_asline(filename) result(line)
        implicit none

        character(len=*), intent(in)  :: filename
            !! name of the file
        integer                       :: io, iostat
        character(len=512)            :: iomsg
        integer                       :: filesize
        character(len=:), allocatable :: tmpline, line

        ! open file for reading
        open(newunit=io, file=filename, status='old', action='read', iostat=iostat, iomsg=iomsg)
        if (iostat /= 0) then
            call printioerror(iostat, iomsg, .true.)
        end if

        ! get file size
        inquire(file=filename, size=filesize)

        ! allocate string to read complete file
        allocate(character(len=filesize) :: tmpline)
        read(io, '(A)', iostat=iostat, iomsg=iomsg) tmpline
        if (iostat /= 0) then
            ! end of file or I/O error -> exit loop
            call printioerror(iostat, iomsg, .true.)
        end if
        close(io)

        ! remove blanks or newlines at the end of the file
        line = tmpline(:len_trim(tmpline))
        deallocate(tmpline)
    end function

    !> read all lines of a file into an array of strings, with the length
    !> of the strings fitting to the longest line
    function readinputfile_asstringarray(filename, linebufferlength) result(lines)
        implicit none

        character(len=*), intent(in)    :: filename
            !! name of the file
        integer, intent(in)             :: linebufferlength
            !! length of a buffer for one line, big enough for the longest line
        integer                         :: io, iostat
        character(len=512)              :: iomsg
        integer                         :: linecount, linelength, maxlinelength
        character(len=linebufferlength) :: tmpline
        character(len=:), allocatable   :: line
        character(len=:), allocatable   :: lines(:)

        ! open file for reading
        open(newunit=io, file=filename, status='old', action='read', iostat=iostat, iomsg=iomsg)
        if (iostat /= 0) then
            call printioerror(iostat, iomsg, .true.)
        end if

        ! get file's number of lines and length of the longest line
        linecount = 0
        linelength = 0
        maxlinelength = 0
        do
            read(io, '(A)', iostat=iostat, iomsg=iomsg) tmpline
            if (iostat /= 0) then
                ! end of file or I/O error -> stop program
                call printioerror(iostat, iomsg)
                exit
            end if
            linecount = linecount + 1
            linelength = len_trim(tmpline)
            maxlinelength = max(maxlinelength, linelength)
        end do
        ! print *, 'linecount=', linecount
        ! print *, 'maxlinelength=', maxlinelength

        ! create new array and read all lines from file
        allocate(character(len=maxlinelength) :: lines(linecount))
        allocate(character(len=maxlinelength) :: line)
        rewind(io)
        linecount = 0
        do
            read(io, '(A)', iostat=iostat, iomsg=iomsg) line
            if (iostat /= 0) then
                ! end of file or I/O error -> stop program
                call printioerror(iostat, iomsg)
                exit
            end if
            linecount = linecount + 1
            lines(linecount) = line
            ! print *, '"', line, '"', linecount
        end do
        close(io)
    end function

    !> read all lines of a file into an array of int, with the dimensions
    !> of the array fitting to the content of the rectangular file
    function readinputfile_asintarray(filename, linebufferlength) result(intarray)
        use iso_fortran_env, only : int8
        implicit none

        character(len=*), intent(in)    :: filename
            !! name of the file
        integer, intent(in)             :: linebufferlength
            !! length of a buffer for one line, big enough for the longest line
        integer                         :: io, iostat
        character(len=512)              :: iomsg
        integer                         :: linecount, linelength
        character(len=linebufferlength) :: tmpline
        character(len=20)               :: format
        integer(int8), allocatable      :: tmpintarray(:,:), intarray(:,:)

        ! open file for reading
        open(newunit=io, file=filename, status='old', action='read', iostat=iostat, iomsg=iomsg)
        if (iostat /= 0) then
            call printioerror(iostat, iomsg, .true.)
        end if

        ! get file's number of lines and length per line
        linecount = 0
        linelength = 0
        do
            read(io, '(A)', iostat=iostat, iomsg=iomsg) tmpline
            if (iostat /= 0) then
                ! end of file or I/O error -> stop program
                call printioerror(iostat, iomsg)
                exit
            end if
            linecount = linecount + 1
            linelength = len_trim(tmpline)
        end do
        ! print *, 'linecount=', linecount
        ! print *, 'linelength=', linelength

        ! create new array and read all lines from file
        allocate(tmpintarray(linelength,linecount))
        ! allocate(intarray(linecount, linelength))
        rewind(io)
        write(format, *) '(', linelength, '(I1))'  ! format for a single line of the array
        read(io, format, iostat=iostat, iomsg=iomsg) tmpintarray
        if (iostat /= 0) then
            ! end of file or I/O error -> stop program
            call printioerror(iostat, iomsg, .true.)
        end if
        intarray = transpose(tmpintarray)
        deallocate(tmpintarray)
        close(io)
    end function

    !> prepare an array and fill it with int64 integers extracted from a string
    subroutine string_extract_int64s(string, numbers)
        implicit none

        character(len=*), intent(in)             :: string
        integer(int64), allocatable, intent(out) :: numbers(:)
        integer                           :: i, numbercount
        logical                           :: gap

        ! count amount of numbers
        numbercount = 0
        gap = .true.
        do i = 1, len_trim(string)
            if (iachar(string(i:i)) >= code_0) then
                if (iachar(string(i:i)) <= code_9) then
                    ! current character is a number
                    if (gap) then
                        numbercount = numbercount + 1
                        gap = .false.
                    end if
                    cycle
                end if
            end if
            ! current character is not a number
            gap = .true.
        end do

        ! prepare array
        allocate(numbers(numbercount))

        ! read numbers into array
        read (string, *) numbers(:)
    end subroutine

    !> prepare an array and fill it with integers extracted from a string
    subroutine string_extract_integers(string, numbers)
        implicit none

        character(len=*), intent(in)      :: string
        integer, allocatable, intent(out) :: numbers(:)
        integer                           :: i, numbercount
        logical                           :: gap

        ! count amount of numbers
        numbercount = 0
        gap = .true.
        do i = 1, len_trim(string)
            if (iachar(string(i:i)) >= code_0) then
                if (iachar(string(i:i)) <= code_9) then
                    ! current character is a number
                    if (gap) then
                        numbercount = numbercount + 1
                        gap = .false.
                    end if
                    cycle
                end if
            end if
            ! current character is not a number
            gap = .true.
        end do

        ! prepare array
        allocate(numbers(numbercount))

        ! read numbers into array
        read (string, *) numbers(:)
    end subroutine

end module util
