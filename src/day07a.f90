!> Solution for https://adventofcode.com/2023/day/7 part a
module day07a
    use iso_fortran_env, only : int64
    use util, only : readinputfile_asstringarray, code_0, code_9
    implicit none
    private

    integer, parameter :: maxlinelength = 60
    integer, parameter :: cardcount = 5
    integer, parameter :: totalcardtypes = 14  ! but cardtype 1 is never used

    type :: Hand
        private

        character(len=1) :: cards(cardcount)
        integer          :: cardtypes(cardcount)
        integer          :: bid
        integer          :: handtype
    contains
        procedure :: init    => hand_init
        ! procedure :: compare => hand_compare
        procedure :: print   => hand_print
    end type Hand

    public :: solve

contains

    subroutine hand_init(this, handtype)
        implicit none

        class(Hand), intent(inout) :: this
        integer, intent(in)        :: handtype

        ! cards and bid are already set
        this%handtype = handtype
    end subroutine

    subroutine hand_print(this, rank)
        implicit none

        class(Hand), intent(in) :: this
        integer, intent(in)     :: rank

        print '(I4, A1, 5A1, 5I3, A, I4, A, I1)', rank, '', this%cards, this%cardtypes, ' / ', this%bid, ' / ', this%handtype
        ! print '(I4, A1, 5A1, A, I4, A, I1)', rank, '', this%cards, ' ', this%bid, ' ', this%handtype
    end subroutine

    ! order ascending
    function inrightorder(handleft, handright) result(ordered)
        implicit none

        type(Hand), intent(in) :: handleft, handright
        logical                :: ordered
        integer                :: c

        ! print *, '------------'
        ! call handleft%print()
        ! call handright%print()
        ! print *, '------------'
        if (handleft%handtype < handright%handtype) then
            ordered = .true.
            ! print *, 'ordered'
        else if (handleft%handtype > handright%handtype) then
            ordered = .false.
            ! print *, 'not ordered'
        else
            ! both hand have the same handtype
            do c = 1, cardcount
                ! print *, handleft%cardtypes(c), handright%cardtypes(c)
                if (handleft%cardtypes(c) < handright%cardtypes(c)) then
                    ordered = .true.
                    ! print *, 'ordered'
                    exit
                else if (handleft%cardtypes(c) > handright%cardtypes(c)) then
                    ordered = .false.
                    ! print *, 'not ordered'
                    exit
                end if
            end do
            ! write (error_unit, *) 'Hand error for inrightorder(): hands have the same handtype and the same cards'
        end if
    end function

    recursive subroutine merge_sort(hands, from, to, handstmp)
        implicit none

        type(Hand), intent(inout) :: hands(:), handstmp(:)
        integer, intent(in)                :: from, to
        integer                            :: from2, index1, index2, indexall

        ! only sort if more than one element
        if (from < to) then
            ! divide and sort both parts
            from2 = (from + to + 1) / 2
            call merge_sort(hands, from, from2 - 1, handstmp)
            call merge_sort(hands, from2, to, handstmp)

            ! merge both parts into tmp buffer
            index1 = from
            index2 = from2
            indexall = from
            do while(index1 <= from2 - 1 .and. index2 <= to)
                if (inrightorder(hands(index1), hands(index2))) then
                    handstmp(indexall) = hands(index1)
                    index1 = index1 + 1
                else
                    handstmp(indexall) = hands(index2)
                    index2 = index2 + 1
                end if
                indexall = indexall + 1
            end do
            do while(index1 <= from2 - 1)
                handstmp(indexall) = hands(index1)
                index1 = index1 + 1
                indexall = indexall + 1
            end do
            do while(index2 <= to)
                handstmp(indexall) = hands(index2)
                index2 = index2 + 1
                indexall = indexall + 1
            end do

            ! copy tmp buffer back to intial array
            hands(from:to) = handstmp(from:to)
        end if
    end subroutine

    function scan_hands(lines) result(total_winnings)
        implicit none

        character(len=*), intent(in)  :: lines(:)
        integer(int64)                :: total_winnings
        character(len=:), allocatable :: line
        type(Hand), allocatable       :: hands(:), handstmp(:)
        integer                       :: i, c, frequencies(2:totalcardtypes), cardtype, handtype, first2loc
        character(len=1)              :: card

        allocate(hands(size(lines)))
        allocate(handstmp(size(lines)))  ! for merge_sort

        ! scan all hands to assign a handtype each
        do i = 1, size(lines)
            line = lines(i)
            read (line, '(5A1, 1X, I4)') hands(i)%cards, hands(i)%bid
            frequencies(:) = 0
            do c = 1, cardcount
                card = hands(i)%cards(c)
                select case(card)
                case ('A')
                    cardtype = totalcardtypes
                case ('K')
                    cardtype = 13
                case ('Q')
                    cardtype = 12
                case ('J')
                    cardtype = 11
                case ('T')
                    cardtype = 10
                case default
                    cardtype = iachar(card) - code_0
                end select
                hands(i)%cardtypes(c) = cardtype
                frequencies(cardtype) = frequencies(cardtype) + 1
            end do
            ! print '(14I2)', frequencies
            if (findloc(frequencies, 5, 1) > 0) then
                handtype = 7  ! Five of a kind
            else if (findloc(frequencies, 4, 1) > 0) then
                handtype = 6  ! Four of a kind
            else if (findloc(frequencies, 3, 1) > 0) then
                if (findloc(frequencies, 2, 1) > 0) then
                    handtype = 5  ! Full House
                else
                    handtype = 4  ! Three of a kind
                end if
            else
                first2loc = findloc(frequencies, 2, 1)
                if (first2loc > 0) then
                    frequencies(first2loc + 1) = 0  ! remove first 2 and search again for a second 2
                    if (findloc(frequencies, 2, 1) > 0) then
                        handtype = 3  ! Two pairs
                    else
                        handtype = 2  ! One pair
                    end if
                else
                    handtype = 1  ! High card
                end if
            end if
            call hands(i)%init(handtype)
            ! print '(14I2)', frequencies
            ! call hands(i)%print()
        end do
        ! print *

        ! sort hands
        call merge_sort(hands, 1, size(hands), handstmp)

        ! calculate total winnings from bid and rank
        total_winnings = 0
        do i = 1, size(hands)
            ! call hands(i)%print(i)
            total_winnings = total_winnings + i * hands(i)%bid
        end do
    end function scan_hands

    integer(int64) function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        integer(int64)                :: total_winnings

        lines = readinputfile_asstringarray(filename, maxlinelength)
        total_winnings = scan_hands(lines)

        solve = total_winnings
    end function

end module day07a
