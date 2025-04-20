program main

    use, intrinsic :: iso_fortran_env, only: r64 => real64

    implicit none

    integer :: filesize

    open(10, file='ex_2fib.vtu', access='stream', action='read', status='old')
    inquire(10, size=filesize)
    print *, filesize

end program main

! !  TestStream.f90 
! program TestStream
!     implicit none
!     integer, parameter :: BigBufferSize = 100   ! Your number will vary
!     character(len=BigBufferSize) :: BigBuffer
!     integer :: i
!     integer(8) :: fileSize, position, charactersRead
!     open(10,file="testData", access="stream", action="write", status="unknown")
!     do i = 1, 123   ! some number not multiple of BigBufferSize
!         write(10) "x"
!     end do
!     close(10)
!     open(10,file="testData", access="stream", action="read", status="old")
!     inquire(10, size=filesize)
!     position = 0
!     do while(position < fileSize)
!         charactersRead = min(fileSize - position, BigBufferSize)
!         read(10, end=100) BigBuffer(1:charactersRead)
! 100     if(charactersRead < BigBufferSize) BigBuffer(charactersRead+1:) = " "
!         position = position + charactersRead
!         print *, BigBuffer
!     end do
!     close(10)