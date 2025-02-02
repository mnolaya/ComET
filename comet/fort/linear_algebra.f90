module linear_algebra
    use iso_fortran_env, only: r64 => real64
    
    implicit none
    private
    public :: invert, determinant
    
    contains
          function invert(A) result(A_inv)
                ! Compute the inverse of a matrix A using LAPACK routines for LU-factorization
                real(r64), intent(in) :: A(:, :)
                real(r64), allocatable :: A_inv(:, :)

                ! Loc vars
                integer :: n, lda, lwork, info
                integer, allocatable :: ipiv(:)
                real(r64), allocatable :: A_(:, :), work(:)

                ! From LAPACK documenation:
                      ! DGETRF computes an LU factorization of a general M-by-N matrix A
                      ! using partial pivoting with row interchanges.
                
                      ! The factorization has the form
                      !    A = P * L * U
                      ! where P is a permutation matrix, L is lower triangular with unit
                      ! diagonal elements (lower trapezoidal if m > n), and U is upper
                      ! triangular (upper trapezoidal if m < n).
                
                      ! This is the right-looking Level 3 BLAS version of the algorithm.
                      
                      ! DGETRI computes the inverse of a matrix using the LU factorization
                      ! computed by DGETRF.
                
                      ! This method inverts U and then computes inv(A) by solving the system
                      ! inv(A)*L = inv(U) for inv(A).

                ! Make a copy of the input matrix for the LAPACK routine
                A_ = A

                ! Set dimensions
                n = size(A, dim=1)
                lda = n
                lwork = n
                allocate(ipiv(n), work(lwork))

                ! Factorize
                call dgetrf(n, n, A_, lda, ipiv, info)

                ! Invert
                call dgetri(n, A_, lda, ipiv, work, lwork, info)

                ! Copy inverted matrix to output argument
                A_inv = A_
          end function invert      

          function determinant(A) result(det)
            ! Compute the determinant of matrix A using LAPACK routines for LU-factorization
            real(r64), intent(in) :: A(:, :)
            real(r64) :: det

            ! Loc vars
            integer :: i, n, lda, lwork, info
            integer, allocatable :: ipiv(:)
            real(r64), allocatable :: A_(:, :), work(:)

            ! From LAPACK documenation:
                  ! DGETRF computes an LU factorization of a general M-by-N matrix A
                  ! using partial pivoting with row interchanges.
            
                  ! The factorization has the form
                  !    A = P * L * U
                  ! where P is a permutation matrix, L is lower triangular with unit
                  ! diagonal elements (lower trapezoidal if m > n), and U is upper
                  ! triangular (upper trapezoidal if m < n).
            
                  ! This is the right-looking Level 3 BLAS version of the algorithm.

            ! Make a copy of the input matrix for the LAPACK routine
            A_ = A

            ! Set dimensions
            n = size(A, dim=1)
            lda = n
            lwork = n
            allocate(ipiv(n), work(lwork))

            ! Factorize
            call dgetrf(n, n, A_, lda, ipiv, info)

            ! Invert
            call dgetri(n, A_, lda, ipiv, work, lwork, info)

            ! Compute the determinant
            det = 1
            do i = 1, n
                  det = det*A(i, i)
                  if (ipiv(i) /= i) det = -det
            end do
          end function determinant
end module linear_algebra