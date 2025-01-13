program solve_linear_system
      use iso_fortran_env, only: r64 =>real64
      use linear_algebra, only: invert

      implicit none
    
      real(r64), allocatable :: A(:,:), B(:, :)

      A = reshape([1, 2, 3, 2, 2, 1, 3, 1, 2], shape=[3, 3])
      print *, invert(A)
end program solve_linear_system
    