module element_utils

    use iso_fortran_env, only: r64 => real64

    implicit none
    private
    public :: linspace, meshgrid

    contains
        function linspace(x0, x1, n) result(x)
            ! Generate an array of n evenly spaced points from x0 to x1
            real(r64), intent(in) :: x0, x1
            integer, intent(in) :: n
            real(r64) :: x(n)

            ! Loc vars
            integer :: i
            real(r64) :: dx
            
            ! Compute space between points
            x(1) = x0
            x(n) = x1
            dx = (x1 - x0)/(n - 1)

            ! Compute the array of evenly spaced points 
            x(2:n-1) = [(x0 + (i-1)*dx, i=2, n-1)]
        end function linspace

        function meshgrid(x, y) result(grid)
            ! Create a stacked 2D grid of coordinate points from the cartesian product of two 1D input arrays x and y
            ! x-coordinates are stored in the first index
            ! y-coordinates are stored in the second index
            real(r64), intent(in) :: x(:), y(:)
            real(r64) :: grid(2, size(x), size(y))

            ! Loc vars
            integer :: i, j
            
            ! Create the grid for all combination of x and y
            do i = 1, size(x)
                do j = 1, size(y)
                    grid(1, i, j) = x(i)
                    grid(2, i, j) = y(j)
                end do
            end do
        end function meshgrid
end module element_utils