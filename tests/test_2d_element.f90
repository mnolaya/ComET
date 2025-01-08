program main

    use iso_fortran_env, only: r64 => real64
    use element_utils, only: linspace, meshgrid
    use element_library, only: LinearElement_t
    use constants

    implicit none

    integer, parameter :: ngrid = 50
    integer :: i, j
    real(r64) :: elem_coords(4, 2), eta_1(ngrid), eta_2(ngrid), grid(2, ngrid, ngrid)
    type(LinearElement_t) :: elem
    
    ! Construct a linear element
    elem_coords(1, :) = [-2.0_r64, 0.0_r64]
    elem_coords(2, :) = [1.0_r64, 1.0_r64]
    elem_coords(3, :) = [2.0_r64, 1.5_r64]
    elem_coords(4, :) = [-1.0_r64, 1.0_r64]
    elem = LinearElement_t(elem_coords)

    ! Create a grid of natural coordinates
    eta_1 = linspace(-1.0_r64, 1.0_r64, ngrid)
    eta_2 = linspace(-1.0_r64, 1.0_r64, ngrid)
    grid = meshgrid(eta_1, eta_2)

    ! Compute shape functions for verification
    print *, 'eta1 ', 'eta2 ', 'N1 ', 'N2 ', 'N3 ', 'N4'
    do i = 1, ngrid
        do j = 1, ngrid
            print *, grid(1, i, j), grid(2, i, j), &
            elem%nodes(1)%shape_func([grid(1, i, j), grid(2, i, j)]), &
            elem%nodes(2)%shape_func([grid(1, i, j), grid(2, i, j)]), &
            elem%nodes(3)%shape_func([grid(1, i, j), grid(2, i, j)]), &
            elem%nodes(4)%shape_func([grid(1, i, j), grid(2, i, j)])
        end do
    end do
end program main