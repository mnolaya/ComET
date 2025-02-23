program main

    use iso_fortran_env, only: r64 => real64
    use element_utils, only: linspace, meshgrid
    use element_library, only: LinearElement_t
    use elastic_materials, only: compute_D_2D_isotropic
    use constants

    implicit none

    integer, parameter :: ngrid = 50
    integer :: i, j, funit
    real(r64), parameter :: E = 250, nu = 0.25
    real(r64) :: elem_coords(4, 2), eta_1(ngrid), eta_2(ngrid), grid(2, ngrid, ngrid), xy(2), D(3, 3)
    real(r64), allocatable :: N(:, :), node_coords(:), k(:, :)
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

    ! Create file for writing output
    open(newunit=funit, file='N.out', status='old', action='write')

    ! Compute global coordinates and shape functions for verification
    node_coords = elem%get_nodal_coordinate_vec(loc='global')
    write(funit, *) 'eta1 ', 'eta2 ', 'x1 ', 'x2 ', 'N1 ', 'N2 ', 'N3 ', 'N4'
    do i = 1, size(grid, dim=2)
        do j = 1, size(grid, dim=3)
            ! Shape function matrix 
            N = elem%compute_N(grid(:, i, j))

            ! Global xy coordinates
            xy = matmul(N, node_coords)
            write(funit, *) grid(1, i, j), grid(2, i, j), xy, N(1, 1), N(1, 3), N(1, 5), N(1, 7)
        end do
    end do
    close(funit)

    ! Create file for writing output
    open(newunit=funit, file='k.out', status='old', action='write')

    ! Compute the element stiffness matrix.
    D = compute_D_2D_isotropic(E, nu, 'strain')
    k = elem%compute_k(D)
    do i = 1, size(k, dim=1)
        write(funit, *) k(i, :)
    end do
    close(funit)
end program main