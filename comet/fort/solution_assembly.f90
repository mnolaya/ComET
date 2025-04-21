module solution_assembly

    use, intrinsic :: iso_fortran_env, only: r64 => real64
    use element_library, only: Connectivity_t, FiniteElement_t

    implicit none

    private
    public :: assemble_global_matrices

    contains
        !> Get the index corresponding to a (node, component) combination for some dofs in the finite element matrices
        function get_node_matrix_index(node_num, component, dofs) result(idx)
            ! Args
            integer, intent(in) :: node_num, component, dofs
            integer :: idx

            idx = dofs*(node_num - 1) + component - 1
        end function get_node_matrix_index

        !> Assemble the global K and F matrices to solve the system of equations formed by the FE assembly (K*U = F) 
        subroutine assemble_global_matrices(dofs, connectivity, elements, K, F)
            ! Args
            integer, intent(in) :: dofs
            type(Connectivity_t), intent(in) :: connectivity(:)
            class(FiniteElement_t), allocatable, intent(in) :: elements(:)
            real(r64), allocatable, intent(out) :: K(:, :), F(:, :)

            ! Loc vars
            integer :: i, j, m, n, p   ! Looping variables
            integer :: num_nodes, num_nodes_
            integer :: loc_node_num, glob_node_num, iloc_row, iglob_row, iloc_col, iglob_col
            real(r64), allocatable :: k_e(:, :), f_e(:, :)

            ! Initialize global matrices
            num_nodes_ = 0
            do i = 1, size(connectivity)
                num_nodes_ = maxval(connectivity(i)%nodes)
                if (num_nodes_ > num_nodes) num_nodes = num_nodes_
            end do          
            allocate(K(dofs*num_nodes, dofs*num_nodes))
            allocate(F(dofs*num_nodes, 1))
            K = 0
            F = 0

            ! Loop through all elements and assemble
            do i = 1, size(connectivity)
                ! Compute the local element stiffness matrix and force vector
                k_e = elements(i)%compute_k()
                ! f_e = ...

                ! Loop through nodes on element
                do j = 1, size(connectivity(i)%nodes)
                    ! Get the current local and global node numbers
                    loc_node_num = j
                    glob_node_num = connectivity(i)%nodes(j)

                    ! Loop through degrees of freedom
                    do m = 1, dofs
                        ! Get the row index corresponding to local/global node, dof component m
                        iloc_row = get_node_matrix_index(loc_node_num, m, dofs)
                        iglob_row = get_node_matrix_index(glob_node_num, m, dofs)

                        ! ! Update the global force vector
                        ! F(iglob_row, 1) = F(iglob_row, 1) + f_e(iloc_row, 1)

                        ! Loop through nodes on element
                        do n = 1, size(connectivity(i)%nodes)
                            ! Get the current local and global node numbers
                            loc_node_num = n
                            glob_node_num = connectivity(i)%nodes(n)

                            ! Loop through degrees of freedom
                            do p = 1, dofs
                                ! Get the row index corresponding to local/global node, dof component m
                                iloc_col = get_node_matrix_index(loc_node_num, p, dofs)
                                iglob_col = get_node_matrix_index(glob_node_num, p, dofs)

                                ! Update the global stiffness matrix
                                K(iglob_row, iglob_col) = K(iglob_row, iglob_col) + k_e(iloc_row, iloc_col)
                            end do
                        end do
                    end do
                end do
            end do          
        end subroutine assemble_global_matrices
end module solution_assembly