module element_library

    use iso_fortran_env, only: r64 => real64
    use constants, only: CHAR_SIZE

    implicit none
    private

    type :: ShapeFunctionPointer
        procedure(shape_func_iface), pointer, nopass :: f => null()
    end type ShapeFunctionPointer

    type :: Node_t
        ! Type for a node on a finite element
        integer :: number
        real(r64), allocatable :: natural_coords(:)
        real(r64), allocatable :: element_coords(:)
        real(r64), allocatable :: global_coords(:)
        type(ShapeFunctionPointer) :: shape_func
        type(ShapeFunctionPointer), allocatable :: shape_func_deriv(:)
    end type Node_t

    type, abstract :: FiniteElement_t
        ! Base class for a finite element
        integer :: number, ndim, ndof, nnodes
        character(CHAR_SIZE) :: material_name
        type(Node_t), allocatable :: nodes(:)
        real(r64), allocatable :: N(:, :)
        contains
            procedure, pass :: inspect => inspect_element
            procedure, pass :: get_nodal_coordinate_vec
            procedure(shape_func_matrix_iface), deferred, pass :: compute_N
            procedure(shape_func_matrix_iface), deferred, pass :: compute_dN
            procedure(jacobian_iface), deferred, pass :: compute_J
            procedure(B_matrix_iface), deferred, pass :: compute_B
    end type FiniteElement_t

    type, extends(FiniteElement_t), public :: LinearElement_t
        ! Linear finite element type
        contains
            procedure, pass :: compute_N => compute_N_linear
            procedure, pass :: compute_dN => compute_dN_linear
            procedure, pass :: compute_J => compute_J_linear
            procedure, pass :: compute_B => compute_B_linear
    end type LinearElement_t

    abstract interface
        function shape_func_iface(natural_coords) result(N)
            ! Generic interface for a shape function computation
            import r64
            real(r64), intent(in) :: natural_coords(:)
            real(r64) :: N
        end function shape_func_iface

        function shape_func_matrix_iface(self, natural_coords) result(N)
            ! Deferred interface for computing the element shape function matrix N and shape function derivative matrix dN
            import r64, FiniteElement_t
            class(FiniteElement_t), intent(in) :: self
            real(r64), intent(in) :: natural_coords(:)
            real(r64), allocatable :: N(:, :)
        end function shape_func_matrix_iface

        function jacobian_iface(self, dN) result(J)
            ! Deferred interface for computing the "full" element Jacobian matrix J
            import r64, FiniteElement_t
            class(FiniteElement_t), intent(in) :: self
            real(r64), intent(in) :: dN(:, :)
            real(r64), allocatable :: J(:, :)
        end function jacobian_iface

        function B_matrix_iface(self, dN, J) result(B)
            ! Deferred interface for computing the element B matrix
            import r64, FiniteElement_t
            class(FiniteElement_t), intent(in) :: self
            real(r64), intent(in) :: dN(:, :), J(:, :)
            real(r64), allocatable :: B(:, :)            
        end function B_matrix_iface
    end interface

    interface LinearElement_t
        module procedure construct_linear_element
    end interface LinearElement_t

    contains
        function construct_linear_element(global_coords) result(elem)
            ! Construct a linear finite element
            real(r64), intent(in) :: global_coords(:, :)
            type(LinearElement_t) :: elem

            ! Loc vars
            integer :: i

            ! Assign element parameters
            elem%nnodes = size(global_coords, dim=1)
            elem%ndim = size(global_coords, dim=2)
            elem%ndof = elem%ndim

            ! Allocate...
            allocate(elem%nodes(elem%nnodes)) ! Element nodes
            
            ! Set the element global coordinates
            do i = 1, elem%nnodes
                elem%nodes(i)%number = i
                allocate(elem%nodes(i)%element_coords(elem%ndim))
                allocate(elem%nodes(i)%global_coords(elem%ndim))
                allocate(elem%nodes(i)%natural_coords(elem%ndim))
                allocate(elem%nodes(i)%shape_func_deriv(elem%ndim))
                elem%nodes(i)%global_coords = global_coords(i, :)
                elem%nodes(i)%element_coords = global_coords(i, :) - elem%nodes(1)%global_coords
            end do
            
            ! Update the element natural coordinates, ordered CCW
            ! Update the shape function and shape function derivative pointers
            select case (elem%ndim)
            case (2)
                elem%nodes(1)%natural_coords = [-1, -1]
                elem%nodes(1)%shape_func%f => shape_N1_linear
                elem%nodes(1)%shape_func_deriv(1)%f => shape_deriv_N11_linear
                elem%nodes(1)%shape_func_deriv(2)%f => shape_deriv_N12_linear
                elem%nodes(2)%natural_coords = [1, -1]
                elem%nodes(2)%shape_func%f => shape_N2_linear
                elem%nodes(2)%shape_func_deriv(1)%f => shape_deriv_N21_linear
                elem%nodes(2)%shape_func_deriv(2)%f => shape_deriv_N22_linear
                elem%nodes(3)%natural_coords = [1, 1]
                elem%nodes(3)%shape_func%f => shape_N3_linear
                elem%nodes(3)%shape_func_deriv(1)%f => shape_deriv_N31_linear
                elem%nodes(3)%shape_func_deriv(2)%f => shape_deriv_N32_linear
                elem%nodes(4)%natural_coords = [-1, 1]
                elem%nodes(4)%shape_func%f => shape_N4_linear
                elem%nodes(4)%shape_func_deriv(1)%f => shape_deriv_N41_linear
                elem%nodes(4)%shape_func_deriv(2)%f => shape_deriv_N42_linear
            end select
        end function construct_linear_element

        function compute_N_linear(self, natural_coords) result(N)
            ! Compute the shape function matrix N for a 2D element
            class(LinearElement_t), intent(in) :: self
            real(r64), intent(in) :: natural_coords(:)
            real(r64), allocatable :: N(:, :)

            ! Loc vars
            real(r64), allocatable :: N_(:)

            ! Assign to correct position in shape function matrix
            allocate(N(self%ndim, self%nnodes*self%ndof))
            select case (self%ndim)
            case (2)
                ! Compute the components of N
                N_ = [ &
                    self%nodes(1)%shape_func%f(natural_coords), &
                    self%nodes(2)%shape_func%f(natural_coords), &
                    self%nodes(3)%shape_func%f(natural_coords), &
                    self%nodes(4)%shape_func%f(natural_coords) &
                ]

                ! Assign to shape function matrix
                N(1, :) = [N_(1), 0.0_r64, N_(2), 0.0_r64, N_(3), 0.0_r64, N_(4), 0.0_r64]
                N(2, :) = [0.0_r64, N_(1), 0.0_r64, N_(2), 0.0_r64, N_(3), 0.0_r64, N_(4)]
            end select
        end function compute_N_linear

        function compute_dN_linear(self, natural_coords) result(dN)
            ! Compute the shape function derivative matrix dN for a 2D element
            class(LinearElement_t), intent(in) :: self
            real(r64), intent(in) :: natural_coords(:)
            real(r64), allocatable :: dN(:, :)

            ! Loc vars
            real(r64), allocatable :: dN1_(:), dN2_(:)

            ! Assign to correct position in shape function matrix
            allocate(dN(self%ndim*self%ndof, self%nnodes*self%ndof))
            select case (self%ndim)
            case (2)
                ! Compute the components of N
                dN1_ = [ &
                    self%nodes(1)%shape_func_deriv(1)%f(natural_coords), &
                    self%nodes(2)%shape_func_deriv(1)%f(natural_coords), &
                    self%nodes(3)%shape_func_deriv(1)%f(natural_coords), &
                    self%nodes(4)%shape_func_deriv(1)%f(natural_coords) &
                ]
                dN2_ = [ &
                    self%nodes(1)%shape_func_deriv(2)%f(natural_coords), &
                    self%nodes(2)%shape_func_deriv(2)%f(natural_coords), &
                    self%nodes(3)%shape_func_deriv(2)%f(natural_coords), &
                    self%nodes(4)%shape_func_deriv(2)%f(natural_coords) &
                ]

                ! Assign to shape function derivative matrix
                dN(1, :) = [dN1_(1), 0.0_r64, dN1_(2), 0.0_r64, dN1_(3), 0.0_r64, dN1_(4), 0.0_r64]
                dN(2, :) = [dN2_(1), 0.0_r64, dN2_(2), 0.0_r64, dN2_(3), 0.0_r64, dN2_(4), 0.0_r64]
                dN(3, :) = [0.0_r64, dN1_(1), 0.0_r64, dN1_(2), 0.0_r64, dN1_(3), 0.0_r64, dN1_(4)]
                dN(4, :) = [0.0_r64, dN2_(1), 0.0_r64, dN2_(2), 0.0_r64, dN2_(3), 0.0_r64, dN2_(4)]
            end select
        end function compute_dN_linear

        function compute_J_linear(self, dN) result(J)
            ! Compute the "full" Jacobian matrix J for a 2D element
            class(LinearElement_t), intent(in) :: self
            real(r64), intent(in) :: dN(:, :)
            real(r64), allocatable :: J(:, :)

            ! Loc vars
            real(r64), allocatable :: elem_coords(:), J_col(:), J_mat(:, :)

            ! Get element coordinates as a vector and compute the Jacobian in column form
            elem_coords = self%get_nodal_coordinate_vec()
            J_col = matmul(dN, elem_coords)

            ! Convert to matrix format
            allocate(J_mat(self%ndim, self%ndim))
            select case (self%ndim)
            case (2)
                J_mat(1, :) = [J_col(1), J_col(3)]
                J_mat(2, :) = [J_col(2), J_col(4)]
            end select

            ! Convert to "full" format (zero matrix in quadrants 2 and 3)
            allocate(J(2*self%ndim, 2*self%ndim))
            J = 0
            J(1:self%ndim, 1:self%ndim) = J_mat
            J(self%ndim+1:2*self%ndim, self%ndim+1:2*self%ndim) = J_mat
        end function compute_J_linear

        function compute_B_linear(self, dN, J) result(B)
            ! Compute the "full" Jacobian matrix J for a 2D element
            class(LinearElement_t), intent(in) :: self
            real(r64), intent(in) :: dN(:, :), J(:, :)
            real(r64), allocatable :: B(:, :)

            ! Loc vars
            ! real(r64), parameter
        end function compute_B_linear

        subroutine inspect_element(self)
            ! Print a summary of information about the finite element
            ! E.g., nodal coordinates
            class(FiniteElement_t), intent(in) :: self

            integer :: i

            print *, 'node', ' glob_1', ' glob_2', ' glob_3', ' elem_1', ' elem_2', ' elem_3', ' nat_1', ' nat_2', ' nat_3'
            do i = 1, size(self%nodes)
                print *, self%nodes(i)%number, self%nodes(i)%global_coords, self%nodes(i)%element_coords, self%nodes(i)%natural_coords
            end do            
        end subroutine inspect_element

        function get_nodal_coordinate_vec(self, loc) result(coords)
            ! Get the (nnodes*ndim, 1) vector of nodal coordinates for the element
            class(FiniteElement_t), intent(in) :: self
            character(*), intent(in), optional :: loc

            ! Loc vars
            character(CHAR_SIZE) :: loc_
            integer :: i
            real(r64), allocatable :: coords_(:, :), coords(:)

            ! Override default global coordinates if provided
            loc_ = 'global'
            if (present(loc)) then
                loc_ = loc
            end if

            ! Allocate temp 2D matrix for storing coordinates, 1 row per node
            allocate(coords_(size(self%nodes), size(self%nodes(1)%element_coords)))

            ! Get coordinates
            select case (trim(loc_))
            case ('element')
                do i = 1, size(self%nodes)
                    coords_(i, :) = self%nodes(i)%element_coords
                end do
            case ('natural')
                do i = 1, size(self%nodes)
                    coords_(i, :) = self%nodes(i)%natural_coords
                end do
            case default
                do i = 1, size(self%nodes)
                    coords_(i, :) = self%nodes(i)%global_coords
                end do
            end select

            ! Reshape 2D matrix to 1D vector, where x,y pairs of nodal coordinates are indexed consecutively
            coords = reshape(transpose(coords_), shape=[size(coords_)])
        end function get_nodal_coordinate_vec

        function shape_N1_linear(natural_coords) result(N)
            ! Shape function for node 1 of a linear element
            real(r64), intent(in) :: natural_coords(:)
            real(r64) :: N
            
            select case (size(natural_coords))
            case (2)
                N = 0.25*(natural_coords(1) - 1)*(natural_coords(2) - 1)
            end select
        end function shape_N1_linear

        function shape_N2_linear(natural_coords) result(N)
            ! Shape function for node 2 of a linear element
            real(r64), intent(in) :: natural_coords(:)
            real(r64) :: N
            
            select case (size(natural_coords))
            case (2)
                N = -0.25*(natural_coords(1) + 1)*(natural_coords(2) - 1)
            end select
        end function shape_N2_linear

        function shape_N3_linear(natural_coords) result(N)
            ! Shape function for node 3 of a linear element
            real(r64), intent(in) :: natural_coords(:)
            real(r64) :: N
            
            select case (size(natural_coords))
            case (2)
                N = 0.25*(natural_coords(1) + 1)*(natural_coords(2) + 1)
            end select
        end function shape_N3_linear

        function shape_N4_linear(natural_coords) result(N)
            ! Shape function for node 4 of a linear element
            real(r64), intent(in) :: natural_coords(:)
            real(r64) :: N
            
            select case (size(natural_coords))
            case (2)
                N = -0.25*(natural_coords(1) - 1)*(natural_coords(2) + 1)
            end select
        end function shape_N4_linear

        function shape_deriv_N11_linear(natural_coords) result(N)
            ! Shape function derivative for node 1 with respect to eta1 of a linear element
            real(r64), intent(in) :: natural_coords(:)
            real(r64) :: N
            
            select case (size(natural_coords))
            case (2)
                N = 0.25*(natural_coords(2) - 1)
            end select
        end function shape_deriv_N11_linear

        function shape_deriv_N12_linear(natural_coords) result(N)
            ! Shape function derivative for node 1 with respect to eta1 of a linear element
            real(r64), intent(in) :: natural_coords(:)
            real(r64) :: N
            
            select case (size(natural_coords))
            case (2)
                N = 0.25*(natural_coords(1) - 1)
            end select
        end function shape_deriv_N12_linear

        function shape_deriv_N21_linear(natural_coords) result(N)
            ! Shape function derivative for node 1 with respect to eta1 of a linear element
            real(r64), intent(in) :: natural_coords(:)
            real(r64) :: N
            
            select case (size(natural_coords))
            case (2)
                N = -0.25*(natural_coords(2) - 1)
            end select
        end function shape_deriv_N21_linear

        function shape_deriv_N22_linear(natural_coords) result(N)
            ! Shape function derivative for node 1 with respect to eta1 of a linear element
            real(r64), intent(in) :: natural_coords(:)
            real(r64) :: N
            
            select case (size(natural_coords))
            case (2)
                N = -0.25*(natural_coords(1) + 1)
            end select
        end function shape_deriv_N22_linear

        function shape_deriv_N31_linear(natural_coords) result(N)
            ! Shape function derivative for node 1 with respect to eta1 of a linear element
            real(r64), intent(in) :: natural_coords(:)
            real(r64) :: N
            
            select case (size(natural_coords))
            case (2)
                N = 0.25*(natural_coords(2) + 1)
            end select
        end function shape_deriv_N31_linear

        function shape_deriv_N32_linear(natural_coords) result(N)
            ! Shape function derivative for node 1 with respect to eta1 of a linear element
            real(r64), intent(in) :: natural_coords(:)
            real(r64) :: N
            
            select case (size(natural_coords))
            case (2)
                N = 0.25*(natural_coords(1) + 1)
            end select
        end function shape_deriv_N32_linear

        function shape_deriv_N41_linear(natural_coords) result(N)
            ! Shape function derivative for node 1 with respect to eta1 of a linear element
            real(r64), intent(in) :: natural_coords(:)
            real(r64) :: N
            
            select case (size(natural_coords))
            case (2)
                N = -0.25*(natural_coords(2) + 1)
            end select
        end function shape_deriv_N41_linear

        function shape_deriv_N42_linear(natural_coords) result(N)
            ! Shape function derivative for node 1 with respect to eta1 of a linear element
            real(r64), intent(in) :: natural_coords(:)
            real(r64) :: N
            
            select case (size(natural_coords))
            case (2)
                N = -0.25*(natural_coords(1) - 1)
            end select
        end function shape_deriv_N42_linear
end module element_library