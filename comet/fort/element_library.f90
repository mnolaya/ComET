module element_library

    use iso_fortran_env, only: r64 => real64
    use constants, only: CHAR_SIZE

    implicit none
    private

    type :: Node_t
        ! Type for a node on a finite element
        integer :: number
        real(r64), allocatable :: natural_coords(:)
        real(r64), allocatable :: element_coords(:)
        real(r64), allocatable :: global_coords(:)
        procedure(shape_func), pointer, nopass :: shape_func => null()
    end type Node_t

    type, abstract :: FiniteElement_t
        ! Base class for a finite element
        integer :: number, ndim, ndof, nnodes
        character(CHAR_SIZE) :: material_name
        type(Node_t), allocatable :: nodes(:)
        real(r64), allocatable :: N(:, :)
        contains
            procedure, pass :: inspect => inspect_element
            procedure(compute_N), deferred, pass :: compute_N
    end type FiniteElement_t

    type, extends(FiniteElement_t), public :: LinearElement_t
        ! Linear finite element type
        contains
            procedure, pass :: compute_N => compute_N_linear
    end type LinearElement_t

    abstract interface
        function shape_func(natural_coords) result(N)
            ! Generic interface for a shape function computation
            import r64
            real(r64), intent(in) :: natural_coords(:)
            real(r64) :: N
        end function shape_func

        function compute_N(self, natural_coords) result(N)
            ! Deferred interface for computing the element shape function derivative matrix N
            import r64, FiniteElement_t
            class(FiniteElement_t), intent(in) :: self
            real(r64), intent(in) :: natural_coords(:)
            real(r64), allocatable :: N(:, :)
        end function compute_N
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
                elem%nodes(i)%global_coords = global_coords(i, :)
                elem%nodes(i)%element_coords = global_coords(i, :) - elem%nodes(1)%global_coords
            end do
            
            ! Update the element natural coordinates, ordered CCW
            ! Update the shape function pointers
            select case (elem%ndim)
            case (2)
                elem%nodes(1)%natural_coords = [-1, -1]
                elem%nodes(1)%shape_func => shape_N1_linear
                elem%nodes(2)%natural_coords = [1, -1]
                elem%nodes(2)%shape_func => shape_N2_linear
                elem%nodes(3)%natural_coords = [1, 1]
                elem%nodes(3)%shape_func => shape_N3_linear
                elem%nodes(4)%natural_coords = [-1, 1]
                elem%nodes(4)%shape_func => shape_N4_linear
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
                N_ = [self%nodes(1)%shape_func(natural_coords), self%nodes(2)%shape_func(natural_coords), self%nodes(3)%shape_func(natural_coords), self%nodes(4)%shape_func(natural_coords)]

                ! Assign to shape function derivative matrix
                N(1, :) = [N_(1), 0.0_r64, N_(2), 0.0_r64, N_(3), 0.0_r64, N_(4), 0.0_r64]
                N(2, :) = [0.0_r64, N_(1), 0.0_r64, N_(2), 0.0_r64, N_(3), 0.0_r64, N_(4)]
            end select
        end function compute_N_linear

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
end module element_library