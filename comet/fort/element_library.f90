module element_library

    use iso_fortran_env, only: r64 => real64
    use constants, only: CHAR_SIZE

    implicit none
    private
    public :: LinearElement_t

    type :: ShapeFunction_t
        ! Type containing pointer to a shape function for assembling into an array
        procedure(shape_func), pointer, nopass :: f => null()
    end type ShapeFunction_t

    type :: Node_t
        ! Type for a node on a finite element
        integer :: number
        real(r64), allocatable :: natural_coords(:)
        real(r64), allocatable :: element_coords(:)
        real(r64), allocatable :: global_coords(:)
    end type Node_t

    type, abstract :: FiniteElement_t
        ! Base class for a finite element
        integer :: number
        character(CHAR_SIZE) :: material_name
        ! type(ShapeFunction_t), allocatable :: shape_funcs(:)
        type(Node_t), allocatable :: nodes(:)
        real(r64), allocatable :: N(:, :)
    end type FiniteElement_t

    type, extends(FiniteElement_t), public :: LinearElement_t
        ! Linear finite element type
    end type LinearElement_t

    abstract interface
        function shape_func(natural_coords) result(N)
            ! Generic interface for a shape function computation
            import r64
            real(r64), intent(in) :: natural_coords(:)
            real(r64) :: N
        end function shape_func
    end interface

    interface LinearElement_t
        module procedure construct_linear_element
    end interface LinearElement_t

    contains
        function construct_linear_element(global_coords) result(elem)
            ! Construct a linear finite element
            real(r64), intent(in) :: global_coords(:, :)
            type(LinearElement_t) :: elem

            integer :: i, j, nnodes, ndof, ndim
            real(r64) :: N

            nnodes = size(global_coords, dim=1)
            ndim = size(global_coords, dim=2)
            ndof = ndim

            ! Allocate the nodes and shape functions for the element
            ! allocate(elem%shape_funcs(nnodes))
            allocate(elem%nodes(nnodes))
            allocate(elem%N(ndim, ndim*nnodes))
            
            ! Set the element global coordinates
            do i = 1, nnodes
                elem%nodes(i)%number = i
                allocate(elem%nodes(i)%element_coords(ndim))
                allocate(elem%nodes(i)%global_coords(ndim))
                allocate(elem%nodes(i)%natural_coords(ndim))
                elem%nodes(i)%global_coords = global_coords(i, :)
                elem%nodes(i)%element_coords = global_coords(i, :) - elem%nodes(1)%global_coords
            end do
            
            ! Update the element natural coordinates, ordered CCW
            ! Update the shape function pointers
            select case (nnodes)
            case (4)
                elem%nodes(1)%natural_coords = [-1, -1]
                ! elem%shape_funcs(1)%f => shape_N1_linear
                elem%nodes(2)%natural_coords = [1, -1]
                ! elem%shape_funcs(2)%f => shape_N1_linear
                elem%nodes(3)%natural_coords = [1, 1]
                ! elem%shape_funcs(3)%f => shape_N1_linear
                elem%nodes(4)%natural_coords = [-1, 1]
                ! elem%shape_funcs(4)%f => shape_N1_linear
            end select

            ! Compute the shape function matrix N
            ! do i = 1, nnodes
            !     ! Compute shape function N for node i
                ! N = elem%shape_funcs(i)%f(elem%nodes(i)%natural_coords)

                ! Assign to correct position in shape function matrix
                ! select case (ndim)
                ! case (2)
                !     elem%N(1, :) = [ &
                !         elem%shape_funcs(1)%f(elem%nodes(1)%natural_coords), &
                !         0_r64, &
                !         elem%shape_funcs(2)%f(elem%nodes(2)%natural_coords), &
                !         0_r64, &
                !         elem%shape_funcs(3)%f(elem%nodes(3)%natural_coords), &
                !         0_r64, &
                !         elem%shape_funcs(4)%f(elem%nodes(4)%natural_coords), &
                !         0_r64, &
                !     ]
                ! end select
            ! end do
        end function construct_linear_element

        ! function shape_N1_linear(natural_coords) result(N)
        !     ! Shape function for node 1 of a linear element
        !     real(r64), intent(in) :: natural_coords(:)
        !     real(r64) :: N

        !     select case (size(natural_coords))
        !     case (2)
        !         N = 0.25*(natural_coords(1) - 1)*(natural_coords(2) - 1)
        !     end select
        ! end function shape_N1_linear

        ! function shape_N2_linear(natural_coords) result(N)
        !     ! Shape function for node 2 of a linear element
        !     real(r64), intent(in) :: natural_coords(:)
        !     real(r64) :: N

        !     select case (size(natural_coords))
        !     case (2)
        !         N = 0.25*(natural_coords(1) - 1)*(natural_coords(2) - 1)
        !     end select
        ! end function shape_N2_linear

        ! function shape_N3_linear(natural_coords) result(N)
        !     ! Shape function for node 3 of a linear element
        !     real(r64), intent(in) :: natural_coords(:)
        !     real(r64) :: N

        !     select case (size(natural_coords))
        !     case (2)
        !         N = 0.25*(natural_coords(1) - 1)*(natural_coords(2) - 1)
        !     end select
        ! end function shape_N3_linear

        ! function shape_N4_linear(natural_coords) result(N)
        !     ! Shape function for node 4 of a linear element
        !     real(r64), intent(in) :: natural_coords(:)
        !     real(r64) :: N

        !     select case (size(natural_coords))
        !     case (2)
        !         N = 0.25*(natural_coords(1) - 1)*(natural_coords(2) - 1)
        !     end select
        ! end function shape_N4_linear
end module element_library