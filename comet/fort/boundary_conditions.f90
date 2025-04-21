module boundary_conditions

    use, intrinsic :: iso_fortran_env, only: r64 => real64
    use gauss_integration, only: make_integration_points, IntegrationPoint_t

    implicit none

    type, public :: PolynomialConstants_t
        real(r64), allocatable :: c(:)
    end type PolynomialConstants_t

    type, public :: SurfaceTraction_t
        integer :: order, i_const, val_const
        integer :: J1_indices(2), J2_indices(2)
        character(2) :: face
            ! Face surface traction is on with respect to isoparametric element
            ! Identify with +/- and x/y/z -> e.g., +x
        real(r64) :: thickness
        type(PolynomialConstants_t), allocatable :: poly_constants(:, :)
            ! Polynomial constants defining the surface load, ordered from lowest to highest order
            ! p0*x^0 + p1*x^1 + ... pn*x^n
        contains
            procedure, pass :: compute_J_det
    end type SurfaceTraction_t

    interface SurfaceTraction_t
        module procedure init_surface_traction
    end interface SurfaceTraction_t

    contains
        function init_surface_traction(order, face, poly_constants) result(surf_traction)
            ! Args
            character(2), intent(in) :: face
            integer, intent(in) :: order
            type(PolynomialConstants_t), allocatable :: poly_constants(:, :)
            type(SurfaceTraction_t) :: surf_traction

            ! Surface traction polynomial constants should be arranged where each row corresponds to a dof, 
            ! and each column corresponds to the polynomial constants associated with components of the natural coordinate vector
            surf_traction%poly_constants = poly_constants

            ! Depending on the face, set the index of the integration point coorindates 
            ! that are are held constant during numerical integration of the surface load (along the +/-1 faces)
            ! and the indices of the Jacobian that must be retrieved to compute the surface's Jacobi-determinant
            surf_traction%face = face
            surf_traction%order = order
            select case(surf_traction%face)
                case ('+x')
                    surf_traction%i_const = 1
                    surf_traction%val_const = 1
                    surf_traction%J1_indices = [2, 1]
                    surf_traction%J2_indices = [2, 2]
                case ('-x')
                    surf_traction%i_const = 1
                    surf_traction%val_const = -1
                    surf_traction%J1_indices = [2, 1]
                    surf_traction%J2_indices = [2, 2]
                case ('+y')
                    surf_traction%i_const = 2
                    surf_traction%val_const = 1
                    surf_traction%J1_indices = [1, 1]
                    surf_traction%J2_indices = [1, 2]
                case ('-y')
                    surf_traction%i_const = 2
                    surf_traction%val_const = -1
                    surf_traction%J1_indices = [1, 1]
                    surf_traction%J2_indices = [1, 2]
            end select
        end function init_surface_traction

        !> Compute the Jacboi-determinant for the applied surface traction
        function compute_J_det(self, J) result(J_det)
            ! Args
            class(SurfaceTraction_t), intent(in) :: self
            real(r64), intent(in) :: J(:, :)
            real(r64) :: J_det

            ! Loc vars
            real(r64) :: J1, J2

            ! Get J1 and J2 terms from the Jacobian and compute
            J1 = J(self%J1_indices(1), self%J1_indices(2))
            J2 = J(self%J2_indices(1), self%J2_indices(2))
            J_det = (J1**2 + J2**2)**0.5
        end function compute_J_det

        ! !> Get the natural coordinates of the integration points corresponding to the surface traction
        ! function get_natural_coords(self, itg_pt, ndim) result(natural_coords)
        !     ! Args
        !     class(SurfaceTraction_t), intent(in) :: self
        !     type(IntegrationPoint_t), intent(in) :: itg_pt
        !     integer, intent(in) :: ndim
        !     real(r64) :: natural_coords(ndim)

        !     ! Get the natural coordinates of the integration point corresponding to the surface traction
        !     natural_coords = itg_pt%loc
        !     natural_coords(self%surf_traction%i_const) = self%surf_traction%val_const

        ! end function get_natural_coords
end module boundary_conditions