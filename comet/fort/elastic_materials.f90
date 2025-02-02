module elastic_materials

    use iso_fortran_env, only: r64 => real64
    use constants, only: CHAR_SIZE

    implicit none

    private
    public :: compute_D_2D_isotropic

    contains
        function compute_D_2D_isotropic(E, nu, plane) result(D)
            ! Compute the material stiffness matrix D for an isotropic material in 2D
            ! Defaults to plane strain behavior
            real(r64), intent(in) :: E, nu
            character(*), intent(in), optional :: plane
            real(r64) :: D(3, 3)

            ! Loc vars
            character(CHAR_SIZE) :: plane_
            real(r64) :: G, x

            ! Default to plane strain
            plane_ = 'strain'
            if (present(plane)) plane_ = plane

            ! Initialize D
            D = 0

            ! Compute shear modulus
            G = E/(2*(1 + nu))

            ! Compute D matrix for 2D isotropic material either plane stress or plane strain behavior
            select case (trim(plane_))
            case ('stress') ! Plane stress
                x = E/(1 - nu**2)
                D(1, 1) = 1
                D(1, 2) = nu
                D(2, 1) = nu
                D(2, 2) = 1
                D(3, 3) = (1 - nu)/2
                D = x*D
            case ('strain') ! Plane strain
                x = E/((1 + nu)*(1 - 2*nu))
                D(1, 1) = 1 - nu
                D(1, 2) = nu
                D(2, 1) = nu
                D(2, 2) = 1 - nu
                D(3, 3) = (1 - 2*nu)/2
                D = x*D
            case default
                ! Add more robust error messaging...
                print *, "error: plane must be either 'strain' or 'stress' (input:", trim(plane_), ")"
            end select
        end function compute_D_2D_isotropic
end module elastic_materials