module gauss_integration

    use iso_fortran_env, only: r64 => real64

    implicit none
    private
    public :: make_integration_points

    type, public :: IntegrationPoint_t
        real(r64) :: weight, loc
    end type IntegrationPoint_t

    ! type, public :: IntegrationPointCollection

    ! end type IntegrationPointCollection

    contains
        function make_integration_points(num) result(itg_pts)
            ! Get num Gaussian integration points set with their locations and weights
            integer, intent(in) :: num
            type(IntegrationPoint), allocatable :: itg_pts(:)

            ! Set the appropriate integration point locations and weights
            allocate(itg_pts(num))
            select case (num)
            case (1)
                itg_pts(1)%loc = 0
                itg_pts(1)%weight = 2
            case (2)
                itg_pts(1)%loc = -0.57735_r64
                itg_pts(1)%weight = 1

                itg_pts(2)%loc = 0.57735_r64
                itg_pts(2)%weight = 1
            case (3)
                itg_pts(1)%loc = -0.774597_r64
                itg_pts(1)%weight = 0.555556_r64

                itg_pts(2)%loc = 0
                itg_pts(2)%weight = 0.888889_r64

                itg_pts(3)%loc = 0.774597_r64
                itg_pts(3)%weight = 0.555556_r64
            case (4)
                itg_pts(1)%loc = -0.861136_r64
                itg_pts(1)%weight = 0.347855_r64

                itg_pts(2)%loc = -0.339981_r64
                itg_pts(2)%weight = 0.652145_r64

                itg_pts(3)%loc = 0.339981_r64
                itg_pts(3)%weight = 0.652145_r64

                itg_pts(4)%loc = 0.861136_r64
                itg_pts(4)%weight = 0.347855_r64
            end select
        end function make_integration_points
end module gauss_integration