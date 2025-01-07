program main

    use iso_fortran_env, only: r64 => real64
    use element_library, only: LinearElement_t
    use constants

    implicit none

    integer :: i
    real(r64) :: coords(4, 2)
    type(LinearElement_t) :: elem
    
    coords(1, :) = [-2.0_r64, 0.0_r64]
    coords(2, :) = [1.0_r64, 1.0_r64]
    coords(3, :) = [2.0_r64, 1.5_r64]
    coords(4, :) = [-1.0_r64, 1.0_r64]

    elem = LinearElement_t(coords)

    call elem%inspect()
    print *, elem%nodes(1)%shape_func([1.0_r64, 1.0_r64])
    print *, elem%nodes(2)%shape_func([1.0_r64, 1.0_r64])
    print *, elem%nodes(3)%shape_func([1.0_r64, 1.0_r64])
    print *, elem%nodes(4)%shape_func([1.0_r64, 1.0_r64])
    print *, elem%compute_N([1.0_r64, 1.0_r64])
end program main