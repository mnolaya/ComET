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
    print *, elem%shape_funcs(1)%f(elem%nodes(1)%natural_coords)
    print *, elem%shape_funcs(2)%f(elem%nodes(2)%natural_coords)
    print *, elem%shape_funcs(3)%f(elem%nodes(3)%natural_coords)
    print *, elem%shape_funcs(4)%f(elem%nodes(4)%natural_coords)
end program main