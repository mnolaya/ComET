program main

    use iso_fortran_env, only: r64 => real64
    use element_library, only: LinearElement_t

    implicit none

    real(r64) :: coords(4, 2)
    type(LinearElement_t) :: elem
    
    coords(1, :) = [-2.0_r64, 0.0_r64]
    coords(2, :) = [1.0_r64, 1.0_r64]
    coords(3, :) = [2.0_r64, 1.5_r64]
    coords(4, :) = [-1.0_r64, 1.0_r64]
    
    print *, 'check'
    elem = LinearElement_t(coords)

end program main