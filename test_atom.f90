program test_atom
  
    use atom_type
    implicit none

    type(atom) :: a
    real ,dimension(3) :: data

    call random_number(data)
        
    call a%init_atom(' H', data)

    print *, a

  end program test_atom
  
