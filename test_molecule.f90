program test_molecule
  
    use molecule_type
    implicit none

    type(atom) :: a
    real ,dimension(3) :: data
    type(molecule) :: mol

    integer, parameter :: NB_MAX_ATOMS=128
    integer :: i,index

    character, dimension(2,4) :: elements
    character(len=2) :: elt

    elements(1,:)=' '
    elements(2,1)='H'
    elements(2,2)='C'
    elements(2,3)='O'
    elements(2,4)='N'
    
    call mol%init_mol(NB_MAX_ATOMS)

    do i=1,NB_MAX_ATOMS
       
        index=modulo(i,size(elements,2))+1

        ! // <=> concatenate
        elt = elements(1,index)//elements(2,index)
       
        call random_number(data)

        call a%init_atom(elt,data)
        
        call mol%add_atom(a)
    
    end do
    
    print *, mol
    
end program test_molecule
  
