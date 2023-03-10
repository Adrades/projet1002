program test_molecule
  
    use molecule_type
    implicit none

    type(molecule) :: mol

    integer, parameter :: NB_MAX_ATOMS=128
    integer :: ok

    character(len=100) :: fileName

    ! Verify args
    if(iargc() /= 1) then
        print '(a)', "Please provide a xyz file"
        stop 10
    end if

    ! Open File provided
    call getarg(1,fileName)
    print '(/,a,a)', "File to read = ",trim(fileName)
    open(unit=10,file=fileName,iostat=ok,status='old')
    if(ok/=0) then
     print '(a,4x,a)', "Error during opening", fileName
     stop 20
    end if

    call mol%read_mol(fileName)

    call mol%write_mol("ligand2.xyz")
end program test_molecule
  
