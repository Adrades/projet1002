program EcritureLigand

    use molecule_type
    use atom_type

    implicit none

    character(len=100):: fileName
    integer :: i, ok
    type(molecule) :: ligand
    type(atom) :: a
    real :: x,y,z

    ! Verify args
    if(iargc() /= 1) then
        print '(a)', "Please provide a place to write output file"
        stop 10
    end if

    ! Open File provided
    call getarg(3,fileName)
    print '(/,a,a)', "File to write = ",trim(fileName)
    open(unit=10,file=fileName,iostat=ok,status='old')
    if(ok/=0) then
     print '(a,4x,a)', "Error during opening", fileName
     stop 20
    end if

    ! écrire le nombre d'atomes du ligand
    write(10, '(i10)', iostat=ok) ligand%nb_atoms

    ! Ligand fourni en entrée ?
    write(10, '(a)', iostat=ok) "ligand"

    ! Read the content
    do i=1, ligand%nb_atoms
        a = ligand%atoms(i)
        write(10, '(a3,3i8/)', iostat=ok) a%element, a%coordinates(1), a%coordinates(2), a%coordinates(3)
    enddo
end program
