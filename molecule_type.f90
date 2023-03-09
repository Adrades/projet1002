module molecule_type

use atom_type
    implicit none

    type molecule
    type(atom), dimension(:), allocatable, private :: atoms
    integer , private :: nb_atoms
contains
    procedure :: init_mol, add_atom, box, dist, test_valid, print_mol

    ! allow calls like print *
    generic :: write(formatted) => print_mol

    end type molecule

contains
    subroutine init_mol(m, size)
        class (molecule),intent(inout) :: m
        integer, intent(in) :: size

        allocate(m%atoms(size))

        m%nb_atoms=0
    end subroutine init_mol

    subroutine add_atom(m, a)
        class(molecule),intent(inout) :: m
        type(atom), intent(in) ::a 

        m%nb_atoms = m%nb_atoms+1
        m%atoms(m%nb_atoms)=a

    end subroutine add_atom

    subroutine box(m, ligand, volume)
        class(molecule), intent(inout) :: m
        type(molecule), intent(in) :: ligand
        real, intent(out) :: volume

        type(atom) :: a, a1
        integer :: i
        real :: max_x, min_x, max_y, min_y, max_z, min_z
        real :: x_side, y_side, z_side
        a1 = m%atoms(1)

        max_x = a1%coordinates(1)
        min_x = a1%coordinates(1)
        max_y = a1%coordinates(2)
        min_y = a1%coordinates(2)
        max_z = a1%coordinates(3)
        min_z = a1%coordinates(3)

        do i=2, m%nb_atoms
            a = m%atoms(i)
            if (max_x < a%coordinates(1)) then
                max_x = a%coordinates(1)
            end if
            if (min_x > a%coordinates(1)) then
                min_x = a%coordinates(1)
            end if
            if (max_y < a%coordinates(2)) then
                max_y = a%coordinates(2)
            end if
            if (min_y > a%coordinates(2)) then
                min_y = a%coordinates(2)
            end if
            if (max_z < a%coordinates(3)) then
                max_z = a%coordinates(3)
            end if
            if (min_z > a%coordinates(3)) then
                min_z = a%coordinates(3)
            end if
        end do

        do i=2, ligand%nb_atoms
            a = ligand%atoms(i)
            if (max_x < a%coordinates(1)) then
                max_x = a%coordinates(1)
            end if
            if (min_x > a%coordinates(1)) then
                min_x = a%coordinates(1)
            end if
            if (max_y < a%coordinates(2)) then
                max_y = a%coordinates(2)
            end if
            if (min_y > a%coordinates(2)) then
                min_y = a%coordinates(2)
            end if
            if (max_z < a%coordinates(3)) then
                max_z = a%coordinates(3)
            end if
            if (min_z > a%coordinates(3)) then
                min_z = a%coordinates(3)
            end if
        end do

        x_side = max_x - min_x
        y_side = max_y - min_y
        z_side = max_z - min_z

        volume = x_side * y_side * z_side
    end subroutine box

    subroutine dist(m, ligand, d)
        class(molecule), intent(inout) :: m

        type(molecule), intent(in) :: ligand
        real, intent(out) :: d

    end subroutine dist

    subroutine test_valid(m, ligand, bool)
        class(molecule), intent(inout) :: m

        type(molecule), intent(in) :: ligand
        logical, intent(out) :: bool

        ! TODO
    end subroutine test_valid

    subroutine print_mol(m, unit, iotype, v_list, iostat, iomsg)
        class (molecule),intent(in) :: m

        integer, intent(in) :: unit
        character(len=*), intent(in) :: iotype
        integer, dimension(:), intent(in):: v_list
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg

        integer :: i

        print *, m%atoms

        ! Everything ok here <=> need by ifort not gfortran
        iostat=0

    end subroutine print_mol

end module molecule_type


