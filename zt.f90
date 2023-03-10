program zt

    
    use vdw_type
    implicit none

    type(atomvdw) :: atom

    type(atomvdw), dimension(52):: vdw_array

    character(len=2):: na
    integer :: i
    real :: rad, radinser

    na = "H "
    rad = 2.5

    call atom%init_vdw(na, rad)

    print '(f4.2)',atom%radius
    
    call atom%read_vdw_file(vdw_array)

    print '(f4.2)', vdw_array(1)%radius

    call atom%get_atom_radius("H ", vdw_array, radinser)


    print '(f4.2)', radinser

end program zt