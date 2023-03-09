program LectureVdW
    implicit none
    
    ! declarations 
    integer :: ok, i
    character(80) :: line
    character(10) :: buffer
    character(len=128) :: fileName
    character(2) atom_name(53)
    real atom_size(53)

    ! Verify args
    if(iargc() /= 1) then
        print '(a)', "Please provide a WFN file"
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
    
    ! Skip first 6 lines
    do i=1, 6
        read(10,'(a80)') line
        print '(a)',line
    enddo

    ! Read the content
    do i=1, 256
        read(10, '(A8)', iostat=ok) buffer
        if(ok/=0) then
            exit
        endif
        atom_name(i)=buffer(1:2)
        read(buffer(3:6), '(f4.2)') atom_size(i) 
    enddo

    do i=1, 53
        print '(a,x,f4.2)',  atom_name(i),atom_size(i)
    enddo

end program LectureVdW
