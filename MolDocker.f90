program MolDocker
    use atom_type
    use molecule_type
    
    implicit none

    ! character(2) atom_name(53)
    ! real atom_size(53)
    integer, parameter :: NB_MAX_ATOMS=128
    integer, parameter :: N_CHILD=50
    integer, parameter :: EPOCH=5

    integer :: e, i, j, k

    ! logical, dimension(N_CHILD) :: arr_valid 
    real, dimension(N_CHILD) :: arr_res 
    real :: volume, best_volume

    type(molecule) :: ligand, best_ligand
    type(molecule) :: site
    type(molecule), dimension(N_CHILD) :: arr_ligand    

    character(len=100) :: ligand_path
    character(len=100) :: site_path
    character(len=100) :: vdw_path

    if(iargc() < 1) then
        print '(a)', "Please provide a xyz file for the site" 
        stop 10
    end if
    call getarg(1,site_path)

    if(iargc() < 2) then
        print '(a)', "Please provide a xyz file for the ligand"
        stop 20
    end if
    call getarg(2, ligand_path)

    if(iargc() < 3) then
        print '(a)', "Please provide a Van Der Walls file"
        stop 30
    end if
    call getarg(3, vdw_path)

    ! TODO read VdW => tableau avec radius atomes


    ! TODO read both xyz
    call site%read_mol(site_path)
    call ligand%read_mol(ligand_path)

    ! FIXME try validating
    !init best volume as starting point
    call site%box(ligand, best_volume)

    ! gen ligand (rotate) n fois
    ! #OMP on attribue les ligands, 1 par process (ou autrement ?)
    do e=1,EPOCH
        arr_ligand=genligands(ligand, N_CHILD) 
        do i=1,N_CHILD
            ! Calcul du volume occupé par le ligand et la molécule
            call site%box(arr_ligand(i), volume)
            if (best_volume > volume) then
                ! Enregistrement du meilleur
                best_volume = volume
                best_ligand = arr_ligand(i)
            endif
        enddo
    enddo

    ! for ligands
        ! for nbgenerations
            ! tq pas dans la mol
                ! placer ligand aléatoirement autour de la molécule (translate)
            ! si resultat meilleur (volume molecule / ligand < )
                ! nouveau best du ligand en cours = vecteur translate utilisé
        ! if best ligand en cours > best ligand all
            ! nouveau best ligand all = rotation + translation
    ! #OMP max(bestdesjobs)
    ! output ligandtranslatérotaté + molécule
    call ligand%write_mol("result.xyz")

contains
function genligands(ligan, n) result(arr_ligand)
    use molecule_type
    integer, intent(in) :: n !input
    type(molecule), intent (in) :: ligan ! input

    type(molecule), dimension(n) :: arr_ligand ! output

end function 


end program MolDocker
