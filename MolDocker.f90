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
    type(molecule) :: mol
    type(molecule), dimension(N_CHILD) :: arr_ligand    
    ! TODO read VdW

    ! TODO read both xyz

    ! read VdW => tableau avec radius atomes
    

    ! read xyz ligand 
    ! read xyz molecule
    call mol%init_mol(NB_MAX_ATOMS)
    call ligand%init_mol(NB_MAX_ATOMS)

    ! FIXME try validating
    !init best volume as starting point
    call mol%box(ligand, best_volume)

    ! gen ligand (rotate) n fois
    ! #OMP on attribue les ligands, 1 par process (ou autrement ?)
    do e=1,EPOCH
        arr_ligand=genligands(ligand, N_CHILD) 
        do i=1,N_CHILD
            ! Calcul du volume occupé par le ligand et la molécule
            call mol%box(arr_ligand(i), volume)
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

contains
function genligands(ligan, n) result(arr_ligand)
    use molecule_type
    integer, intent(in) :: n !input
    type(molecule), intent (in) :: ligan ! input

    type(molecule), dimension(n) :: arr_ligand ! output

end function 


end program MolDocker
