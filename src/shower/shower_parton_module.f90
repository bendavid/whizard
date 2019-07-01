!!! module: shower_parton_module
!!! This code is part of my Ph.D studies.
!!! 
!!! Copyright (C) 2011 Sebastian Schmidt <sebastian.t.schmidt@desy.de>
!!! 
!!! This program is free software; you can redistribute it and/or modify it
!!! under the terms of the GNU General Public License as published by the Free 
!!! Software Foundation; either version 3 of the License, or (at your option) 
!!! any later version.
!!! 
!!! This program is distributed in the hope that it will be useful, but WITHOUT
!!! ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
!!! FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
!!! more details.
!!! 
!!! You should have received a copy of the GNU General Public License along
!!! with this program; if not, see <http://www.gnu.org/licenses/>.
!!! 
!!! Latest Change: Thu Jan 13 17:11:14 2011 Time zone: 3600 seconds
!!! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module shower_parton_module
  
  use kinds, only: default !NODEP!
  use constants, only: pi, twopi !NODEP!
  use lorentz !NODEP!
  use shower_basics_module

  implicit none

  type :: parton_t
     integer :: nr=0         
     integer :: typ=0        
     type(vector4_t) :: momentum = vector4_null
     real(default) :: t  = 0._default
     real(default) :: scale = 0._default  
     real(default) :: z = 0._default
     real(default) :: costheta = 0._default
     real(default) :: x=0._default  ! x-value of the parton, only needed for spacelike shower
     logical :: simulated=.false.
     logical :: belongstoFSR=.true.
     logical :: belongstointeraction=.false.
     type(parton_t), pointer :: parent => null ()
     type(parton_t), pointer :: child1 => null ()
     type(parton_t), pointer :: child2 => null ()
     ! initial only needed for partons in initial showers, points to the hadron the parton is coming from
     type(parton_t), pointer :: initial => null ()
     integer :: c1 = 0, c2 = 0
     integer :: aux_pt = 0                 ! auxiliary value for pt-ordered isr
  end type parton_t

  type :: parton_pointer_t
     type(parton_t), pointer :: p => null ()
  end type parton_pointer_t

contains

  subroutine parton_copy(prt1, prt2)
    type(parton_t), intent(in) :: prt1
    type(parton_t), intent(out) :: prt2

    prt2%nr = prt1%nr
    prt2%typ = prt1%typ
    prt2%momentum = prt1%momentum
    prt2%t = prt1%t
    prt2%scale = prt1%scale
    prt2%z = prt1%z
    prt2%costheta = prt1%costheta
    prt2%x = prt1%x
    prt2%simulated = prt1%simulated
    prt2%belongstoFSR = prt1%belongstoFSR
    prt2%belongstointeraction = prt1%belongstointeraction
    if(associated(prt1%parent)) prt2%parent => prt1%parent
    if(associated(prt1%child1)) prt2%child1 => prt1%child1
    if(associated(prt1%child2)) prt2%child2 => prt1%child2
    if(associated(prt1%initial)) prt2%initial => prt1%initial
    prt2%c1 = prt1%c1
    prt2%c2 = prt1%c2
    prt2%aux_pt = prt1%aux_pt
  end subroutine parton_copy

  function parton_get_costheta(prt) result(costheta)		! returns the angle between the daughters assuming them to be massless
    type(parton_t), intent(in) :: prt
    real(default) :: costheta

    if(prt%z*(1.-prt%z)*parton_get_energy(prt)**2 .gt. 0._default) then
       costheta = 1.-prt%t/(2.*prt%z*(1.-prt%z)*parton_get_energy(prt)**2)
    else
       costheta = -1._default
    end if
  end function parton_get_costheta

  function parton_get_costheta_korrekt(prt) result(costheta) ! returns the angle between the daughters for massive daughters
    type(parton_t), intent(in) :: prt
    real(default) :: costheta

    if (parton_is_branched(prt)) then
       if (parton_is_simulated(prt%child1) .and. parton_is_simulated(prt%child2) .and. & 
            sqrt(max(0._default, prt%z*prt%z*parton_get_energy(prt)**2 - prt%child1%t)) * &
            sqrt(max(0._default, (1.-prt%z)*(1.-prt%z)*parton_get_energy(prt)**2 - prt%child2%t)) > 0._default) then
          costheta=(prt%t-prt%child1%t-prt%child2%t - 2.*prt%z*(1.-prt%z)* parton_get_energy(prt)**2)/ &
                   (-2.* sqrt(prt%z*prt%z*parton_get_energy(prt)**2 - prt%child1%t) * &
                   sqrt( (1.-prt%z)*(1.-prt%z)*parton_get_energy(prt)**2 - prt%child2%t))
       else
          costheta = parton_get_costheta(prt)
       end if
    else
       costheta = parton_get_costheta(prt)
    end if
  end function parton_get_costheta_korrekt

  function parton_get_costheta_motherfirst(prt) result(costheta)
    ! returns the angle between the momentum vectors of the parton and 1st daughter
    type(parton_t), intent(in) :: prt
    real(default) :: costheta

    if (parton_is_branched(prt)) then
       if ((parton_is_simulated(prt%child1).or.parton_is_final(prt%child1).or.parton_is_branched(prt%child1)) .and. &
           (parton_is_simulated(prt%child2).or.parton_is_final(prt%child2).or.parton_is_branched(prt%child2)) .and. &
           (space_part_norm(prt%momentum)*space_part_norm(prt%child1%momentum) > 0._default) ) then
          costheta=(space_part(prt%momentum)*space_part(prt%child1%momentum))/ &
                   (space_part_norm(prt%momentum)*space_part_norm(prt%child1%momentum))
       else
          costheta=-2._default
       end if
    else
       costheta = -2._default
    end if
  end function parton_get_costheta_motherfirst

  function get_beta(t,E) result(beta)
    real(default), intent(in) :: t,E
    real(default) :: beta

    beta=sqrt(max(0.000001_default , 1._default-t/(E*E)))
  end function get_beta

  function parton_get_beta(prt) result(beta)
    type(parton_t), intent(in) :: prt
    real(default) :: beta

    beta = get_beta(prt%t, vector4_get_component(prt%momentum,0))
  end function parton_get_beta

  subroutine parton_print(prt)
    type(parton_t), intent(in) :: prt

    write(*,100, ADVANCE = "NO") prt%nr
100 format(1x, I5)
    if(parton_is_final(prt)) then
110    format(1x,' ', I5, ' ')
       write(*, 110, ADVANCE="NO") prt%typ
    else
111    format(1x,'(', I5, ')')
       write(*, 111, ADVANCE="NO") prt%typ
    end if
101 format(I5)
102 format(5x)
    if (associated(prt%parent)) then
       write(*,101, ADVANCE="NO") prt%parent%nr
    else
       write(*,102, ADVANCE="NO")
    end if
103 format(1x, F9.3, F9.3, F9.3, F10.3, F14.5, F14.5, F14.5)
    write(*,103, ADVANCE="NO") vector4_get_component(prt%momentum,1), vector4_get_component(prt%momentum,2), & 
                               vector4_get_component(prt%momentum,3), vector4_get_component(prt%momentum,0), & 
                               parton_p4square(prt), prt%t, prt%scale

    if (parton_is_branched(prt)) then
104    format(1x, F8.5, F8.5, F8.5, F8.5, F8.5, 1x, A1)
       if(prt%belongstoFSR) then
          write(*,104, ADVANCE="NO") prt%z, parton_get_costheta(prt), parton_get_costheta_korrekt(prt), &
                                     prt%costheta, parton_get_costheta_motherfirst(prt), 'b'
       else
          write(*,104, ADVANCE="NO") prt%z, prt%x, parton_get_costheta_korrekt(prt), prt%costheta, & 
                                     parton_get_costheta_motherfirst(prt), 'b'
       end if
    else
       if(prt%belongstoFSR) then
105       format(43x)
          write(*,105, ADVANCE="NO")
       else
106       format(9x, F8.5, 26x)
          write(*,106, ADVANCE="NO") prt%x
       end if
    end if
107 format(A1)
    if(prt%belongstoFSR) then
       write(*,107, ADVANCE="NO") "F"
    else
       write(*,107, ADVANCE="NO") "I"
    end if
    if(parton_is_final(prt)) then
       write(*,107, ADVANCE="NO") "f"
    else
       write(*,107, ADVANCE="NO") " "
    end if

    if(parton_is_simulated(prt)) then
       write(*,107, ADVANCE="NO") "s"
    else 
       write(*,107, ADVANCE="NO") " "
    end if
    if(associated(prt%child1).and.associated(prt%child2)) then
108    format("  C:", I3, I3)
       write(*,108, ADVANCE="NO") prt%child1%nr,prt%child2%nr
    else if(associated(prt%child1)) then
109    format("  C:", I3)
       write(*,109, ADVANCE="NO") prt%child1%nr
    end if 
    if(associated(prt%initial)) then
112    format("  I:", I4)
       write(*,112, ADVANCE="NO") prt%initial%nr
    end if 
    if(prt%belongstointeraction .eqv. .true.) then
       write(*,107, ADVANCE="NO") "T"
    end if
113 format(A4)
114 format(I4)
    write(*, 113, ADVANCE = "NO") " CPP: "
    print *, "CI:", prt%c1, prt%c2
!    write(*,*)
  end  subroutine parton_print

  function parton_is_final(prt) result(is_final)
    type(parton_t), intent(in) :: prt
    logical :: is_final

    is_final = .false.
    if(prt%belongstoFSR) then 
       is_final = (.not. associated(prt%child1)) .and. ( (prt%belongstointeraction.eqv..false.) & 
                   .or. ((prt%belongstointeraction.eqv..true.) .and. (prt%simulated)) )
    end if
  end function parton_is_final

  function parton_is_branched(prt) result(is_branched)
    type(parton_t), intent(in) :: prt
    logical :: is_branched

    is_branched = (associated(prt%child1).and.associated(prt%child2))
  end function parton_is_branched

  subroutine parton_set_simulated(prt, sim)
    type(parton_t), intent(inout) :: prt
    logical, intent(in), optional :: sim
    
    if(present(sim)) then
       prt%simulated=sim
    else
       prt%simulated=.true.
    end if
  end subroutine parton_set_simulated

  function parton_is_simulated(prt) result(is_simulated)
    type(parton_t), intent(in) :: prt
    logical :: is_simulated

    is_simulated = prt%simulated
  end function parton_is_simulated

  function parton_get_momentum(prt, i) result(mom)
    type(parton_t), intent(in) :: prt
    integer, intent(in) :: i
    real(default) :: mom

    select case (i)
    case(0)
       mom=vector4_get_component(prt%momentum,0)
    case(1)
       mom=vector4_get_component(prt%momentum,1)
    case(2)
       mom=vector4_get_component(prt%momentum,2)
    case(3)
       mom=vector4_get_component(prt%momentum,3)
    case default
       mom=0
    end select
  end function parton_get_momentum

  subroutine parton_set_momentum(prt, EE, ppx, ppy, ppz)
    type(parton_t), intent(inout) :: prt
    real(default), intent(in) :: EE, ppx, ppy, ppz

    prt%momentum = vector4_moving(EE, vector3_moving( (/ppx, ppy, ppz/) ) )
  end subroutine parton_set_momentum

  subroutine parton_set_energy(prt, E)
    type(parton_t), intent(inout) :: prt
    real(default), intent(in) :: E

    call vector4_set_component(prt%momentum, 0, E)
  end subroutine parton_set_energy

  function parton_get_energy(prt) result(E)
    type(parton_t), intent(in) :: prt
    real(default) :: E
    
    E = vector4_get_component(prt%momentum, 0)
  end function parton_get_energy

  subroutine parton_set_parent(prt, parent)
    type(parton_t), intent(inout) :: prt
    type(parton_t), intent(in) , target :: parent

    prt%parent=>parent
  end subroutine parton_set_parent

  function parton_get_parent(prt) result(parent)
    type(parton_t), intent(in) :: prt
    type(parton_t), pointer :: parent

    parent=>prt%parent
  end function parton_get_parent

  subroutine parton_set_initial(prt, initial)
    type(parton_t), intent(inout) :: prt
    type(parton_t), intent(in) , target :: initial

    prt%initial=>initial
  end subroutine parton_set_initial

  function parton_get_initial(prt) result(initial)
    type(parton_t), intent(in) :: prt
    type(parton_t), pointer :: initial

    initial=>prt%initial
  end function parton_get_initial

  subroutine parton_set_child(prt, child, i)
    type(parton_t), intent(inout) :: prt
    type(parton_t), intent(in), target :: child
    integer, intent(in) ::  i
    if (i .eq. 1) then
       prt%child1 => child
    else
       prt%child2 => child
    end if
  end subroutine parton_set_child

  function parton_get_child(prt,i) result(child)
    type(parton_t), intent(in) :: prt
    integer, intent(in) :: i
    type(parton_t), pointer :: child

    child => null()
    if(i.eq.1) then
       child=>prt%child1
    else
       child=>prt%child2
    end if
  end function parton_get_child

  function parton_is_quark(prt) result(is_quark)
    type(parton_t), intent(in) ::prt
    logical :: is_quark

    is_quark=((abs(prt%typ) <= 6) .and. (prt%typ.ne.0))
  end function parton_is_quark

!!$  function parton_is_squark(prt) result(is_squark)
!!$    type(parton_t), intent(in) ::prt
!!$    logical :: is_squark
!!$
!!$    is_squark=(((abs(prt%typ)>=1000001).and.(abs(prt%typ)<=1000006)).or.((abs(prt%typ)>=2000001).and.(abs(prt%typ)<=2000006)))
!!$  end function parton_is_squark

  function parton_is_gluon(prt) result(is_gluon)
    type(parton_t), intent(in) :: prt
    logical :: is_gluon

    is_gluon = (prt%typ .eq. 21)
  end function parton_is_gluon

!!$  function parton_is_gluino(prt) result(is_gluino)
!!$    type(parton_t), intent(in) :: prt
!!$    logical :: is_gluino
!!$
!!$    is_gluino = (prt%typ .eq. 1000021)
!!$  end function parton_is_gluino

  function parton_is_hadron(prt) result(is_hadron)
    type(parton_t), intent(in) ::prt
    logical :: is_hadron

    is_hadron=(abs(prt%typ) .eq. 2212)  ! only proton implemented yet
  end function parton_is_hadron

  function parton_p4square(prt) result(p4square)
    type(parton_t), intent(in) :: prt
    real(default) :: p4square

    p4square=prt%momentum**2
  end function parton_p4square

  function parton_p3square(prt) result(p3square)
    type(parton_t), intent(in) :: prt
    real(default) :: p3square
    
    p3square=parton_p3abs(prt)**2
  end function parton_p3square

  function parton_p3abs(prt) result(p3abs)
    type(parton_t), intent(in) :: prt
    real(default) :: p3abs

    p3abs=space_part_norm(prt%momentum)
  end function parton_p3abs

  function parton_mass(prt) result(mass)
    type(parton_t), intent(in) :: prt
    real(default) :: mass

    mass=mass_typ(prt%typ)
  end function parton_mass

  function parton_mass_squared(prt) result(mass_squared)
    type(parton_t), intent(in) :: prt
    real(default) :: mass_squared

    mass_squared=mass_squared_typ(prt%typ)
  end function parton_mass_squared

  function P_prt_to_child1(prt) result(retvalue)
    type(parton_t), intent(in) :: prt
    real(default) :: retvalue

    if(parton_is_gluon(prt)) then
       if(parton_is_quark(prt%child1)) then
          retvalue=P_gqq(prt%z)
       else if(parton_is_gluon(prt%child1)) then
          retvalue=P_ggg(prt%z)+P_ggg(1._default-prt%z)
       end if
    else if(parton_is_quark(prt)) then
       if(parton_is_quark(prt%child1)) then
          retvalue=P_qqg(prt%z)
       else if(parton_is_gluon(prt%child1)) then
          retvalue=P_qqg(1._default-prt%z)
       end if
    end if
  end function P_prt_to_child1

  function thetabar(prt, recoiler) result(retvalue)
    ! returns whether kinematics of branching of prt into its daughters are allowed
    type(parton_t), intent(inout) :: prt
    type(parton_t), intent(in) :: recoiler
    logical :: retvalue

    real(default) :: ctheta, cthetachild1
    real(default) p1, p4, p3, E3, shat

    shat = (prt%child1%momentum + recoiler%momentum)**2
    E3 = 0.5_default*(shat/prt%z -recoiler%t + prt%child1%t - parton_mass_squared(prt%child2))/sqrt(shat)

    ! absolute values of momenta in a 3 -> 1 + 4 branching
    p3=sqrt(E3**2-prt%t)
    p1=sqrt(parton_get_energy(prt%child1)**2-prt%child1%t)
    p4=sqrt(max(0._default, (E3-parton_get_energy(prt%child1))**2-prt%child2%t))

    if(p3>0._default) then
       retvalue=( (p1+p4 .ge. p3) .and. (p3 .ge. abs(p1-p4)) )
       if (retvalue .and. isr_angular_ordered) then
          ! check angular ordering
          if(associated(prt%child1)) then
             if(associated(prt%child1%child2)) then
                ctheta = ( E3**2 - p1**2 - p4**2 +prt%t)/(2._default*p1*p4)
                cthetachild1=( parton_get_energy(prt%child1)**2 - space_part(prt%child1%child1%momentum)**2 &
                     - space_part(prt%child1%child2%momentum)**2 + prt%child1%t) &
                     /(2._default*space_part(prt%child1%child1%momentum)**1*space_part(prt%child1%child2%momentum)**1)
                retvalue= (ctheta > cthetachild1)
             end if
          end if
       end if
    else
       retvalue=.false.
    end if
  end function thetabar

  recursive subroutine parton_apply_z(prt, newz)
    type(parton_t), intent(inout) :: prt
    real(default), intent(in) :: newz

    if (D_print) print *, "old z:", prt%z , " new z: ", newz
    prt%z=newz
    if(associated(prt%child1) .and. associated(prt%child2) ) then
       call parton_set_energy(prt%child1, newz*parton_get_energy(prt))
       call parton_apply_z(prt%child1, prt%child1%z)
       call parton_set_energy(prt%child2, (1.-newz)*parton_get_energy(prt))
       call parton_apply_z(prt%child2, prt%child2%z)
    end if
  end subroutine parton_apply_z

  recursive subroutine parton_apply_costheta(prt)
    type(parton_t), intent(inout) :: prt

    prt%z=0.5_default*(1._default+parton_get_beta(prt)*prt%costheta)
    if(associated(prt%child1) .and. associated(prt%child2) ) then
       if(parton_is_simulated(prt%child1) .and. parton_is_simulated(prt%child2)) then
          prt%z=0.5_default*(1._default+(prt%child1%t-prt%child2%t)/prt%t+parton_get_beta(prt)*prt%costheta* & 
                sqrt( (prt%t - prt%child1%t - prt%child2%t)**2 - 4 *prt%child1%t*prt%child2%t)/prt%t)
          if(prt%typ .ne. 94) then
             call parton_set_energy(prt%child1, prt%z*parton_get_energy(prt))
             call parton_set_energy(prt%child2, (1._default-prt%z)*parton_get_energy(prt))
          end if
          call parton_generate_ps(prt)
          call parton_apply_costheta(prt%child1)
          call parton_apply_costheta(prt%child2)
       end if
    end if
  end subroutine parton_apply_costheta

  subroutine parton_apply_lorentztrafo(prt, L)
    type(parton_t), intent(inout) :: prt
    type(lorentz_transformation_t), intent(in) :: L

    prt%momentum = L*prt%momentum
  end subroutine parton_apply_lorentztrafo

  recursive subroutine parton_apply_lorentztrafo_recursiv(prt, L)
    type(parton_t), intent(inout) :: prt
    type(lorentz_transformation_t) ,intent(in) :: L

    if(prt%typ/=2212.and.prt%typ/=9999) then ! don't boost hadrons and beam-remnants
       call parton_apply_lorentztrafo(prt, L)
    end if
    if(associated(prt%child1) .and. associated(prt%child2)) then
       if((parton_p3abs(prt%child1).eq.0._default).and.(parton_p3abs(prt%child2).eq.0._default).and. & 
           (prt%child1%belongstointeraction.eqv..false.).and.(prt%child2%belongstointeraction.eqv..false.)) then
          ! don't boost unevolved timelike partons
       else
          call parton_apply_lorentztrafo_recursiv(prt%child1, L)
          call parton_apply_lorentztrafo_recursiv(prt%child2, L)
       end if
    else
       if(associated(prt%child1)) then
          call parton_apply_lorentztrafo_recursiv(prt%child1, L)
       end if
       if(associated(prt%child2)) then
          call parton_apply_lorentztrafo_recursiv(prt%child2, L)
       end if
    end if
  end subroutine parton_apply_lorentztrafo_recursiv

  subroutine parton_generate_ps(prt)
  ! takes the three-momentum of a parton and generates three-momenta of its children
    type(parton_t), intent(inout) :: prt
    real(default), dimension(1:3, 1:3) :: directions
    integer i,j
    real(default) :: scprodukt, pbetrag, p1betrag, p2betrag, x, pTbetrag, phi
    real(default), dimension(1:3) :: momentum

    type(vector3_t) :: pchild1_direction
    type(lorentz_transformation_t) :: L, rotation

    if(D_print) print *, " generate_ps for parton " , prt%nr
    if(.not. (associated(prt%child1) .and. associated(prt%child2))) then
       print *, "no children for generate_ps"
       return
    end if
    ! test if parton is a virtual parton from the imagined parton shower history
    if(prt%typ .eq. 94) then
       L = inverse(boost(prt%momentum, sqrt(prt%t)))        ! boost to restframe of mother
       call parton_apply_lorentztrafo(prt, L)
       call parton_apply_lorentztrafo(prt%child1, L)
       call parton_apply_lorentztrafo(prt%child2, L)

       ! store child1's momenta
       pchild1_direction = direction(space_part(prt%child1%momentum))
       
       ! redistribute energy
       call parton_set_energy(prt%child1, (parton_get_energy(prt)**2- & 
                                           prt%child2%t+prt%child1%t)/(2._default*parton_get_energy(prt)))
       call parton_set_energy(prt%child2, parton_get_energy(prt)-parton_get_energy(prt%child1))

       ! rescale momenta and set momenta to be along z-axis 
       prt%child1%momentum = vector4_moving( parton_get_energy(prt%child1), & 
            vector3_canonical(3)* sqrt(parton_get_energy(prt%child1)**2-prt%child1%t))
       prt%child2%momentum = vector4_moving( parton_get_energy(prt%child2), & 
            vector3_canonical(3)*(-sqrt(parton_get_energy(prt%child2)**2-prt%child2%t)))

       ! rotate so that total momentum is along former total momentum
       rotation = rotation_to_2nd(space_part(prt%child1%momentum), pchild1_direction)
       call parton_apply_lorentztrafo(prt%child1, rotation)
       call parton_apply_lorentztrafo(prt%child2, rotation)

       L = inverse(L)             ! inverse of the boost to restframe of mother
       call parton_apply_lorentztrafo(prt, L)
       call parton_apply_lorentztrafo(prt%child1, L)
       call parton_apply_lorentztrafo(prt%child2, L)
    else
       ! directions(1,:) -> direction of the parent parton
       if(parton_p3abs(prt) .eq. 0._default) return
       do i=1,3
          directions(1,i) = parton_get_momentum(prt,i)/parton_p3abs(prt)
       end do
       ! directions(2,:) and directions(3,:) -> two random directions perpendicular to the direction of the parent parton
       do i=1,3
          do j=2,3
             call tao_random_number(directions(j,i))
          end do
       end do
       do i=2,3
          scprodukt=0._default
          do j=1, i-1
             scprodukt = directions(i,1)*directions(j,1)+directions(i,2)*directions(j,2)+directions(i,3)*directions(j,3)
             directions(i,1)=directions(i,1)-directions(j,1)*scprodukt
             directions(i,2)=directions(i,2)-directions(j,2)*scprodukt
             directions(i,3)=directions(i,3)-directions(j,3)*scprodukt
          end do
          scprodukt=directions(i,1)**2+directions(i,2)**2+directions(i,3)**2
          do j=1,3
             directions(i,j) = directions(i,j)/sqrt(scprodukt)
          end do
       end do
       ! enforce righthanded system
       if((directions(1,1)*(directions(2,2)*directions(3,3)-directions(2,3)&
*directions(3,2))+directions(1,2)*(directions(2,3)*directions(3,1)-&
directions(2,1)*directions(3,3))+directions(1,3)*(directions(2,1)*directions(3,2)&
-directions(2,2)*directions(3,1)))<0) then
          directions(3,1)=-directions(3,1)
          directions(3,2)=-directions(3,2)
          directions(3,3)=-directions(3,3)
       end if

       pbetrag=parton_p3abs(prt)
       if( (parton_get_energy(prt%child1)**2-prt%child1%t < 0) .or. & 
           (parton_get_energy(prt%child2)**2-prt%child2%t < 0)) then
          if(D_print) print *, "err: error at generate_ps(), E^2 < t"
          return
       end if
       p1betrag = sqrt(parton_get_energy(prt%child1)**2-prt%child1%t)
       p2betrag = sqrt(parton_get_energy(prt%child2)**2-prt%child2%t)
       x=(pbetrag*pbetrag +p1betrag*p1betrag - p2betrag*p2betrag)/(2.*pbetrag)
       if(parton_p3abs(prt)>p1betrag+p2betrag .or. parton_p3abs(prt) < abs(p1betrag-p2betrag)) then
          if(D_print) then
             print *,"error at generate_ps, Dreiecksungleichung for parton ", & 
                     prt%nr, " ", parton_p3abs(prt)," ",p1betrag," ",p2betrag
             call parton_print(prt)
             call parton_print(prt%child1)
             call parton_print(prt%child2)
          end if
          return
       end if
       ! due to numerical problems transverse momentum could be imaginary -> set transverse momentum to zero
       pTbetrag=sqrt(max(p1betrag*p1betrag - x*x, 0._default))
       call tao_random_number(phi)
       phi=twopi*phi
       do i=1,3
          momentum(i) = x*directions(1,i)+pTbetrag*(cos(phi)*directions(2,i)+sin(phi)*directions(3,i))
       end do
       call parton_set_momentum(prt%child1, parton_get_energy(prt%child1), momentum(1), momentum(2), momentum(3))
       do i=1,3
          momentum(i) = (parton_p3abs(prt)-x)*directions(1,i)-pTbetrag*(cos(phi)*directions(2,i)+sin(phi)*directions(3,i))
       end do
       call parton_set_momentum(prt%child2, parton_get_energy(prt%child2), momentum(1), momentum(2), momentum(3))
    end if
  end subroutine parton_generate_ps

  subroutine parton_generate_ps_ini(prt)
    ! takes the three-momentum of a partons first child as fixed and generates the two remaining three-momenta
    ! similar to parton_generate_ps, but now for ISR
    type(parton_t), intent(inout) :: prt
    real(default), dimension(1:3, 1:3) :: directions
    integer i,j
    real(default) :: scprodukt, pbetrag, p1betrag, p2betrag, x, pTbetrag, phi
    real(default), dimension(1:3) :: momentum
 
    if(D_print) print *, " generate_ps_ini for parton " , prt%nr
    if(.not. (associated(prt%child1) .and. associated(prt%child2))) then
       print *, "error in parton_generate_ps_ini"
       return
    end if

    if(parton_is_hadron(prt) .eqv. .false.) then ! generate ps for normal partons
       do i=1,3
          directions(1,i) = parton_get_momentum(prt%child1,i)/parton_p3abs(prt%child1)
       end do
       do i=1,3
          do j=2,3
             call tao_random_number(directions(j,i))
          end do
       end do
       do i=2,3
          scprodukt=0._default
          do j=1, i-1
             scprodukt = directions(i,1)*directions(j,1)+directions(i,2)*directions(j,2)+directions(i,3)*directions(j,3)
             directions(i,1)=directions(i,1)-directions(j,1)*scprodukt
             directions(i,2)=directions(i,2)-directions(j,2)*scprodukt
             directions(i,3)=directions(i,3)-directions(j,3)*scprodukt
          end do
          scprodukt=directions(i,1)**2+directions(i,2)**2+directions(i,3)**2
          do j=1,3
             directions(i,j) = directions(i,j)/sqrt(scprodukt)
          end do
       end do
       ! enforce righthanded system
       if((directions(1,1)*(directions(2,2)*directions(3,3)-directions(2,3)&
            *directions(3,2))+directions(1,2)*(directions(2,3)*directions(3,1)-&
            directions(2,1)*directions(3,3))+directions(1,3)*(directions(2,1)*directions(3,2)&
            -directions(2,2)*directions(3,1)))<0) then
          directions(3,1)=-directions(3,1)
          directions(3,2)=-directions(3,2)
          directions(3,3)=-directions(3,3)
       end if

       pbetrag=parton_p3abs(prt%child1)
       p1betrag = sqrt(parton_get_energy(prt)**2-prt%t)
       p2betrag = sqrt(max(0._default, parton_get_energy(prt%child2)**2-prt%child2%t))
       
       x=(pbetrag*pbetrag +p1betrag*p1betrag - p2betrag*p2betrag)/(2.*pbetrag)
       if(pbetrag>p1betrag+p2betrag .or.&
            pbetrag < abs(p1betrag-p2betrag)) then
          ! if(D_print) 
          print *,"error at generate_ps, Dreiecksungleichung for parton ",prt%nr, " ", pbetrag," ",p1betrag," ",p2betrag
          call parton_print(prt)
          call parton_print(prt%child1)
          call parton_print(prt%child2)
          return
       end if
       if(D_print) print *, "x:",x 
       pTbetrag=sqrt(p1betrag*p1betrag - x*x)
       call tao_random_number(phi)
       phi=twopi*phi
       do i=1,3
          momentum(i) = x*directions(1,i)+pTbetrag*(cos(phi)*directions(2,i)+sin(phi)*directions(3,i))
       end do
       call parton_set_momentum(prt, parton_get_energy(prt), momentum(1), momentum(2), momentum(3))
       do i=1,3
          momentum(i) = (x-pbetrag)*directions(1,i)+pTbetrag*(cos(phi)&
               *directions(2,i)+sin(phi)*directions(3,i))
       end do
       call parton_set_momentum(prt%child2, parton_get_energy(prt%child2), momentum(1), momentum(2), momentum(3))
    else ! for first partons just set beam remnants momentum
       prt%child2%momentum = prt%momentum - prt%child1%momentum
    end if
  end subroutine parton_generate_ps_ini

! ---------------------analytic FSR-----------------

  function cmax(prt, tt) result(cma)
    type(parton_t), intent(in) :: prt
    real(default), intent(in), optional :: tt
    real(default) :: cma

    real(default) :: t, cost

    if(present(tt)) then
       t = tt
    else
       t = prt%t
    end if

    if(associated(prt%parent)) then
       cost = parton_get_costheta(prt%parent)
       cma = min(0.99999_default, sqrt( max(0._default, 1._default - t/ & 
              (parton_get_beta(prt)*parton_get_energy(prt))**2 * (1._default+cost)/(1._default-cost) )))
    else
       cma = 0.99999_default
    end if
  end function cmax

  subroutine parton_next_t_ana(prt)
    type(parton_t), intent(inout) :: prt
    integer :: gtoqq

    real(default) :: integral, zufall

    if(D_print) then
       print *, "next_t_ana for parton " , prt%nr
    end if

    ! check if branchings are possible at all
    prt%t=min(prt%t, abs(prt%parent%t) )
    if(min(prt%t, parton_get_energy(prt)**2)<parton_mass_squared(prt)+D_Min_t) then
       prt%t=parton_mass_squared(prt)
       call parton_set_simulated(prt)
       return
    end if

    integral=0._default
    call tao_random_number(zufall)

    do
       call parton_simulate_stept(prt, integral, zufall, gtoqq, .false.)
       if(parton_is_simulated(prt)) then
          if(parton_is_gluon(prt)) then
             ! misusing the x-variable to store the informatin to which quark flavour the gluon branches (if any)
             prt%x=1._default*gtoqq+0.1_default
             ! x=gtoqq+0.1 -> int(x) will be the quark flavour or zero for g -> gg
          end if
          exit
       end if
    end do
  end subroutine parton_next_t_ana

  subroutine parton_simulate_stept(prt, integral, zufall, gtoqq, lookatsister)
    type(parton_t), intent(inout) :: prt
    real(default), intent(inout) :: integral
    real(default), intent(inout) :: zufall
    integer, intent(out) :: gtoqq
    logical, intent(in), optional :: lookatsister   ! take limitations by sister into account, if not given assume .true.

    type(parton_t), pointer :: sister
    real(default) :: tstep,tmin, oldt
    real(default) :: c, cstep
    real(default) :: z(3), P(3)
    real(default) :: zuintegral
    real(default) :: a11,a12,a13,a21,a22,a23
    real(default) :: cmax_t
    real(default) :: temprand

    ! values for integration
    real(default) :: a(3),x(3)

    ! higher values -> faster but coarser
    real(default), parameter :: tstepfactor=0.02_default
    real(default), parameter :: tstepmin=0.5_default
    real(default), parameter :: cstepfactor=0.8_default
    real(default), parameter :: cstepmin=0.03_default

    gtoqq = 111 ! illegal value
    call parton_set_simulated(prt, .false.)

    sister=>null()
    if(present(lookatsister)) then
       if(lookatsister .eqv. .true.) then
          if(prt%nr.eq.prt%parent%child1%nr) then
             sister => prt%parent%child2
          else
             sister => prt%parent%child1
          end if
       end if
    else
       if(prt%nr.eq.prt%parent%child1%nr) then
          sister => prt%parent%child2
       else
          sister => prt%parent%child1
       end if
    end if

    tmin=D_Min_t+parton_mass_squared(prt)
    if(parton_is_quark(prt)) then
       zuintegral = 3._default*pi*log(1._default/zufall)
    else if(parton_is_gluon(prt)) then
       zuintegral = 4._default*pi*log(1._default/zufall)
    else
       prt%t = parton_mass_squared(prt)
       call parton_set_simulated(prt)
       return
    end if 
    
    if(associated(sister)) then
       if(sqrt(prt%t) > sqrt(prt%parent%t) - sqrt(parton_mass_squared(sister))) then
          prt%t=(sqrt(prt%parent%t) - sqrt(parton_mass_squared(sister)))**2
       end if
    end if
    if(prt%t>parton_get_energy(prt)**2) then
       prt%t=parton_get_energy(prt)**2
    end if

    if(prt%t .le. tmin) then
       prt%t=parton_mass_squared(prt)
       call parton_set_simulated(prt)
       return
    end if

    ! simulate the branchings between prt%t and prt%t-tstep
    tstep=max(tstepfactor*(prt%t-0.9_default*tmin), tstepmin)
    cmax_t=cmax(prt)
    c=-cmax_t ! take highest t -> minimal constraint
    cstep=max(cstepfactor*(1._default-abs(c)), cstepmin)
    ! get values at border of "previous" bin -> to be used in first bin
    z(3)=0.5_default+0.5_default*get_beta(prt%t-0.5_default*tstep, parton_get_energy(prt))*c
    if(parton_is_gluon(prt)) then
       P(3)=P_ggg(z(3))+P_gqq(z(3))*number_of_flavors(prt%t)
    else
       P(3)=P_qqg(z(3))
    end if
    a(3)=D_alpha_s_fsr(z(3)*(1._default-z(3))*prt%t)*P(3)/(prt%t-0.5_default*tstep)

    do while(c<cmax_t.and.(integral<zuintegral))
       cmax_t=cmax(prt)
       cstep=max(cstepfactor*(1-abs(c)**2), cstepmin)
       if(c+cstep>cmax_t) then
          cstep=cmax_t-c
       end if
       if(cstep < 1D-10) then
          ! reject too small bins
          exit
       end if
       z(1)=z(3)
       z(2)=0.5_default+0.5_default*get_beta(prt%t-0.5_default*tstep, parton_get_energy(prt))*(c+0.5_default*cstep)
       z(3)=0.5_default+0.5_default*get_beta(prt%t-0.5_default*tstep, parton_get_energy(prt))*(c+cstep)
       P(1)=P(3)
       if(parton_is_gluon(prt)) then
          P(2)=P_ggg(z(2))+P_gqq(z(2))*number_of_flavors(prt%t)
          P(3)=P_ggg(z(3))+P_gqq(z(3))*number_of_flavors(prt%t)
       else
          P(2)=P_qqg(z(2))
          P(3)=P_qqg(z(3))
       end if
       ! get values at borders of the intgral and in the middle
       a(1)=a(3)
       a(2)=D_alpha_s_fsr(z(2)*(1._default-z(2))*prt%t)*P(2)/(prt%t-0.5_default*tstep)
       a(3)=D_alpha_s_fsr(z(3)*(1._default-z(3))*prt%t)*P(3)/(prt%t-0.5_default*tstep)

       ! fit x(1)+x(2)/(1+c)+x(3)/(1-c) to these values !! a little tricky
       a11 = (1._default+c+0.5_default*cstep)*(1._default-c-0.5_default*cstep) - &
             (1._default-c)*(1._default+c+0.5_default*cstep)
       a12 = (1._default-c-0.5_default*cstep)-(1._default+c+0.5_default*cstep) * &
             (1._default-c)/(1._default+c)
       a13 = a(2)*(1._default+c+0.5_default*cstep)*(1._default-c-0.5_default*cstep)- & 
             a(1)*(1._default-c)*(1._default+c+0.5_default*cstep)
       a21 = (1._default+c+cstep)*(1._default-c-cstep)-(1._default+c+cstep)*(1._default-c)
       a22 = (1._default-c-cstep)-(1._default+c+cstep)*(1._default-c)/(1._default+c)
       a23 = a(3)*(1._default+c+cstep)*(1._default-c-cstep)-a(1)*(1._default-c)*(1._default+c+cstep)

       x(2)=(a23-a21*a13/a11)/(a22-a12*a21/a11)
       x(1)=(a13-a12*x(2))/a11
       x(3)=a(1)*(1._default-c)-x(1)*(1._default-c)-x(2)*(1._default-c)/(1._default+c)

       integral = integral+tstep*(x(1)*cstep+x(2)*log((1._default+c+cstep)/(1._default+c))-x(3) * &
                 log((1._default-c-cstep)/(1._default-c)))
       
       if(integral>zuintegral) then
          oldt=prt%t
          call tao_random_number(temprand)
          prt%t=prt%t-temprand*tstep
          call tao_random_number(temprand)
          prt%costheta=c+(0.5_default-temprand)*cstep
          call parton_set_simulated(prt)

          if(prt%t < D_Min_t + parton_mass_squared(prt)) then
             prt%t=parton_mass_squared(prt)
          end if
          if(prt%costheta.lt.-cmax_t .or. prt%costheta.gt.cmax_t) then
             ! reject branching due to violation of costheta-limits
             call tao_random_number(zufall)
             if(parton_is_quark(prt)) then
                zuintegral = 3._default*pi*log(1._default/zufall)
             else if(parton_is_gluon(prt)) then
                zuintegral = 4._default*pi*log(1._default/zufall)
             end if
             integral=0._default
             prt%t=oldt
             call parton_set_simulated(prt, .false.)
          end if
          if(parton_is_gluon(prt)) then
             ! decide between g->gg and g->qqbar
             z(1)=0.5_default+0.5_default*prt%costheta
             call tao_random_number(temprand)
             if(P_ggg(z(1)) > temprand*(P_ggg(z(1))+P_gqq(z(1))*number_of_flavors(prt%t))) then
                gtoqq=0
             else
                call tao_random_number(temprand)
                gtoqq=1+temprand*number_of_flavors(prt%t)
             end if
          end if
       else
          c=c+cstep
       end if
       cmax_t=cmax(prt)
    end do
    if(integral<=zuintegral) then
       prt%t=prt%t-tstep
       if(prt%t < D_Min_t + parton_mass_squared(prt)) then
          prt%t=parton_mass_squared(prt)
          call parton_set_simulated(prt)
       end if
    end if
  end subroutine parton_simulate_stept

!------------------------------------------------------------
! ISR-algorithm
! all the ISR-stuff moved to shower_module.f90
! only maxzz remains here -> needed in more than one procedure in shower_module

  function maxzz(shat, s) result(maxz)
    real(default), intent(in) :: shat,s
    real(default) :: maxz
    
    maxz=min(maxz_isr, 1._default-(2._default*minenergy_timelike*sqrt(shat))/s)
  end function maxzz

end module shower_parton_module
