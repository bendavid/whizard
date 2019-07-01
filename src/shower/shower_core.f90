! WHIZARD 2.2.3 Nov 30 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Christian Weiss <christian.weiss@desy.de>
!     and Felix Braam, Sebastian Schmidt, Daniel Wiesler 
!
! WHIZARD is free software; you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by 
! the Free Software Foundation; either version 2, or (at your option)
! any later version.
!
! WHIZARD is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This file has been stripped of most comments.  For documentation, refer
! to the source 'whizard.nw'

module shower_core

  use kinds, only: default !NODEP!
  use io_units !NODEP!
  use constants !NODEP!
  use lorentz !NODEP!
  use system_defs, only: TAB !NODEP!
  use diagnostics !NODEP!
  use tao_random_numbers !NODEP!
  use pdf_builtin !NODEP!
  use lhapdf !NODEP!
  use shower_base
  use shower_partons
  use ckkw_pseudo_weights

  implicit none
  private

  public :: shower_interaction_t
  public :: shower_t
  public :: shower_get_final_partons
  public :: shower_interaction_get_shat
  public :: shower_interaction_get_s

  ! real(default), parameter :: alphasmax = one
  ! real(default), parameter :: xpdfmax = 10._default
  real(default), save :: alphasxpdfmax = 12._default


  type :: shower_interaction_t
     type(parton_pointer_t), dimension(:), allocatable :: partons
  end type shower_interaction_t

  type :: shower_interaction_pointer_t
     type(shower_interaction_t), pointer :: i => null ()
  end type shower_interaction_pointer_t

  type :: shower_t
     type(shower_interaction_pointer_t), dimension(:), allocatable :: &
          interactions
     type(parton_pointer_t), dimension(:), allocatable :: partons
     type(lhapdf_pdf_t) :: pdf
     integer :: next_free_nr
     integer :: next_color_nr
     logical :: valid
   contains
     procedure :: add_interaction_2ton => shower_add_interaction_2ton
     procedure :: add_interaction_2ton_CKKW => shower_add_interaction_2ton_CKKW
     procedure :: simulate_no_isr_shower => shower_simulate_no_isr_shower
     procedure :: simulate_no_fsr_shower => shower_simulate_no_fsr_shower
     procedure :: sort_partons => shower_sort_partons
     procedure :: create => shower_create
     procedure :: final => shower_final
     procedure :: get_next_free_nr => shower_get_next_free_nr
     procedure :: set_next_color_nr => shower_set_next_color_nr
     procedure :: get_next_color_nr => shower_get_next_color_nr
     procedure :: add_child => shower_add_child
     procedure :: add_parent => shower_add_parent
     procedure :: get_final_colored_ME_partons => &
          shower_get_final_colored_ME_partons
     procedure :: update_beamremnants => shower_update_beamremnants
     procedure :: boost_to_labframe => shower_boost_to_labframe
     procedure :: generate_primordial_kt => shower_generate_primordial_kt
     procedure :: write => shower_write
     procedure :: write_lhef => shower_write_lhef
     procedure :: generate_next_isr_branching_veto => &
          shower_generate_next_isr_branching_veto
     procedure :: generate_next_isr_branching => &
          shower_generate_next_isr_branching
     procedure :: generate_fsr_for_isr_partons => &
          shower_generate_fsr_for_partons_emitted_in_ISR
     procedure :: execute_next_isr_branching => shower_execute_next_isr_branching
     procedure :: get_ISR_scale => shower_get_ISR_scale
     procedure :: set_max_isr_scale => shower_set_max_isr_scale
     procedure :: interaction_generate_fsr_2ton => &
          shower_interaction_generate_fsr_2ton
     procedure :: get_pdf => shower_get_pdf
     procedure :: get_xpdf => shower_get_xpdf
     procedure :: pdf_func => shower_pdf_func
  end type shower_t


  interface
     subroutine evolvePDFM (set, x, q, ff)
       integer, intent(in) :: set
       double precision, intent(in) :: x, q
       double precision, dimension(-6:6), intent(out) :: ff
     end subroutine evolvePDFM
  end interface


contains

  subroutine shower_add_interaction_2ton (shower, partons)
    class(shower_t), intent(inout) :: shower
    type(parton_pointer_t), intent(in), dimension(:), allocatable :: partons
    type(ckkw_pseudo_shower_weights_t) :: ckkw_pseudo_weights
    call shower%add_interaction_2ton_CKKW (partons, ckkw_pseudo_weights)
  end subroutine shower_add_interaction_2ton

  subroutine shower_add_interaction_2ton_CKKW &
       (shower, partons, ckkw_pseudo_weights)
    class(shower_t), intent(inout) :: shower
    type(parton_pointer_t), intent(in), dimension(:), allocatable :: partons
    type(ckkw_pseudo_shower_weights_t), intent(in) :: ckkw_pseudo_weights

    integer :: n_partons, n_out, n_partons_shower
    integer :: i, j, imin, jmin
    real(default) :: y, ymin
    real(default) :: w, wmax
    real(default) :: random, sum
    ! real(default) :: s
    type(parton_pointer_t), dimension(:), allocatable :: new_partons
    type(parton_t), pointer :: prt
    integer :: n_int
    type(shower_interaction_pointer_t), dimension(:), allocatable :: temp
    type(vector4_t) :: prtmomentum, childmomentum
    logical :: isr_is_possible
    type(lorentz_transformation_t) :: L

    if (signal_is_pending ()) return
    n_partons = size (partons)
    n_out = n_partons - 2
    if (n_out < 2) then
       call msg_bug &
            ("Shower core: trying to add a 2-> (something<2) interaction")
    end if
    ! print *, " adding a 2-> ", n_out, " interaction"

    isr_is_possible = associated (partons(1)%p%initial) .and. &
         associated (partons(2)%p%initial)

    if (associated (partons(1)%p%initial) .and. &
         parton_is_quark (partons(1)%p)) then
       if (parton_get_energy (partons(1)%p) < &
            2._double * parton_mass (partons(1)%p)) then
          if (abs(partons(1)%p%type) < 2) then
             treat_light_quarks_massless = .true.
          else
             treat_duscb_quarks_massless = .true.
          end if
       end if
    end if
    if (associated (partons(2)%p%initial) .and. &
         parton_is_quark (partons(2)%p)) then
       if (parton_get_energy (partons(2)%p) < &
            two * parton_mass (partons(2)%p)) then
          if (abs(partons(2)%p%type) < 2) then
             treat_light_quarks_massless = .true.
          else
             treat_duscb_quarks_massless = .true.
          end if
       end if
    end if

    if (allocated (shower%interactions)) then
       n_int = size (shower%interactions) + 1
    else
       n_int = 1
    end if
    allocate (temp (1:n_int))
    do i = 1, n_int - 1
       allocate (temp(i)%i)
       temp(i)%i = shower%interactions(i)%i
    end do
    allocate (temp(n_int)%i)
    allocate (temp(n_int)%i%partons(1:n_partons))
    do i = 1, n_partons
       allocate (temp(n_int)%i%partons(i)%p)
       call parton_copy (partons(i)%p, temp(n_int)%i%partons(i)%p)
    end do
    if (allocated (shower%interactions)) deallocate(shower%interactions)
    allocate (shower%interactions(1:n_int))
    do i = 1, n_int
       shower%interactions(i)%i => temp(i)%i
    end do
    deallocate (temp)

    if (associated (shower%interactions(n_int)%i%partons(1)%p%initial))  &
         call parton_set_simulated &
         (shower%interactions(n_int)%i%partons(1)%p%initial)
    if (associated (shower%interactions(n_int)%i%partons(2)%p%initial))  &
         call parton_set_simulated &
         (shower%interactions(n_int)%i%partons(2)%p%initial)
    if (isr_is_possible) then
       !!! boost to the CMFrame of the incoming partons
       L = boost (-(shower%interactions(n_int)%i%partons(1)%p%momentum + &
            shower%interactions(n_int)%i%partons(2)%p%momentum), &
            (shower%interactions(n_int)%i%partons(1)%p%momentum + &
            shower%interactions(n_int)%i%partons(2)%p%momentum)**1 )
       do i = 1, n_partons
          call parton_apply_lorentztrafo &
               (shower%interactions(n_int)%i%partons(i)%p, L)
       end do
    end if
    do i = 1, size (partons)
       if (signal_is_pending ()) return
       !!! ensure that partons are marked as belonging to the hard interaction
       shower%interactions(n_int)%i%partons(i)%p%belongstointeraction &
            = .true.
       !!! ensure that incoming partons are marked as belonging to ISR
       shower%interactions(n_int)%i%partons(i)%p%belongstoFSR = i > 2

       shower%interactions(n_int)%i%partons(i)%p%interactionnr &
            = n_int
       !!! include a 2^(i - 1) number as a label for the ckkw clustering
       shower%interactions(n_int)%i%partons(i)%p%ckkwlabel = 2**(i - 1)
    end do

    if (allocated (shower%partons)) then
       allocate (new_partons(1:size(shower%partons) + &
            size(shower%interactions(n_int)%i%partons)))
       do i = 1, size (shower%partons)
          new_partons(i)%p => shower%partons(i)%p
       end do
       do i = 1, size (shower%interactions(n_int)%i%partons)
          new_partons(size(shower%partons) + i)%p => &
               shower%interactions(n_int)%i%partons(i)%p
       end do
       deallocate (shower%partons)
    else
       allocate (new_partons(1:size(shower%interactions(n_int)%i%partons)))
       do i = 1, size (partons)
          new_partons(i)%p => shower%interactions(n_int)%i%partons(i)%p
       end do
    end if
    allocate (shower%partons(1:size (new_partons)))
    do i = 1, size (new_partons)
       shower%partons(i)%p => new_partons(i)%p
    end do
    deallocate (new_partons)
    !!! call shower%write ()

    if (isr_is_possible) then
       if (isr_pt_ordered) then
          call shower_prepare_for_simulate_isr_pt &
               (shower, shower%interactions(size (shower%interactions))%i)
       else
          call shower_prepare_for_simulate_isr_ana_test &
               (shower, shower%interactions(n_int)%i%partons(1)%p, &
               shower%interactions(n_int)%i%partons(2)%p)
       end if
    end if

    !!! generate pseudo parton shower history and add all partons to
    !!!          shower%partons-array
    !!! TODO initial -> initial + final branchings ??
    allocate (new_partons(1:(n_partons - 2)))
    do i = 1, size (new_partons)
       nullify (new_partons(i)%p)
    end do
    do i = 1, size (new_partons)
       new_partons(i)%p => shower%interactions(n_int)%i%partons(i + 2)%p
    end do
    imin = 0
    jmin = 0

    if (allocated (ckkw_pseudo_weights%weights)) then
       CKKW_CLUSTERING: do
          !!! search for the combination with the highest weight
          wmax = zero
          CKKW_OUTER: do i = 1, size (new_partons)
             CKKW_INNER: do j = i + 1, size (new_partons)
                if (.not. associated (new_partons(i)%p)) cycle
                if (.not. associated (new_partons(j)%p)) cycle
                w = ckkw_pseudo_weights%weights(new_partons(i)%p%ckkwlabel + &
                     new_partons(j)%p%ckkwlabel)
                if (w > wmax .or. wmax == zero) then
                   wmax = w
                   imin = i
                   jmin = j
                end if
             end do CKKW_INNER
          end do CKKW_OUTER
          if (wmax > zero) then
             call shower%add_parent (new_partons(imin)%p)
             call parton_set_child (new_partons(imin)%p%parent, &
                  new_partons(jmin)%p, 2)
             call parton_set_parent (new_partons(jmin)%p, &
                  new_partons(imin)%p%parent)
             prt => new_partons(imin)%p%parent
             prt%nr = shower_get_next_free_nr (shower)
             prt%type = 94
             !!! something for internal use needed, 81-100 should be reserved
             !!!    for internal purposes

             prt%momentum = new_partons(imin)%p%momentum + &
                  new_partons(jmin)%p%momentum
             prt%t = prt%momentum**2

             !!! auxilliary values for the ckkw matching
             !!! for now, randomly choose the type of the intermediate
             prt%ckkwlabel = new_partons(imin)%p%ckkwlabel + &
                  new_partons(jmin)%p%ckkwlabel
             sum = zero
             call tao_random_number (random)
             CKKW_TYPE: do i = 0, 4
                if (sum + &
                     ckkw_pseudo_weights%weights_by_type(prt%ckkwlabel, i) > &
                     random * ckkw_pseudo_weights%weights(prt%ckkwlabel) ) then
                   prt%ckkwtype = i
                   exit ckkw_type
                end if
                sum = sum + &
                     ckkw_pseudo_weights%weights_by_type(prt%ckkwlabel, i)
             end do CKKW_TYPE

             !!! TODO -> calculate costheta and store it for
             !!!        later use in generate_ps

             if (space_part_norm(prt%momentum) > 1E-10_default) then
                prtmomentum = prt%momentum
                childmomentum = prt%child1%momentum
                prtmomentum = boost (-parton_get_beta(prt) / &
                     sqrt (one - &
                     (parton_get_beta(prt))**2), space_part (prt%momentum) / &
                     space_part_norm(prt%momentum)) * prtmomentum
                childmomentum = boost (-parton_get_beta(prt) / &
                     sqrt(one - &
                     (parton_get_beta(prt))**2), space_part (prt%momentum) / &
                     space_part_norm(prt%momentum)) * childmomentum
                prt%costheta = enclosed_angle_ct(prtmomentum, childmomentum)
             else
                prt%costheta = - one
             end if

             prt%belongstointeraction = .true.
             prt%belongstoFSR = &
                  new_partons(imin)%p%belongstoFSR .and. &
                  new_partons(jmin)%p%belongstoFSR

             nullify (new_partons(imin)%p)
             nullify (new_partons(jmin)%p)
             new_partons(imin)%p => prt
          else
             exit CKKW_CLUSTERING
          end if
       end do CKKW_CLUSTERING
    else
       CLUSTERING: do
          !!! search for the partons to be clustered together
          ymin = zero
          OUTER: do i = 1, size (new_partons)
             INNER: do j = i + 1, size (new_partons)
                !!! calculate the jet measure
                if (.not.associated (new_partons(i)%p)) cycle INNER
                if (.not.associated (new_partons(j)%p)) cycle INNER
                !if (.not. shower_clustering_allowed &
                     !(shower, new_partons, i,j)) &
                     !cycle inner
                !!! Durham jet-measure ! don't care about constants
                y = min (parton_get_energy (new_partons(i)%p), &
                     parton_get_energy (new_partons(j)%p)) * &
                     (one - enclosed_angle_ct &
                     (new_partons(i)%p%momentum, &
                     new_partons(j)%p%momentum))
                if (y < ymin .or. ymin == zero) then
                   ymin = y
                   imin = i
                   jmin = j
                end if
             end do INNER
          end do OUTER
          if (ymin > zero) then
             call shower%add_parent (new_partons(imin)%p)
             call parton_set_child &
                  (new_partons(imin)%p%parent, new_partons(jmin)%p, 2)
             call parton_set_parent &
                  (new_partons(jmin)%p, new_partons(imin)%p%parent)
             prt => new_partons(imin)%p%parent
             prt%nr = shower_get_next_free_nr (shower)
             prt%type = 94
             !!! something for internal use needed, 81-100 should be
             !!!     reserved for internal purposes

             prt%momentum = new_partons(imin)%p%momentum + &
                  new_partons(jmin)%p%momentum
             prt%t = prt%momentum**2
             !!! TODO -> calculate costheta and store it for
             !!!       later use in generate_ps

             if (space_part_norm(prt%momentum) > 1E-10_default) then
                prtmomentum = prt%momentum
                childmomentum = prt%child1%momentum
                prtmomentum = boost (-parton_get_beta(prt) / sqrt(one - &
                     (parton_get_beta(prt))**2), space_part(prt%momentum) / &
                     space_part_norm(prt%momentum)) * prtmomentum
                childmomentum = boost (-parton_get_beta(prt) / &
                     sqrt(one - &
                     (parton_get_beta(prt))**2), space_part(prt%momentum) / &
                     space_part_norm(prt%momentum)) * childmomentum
                prt%costheta = enclosed_angle_ct(prtmomentum, childmomentum)
             else
                prt%costheta = - one
             end if

             prt%belongstointeraction = .true.
             nullify (new_partons(imin)%p)
             nullify (new_partons(jmin)%p)
             new_partons(imin)%p => prt
          else
             exit CLUSTERING
          end if
       end do CLUSTERING
    end if

    !!! add all partons to the shower
    n_partons_shower = 0
    if (allocated (shower%partons)) then
       do i = 1, size (shower%partons)
          if (associated (shower%partons(i)%p)) &
               n_partons_shower = n_partons_shower + 1
       end do
    end if

    !!! set the FSR starting scale for all partons
    do i = 1, size (new_partons)
       !!! the imaginary mother is the only parton remaining in new_partons
       if (.not. associated (new_partons(i)%p)) cycle

       call set_starting_scale (new_partons(i)%p, &
            get_starting_scale (new_partons(i)%p))
       exit
    end do

    deallocate (new_partons)

    !!! call shower%write ()
    !!! print *, "end of shower_interactionadd2ton"

  contains

    recursive subroutine transfer_pointers (destiny, start, prt)
      type(parton_pointer_t), dimension(:), allocatable :: destiny
      integer, intent(inout) :: start
      type(parton_t), pointer :: prt
      destiny(start)%p => prt
      start = start + 1
      if (associated (prt%child1)) then
         call transfer_pointers (destiny, start, prt%child1)
      end if
      if (associated (prt%child2)) then
         call transfer_pointers (destiny, start, prt%child2)
      end if
    end subroutine transfer_pointers

    recursive function get_starting_scale (prt) result (scale)
      type(parton_t), pointer :: prt
      real(default) :: scale
      scale = huge (scale)
      if (associated (prt%child1) .and. associated (prt%child2)) then
         scale = min(scale, prt%t)
      end if
      if (associated (prt%child1)) then
         scale = min (scale, get_starting_scale (prt%child1))
      end if
      if (associated (prt%child2)) then
         scale = min (scale, get_starting_scale (prt%child2))
      end if
    end function get_starting_scale

    recursive subroutine set_starting_scale (prt, scale)
      type(parton_t), pointer :: prt
      real(default) :: scale
      if (prt%type /= 94) then
         if (scale > D_Min_t + parton_mass_squared (prt)) then
            prt%t = scale
         else
            prt%t = parton_mass_squared (prt)
            call parton_set_simulated (prt)
         end if
      end if
      if (associated (prt%child1)) then
         call set_starting_scale (prt%child1, scale)
      end if
      if (associated (prt%child2)) then
         call set_starting_scale (prt%child2, scale)
      end if
    end subroutine set_starting_scale

  end subroutine shower_add_interaction_2ton_CKKW

  subroutine shower_simulate_no_isr_shower (shower)
    class(shower_t), intent(inout) :: shower
    integer :: i, j
    type(parton_t), pointer :: prt
    do i = 1, size (shower%interactions)
       do j = 1, 2
          prt => shower%interactions(i)%i%partons(j)%p
          if (associated (prt%initial)) then
             !!! for virtuality ordered: remove unneeded partons
             if (associated (prt%parent)) then
                if (.not. parton_is_hadron(prt%parent)) then
                   if (associated (prt%parent%parent)) then
                      if (.not. parton_is_hadron (prt%parent)) then
                         call shower_remove_parton_from_partons &
                              (shower, prt%parent%parent)
                      end if
                   end if
                   call shower_remove_parton_from_partons &
                        (shower, prt%parent)
                end if
             end if
             call parton_set_parent (prt, prt%initial)
             call parton_set_child (prt%initial, prt, 1)
             if (associated (prt%initial%child2)) then
                call shower_remove_parton_from_partons &
                     (shower,prt%initial%child2)
                deallocate (prt%initial%child2)
             end if
             call shower%add_child (prt%initial, 2)
          end if
       end do
    end do
  end subroutine shower_simulate_no_isr_shower

  subroutine shower_simulate_no_fsr_shower (shower)
    class(shower_t), intent(inout) :: shower
    integer :: i, j
    type(parton_t), pointer :: prt
    do i = 1, size (shower%interactions)
       do j = 3, size (shower%interactions(i)%i%partons)
          prt => shower%interactions(i)%i%partons(j)%p
          call parton_set_simulated (prt)
          prt%scale = 0._double
          prt%t = parton_mass_squared (prt)
       end do
    end do
  end subroutine shower_simulate_no_fsr_shower

  subroutine swap_pointers (prtp1, prtp2)
    type(parton_pointer_t), intent(inout) :: prtp1, prtp2
    type(parton_pointer_t) :: prtptemp
    prtptemp%p => prtp1%p
    prtp1%p => prtp2%p
    prtp2%p => prtptemp%p
  end subroutine swap_pointers

  recursive subroutine shower_remove_parton_from_partons (shower, prt)
    type(shower_t), intent(inout) :: shower
    type(parton_t), pointer :: prt
    integer :: i
    if (.not. prt%belongstoFSR .and. associated (prt%child2)) then
       call shower_remove_parton_from_partons_recursive (shower, prt%child2)
    end if
    do i = 1, size (shower%partons)
       if (associated (shower%partons(i)%p, prt)) then
          shower%partons(i)%p => null()
          exit
       end if
       if (i == size (shower%partons)) then
          ! call msg_bug ("Shower core: parton to be removed not found")
       end if
    end do
  end subroutine shower_remove_parton_from_partons

  recursive subroutine shower_remove_parton_from_partons_recursive (shower, prt)
    type(shower_t), intent(inout) :: shower
    type(parton_t), pointer :: prt
    if (associated (prt%child1)) then
       call shower_remove_parton_from_partons_recursive (shower, prt%child1)
       deallocate (prt%child1)
    end if
    if (associated (prt%child2)) then
       call shower_remove_parton_from_partons_recursive (shower, prt%child2)
       deallocate (prt%child2)
    end if
    call shower_remove_parton_from_partons (shower, prt)
  end subroutine shower_remove_parton_from_partons_recursive

  subroutine shower_sort_partons (shower)
    class(shower_t), intent(inout) :: shower
    integer :: i, j, maxsort, size_partons
    logical :: changed
    !!! print *, " shower_sort_partons"
    if (.not. allocated (shower%partons)) return
    size_partons = size (shower%partons)
    maxsort = 0
    do i = 1, size_partons
       if (signal_is_pending ()) return
       if (associated (shower%partons(i)%p))  maxsort = i
    end do
    size_partons = size (shower%partons)
    if (size_partons <= 1) return
    do i = 1, maxsort
       if (signal_is_pending ()) return
       if (.not. associated (shower%partons(i)%p)) cycle
       if (.not. isr_pt_ordered) then
          !!! set unsimulated ISR partons to be "typeless" to prevent
          !!!     influences from "wrong" masses
          if (.not. shower%partons(i)%p%belongstoFSR .and. &
              .not. parton_is_simulated (shower%partons(i)%p) .and. &
              .not. shower%partons(i)%p%belongstointeraction) then
             shower%partons(i)%p%type = 0
          end if
       end if
    end do
    !!! Just a Bubblesort
    !!! Different algorithms needed for t-ordered and pt^2-ordered shower
    !!! Pt-ordered:
    if (isr_pt_ordered) then
       OUTERDO_PT: do i = 1, maxsort - 1
          changed = .false.
          INNERDO_PT: do j = 1, maxsort - i
             if (signal_is_pending ()) return
             if (.not. associated (shower%partons(j + 1)%p)) cycle
             if (.not. associated (shower%partons(j)%p)) then
                !!! change if j + 1 ist assoaciated and j is not
                call swap_pointers (shower%partons(j), shower%partons(j + 1))
                changed = .true.
             else if (shower%partons(j)%p%scale < &
                  shower%partons(j + 1)%p%scale) then
                call swap_pointers (shower%partons(j), shower%partons(j + 1))
                changed = .true.
             else if (shower%partons(j)%p%scale == &
                  shower%partons(j + 1)%p%scale) then
                if (shower%partons(j)%p%nr > shower%partons(j + 1)%p%nr) then
                   call swap_pointers (shower%partons(j), shower%partons(j + 1))
                   changed = .true.
                end if
             end if
          end do INNERDO_PT
          if (.not. changed) exit OUTERDO_PT
       end do outerdo_pt
    !!! |t|-ordered
    else
       OUTERDO_T: do i = 1, maxsort - 1
          changed = .false.
          INNERDO_T: do j = 1, maxsort - i
             if (signal_is_pending ()) return
             if (.not. associated (shower%partons(j + 1)%p)) cycle
             if (.not. associated (shower%partons(j)%p)) then
                !!! change if j+1 ist assoaciated and j isn't
                call swap_pointers (shower%partons(j), shower%partons(j + 1))
                changed = .true.
             else if (.not. shower%partons(j)%p%belongstointeraction .and. &
                  shower%partons(j + 1)%p%belongstointeraction) then
                !!! move partons belonging to the interaction to the front
                call swap_pointers (shower%partons(j), shower%partons(j + 1))
                changed = .true.
             else if (.not. shower%partons(j)%p%belongstointeraction .and. &
                  .not. shower%partons(j + 1)%p%belongstointeraction ) then
                if (abs (shower%partons(j)%p%t) - parton_mass_squared &
                     (shower%partons(j)%p) < &
                     abs(shower%partons(j + 1)%p%t) - parton_mass_squared &
                     (shower%partons(j + 1)%p)) then
                   call swap_pointers (shower%partons(j), shower%partons(j + 1))
                   changed = .true.
                else
                   if (abs (shower%partons(j)%p%t) - parton_mass_squared &
                        (shower%partons(j)%p) == &
                        abs(shower%partons(j + 1)%p%t) - parton_mass_squared &
                        (shower%partons(j + 1)%p)) then
                      if (shower%partons(j)%p%nr > &
                           shower%partons(j + 1)%p%nr) then
                         call swap_pointers (shower%partons(j), &
                              shower%partons(j + 1))
                         changed = .true.
                      end if
                   end if
                end if
             end if
          end do INNERDO_T
          if (.not. changed) exit OUTERDO_T
       end do OUTERDO_T
    end if
    !!! print *, "  shower_sort_partons finished"
  end subroutine shower_sort_partons

  subroutine shower_create (shower, pdf)
    class(shower_t), intent(inout) :: shower
    type(lhapdf_pdf_t), intent(in), target :: pdf
    shower%next_free_nr = 1
    shower%next_color_nr = 1
    if (allocated (shower%interactions)) then
       call msg_bug ("Shower: creating new shower while old one " // &
            "still associated (interactions)")
    end if
    if (allocated (shower%partons)) then
       call msg_bug ("Shower: creating new shower while old one " // &
            "still associated (partons)")
    end if
    treat_light_quarks_massless = .true.
    treat_duscb_quarks_massless = .false.
    shower%valid = .true.
    shower%pdf = pdf
  end subroutine shower_create

  subroutine shower_final (shower)
    class(shower_t), intent(inout) :: shower
    integer :: i
    if (.not. allocated (shower%interactions))  return
    do i = 1, size (shower%interactions)
       if (allocated (shower%interactions(i)%i%partons)) &
            deallocate (shower%interactions(i)%i%partons)
       deallocate (shower%interactions(i)%i)
    end do
    deallocate (shower%interactions)
    deallocate (shower%partons)
    call shower%pdf%final
  end subroutine shower_final

  function shower_get_next_free_nr (shower) result (next_number)
    class(shower_t), intent(inout) :: shower
    integer :: next_number
    next_number = shower%next_free_nr
    shower%next_free_nr = shower%next_free_nr + 1
  end function shower_get_next_free_nr

  subroutine shower_set_next_color_nr (shower, index)
    class(shower_t), intent(inout) :: shower
    integer, intent(in) :: index
    if (index < shower%next_color_nr) then
       call msg_error ("in shower_set_next_color_nr")
    else
       shower%next_color_nr = max(shower%next_color_nr, index)
    end if
  end subroutine shower_set_next_color_nr

  function shower_get_next_color_nr (shower) result (next_color)
    class(shower_t), intent(inout) :: shower
    integer :: next_color
    next_color = shower%next_color_nr
    shower%next_color_nr = shower%next_color_nr + 1
  end function shower_get_next_color_nr

  subroutine shower_enlarge_partons_array (shower, length)
    type(shower_t), intent(inout) :: shower
    integer, intent(in) :: length
    integer :: i, oldlength
    type(parton_pointer_t), dimension(:), allocatable :: new_partons
    !!! print *, "shower_enlarge_partons_array ", length
    if (length > 0) then
       if (allocated (shower%partons)) then
          oldlength = size (shower%partons)
          allocate (new_partons(1:oldlength))
          do i = 1, oldlength
             new_partons(i)%p => shower%partons(i)%p
          end do
          deallocate (shower%partons)
       else
          oldlength = 0
       end if
       allocate (shower%partons(1:oldlength + length))
       do i = 1, oldlength
          shower%partons(i)%p => new_partons(i)%p
       end do
       do i = oldlength + 1, oldlength + length
          shower%partons(i)%p => null()
       end do
    else
       call msg_bug ("Shower: no parton_pointers added in shower%partons")
    end if
    !!! print *, "  shower_enlarge_partons_array finished"
  end subroutine shower_enlarge_partons_array

  subroutine shower_add_child (shower, prt, child)
    class(shower_t), intent(inout) :: shower
    type(parton_t), pointer :: prt
    !!! type(parton_t), intent(inout), target :: prt
    integer, intent(in) :: child
    integer :: i, lastfree
    type(parton_pointer_t) :: newprt
    !!! print *, " shower_add_child for parton ", prt%nr
    if (child /= 1 .and. child /= 2) then
       call msg_bug ("Shower: Adding child in nonexisting place")
    end if
    allocate (newprt%p)
    newprt%p%nr = shower%get_next_free_nr ()
    !!! add new parton as child
    if (child == 1) then
       prt%child1 => newprt%p
    else
       prt%child2 => newprt%p
    end if
    newprt%p%parent => prt
    newprt%p%interactionnr = prt%interactionnr
    !!! add new parton to shower%partons list
    if (associated (shower%partons (size(shower%partons))%p)) then
       call shower_enlarge_partons_array (shower, 10)
    end if
    !!! find last free pointer and let it point to the new parton
    lastfree = 0
    do i = size (shower%partons), 1, -1
       if (.not. associated (shower%partons(i)%p)) then
          lastfree = i
       end if
    end do
    if (lastfree == 0) then
       call msg_bug ("Shower: no free pointers found")
    end if
    shower%partons(lastfree)%p => newprt%p
  end subroutine shower_add_child

  subroutine shower_add_parent (shower, prt)
    class(shower_t), intent(inout) :: shower
    type(parton_t), intent(inout), target :: prt
    integer :: i, lastfree
    type(parton_pointer_t) :: newprt
    !!! print *, " shower_add_parent for parton ", prt%nr
    allocate (newprt%p)
    newprt%p%nr = shower%get_next_free_nr ()
    !!! add new parton as parent
    newprt%p%child1 => prt
    prt%parent => newprt%p
    newprt%p%interactionnr = prt%interactionnr
    !!! add new parton to shower%partons list
    if (.not. allocated (shower%partons)) then
       call shower_enlarge_partons_array (shower, 10)
    else if (associated (shower%partons(size(shower%partons))%p)) then
       call shower_enlarge_partons_array (shower, 10)
    end if
    !!! find last free pointer and let it point to the new parton
    lastfree = 0
    do i = size(shower%partons), 1, -1
       if (.not. associated (shower%partons(i)%p)) then
          lastfree = i
       end if
    end do
    if (lastfree == 0) then
       call msg_bug ("Shower: no free pointers found")
    end if
    shower%partons(lastfree)%p => newprt%p
    ! print *, "  shower_add_parent finished"
  end subroutine shower_add_parent

  function shower_get_total_momentum (shower, c) result (mom)
    type(shower_t), intent(in) :: shower
    integer, intent(in) :: c
    real(default) :: mom
    integer :: i
    mom = zero
    if (.not. allocated (shower%partons)) return
    do i = 1, size (shower%partons)
       if (.not. associated (shower%partons(i)%p)) cycle
       if (parton_is_final (shower%partons(i)%p)) then
          select case (c)
          case (0)
             mom = mom + vector4_get_component(shower%partons(i)%p%momentum, 0)
          case (1)
             mom = mom + vector4_get_component(shower%partons(i)%p%momentum, 1)
          case (2)
             mom = mom + vector4_get_component(shower%partons(i)%p%momentum, 2)
          case (3)
             mom = mom + vector4_get_component(shower%partons(i)%p%momentum, 3)
          case default
             call msg_error ("Shower: wrong component of 4momentum")
          end select
       end if
    end do
  end function shower_get_total_momentum

  function shower_get_nr_of_partons (shower, mine, include_remnants) &
       result (nr)
    type(shower_t), intent(in) :: shower
    real(default), intent(in), optional :: mine
    logical, intent(in), optional :: include_remnants
    integer :: nr
    integer :: i
    type(parton_t), pointer :: prt
    real(default) :: minenergy
    nr = 0
    if (present (mine)) then
       minenergy = mine
    else
       minenergy = zero
    end if
    do i = 1, size (shower%partons)
       prt => shower%partons(i)%p
       if (.not. associated (prt)) cycle
       if (.not. parton_is_final (prt)) cycle
       if (prt%type == 9999) then
          if (present (include_remnants)) then
             if (.not. include_remnants) cycle
          end if
       end if
       if (present(mine)) then
          if (parton_get_energy (prt) > mine) then
             nr = nr + 1
          end if
       else
          nr = nr +1
       end if
    end do
  end function shower_get_nr_of_partons

  function shower_get_nr_of_final_colored_ME_partons (shower) result(nr)
    type(shower_t), intent(in) :: shower
    integer :: nr
    integer :: i, j
    type(parton_t), pointer :: prt
    nr = 0
    do i = 1, size (shower%interactions)
       do j = 1, size (shower%interactions(i)%i%partons)
          prt => shower%interactions(i)%i%partons(j)%p
          if (.not. associated (prt)) cycle
          if (.not. parton_is_colored (prt)) cycle
          if (prt%belongstointeraction .and. prt%belongstoFSR .and. &
               (prt%type /= 94)) then
             nr = nr +1
          end if
       end do
    end do
  end function shower_get_nr_of_final_colored_ME_partons

  subroutine shower_get_final_colored_ME_partons (shower, partons)
    class(shower_t), intent(in) :: shower
    type(parton_pointer_t), dimension(:), allocatable, intent(inout) :: partons
    integer :: i, j, index, s
    type(parton_t), pointer :: prt
    if (allocated(partons))  deallocate(partons)
    s = shower_get_nr_of_final_colored_ME_partons (shower)
    if (s == 0) return
    allocate (partons(1:s))
    index = 0
    do i = 1, size (shower%interactions)
       do j = 1, size (shower%interactions(i)%i%partons)
          prt => shower%interactions(i)%i%partons(j)%p
          if (.not. associated (prt)) cycle
          if (.not. parton_is_colored (prt)) cycle
          if (prt%belongstointeraction .and. prt%belongstoFSR .and. &
               (prt%type /= 94)) then
             index = index + 1
             partons(index)%p => prt
          end if
       end do
    end do
  end subroutine shower_get_final_colored_ME_partons

  subroutine shower_get_final_partons (shower, partons, include_remnants)
    type(shower_t), intent(in) :: shower
    type(parton_pointer_t), dimension(:), allocatable, intent(inout) :: partons
    logical, intent(in), optional :: include_remnants
    integer :: i, j
    type(parton_t), pointer :: prt
    if (allocated (partons))  deallocate (partons)
    allocate (partons(1:shower_get_nr_of_partons(shower, &
         include_remnants = include_remnants)))
    j = 0
    do i = 1, size (shower%partons)
       prt => shower%partons(i)%p
       if (.not. associated (prt)) cycle
       if (.not. parton_is_final (prt)) cycle
       !!! remnant
       if (prt%type == 9999) then
          if (present (include_remnants)) then
             if (.not. include_remnants) cycle
          end if
       end if
       j = j + 1
       partons(j)%p => prt
    end do
  end subroutine shower_get_final_partons

  recursive function interaction_fsr_is_finished_for_parton(prt) result (finished)
    type(parton_t), intent(in) :: prt
    logical :: finished
    if (prt%belongstoFSR) then
       !!! FSR partons
       if (associated (prt%child1)) then
          finished = interaction_fsr_is_finished_for_parton (prt%child1) &
               .and. interaction_fsr_is_finished_for_parton (prt%child2)
       else
          finished = prt%t <= parton_mass_squared(prt)
       end if
    else
       !!! search for emitted timelike partons in ISR shower
       if (.not. associated (prt%initial)) then
          !!! no inital -> no ISR
          finished = .true.
       else if (.not. associated (prt%parent)) then
          finished = .false.
       else
          if (.not. parton_is_hadron (prt%parent)) then
             if (associated (prt%child2)) then
                finished = interaction_fsr_is_finished_for_parton (prt%parent) .and. &
                     interaction_fsr_is_finished_for_parton (prt%child2)
             else
                finished = interaction_fsr_is_finished_for_parton (prt%parent)
             end if
          else
             if (associated (prt%child2)) then
                finished = interaction_fsr_is_finished_for_parton (prt%child2)
             else
                !!! only second partons can come here -> if that happens FSR
                !!!     evolution is not existing
                finished = .true.
             end if
          end if
       end if
    end if
  end function interaction_fsr_is_finished_for_parton

  function interaction_fsr_is_finished (interaction) result (finished)
    type(shower_interaction_t), intent(in) :: interaction
    logical :: finished
    integer :: i
    finished = .true.
    if (.not. allocated (interaction%partons)) return
    do i = 1, size (interaction%partons)
       if (.not. interaction_fsr_is_finished_for_parton &
            (interaction%partons(i)%p)) then
          finished = .false.
          exit
       end if
    end do
  end function interaction_fsr_is_finished

  function shower_interaction_get_shat (interaction) result (shat)
    type(shower_interaction_t), intent(in) :: interaction
    real(default) :: shat
    shat = (interaction%partons(1)%p%momentum + &
            interaction%partons(2)%p%momentum)**2
  end function shower_interaction_get_shat

  function shower_interaction_get_s (interaction) result (s)
    type(shower_interaction_t), intent(in) :: interaction
    real(default) :: s
    s = (interaction%partons(1)%p%initial%momentum + &
         interaction%partons(2)%p%initial%momentum)**2
  end function shower_interaction_get_s

  function shower_fsr_is_finished (shower) result (finished)
    type(shower_t), intent(in) :: shower
    logical :: finished
    integer :: i
    finished = .true.
    if (.not.allocated (shower%interactions)) return
    do i = 1, size(shower%interactions)
       if (.not. interaction_fsr_is_finished (shower%interactions(i)%i)) then
          finished = .false.
          exit
       end if
    end do
  end function shower_fsr_is_finished

  function shower_isr_is_finished (shower) result (finished)
    type(shower_t), intent(in) :: shower
    logical :: finished
    integer :: i
    type(parton_t), pointer :: prt
    finished = .true.
    if (.not.allocated (shower%partons)) return
    do i = 1, size (shower%partons)
       if (.not. associated (shower%partons(i)%p)) cycle
       prt => shower%partons(i)%p
       if (isr_pt_ordered) then
          if (.not. prt%belongstoFSR .and. .not. parton_is_simulated (prt) &
              .and. prt%scale > zero) then
             finished = .false.
             exit
          end if
       else
          if (.not. prt%belongstoFSR .and. .not. parton_is_simulated (prt) &
              .and. prt%t < zero) then
             finished = .false.
             exit
          end if
       end if
    end do
  end function shower_isr_is_finished

  subroutine interaction_find_partons_nearest_to_hadron (interaction, prt1, prt2)
    type(shower_interaction_t), intent(in) :: interaction
    type(parton_t), pointer :: prt1, prt2
    prt1 => null ()
    prt2 => null ()
    prt1 => interaction%partons(1)%p
    do
       if (associated (prt1%parent)) then
          if (parton_is_hadron (prt1%parent)) then
             exit
          else if ((.not. isr_pt_ordered .and. .not. parton_is_simulated (prt1%parent)) &
               .or. (isr_pt_ordered .and. .not. parton_is_simulated (prt1))) then
             exit
          else
             prt1 => prt1%parent
          end if
       else
          exit
       end if
    end do
    prt2 => interaction%partons(2)%p
    do
       if (associated (prt2%parent)) then
          if (parton_is_hadron (prt2%parent)) then
             exit
          else if ((.not. isr_pt_ordered .and. .not. parton_is_simulated (prt2%parent)) &
               .or. (isr_pt_ordered .and. .not. parton_is_simulated (prt2))) then
             exit
          else
             prt2 => prt2%parent
          end if
       else
          exit
       end if
    end do
  end subroutine interaction_find_partons_nearest_to_hadron

  subroutine shower_update_beamremnants (shower)
    class(shower_t), intent(inout) :: shower
    type(parton_t), pointer :: hadron, remnant
    integer :: i
    real(default) :: random
    !!! only proton in first interaction !!?
    !!! currently only first beam-remnant will be updated
    do i = 1,2
       if (associated (shower%interactions(1)%i%partons(i)%p%initial)) then
          hadron => shower%interactions(1)%i%partons(i)%p%initial
       else
          cycle
       end if
       remnant => hadron%child2
       if (associated (remnant)) then
          remnant%momentum = hadron%momentum - hadron%child1%momentum
       end if
       !!! generate flavor of the beam-remnant if beam was proton
       if (abs (hadron%type) == 2212 .and. associated (hadron%child1)) then
          if (parton_is_quark (hadron%child1)) then
             !!! decide if valence (u,d) or sea quark (s,c,b)
             if ((abs (hadron%child1%type) <= 2) .and. &
                  (hadron%type * hadron%child1%type > zero)) then
                !!! valence quark
                if (abs (hadron%child1%type) == 1) then
                   !!! if d then remaining diquark is uu_1
                   remnant%type = sign (2203, hadron%type)
                else
                   call tao_random_number (random)
                   !!! if u then remaining diqaurk is either
                   if (random < 0.75_default) then
                      !!! with 75% a ud_0
                      remnant%type = sign (2101, hadron%type)
                   else
                      !!! with 25% a ud_1
                      remnant%type = sign (2103, hadron%type)
                   end if
                end if
                remnant%c1 = hadron%child1%c2
                remnant%c2 = hadron%child1%c1
             else if ((hadron%type * hadron%child1%type) < zero) then
                !!! antiquark
                if (.not. associated (remnant%child1)) then
                   call shower%add_child (remnant, 1)
                end if
                if (.not. associated (remnant%child2)) then
                   call shower%add_child (remnant, 2)
                end if
                call tao_random_number (random)
                if (random < 0.6666_default) then
                   !!! 2/3 into udq + u
                   if (abs (hadron%child1%type) == 1) then
                      remnant%child1%type = sign (2112, hadron%type)
                   else if (abs (hadron%child1%type) == 2) then
                      remnant%child1%type = sign (2212, hadron%type)
                   else if (abs (hadron%child1%type) == 3) then
                      remnant%child1%type = sign(3212, hadron%type)
                   else if (abs (hadron%child1%type) == 4) then
                      remnant%child1%type = sign (4212, hadron%type)
                   else if (abs (hadron%child1%type) == 5) then
                      remnant%child1%type = sign (5212, hadron%type)
                   end if
                   remnant%child2%type = sign (2, hadron%type)
                else
                   !!! 1/3 into uuq + d
                   if (abs (hadron%child1%type).eq.1) then
                      remnant%child1%type = sign (2212, hadron%type)
                   else if (abs (hadron%child1%type).eq.2) then
                      remnant%child1%type = sign (2224, hadron%type)
                   else if (abs (hadron%child1%type).eq.3) then
                      remnant%child1%type = sign (3222, hadron%type)
                   else if (abs (hadron%child1%type).eq.4) then
                      remnant%child1%type = sign (4222, hadron%type)
                   else if (abs (hadron%child1%type).eq.5) then
                      remnant%child1%type = sign (5222, hadron%type)
                   end if
                   remnant%child2%type = sign (1, hadron%type)
                end if
                remnant%c1 = hadron%child1%c2
                remnant%c2 = hadron%child1%c1
                remnant%child1%c1 = 0
                remnant%child1%c2 = 0
                remnant%child2%c1 = remnant%c1
                remnant%child2%c2 = remnant%c2
             else
                !!! sea quark
                if (.not. associated (remnant%child1)) then
                   call shower%add_child (remnant, 1)
                end if
                if (.not. associated (remnant%child2)) then
                   call shower%add_child (remnant, 2)
                end if
                call tao_random_number (random)
                if (random < 0.5_default) then
                   !!! 1/2 into usbar + ud_0
                   if (abs (hadron%child1%type) == 3) then
                      remnant%child1%type = sign (321, hadron%type)
                   else if (abs (hadron%child1%type) == 4) then
                      remnant%child1%type = sign (421, hadron%type)
                   else if (abs (hadron%child1%type) == 5) then
                      remnant%child1%type = sign (521, hadron%type)
                   end if
                   remnant%child2%type = sign (2101, hadron%type)
                else if (random < 0.6666_default) then
                   !!! 1/6 into usbar + ud_1
                   if (abs (hadron%child1%type) == 3) then
                      remnant%child1%type = sign (321, hadron%type)
                   else if (abs (hadron%child1%type) == 4) then
                      remnant%child1%type = sign (421, hadron%type)
                   else if (abs (hadron%child1%type) == 5) then
                      remnant%child1%type = sign (521, hadron%type)
                   end if
                   remnant%child2%type = sign (2103, hadron%type)
                else
                   !!! 1/3 into dsbar + uu_1
                   if (abs (hadron%child1%type) == 3) then
                      remnant%child1%type = sign (311, hadron%type)
                   else if (abs (hadron%child1%type) == 4) then
                      remnant%child1%type = sign (411, hadron%type)
                   else if (abs (hadron%child1%type) == 5) then
                      remnant%child1%type = sign (511, hadron%type)
                   end if
                   remnant%child2%type = sign (2203, hadron%type)
                end if
                remnant%c1 = hadron%child1%c2
                remnant%c2 = hadron%child1%c1
                remnant%child2%c1 = remnant%c1
                remnant%child2%c2 = remnant%c2
             end if
          else if (parton_is_gluon (hadron%child1)) then
             if (.not.associated (remnant%child1)) then
                call shower%add_child (remnant, 1)
             end if
             if (.not.associated (remnant%child2)) then
                call shower%add_child (remnant, 2)
             end if
             call tao_random_number (random)
             if (random < 0.5_default) then
                !!! 1/2 into u + ud_0
                remnant%child1%type = sign (2, hadron%type)
                remnant%child2%type = sign (2101, hadron%type)
             else if (random < 0.6666_default) then
                !!! 1/6 into u + ud_1
                remnant%child1%type = sign (2, hadron%type)
                remnant%child2%type = sign (2103, hadron%type)
             else
                !!! 1/3 into d + uu_1
                remnant%child1%type = sign (1, hadron%type)
                remnant%child2%type = sign (2203, hadron%type)
             end if
             remnant%c1 = hadron%child1%c2
             remnant%c2 = hadron%child1%c1
             if (sign (1,hadron%type) > 0) then
                remnant%child1%c1 = remnant%c1
                remnant%child2%c2 = remnant%c2
             else
                remnant%child1%c2 = remnant%c2
                remnant%child2%c1 = remnant%c1
             end if
          end if
          if (associated (remnant%child1)) then
             !!! don't care about on-shellness for now
             remnant%child1%momentum = 0.5_default * remnant%momentum
             remnant%child2%momentum = 0.5_default * remnant%momentum
             !!! but care about on-shellness for baryons
             if (mod (remnant%child1%type, 100) >= 10) then
                !!! check if the third quark is set -> meson or baryon
                remnant%child1%t = parton_mass_squared(remnant%child1)
                remnant%child1%momentum = vector4_moving( &
                     energy(remnant%child1%momentum), &
                     ( space_part(remnant%child1%momentum) / &
                     space_part_norm(remnant%child1%momentum) ) *sqrt( &
                     energy(remnant%child1%momentum)**2 - remnant%child1%t))
                remnant%child2%momentum = remnant%momentum &
                     - remnant%child1%momentum
             end if
          end if
       end if
    end do
  end subroutine shower_update_beamremnants

  subroutine interaction_apply_lorentztrafo (interaction, L)
    type(shower_interaction_t), intent(inout) :: interaction
    type(lorentz_transformation_t), intent(in) :: L
    type(parton_t), pointer :: prt
    integer :: i
    !!! ISR part
    do i = 1,2
       prt => interaction%partons(i)%p
       !!! loop over ancestors
       MOTHERS: do
          !!! boost parton
          call parton_apply_lorentztrafo (prt, L)
          if (associated (prt%child2)) then
             !!! boost emitted timelike parton (and daughters)
             call parton_apply_lorentztrafo_recursive (prt%child2, L)
          end if
          if (associated (prt%parent)) then
             if (.not. parton_is_hadron (prt%parent)) then
                prt => prt%parent
             else
                exit
             end if
          else
             exit
          end if
       end do MOTHERS
    end do
    !!! FSR part
    if (associated (interaction%partons(3)%p%parent)) then
       !!! pseudo Parton-Shower histora has been generated -> find
       !!!     mother and go on from there recursively
       prt => interaction%partons(3)%p
       do while (associated (prt%parent))
          prt => prt%parent
       end do
       call parton_apply_lorentztrafo_recursive (prt, L)
    else
       do i = 3, size (interaction%partons)
          call parton_apply_lorentztrafo (interaction%partons(i)%p, L)
       end do
    end if
    !!! print *, " end interaction_apply_lorentztrafo"
  end subroutine interaction_apply_lorentztrafo

  subroutine shower_apply_lorentztrafo (shower, L)
    type(shower_t), intent(inout) :: shower
    type(lorentz_transformation_t), intent(in) :: L
    integer :: i
    do i = 1, size (shower%interactions)
       call interaction_apply_lorentztrafo (shower%interactions(i)%i, L)
    end do
  end subroutine shower_apply_lorentztrafo

  subroutine interaction_boost_to_CMframe (interaction)
    type(shower_interaction_t), intent(inout) :: interaction
    type(vector4_t) :: beta
    type(parton_t), pointer :: prt1, prt2
    call interaction_find_partons_nearest_to_hadron (interaction, prt1, prt2)
    beta = prt1%momentum + prt2%momentum
    beta = beta / vector4_get_component(beta,0)
    if (beta**2 > one) then
       call msg_error ("Shower, boost to CM frame: beta > 1")
       return
    end if
    if (space_part(beta)**2 > 1E-14_default) then
       call interaction_apply_lorentztrafo(interaction, &
            boost(space_part(beta)**1 / &
              sqrt (one - space_part(beta)**2), -direction(beta)))
    end if
  end subroutine interaction_boost_to_CMframe

  subroutine shower_boost_to_CMframe (shower)
    type(shower_t), intent(inout) :: shower
    integer :: i
    do i = 1, size (shower%interactions)
       call interaction_boost_to_CMframe (shower%interactions(i)%i)
    end do
    call shower%update_beamremnants ()
  end subroutine shower_boost_to_CMframe

  subroutine shower_boost_to_labframe (shower)
    class(shower_t), intent(inout) :: shower
    integer :: i
    do i = 1, size (shower%interactions)
       call interaction_boost_to_labframe (shower%interactions(i)%i)
    end do
  end subroutine shower_boost_to_labframe

  subroutine interaction_boost_to_labframe (interaction)
    type(shower_interaction_t), intent(inout) :: interaction
    type(parton_t), pointer :: prt1, prt2
    type(vector3_t) :: beta
    call interaction_find_partons_nearest_to_hadron (interaction, prt1, prt2)
    if (.not. associated (prt1%initial) .or. .not. associated (prt2%initial)) then
       return
    end if
    !!! if no branching at all occured, take original partons
    ! if (prt1%child1%belongstointeraction .and. prt2%child1%belongstointeraction) then
    !    prt1 => prt1%child1
    !    prt2 => prt2%child1
    ! end if
    !!! transform partons to overall labframe.
    beta = vector3_canonical(3) * &
         ((prt1%x * vector4_get_component (prt2%momentum, 0) - &
           prt2%x * vector4_get_component (prt1%momentum, 0)) / &
          (prt1%x * vector4_get_component (prt2%momentum, 3) - &
           prt2%x * vector4_get_component (prt1%momentum, 3)))
    if (beta**1 > 1E-10_default) &
         call interaction_apply_lorentztrafo (interaction, &
             boost (beta**1 / sqrt(one - beta**2), -direction(beta)))
  end subroutine interaction_boost_to_labframe

  subroutine interaction_rotate_to_z (interaction)
    type(shower_interaction_t), intent(inout) :: interaction
    type(parton_t), pointer :: prt1, prt2
    call interaction_find_partons_nearest_to_hadron (interaction, prt1, prt2)
    if (associated (prt1%initial)) then
       call interaction_apply_lorentztrafo (interaction, &
            rotation_to_2nd (space_part (prt1%momentum), &
            vector3_canonical(3) * sign (one, &
            vector4_get_component(prt1%initial%momentum,3))))
    end if
  end subroutine interaction_rotate_to_z

  subroutine shower_rotate_to_z (shower)
    type(shower_t), intent(inout) :: shower
    integer :: i
    do i = 1, size (shower%interactions)
       call interaction_rotate_to_z (shower%interactions(i)%i)
    end do
    call shower%update_beamremnants ()
  end subroutine shower_rotate_to_z

  subroutine interaction_generate_primordial_kt (interaction)
    type(shower_interaction_t), intent(inout) :: interaction
    type(parton_t), pointer :: had1, had2
    type(vector4_t) :: momenta(2)
    type(vector3_t) :: beta
    real(default) :: pt (2), phi(2)
    real(default) :: shat
    !!! variables for boosting and rotating
    real(default) :: btheta, bphi
    integer :: i
    if (primordial_kt_width == zero)  return
    !!! print *, "interaction_generate_primordial_kt"
    !!! Return if there are no initials, electron-hadron collision not implemented
    if (.not. associated (interaction%partons(1)%p%initial) .or. &
        .not. associated (interaction%partons(2)%p%initial)) then
       return
    end if
    had1 => interaction%partons(1)%p%initial
    had2 => interaction%partons(2)%p%initial
    !!! copy momenta and energy
    momenta(1) = had1%child1%momentum
    momenta(2) = had2%child1%momentum
    GENERATE_PT_PHI: do i = 1, 2
       !!! generate transverse momentum and phi
       GENERATE_PT: do
          call tao_random_number (pt (i))
          pt(i) = primordial_kt_width * sqrt(-log(pt(i)))
          if (pt(i) < primordial_kt_cutoff) exit
       end do GENERATE_PT
       call tao_random_number (phi (i))
       phi(i) = twopi * phi(i)
    end do GENERATE_PT_PHI
    !!! adjust momenta
    shat = (momenta(1) + momenta(2))**2
    momenta(1) = vector4_moving (vector4_get_component (momenta(1),0), &
         vector3_moving ([pt(1) * cos(phi(1)), pt(1) * sin(phi(1)), &
         vector4_get_component (momenta(1), 3)]))
    momenta(2) = vector4_moving (vector4_get_component (momenta(2),0), &
         vector3_moving ([pt(2) * cos(phi(2)), pt(2) * sin(phi(2)), &
         vector4_get_component (momenta(2), 3)]))
    beta = vector3_moving ([vector4_get_component (momenta(1), 1) + &
         vector4_get_component (momenta(2), 1) , &
         vector4_get_component (momenta(1), 2) + &
         vector4_get_component (momenta(2), 2), zero]) / sqrt(shat)
    momenta(1) = boost (beta**1 / sqrt(one - beta**2), -direction(beta)) * momenta(1)
    bphi = azimuthal_angle (momenta(1))
    btheta = polar_angle (momenta(1))
    call interaction_apply_lorentztrafo (interaction, &
         rotation (cos(bphi), sin(bphi), 3) * rotation(cos(btheta), &
         sin(btheta), 2) * rotation(cos(-bphi), sin(-bphi), 3))
    call interaction_apply_lorentztrafo (interaction, &
         boost (beta**1 / sqrt(one - beta**2), -direction(beta)))
  end subroutine interaction_generate_primordial_kt

  subroutine shower_generate_primordial_kt (shower)
    class(shower_t), intent(inout) :: shower
    integer :: i
    ! print *, "shower_generate_primordial_kt"
    ! call shower%write ()
    do i = 1, size (shower%interactions)
       call interaction_generate_primordial_kt (shower%interactions(i)%i)
    end do
    call shower%update_beamremnants ()
    ! call shower%write ()
  end subroutine shower_generate_primordial_kt

  subroutine interaction_write (interaction, unit)
    type(shower_interaction_t), intent(in) :: interaction
    integer, intent(in), optional :: unit
    integer :: i, u
    u = given_output_unit (unit); if (u < 0) return
    if (associated (interaction%partons(1)%p)) then
       if (associated (interaction%partons(1)%p%initial)) &
            call parton_write (interaction%partons(1)%p%initial, u)
    end if
    if (associated (interaction%partons(2)%p)) then
       if (associated (interaction%partons(2)%p%initial)) &
            call parton_write (interaction%partons(2)%p%initial, u)
    end if
    if (allocated (interaction%partons)) then
       do i = 1, size (interaction%partons)
          call parton_write (interaction%partons(i)%p, u)
       end do
    end if
    write (u, "(A)")
  end subroutine interaction_write

  subroutine shower_write (shower, unit)
    class(shower_t), intent(in) :: shower
    integer, intent(in), optional :: unit
    integer :: i, u
    u = given_output_unit (unit); if (u < 0) return
    if (size (shower%interactions) > 0) then
       write (u, "(3x,A)") "Interactions: "
       do i = 1, size (shower%interactions)
          write (u, "(4x,A,I0)") "Interaction number ", i
          if (.not. associated (shower%interactions(i)%i)) then
             call msg_fatal ("Shower: missing interaction in shower")
          end if
          call interaction_write (shower%interactions(i)%i, u)
       end do
    else
       write (u, "(3x,A)") "[no interactions in shower]"
    end if
    write (u, "(A)")
    if (allocated (shower%partons)) then
       write (u, "(5x,A)")  "Partons:"
       do i = 1, size (shower%partons)
          ! print *, " i = ", i
          if (associated (shower%partons(i)%p)) then
             call parton_write(shower%partons(i)%p, u)
             if (i < size (shower%partons)) then
                if (associated (shower%partons(i + 1)%p)) then
                   if (shower%partons(i)%p%belongstointeraction .and. &
                        .not. shower%partons(i + 1)%p%belongstointeraction) then
                      write (u, "(A)") &
                           "-------------------------------------------------------"
                      ! stop "END"
                   end if
                end if
             end if
          end if
       end do
    else
       write (u, "(5x,A)")  "[no partons in shower]"
    end if
    write (u, "(4x,A,4(ES12.5,A))")  "Total Momentum [0:3]: ", &
         shower_get_total_momentum (shower, 0), TAB, &
         shower_get_total_momentum (shower, 1), TAB, &
         shower_get_total_momentum (shower, 2), TAB, &
         shower_get_total_momentum (shower, 3), TAB
    write (u, "(1x,A,L1)") "ISR finished: ", shower_isr_is_finished (shower)
    write (u, "(1x,A,L1)") "FSR finished: ", shower_fsr_is_finished (shower)
  end subroutine shower_write

  subroutine shower_write_lhef (shower, unit)
    class(shower_t), intent(in) :: shower
    integer, intent(in), optional :: unit
    integer :: u
    integer :: i
    integer :: c1, c2
    u = given_output_unit (unit);  if (u < 0)  return
    write(u,'(A)') '<LesHouchesEvents version="1.0">'
    write(u,'(A)') '<-- not a complete lhe file - just one event -->'
    write(u,'(A)') '<event>'
    write(u, *) 2 + shower_get_nr_of_partons (shower), 1, 1.0, 1.0, 1.0, 1.0
    !!! write incoming partons
    do i = 1, 2
       if (abs (shower%partons(i)%p%type) < 1000) then
          c1 = 0
          c2 = 0
          if (parton_is_colored (shower%partons(i)%p)) then
             if (shower%partons(i)%p%c1 /= 0) c1 = 500 + shower%partons(i)%p%c1
             if (shower%partons(i)%p%c2 /= 0) c2 = 500 + shower%partons(i)%p%c2
          end if
          write(u,*) shower%partons(i)%p%type, -1, 0, 0, c1, c2, &
               vector4_get_component (shower%partons(i)%p%momentum, 1), &
               vector4_get_component (shower%partons(i)%p%momentum, 2), &
               vector4_get_component (shower%partons(i)%p%momentum, 3), &
               vector4_get_component (shower%partons(i)%p%momentum, 0), &
               shower%partons(i)%p%momentum**2, zero, 9.0
       else
          write(u,*) shower%partons(i)%p%type, -9, 0, 0, 0, 0, &
               vector4_get_component (shower%partons(i)%p%momentum, 1), &
               vector4_get_component (shower%partons(i)%p%momentum, 2), &
               vector4_get_component (shower%partons(i)%p%momentum, 3), &
               vector4_get_component (shower%partons(i)%p%momentum, 0), &
               shower%partons(i)%p%momentum**2, zero, 9.0
       end if
    end do
    !!! write outgoing partons
    do i = 3, size (shower%partons)
       if (.not. associated (shower%partons(i)%p)) cycle
       if (.not. parton_is_final (shower%partons(i)%p)) cycle
       c1 = 0
       c2 = 0
       if (parton_is_colored (shower%partons(i)%p)) then
          if (shower%partons(i)%p%c1 .ne. 0) c1 = 500 + shower%partons(i)%p%c1
          if (shower%partons(i)%p%c2 .ne. 0) c2 = 500 + shower%partons(i)%p%c2
       end if
       write(u,*) shower%partons(i)%p%type, 1, 1, 2, c1, c2, &
            vector4_get_component (shower%partons(i)%p%momentum, 1), &
            vector4_get_component (shower%partons(i)%p%momentum, 2), &
            vector4_get_component (shower%partons(i)%p%momentum, 3), &
            vector4_get_component (shower%partons(i)%p%momentum, 0), &
            shower%partons(i)%p%momentum**2, zero, 9.0
    end do
    write(u,'(A)') '</event>'
    write(u,'(A)') '</LesHouchesEvents>'
  end subroutine shower_write_lhef

  subroutine shower_replace_parent_by_hadron (shower, prt)
    type(shower_t), intent(inout) :: shower
    type(parton_t), intent(inout), target :: prt
    type(parton_t), pointer :: remnant => null()
    ! print *, " shower_replace_parent_by_hadron for parton ", prt%nr
    if (associated (prt%parent)) then
       call shower_remove_parton_from_partons (shower, prt%parent)
       deallocate (prt%parent)
    end if
    if (.not. associated (prt%initial%child2)) then
       call shower%add_child (prt%initial, 2)
    end if
    prt%parent => prt%initial
    prt%parent%child1 => prt
    ! make other child to be a beam-remnant
    remnant => prt%initial%child2
    remnant%type = 9999
    remnant%momentum = prt%parent%momentum - prt%momentum
    remnant%x = one - prt%x
    remnant%parent => prt%initial
    remnant%t = zero
  end subroutine shower_replace_parent_by_hadron

  subroutine shower_get_first_ISR_scale_for_parton (shower, prt, tmax)
    type(shower_t), intent(inout) :: shower
    type(parton_t), intent(inout), target :: prt
    real(default), intent(in), optional :: tmax
    real(default) :: t, tstep, random, integral, temp1
    real(default) :: temprand
    if (present(tmax)) then
       t = max (max (-tscalefactor_isr * parton_get_energy (prt)**2, &
            -abs(tmax)), prt%t)
    else
       t = max (-tscalefactor_isr * parton_get_energy (prt)**2, prt%t)
    end if
    call tao_random_number (random)
    random = -twopi * log(random)
    !!! compare Integral and log(random) instead of random and exp(-Integral)
    random = random / first_integral_suppression_factor
    integral = zero
    call parton_set_simulated (prt, .false.)
    do
       call tao_random_number (temprand)
       tstep = max (abs (0.01_default * t) * temprand, 0.1_default * D_Min_t)
       if (t + 0.5_default * tstep > - D_Min_t) then
          prt%t = parton_mass_squared (prt)
          call parton_set_simulated (prt)
          exit
       end if
       prt%t = t + 0.5_default * tstep
       temp1 = integral_over_z_simple (prt, (random - integral) / tstep)
       integral = integral + tstep * temp1
       if (integral > random) then
          prt%t = t + 0.5_default * tstep
          exit
       end if
       t = t + tstep
    end do
    if (prt%t > - D_Min_t) then
       call shower_replace_parent_by_hadron (shower, prt)
    end if
    ! print *, "  shower_get_first_ISR_scale_for_parton finished"

  contains

    function integral_over_z_simple (prt, final) result (integral)
      type(parton_t), intent(inout) :: prt
      ! real(default), intent(in) :: shat, s,
      real(default), intent(in) :: final
      real(default) :: integral

      real(default), parameter :: zstepfactor = one
      real(default), parameter :: zstepmin = 0.0001_default
      real(default) :: z, zstep, minz, maxz
      real(default) :: pdfsum
      integer :: quark

      integral = zero
      if (D_print) then
         print *, "integral_over_z_simple for t = ", prt%t
      end if
      minz = prt%x
      ! maxz = maxzz(shat, s)
      maxz = maxz_isr
      z = minz
      !!! TODO -> Adapt zstep to structure of divergencies
      if (parton_is_gluon (prt%child1)) then
         !!! gluon coming from g->gg
         do
            call tao_random_number (temprand)
            zstep = max(zstepmin, temprand * zstepfactor * z * (one - z))
            zstep = min(zstep, maxz - z)
            integral = integral + zstep * (D_alpha_s_isr ((one - &
                 (z + 0.5_default * zstep)) * abs(prt%t)) / (abs(prt%t))) * &
                 P_ggg (z + 0.5_default * zstep) * shower%get_pdf (prt%initial%type, &
                 prt%x / (z + 0.5_default * zstep), abs(prt%t), 21)
            if (integral > final) then
               exit
            end if
            z = z + zstep
            if (z >= maxz) then
               exit
            end if
         end do
         !!! gluon coming from q->qg  ! correctly implemented yet?
         if (integral < final) then
            z = minz
            do
               call tao_random_number (temprand)
               zstep = max(zstepmin, temprand * zstepfactor * z * (one - z))
               zstep = min(zstep, maxz - z)
               pdfsum = zero
               do quark = -D_Nf, D_Nf
                  if (quark == 0) cycle
                  pdfsum = pdfsum + shower%get_pdf (prt%initial%type, &
                       prt%x / (z + 0.5_default * zstep), abs(prt%t), quark)
               end do
               integral = integral + zstep * (D_alpha_s_isr &
                    ((z + 0.5_default * zstep) * abs(prt%t)) / (abs(prt%t))) * &
                        P_qqg (one - (z + 0.5_default * zstep)) * pdfsum
               if (integral > final) then
                  exit
               end if
               z = z + zstep
               if (z >= maxz) then
                  exit
               end if
            end do
         end if
      else if (parton_is_quark (prt%child1)) then
         !!! quark coming from q->qg
         do
            call tao_random_number(temprand)
            zstep = max(zstepmin, temprand * zstepfactor * z * (one - z))
            zstep = min(zstep, maxz - z)
            integral = integral + zstep * (D_alpha_s_isr ((one - &
                 (z + 0.5_default * zstep)) * abs(prt%t)) / (abs(prt%t))) * &
                 P_qqg (z + 0.5_default * zstep) * shower%get_pdf (prt%initial%type, &
                 prt%x / (z + 0.5_default * zstep), abs(prt%t), prt%type)
            if (integral > final) then
               exit
            end if
            z = z + zstep
            if (z >= maxz) then
               exit
            end if
         end do
         !!! quark coming from g->qqbar
         if (integral < final) then
            z = minz
            do
               call tao_random_number (temprand)
               zstep = max(zstepmin, temprand * zstepfactor * z*(one - z))
               zstep = min(zstep, maxz - z)
               integral = integral + zstep * (D_alpha_s_isr &
                    ((one - (z + 0.5_default * zstep)) * abs(prt%t)) / (abs(prt%t))) * &
                    P_gqq (z + 0.5_default * zstep) * shower%get_pdf (prt%initial%type, &
                    prt%x / (z + 0.5_default * zstep), abs(prt%t), 21)
               if (integral > final) then
                  exit
               end if
               z = z + zstep
               if (z >= maxz) then
                  exit
               end if
            end do
         end if

      end if
      integral = integral / shower%get_pdf (prt%initial%type, prt%x, &
           abs(prt%t), prt%type)
    end function integral_over_z_simple

  end subroutine shower_get_first_ISR_scale_for_parton

  subroutine shower_prepare_for_simulate_isr_pt (shower, interaction)
    type(shower_t), intent(inout) :: shower
    type(shower_interaction_t), intent(inout) :: interaction
    real(default) :: s
    ! print *, " shower_prepare_for_simulate_isr_pt"
    !!! get sqrts of interaction
    s = (interaction%partons(1)%p%momentum + &
         interaction%partons(2)%p%momentum)**2
    interaction%partons(1)%p%scale = tscalefactor_isr * 0.25_default * s
    interaction%partons(2)%p%scale = tscalefactor_isr * 0.25_default * s
    ! call shower%add_parent (interaction%partons(1)%p)
    ! call shower$add_parent (interaction%partons(2)%p)
    !
    ! interaction%partons(1)%p%parent%scale = 0.5_default * sqrts
    ! interaction%partons(1)%p%parent%momentum = &
    !    interaction%partons(1)%p%momentum
    ! interaction%partons(1)%p%parent%belongstoFSR = .false.
    ! interaction%partons(1)%p%parent%initial => &
    !    interaction%partons(1)%p%initial
    ! interaction%partons(2)%p%parent%scale = 0.5_default * sqrts
    ! interaction%partons(2)%p%parent%momentum = &
    !     interaction%partons(2)%p%momentum
    ! interaction%partons(2)%p%parent%belongstoFSR = .false.
    ! interaction%partons(2)%p%parent%initial => &
    !    interaction%partons(2)%p%initial
    !
    ! call shower%add_child (interaction%partons(1)%p%parent, 2)
    ! call shower%add_child (interaction%partons(2)%p%parent, 2)
    ! print *, " shower_prepare_for_simulate_isr_pt finished"
  end subroutine shower_prepare_for_simulate_isr_pt

  subroutine shower_prepare_for_simulate_isr_ana_test (shower, prt1, prt2)
    type(shower_t), intent(inout) :: shower
    type(parton_t), intent(inout), target :: prt1, prt2
    type(parton_t), pointer :: prt, prta, prtb
    real(default) ::  pini(0:3), scale, factor
    integer :: i
    ! print *, " shower_prepare_for_simulate_isr_ana"
    if (.not. associated (prt1%initial) .or. .not. associated (prt2%initial)) then
       return
    end if
    do i = 0, 3
       pini(i) = parton_get_momentum (prt1,i) + parton_get_momentum (prt2,i)
    end do
    scale = - (pini(0)**2 - pini(1)**2 - pini(2)**2 - pini(3)**2)
    call parton_set_simulated (prt1)
    call parton_set_simulated (prt2)
    call shower%add_parent (prt1)
    call shower%add_parent (prt2)
    factor = sqrt (vector4_get_component (prt1%momentum, 0)**2 - scale) / &
         space_part_norm(prt1%momentum)
    prt1%parent%type = prt1%type
    prt1%parent%z = 1._double
    prt1%parent%momentum = prt1%momentum
    prt1%parent%t = scale
    prt1%parent%x = prt1%x
    prt1%parent%initial => prt1%initial
    prt1%parent%belongstoFSR = .false.
    prt1%parent%c1 = prt1%c1
    prt1%parent%c2 = prt1%c2

    prt2%parent%type= prt2%type
    prt2%parent%z = 1._double
    prt2%parent%momentum = prt2%momentum
    prt2%parent%t = scale
    prt2%parent%x = prt2%x
    prt2%parent%initial => prt2%initial
    prt2%parent%belongstoFSR = .false.
    prt2%parent%c1 = prt2%c1
    prt2%parent%c2 = prt2%c2

    do
       call shower_get_first_ISR_scale_for_parton (shower, prt1%parent)
       call shower_get_first_ISR_scale_for_parton (shower, prt2%parent)

       !!! redistribute energy among first partons
       prta => prt1%parent
       prtb => prt2%parent

       do i = 0, 3
          pini(i) = parton_get_momentum (prt1,i) + parton_get_momentum (prt2,i)
       end do
       call parton_set_energy (prta, (pini(0)**2 - prtb%t + prta%t) &
            / (two * pini(0)))
       call parton_set_energy (prtb, pini(0) &
            - vector4_get_component (prta%momentum, 0))

       ! if (abs( (prt1%parent%x * vector4_get_component(prt2%parent%momentum, 0)-&
       !     prt2%parent%x * vector4_get_component(prt1%parent%momentum, 0))/ &
       !     (prt1%parent%x * vector4_get_component(prt2%parent%momentum, 3)-&
       !     prt2%parent%x * vector4_get_component(prt1%parent%momentum, 3)) ).gt. 1._double) then
       !         print *, "cycle", abs( (prt1%parent%x * vector4_get_component(prt2%parent%momentum, 0)-&
       !       prt2%parent%x * vector4_get_component(prt1%parent%momentum, 0))/ &
       !       (prt1%parent%x * vector4_get_component(prt2%parent%momentum, 3)-&
       !       prt2%parent%x * vector4_get_component(prt1%parent%momentum, 3)) )
       !     cycle
       !  else
       exit
       ! end if
    end do
    !      print *, "BETA+", (prt1%parent%x * vector4_get_component(prt2%parent%momentum, 0)-&
    !           prt2%parent%x * vector4_get_component(prt1%parent%momentum, 0))/ &
    !           (prt1%parent%x * vector4_get_component(prt2%parent%momentum, 3)-&
    !           prt2%parent%x * vector4_get_component(prt1%parent%momentum, 3))
    !!! TODO check the code above
    call parton_set_simulated (prt1%parent)
    call parton_set_simulated (prt2%parent)
    !!! rescale momenta
    do i = 1, 2
       if (i == 1) then
          prt => prt1%parent
       else
          prt => prt2%parent
       end if
       factor = sqrt (vector4_get_component (prt%momentum,0)**2 - prt%t) &
            / space_part_norm (prt%momentum)
       prt%momentum = vector4_moving (vector4_get_component (prt%momentum, 0), &
            factor * space_part (prt%momentum))
    end do

    if (prt1%parent%t < 0._double) then
       call shower%add_parent (prt1%parent)
       prt1%parent%parent%momentum = prt1%parent%momentum
       prt1%parent%parent%t = prt1%parent%t
       prt1%parent%parent%x = prt1%parent%x
       prt1%parent%parent%initial => prt1%parent%initial
       prt1%parent%parent%belongstoFSR = .false.
       call shower%add_child (prt1%parent%parent, 2)
    end if

    if (prt2%parent%t < 0._double) then
       call shower%add_parent (prt2%parent)
       prt2%parent%parent%momentum = prt2%parent%momentum
       prt2%parent%parent%t = prt2%parent%t
       prt2%parent%parent%x = prt2%parent%x
       prt2%parent%parent%initial => prt2%parent%initial
       prt2%parent%parent%belongstoFSR = .false.
       call shower%add_child (prt2%parent%parent, 2)
    end if
    ! print *, "after isr_ana_test"
    ! call shower%write ()
    ! print *, "BETA:"
    ! call vector3_write(vector3_canonical(3) * &
    !      ( (prt1%parent%x * vector4_get_component(prt2%parent%momentum, 0)-&
    !      prt2%parent%x * vector4_get_component(prt1%parent%momentum, 0))/ &
    !        (prt1%parent%x * vector4_get_component(prt2%parent%momentum, 3)-&
    !        prt2%parent%x * vector4_get_component(prt1%parent%momentum, 3)) ) )

    ! print *, "after isr_ana_test finished"

  end subroutine shower_prepare_for_simulate_isr_ana_test

  !! subroutine shower_prepare_for_simulate_fsr_ana (shower, prt1, prt2)
  !!   type(shower_t), intent(inout) :: shower
  !!   type(parton_t), pointer :: prt1, prt2
  !!   real(default) ::  pini(4)
  !!   integer i
  !!
  !!   ! print *, "shower_prepare_for_simulate_fsr_ana"
  !!
  !!   !!! Define imagined single initiator of shower
  !!   call shower%add_child (prt1, 1)
  !!   do i = 1, 4
  !!      pini(i) = parton_get_momentum (prt1,i - 1) + parton_get_momentum (prt2, i - 1)
  !!   end do
  !!   call parton_set_simulated (prt1)
  !!   call parton_set_child (prt1, prt1%child1, 1)
  !!   call parton_set_child (prt1, prt1%child1, 2)
  !!   call parton_set_simulated (prt2)
  !!   call parton_set_child (prt2, prt1%child1, 1)
  !!   call parton_set_child (prt2, prt1%child1, 2)
  !!
  !!   prt1%child1%type = 94
  !!   prt1%child1%z = parton_get_energy (prt1) / &
  !!        (parton_get_energy (prt1) + parton_get_energy (prt2))
  !!   call parton_set_simulated (prt1%child1)
  !!   call parton_set_parent (prt1%child1, prt1)
  !!   call parton_set_momentum (prt1%child1, pini(1), pini(2), pini(3), pini(4))
  !!   prt1%child1%t = parton_p4square(prt1%child1)
  !!   prt1%child1%costheta = -one
  !!
  !!   call shower%add_child (prt1%child1, 1)
  !!   call shower%add_child (prt1%child1, 2)
  !!
  !!   prt1%child1%child1%typ = prt1%typ
  !!   prt1%child1%child1%momentum = prt1%momentum
  !!   prt1%child1%child1%t = prt1%child1%t
  !!   call parton_set_parent (prt1%child1%child1, prt1%child1)
  !!   prt1%child1%child1%c1 = prt1%c1
  !!   prt1%child1%child1%c2 = prt1%c2
  !!
  !!   prt1%child1%child2%typ = prt2%typ
  !!   prt1%child1%child2%momentum = prt2%momentum
  !!   prt1%child1%child2%t = prt2%child1%t
  !!   call parton_set_parent (prt1%child1%child2, prt1%child1)
  !!   prt1%child1%child2%c1 = prt2%c1
  !!   prt1%child1%child2%c2 = prt2%c2
  !!
  !!   ! print *, "  shower_prepare_for_simulate_fsr_ana finished"
  !! end subroutine shower_prepare_for_simulate_fsr_ana

  subroutine shower_add_children_of_emitted_timelike_parton (shower, prt)
    type(shower_t), intent(inout) :: shower
    type(parton_t), pointer :: prt

    if (prt%t > parton_mass_squared (prt) + D_Min_t) then
       if (parton_is_quark (prt)) then
          !!! q -> qg
          call shower%add_child (prt, 1)
          prt%child1%type = prt%type
          call parton_set_energy (prt%child1, prt%z * parton_get_energy (prt))
          prt%child1%t = prt%t
          call shower%add_child (prt, 2)
          prt%child2%type = 21
          call parton_set_energy (prt%child2, (one - prt%z) * &
               parton_get_energy (prt))
          prt%child2%t = prt%t
       else
          if (int (prt%x) > 0) then
             call shower%add_child (prt, 1)
             prt%child1%type = int (prt%x)
             call parton_set_energy (prt%child1, prt%z * parton_get_energy (prt))
             prt%child1%t = prt%t
             call shower%add_child (prt, 2)
             prt%child2%type = -int (prt%x)
             call parton_set_energy (prt%child2, (one - prt%z) * &
                  parton_get_energy (prt))
             prt%child2%t= prt%t
          else
             call shower%add_child (prt, 1)
             prt%child1%type = 21
             call parton_set_energy (prt%child1, prt%z * parton_get_energy (prt))
             prt%child1%t = prt%t
             call shower%add_child (prt, 2)
             prt%child2%type = 21
             call parton_set_energy (prt%child2, (one - prt%z) * &
                  parton_get_energy (prt))
             prt%child2%t = prt%t
          end if
       end if
    end if
  end subroutine shower_add_children_of_emitted_timelike_parton

  subroutine shower_simulate_children_ana (shower,prt)
    type(shower_t), intent(inout) :: shower
    type(parton_t), intent(inout) :: prt
    real(default), dimension(1:2) :: t, random, integral
    integer, dimension(1:2) :: gtoqq
    integer :: daughter
    type(parton_t), pointer :: daughterprt
    integer :: n_loop

    if (signal_is_pending ()) return
    gtoqq = 0

    if (D_print) print *, "shower_simulate_children_ana for parton " , prt%nr

    if (.not. associated (prt%child1) .or. .not. associated (prt%child2)) then
       call msg_error ("Shower: error in simulate_children_ana: no children.")
       return
    end if

    if (abs (prt%type) >= 90 .and. abs (prt%type) <= 93) then
       !!! prt is beam-remnant
       call parton_set_simulated (prt)
       return
    end if

    !!! check if partons are "internal" -> fixed scale
    if (prt%child1%type == 94) then
       call parton_set_simulated (prt%child1)
    end if
    if (prt%child2%type == 94) then
       call parton_set_simulated (prt%child2)
    end if

    integral = zero

    !!! impose constraints by angular ordering -> cf. (26) of Gaining analytic control
    !!! check if no branchings are possible
    if (.not. prt%child1%simulated) then
       prt%child1%t = min (prt%child1%t, &
            0.5_default * parton_get_energy (prt%child1)**2 * (one - &
            parton_get_costheta (prt)))
       if (min (prt%child1%t, parton_get_energy (prt%child1)**2) < &
            parton_mass_squared (prt%child1) + D_Min_t) then
          prt%child1%t = parton_mass_squared (prt%child1)
          call parton_set_simulated (prt%child1)
       end if
    end if
    if (.not. prt%child2%simulated) then
       prt%child2%t = min (prt%child2%t, &
            0.5_default * parton_get_energy (prt%child2)**2 * (one - &
            parton_get_costheta (prt)))
       if (min (prt%child2%t, parton_get_energy (prt%child2)**2) < &
            parton_mass_squared (prt%child2) + D_Min_t) then
          prt%child2%t = parton_mass_squared (prt%child2)
          call parton_set_simulated (prt%child2)
       end if
    end if

    call tao_random_number (random)

    n_loop = 0
    do
       if (signal_is_pending ()) return
       n_loop = n_loop + 1
       if (n_loop > 900) then
          !!! try with massless quarks
          treat_duscb_quarks_massless = .true.
       end if
       if (n_loop > 1000) then
          call shower%write ()
          call msg_message (" simulate_children_ana failed for parton ", prt%nr)
          call msg_error ("BUG: too many loops in simulate_children_ana (?)")
          shower%valid = .false.
          return
       end if

       t(1) = prt%child1%t
       t(2) = prt%child2%t

       !!! check if a branching in the range t(i) to t(i) - tstep(i) occurs

       !!! check for child1
       if (.not. parton_is_simulated (prt%child1)) then
          call parton_simulate_stept &
               (prt%child1, integral(1), random(1), gtoqq(1))
       end if
       !!! check for child2
       if (.not. parton_is_simulated(prt%child2)) then
          call parton_simulate_stept &
               (prt%child2, integral(2), random(2), gtoqq(2))
       end if

       if (parton_is_simulated (prt%child1) .and. &
           parton_is_simulated (prt%child2)) then
          if (sqrt (prt%t) <= sqrt (prt%child1%t) + sqrt (prt%child2%t)) then
            !!! virtuality : t - m**2 (assuming it's not fixed)
            if (prt%child1%type == 94 .and. prt%child2%type == 94) then
               call msg_fatal &
                    ("Shower: both partons fixed, but momentum not conserved")
            else if (prt%child1%type == 94) then
               !!! reset child2
               call parton_set_simulated (prt%child2, .false.)
               prt%child2%t = min (prt%child1%t, (sqrt (prt%t) - &
                    sqrt (prt%child1%t))**2)
               integral(2) = zero
               call tao_random_number (random(2))
            else if (prt%child2%type.eq.94) then
               ! reset child1
               call parton_set_simulated (prt%child1, .false.)
               prt%child1%t = min (prt%child2%t, (sqrt (prt%t) - &
                    sqrt (prt%child2%t))**2)
               integral(1) = zero
               call tao_random_number (random(1))
            else if (prt%child1%t - parton_mass_squared (prt%child1) > &
                 prt%child2%t - parton_mass_squared (prt%child2)) then
               !!! reset child2
               call parton_set_simulated (prt%child2, .false.)
               prt%child2%t = min (prt%child1%t, (sqrt (prt%t) - &
                    sqrt (prt%child1%t))**2)
               integral(2) = zero
               call tao_random_number (random(2))
            else
               !!! reset child1 ! TODO choose child according to their t
               call parton_set_simulated (prt%child1, .false.)
               prt%child1%t = min (prt%child2%t, (sqrt (prt%t) - &
                    sqrt (prt%child2%t))**2)
               integral(1) = zero
               call tao_random_number (random(1))
            end if
          else
             exit
          end if
       end if
    end do

    call parton_apply_costheta (prt)

    do daughter = 1, 2
       if (signal_is_pending ()) return
       if (daughter == 1) then
          daughterprt => prt%child1
       else
          daughterprt => prt%child2
       end if
       if (daughterprt%t < parton_mass_squared (daughterprt) + D_Min_t) then
          cycle
       end if
       if (.not. (parton_is_quark (daughterprt) .or. &
            parton_is_gluon (daughterprt))) then
          cycle
       end if
       if (parton_is_quark (daughterprt)) then
          !!! q -> qg
          call shower%add_child (daughterprt, 1)
          daughterprt%child1%type = daughterprt%type
          call parton_set_energy (daughterprt%child1, &
               daughterprt%z * parton_get_energy (daughterprt))
          daughterprt%child1%t = daughterprt%t
          call shower%add_child (daughterprt, 2)
          daughterprt%child2%type = 21
          call parton_set_energy (daughterprt%child2, (one - &
               daughterprt%z) * parton_get_energy (daughterprt))
          daughterprt%child2%t = daughterprt%t
       else if (parton_is_gluon (daughterprt)) then
          if (gtoqq(daughter) > 0) then
             call shower%add_child (daughterprt, 1)
             daughterprt%child1%type = gtoqq (daughter)
             call parton_set_energy (daughterprt%child1, &
                  daughterprt%z * parton_get_energy (daughterprt))
             daughterprt%child1%t = daughterprt%t
             call shower%add_child (daughterprt, 2)
             daughterprt%child2%type = - gtoqq (daughter)
             call parton_set_energy (daughterprt%child2, (one - &
                  daughterprt%z) * parton_get_energy (daughterprt))
             daughterprt%child2%t = daughterprt%t
          else
             call shower%add_child (daughterprt, 1)
             daughterprt%child1%type = 21
             call parton_set_energy (daughterprt%child1, &
                  daughterprt%z * parton_get_energy (daughterprt))
             daughterprt%child1%t = daughterprt%t
             call shower%add_child (daughterprt, 2)
             daughterprt%child2%type = 21
             call parton_set_energy (daughterprt%child2, (one - &
                  daughterprt%z) * parton_get_energy (daughterprt))
             daughterprt%child2%t = daughterprt%t
          end if
       end if
    end do
    call shower_parton_update_color_connections (shower, prt)
  end subroutine shower_simulate_children_ana

  subroutine shower_isr_step_pt (shower, prt)
    type(shower_t), intent(inout) :: shower
    type(parton_t), target, intent(inout) :: prt
    type(parton_t), pointer :: otherprt

    real(default) :: scale, scalestep
    real(default) :: integral, random, factor
    real(default) :: temprand1, temprand2

    otherprt => shower_find_recoiler (shower, prt)

    scale = prt%scale
    call tao_random_number (temprand1)
    call tao_random_number (temprand2)
    scalestep = max (abs (scalefactor1 * scale) * temprand1, &
         scalefactor2 * temprand2 * D_Min_scale)
    call tao_random_number (random)
    random = - twopi * log(random)
    integral = zero

    if (scale - 0.5_default * scalestep < D_Min_scale) then
       !!! close enough to cut-off scale -> ignore
       prt%scale = zero
       prt%t = parton_mass_squared (prt)
       call parton_set_simulated (prt)
    else
       prt%scale = scale - 0.5_default * scalestep
       factor = scalestep * (D_alpha_s_isr (prt%scale) / (prt%scale * &
            shower%get_pdf (prt%initial%type, prt%x, prt%scale, prt%type)))
       integral = integral + factor * integral_over_z_isr_pt &
            (prt, otherprt, (random - integral) / factor)
       if (integral > random) then
          !!! prt%scale set above and prt%z set in integral_over_z_isr_pt
          call parton_set_simulated (prt)
          prt%t = - prt%scale / (one - prt%z)
       else
          prt%scale = scale - scalestep
       end if
    end if

  contains

    function integral_over_z_isr_pt (prt, otherprt, final) &
         result (integral)
      type(parton_t), intent(inout) :: prt, otherprt
      real(default), intent(in) :: final
      real(default) :: integral
      real(default) :: mbr, r
      real(default) :: zmin, zmax, z, zstep
      integer :: n_bin
      integer, parameter :: n_total_bins = 100
      real(default) :: quarkpdfsum
      real(default) :: temprand
      integer :: quark

      quarkpdfsum = zero
      if (D_print) then
         print *, "integral_over_z_isr_pt for scale = ", prt%scale
      end if

      integral = zero
      mbr = (prt%momentum + otherprt%momentum)**1
      zmin = prt%x
      zmax = min (one - (sqrt (prt%scale) / mbr) * &
           (sqrt(one + 0.25_default * prt%scale / mbr**2) - &
           0.25_default * sqrt(prt%scale) / mbr), maxz_isr)
      zstep = (zmax - zmin) / n_total_bins

      if (zmin > zmax) then
         !!! print *, " error in integral_over_z_isr_pt: zmin > zmax ", &
         !!!        zmin, zmax, prt%scale, mbr
         integral = zero
         return
      end if

      !!! divide the range [zmin:zmax] in n_total_bins ->
      BINS: do n_bin = 1, n_total_bins
         z = zmin + zstep * (n_bin -0.5_default)
         !!! z-value in the middle of the bin

         if (parton_is_gluon (prt)) then
            QUARKS: do quark = -D_Nf, D_Nf
               if (quark == 0) cycle quarks
               quarkpdfsum = quarkpdfsum + shower%get_pdf &
                    (prt%initial%type, prt%x / z, prt%scale, quark)
            end do QUARKS
            !!! g -> gg or q -> gq
            integral = integral + (zstep / z) * ((P_ggg (z) + &
                 P_ggg (one - z)) * shower%get_pdf (prt%initial%type, &
                 prt%x / z, prt%scale, 21) + P_qqg (one - z) * quarkpdfsum)
         else if (parton_is_quark (prt)) then
            !!! q -> qg or g -> qq
            integral = integral + (zstep / z) * ( P_qqg (z) * &
                 shower%get_pdf (prt%initial%type, prt%x / z, prt%scale, prt%type) + &
                 P_gqq(z) * shower%get_pdf (prt%initial%type, prt%x / z, prt%scale, 21))
         else
            ! call msg_fatal ("Bug neither quark nor gluon in" &
            !           // " integral_over_z_isr_pt")
         end if
         if (integral > final) then
            prt%z = z
            call tao_random_number (temprand)
            !!! decide typ of father partons
              if (parton_is_gluon (prt)) then
                 if (temprand > (P_qqg (one - z) * quarkpdfsum) / &
                      ((P_ggg (z) + P_ggg (one - z)) * shower%get_pdf &
                      (prt%initial%type, prt%x / z, prt%scale, 21) &
                      + P_qqg (one - z) * quarkpdfsum)) then
                    !!! gluon => gluon + gluon
                    prt%aux_pt = 21
                 else
                    !!! quark => quark + gluon
                    !!! decide which quark flavor the parent is
                    r = temprand * quarkpdfsum
                    WHICH_QUARK: do quark = -D_Nf, D_Nf
                       if (quark == 0) cycle WHICH_QUARK
                       if (r > quarkpdfsum - shower%get_pdf (prt%initial%type, &
                            prt%x / z, prt%scale, quark)) then
                          prt%aux_pt = quark
                          exit WHICH_QUARK
                       else
                          quarkpdfsum = quarkpdfsum - shower%get_pdf &
                               (prt%initial%type, prt%x / z, prt%scale, quark)
                       end if
                    end do WHICH_QUARK
                 end if

              else if (parton_is_quark (prt)) then
                 if (temprand > (P_qqg (z) * shower%get_pdf (prt%initial%type, &
                      prt%x / z, prt%scale, prt%type)) / &
                      (P_qqg (z) * shower%get_pdf (prt%initial%type, prt%x / z, &
                      prt%scale, prt%type) + &
                      P_gqq (z) * shower%get_pdf (prt%initial%type, prt%x / z, &
                      prt%scale, 21))) then
                    !!! gluon => quark + antiquark
                    prt%aux_pt = 21
                 else
                    !!! quark => quark + gluon
                    prt%aux_pt = prt%type
                 end if
              end if
              exit BINS
           end if
        end do BINS
      end function integral_over_z_isr_pt
    end subroutine shower_isr_step_pt

  function shower_generate_next_isr_branching_veto &
       (shower) result (next_brancher)
    class(shower_t), intent(inout) :: shower
    type(parton_pointer_t) :: next_brancher
    integer :: i
    type(parton_t),  pointer :: prt
    real(default) :: random
    !!! pointers to branchable partons
    type(parton_pointer_t), dimension(:), allocatable :: partons
    integer :: n_partons
    real(default) :: weight
    real(default) :: temp1, temp2, temp3, E3

    if (signal_is_pending ()) return

    if (isr_pt_ordered) then
       next_brancher = shower%generate_next_isr_branching ()
       return
    end if
    next_brancher%p => null()
    !!! branchable partons
    n_partons = 0
    do i = 1,size (shower%partons)
       prt => shower%partons(i)%p
       if (signal_is_pending ()) return
       if (.not. associated (prt)) cycle
       if (prt%belongstoFSR) cycle
       if (parton_is_final (prt)) cycle
       if (.not. prt%belongstoFSR .and. parton_is_simulated (prt)) cycle
       n_partons = n_partons + 1
    end do
    !!! print *, "n_partons = ", n_partons
    if (n_partons == 0) then
       return
    end if
    allocate (partons(1:n_partons))
    n_partons = 1
    do i = 1, size (shower%partons)
       prt => shower%partons(i)%p
       if (.not. associated (prt)) cycle
       if (prt%belongstoFSR) cycle
       if (parton_is_final (prt)) cycle
       if (.not. prt%belongstoFSR .and. parton_is_simulated (prt)) cycle
       if (signal_is_pending ()) return
       partons(n_partons)%p => shower%partons(i)%p
       n_partons = n_partons + 1
    end do
    ! print *, "trials"
    !!! generate initial trial scales
    do i = 1, size (partons)
       if (signal_is_pending ()) return
       call generate_next_trial_scale (partons(i)%p)
    end do
    ! print *, "trials finished"

    do
       ! call shower%write ()
       !!! search for parton with the highest trial scale
       prt => partons(1)%p
       do i = 1, size (partons)
          if (prt%t >= zero) cycle
          if (abs (partons(i)%p%t) > abs (prt%t)) then
             prt => partons(i)%p
          end if
       end do

       if (prt%t >= zero) then
          next_brancher%p => null()
          exit
       end if
       !!! generate trial z and type of mother prt
       call generate_trial_z_and_typ (prt)

       !!! weight with pdf and alpha_s

       temp1 = (D_alpha_s_isr ((one - prt%z) * abs(prt%t)) &
            / sqrt (alphasxpdfmax))
       temp2 = shower%get_xpdf (prt%initial%type, prt%x, prt%t, &
            prt%type) / sqrt (alphasxpdfmax)
       temp3 = shower%get_xpdf (prt%initial%type, prt%child1%x, prt%child1%t, &
            prt%child1%type) / &
            shower%get_xpdf (prt%initial%type, prt%child1%x, prt%t, prt%child1%type)
       if (temp1 * temp2 * temp3 > one) then
          print *, "weights:", temp1, temp2, temp3
       end if
       weight = (D_alpha_s_isr ((one - prt%z) * abs(prt%t))) * &
            shower%get_xpdf (prt%initial%type, prt%x, prt%t, prt%type) * &
            shower%get_xpdf (prt%initial%type, prt%child1%x, prt%child1%t, &
            prt%child1%type) / &
            shower%get_xpdf (prt%initial%type, prt%child1%x, prt%t, prt%child1%type)
       if (weight > alphasxpdfmax) then
          print *, "Setting alphasxpdfmax from ", alphasxpdfmax, " to ", weight
          alphasxpdfmax = weight
       end if
       weight = weight / alphasxpdfmax
       call tao_random_number (random)
       ! print *, weight , ">", random, "?"
       if (weight < random) then
          !!! discard branching
          call generate_next_trial_scale (prt)
          cycle
       end if
       !!! branching accepted so far
       !!! generate emitted parton
       prt%child2%t = abs(prt%t)
       call parton_set_energy (prt%child2, sqrt (abs(prt%t)))
       if (isr_only_onshell_emitted_partons) then
          prt%child2%t = parton_mass_squared(prt%child2)
       else
          call parton_next_t_ana (prt%child2)
       end if

       if (thetabar (prt, shower_find_recoiler (shower, prt), E3)) then
          !!! setting energies
          call parton_set_energy (prt, E3)
          call parton_set_energy (prt%child2, &
               E3 - parton_get_energy (prt%child1))

          !!! found branching
          call parton_generate_ps_ini (prt)
          next_brancher%p => prt
          call parton_set_simulated (prt)
          exit
       else
          call generate_next_trial_scale (prt)
          cycle
       end if
    end do
    if (.not. associated (next_brancher%p)) then
       !!! no further branching found -> all partons emitted by hadron
       print *, "--all partons emitted by hadrons---"
       do i = 1, size(partons)
          call shower_replace_parent_by_hadron (shower, partons(i)%p%child1)
       end do
    end if
    !!! some bookkeeping
    call shower%sort_partons ()
    ! call shower_boost_to_CMframe(shower)         ! really necessary?
    ! call shower_rotate_to_z(shower)              ! really necessary?
  contains

    subroutine generate_next_trial_scale(prt)
      type(parton_t), pointer, intent(inout) :: prt
      real(default) :: random, F
      real(default) :: zmax = 0.99_default !! ??
      ! print *, "generate next_trial"
      call tao_random_number (random)
      F = one   !!! TODO
      F = alphasxpdfmax / (two * pi)
      if (parton_is_quark (prt%child1)) then
         F = F * (integral_over_P_gqq (prt%child1%x, zmax) + &
                  integral_over_P_qqg (prt%child1%x, zmax))
      else if (parton_is_gluon (prt%child1)) then
         F = F * (integral_over_P_ggg (prt%child1%x, zmax) + &
              two * D_Nf * &
              integral_over_P_qqg (one - zmax, one - prt%child1%x))
      else
         print *, "neither quark nor gluon in generate_next_trial_scale"
      end if
      F = F / shower%get_xpdf (prt%child1%initial%type, prt%child1%x, &
           prt%child1%t, prt%child1%type)
      prt%t = prt%t * random**(one / F)
      if (abs (prt%t) - parton_mass_squared (prt) < D_Min_t) then
         prt%t = parton_mass_squared (prt)
      end if
      ! print *, "generate next_trial finished"
    end subroutine generate_next_trial_scale

    subroutine generate_trial_z_and_typ(prt)
      type(parton_t), pointer, intent(inout) :: prt
      real(default) :: random
      real(default) :: z, zstep, zmin, integral
      real(default) :: zmax = 0.99_default !! ??
      ! print *, "generate next_z"
      call tao_random_number(random)
      integral = zero
      !!! decide which branching a->bc occurs
      if (parton_is_quark (prt%child1)) then
         if (random < integral_over_P_qqg (prt%child1%x, zmax) / &
              (integral_over_P_qqg (prt%child1%x, zmax) + &
               integral_over_P_gqq(prt%child1%x, zmax))) then
            prt%type = prt%child1%type
            prt%child2%type = 21
            integral = integral_over_P_qqg (prt%child1%x, zmax)
         else
            prt%type = 21
            prt%child2%type = - prt%child1%type
            integral = integral_over_P_gqq (prt%child1%x, zmax)
         end if
      else if (parton_is_gluon (prt%child1)) then
         if (random < integral_over_P_ggg (prt%child1%x, zmax) / &
              (integral_over_P_ggg (prt%child1%x, zmax) + two * D_Nf * &
              integral_over_P_qqg (one - zmax, &
              one - prt%child1%x))) then
            prt%type = 21
            prt%child2%type = 21
            integral = integral_over_P_ggg (prt%child1%x, zmax)
         else
            call tao_random_number (random)
            prt%type = 1 + floor(random * D_Nf)
            call tao_random_number (random)
            if (random > 0.5_default) prt%type = - prt%type
            prt%child2%type = prt%type
            integral = integral_over_P_qqg (one - zmax, &
                 one - prt%child1%x)
         end if
      else
         print *, "neither quark nor gluon in generate_next_trial_scale"
      end if
      !!! generate the z-value
      !!! z between prt%child1%x and zmax
      ! prt%z = one - random * (one - prt%child1%x)      ! TODO

      call tao_random_number(random)
      zmin = prt%child1%x
      zstep = max(0.1_default, 0.5_default * (zmax - zmin))
      z = zmin
      ! print *, zmin, z
      if (zmin > zmax) then
         print *, " zmin = ", zmin, " zmax = ", zmax
         call msg_fatal ("Shower: zmin greater than zmax")
      end if
      !!! procedure pointers would be helpful here
      if (parton_is_quark (prt) .and. parton_is_quark (prt%child1)) then
         do
            ! print *, 1, z, zstep, zmin, zmax
            zstep = min(zstep, 0.5_default * (zmax - z))
            if (abs(zstep) < 0.00001) exit
            if (integral_over_P_qqg (zmin, z) < random * integral) then
               if (integral_over_P_qqg (zmin, min(z + zstep, zmax)) &
                    < random * integral) then
                  z = min (z + zstep, zmax)
                  cycle
               else
                  zstep = zstep * 0.5_default
                  cycle
               end if
            end if
         end do
      else if (parton_is_quark (prt) .and. parton_is_gluon (prt%child1)) then
         do
            ! print *, 2, z, zstep
            zstep = min(zstep, 0.5_default * (zmax - z))
            if (abs(zstep) < 0.00001) exit
            if (integral_over_P_qqg (zmin, z) < random * integral) then
               if (integral_over_P_qqg (zmin, min(z + zstep, zmax)) &
                    < random * integral) then
                  z = min(z + zstep, zmax)
                  cycle
               else
                  zstep = zstep * 0.5_default
                  cycle
               end if
            end if
         end do
      else if (parton_is_gluon (prt) .and. parton_is_quark (prt%child1)) then
         do
            ! print *, 3, z, zstep
            zstep = min(zstep, 0.5_default * (zmax - z))
            if (abs (zstep) < 0.00001) exit
            if (integral_over_P_gqq (zmin, z) < random * integral) then
               if (integral_over_P_gqq (zmin, min(z + zstep, zmax)) &
                    < random * integral) then
                  z = min (z + zstep, zmax)
                  cycle
               else
                  zstep = zstep * 0.5_default
                  cycle
               end if
            end if
         end do
      else if (parton_is_gluon (prt) .and. parton_is_gluon (prt%child1)) then
         do
            ! print *, 4, z, zstep
            zstep = min(zstep, 0.5_default * (zmax - z))
            if (abs (zstep) < 0.00001) exit
            if (integral_over_P_ggg(zmin, z) < random * integral) then
               if (integral_over_P_ggg (zmin, min(z + zstep, zmax)) &
                    < random * integral) then
                  z = min(z + zstep, zmax)
                  cycle
               else
                  zstep = zstep * 0.5_default
                  cycle
               end if
            end if
         end do
      else
      end if
      prt%z = z
      prt%x = prt%child1%x / prt%z
      ! print *, "generate next_z finished"
    end subroutine generate_trial_z_and_typ
  end function shower_generate_next_isr_branching_veto

  function shower_find_recoiler (shower, prt) result(recoiler)
    type(shower_t), intent(inout) :: shower
    type(parton_t), intent(inout), target :: prt
    type(parton_t), pointer :: recoiler
    type(parton_t), pointer :: otherprt1, otherprt2
    integer :: n_int
    otherprt1 => null()
    otherprt2 => null()
    DO_INTERACTIONS: do n_int = 1, size(shower%interactions)
       otherprt1=>shower%interactions(n_int)%i%partons(1)%p
       otherprt2=>shower%interactions(n_int)%i%partons(2)%p
       PARTON1: do
          if (associated (otherprt1%parent)) then
             if (.not. parton_is_hadron(otherprt1%parent) .and. &
                  parton_is_simulated (otherprt1%parent)) then
                otherprt1 => otherprt1%parent
                if (associated (otherprt1, prt)) then
                   exit PARTON1
                end if
             else
                exit PARTON1
             end if
          else
             exit PARTON1
          end if
       end do PARTON1
       PARTON2: do
          if (associated (otherprt2%parent)) then
             if (.not. parton_is_hadron(otherprt2%parent) .and. &
                  parton_is_simulated (otherprt2%parent)) then
                otherprt2 => otherprt2%parent
                if (associated (otherprt2, prt)) then
                   exit PARTON2
                end if
             else
                exit PARTON2
             end if
          else
             exit PARTON2
          end if
       end do PARTON2

       if (associated (otherprt1, prt) .or. associated (otherprt2, prt)) then
          exit DO_INTERACTIONS
       end if
       if (associated (otherprt1%parent, prt) .or. &
            associated (otherprt2%parent, prt)) then
          exit DO_INTERACTIONS
       end if
    end do DO_INTERACTIONS

    recoiler => null()
    if (associated (otherprt1%parent, prt)) then
       recoiler => otherprt2
    else if (associated (otherprt2%parent, prt)) then
       recoiler => otherprt1
    else if (associated (otherprt1, prt)) then
       recoiler => otherprt2
    else if (associated (otherprt2, prt)) then
       recoiler => otherprt1
    else
       call shower%write ()
       call parton_write (prt)
       call msg_error ("Shower: no otherparton found")
    end if
  end function shower_find_recoiler

  subroutine shower_isr_step (shower, prt)
    type(shower_t), intent(inout) :: shower
    type(parton_t), target, intent(inout) :: prt
    type(parton_t), pointer :: otherprt => null()
    real(default) :: t, tstep
    real(default) :: integral, random
    real(default) :: temprand1, temprand2
    ! print *, "shower_isr_step for parton ", prt%nr
    otherprt => shower_find_recoiler(shower, prt)
    ! if (.not. otherprt%child1%belongstointeraction) then
    !    otherprt => otherprt%child1
    ! end if

    if (signal_is_pending ()) return
    t = max(prt%t, prt%child1%t)
    call tao_random_number(random)
    ! compare Integral and log(random) instead of random and exp(-Integral)
    random = - twopi * log(random)
    integral = zero
    call tao_random_number (temprand1)
    call tao_random_number (temprand2)
    tstep = max (abs (0.02_default * t) * temprand1, &
         0.02_default * temprand2 * D_Min_t)
    if (t + 0.5_default * tstep > - D_Min_t) then
       prt%t = parton_mass_squared (prt)
       call parton_set_simulated (prt)
    else
       prt%t = t + 0.5_default * tstep
       integral = integral + tstep * &
            integral_over_z_isr (prt, otherprt,(random - integral) / tstep)
       if (integral > random) then
          prt%t = t + 0.5_default * tstep
          prt%x = prt%child1%x / prt%z
          call parton_set_simulated (prt)
       else
          prt%t = t + tstep
       end if
    end if

    ! print *, "  shower_isr_step finished"

  contains
    function integral_over_z_isr (prt, otherprt, final) result (integral)
      type(parton_t), intent(inout) :: prt, otherprt
      real(default), intent(in) :: final
      real(default) integral
      real(default) :: minz, maxz, z, shat,s
      integer :: quark

      !!! calculate shat -> s of parton-parton system
      shat= (otherprt%momentum + prt%child1%momentum)**2
      !!! calculate s -> s of hadron-hadron system
      s= (otherprt%initial%momentum + prt%initial%momentum)**2
      integral = zero
      minz = prt%child1%x
      maxz = maxzz (shat, s)

      !!! for gluon
      if (parton_is_gluon (prt%child1)) then
         !!! 1: g->gg
         prt%type = 21
         prt%child2%type = 21
           z = minz
           prt%child2%t = abs(prt%t)
           call integral_over_z_part_isr &
                (prt,otherprt, shat, minz, maxz, integral, final)
           if (integral > final) then
              ! print *, "prt%type = ", prt%type
              return
           end if
           !!! 2: q->gq
           do quark = -D_Nf, D_Nf
              if (quark == 0) cycle
              prt%type = quark
              prt%child2%type = quark
              z = minz
              prt%child2%t = abs(prt%t)
              call integral_over_z_part_isr &
                   (prt,otherprt, shat, minz, maxz, integral, final)
              if (integral > final) then
                 ! print *, "prt%type = ", prt%type
                 return
              end if
           end do
        else if (parton_is_quark (prt%child1)) then
           !!! 1: q->qg
           prt%type = prt%child1%type
           prt%child2%type = 21
           z = minz
           prt%child2%t = abs(prt%t)
           call integral_over_z_part_isr &
                (prt,otherprt, shat, minz, maxz, integral, final)
           if (integral > final) then
              ! print *, "prt%type = ", prt%type
              return
           end if
           !!! 2: g->qqbar
           prt%type = 21
           prt%child2%type = -prt%child1%type
           z = minz
           prt%child2%t = abs(prt%t)
           call integral_over_z_part_isr &
                (prt,otherprt, shat, minz, maxz, integral, final)
           if (integral > final) then
              ! print *, "prt%type = ", prt%type
              return
           end if
        end if
      end function integral_over_z_isr

      subroutine integral_over_z_part_isr &
           (prt, otherprt, shat ,minz, maxz, retvalue, final)
        type(parton_t), intent(inout) :: prt, otherprt
        real(default), intent(in) :: shat, minz, maxz, final
        real(default), intent(inout) :: retvalue
        real(default) :: z, zstep
        real(default) :: r1,r3,s1,s3
        real(default) :: pdf_divisor
        real(default) :: temprand
        real(default), parameter :: zstepfactor = 0.1_default
        real(default), parameter :: zstepmin = 0.0001_default
        if (D_print) print *, "integral_over_z_part_isr"
        pdf_divisor = shower%get_pdf &
             (prt%initial%type, prt%child1%x, prt%t, prt%child1%type)
        z = minz
        s1 = shat + abs(otherprt%t) + abs(prt%child1%t)
        r1 = sqrt (s1**2 - four * abs(otherprt%t * prt%child1%t))
        ZLOOP: do
           if (signal_is_pending ()) return
           if (z >= maxz) then
              exit
           end if
           call tao_random_number (temprand)
           if (parton_is_gluon (prt%child1)) then
              if (parton_is_gluon (prt)) then
                 !!! g-> gg -> divergencies at z->0 and z->1
                 zstep = max(zstepmin, temprand * zstepfactor * z * (one - z))
              else
                 !!! q-> gq -> divergencies at z->0
                 zstep = max(zstepmin, temprand * zstepfactor * (one - z))
              end if
           else
              if (parton_is_gluon (prt)) then
                 !!! g-> qqbar -> no divergencies
                 zstep = max(zstepmin, temprand * zstepfactor)
              else
                 !!! q-> qg -> divergencies at z->1
                 zstep = max(zstepmin, temprand * zstepfactor * (one - z))
              end if
           end if
           zstep = min(zstep, maxz - z)
           prt%z = z + 0.5_default * zstep
           s3 = shat / prt%z + abs(otherprt%t) + abs(prt%t)
           r3 = sqrt (s3**2 - four * abs(otherprt%t * prt%t))
           !!! TODO: WHY is this if needed?
           if (otherprt%t /= zero) then
              prt%child2%t = min ((s1 * s3 - r1 * r3) / &
                   (two * abs(otherprt%t)) - abs(prt%child1%t) - &
                   abs(prt%t), abs(prt%child1%t))
           else
              prt%child2%t = abs(prt%child1%t)
           end if
           do
              call parton_set_energy (prt%child2, sqrt (abs(prt%child2%t)))
              if (isr_only_onshell_emitted_partons) then
                 prt%child2%t = parton_mass_squared (prt%child2)
              else
                 call parton_next_t_ana (prt%child2)
              end if
              !!! take limits by recoiler into account
              call parton_set_energy (prt, (shat / prt%z + &
                   abs(otherprt%t) - abs(prt%child1%t) - &
                   prt%child2%t) / (two * sqrt(shat)))
              call parton_set_energy (prt%child2, &
                   parton_get_energy (prt) - parton_get_energy (prt%child1))
              !!! check if E and t of prt%child2 are consistent
              if (parton_get_energy (prt%child2)**2 < prt%child2%t &
                   .and. prt%child2%t > parton_mass_squared (prt%child2)) then
                 !!! E is too small to have p_T^2 = E^2 - t > 0
                 !!!      -> cycle to find another solution
                 cycle
              else
                 !!! E is big enough -> exit
                 exit
              end if
           end do
           if (thetabar (prt, otherprt) .and. pdf_divisor > zero &
                .and. parton_get_energy (prt%child2) > zero) then
              retvalue = retvalue + (zstep / prt%z) * &
                   (D_alpha_s_isr ((one - prt%z) * prt%t) * &
                   P_prt_to_child1 (prt) * &
                   shower%get_pdf (prt%initial%type, prt%child1%x / prt%z, &
                   prt%t, prt%type)) / (abs(prt%t) * pdf_divisor)
           end if
           if (retvalue > final) then
              exit
           else
              z = z + zstep
           end if
        end do ZLOOP
      end subroutine integral_over_z_part_isr
    end subroutine shower_isr_step

  function shower_generate_next_isr_branching &
       (shower) result (next_brancher)
    class(shower_t), intent(inout) :: shower
    type(parton_pointer_t) :: next_brancher
    integer i, index
    type(parton_t),  pointer :: prt
    real(default) :: maxscale
    next_brancher%p => null()
    do
       if (signal_is_pending ()) return
       if (shower_isr_is_finished (shower)) exit
       !!! find mother with highest |t| or pt to be simulated
       index = 0
       maxscale = zero
       call shower%sort_partons ()
       do i = 1,size (shower%partons)
          prt => shower%partons(i)%p
          if (.not. associated (prt)) cycle
          if (.not. isr_pt_ordered) then
             if (prt%belongstointeraction) cycle
          end if
          if (prt%belongstoFSR) cycle
          if (parton_is_final (prt)) cycle
          if (.not. prt%belongstoFSR .and. parton_is_simulated (prt)) cycle
          index = i
          exit
       end do
       if (index == 0) then
          ! print *, " no branchable partons found"
          return
       end if

       prt => shower%partons(index)%p

       !!! ISR simulation
       if (isr_pt_ordered) then
          call shower_isr_step_pt (shower, prt)
       else
          call shower_isr_step (shower, prt)
       end if
       if (parton_is_simulated (prt)) then
          if (prt%t < zero) then
             next_brancher%p => prt
             if (.not. isr_pt_ordered) call parton_generate_ps_ini (prt)
             exit
          else
             if (.not. isr_pt_ordered) then
                call shower_replace_parent_by_hadron (shower, prt%child1)
             else
                call shower_replace_parent_by_hadron (shower, prt)
             end if
          end if
       end if
    end do

    !!! some bookkeeping
    call shower%sort_partons ()
    call shower_boost_to_CMframe (shower)         !!! really necessary?
    call shower_rotate_to_z (shower)              !!! really necessary?
    ! print *, "shower_generate_next_isr_branching finished"
    end function shower_generate_next_isr_branching

  subroutine shower_generate_fsr_for_partons_emitted_in_ISR (shower)
    class(shower_t), intent(inout) :: shower
    integer :: n_int, i
    type(parton_t), pointer :: prt
    if (isr_only_onshell_emitted_partons) return

    INTERACTIONS_LOOP: do n_int = 1, size (shower%interactions)
       INCOMING_PARTONS_LOOP: do i = 1, 2
          if (signal_is_pending ()) return
          prt => shower%interactions(n_int)%i%partons(i)%p
          PARENT_PARTONS_LOOP: do
             if (associated (prt%parent)) then
                if (.not. parton_is_hadron (prt%parent)) then
                   prt => prt%parent
                else
                   exit
                end if
             else
                exit
             end if
             if (associated (prt%child2)) then
                if (parton_is_branched (prt%child2)) then
                   call shower_parton_generate_fsr (shower, prt%child2)
                end if
             else
                ! call msg_fatal ("Shower: no child2 associated?")
             end if
          end do PARENT_PARTONS_LOOP
       end do INCOMING_PARTONS_LOOP
    end do INTERACTIONS_LOOP
  end subroutine shower_generate_fsr_for_partons_emitted_in_ISR

  subroutine shower_execute_next_isr_branching (shower, prtp)
    class(shower_t), intent(inout) :: shower
    type(parton_pointer_t), intent(inout) :: prtp
    type(parton_t), pointer :: prt, otherprt
    type(parton_t), pointer :: prta, prtb, prtc, prtr
    real(default) :: mar, mbr
    real(default) :: phirand
    ! print *, "shower_execute_next_isr_branching"
    if (.not. associated (prtp%p)) then
       call msg_fatal ("Shower: prtp not associated")
    end if

    prt => prtp%p

    if ((.not. isr_pt_ordered .and. prt%t > - D_Min_t) .or. &
         (isr_pt_ordered .and. prt%scale < D_Min_scale)) then
       call msg_error ("Shower: no branching to be executed.")
    end if

    otherprt => shower_find_recoiler (shower, prt)
    if (isr_pt_ordered) then
       !!! get the recoiler
       otherprt => shower_find_recoiler (shower, prt)
       if (associated (otherprt%parent)) then
          !!! Why only for pt ordered
          if (.not. parton_is_hadron (otherprt%parent) .and. &
               isr_pt_ordered) otherprt => otherprt%parent
       end if
       if (.not. associated (prt%parent)) then
          call shower%add_parent (prt)
       end if
       prt%parent%belongstoFSR = .false.
       if (.not. associated (prt%parent%child2)) then
          call shower%add_child (prt%parent, 2)
       end if

       prta => prt%parent          !!! new parton a with branching a->bc
       prtb => prt                 !!! former parton
       prtc => prt%parent%child2   !!! emitted parton
       prtr => otherprt            !!! recoiler

       mbr = (prtb%momentum + prtr%momentum)**1
       mar = mbr / sqrt(prt%z)

       !!! 1. assume you are in the restframe
       !!! 2. rotate by random phi
       call tao_random_number (phirand)
       phirand = twopi * phirand
       call shower_apply_lorentztrafo (shower, &
            rotation(cos(phirand), sin(phirand),vector3_canonical(3)))
       !!! 3. Put the b off-shell
       !!! and
       !!! 4. construct the massless a
       !!! and the parton (eventually emitted by a)

       !!! generate the flavour of the parent (prta)
       if (prtb%aux_pt /= 0) prta%type = prtb%aux_pt
       if (parton_is_quark (prtb)) then
          if (prta%type == prtb%type) then
             !!! (anti)-quark -> (anti-)quark + gluon
             prta%type = prtb%type   ! quarks have same flavour
             prtc%type = 21         ! emitted gluon
          else
             !!! gluon -> quark + antiquark
             prta%type = 21
             prtc%type = - prtb%type
          end if
       else if (parton_is_gluon (prtb)) then
          prta%type = 21
          prtc%type = 21
       else
          ! STOP "Bug in shower_execute_nexT_branching: neither quark nor gluon"
       end if

       prta%initial => prtb%initial
       prta%belongstoFSR = .false.
       prta%scale = prtb%scale
       prta%x = prtb%x / prtb%z

       prtb%momentum = vector4_moving ((mbr**2 + prtb%t) / (two * mbr), &
            vector3_canonical(3) * &
            sign ((mbr**2 - prtb%t) / (two * mbr), &
            vector4_get_component (prtb%momentum, 3)))
       prtr%momentum = vector4_moving ((mbr**2 - prtb%t) / (two * mbr), &
            vector3_canonical(3) * &
            sign( (mbr**2 - prtb%t) / (two * mbr), &
            vector4_get_component(prtr%momentum, 3)))

       prta%momentum = vector4_moving ((0.5_default / mbr) * &
            ((mbr**2 / prtb%z) + prtb%t - parton_mass_squared(prtc)), &
            vector3_null)
       prta%momentum = vector4_moving (parton_get_energy (prta), &
            vector3_canonical(3) * &
            (0.5_default / vector4_get_component (prtb%momentum, 3)) * &
            ((mbr**2 / prtb%z) - two &
            * parton_get_energy(prtr) * parton_get_energy(prta) ) )
       if (parton_get_energy(prta)**2 - vector4_get_component &
            (prta%momentum,3)**2 - parton_mass_squared (prtc) &
            > zero) then
          !!! This SHOULD be always fulfilled???
          prta%momentum = vector4_moving (parton_get_energy (prta), &
               vector3_moving([sqrt (parton_get_energy(prta)**2 - &
               vector4_get_component (prta%momentum, 3)**2 - &
               parton_mass_squared (prtc)), zero, &
               vector4_get_component(prta%momentum, 3) ]))
       end if
       prtc%momentum = prta%momentum - prtb%momentum

       !!! 5. rotate to have a along z-axis
       call shower_boost_to_CMframe (shower)
       call shower_rotate_to_z (shower)
       !!! 6. rotate back in phi
       call shower_apply_lorentztrafo (shower, rotation &
            (cos(-phirand), sin(-phirand), vector3_canonical(3)))
    else
       if (prt%child2%t > parton_mass_squared(prt%child2)) then
          call shower_add_children_of_emitted_timelike_parton &
               (shower, prt%child2)
          call parton_set_simulated (prt%child2)
       end if

       call shower%add_parent (prt)
       call shower%add_child (prt%parent, 2)

       prt%parent%momentum = prt%momentum
       prt%parent%t = prt%t
       prt%parent%x = prt%x
       prt%parent%initial => prt%initial
       prt%parent%belongstoFSR = .false.

       prta => prt
       prtb => prt%child1
       prtc => prt%child2
    end if
    if (signal_is_pending ()) return
    if (isr_pt_ordered) then
       call parton_generate_ps_ini (prt%parent)
    else
       call parton_generate_ps_ini (prt)
    end if

    !!! add color connections
    if (parton_is_quark (prtb)) then

       if (prta%type == prtb%type) then
          if (prtb%type > 0) then
             !!! quark -> quark + gluon
             prtc%c2 = prtb%c1
             prtc%c1 = shower%get_next_color_nr ()
             prta%c1 = prtc%c1
          else
             !!! antiquark -> antiquark + gluon
             prtc%c1 = prtb%c2
             prtc%c2 = shower%get_next_color_nr ()
             prta%c2 = prtc%c2
          end if
       else
          !!! gluon -> quark + antiquark
          if (prtb%type > 0) then
             !!! gluon -> quark + antiquark
             prta%c1 = prtb%c1
             prtc%c1 = 0
             prtc%c2 = shower%get_next_color_nr ()
             prta%c2 = prtc%c2
          else
             !!! gluon -> antiquark + quark
             prta%c2 = prtb%c2
             prtc%c1 = shower%get_next_color_nr ()
             prtc%c2 = 0
             prta%c1 = prtc%c1
            end if
         end if
      else if (parton_is_gluon (prtb)) then
         if (parton_is_gluon (prta)) then
            !!! g -> gg
            prtc%c2 = prtb%c1
            prtc%c1 = shower%get_next_color_nr ()
            prta%c1 = prtc%c1
            prta%c2 = prtb%c2
         else if (parton_is_quark (prta)) then
            if (prta%type > 0) then
               prta%c1 = prtb%c1
               prta%c2 = 0
               prtc%c1 = prtb%c2
               prtc%c2 = 0
            else
               prta%c1 = 0
               prta%c2 = prtb%c2
               prtc%c1 = 0
               prtc%c2 = prtb%c1
            end if
         end if
      else
         ! STOP "Bug in shower_execute_nexT_branching:
         !   neither quark nor gluon"
      end if

      call shower%sort_partons ()
      call shower_boost_to_CMframe (shower)
      call shower_rotate_to_z (shower)

      ! print *, "  shower_execute_next_isr_branching finished"
    end subroutine shower_execute_next_isr_branching

  subroutine shower_remove_parents_and_stuff (shower, prt)
    type(shower_t), intent(inout) :: shower
    type(parton_t), intent(inout), target :: prt
    type(parton_t), pointer :: actprt, nextprt
    ! print *, " shower_remove_parents for parton ", prt%nr
    nextprt => prt%parent
    actprt => null()
    !!! remove children of emitted timelike parton
    if (associated (prt%child2)) then
       if (associated (prt%child2%child1)) then
          call shower_remove_parton_from_partons_recursive &
               (shower, prt%child2%child1)
       end if
       prt%child2%child1 => null()
       if (associated (prt%child2%child2)) then
          call shower_remove_parton_from_partons_recursive &
               (shower, prt%child2%child2)
       end if
       prt%child2%child2 => null()
    end if
    do
       actprt => nextprt
       if (.not. associated (actprt)) then
          exit
       else if (parton_is_hadron (actprt)) then
          !!! remove beam-remnant
          call shower_remove_parton_from_partons (shower, actprt%child2)
          exit
       end if
       if (associated (actprt%parent)) then
          nextprt => actprt%parent
       else
          nextprt => null()
       end if
       ! print *, " removing parton ", actprt%child2%nr
       call shower_remove_parton_from_partons_recursive &
            (shower, actprt%child2)
       ! print *, " removing parton ", actprt%nr
       call shower_remove_parton_from_partons (shower, actprt)

    end do
    prt%parent=>null()

    !      print *, " shower_remove_parents for parton finished"
  end subroutine shower_remove_parents_and_stuff

!!! MERGIND: not sure which function is the right one
!!!    function shower_get_ISR_scale(shower) result (scale)
!!!      class(shower_t), intent(in) :: shower
!!!      real(default) :: scale
!!!
!!!      type(parton_t), pointer :: prt1, prt2
!!!      integer :: i
!!!
!!!      scale = (10._default)**10
!!!      do i = 1, size(shower%interactions)
!!!         call interaction_find_partons_nearest_to_hadron(shower%interactions(i)%i, &
!!!              prt1, prt2)
!!!         call shower%write ()
!!!         call parton_write(prt1)
!!!         call parton_write(prt2)
!!!
!!!         if (isr_pt_ordered) then
!!!            if ((parton_is_hadron(prt1%parent).eqv..false.).and.(abs(prt1%parent%scale).gt.scale)) scale = abs(prt1%parent%scale)
!!!            if ((parton_is_hadron(prt1%parent).eqv..false.).and.(abs(prt2%parent%scale).gt.scale)) scale = abs(prt2%parent%scale)
!!!         else
!!!            if ((parton_is_simulated(prt1)).and.(abs(prt1%t).lt.scale)) scale = abs(prt1%t)
!!!            if ((parton_is_simulated(prt2)).and.(abs(prt2%t).lt.scale)) scale = abs(prt2%t)
!!!!            if ((parton_is_hadron(prt1%parent).eqv..false.).and.(abs(prt2%parent%t).gt.scale)) scale = abs(prt2%parent%t)
!!!         end if
!!!      end do
!!!      print *, "returning: ", scale
!!!!      pause
!!!    end function shower_get_ISR_scale

  function shower_get_ISR_scale (shower) result (scale)
    class(shower_t), intent(in) :: shower
    real(default) :: scale
    type(parton_t), pointer :: prt1, prt2
    integer :: i
    scale = zero
    do i = 1, size (shower%interactions)
       call interaction_find_partons_nearest_to_hadron &
            (shower%interactions(i)%i, prt1, prt2)
       if (.not. parton_is_simulated (prt1) .and. abs(prt1%scale) > scale) &
            scale = abs(prt1%scale)
       if (.not. parton_is_simulated (prt1) .and. abs(prt2%scale) > scale) &
            scale = abs(prt2%scale)
    end do
    ! call shower%write ()
    ! call parton_write(prt1)
    ! call parton_write(prt2)
    ! print *, "returning: ", scale
  end function shower_get_ISR_scale

  subroutine shower_set_max_isr_scale (shower, newscale)
    class(shower_t), intent(inout) :: shower
    real(default), intent(in) :: newscale
    real(default) :: scale
    type(parton_t), pointer :: prt
    integer :: i,j
    print *, "shower_set_max_isr_scale", newscale
    call shower%write ()

    if (isr_pt_ordered) then
       scale = newscale
    else
       scale = -abs(newscale)
    end if

    INTERACTIONS: do i = 1, size (shower%interactions)
       PARTONS: do j = 1, 2
          prt => shower%interactions(i)%i%partons(j)%p
          do
             if (.not. isr_pt_ordered) then
                if (prt%belongstointeraction) prt => prt%parent
             end if
               if (prt%t < scale) then
                  if (associated (prt%parent)) then
                     prt => prt%parent
                  else
                     exit   !!! unresolved prt found
                  end if
               else
                  exit   !!! prt with scale above newscale found
               end if
            end do
            if (.not. isr_pt_ordered) then
               if (prt%child1%belongstointeraction .or. &
                    parton_is_hadron (prt)) then
                  !!! don't reset scales of "first" spacelike partons
                  !!!    in virtuality ordered shower or hadrons
                  cycle
               end if
            else
               if (parton_is_hadron (prt)) then
                  !!! don't reset scales of hadrons
                  cycle
               end if
            end if
            if (isr_pt_ordered) then
               prt%scale = scale
            else
               prt%t = scale
            end if
            call parton_set_simulated (prt, .false.)
            call shower_remove_parents_and_stuff (shower, prt)
         end do PARTONS
      end do INTERACTIONS

      call shower%write ()
      print *, "shower_set_max_isr_scale finished"
    end subroutine shower_set_max_isr_scale

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!! new version
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!    subroutine shower_interaction_generate_fsr(shower, interaction)
!!!      type(shower_t), intent(inout) :: shower
!!!      type(shower_interaction_t), intent(inout) :: interaction
!!!      type(parton_pointer_t), dimension(:), allocatable :: partons   ! array of partons whose children are to be evolved
!!!
!!!      ! arrange partons to be included in <partons>
!!!      ! for qqbar state: include imaginary mother + first branching
!!!      call shower_prepare_for_simulate_fsr_ana(shower, interaction%out1%p, interaction%out2%p)
!!!
!!!      allocate(partons(1:1))
!!!      partons(1)%p => interaction%out1%p%child1
!!!      call shower_parton_pointer_array_generate_fsr(shower, partons)
!!!      call shower_parton_update_color_connections(shower, interaction%out1%p%child1%child1)
!!!      call shower_parton_update_color_connections(shower, interaction%out1%p%child1%child2)
!!!    end subroutine shower_interaction_generate_fsr

  subroutine shower_interaction_generate_fsr_2ton (shower, interaction)
    class(shower_t), intent(inout) :: shower
    type(shower_interaction_t), intent(inout) :: interaction
    type(parton_t), pointer :: prt
    prt => interaction%partons(3)%p
    do
       if (.not. associated (prt%parent)) exit
       prt => prt%parent
    end do
    call shower_parton_generate_fsr (shower, prt)
    call shower_parton_update_color_connections (shower, prt)
  end subroutine shower_interaction_generate_fsr_2ton

  subroutine shower_parton_generate_fsr (shower, prt)
    type(shower_t), intent(inout) :: shower
    type(parton_t), intent(inout), target :: prt
    type(parton_pointer_t), dimension(:), allocatable :: partons
    if (signal_is_pending ()) return
    if (.not. parton_is_branched (prt)) then
       print *, " error in shower_parton_generate_fsr: parton not branched"
       return
    end if
    if (parton_is_simulated (prt%child1) .or. &
        parton_is_simulated (prt%child2)) then
       ! print *, " error in shower_parton_generate_fsr: children
       !       already simulated for parton ", prt%nr
       ! call shower%write ()
       return
    end if

    allocate (partons(1:1))
    partons(1)%p => prt
    call shower_parton_pointer_array_generate_fsr (shower, partons)

  end subroutine shower_parton_generate_fsr

    recursive subroutine shower_parton_pointer_array_generate_fsr &
         (shower, partons)
      type(shower_t), intent(inout) :: shower
      type(parton_pointer_t), dimension(:), allocatable, intent(inout) :: &
           partons
      type(parton_pointer_t), dimension(:), allocatable :: partons_new
      integer :: i, size_partons, size_partons_new
      size_partons = size (partons)
      if (signal_is_pending ()) return
      if (size_partons == 0) return
      !!! sort partons -> necessary ?? -> probably not
      !!! simulate highest/first parton
      call shower_simulate_children_ana (shower, partons(1)%p)
      !!! check for new daughters to be included in new_partons
      size_partons_new = size_partons - 1   !!! partons(1) not needed anymore
      if (parton_is_branched (partons(1)%p%child1)) &
           size_partons_new = size_partons_new + 1
      if (parton_is_branched (partons(1)%p%child2)) &
           size_partons_new = size_partons_new + 1

      allocate (partons_new(1:size_partons_new))

      if (size_partons > 1) then
         do i = 2, size_partons
            partons_new(i - 1)%p => partons(i)%p
         end do
      end if
      if (parton_is_branched (partons(1)%p%child1)) &
           partons_new(size_partons)%p => partons(1)%p%child1
      if (parton_is_branched (partons(1)%p%child2)) then
         !!! check if child1 is already included
         if (size_partons_new == size_partons) then
            partons_new(size_partons)%p => partons(1)%p%child2
         else if (size_partons_new == size_partons + 1) then
            partons_new(size_partons + 1)%p => partons(1)%p%child2
         else
            call msg_fatal ("Shower: wrong sizes in" &
                 // "shower_parton_pointer_array_generate_fsr")
         end if
      end if
      deallocate(partons)

      ! print *, "end subroutine shower_parton_pointer_array_generate_fsr"
      call shower_parton_pointer_array_generate_fsr (shower, partons_new)

    end subroutine shower_parton_pointer_array_generate_fsr

  recursive subroutine shower_parton_update_color_connections &
       (shower, prt)
    type(shower_t), intent(inout) :: shower
    type(parton_t), intent(inout) :: prt
    real(default) :: temprand
    if (.not. associated (prt%child1) .or. &
         .not. associated (prt%child2)) return

    if (signal_is_pending ()) return
    if (parton_is_gluon (prt)) then
       if (parton_is_quark (prt%child1)) then
          !!! give the quark the colorpartner and the antiquark
          !!!     the anticolorpartner
          if (prt%child1%type > 0) then
             !!! child1 is quark, child2 is antiquark
             prt%child1%c1 = prt%c1
             prt%child2%c2 = prt%c2
          else
             !!! child1 is antiquark, child2 is quark
             prt%child1%c2 = prt%c2
             prt%child2%c1 = prt%c1
          end if
       else
          !!! g -> gg splitting -> random choosing of partners
          call tao_random_number (temprand)
          if (temprand > 0.5_default) then
             prt%child1%c1 = prt%c1
             prt%child1%c2 = shower%get_next_color_nr ()
             prt%child2%c1 = prt%child1%c2
             prt%child2%c2 = prt%c2
          else
             prt%child1%c2 = prt%c2
             prt%child2%c1 = prt%c1
             prt%child2%c2 = shower%get_next_color_nr ()
             prt%child1%c1 = prt%child2%c2
          end if
       end if
    else if (parton_is_quark (prt)) then
       if (parton_is_quark (prt%child1)) then
          if (prt%child1%type > 0) then
             !!! q -> q + g
             prt%child2%c1 = prt%c1
             prt%child2%c2 = shower%get_next_color_nr ()
             prt%child1%c1 = prt%child2%c2
          else
             !!! qbar -> qbar + g
             prt%child2%c2 = prt%c2
             prt%child2%c1 = shower%get_next_color_nr ()
             prt%child1%c2 = prt%child2%c1
          end if
       else
          if (prt%child2%type > 0) then
             !!! q -> g + q
             prt%child1%c1 = prt%c1
             prt%child1%c2 = shower%get_next_color_nr ()
             prt%child2%c1 = prt%child1%c2
          else
             !!! qbar -> g + qbar
             prt%child1%c2 = prt%c2
             prt%child1%c1 = shower%get_next_color_nr ()
             prt%child2%c2 = prt%child1%c1
          end if
       end if
    end if

    call shower_parton_update_color_connections (shower, prt%child1)
    call shower_parton_update_color_connections (shower, prt%child2)
  end subroutine shower_parton_update_color_connections

  function shower_get_pdf (shower, mother, x, Q2, daughter) result (pdf)
    class(shower_t), intent(inout) :: shower
    integer, intent(in) :: mother, daughter
    real(default), intent(in) :: x, Q2
    real(default) :: pdf
    real(double), save :: f(-6:6) = 0._double
    real(double), save :: lastx, lastQ2 = 0._double

    if (abs (mother) /= 2212) then
       pdf = zero
       call msg_fatal ("Shower: pdf only implemented for (anti-)proton")
    else
       if (x > zero .and. x < one) then
          if (dble(Q2) /= lastQ2 .or. dble(x) /= lastx) then
             call shower%pdf_func &
                  (shower_pdf_set, dble(x), sqrt (abs (dble(Q2))), f)
          end if
          if (abs (daughter) >= 1 .and. abs (daughter) <= 6) then
             pdf = max (f(daughter * sign (1,mother)), 1E-10_default) / x
          else if (daughter == 21) then
             pdf = max (f(0), 1E-10_default) / x
          else
             print *, "error in pdf, unknown daughter", daughter
             pdf = zero
          end if
       else
          pdf = zero
       end if
    end if
    lastQ2 = dble(Q2)
    lastx  = dble(x)
  end function shower_get_pdf

  function shower_get_xpdf (shower, mother, x, Q2, daughter) result (pdf)
    class(shower_t), intent(inout) :: shower
    integer, intent(in) :: mother, daughter
    real(default), intent(in) :: x, Q2
    real(default) :: pdf
    real(double), save :: f(-6:6) = 0._double
    real(double), save :: lastx, lastQ2 =0._double

    if (abs (mother) /= 2212) then
       pdf = zero
       call msg_fatal ("Shower: pdf only implemented for (anti-)proton")
    else
       if (x > zero .and. x < one) then
          if (dble(Q2) /= lastQ2 .or. dble(x) /= lastx) then
             call shower%pdf_func &
                  (shower_pdf_set, dble(x), sqrt (abs (dble(Q2))), f)
          end if
          if (abs(daughter) >= 1 .and. abs(daughter) <= 6) then
             pdf = max (f (daughter * sign (1,mother)), 1E-10_default)
          else if (daughter == 21) then
             pdf = max(f(0), 1E-10_default)
          else
             print *, "error in pdf, unknown daughter", daughter
             pdf = zero
          end if
       else
          pdf = zero
       end if
    end if
    lastQ2 = dble(Q2)
    lastx  = dble(x)
  end function shower_get_xpdf

  subroutine shower_pdf_func (shower, set, x, q2, f)
    class(shower_t), intent(inout) :: shower
    integer, intent(in) :: set
    real(double) :: x, q2
    real(double), dimension(-6:6), intent(out) :: f
    select case (shower_pdf_type)
    case (STRF_PDF_BUILTIN) 
       call pdf_evolve_LHAPDF (set, x, q2, f)
    case (STRF_LHAPDF6)
       call shower%pdf%evolve_pdfm (x, q2, f)
    case (STRF_LHAPDF5)       
       call evolvePDFM (set, x, q2, f)
    case default
       call msg_fatal ("Shower PDF function: unknown PDF method.")   
    end select
  end subroutine shower_pdf_func

end module shower_core
