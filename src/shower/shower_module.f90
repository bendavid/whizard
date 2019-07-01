!!! module: shower_module
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
!!! Latest Change: Thu Jan 13 17:20:53 2011 Time zone: 3600 seconds
!!! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module shower_module

  use kinds, only: default !NODEP!
  use constants, only: pi, twopi !NODEP!
  use shower_basics_module
  use shower_parton_module
  use lorentz !NODEP!

  implicit none
!  private

  type :: my_interaction_t
     type(parton_pointer_t), dimension(:), allocatable :: partons
  end type my_interaction_t

  type :: interaction_pointer_t
     type(my_interaction_t), pointer :: i => null()
  end type interaction_pointer_t

  type :: shower_t
     type(interaction_pointer_t), dimension(:), allocatable :: interactions
     type(parton_pointer_t), dimension(:), allocatable :: partons
     integer :: next_free_nr
     integer :: next_color_nr
  end type shower_t
  
  public :: shower_t
  public :: my_interaction_t

  public :: shower_create
  public :: shower_final
  public :: shower_print
  public :: shower_add_interaction2ton
  public :: shower_interaction_generate_fsr2ton
  public :: shower_get_next_free_nr
  public :: shower_get_next_color_nr
  public :: shower_get_final_partons
  public :: shower_generate_next_isr_branching
  public :: shower_simulate_no_isr_shower
  public :: shower_simulate_no_fsr_shower

  public :: interaction_get_shat
  public :: interaction_get_s

contains

    subroutine shower_add_interaction2ton(shower, partons)
      type(shower_t), intent(inout) :: shower
      type(parton_pointer_t), intent(in), dimension(:), allocatable :: partons

      integer :: n_partons, n_out, n_partons_shower
      integer :: i,j, imin, jmin
      real(default) :: y, ymin ! the jet measure
!      real(default) :: s ! s of the interaction
      type(parton_pointer_t), dimension(:), allocatable :: new_partons
      type(parton_t), pointer :: prt
      integer :: n_interactions
      type(interaction_pointer_t), dimension(:), allocatable :: temp
      type(vector4_t) :: prtmomentum, childmomentum
      logical :: isr_is_possible
      type(lorentz_transformation_t) :: L

      n_partons = size(partons)
      n_out = n_partons-2
      if(n_out < 2) then
         STOP "BUG: trying to add a 2-> (something<2) interaction"
      end if
!      print *, " adding a 2-> ", n_out, " interaction"

      ! if ISR is possible (-> initial parton set, lepton-hadron not implemented)
      isr_is_possible = (associated(partons(1)%p%initial)) .and. (associated(partons(2)%p%initial))

      ! workaround: as WHIZARD can treat partons as massless, there might be partons with E<m
      ! if such a parton is found, quarks will be treated massless
      if(associated(partons(1)%p%initial).and.parton_is_quark(partons(1)%p)) then
         if(parton_get_energy(partons(1)%p) .lt. 2._double*parton_mass(partons(1)%p)) then
            if(abs(partons(1)%p%typ).lt.2) then
               treat_light_quarks_massless = .true.
            else
               treat_duscb_quarks_massless = .true.
            end if
         end if
      end if
      if(associated(partons(2)%p%initial).and.parton_is_quark(partons(2)%p)) then
         if(parton_get_energy(partons(2)%p) .lt. 2._double*parton_mass(partons(2)%p)) then
            if(abs(partons(2)%p%typ).lt.2) then
               treat_light_quarks_massless = .true.
            else
               treat_duscb_quarks_massless = .true.
            end if
         end if
      end if
      
      ! add a new interaction to shower%itneractions
      if(allocated(shower%interactions)) then
         n_interactions=size(shower%interactions)+1
      else
         n_interactions=1
      end if
      allocate(temp(1:n_interactions))
      do i=1, n_interactions-1
         allocate(temp(i)%i)
         temp(i)%i=shower%interactions(i)%i
      end do
      allocate(temp(n_interactions)%i)
      allocate(temp(n_interactions)%i%partons(1:n_partons))
      do i=1, n_partons
         allocate(temp(n_interactions)%i%partons(i)%p)
         call parton_copy(partons(i)%p, temp(n_interactions)%i%partons(i)%p)
      end do
      if(allocated(shower%interactions)) deallocate(shower%interactions)
      allocate(shower%interactions(1:n_interactions))
      do i=1, n_interactions
         shower%interactions(i)%i=>temp(i)%i
      end do
      deallocate(temp)

      if(associated(shower%interactions(n_interactions)%i%partons(1)%p%initial))  &
           call parton_set_simulated(shower%interactions(n_interactions)%i%partons(1)%p%initial)
      if(associated(shower%interactions(n_interactions)%i%partons(2)%p%initial))  &
           call parton_set_simulated(shower%interactions(n_interactions)%i%partons(2)%p%initial)
      if(isr_is_possible) then
         ! boost to the CMFrame of the incoming partons
         L = boost( -(shower%interactions(n_interactions)%i%partons(1)%p%momentum+&
              shower%interactions(n_interactions)%i%partons(2)%p%momentum), &
              (shower%interactions(n_interactions)%i%partons(1)%p%momentum +&
              shower%interactions(n_interactions)%i%partons(2)%p%momentum)**1 )
         do i=1, n_partons
            call parton_apply_lorentztrafo(shower%interactions(n_interactions)%i%partons(i)%p, L)
         end do
      end if
      do i=1, size(partons)
         ! ensure that partons are marked as belonging to the hard interaction
         shower%interactions(n_interactions)%i%partons(i)%p%belongstointeraction = .true.
         ! ensure that incoming partons are marked as belonging to ISR
         if(i.le.2) shower%interactions(n_interactions)%i%partons(i)%p%belongstoFSR = .false.
      end do

      ! Add partons to shower%partons
      if(allocated(shower%partons)) then
         allocate(new_partons(1:size(shower%partons)+size(shower%interactions(n_interactions)%i%partons)))
         do i=1, size(shower%partons)
            new_partons(i)%p => shower%partons(i)%p
         end do
         do i=1, size(shower%interactions(n_interactions)%i%partons)
            new_partons(size(shower%partons)+i)%p => shower%interactions(n_interactions)%i%partons(i)%p
         end do
         deallocate(shower%partons)
      else
         allocate(new_partons(1:size(shower%interactions(n_interactions)%i%partons)))
         do i=1, size(partons)
            new_partons(i)%p => shower%interactions(n_interactions)%i%partons(i)%p
         end do
      end if
      allocate(shower%partons(1:size(new_partons)))
      do i=1, size(new_partons)
         shower%partons(i)%p => new_partons(i)%p
      end do
      deallocate(new_partons)
      call shower_print(shower)
!      pause

      if(isr_is_possible) then
         if(isr_pt_ordered) then
            call shower_prepare_for_simulate_isr_pt(shower, shower%interactions(size(shower%interactions))%i)
         else
            call shower_prepare_for_simulate_isr_ana_test(shower, shower%interactions(n_interactions)%i%partons(1)%p, & 
                                                             shower%interactions(n_interactions)%i%partons(2)%p)
         end if
      end if
      
      ! generate pseudo parton shower history and add all partons to shower%partons-array 
      ! TODO initial -> initial + final branchings ??
      allocate(new_partons(1:(n_partons-2)))
      do i=1, size(new_partons)
         nullify(new_partons(i)%p)
      end do
      do i=1, size(new_partons)
         new_partons(i)%p => shower%interactions(n_interactions)%i%partons(i+2)%p
      end do
      clustering: do
         ! search for the partons to be clustered together
         ymin=0._default
         outer: do i = 1, size(new_partons)
            inner: do j = i+1, size(new_partons)
               ! calculate the jet measure
               if(.not.associated(new_partons(i)%p)) cycle
               if(.not.associated(new_partons(j)%p)) cycle
               if(.not. shower_clustering_allowed(shower, shower%interactions(n_interactions)%i%partons, i,j)) cycle inner
               ! Durham jet-measure ! don't care about constants
               y = min(parton_get_energy(new_partons(i)%p),&
                    parton_get_energy(new_partons(j)%p))* &
                    (1._default -enclosed_angle_ct(new_partons(i)%p%momentum,&
                    new_partons(j)%p%momentum))
               if(y<ymin .or. ymin==0._default) then
                  ymin = y
                  imin = i
                  jmin = j
               end if
            end do inner
         end do outer
         if(ymin.gt.0._default) then
            call shower_add_parent(shower, new_partons(imin)%p)
            call parton_set_child(new_partons(imin)%p%parent, new_partons(jmin)%p, 2)
            call parton_set_parent(new_partons(jmin)%p, new_partons(imin)%p%parent)
            prt=>new_partons(imin)%p%parent
            prt%nr = shower_get_next_free_nr(shower)
            prt%typ = 94                 ! something for internal use needed, 81-100 should be reserved for internal purposes
            
            prt%momentum = shower%interactions(n_interactions)%i%partons(imin)%p%momentum &
                 + shower%interactions(n_interactions)%i%partons(jmin)%p%momentum
            prt%t = prt%momentum**2
            ! TODO -> calculate costheta and store it for later use in generate_ps
               
            if(space_part_norm(prt%momentum) > 1D-10) then
               prtmomentum = prt%momentum
               childmomentum = prt%child1%momentum
               prtmomentum = boost(-parton_get_beta(prt)/sqrt(1._default-(parton_get_beta(prt))**2), & 
                    space_part(prt%momentum)/space_part_norm(prt%momentum)) * prtmomentum 
               childmomentum = boost(-parton_get_beta(prt)/sqrt(1._default-(parton_get_beta(prt))**2), &
                    space_part(prt%momentum)/space_part_norm(prt%momentum)) * childmomentum
               prt%costheta = enclosed_angle_ct(prtmomentum, childmomentum)
            else
               prt%costheta=-1._default
            end if
            
            prt%belongstointeraction = .true.
            nullify(new_partons(imin)%p)
            nullify(new_partons(jmin)%p)
            new_partons(imin)%p => prt
         else
            exit clustering
         end if
      end do clustering
      
      ! add all partons to the shower
      n_partons_shower=0
      if(allocated(shower%partons)) then
         do i=1, size(shower%partons)
            if(associated(shower%partons(i)%p)) n_partons_shower=n_partons_shower + 1
         end do
      end if

      ! set the FSR starting scale for all partons
      do i=1, size(new_partons)
         ! the imaginary mother is the only parton remaining in new_partons
         if(.not.associated(new_partons(i)%p)) cycle

         call set_starting_scale(new_partons(i)%p, get_starting_scale(new_partons(i)%p))
         exit
      end do

      deallocate(new_partons)

!!$      call shower_print(shower)
!!$      print *, "end of shower_interactionadd2ton"
!!$      pause
    contains
      logical function shower_clustering_allowed(shower, partons, i, j)
        type(shower_t), intent(inout) :: shower
        type(parton_pointer_t), intent(in), dimension(:), allocatable :: partons
        integer, intent(in) :: i, j

        ! TODO implement checking if clustering is allowed, e.g. in e+e- -> qqg don't cluster the quarks together first
        shower_clustering_allowed = .true.
      end function shower_clustering_allowed

      recursive subroutine transfer_pointers(destiny, start, prt)
        type(parton_pointer_t), dimension(:), allocatable :: destiny
        integer, intent(inout) :: start
        type(parton_t), pointer :: prt
        
        destiny(start)%p => prt
        start=start+1
        if(associated(prt%child1)) then
           call transfer_pointers(destiny, start, prt%child1)
        end if
        if(associated(prt%child2)) then
           call transfer_pointers(destiny, start, prt%child2)
        end if
      end subroutine transfer_pointers

      recursive function get_starting_scale(prt) result(scale)
        type(parton_t), pointer :: prt
        real(default) :: scale

        scale = huge(scale)
        if(associated(prt%child1).and.associated(prt%child2)) then
           scale = min(scale, prt%t)
        end if
        if(associated(prt%child1)) then
           scale = min(scale, get_starting_scale(prt%child1))
        end if
        if(associated(prt%child2)) then
           scale = min(scale, get_starting_scale(prt%child2))
        end if
      end function get_starting_scale

      recursive subroutine set_starting_scale(prt, scale)
        type(parton_t), pointer :: prt
        real(default) :: scale

        if(prt%typ .ne. 94) then
           if(scale > D_Min_t + parton_mass_squared(prt)) then
              prt%t = scale
           else
              prt%t = parton_mass_squared(prt)
              call parton_set_simulated(prt)
           end if
        end if
        if(associated(prt%child1)) then
           call set_starting_scale(prt%child1, scale)
        end if
        if(associated(prt%child2)) then
           call set_starting_scale(prt%child2, scale)
        end if
      end subroutine set_starting_scale
    end subroutine shower_add_interaction2ton

    subroutine shower_simulate_no_isr_shower(shower)
      type(shower_t), intent(inout) :: shower
      integer :: i,j
      type(parton_t), pointer :: prt

      do i=1, size(shower%interactions)
         do j=1,2
            prt=>shower%interactions(i)%i%partons(j)%p
            if(associated(prt%initial)) then
               ! for virtuality ordered: remove unneeded partons
               if(associated(prt%parent)) then
                  if(parton_is_hadron(prt%parent).eqv..false.) call shower_remove_parton_from_partons(shower, prt%parent)
               end if

               call parton_set_parent(prt, prt%initial)
               call parton_set_child(prt%initial, prt, 1)
               if(associated(prt%initial%child2)) then
                  call shower_remove_parton_from_partons(shower,prt%initial%child2)
                  deallocate(prt%initial%child2)
               end if
               call shower_add_child(shower, prt%initial, 2)
            end if
         end do
      end do
    end subroutine shower_simulate_no_isr_shower

    subroutine shower_simulate_no_fsr_shower(shower)
      type(shower_t), intent(inout) :: shower
      integer :: i, j
      type(parton_t), pointer :: prt

      do i=1, size(shower%interactions)
         do j=3,size(shower%interactions(i)%i%partons)
            prt=>shower%interactions(i)%i%partons(j)%p
            call parton_set_simulated(prt)
            prt%scale = 0._double
            prt%t = parton_mass_squared(prt)
         end do
      end do
    end subroutine shower_simulate_no_fsr_shower

    subroutine swap_pointers(prtp1, prtp2)
      type(parton_pointer_t), intent(inout) :: prtp1, prtp2
      type(parton_pointer_t) :: prtptemp

      prtptemp%p=>prtp1%p
      prtp1%p=>prtp2%p
      prtp2%p=>prtptemp%p

    end subroutine swap_pointers

    subroutine shower_remove_parton_from_partons(shower, prt)
      type(shower_t), intent(inout) :: shower
      type(parton_t), pointer :: prt
      integer :: i

      if((prt%belongstoFSR .eqv. .false.).and.(associated(prt%child2))) then
         ! remove emitted timelike partons
         call shower_remove_parton_from_partons_recursive(shower, prt%child2)
      end if
      do i=1, size(shower%partons)
         if(associated(shower%partons(i)%p, prt)) then
            shower%partons(i)%p=>null()
            exit
         end if
         if(i.eq.size(shower%partons)) then
!            stop "Bug: parton to be removed not found"
         end if
      end do
    end subroutine shower_remove_parton_from_partons

    recursive subroutine shower_remove_parton_from_partons_recursive(shower, prt)
      ! remove prt and all its children
      type(shower_t), intent(inout) :: shower
      type(parton_t), pointer :: prt

      if(associated(prt%child1)) then
         call shower_remove_parton_from_partons_recursive(shower, prt%child1)
         deallocate(prt%child1)
      end if
      if(associated(prt%child2)) then
         call shower_remove_parton_from_partons_recursive(shower, prt%child2)
         deallocate(prt%child2)
      end if
      call shower_remove_parton_from_partons(shower, prt)
    end subroutine shower_remove_parton_from_partons_recursive

    subroutine shower_sort_partons(shower)
      type(shower_t), intent(inout) :: shower
      integer i,j, maxsort, size_partons
      logical :: changed

!!$      print *, " shower_sort_partons"

      if(.not.allocated(shower%partons)) return
      size_partons=size(shower%partons)
      do i=1, size_partons
         if(associated(shower%partons(i)%p)) maxsort=i
      end do

      size_partons=size(shower%partons)
      if(size_partons<=1) return

      do i=1, maxsort
         if(.not. associated(shower%partons(i)%p)) cycle
         if(isr_pt_ordered .eqv. .false.) then
            ! set unsimulated ISR partons to be "typeless" to prevent influences from "wrong" masses
            if( (shower%partons(i)%p%belongstoFSR.eqv. .false.) .and.  (parton_is_simulated(shower%partons(i)%p).eqv. .false.) & 
                 .and. (shower%partons(i)%p%belongstointeraction .eqv. .false.)) then
               shower%partons(i)%p%typ=0
            end if
         end if
      end do
      ! just a Bubblesort
      ! different algorithms needed for t-ordered and pt^2-ordered shower
      if(isr_pt_ordered) then ! pt-ordered
         outerdo_pt: do i=1, maxsort-1
            changed=.false.
            innerdo_pt: do j=1, maxsort-i
               if(.not. associated(shower%partons(j+1)%p)) cycle
               
               if(.not. associated(shower%partons(j)%p)) then
                  ! change if j+1 ist assoaciated and j isn't
                  call swap_pointers(shower%partons(j), shower%partons(j+1))
                  changed=.true.
               else if(shower%partons(j)%p%scale < shower%partons(j+1)%p%scale) then
                  call swap_pointers(shower%partons(j), shower%partons(j+1))
                  changed=.true.
               else if(shower%partons(j)%p%scale .eq. shower%partons(j+1)%p%scale) then
                  if(shower%partons(j)%p%nr >shower%partons(j+1)%p%nr) then
                     call swap_pointers(shower%partons(j), shower%partons(j+1))
                     changed=.true.
                  end if
               end if
            end do innerdo_pt
            if(changed.eqv..false.) exit outerdo_pt
         end do outerdo_pt
      else ! |t|-ordered
         outerdo_t: do i=1, maxsort-1
            changed=.false.
            innerdo_t: do j=1, maxsort-i
               if(.not. associated(shower%partons(j+1)%p)) cycle
               
               if(.not. associated(shower%partons(j)%p)) then
                  ! change if j+1 ist assoaciated and j isn't
                  call swap_pointers(shower%partons(j), shower%partons(j+1))
                  changed=.true.
               else if((shower%partons(j)%p%belongstointeraction.eqv..false.) .and. &
                    (shower%partons(j+1)%p%belongstointeraction.eqv..true.)) then
                  ! move partons belonging to the interaction to the front
                  call swap_pointers(shower%partons(j), shower%partons(j+1))
                  changed=.true.
               else if( (shower%partons(j)%p%belongstointeraction.eqv..false.) .and. &
                    (shower%partons(j+1)%p%belongstointeraction.eqv..false.) ) then
                  if(abs(shower%partons(j)%p%t)-parton_mass_squared(shower%partons(j)%p) < &
                       abs(shower%partons(j+1)%p%t)-parton_mass_squared(shower%partons(j+1)%p)) then
                     call swap_pointers(shower%partons(j), shower%partons(j+1))
                     changed=.true.
                  else
                     if(abs(shower%partons(j)%p%t)-parton_mass_squared(shower%partons(j)%p) .eq. &
                          abs(shower%partons(j+1)%p%t)-parton_mass_squared(shower%partons(j+1)%p)) then
                        if(shower%partons(j)%p%nr >shower%partons(j+1)%p%nr) then
                           call swap_pointers(shower%partons(j), shower%partons(j+1))
                           changed=.true.
                        end if
                     end if
                  end if
               end if
            end do innerdo_t
            if(changed.eqv..false.) exit outerdo_t
         end do outerdo_t
      end if
      
!!$      print *, "  shower_sort_partons finished"
    end subroutine shower_sort_partons

    !!! creation and finalization

    subroutine shower_create(shower)
      type(shower_t), intent(inout) :: shower
      
      shower%next_free_nr=1
      shower%next_color_nr=1
      if(allocated(shower%interactions)) then
         STOP "Bug: creating new shower while old one still associated (interactions)"
      end if
      if(allocated(shower%partons)) then
         STOP "Bug: creating new shower while old one still associated (partons)"
      end if
      treat_light_quarks_massless = .false.
      treat_duscb_quarks_massless = .false.
    end subroutine shower_create

    subroutine shower_final(shower)
      type(shower_t), intent(inout) :: shower
      integer :: i

      if(.not. allocated(shower%interactions)) then
         return
      end if

!!$      ! deallocate hadrons
!!$      if(associated(shower%interactions(1)%i%partons(1)%p%initial)) deallocate(shower%interactions(1)%i%partons(1)%p%initial)
!!$      if(associated(shower%interactions(1)%i%partons(2)%p%initial)) deallocate(shower%interactions(1)%i%partons(2)%p%initial)

      ! deallocate interaction pointers
      do i=1, size(shower%interactions)
         if(allocated(shower%interactions(i)%i%partons)) deallocate (shower%interactions(i)%i%partons)
         deallocate(shower%interactions(i)%i)
      end do

!!$      ! deallocate partons
!!$      do i=1, size(shower%partons)
!!$         if(associated(shower%partons(i)%p)) then
!!$            deallocate(shower%partons(i)%p)
!!$         end if
!!$      end do
      deallocate(shower%interactions)
      deallocate(shower%partons)

    end subroutine shower_final

    !!! bookkeeping

    function shower_get_next_free_nr(shower) result(next_number)
      type(shower_t), intent(inout) :: shower
      integer :: next_number

      next_number = shower%next_free_nr
      shower%next_free_nr = shower%next_free_nr+1
    end function shower_get_next_free_nr

    subroutine shower_set_next_color_nr(shower, index)
      type(shower_t), intent(inout) :: shower
      integer, intent(in) :: index

      if(index < shower%next_color_nr) then
         print *, " error in showeer_set_next_color_nr"
         STOP
      else
         shower%next_color_nr=max(shower%next_color_nr, index)
      end if
    end subroutine shower_set_next_color_nr

    function shower_get_next_color_nr(shower) result(next_color)
      type(shower_t), intent(inout) :: shower
      integer :: next_color

      next_color = shower%next_color_nr
      shower%next_color_nr = shower%next_color_nr+1
    end function shower_get_next_color_nr

    subroutine shower_enlarge_partons_array(shower, length)
      type(shower_t), intent(inout) :: shower
      integer, intent(in) :: length
      
      integer :: i, oldlength
      type(parton_pointer_t), dimension(:), allocatable :: new_partons

!      print *, "shower_enlarge_partons_array ", length

      if(length>0) then
         if(allocated(shower%partons)) then
            oldlength=size(shower%partons)
            allocate(new_partons(1:oldlength))
            do i=1, oldlength
               new_partons(i)%p=>shower%partons(i)%p
            end do
            
            deallocate(shower%partons)
         else
            oldlength = 0
         end if
         allocate(shower%partons(1:oldlength+length))
         do i=1, oldlength
            shower%partons(i)%p=>new_partons(i)%p
         end do
         do i=oldlength+1, oldlength+length
            shower%partons(i)%p => null()
         end do
      else
         stop "Bug: no parton_pointers added in shower%partons"
      end if
         
!      print *, "  shower_enlarge_partons_array finished"
    end subroutine shower_enlarge_partons_array

    subroutine shower_add_child(shower, prt, child)
      type(shower_t), intent(inout) :: shower
      type(parton_t), pointer :: prt
!      type(parton_t), intent(inout), target :: prt
      integer, intent(in) :: child

      integer :: i, lastfree
      type(parton_pointer_t) :: newprt

!      print *, " shower_add_child for parton ", prt%nr

      if(child.ne.1 .and. child.ne.2) then
         stop "BUG: Adding child in nonexisting place"
      end if
      
      allocate(newprt%p)
      newprt%p%nr=shower_get_next_free_nr(shower)

      ! add new parton as child
      if(child .eq. 1) then
         prt%child1=>newprt%p
      else
         prt%child2=>newprt%p
      end if
      newprt%p%parent=>prt

      ! add new parton to shower%partons list
      if(associated(shower%partons(size(shower%partons))%p)) then
         call shower_enlarge_partons_array(shower, 10)
      end if

      ! find last free pointer and let it point to the new parton
      lastfree=0
      do i=size(shower%partons), 1, -1
         if(.not. associated(shower%partons(i)%p)) then
            lastfree=i
         end if
      end do
      if(lastfree.eq.0) then
         stop "BUG: no free pointers found"
      end if
      shower%partons(lastfree)%p => newprt%p
    end subroutine shower_add_child

    subroutine shower_add_parent(shower, prt)
      type(shower_t), intent(inout) :: shower
      type(parton_t), intent(inout), target :: prt

      integer :: i, lastfree
      type(parton_pointer_t) :: newprt

!      print *, " shower_add_parent for parton ", prt%nr

      allocate(newprt%p)
      newprt%p%nr=shower_get_next_free_nr(shower)

      ! add new parton as parent
      newprt%p%child1=>prt
      prt%parent=>newprt%p

      ! add new parton to shower%partons list
      if(.not. allocated(shower%partons)) then
         call shower_enlarge_partons_array(shower, 10)
      else if(associated(shower%partons(size(shower%partons))%p)) then
         call shower_enlarge_partons_array(shower, 10)
      end if

      ! find last free pointer and let it point to the new parton
      lastfree=0
      do i=size(shower%partons), 1, -1
         if(.not. associated(shower%partons(i)%p)) then
            lastfree=i
         end if
      end do
      if(lastfree.eq.0) then
         stop "BUG: no free pointers found"
      end if
      shower%partons(lastfree)%p => newprt%p
!      print *, "  shower_add_parent finished"
    end subroutine shower_add_parent

    function shower_get_total_momentum(shower, c) result (mom)
      type(shower_t), intent(in) :: shower
      integer, intent(in) :: c
      real(default) :: mom
      integer :: i

      mom=0._default
      if(.not.allocated(shower%partons)) return
      do i=1, size(shower%partons)
         if(.not. associated(shower%partons(i)%p)) cycle
         if(parton_is_final(shower%partons(i)%p)) then
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
               stop "Bug: wrong component of 4momentum"
            end select
         end if
      end do
    end function shower_get_total_momentum

    function shower_get_nr_of_partons(shower, mine, include_remnants) result(nr)
      type(shower_t), intent(in) :: shower
      real(default), intent(in), optional :: mine
      logical, intent(in), optional :: include_remnants
      integer :: nr

      integer :: i
      type(parton_t), pointer :: prt
      real(default) :: minenergy

      nr = 0
      if(present(mine)) then
         minenergy=mine
      else
         minenergy = 0._default
      end if

      do i=1, size(shower%partons)
         prt=>shower%partons(i)%p
         if(.not. associated(prt)) cycle
         if(.not. parton_is_final(prt)) cycle
         if(prt%typ.eq.9999) then
            if(present(include_remnants)) then
               if (include_remnants .eqv. .false.) cycle
            end if
         end if
         if(present(mine)) then
            if(parton_get_energy(prt)>mine) then
               nr = nr + 1
            end if
         else
            nr = nr +1
         end if
      end do

    end function shower_get_nr_of_partons

    subroutine shower_get_final_partons(shower, partons, include_remnants)
      type(shower_t), intent(in) :: shower
      type(parton_pointer_t), dimension(:), allocatable, intent(inout) :: partons
      logical, intent(in), optional :: include_remnants

      integer :: i, j
      type(parton_t), pointer :: prt

      if(allocated(partons)) deallocate(partons)

      allocate(partons(1:shower_get_nr_of_partons(shower, include_remnants=include_remnants)))
      j=0

      do i=1, size(shower%partons)
         prt=>shower%partons(i)%p
         if(.not. associated(prt)) cycle
         if(.not. parton_is_final(prt)) cycle
         if(prt%typ.eq.9999) then ! remnant
            if(present(include_remnants)) then
               if(include_remnants .eqv. .false.) cycle
            end if
         end if
         j=j+1
         partons(j)%p => prt
      end do
      
    end subroutine shower_get_final_partons

    recursive function interaction_fsr_is_finished_for_parton(prt) result(finished)
      type(parton_t), intent(in) :: prt
      logical :: finished

      if(prt%belongstoFSR) then
         ! FSR partons
         if(associated(prt%child1)) then
            finished = interaction_fsr_is_finished_for_parton(prt%child1) .and. interaction_fsr_is_finished_for_parton(prt%child2)
         else
            finished = (prt%t <= parton_mass_squared(prt))
         end if
      else
         ! search for emitted timelike partons in ISR shower
         if(.not. associated(prt%initial)) then
            ! no inital -> no ISR
            finished = .true.
         else if(.not. associated(prt%parent)) then
            finished = .false.
         else
            if(.not. parton_is_hadron(prt%parent)) then
               if(associated(prt%child2)) then
                  finished = interaction_fsr_is_finished_for_parton(prt%parent) .and. & 
                             interaction_fsr_is_finished_for_parton(prt%child2)
               else
                  finished = interaction_fsr_is_finished_for_parton(prt%parent)
               end if
            else
               if(associated(prt%child2)) then
                  finished = interaction_fsr_is_finished_for_parton(prt%child2)
               else
                  ! only second partons can come here -> if that happens fsr evolution is not existing
                  finished = .true.
               end if
            end if
         end if
      end if
    end function interaction_fsr_is_finished_for_parton

    function interaction_fsr_is_finished(interaction) result(finished)
      type(my_interaction_t), intent(in) :: interaction
      logical :: finished
      integer :: i

      finished=.true.
      if(.not.allocated(interaction%partons)) return
      do i=1, size(interaction%partons)
         if(interaction_fsr_is_finished_for_parton(interaction%partons(i)%p) .eqv. .false.) then
            finished = .false.
            exit
         end if
      end do
    end function interaction_fsr_is_finished

    function interaction_get_shat(interaction) result(shat)
      type(my_interaction_t), intent(in) :: interaction
      real(kind=default) :: shat

      shat = (interaction%partons(1)%p%momentum + &
              interaction%partons(2)%p%momentum)**2
    end function interaction_get_shat

    function interaction_get_s(interaction) result(s)
      type(my_interaction_t), intent(in) :: interaction
      real(kind=default) :: s

      s = (interaction%partons(1)%p%initial%momentum + &
           interaction%partons(2)%p%initial%momentum)**2
    end function interaction_get_s

    function shower_fsr_is_finished(shower) result(finished)
      type(shower_t), intent(in) :: shower
      logical :: finished
      integer :: i

      finished=.true.
      if(.not.allocated(shower%interactions)) return
      do i=1, size(shower%interactions)
         if(interaction_fsr_is_finished(shower%interactions(i)%i) .eqv. .false.) then
            finished=.false.
            exit
         end if
      end do
    end function shower_fsr_is_finished

    function shower_isr_is_finished(shower) result(finished)
      type(shower_t), intent(in) :: shower
      logical :: finished

      integer :: i
      type(parton_t), pointer :: prt

      finished=.true.
      if(.not.allocated(shower%partons)) return
      do i=1, size(shower%partons)
         if(.not. associated(shower%partons(i)%p)) cycle
         prt=>shower%partons(i)%p
         if(isr_pt_ordered) then
            if((prt%belongstoFSR.eqv..false.) .and. (parton_is_simulated(prt).eqv..false.) .and. (prt%scale>0._default)) then
               finished=.false.
               exit
            end if
         else
            if((prt%belongstoFSR.eqv..false.) .and. (parton_is_simulated(prt).eqv..false.) .and. (prt%t<0._default)) then
               finished=.false.
               exit
            end if
         end if
      end do
    end function shower_isr_is_finished

    function shower_is_finished(shower) result(finished)
      type(shower_t), intent(in) :: shower
      logical :: finished

      finished=(shower_isr_is_finished(shower)) .and. (shower_fsr_is_finished(shower))
    end function shower_is_finished

    subroutine interaction_find_partons_nearest_to_hadron(interaction, prt1, prt2)
      type(my_interaction_t), intent(in) :: interaction
      type(parton_t), pointer :: prt1, prt2

      prt1=>null()
      prt2=>null()
    
      prt1=>interaction%partons(1)%p
      do
         if(associated(prt1%parent)) then
            if(parton_is_hadron(prt1%parent)) then
               exit
            else if( ((isr_pt_ordered.eqv..false.).and.(parton_is_simulated(prt1%parent).eqv..false.)) .or. & 
                 ((isr_pt_ordered).and.(parton_is_simulated(prt1).eqv..false.)) ) then
               exit
            else
               prt1=>prt1%parent
            end if
         else
            exit
         end if
      end do
      prt2=>interaction%partons(2)%p
      do
         if(associated(prt2%parent)) then
            if(parton_is_hadron(prt2%parent)) then
               exit
            else if( ((isr_pt_ordered.eqv..false.).and.(parton_is_simulated(prt2%parent).eqv..false.)) .or. &
                 ((isr_pt_ordered).and.(parton_is_simulated(prt2).eqv..false.)) ) then
               exit
            else
               prt2=>prt2%parent
            end if
         else
            exit
         end if
      end do
    end subroutine interaction_find_partons_nearest_to_hadron

    subroutine shower_update_beamremnants(shower)
      type(shower_t), intent(inout) :: shower
      type(parton_t), pointer :: hadron, remnant
      integer :: i
      real(kind=default) :: rand

      ! only proton in first interaction !!?
      ! currently only first beam-remnant will be updated

      do i=1,2
         if(associated(shower%interactions(1)%i%partons(i)%p%initial)) then
            hadron=>shower%interactions(1)%i%partons(i)%p%initial
         else
            cycle
         end if

         remnant => hadron%child2
         if(associated(remnant)) then
            remnant%momentum = hadron%momentum - hadron%child1%momentum
         end if
         
         ! generate flavour of the beam-remnant if beam was proton
         if(abs(hadron%typ).eq. 2212 .and. associated(hadron%child1)) then
            if(parton_is_quark(hadron%child1)) then
               ! decide if valence (u,d) or sea quark (s,c,b)
               if((abs(hadron%child1%typ) .le. 2) .and. &
                    (hadron%typ*hadron%child1%typ .gt. 0._default)) then
                  ! valence quark
                  if(abs(hadron%child1%typ).eq.1) then
                     ! if d then remaining diquark is uu_1
                     remnant%typ = sign(2203, hadron%typ)
                  else
                     call tao_random_number(rand)
                     ! if u then remaining diqaurk is either
                     if(rand < 0.75_default) then
                        ! with 75% a ud_0
                        remnant%typ = sign(2101, hadron%typ)
                     else
                        ! with 25% a ud_1
                        remnant%typ = sign(2103, hadron%typ)
                     end if
                  end if
                  remnant%c1=hadron%child1%c2
                  remnant%c2=hadron%child1%c1
               else if((hadron%typ*hadron%child1%typ) .lt. 0._default) then
                  ! antiquark
                  if(.not.associated(remnant%child1)) then
                     call shower_add_child(shower, remnant, 1)
                  end if
                  if(.not.associated(remnant%child2)) then
                     call shower_add_child(shower, remnant, 2)
                  end if
                  call tao_random_number(rand)
                  if(rand < 0.6666_default) then
                     ! 2/3 into udq + u
                     if(abs(hadron%child1%typ).eq.1) then
                        remnant%child1%typ = sign(2112, hadron%typ)
                     else if(abs(hadron%child1%typ).eq.2) then
                        remnant%child1%typ = sign(2212, hadron%typ)
                     else if(abs(hadron%child1%typ).eq.3) then
                        remnant%child1%typ = sign(3212, hadron%typ)
                     else if(abs(hadron%child1%typ).eq.4) then
                        remnant%child1%typ = sign(4212, hadron%typ)
                     else if(abs(hadron%child1%typ).eq.5) then
                        remnant%child1%typ = sign(5212, hadron%typ)
                     end if
                     remnant%child2%typ = sign(2, hadron%typ)
                  else
                     ! 1/3 into uuq + d
                     if(abs(hadron%child1%typ).eq.1) then
                        remnant%child1%typ = sign(2212, hadron%typ)
                     else if(abs(hadron%child1%typ).eq.2) then
                        remnant%child1%typ = sign(2224, hadron%typ)
                     else if(abs(hadron%child1%typ).eq.3) then
                        remnant%child1%typ = sign(3222, hadron%typ)
                     else if(abs(hadron%child1%typ).eq.4) then
                        remnant%child1%typ = sign(4222, hadron%typ)
                     else if(abs(hadron%child1%typ).eq.5) then
                        remnant%child1%typ = sign(5222, hadron%typ)
                     end if
                     remnant%child2%typ = sign(1, hadron%typ)
                  end if
                  remnant%c1=hadron%child1%c2
                  remnant%c2=hadron%child1%c1
                  remnant%child1%c1=0
                  remnant%child1%c2=0
                  remnant%child2%c1=remnant%c1
                  remnant%child2%c2=remnant%c2
               else
                  ! sea quark
                  if(.not.associated(remnant%child1)) then
                     call shower_add_child(shower, remnant, 1)
                  end if
                  if(.not.associated(remnant%child2)) then
                     call shower_add_child(shower, remnant, 2)
                  end if
                  call tao_random_number(rand)
                  if(rand < 0.5_default) then
                     ! 1/2 into usbar + ud_0
                     if(abs(hadron%child1%typ).eq.3) then
                        remnant%child1%typ = sign(321, hadron%typ)
                     else if(abs(hadron%child1%typ).eq.4) then
                        remnant%child1%typ = sign(421, hadron%typ)
                     else if(abs(hadron%child1%typ).eq.5) then
                        remnant%child1%typ = sign(521, hadron%typ)
                     end if
                     remnant%child2%typ = sign(2101, hadron%typ)
                  else if(rand<0.6666_default) then
                     ! 1/6 into usbar + ud_1
                     if(abs(hadron%child1%typ).eq.3) then
                        remnant%child1%typ = sign(321, hadron%typ)
                     else if(abs(hadron%child1%typ).eq.4) then
                        remnant%child1%typ = sign(421, hadron%typ)
                     else if(abs(hadron%child1%typ).eq.5) then
                        remnant%child1%typ = sign(521, hadron%typ)
                     end if
                     remnant%child2%typ = sign(2103, hadron%typ)
                  else
                     ! 1/3 into dsbar + uu_1
                     if(abs(hadron%child1%typ).eq.3) then
                        remnant%child1%typ = sign(311, hadron%typ)
                     else if(abs(hadron%child1%typ).eq.4) then
                        remnant%child1%typ = sign(411, hadron%typ)
                     else if(abs(hadron%child1%typ).eq.5) then
                        remnant%child1%typ = sign(511, hadron%typ)
                     end if
                     remnant%child2%typ = sign(2203, hadron%typ)
                  end if
                  remnant%c1=hadron%child1%c2
                  remnant%c2=hadron%child1%c1
                  remnant%child2%c1=remnant%c1
                  remnant%child2%c2=remnant%c2
               end if
            else if(parton_is_gluon(hadron%child1)) then
               if(.not.associated(remnant%child1)) then
                  call shower_add_child(shower, remnant, 1)
               end if
               if(.not.associated(remnant%child2)) then
                  call shower_add_child(shower, remnant, 2)
               end if
               call tao_random_number(rand)
               if(rand < 0.5_default) then
                  ! 1/2 into u + ud_0
                  remnant%child1%typ = sign(2, hadron%typ)
                  remnant%child2%typ = sign(2101, hadron%typ)
               else if(rand < 0.6666_default) then
                  ! 1/6 into u + ud_1
                  remnant%child1%typ = sign(2, hadron%typ)
                  remnant%child2%typ = sign(2103, hadron%typ)
               else
                  ! 1/3 into d + uu_1
                  remnant%child1%typ = sign(1, hadron%typ)
                  remnant%child2%typ = sign(2203, hadron%typ)
               end if
               remnant%c1=hadron%child1%c2
               remnant%c2=hadron%child1%c1
               if(sign(1,hadron%typ)>0) then
                  remnant%child1%c1 = remnant%c1
                  remnant%child2%c2 = remnant%c2
               else
                  remnant%child1%c2 = remnant%c2
                  remnant%child2%c1 = remnant%c1
               end if
            end if
            if(associated(remnant%child1)) then
               ! don't care about on-shellness for now
               remnant%child1%momentum = 0.5_default*remnant%momentum
               remnant%child2%momentum = 0.5_default*remnant%momentum
               ! but care about on-shellness for baryons
               if(mod(remnant%child1%typ, 100) .ge. 10) then
                  ! check if the third quark is set -> meson or baryon
                  remnant%child1%t = parton_mass_squared(remnant%child1)
                  remnant%child1%momentum = vector4_moving( &
                       energy(remnant%child1%momentum), &
                       ( space_part(remnant%child1%momentum) / &
                       space_part_norm(remnant%child1%momentum) ) *sqrt( &
                       energy(remnant%child1%momentum)**2 - remnant%child1%t))
                  remnant%child2%momentum = remnant%momentum - remnant%child1%momentum
               end if
            end if
         end if
      end do
    end subroutine shower_update_beamremnants

    subroutine interaction_apply_lorentztrafo(interaction, L)
      type(my_interaction_t), intent(inout) :: interaction
      type(lorentz_transformation_t), intent(in) :: L

      type(parton_t), pointer :: prt
      integer :: i
      
      ! ISR part
      do i=1,2
         prt=>interaction%partons(i)%p
         ! loop over ancestors
         mothers: do
            ! boost parton
            call parton_apply_lorentztrafo(prt, L)
            if(associated(prt%child2)) then
               ! boost emitted timelike parton (and daughters)
               call parton_apply_lorentztrafo_recursiv(prt%child2, L)
            end if
            if(associated(prt%parent)) then
               if(parton_is_hadron(prt%parent).eqv..false.) then
                  prt=>prt%parent
               else
                  exit
               end if
            else
               exit
            end if
         enddo mothers
      end do

      ! FSR part
      if(associated(interaction%partons(3)%p%parent)) then
         ! pseudo Parton-Shower histora has been generated -> find mother and go on from there recursively
         prt => interaction%partons(3)%p
         do while(associated(prt%parent))
            prt=>prt%parent
         end do
         call parton_apply_lorentztrafo_recursiv(prt, L)
      else
         do i=3, size(interaction%partons)
            call parton_apply_lorentztrafo(interaction%partons(i)%p, L)
         end do
      end if
!      print *, " end interaction_apply_lorentztrafo"
    end subroutine interaction_apply_lorentztrafo

    subroutine shower_apply_lorentztrafo(shower, L)
      type(shower_t), intent(inout) :: shower
      type(lorentz_transformation_t), intent(in) :: L
      integer :: i

      do i=1, size(shower%interactions)
         call interaction_apply_lorentztrafo(shower%interactions(i)%i, L)
      end do
      
    end subroutine shower_apply_lorentztrafo

    subroutine interaction_boost_to_CMframe(interaction)
      ! boosts partons belonging to the interaction to the center-of-mass-frame of its partons nearest to the hadron
      type(my_interaction_t), intent(inout) :: interaction
      type(vector4_t) :: beta
      type(parton_t), pointer :: prt1, prt2

      call interaction_find_partons_nearest_to_hadron(interaction, prt1, prt2)

      beta=prt1%momentum+prt2%momentum
      beta=beta/vector4_get_component(beta,0)

      if(beta**2>1._default) then
         print *, " BUG: beta > 1"
         return
      end if
      if(space_part(beta)**2>1D-14) then
         call interaction_apply_lorentztrafo(interaction, boost(space_part(beta)**1 / &
              sqrt(1._default-space_part(beta)**2), -direction(beta)))
      end if
    end subroutine interaction_boost_to_CMframe
    
    subroutine shower_boost_to_CMframe(shower)
      ! boosts every interaction to the center-of-mass-frame of its partons nearest to the hadron
      type(shower_t), intent(inout) :: shower
      integer :: i

      do i=1, size(shower%interactions)
         call interaction_boost_to_CMframe(shower%interactions(i)%i)
      end do
      call shower_update_beamremnants(shower)
    end subroutine shower_boost_to_CMframe

    subroutine shower_boost_to_labframe(shower)
      ! boost all partons so that initial partons have their assigned x-value
      type(shower_t), intent(inout) :: shower
      integer :: i

      do i=1, size(shower%interactions)
         call interaction_boost_to_labframe(shower%interactions(i)%i)
      end do
    end subroutine shower_boost_to_labframe

    subroutine interaction_boost_to_labframe(interaction)
      ! boost all partons so that initial partons have their assigned x-value
      type(my_interaction_t), intent(inout) :: interaction
      type(parton_t), pointer :: prt1, prt2
      type(vector3_t) :: beta

      call interaction_find_partons_nearest_to_hadron(interaction, prt1, prt2)

      if( (.not.associated(prt1%initial)) .or. (.not.associated(prt2%initial)) ) then
         return
      end if

      ! if no branching at all occured, take original partons
!      if(prt1%child1%belongstointeraction .and. prt2%child1%belongstointeraction) then
!         prt1=>prt1%child1
!         prt2=>prt2%child1
!      end if
      
      ! transform partons to overall labframe.
      beta=vector3_canonical(3) * & 
           ( (prt1%x*vector4_get_component(prt2%momentum, 0)-prt2%x*vector4_get_component(prt1%momentum, 0))/ &
             (prt1%x*vector4_get_component(prt2%momentum, 3)-prt2%x*vector4_get_component(prt1%momentum, 3)) )
      if (beta**1 > 1D-10) &
           call interaction_apply_lorentztrafo(interaction, boost(beta**1/sqrt(1._default-beta**2), -direction(beta)))
    end subroutine interaction_boost_to_labframe

    subroutine interaction_rotate_to_z(interaction)
      type(my_interaction_t), intent(inout) :: interaction
      type(parton_t), pointer :: prt1, prt2
      
      call interaction_find_partons_nearest_to_hadron(interaction, prt1, prt2)

      ! only rotate to z if inital hadrons are given (and they are assumed to be aligned along the z-axis)
      if(associated(prt1%initial)) then
         call interaction_apply_lorentztrafo(interaction, rotation_to_2nd( space_part(prt1%momentum), &
              vector3_canonical(3) * sign(1._default, vector4_get_component(prt1%initial%momentum,3)) ) )
      end if
    end subroutine interaction_rotate_to_z

    subroutine shower_rotate_to_z(shower)
      !rotate initial partons to lie along +/- z axis
      type(shower_t), intent(inout) :: shower
      integer :: i

      do i=1, size(shower%interactions)
         call interaction_rotate_to_z(shower%interactions(i)%i)
      end do
      call shower_update_beamremnants(shower)
    end subroutine shower_rotate_to_z

    subroutine interaction_generate_primordial_kt(interaction)
      type(my_interaction_t), intent(inout) :: interaction
      type(parton_t), pointer :: had1, had2
      type(vector4_t) :: momenta(2)
      type(vector3_t) :: beta
      real(default) :: pt (2), phi(2)
      real(default) :: shat
      ! variables for boosting and rotating
      real(default) :: btheta, bphi
      integer :: i

      if(primordial_kt_width .eq. 0._default) then
         return
      end if

!      print *, "interaction_generate_primordial_kt"

      !return if there are no initials, electron-hadron collision not implemented
      if( (.not. associated(interaction%partons(1)%p%initial)) .or. (.not. associated(interaction%partons(2)%p%initial)) ) then
         return
      end if
      
      had1=>interaction%partons(1)%p%initial
      had2=>interaction%partons(2)%p%initial

      ! copy momenta and energy
      momenta(1)=had1%child1%momentum
      momenta(2)=had2%child1%momentum

      generate_pt_phi: do i=1,2
         ! generate transverse momentum and phi
         generate_pt: do
            call tao_random_number(pt(i))
            pt(i)=primordial_kt_width*sqrt(-log(pt(i)))
            if(pt(i)<primordial_kt_cutoff) exit
         end do generate_pt
         call tao_random_number(phi(i))
         phi(i)=twopi*phi(i)
      end do generate_pt_phi

      ! adjust momenta
      shat=(momenta(1) + momenta(2))**2

      momenta(1)=vector4_moving(vector4_get_component(momenta(1),0), &
           vector3_moving( (/pt(1)*cos(phi(1)),pt(1)*sin(phi(1)),vector4_get_component(momenta(1),3)/) ) )
      momenta(2)=vector4_moving(vector4_get_component(momenta(2),0), & 
           vector3_moving( (/pt(2)*cos(phi(2)),pt(2)*sin(phi(2)),vector4_get_component(momenta(2),3)/) ) )

      beta=vector3_moving( (/ vector4_get_component(momenta(1),1)+vector4_get_component(momenta(2), 1) , &
           vector4_get_component(momenta(1),2)+vector4_get_component(momenta(2), 2), 0._default /) )/sqrt(shat)

      momenta(1)=boost(beta**1/sqrt(1._default-beta**2), -direction(beta))*momenta(1)
      bphi=azimuthal_angle(momenta(1))
      btheta=polar_angle(momenta(1))

      call interaction_apply_lorentztrafo(interaction, rotation(cos(bphi), sin(bphi), 3)*rotation(cos(btheta), & 
           sin(btheta), 2)*rotation(cos(-bphi), sin(-bphi), 3))
      call interaction_apply_lorentztrafo(interaction, boost(beta**1/sqrt(1._default-beta**2), -direction(beta)))
    end subroutine interaction_generate_primordial_kt

    subroutine shower_generate_primordial_kt(shower)
      type(shower_t), intent(inout) :: shower
      integer :: i

      !      print *, "shower_generate_primordial_kt"
      !      call shower_print(shower)

      do i=1, size(shower%interactions)
         call interaction_generate_primordial_kt(shower%interactions(i)%i)
      end do
      call shower_update_beamremnants(shower)

      !      call shower_print(shower)
    end subroutine shower_generate_primordial_kt

!!!! for printing

    subroutine interaction_print(interaction)
      type(my_interaction_t), intent(in) :: interaction
      integer :: i

      if(associated(interaction%partons(1)%p)) then
         call parton_print(interaction%partons(1)%p)
         if(associated(interaction%partons(1)%p%initial)) call parton_print(interaction%partons(1)%p%initial)
      end if
      if(associated(interaction%partons(2)%p)) then
         call parton_print(interaction%partons(2)%p)
         if(associated(interaction%partons(2)%p%initial)) call parton_print(interaction%partons(2)%p%initial)
      end if
      if(allocated(interaction%partons)) then
         do i=1, size(interaction%partons)
            call parton_print(interaction%partons(i)%p)
         end do
      end if
      print *
    end subroutine interaction_print

    subroutine shower_print(shower)
      type(shower_t), intent(in) :: shower
      integer :: i

      if(size(shower%interactions) > 0) then
         print *, "    interactions: "
         do i=1, size(shower%interactions)
            print *, " interaction number ", i
            if(.not. associated(shower%interactions(i)%i)) then
               stop "Bug: missing interaction in shower"
            end if
            call interaction_print(shower%interactions(i)%i)
         end do
      else
         print *, " no interactions in shower"
      end if

      print *

      if(allocated(shower%partons)) then
         print *, "    partons:"
         do i=1, size(shower%partons)
            !            print *, " i=", i
            if(associated(shower%partons(i)%p)) then
               call parton_print(shower%partons(i)%p)
               if(i<size(shower%partons)) then
                  if(associated(shower%partons(i+1)%p)) then
                     if((shower%partons(i)%p%belongstointeraction.eqv..true.) .and. & 
                          (shower%partons(i+1)%p%belongstointeraction.eqv..false.)) then
                        print *, "-------------------------------------------------------"
                        !                     stop "END"
                     end if
                  end if
               end if
            end if
         end do
      else
         print *, " no partons in shower"
      end if

100   format(4x, A16, F9.3, F9.3, F9.3, F10.3)
      write(*,100) "Total Momentum: ", shower_get_total_momentum(shower, 1),  shower_get_total_momentum(shower,2), &
           shower_get_total_momentum(shower, 3), shower_get_total_momentum(shower, 0)

      print *, " ISR finished: ", shower_isr_is_finished(shower)
      print *, " FSR finished: ", shower_fsr_is_finished(shower)
    end subroutine shower_print

!!! physics

    subroutine shower_replace_parent_by_hadron(shower, prt)
      type(shower_t), intent(inout) :: shower
      type(parton_t), intent(inout), target :: prt
      type(parton_t), pointer :: remnant => null()

      !      print *, " shower_replace_parent_by_hadron for parton ", prt%nr

      if(associated(prt%parent)) then
         call shower_remove_parton_from_partons(shower, prt%parent)
         deallocate(prt%parent)
      end if

      if(.not.associated(prt%initial%child2)) then
         call shower_add_child(shower, prt%initial, 2)
      end if
      prt%parent=>prt%initial
      prt%parent%child1=>prt

      ! make other child to be a beam-remnant
      remnant=> prt%initial%child2
      remnant%typ= 9999
      remnant%momentum = prt%parent%momentum - prt%momentum
      remnant%x = 1._default - prt%x
      remnant%parent=>prt%initial
      remnant%t = 0._default
    end subroutine shower_replace_parent_by_hadron

    subroutine shower_get_first_ISR_scale_for_parton(shower, prt, tmax)
      type(shower_t), intent(inout) :: shower
      type(parton_t), intent(inout), target :: prt
      real(default), intent(in), optional :: tmax
      real(default) :: t,tstep, zufall, integral, temp1
      integer :: i
      logical :: goon
      real(default) :: temprand

      print *, "get first"

      if(present(tmax)) then
         t=max(max(-tscalefactor_isr*parton_get_energy(prt)**2, -abs(tmax)), prt%t)
      else
         t=max(-tscalefactor_isr*parton_get_energy(prt)**2, prt%t)
      end if
      call tao_random_number(zufall)
      zufall=-twopi*log(zufall)  ! compare Integral and log(zufall) instead of zufall and exp(-Integral)
      zufall=zufall/first_integral_suppression_factor 
      integral=0._default
      call parton_set_simulated(prt, .false.)

      do
         call tao_random_number(temprand)
         tstep=max(abs(0.01_default*t)*temprand, 0.1_default*D_Min_t)
         if(t+0.5_default*tstep>-D_Min_t) then
            prt%t=parton_mass_squared(prt)
            call parton_set_simulated(prt)
            exit
         end if
         prt%t=t+0.5_default*tstep
         temp1=integral_over_z_simple(prt, (zufall-integral)/tstep)
         integral=integral+tstep*temp1
         if(integral>zufall) then
            prt%t=t+0.5_default*tstep
            exit
         end if
         t=t+tstep
      end do

      if (prt%t>-D_Min_t) then
         call shower_replace_parent_by_hadron(shower, prt)
      end if

!      call parton_set_simulated(prt)

      print *, "  shower_get_first_ISR_scale_for_parton finished"

      contains

        function integral_over_z_simple(prt, ende) result(integral)
          type(parton_t), intent(inout) :: prt
!          real(default), intent(in) :: shat,s,
          real(default), intent(in) :: ende
          real(default) :: integral
          
          real(default), parameter :: zstepfactor = 1._default
          real(default), parameter :: zstepmin = 0.0001_default
          real(default) :: z, zstep, minz, maxz
          real(default) :: pdfsum
          integer :: quark
          
          integral=0._default
          if(D_print) then
             print *, "integral_over_z_simple for t=", prt%t
          end if
          
          minz=prt%x
!          maxz=maxzz(shat, s)
          maxz=maxz_isr
          z=minz

          ! TODO -> Adapt zstep to structure of divergencies
          if(parton_is_gluon(prt%child1)) then
             ! gluon coming from g->gg
             do
                call tao_random_number(temprand)
                zstep=max(zstepmin, temprand*zstepfactor*z*(1._default-z))
                zstep=min(zstep, maxz-z)
                integral=integral+zstep*(D_alpha_s_isr((1._default-(z+0.5_default*zstep))*abs(prt%t))/(abs(prt%t)))* & 
                     P_ggg(z+0.5_default*zstep)*get_pdf(prt%initial%typ, prt%x/(z+0.5_default*zstep), abs(prt%t), 21)
                if(integral>ende) then
                   exit
                end if
                z=z+zstep
                if(z>=maxz) then
                   exit
                end if
             enddo
             
             ! gluon coming from q->qg  ! correctly implemented yet?
             if(integral<ende) then
                z=minz
                do
                   call tao_random_number(temprand)
                   zstep=max(zstepmin, temprand*zstepfactor*z*(1._default-z))
                   zstep=min(zstep, maxz-z)
                   pdfsum=0._default
                   do quark=-D_Nf, D_Nf
                      pdfsum=pdfsum + get_pdf(prt%initial%typ, prt%x/(z+0.5_default*zstep), abs(prt%t), quark)
                   end do
                   integral=integral+zstep*(D_alpha_s_isr((z+0.5_default*zstep)*abs(prt%t))/(abs(prt%t)))* & 
                        P_qqg(1._default-(z+0.5_default*zstep))*pdfsum
                   if(integral>ende) then
                      exit
                   end if
                   z=z+zstep
                   if(z>=maxz) then
                      exit
                   end if
                enddo
             end if
          else if(parton_is_quark(prt%child1)) then
             ! quark coming from q->qg
             do
                call tao_random_number(temprand)
                zstep=max(zstepmin, temprand*zstepfactor*z*(1._default-z))
                zstep=min(zstep, maxz-z)
                integral=integral+zstep*(D_alpha_s_isr((1._default-(z+0.5_default*zstep))*abs(prt%t))/(abs(prt%t)))* &
                     P_qqg(z+0.5_default*zstep)*get_pdf(prt%initial%typ, prt%x/(z+0.5_default*zstep), abs(prt%t), prt%typ)
                if(integral>ende) then
                   exit
                end if
                z=z+zstep
                if(z>=maxz) then
                   exit
                end if
             enddo
             
             ! quark coming from g->qqbar
             if(integral<ende) then
                z=minz
                do
                   call tao_random_number(temprand)
                   zstep=max(zstepmin, temprand*zstepfactor*z*(1._default-z))
                   zstep=min(zstep, maxz-z)
                   integral=integral+zstep*(D_alpha_s_isr((1._default-(z+0.5_default*zstep))*abs(prt%t))/(abs(prt%t)))* &
                        P_gqq(z+0.5_default*zstep)*get_pdf(prt%initial%typ, prt%x/(z+0.5_default*zstep), abs(prt%t), 21)
                   if(integral>ende) then
                      exit
                   end if
                   z=z+zstep
                   if(z>=maxz) then
                      exit
                   end if
                enddo
             end if
             
          end if
          integral=integral/get_pdf(prt%initial%typ, prt%x, abs(prt%t),prt%typ)
        end function integral_over_z_simple
    end subroutine shower_get_first_ISR_scale_for_parton

    subroutine shower_prepare_for_simulate_isr_pt(shower, interaction)
      type(shower_t), intent(inout) :: shower
      type(my_interaction_t), intent(inout) :: interaction
      real(default) :: s

!      print *, " shower_prepare_for_simulate_isr_pt"

      ! get sqrts of interaction

      s = (interaction%partons(1)%p%momentum + interaction%partons(2)%p%momentum)**2

      interaction%partons(1)%p%scale = tscalefactor_isr * 0.25_default * s
      interaction%partons(2)%p%scale = tscalefactor_isr * 0.25_default * s

!!$      call shower_add_parent(shower, interaction%partons(1)%p)
!!$      call shower_add_parent(shower, interaction%partons(2)%p)
!!$      
!!$      interaction%partons(1)%p%parent%scale = 0.5_default * sqrts
!!$      interaction%partons(1)%p%parent%momentum = interaction%partons(1)%p%momentum
!!$      interaction%partons(1)%p%parent%belongstoFSR = .false.
!!$      interaction%partons(1)%p%parent%initial => interaction%partons(1)%p%initial
!!$      interaction%partons(2)%p%parent%scale = 0.5_default * sqrts
!!$      interaction%partons(2)%p%parent%momentum = interaction%partons(2)%p%momentum
!!$      interaction%partons(2)%p%parent%belongstoFSR = .false.
!!$      interaction%partons(2)%p%parent%initial => interaction%partons(2)%p%initial
!!$
!!$      call shower_add_child(shower, interaction%partons(1)%p%parent, 2)
!!$      call shower_add_child(shower, interaction%partons(2)%p%parent, 2)
!      print *, " shower_prepare_for_simulate_isr_pt finished"
    end subroutine shower_prepare_for_simulate_isr_pt

    subroutine shower_prepare_for_simulate_isr_ana_test(shower, prt1, prt2)
      type(shower_t), intent(inout) :: shower
      type(parton_t), intent(inout), target :: prt1, prt2
      type(parton_t), pointer :: prt, prta, prtb
      real(kind=double) ::  pini(0:3), scale, factor
      integer :: i

      !      print *, " shower_prepare_for_simulate_isr_ana"

      if( (.not. associated(prt1%initial)) .or. (.not. associated(prt2%initial)) ) then
         return
      end if

      do i=0,3
         pini(i)=parton_get_momentum(prt1, i)+parton_get_momentum(prt2, i)
      end do
      scale = -( pini(0)**2 - pini(1)**2 - pini(2)**2 - pini(3)**2)
      call parton_set_simulated(prt1)
      call parton_set_simulated(prt2)
      call shower_add_parent(shower, prt1)
      call shower_add_parent(shower, prt2)

      factor=sqrt(vector4_get_component(prt1%momentum, 0)**2-scale)/space_part_norm(prt1%momentum)

      prt1%parent%typ=prt1%typ
      prt1%parent%z=1._double
      prt1%parent%momentum = prt1%momentum
      prt1%parent%t=scale
      prt1%parent%x=prt1%x
      prt1%parent%initial=>prt1%initial
      prt1%parent%belongstoFSR=.false.
      prt1%parent%c1=prt1%c1
      prt1%parent%c2=prt1%c2

      prt2%parent%typ=prt2%typ
      prt2%parent%z=1._double
      prt2%parent%momentum = prt2%momentum
      prt2%parent%t=scale
      prt2%parent%x=prt2%x
      prt2%parent%initial=>prt2%initial
      prt2%parent%belongstoFSR=.false.
      prt2%parent%c1=prt2%c1
      prt2%parent%c2=prt2%c2

      do
         call shower_get_first_ISR_scale_for_parton(shower, prt1%parent)
         call shower_get_first_ISR_scale_for_parton(shower, prt2%parent)

         ! redistribute energy among first partons
         prta=>prt1%parent
         prtb=>prt2%parent

         do i=0,3
            pini(i)=parton_get_momentum(prt1, i)+parton_get_momentum(prt2, i)
         end do
         call parton_set_energy(prta, (pini(0)**2-prtb%t+prta%t)/(2._double*pini(0)))
         call parton_set_energy(prtb, pini(0)-vector4_get_component(prta%momentum, 0))

 !        if(abs( (prt1%parent%x*vector4_get_component(prt2%parent%momentum, 0)-&
 !          prt2%parent%x*vector4_get_component(prt1%parent%momentum, 0))/ &
 !            (prt1%parent%x*vector4_get_component(prt2%parent%momentum, 3)-&
 !            prt2%parent%x*vector4_get_component(prt1%parent%momentum, 3)) ).gt. 1._double) then
         print *, "cycle", abs( (prt1%parent%x*vector4_get_component(prt2%parent%momentum, 0)-&
              prt2%parent%x*vector4_get_component(prt1%parent%momentum, 0))/ &
              (prt1%parent%x*vector4_get_component(prt2%parent%momentum, 3)-&
              prt2%parent%x*vector4_get_component(prt1%parent%momentum, 3)) )
!            cycle
!         else
            exit
!         end if
      end do
      print *, "BETA+", (prt1%parent%x*vector4_get_component(prt2%parent%momentum, 0)-&
           prt2%parent%x*vector4_get_component(prt1%parent%momentum, 0))/ &
           (prt1%parent%x*vector4_get_component(prt2%parent%momentum, 3)-&
           prt2%parent%x*vector4_get_component(prt1%parent%momentum, 3))
      call parton_set_simulated(prt1%parent)
      call parton_set_simulated(prt2%parent)
      ! rescale momenta
      do i=1,2
         if(i.eq.1) then
            prt=>prt1%parent
         else
            prt=>prt2%parent
         end if
         factor= sqrt(vector4_get_component(prt%momentum,0)**2-prt%t)/space_part_norm(prt%momentum)
         prt%momentum = vector4_moving( vector4_get_component(prt%momentum, 0), factor*space_part(prt%momentum))
      end do

      if(prt1%parent%t<0._double) then
         call shower_add_parent(shower, prt1%parent)
         prt1%parent%parent%momentum = prt1%parent%momentum
         prt1%parent%parent%t=prt1%parent%t
         prt1%parent%parent%x=prt1%parent%x
         prt1%parent%parent%initial => prt1%parent%initial
         prt1%parent%parent%belongstoFSR=.false.
         call shower_add_child(shower, prt1%parent%parent, 2)
      end if

      if(prt2%parent%t<0._double) then
         call shower_add_parent(shower, prt2%parent)
         prt2%parent%parent%momentum=prt2%parent%momentum
         prt2%parent%parent%t=prt2%parent%t
         prt2%parent%parent%x=prt2%parent%x
         prt2%parent%parent%initial => prt2%parent%initial
         prt2%parent%parent%belongstoFSR=.false.
         call shower_add_child(shower, prt2%parent%parent, 2)
      end if
      print *, "after isr_ana_test"
      call shower_print(shower)
      print *, "BETA:"
      call vector3_write(vector3_canonical(3) * & 
           ( (prt1%parent%x*vector4_get_component(prt2%parent%momentum, 0)-&
           prt2%parent%x*vector4_get_component(prt1%parent%momentum, 0))/ &
             (prt1%parent%x*vector4_get_component(prt2%parent%momentum, 3)-&
             prt2%parent%x*vector4_get_component(prt1%parent%momentum, 3)) ) )

      print *, "after isr_ana_test finished"
      
  end subroutine shower_prepare_for_simulate_isr_ana_test

    subroutine shower_prepare_for_simulate_isr_ana(shower, prt1, prt2)
      type(shower_t), intent(inout) :: shower
      type(parton_t), intent(inout), target :: prt1, prt2
      type(parton_t), pointer :: prt
      real(default) ::  pini(0:3), scale, factor
      real(default) :: oldscales(1:2)
      type(parton_pointer_t) :: temppp
      integer :: i

      !      print *, " shower_prepare_for_simulate_isr_ana"

      if( (.not. associated(prt1%initial)) .or. (.not. associated(prt2%initial)) ) then
         return
      end if

      do i=0,3
         pini(i)=parton_get_momentum(prt1, i)+parton_get_momentum(prt2, i)
      end do
      scale = -( pini(0)**2 - pini(1)**2 - pini(2)**2 - pini(3)**2)

      prt1%t = -tscalefactor_isr*abs(scale)
      prt2%t = -tscalefactor_isr*abs(scale)
      ! rescale momenta
      do i=1,2
         if(i.eq.1) then
            prt=>prt1
         else
            prt=>prt2
         end if

         factor= sqrt(parton_get_energy(prt)**2-prt%t)/space_part_norm(prt%momentum)

         prt%momentum = vector4_moving( parton_get_energy(prt), factor*space_part(prt%momentum))
      end do

      ! ensure that belongstointeraction bits are set correctly
      prt1%belongstointeraction = .true.
      prt2%belongstointeraction = .true.

      call shower_add_parent(shower, prt1)
      call shower_add_parent(shower, prt2)

      call parton_set_simulated(prt1)
      prt1%parent%typ=prt1%typ
      prt1%parent%z=1._default
      prt1%parent%momentum = prt1%momentum
      prt1%parent%t=scale
      prt1%parent%x=prt1%x
      prt1%parent%initial=>prt1%initial
      prt1%parent%belongstoFSR=.false.
      prt1%parent%c1 = prt1%c1
      prt1%parent%c2 = prt1%c2
      call shower_add_child(shower, prt1%parent, 2)

      call parton_set_simulated(prt2)
      prt2%parent%typ=prt2%typ
      prt2%parent%z=1._default
      prt2%parent%momentum = prt2%momentum
      prt2%parent%t=scale
      prt2%parent%x=prt2%x
      prt2%parent%initial=>prt2%initial
      prt2%parent%belongstoFSR=.false.
      prt2%parent%c1 = prt2%c1
      prt2%parent%c2 = prt2%c2
      call shower_add_child(shower, prt2%parent, 2)

      first_branchings: do
         oldscales(1)=prt1%parent%t
         oldscales(2)=prt2%parent%t
         if (abs(prt1%parent%t) > abs(prt2%parent%t)) then
            temppp%p => prt1%parent
         else
            temppp%p => prt2%parent
         end if
         if( (parton_is_simulated(temppp%p).eqv..false.).and.(parton_is_hadron(temppp%p).eqv..false.) ) then
            call shower_isr_step(shower, temppp%p)
            if(parton_is_simulated(temppp%p)) then
!               call parton_generate_ps_ini(prt2%parent)
               if(temppp%p%t<0._default) then
                  call shower_execute_next_isr_branching(shower, temppp)
!                  call shower_print(shower)
               else
                  call shower_replace_parent_by_hadron(shower, temppp%p%child1)
               end if
            end if
         end if

         if(oldscales(1)==prt1%parent%t .and. oldscales(2)==prt2%parent%t) then
            exit first_branchings
         end if
      end do first_branchings

      !      print *, "  shower_prepare_for_simulate_isr_ana finished"
    end subroutine shower_prepare_for_simulate_isr_ana

!!$    subroutine shower_prepare_for_simulate_fsr_ana(shower, prt1, prt2)
!!$      type(shower_t), intent(inout) :: shower
!!$      type(parton_t), pointer :: prt1, prt2
!!$      real(default) ::  pini(4)
!!$      integer i
!!$
!!$      !      print *, "shower_prepare_for_simulate_fsr_ana" 
!!$
!!$      ! Define imagined single initiator of shower 
!!$      call shower_add_child(shower, prt1, 1)
!!$      do i=1,4
!!$         pini(i)=parton_get_momentum(prt1, i-1)+parton_get_momentum(prt2, i-1)
!!$      end do
!!$      call parton_set_simulated(prt1)
!!$      call parton_set_child(prt1, prt1%child1, 1)
!!$      call parton_set_child(prt1, prt1%child1, 2)
!!$      call parton_set_simulated(prt2)
!!$      call parton_set_child(prt2, prt1%child1, 1)
!!$      call parton_set_child(prt2, prt1%child1, 2)
!!$
!!$      prt1%child1%typ=94
!!$      prt1%child1%z=parton_get_energy(prt1) / (parton_get_energy(prt1)+parton_get_energy(prt2))
!!$      call parton_set_simulated(prt1%child1)
!!$      call parton_set_parent(prt1%child1, prt1)
!!$      call parton_set_momentum(prt1%child1, pini(1), pini(2), pini(3), pini(4))
!!$      prt1%child1%t=parton_p4square(prt1%child1)
!!$      prt1%child1%costheta=-1._default
!!$
!!$      call shower_add_child(shower, prt1%child1, 1)
!!$      call shower_add_child(shower, prt1%child1, 2)
!!$
!!$      prt1%child1%child1%typ=prt1%typ
!!$      prt1%child1%child1%momentum=prt1%momentum
!!$      prt1%child1%child1%t=prt1%child1%t
!!$      call parton_set_parent(prt1%child1%child1, prt1%child1)
!!$      prt1%child1%child1%c1=prt1%c1
!!$      prt1%child1%child1%c2=prt1%c2
!!$
!!$      prt1%child1%child2%typ=prt2%typ
!!$      prt1%child1%child2%momentum=prt2%momentum
!!$      prt1%child1%child2%t=prt2%child1%t
!!$      call parton_set_parent(prt1%child1%child2, prt1%child1)
!!$      prt1%child1%child2%c1=prt2%c1
!!$      prt1%child1%child2%c2=prt2%c2
!!$
!!$      !      print *, "  shower_prepare_for_simulate_fsr_ana finished"
!!$    end subroutine shower_prepare_for_simulate_fsr_ana

    subroutine shower_add_children_of_emitted_timelike_parton(shower, prt)
      type(shower_t), intent(inout) :: shower
      type(parton_t), pointer :: prt

      if(prt%t > parton_mass_squared(prt)+D_Min_t) then
         if(parton_is_quark(prt)) then
            ! q -> qg
            call shower_add_child(shower, prt,1)
            prt%child1%typ=prt%typ
            call parton_set_energy(prt%child1, prt%z*parton_get_energy(prt))
            prt%child1%t= prt%t
            call shower_add_child(shower, prt,2)
            prt%child2%typ=21
            call parton_set_energy(prt%child2, (1._default-prt%z)*parton_get_energy(prt))
            prt%child2%t= prt%t
         else
            if(int(prt%x)>0) then
               call shower_add_child(shower, prt,1)
               prt%child1%typ=int(prt%x)
               call parton_set_energy(prt%child1, prt%z*parton_get_energy(prt))
               prt%child1%t= prt%t
               call shower_add_child(shower, prt,2)
               prt%child2%typ=-int(prt%x)
               call parton_set_energy(prt%child2, (1._default-prt%z)*parton_get_energy(prt))
               prt%child2%t= prt%t
            else
               call shower_add_child(shower, prt, 1)
               prt%child1%typ=21
               call parton_set_energy(prt%child1, prt%z*parton_get_energy(prt))
               prt%child1%t=prt%t
               call shower_add_child(shower, prt, 2)
               prt%child2%typ=21
               call parton_set_energy(prt%child2, (1._default-prt%z)*parton_get_energy(prt))
               prt%child2%t= prt%t
            end if
         end if
      end if
    end subroutine shower_add_children_of_emitted_timelike_parton

    subroutine shower_simulate_children_ana(shower,prt)
      type(shower_t), intent(inout) :: shower
      type(parton_t), intent(inout) :: prt
      real(default), dimension(1:2) :: t, zufall, integral
      integer, dimension(1:2) :: gtoqq
      integer :: daughter
      type(parton_t), pointer :: daughterprt
      integer :: n_loop

      gtoqq(1)=0
      gtoqq(2)=0

      if(D_print) print *, " simulate_children_ana for parton " , prt%nr

      if(.not. associated(prt%child1) .or. .not. associated(prt%child2)) then
         print *, " error in simulate_children_ana: no children "
         return
      end if

      if((abs(prt%typ)>=90) .and. (abs(prt%typ)<=93) ) then
         ! prt is beam-remnant
         call parton_set_simulated(prt)
         return
      end if

      ! check if partons are "internal" -> fixed scale
      if(prt%child1%typ .eq. 94) then
         call parton_set_simulated(prt%child1)
      end if
      if(prt%child2%typ .eq. 94) then
         call parton_set_simulated(prt%child2)
      end if

      integral(1)=0._default
      integral(2)=0._default

      ! impose constraints by angular ordering -> cf. (26) of Gaining analytic control
      ! check if no branchings are possible
      if(prt%child1%simulated .eqv. .false.) then
         prt%child1%t = min(prt%child1%t, & 
              0.5_default*parton_get_energy(prt%child1)**2*(1._default-parton_get_costheta(prt)) )
         if(min(prt%child1%t, parton_get_energy(prt%child1)**2)<parton_mass_squared(prt%child1)+D_Min_t) then
            prt%child1%t=parton_mass_squared(prt%child1)
            call parton_set_simulated(prt%child1)
         end if
      end if
      if(prt%child2%simulated .eqv. .false.) then
         prt%child2%t = min(prt%child2%t, & 
              0.5_default*parton_get_energy(prt%child2)**2*(1._default-parton_get_costheta(prt)) )
         if(min(prt%child2%t, parton_get_energy(prt%child2)**2)<parton_mass_squared(prt%child2)+D_Min_t) then
            prt%child2%t=parton_mass_squared(prt%child2)
            call parton_set_simulated(prt%child2)
         end if
      end if

      call tao_random_number(zufall(1))
      call tao_random_number(zufall(2))

      n_loop=0
      do
         n_loop=n_loop+1
         if(n_loop>900) then
            ! try with massless quarks
            treat_duscb_quarks_massless = .true.
         end if
         if(n_loop>1000) then
            call shower_print(shower)
            print *, " simulate_children_ana failed for parton ", prt%nr
            STOP "BUG: too many loops in simulate_children_ana (?)"
         end if

         t(1)=prt%child1%t
         t(2)=prt%child2%t

         ! check if a branching in the range t(i) to t(i)-tstep(i) occurs

         ! check for child1
         if(parton_is_simulated(prt%child1).eqv..false.) then
            call parton_simulate_stept(prt%child1, integral(1), zufall(1), gtoqq(1))
         end if
         ! check for child2
         if(parton_is_simulated(prt%child2).eqv..false.) then
            call parton_simulate_stept(prt%child2, integral(2), zufall(2), gtoqq(2))
         end if

         if( (parton_is_simulated(prt%child1).and.parton_is_simulated(prt%child2)) ) then
            if(sqrt(prt%t) .le. sqrt(prt%child1%t) + sqrt(prt%child2%t)) then
               ! repeat the simulation for the parton with the lower virtuality t-m**2 (assuming it's not fixed)
               if((prt%child1%typ.eq.94).and.(prt%child2%typ.eq.94)) then
                  STOP "Bug: both partons fixed, but momentum not conserved"
               else if(prt%child1%typ.eq.94) then
                  ! reset child2
                  call parton_set_simulated(prt%child2, .false.)
                  prt%child2%t=min(prt%child1%t,(sqrt(prt%t) - sqrt(prt%child1%t))**2)
                  integral(2)=0._default
                  call tao_random_number(zufall(2))
               else if(prt%child2%typ.eq.94) then
                  ! reset child1
                  call parton_set_simulated(prt%child1, .false.)
                  prt%child1%t=min(prt%child2%t,(sqrt(prt%t) - sqrt(prt%child2%t))**2)
                  integral(1)=0._default
                  call tao_random_number(zufall(1))
               elseif(prt%child1%t-parton_mass_squared(prt%child1)>prt%child2%t-parton_mass_squared(prt%child2)) then
                  ! reset child2
                  call parton_set_simulated(prt%child2, .false.)
                  prt%child2%t=min(prt%child1%t,(sqrt(prt%t) - sqrt(prt%child1%t))**2)
                  integral(2)=0._default
                  call tao_random_number(zufall(2))
               else
                  ! reset child1 ! TODO choose child according to their t
                  call parton_set_simulated(prt%child1, .false.)
                  prt%child1%t=min(prt%child2%t,(sqrt(prt%t) - sqrt(prt%child2%t))**2)
                  integral(1)=0._default
                  call tao_random_number(zufall(1))
               end if
            else
               exit
            end if
         end if
      enddo

      call parton_apply_costheta(prt)

      ! add children
      do daughter=1,2
         if (daughter.eq.1) then
            daughterprt=>prt%child1
         else
            daughterprt=>prt%child2
         end if
         if(daughterprt%t < parton_mass_squared(daughterprt)+D_Min_t) then
            cycle
         end if
         if(.not. (parton_is_quark(daughterprt).or.parton_is_gluon(daughterprt))) then
            cycle
         end if
         if(parton_is_quark(daughterprt)) then
            ! q -> qg
            call shower_add_child(shower, daughterprt,1)
            daughterprt%child1%typ=daughterprt%typ
            call parton_set_energy(daughterprt%child1, daughterprt%z*parton_get_energy(daughterprt))
            daughterprt%child1%t= daughterprt%t
            call shower_add_child(shower, daughterprt,2)
            daughterprt%child2%typ=21
            call parton_set_energy(daughterprt%child2, (1._default-daughterprt%z)*parton_get_energy(daughterprt))
            daughterprt%child2%t= daughterprt%t
         else if(parton_is_gluon(daughterprt)) then
            if(gtoqq(daughter)>0) then
               call shower_add_child(shower, daughterprt,1)
               daughterprt%child1%typ=gtoqq(daughter)
               call parton_set_energy(daughterprt%child1, daughterprt%z*parton_get_energy(daughterprt))
               daughterprt%child1%t= daughterprt%t
               call shower_add_child(shower, daughterprt,2)
               daughterprt%child2%typ=-gtoqq(daughter)
               call parton_set_energy(daughterprt%child2, (1._default-daughterprt%z)*parton_get_energy(daughterprt))
               daughterprt%child2%t= daughterprt%t
            else
               call shower_add_child(shower, daughterprt, 1)
               daughterprt%child1%typ=21
               call parton_set_energy(daughterprt%child1, daughterprt%z*parton_get_energy(daughterprt))
               daughterprt%child1%t=daughterprt%t
               call shower_add_child(shower, daughterprt, 2)
               daughterprt%child2%typ=21
               call parton_set_energy(daughterprt%child2, (1._default-daughterprt%z)*parton_get_energy(daughterprt))
               daughterprt%child2%t= daughterprt%t
            end if
         end if
      end do
    end subroutine shower_simulate_children_ana

    subroutine shower_generate_next_fsr_branchings(shower)
      type(shower_t), intent(inout) :: shower
      integer i, index
      type(parton_t),  pointer :: prt

      ! find mother with highest t to be simulated
      index=0
      do i=1,size(shower%partons)
         prt=> shower%partons(i)%p
         if(prt%belongstoFSR.eqv..false.) cycle
         if(prt%belongstointeraction.eqv..true.) cycle
         if(associated(prt%child1) .and. associated(prt%child2)) then
            if(parton_is_simulated(prt%child1) .and. parton_is_simulated(prt%child2)) cycle
         end if
         if(parton_is_final(prt)) cycle

         index=i
         exit
      end do

      if(index.eq.0) then
         print *, " no branchable partons found"
         return
      end if

      prt=> shower%partons(index)%p
      call shower_simulate_children_ana(shower, prt)
      
    end subroutine shower_generate_next_fsr_branchings

    subroutine shower_isr_step_pt(shower, prt)
      type(shower_t), intent(inout) :: shower
      type(parton_t), target, intent(inout) :: prt
      type(parton_t), pointer :: otherprt   ! recoiler

      real(default) :: scale, scalestep
      real(default) :: integral, zufall, factor
      real(default) :: temprand1, temprand2

      otherprt => shower_find_recoiler(shower, prt)

      scale = prt%scale
      call tao_random_number(temprand1)
      call tao_random_number(temprand2)
      scalestep=max(abs(scalefactor1*scale)*temprand1, scalefactor2*temprand2*D_Min_scale)
      call tao_random_number(zufall)
      zufall=-twopi*log(zufall)  ! compare Integral and log(zufall) instead of zufall and exp(-Integral)
      integral=0._default

      if(scale - 0.5_default*scalestep < D_Min_scale) then   ! close enough to cut-off scale -> ignore
         prt%scale = 0._default
         prt%t=parton_mass_squared(prt)
         call parton_set_simulated(prt)
      else
         prt%scale=scale-0.5_default*scalestep
         factor = scalestep * (D_alpha_s_isr(prt%scale)/(prt%scale*get_pdf(prt%initial%typ, prt%x, prt%scale, prt%typ)))
         integral=integral+ factor * integral_over_z_isr_pt(prt, otherprt,(zufall-integral)/factor)
         if(integral>zufall) then
            ! prt%scale set above and prt%z set in integral_over_z_isr_pt
            call parton_set_simulated(prt)
            prt%t = - prt%scale / (1._default - prt%z)
         else
            prt%scale=scale-scalestep
         end if
      end if

    contains

      function integral_over_z_isr_pt(prt, otherprt, ende) result(integral)
        type(parton_t), intent(inout) :: prt, otherprt
        real(default), intent(in) :: ende
        real(default) :: integral
        real(default) :: mbr, r
        real(default) :: zmin, zmax, z, zstep
        integer :: n_bin
        integer, parameter :: n_total_bins = 100 
        real(default) :: quarkpdfsum
        real(default) :: temprand
        integer :: quark
        
        if(D_print) then
           print *, "integral_over_z_isr_pt for scale=", prt%scale
        end if
        
        integral = 0._default
        mbr = (prt%momentum + otherprt%momentum)**1
        zmin = prt%x
        zmax = min(1._default - (sqrt(prt%scale)/mbr) * & 
             ( sqrt(1._default + 0.25_default*prt%scale/mbr**2) - 0.25_default*sqrt(prt%scale)/mbr) , maxz_isr)
        zstep = (zmax - zmin)/n_total_bins
        
        if(zmin>zmax) then
!           print *, " error in integral_over_z_isr_pt: zmin > zmax ", zmin, zmax, prt%scale, mbr
           integral = 0._default
           return
        end if
        
        ! divide the range [zmin:zmax] in n_total_bins -> 
        bins: do n_bin = 1, n_total_bins
           z = zmin + zstep * (n_bin -0.5_default)   ! z-value in the middle of the bin
           
           if(parton_is_gluon(prt)) then
              quarkpdfsum = 0._default
              quarks: do quark = -D_Nf, D_Nf
                 if(quark .eq. 0) cycle quarks
                 quarkpdfsum = quarkpdfsum + get_pdf(prt%initial%typ, prt%x/z, prt%scale, quark)
              end do quarks
              ! g -> gg or q -> gq
              integral = integral + (zstep/z) * ( (P_ggg(z)+P_ggg(1._default-z))* & 
                   get_pdf(prt%initial%typ, prt%x/z, prt%scale, 21) + P_qqg(1._default-z)*quarkpdfsum )
           else if(parton_is_quark(prt)) then
              ! q -> qg or g -> qq
              integral = integral + (zstep/z) * ( & 
                   p_qqg(z)* get_pdf(prt%initial%typ, prt%x/z, prt%scale, prt%typ) + &
                   P_gqq(z)*get_pdf(prt%initial%typ, prt%x/z, prt%scale, 21) )
           else
!              STOP "Bug neither quark nor gluon in integral_over_z_isr_pt"
           end if
           if(integral > ende) then
              prt%z = z
              call tao_random_number(temprand)
              ! decide typ of father partons
              if(parton_is_gluon(prt)) then
                 if(temprand > ( P_qqg(1._default-z)*quarkpdfsum ) / &
                      ( (P_ggg(z)+P_ggg(1._default-z))*get_pdf(prt%initial%typ, prt%x/z, prt%scale, 21) &
                      + P_qqg(1._default-z)*quarkpdfsum )) then
                    ! gluon => gluon + gluon
                    prt%aux_pt = 21
                 else
                    ! quark => quark + gluon
                    ! decide which quark flavor the parent is
                    r = temprand * quarkpdfsum
                    which_quark: do quark = -D_Nf, D_Nf
                       if(quark.eq.0) cycle which_quark
                       if(r > quarkpdfsum - get_pdf(prt%initial%typ, prt%x/z, prt%scale, quark)) then
                          prt%aux_pt = quark
                          exit which_quark
                       else
                          quarkpdfsum = quarkpdfsum - get_pdf(prt%initial%typ, prt%x/z, prt%scale, quark)
                       end if
                    end do which_quark
                 end if
                    
              else if(parton_is_quark(prt)) then
                 if(temprand > ( P_qqg(z)*get_pdf(prt%initial%typ, prt%x/z, prt%scale, prt%typ) ) / &
                      ( P_qqg(z)*get_pdf(prt%initial%typ, prt%x/z, prt%scale, prt%typ) + &
                      P_gqq(z)*get_pdf(prt%initial%typ, prt%x/z, prt%scale, 21) )) then
                    ! gluon => quark + antiquark
                    prt%aux_pt = 21
                 else
                    ! quark => quark + gluon
                    prt%aux_pt = prt%typ
                 end if
              end if
              exit bins
           end if
        end do bins
      end function integral_over_z_isr_pt
    end subroutine shower_isr_step_pt

    function shower_find_recoiler(shower, prt) result(recoiler)
      type(shower_t), intent(inout) :: shower
      type(parton_t), intent(in), target :: prt
      type(parton_t), pointer :: recoiler

      type(parton_t), pointer :: otherprt1, otherprt2
      integer :: n_int
      logical :: goon

      do_interactions: do n_int=1, size(shower%interactions)
         otherprt1=>shower%interactions(n_int)%i%partons(1)%p
         otherprt2=>shower%interactions(n_int)%i%partons(2)%p
         do
            goon=.false.
            if(associated(otherprt1%parent)) then
               if((parton_is_hadron(otherprt1%parent).eqv. .false.).and.(parton_is_simulated(otherprt1%parent))) then
!               if(parton_is_hadron(otherprt1%parent).eqv. .false.) then
                  otherprt1=>otherprt1%parent
                  goon=.true.
               end if
            end if
            if(associated(otherprt2%parent)) then
               if((parton_is_hadron(otherprt2%parent).eqv. .false.).and.(parton_is_simulated(otherprt2%parent))) then
!               if(parton_is_hadron(otherprt2%parent).eqv. .false.) then

                  otherprt2=>otherprt2%parent
                  goon=.true.
               end if
            end if
            if(goon.eqv..false.) exit
         end do
         if(associated(otherprt1, prt).or. associated(otherprt2, prt)) then
            exit do_interactions
         end if
      end do do_interactions

      recoiler => null()
      if(associated(otherprt1%parent, prt)) then
         recoiler=>otherprt2
      else if(associated(otherprt2%parent, prt)) then
         recoiler=>otherprt1
      else if(associated(otherprt1, prt)) then
         recoiler=>otherprt2
      else if(associated(otherprt2, prt)) then
         recoiler=>otherprt1
      else
         call shower_print(shower)
         call parton_print(prt)
         stop "BUG: no otherparton found"
      end if
    end function shower_find_recoiler

    subroutine shower_isr_step(shower, prt)
      type(shower_t), intent(inout) :: shower
      type(parton_t), target, intent(inout) :: prt
      type(parton_t), pointer :: otherprt => null()
      real(default) :: t, tstep
      real(default) :: integral, zufall
      real(default) :: temprand1, temprand2

!      print *, "shower_isr_step for parton ", prt%nr

      otherprt => shower_find_recoiler(shower, prt)
      
!!$      if(.not. otherprt%child1%belongstointeraction) then
!!$         otherprt=>otherprt%child1
!!$      end if

      t=max(prt%t, prt%child1%t)
      call tao_random_number(zufall)
      zufall=-twopi*log(zufall)  ! compare Integral and log(zufall) instead of zufall and exp(-Integral)
      integral=0._default
      call tao_random_number(temprand1)
      call tao_random_number(temprand2)
      tstep=max(abs(0.02_default*t)*temprand1, 0.02_default*temprand2*D_Min_t)
      if(t+0.5_default*tstep>-D_Min_t) then
         prt%t=parton_mass_squared(prt)
         call parton_set_simulated(prt)
      else
         prt%t=t+0.5_default*tstep
         integral=integral+tstep*integral_over_z_isr(prt, otherprt,(zufall-integral)/tstep)
         if(integral>zufall) then
            prt%t=t+0.5_default*tstep
            prt%x=prt%child1%x/prt%z
            call parton_set_simulated(prt)
         else
            prt%t=t+tstep
         end if
      end if

!      PRINT *, "  shower_isr_step finished"

    contains
      function integral_over_z_isr(prt, otherprt, ende) result(integral)
        type(parton_t), intent(inout) :: prt, otherprt
        real(default), intent(in) :: ende
        real(default) integral
        
        real(default) :: minz, maxz, z, shat,s
        integer :: quark
    
        ! calculate shat -> s of parton-parton system
        shat=( otherprt%momentum + prt%child1%momentum)**2
        ! calculate s -> s of hadron-hadron system
        s=(otherprt%initial%momentum + prt%initial%momentum)**2
        
        integral=0._default

        minz=prt%child1%x
        maxz=maxzz(shat, s)

        ! for gluon
        if(parton_is_gluon(prt%child1)) then
           ! 1: g->gg
           prt%typ=21
           prt%child2%typ=21
           z=minz
           prt%child2%t=abs(prt%t)
           call integral_over_z_part_isr(prt,otherprt, shat, minz, maxz, integral, ende)
           if(integral>ende) then
!              print *, "prt%typ=", prt%typ
              return
           end if
           ! 2: q->gq
           do quark=-D_Nf, D_Nf
              if (quark.eq.0) cycle
              prt%typ=quark
              prt%child2%typ=quark
              z=minz
              prt%child2%t=abs(prt%t)
              call integral_over_z_part_isr(prt,otherprt, shat, minz, maxz, integral, ende)
              if(integral>ende) then
!                 print *, "prt%typ=", prt%typ
                 return
              end if
           end do
        else if(parton_is_quark(prt%child1)) then
           ! 1: q->qg
           prt%typ=prt%child1%typ
           prt%child2%typ=21
           z=minz
           prt%child2%t=abs(prt%t)
           call integral_over_z_part_isr(prt,otherprt, shat, minz, maxz, integral, ende)
           if(integral>ende) then
!              print *, "prt%typ=", prt%typ
              return
           end if
           ! 2: g->qqbar
           prt%typ=21
           prt%child2%typ=-prt%child1%typ
           z=minz
           prt%child2%t=abs(prt%t)
           call integral_over_z_part_isr(prt,otherprt, shat, minz, maxz, integral, ende)
           if(integral>ende) then
!              print *, "prt%typ=", prt%typ
              return
           end if
        end if
      end function integral_over_z_isr
      
      subroutine integral_over_z_part_isr(prt, otherprt, shat ,minz, maxz, retvalue, ende)
        type(parton_t), intent(inout) :: prt, otherprt
        real(default), intent(in) :: shat, minz, maxz, ende
        real(default), intent(inout) :: retvalue
        real(default) :: z, zstep
        real(default) :: r1,r3,s1,s3
        real(default) :: pdf_divisor
        real(default) :: temprand
        
        real(default), parameter :: zstepfactor = 0.1_default
        real(default), parameter :: zstepmin = 0.0001_default
            
        if(D_print) print *, "integral_over_z_part_isr"
        
        pdf_divisor=get_pdf(prt%initial%typ, prt%child1%x, prt%t, prt%child1%typ)
        z=minz
        s1=shat+abs(otherprt%t)+abs(prt%child1%t)
        r1=sqrt(s1**2-4._default*abs(otherprt%t*prt%child1%t))
        zloop: do
           if(z.ge.maxz) then
              exit
           end if
           call tao_random_number(temprand)
           if(parton_is_gluon(prt%child1)) then
              if(parton_is_gluon(prt)) then
                 ! g-> gg -> divergencies at z->0 and z->1
                 zstep=max(zstepmin, temprand*zstepfactor*z*(1._default-z))
              else
                 ! q-> gq -> divergencies at z->0
                 zstep=max(zstepmin, temprand*zstepfactor*(1._default-z))
              end if
           else
              if(parton_is_gluon(prt)) then
                 ! g-> qqbar -> no divergencies
                 zstep=max(zstepmin, temprand*zstepfactor)
              else
                 ! q-> qg -> divergencies at z->1
                 zstep=max(zstepmin, temprand*zstepfactor*(1._default-z))
              end if
           end if
           zstep=min(zstep, maxz-z)
           prt%z=z+0.5_default*zstep
           s3=shat/prt%z+abs(otherprt%t)+abs(prt%t)
           r3=sqrt(s3**2-4._default*abs(otherprt%t*prt%t))
           prt%child2%t=min((s1*s3-r1*r3)/(2._default*abs(otherprt%t))-abs(prt%child1%t)-abs(prt%t),abs(prt%child1%t))
           do
              call parton_set_energy(prt%child2, sqrt(abs(prt%child2%t)))
              if(isr_only_onshell_emitted_partons) then
                 prt%child2%t = parton_mass_squared(prt%child2)
              else
                 call parton_next_t_ana(prt%child2)
              end if
              ! take limits by recoiler into account
              call parton_set_energy(prt, (shat/prt%z+abs(otherprt%t)-abs(prt%child1%t)-prt%child2%t)/(2._default*sqrt(shat)) )
              call parton_set_energy(prt%child2, parton_get_energy(prt)-parton_get_energy(prt%child1))
              !       check if E and t of prt%child2 are consistent
              if(parton_get_energy(prt%child2)**2<prt%child2%t & 
                   .and.prt%child2%t>parton_mass_squared(prt%child2)) then
                 ! E is too small to have p_T^2 = E^2 - t > 0  -> cycle to find another solution
                 cycle
              else
                 ! E is big enough -> exit
                 exit
              end if
           end do
           if(thetabar(prt, otherprt) .and. pdf_divisor>0._default .and. parton_get_energy(prt%child2)>0._default) then
              retvalue=retvalue+(zstep/prt%z)*(D_alpha_s_isr((1._default-prt%z)*prt%t)*P_prt_to_child1(prt)* &
                   get_pdf(prt%initial%typ, prt%child1%x/prt%z, prt%t, prt%typ))/(abs(prt%t)*pdf_divisor)
           end if
           if (retvalue>ende) then
              exit
           else
              z=z+zstep
           end if
        end do zloop
      end subroutine integral_over_z_part_isr
    end subroutine shower_isr_step

    function shower_generate_next_isr_branching(shower) result(next_brancher)
      ! returns a pointer to the parton with the next ISR branching / FSR branchings are ignored
      type(shower_t), intent(inout) :: shower
      type(parton_pointer_t) :: next_brancher

      integer i, index
      type(parton_t),  pointer :: prt
      real(default) :: maxscale

      next_brancher%p=> null()

      do
         if(shower_isr_is_finished(shower)) exit

         ! find mother with highest |t| or pt to be simulated
         index=0
         maxscale = 0._default
         call shower_sort_partons(shower)
         do i=1,size(shower%partons)
            prt=> shower%partons(i)%p
            if(.not. associated(prt)) cycle
            if(.not. isr_pt_ordered) then
               if(prt%belongstointeraction.eqv..true.) cycle
            end if
            if(prt%belongstoFSR) cycle
            if(parton_is_final(prt)) cycle
            if((prt%belongstoFSR.eqv..false.) .and. (parton_is_simulated(prt))) cycle
            index=i
            exit
         end do
         
         if(index==0) then
!            print *, " no branchable partons found"
            return
         end if

         prt=> shower%partons(index)%p
         
         ! ISR simulation
         if(isr_pt_ordered) then
            call shower_isr_step_pt(shower, prt)
         else
            call shower_isr_step(shower, prt)
         end if
         if(parton_is_simulated(prt)) then
            if(prt%t<0._default) then
               next_brancher%p=>prt
               if (isr_pt_ordered .eqv. .false.) call parton_generate_ps_ini(prt)
               exit
            else
               if(isr_pt_ordered.eqv. .false.) then
                  call shower_replace_parent_by_hadron(shower, prt%child1)
               else
                  call shower_replace_parent_by_hadron(shower, prt)
               end if
            end if
         end if
      end do
      
      ! some bookkeeping
      call shower_sort_partons(shower)
      call shower_boost_to_CMframe(shower)         ! really necessary?
      call shower_rotate_to_z(shower)              ! really necessary?
!      print *, "shower_generate_next_isr_branching finished"
    end function shower_generate_next_isr_branching

    subroutine shower_generate_fsr_for_partons_emitted_in_ISR(shower)
      type(shower_t), intent(inout) :: shower
      integer :: n_int, i
      type(parton_t), pointer :: prt

      if(isr_only_onshell_emitted_partons) return

      ! search for all emitted and branched partons

      interactions_loop: do n_int=1, size(shower%interactions)
         incoming_partons_loop: do i=1,2
            prt=>shower%interactions(n_int)%i%partons(i)%p

            parent_partons_loop: do
               if(associated(prt%parent)) then
                  if(.not. parton_is_hadron(prt%parent)) then
                     prt=>prt%parent
                  else
                     exit
                  end if
               else
                  exit
               end if
               if(associated(prt%child2)) then
                  if(parton_is_branched(prt%child2)) then
                     call shower_parton_generate_fsr(shower, prt%child2)
                  end if
               else
                  ! STOP "Bug: no child2 associated?"
               end if
            end do parent_partons_loop
         end do incoming_partons_loop
      end do interactions_loop
    end subroutine shower_generate_fsr_for_partons_emitted_in_ISR

    subroutine shower_execute_next_isr_branching(shower, prtp)
      ! executes the branching generated by shower_generate_next_isr_branching, that means it generates the flavours, momenta, etc...
      type(shower_t), intent(inout) :: shower
      type(parton_pointer_t), intent(inout) :: prtp
      type(parton_t), pointer :: prt, otherprt
      type(parton_t), pointer :: prta, prtb, prtc, prtr
      real(default) :: mar, mbr
      real(default) :: phirand

!      print *, "shower_execute_next_isr_branching"
      if(.not. associated(prtp%p)) then
         stop "Bug: prtp not associated"
      end if

      prt=>prtp%p

      if( ((isr_pt_ordered.eqv..false.).and.(prt%t>-D_Min_t)) .or. ((isr_pt_ordered).and.(prt%scale<D_Min_scale)) ) then
         stop "BUG: no branching to be executed "
      end if

      otherprt => shower_find_recoiler(shower, prt)
      if(isr_pt_ordered) then
         ! get the recoiler
         otherprt => shower_find_recoiler(shower, prt)
         if(associated(otherprt%parent)) then
            ! Why only for pt ordered
            if((parton_is_hadron(otherprt%parent).eqv..false.) .and. (isr_pt_ordered.eqv..true.)) otherprt => otherprt%parent
         end if
         if(.not. associated(prt%parent)) then
            call shower_add_parent(shower, prt)
         end if
         prt%parent%belongstoFSR = .false.
         if(.not. associated(prt%parent%child2)) then
            call shower_add_child(shower, prt%parent, 2)
         end if

         prta => prt%parent            ! new parton a with branching a->bc
         prtb => prt                   ! former parton
         prtc => prt%parent%child2     ! emitted parton
         prtr => otherprt              ! recoiler

         mbr = (prtb%momentum + prtr%momentum)**1
         mar = mbr / sqrt(prt%z)

         ! 1. assume you are in the restframe
         ! 2. rotate by random phi
         call tao_random_number(phirand)
         phirand=twopi*phirand
         call shower_apply_lorentztrafo(shower, rotation(cos(phirand), sin(phirand),vector3_canonical(3)))
         ! 3. Put the b off-shell
         ! and
         ! 4. construct the massless a
         ! and the parton (eventually emitted by a)
         
         ! generate the flavour of the parent (prta)
         if(prtb%aux_pt .ne. 0) prta%typ = prtb%aux_pt
         if(parton_is_quark(prtb)) then
            if(prta%typ == prtb%typ) then
               ! (anti)-quark -> (anti-)quark + gluon
               prta%typ = prtb%typ   ! quarks have same flavour
               prtc%typ = 21         ! emitted gluon
            else
               ! gluon -> quark + antiquark
               prta%typ = 21
               prtc%typ = - prtb%typ
            end if
         else if(parton_is_gluon(prtb)) then
            prta%typ = 21
            prtc%typ = 21
         else
!            STOP "Bug in shower_execute_nexT_branching: neither quark nor gluon"
         end if

         prta%initial => prtb%initial
         prta%belongstoFSR=.false.
         prta%scale=prtb%scale
         prta%x = prtb%x / prtb%z

         prtb%momentum = vector4_moving( (mbr**2+prtb%t)/(2._default*mbr), vector3_canonical(3) * &
              sign( (mbr**2-prtb%t)/(2._default*mbr) , vector4_get_component(prtb%momentum, 3)))
         prtr%momentum = vector4_moving( (mbr**2-prtb%t)/(2._default*mbr), vector3_canonical(3) * &
              sign( (mbr**2-prtb%t)/(2._default*mbr) , vector4_get_component(prtr%momentum, 3)))

         prta%momentum = vector4_moving ( (0.5_default/mbr)*( (mbr**2/prtb%z) + prtb%t - parton_mass_squared(prtc) ), &
              vector3_null)
         prta%momentum = vector4_moving ( parton_get_energy(prta) , vector3_canonical(3) * &
              (0.5_default/vector4_get_component(prtb%momentum, 3))*((mbr**2/prtb%z) - 2._default & 
              * parton_get_energy(prtr)*parton_get_energy(prta) ) )
         if(parton_get_energy(prta)**2 - vector4_get_component(prta%momentum,3)**2 - parton_mass_squared(prtc) &
              .ge. 0._default) then
            ! This SHOULD be always fulfilled???
            prta%momentum = vector4_moving ( parton_get_energy(prta) , vector3_moving( &
                 (/ sqrt(parton_get_energy(prta)**2 - vector4_get_component(prta%momentum,3)**2 - & 
                 parton_mass_squared(prtc)), 0._default, vector4_get_component(prta%momentum, 3) /) ) )
         end if
         prtc%momentum = prta%momentum - prtb%momentum

         ! 5. rotate to have a along z-axis
         call shower_boost_to_CMframe(shower)
         call shower_rotate_to_z(shower)
         ! 6. rotate back in phi
         call shower_apply_lorentztrafo(shower, rotation(cos(-phirand), sin(-phirand), & 
              vector3_canonical(3) ) )
      else
         if(prt%child2%t>parton_mass_squared(prt%child2)) then
            call shower_add_children_of_emitted_timelike_parton(shower, prt%child2)
            call parton_set_simulated(prt%child2)
         end if
      
         call shower_add_parent(shower, prt)
         call shower_add_child(shower, prt%parent, 2)
         
         prt%parent%momentum=prt%momentum
         prt%parent%t=prt%t
         prt%parent%x=prt%x
         prt%parent%initial => prt%initial
         prt%parent%belongstoFSR=.false.

         prta => prt
         prtb => prt%child1
         prtc => prt%child2
      end if
      if(isr_pt_ordered) then
         call parton_generate_ps_ini(prt%parent)
      else
         call parton_generate_ps_ini(prt)
      end if
     
      ! add color connections
      if(parton_is_quark(prtb)) then
         
         if(prta%typ == prtb%typ) then
            if(prtb%typ>0) then
               ! quark -> quark + gluon
               prtc%c2 = prtb%c1
               prtc%c1 = shower_get_next_color_nr(shower)
               prta%c1 = prtc%c1
            else
               ! antiquark -> antiquark + gluon
               prtc%c1 = prtb%c2
               prtc%c2 = shower_get_next_color_nr(shower)
               prta%c2 = prtc%c2
            end if
         else
            ! gluon -> quark + antiquark
            if(prtb%typ>0) then
               ! gluon -> quark + antiquark
               prta%c1 = prtb%c1
               prtc%c1 = 0
               prtc%c2 = shower_get_next_color_nr(shower)
               prta%c2 = prtc%c2
            else
               ! gluon -> antiquark + quark
               prta%c2 = prtb%c2
               prtc%c1 = shower_get_next_color_nr(shower)
               prtc%c2 = 0
               prta%c1 = prtc%c1
            end if
         end if
      else if(parton_is_gluon(prtb)) then
         if(parton_is_gluon(prta)) then
            ! g -> gg
            prtc%c2 = prtb%c1
            prtc%c1 = shower_get_next_color_nr(shower)
            prta%c1 = prtc%c1
            prta%c2 = prtb%c2
         else if(parton_is_quark(prta)) then
            if(prta%typ > 0) then
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
         !            STOP "Bug in shower_execute_nexT_branching: neither quark nor gluon"
      end if

      call shower_sort_partons(shower)
      call shower_boost_to_CMframe(shower)
      call shower_rotate_to_z(shower)

!      print *, "  shower_execute_next_isr_branching finished"
    end subroutine shower_execute_next_isr_branching

    subroutine shower_remove_parents_and_stuff(shower, prt)
      type(shower_t), intent(inout) :: shower
      type(parton_t), intent(inout), target :: prt
      type(parton_t), pointer :: actprt, nextprt

!      print *, " shower_remove_parents for parton ", prt%nr

      nextprt=>prt%parent
      actprt=>null()

      ! remove children of emitted timelike parton
      if(associated(prt%child2)) then
         if(associated(prt%child2%child1)) then
            call shower_remove_parton_from_partons_recursive(shower, prt%child2%child1)
         end if
         prt%child2%child1=>null()
         if(associated(prt%child2%child2)) then
            call shower_remove_parton_from_partons_recursive(shower, prt%child2%child2)
         end if
         prt%child2%child2=>null()
      end if

      do
         actprt=>nextprt
         if(.not. associated(actprt)) then
            exit
         else if(parton_is_hadron(actprt)) then
            ! remove beam-remnant
            call shower_remove_parton_from_partons(shower, actprt%child2)
            exit
         end if
         if(associated(actprt%parent)) then
            nextprt=>actprt%parent
         else
            nextprt=>null()
         end if

!         print *, " removing parton ", actprt%child2%nr
         call shower_remove_parton_from_partons_recursive(shower, actprt%child2)
!         print *, " removing parton ", actprt%nr
         call shower_remove_parton_from_partons(shower, actprt)

      end do      
      prt%parent=>null()

!      print *, " shower_remove_parents for parton finished"
    end subroutine shower_remove_parents_and_stuff

    function shower_get_ISR_scale(shower) result (scale)
      type(shower_t), intent(in) :: shower
      real(kind=default) :: scale

      type(parton_t), pointer :: prt1, prt2
      integer :: i

      scale = 0._default
      do i=1, size(shower%interactions)
         call interaction_find_partons_nearest_to_hadron(shower%interactions(i)%i, &
              prt1, prt2)
         if((parton_is_simulated(prt1).eqv..false.).and.(abs(prt1%scale).gt.scale)) scale = abs(prt1%scale)
         if((parton_is_simulated(prt1).eqv..false.).and.(abs(prt2%scale).gt.scale)) scale = abs(prt2%scale)
      end do
!!$      call shower_print(shower)
!!$      call parton_print(prt1)
!!$      call parton_print(prt2)
!!$      print *, "returning: ", scale
!!$      pause
    end function shower_get_ISR_scale

    subroutine shower_set_max_ISR_scale(shower, newscale)
      type(shower_t), intent(inout) :: shower
      real(default), intent(in) :: newscale
      real(default) :: scale
      type(parton_t), pointer :: prt
      
      integer :: i,j

!      print *, "shower_set_max_ISR_scale"

      if(isr_pt_ordered) then
         ! scale = scale
      else 
         scale = -abs(newscale)
      end if


      interactions: do i=1, size(shower%interactions)
         partons: do j=1,2
            prt=>shower%interactions(i)%i%partons(j)%p
            do
               if(isr_pt_ordered.eqv..false.) then
                  if(prt%belongstointeraction.eqv..true.) prt=>prt%parent
               end if
               if(prt%t<scale) then
                  if(associated(prt%parent)) then
                     prt=>prt%parent
                  else
                     exit   ! unresolved prt found
                  end if
               else
                  exit   ! prt with scale above newscale found
               end if
            end do
            if(isr_pt_ordered .eqv. .false.) then
               if(prt%child1%belongstointeraction .or. parton_is_hadron(prt)) then
                  ! don't reset scales of "first" spacelike partons in virtuality ordered shower or hadrons
                  cycle
               end if
            else
               if(parton_is_hadron(prt)) then
                  ! don't reset scales of hadrons 
                  cycle
               end if
            end if
            if(isr_pt_ordered) then
               prt%scale = scale
            else
               prt%t=scale
            end if
            call parton_set_simulated(prt, .false.)
            call shower_remove_parents_and_stuff(shower, prt)
         end do partons
      end do interactions

!      print *, "shower_set_max_ISR_scale finished"
    end subroutine shower_set_max_ISR_scale

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!! new version 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!$    subroutine shower_interaction_generate_fsr(shower, interaction)
!!$      type(shower_t), intent(inout) :: shower
!!$      type(my_interaction_t), intent(inout) :: interaction
!!$      type(parton_pointer_t), dimension(:), allocatable :: partons   ! array of partons whose children are to be evolved
!!$
!!$      ! arrange partons to be included in <partons>
!!$      ! for qqbar state: include imaginary mother + first branching
!!$      call shower_prepare_for_simulate_fsr_ana(shower, interaction%out1%p, interaction%out2%p)
!!$
!!$      allocate(partons(1:1))
!!$      partons(1)%p => interaction%out1%p%child1
!!$      call shower_parton_pointer_array_generate_fsr(shower, partons)
!!$      call shower_parton_update_color_connections(shower, interaction%out1%p%child1%child1)
!!$      call shower_parton_update_color_connections(shower, interaction%out1%p%child1%child2)
!!$    end subroutine shower_interaction_generate_fsr

    subroutine shower_interaction_generate_fsr2ton(shower, interaction)
      type(shower_t), intent(inout) :: shower
      type(my_interaction_t), intent(inout) :: interaction

      type(parton_t), pointer :: prt

      prt=>interaction%partons(3)%p
      do
         if(.not. associated(prt%parent)) exit
         prt=>prt%parent
      end do
      call shower_parton_generate_fsr(shower, prt)
      call shower_parton_update_color_connections(shower, prt)
    end subroutine shower_interaction_generate_fsr2ton

    subroutine shower_parton_generate_fsr(shower, prt)
      ! perform the fsr for one parton, it is assumed, that the parton already branched -> its children are to be simulated
      ! this procedure is intended for branched FSR-partons emitted in the ISR
      type(shower_t), intent(inout) :: shower
      type(parton_t), intent(inout), target :: prt
      type(parton_pointer_t), dimension(:), allocatable :: partons

      if(parton_is_branched(prt) .eqv. .false.) then
         print *, " error in shower_parton_generate_fsr: parton not branched"
         return
      end if
      if(parton_is_simulated(prt%child1) .or. parton_is_simulated(prt%child2)) then
         print *, " error in shower_parton_generate_fsr: children already simulated for parton ", prt%nr
         return
      end if
      
      allocate(partons(1:1))
      partons(1)%p => prt
      call shower_parton_pointer_array_generate_fsr(shower, partons)

    end subroutine shower_parton_generate_fsr

    recursive subroutine shower_parton_pointer_array_generate_fsr(shower, partons)
      type(shower_t), intent(inout) :: shower
      type(parton_pointer_t), dimension(:), allocatable, intent(inout) :: partons
      type(parton_pointer_t), dimension(:), allocatable :: partons_new

      integer :: i, size_partons, size_partons_new

      size_partons=size(partons)
      if(size_partons .eq. 0) return

      ! sort partons -> necessary ?? -> probably not

      ! simulate highest/first parton
      call shower_simulate_children_ana(shower, partons(1)%p)

      ! check for new daughters to be included in new_partons
      size_partons_new=size_partons-1   ! partons(1) not needed anymore
      if(parton_is_branched(partons(1)%p%child1)) size_partons_new=size_partons_new+1
      if(parton_is_branched(partons(1)%p%child2)) size_partons_new=size_partons_new+1

      allocate(partons_new(1:size_partons_new))

      if(size_partons>1) then
         do i=2, size_partons
            partons_new(i-1)%p => partons(i)%p
         end do
      end if
      if(parton_is_branched(partons(1)%p%child1)) partons_new(size_partons)%p=> partons(1)%p%child1
      if(parton_is_branched(partons(1)%p%child2)) then
         ! check if child1 is already included
         if(size_partons_new .eq. size_partons) then
            partons_new(size_partons)%p=> partons(1)%p%child2
         else if(size_partons_new .eq. size_partons+1) then
            partons_new(size_partons+1)%p=> partons(1)%p%child2
         else
            STOP "BUG: wrong sizes in shower_parton_pointer_array_generate_fsr"
         end if
      end if
      deallocate(partons)

!      print *, "end subroutine shower_parton_pointer_array_generate_fsr"
      call shower_parton_pointer_array_generate_fsr(shower, partons_new)

    end subroutine shower_parton_pointer_array_generate_fsr

  recursive subroutine shower_parton_update_color_connections(shower, prt)
    type(shower_t), intent(inout) :: shower
    type(parton_t), intent(inout) :: prt
    real(default) :: temprand

    if( (.not. associated(prt%child1)) .or. (.not. associated(prt%child2)) ) return

    if(parton_is_gluon(prt)) then
       if(parton_is_quark(prt%child1)) then
          ! give the quark the colorpartner and the antiquark the anticolorpartner
          if(prt%child1%typ>0) then
             ! child1 is quark, child2 is antiquark
             prt%child1%c1=prt%c1
             prt%child2%c2=prt%c2
          else
             ! child1 is antiquark, child2 is quark
             prt%child1%c2=prt%c2
             prt%child2%c1=prt%c1
          end if
       else
          ! g -> gg splitting -> random choosing of partners
          call tao_random_number(temprand)
          if(temprand > 0.5_default) then
             prt%child1%c1=prt%c1
             prt%child1%c2=shower_get_next_color_nr(shower)
             prt%child2%c1=prt%child1%c2
             prt%child2%c2=prt%c2
          else
             prt%child1%c2=prt%c2
             prt%child2%c1=prt%c1
             prt%child2%c2=shower_get_next_color_nr(shower)
             prt%child1%c1=prt%child2%c2
          end if
       end if
    else if(parton_is_quark(prt)) then
       if(parton_is_quark(prt%child1)) then
          if(prt%child1%typ>0) then
             ! q -> q + g
             prt%child2%c1=prt%c1
             prt%child2%c2=shower_get_next_color_nr(shower)
             prt%child1%c1=prt%child2%c2
          else
             ! qbar -> qbar + g
             prt%child2%c2=prt%c2
             prt%child2%c1=shower_get_next_color_nr(shower)
             prt%child1%c2=prt%child2%c1
          end if
       else
          if(prt%child2%typ>0) then
             ! q -> g + q
             prt%child1%c1=prt%c1
             prt%child1%c2=shower_get_next_color_nr(shower)
             prt%child2%c1=prt%child1%c2
          else
             ! qbar -> g + qbar
             prt%child1%c2=prt%c2
             prt%child1%c1=shower_get_next_color_nr(shower)
             prt%child2%c2=prt%child1%c1
          end if
       end if
    end if

    call shower_parton_update_color_connections(shower, prt%child1)
    call shower_parton_update_color_connections(shower, prt%child2)
  end subroutine shower_parton_update_color_connections

!!!!!!!!!!!!!!!!!!!!!!!!!
!!! PDF !!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!

  function get_pdf(mother, x, Q2, daughter) result(pdf)
 ! wrapper function to return parton densities
 !   type(shower_t), intent(in), pointer :: shower
    integer, intent(in) :: mother, daughter
    real(default), intent(in) :: x, Q2
    real(default) :: pdf

    ! arguments for evolvepdf need to be defined as double explicitly
    real(double), save :: f(-6:6) = 0._double
    real(double), save :: lastx, lastQ2 = 0._double

    if(abs(mother)/=2212) then
       stop "Bug: pdf only implemented for (anti-)proton"
    else
       if(x>0._default .and. x<1._default) then
          if(DBLE(Q2) .ne. lastQ2 .or. DBLE(x) .ne. lastx) then
             call evolvePDF(DBLE(x),sqrt(abs(DBLE(Q2))),f)
          end if
          if (abs(daughter)>=1 .and. abs(daughter)<=6) then
             pdf=f(daughter*sign(1,mother))/x
          else if(daughter==21) then
             pdf=f(0)/x
          else
             print *, "error in pdf"
             pdf=0._default
          end if
       else
          pdf=0._default
       end if
    end if
    lastQ2 = DBLE(Q2)
    lastx  = DBLE(x)
  end function get_pdf

  function get_xpdf(mother, x, Q2, daughter) result(pdf)
 ! wrapper function to return momentum densities
 !   type(shower_t), intent(in), pointer :: shower
    integer, intent(in) :: mother, daughter
    real(default), intent(in) :: x, Q2
    real(default) :: pdf

    ! arguments for evolvepdf need to be defined as double explicitly
    real(double), save :: f(-6:6) = 0._double
    real(double), save :: lastx, lastQ2 =0._double

    if(abs(mother)/=2212) then
       stop "Bug: pdf only implemented for (anti-)proton"
    else
       if(x>0._default .and. x<1._default) then
          if(DBLE(Q2) .ne. lastQ2 .or. DBLE(x) .ne. lastx) then
             call evolvePDF(DBLE(x),sqrt(abs(DBLE(Q2))),f)
          end if
          if (abs(daughter)>=1 .and. abs(daughter)<=6) then
             pdf=f(daughter*sign(1,mother))
          else if(daughter==21) then
             pdf=f(0)
          else
             print *, "error in pdf"
             pdf=0._default
          end if
       else
          pdf=0._default
       end if
    end if
    lastQ2 = DBLE(Q2)
    lastx  = DBLE(x)
  end function get_xpdf

  end module shower_module
