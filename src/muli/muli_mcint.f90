! WHIZARD 2.4.0 Nov 28 2016
! 
! Copyright (C) 1999-2016 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@t-online.de>
!     Bijan Chokoufe <bijan.chokoufe@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     So Young Shim <soyoung.shim@desy.de>
!     Florian Staub <florian.staub@cern.ch>  
!     Christian Weiss <christian.weiss@desy.de>
!     and Hans-Werner Boschmann, Felix Braam, 
!     Sebastian Schmidt, So-young Shim, Daniel Wiesler 
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

module muli_mcint
  use kinds, only: default  
  use constants
  use muli_base
  use tao_random_numbers !NODEP!
  use muli_interactions

  implicit none
  private  

  integer, parameter :: max_n = 2**30
  real(default), parameter :: max_d = one * max_n
  real(default), parameter, dimension(2,2) :: &
       unit_square = reshape([zero,zero,one,one], [2,2])


  public :: sample_region_t
  public :: sample_2d_t
  public :: sample_3d_t
  public :: sample_int_kind_t
  public :: sample_inclusive_t

  type, extends (ser_class_t) :: sample_region_t
     integer :: n_hits = 0
     integer :: n_alloc = 0
     real(default), dimension(2,2) :: corners = unit_square
     real(default), dimension(:,:), allocatable :: hyp_hits
   contains
     procedure :: write_to_marker => sample_region_write_to_marker
     procedure :: read_from_marker => sample_region_read_from_marker
     procedure :: print_to_unit => sample_region_print_to_unit  
     procedure, nopass :: get_type => sample_region_get_type  
     procedure :: initialize => sample_region_initialize
     procedure :: generate_hit => sample_region_generate_hit
     procedure :: confirm_hit => sample_region_confirm_hit
     procedure :: split => sample_region_split  
     procedure :: write_hits => sample_region_write_hits
     procedure :: is_full => sample_region_is_full  
     procedure :: move_components => sample_region_move_components
     procedure :: mean => sample_region_mean
     procedure :: area => sample_region_area  
     procedure :: density => sample_region_density
     procedure :: contains => sample_region_contains
     procedure :: to_generator => sample_region_to_generator    
  end type sample_region_t
  
  type, extends (ser_class_t) :: sample_2d_t
     integer :: n_regions = 0
     integer :: n_alloc = 0
     integer :: n_hits = 0
     real(default), dimension(2) :: range = [0,1]
     type(sample_region_t), dimension(:), allocatable :: regions
   contains
     procedure :: write_to_marker => sample_2d_write_to_marker
     procedure :: read_from_marker => sample_2d_read_from_marker
     procedure :: print_to_unit => sample_2d_print_to_unit
     procedure, nopass :: get_type => sample_2d_get_type    
     procedure :: initialize => sample_2d_initialize
     procedure :: contains => sample_2d_contains
     procedure :: generate_hit => sample_2d_generate_hit
     procedure :: confirm_hit => sample_2d_confirm_hit
     procedure :: split => sample_2d_split
     procedure :: push => sample_2d_push
     procedure :: write_hits => sample_2d_write_hits
     procedure :: is_full => sample_2d_is_full
     procedure :: move_components => sample_2d_move_components
     procedure :: thickness => sample_2d_thickness
     procedure :: analyse => sample_2d_analyse
     procedure :: to_generator => sample_2d_to_generator
     procedure :: mean => sample_2d_mean  
  end type sample_2d_t
  
  type, extends (ser_class_t) :: sample_3d_t
     integer::n_slices=0
     integer::n_alloc=0
     type(sample_2d_t), dimension(:),allocatable::slices     
   contains
     procedure :: write_to_marker => sample_3d_write_to_marker
     procedure :: read_from_marker => sample_3d_read_from_marker
     procedure :: print_to_unit => sample_3d_print_to_unit
     procedure, nopass :: get_type => sample_3d_get_type  
     procedure :: measure => sample_3d_measure  
     procedure :: to_generator => sample_3d_to_generator
     generic :: initialize => sample_3d_initialize  
     procedure :: sample_3d_initialize
     procedure :: sample_3d_generate_hit
     generic :: generate_hit => sample_3d_generate_hit    
     procedure :: sample_3d_confirm_hit
     generic :: confirm_hit => sample_3d_confirm_hit    
     procedure :: enlarge => sample_3d_enlarge
  end type sample_3d_t
  
  type, extends (sample_3d_t) :: sample_int_kind_t
     integer :: n_proc = 0
     integer(kind=i64) :: n_tries = 0
     integer :: n_hits = 0
     integer :: n_over = 0
     integer, dimension(:), allocatable :: hits, weights, processes
     real(default) :: overall_boost = 1E-1_default
   contains
     procedure :: write_to_marker => sample_int_kind_write_to_marker
     procedure :: read_from_marker => sample_int_kind_read_from_marker
     procedure :: print_to_unit => sample_int_kind_print_to_unit
     procedure, nopass :: get_type => sample_int_kind_get_type
     procedure :: to_generator => sample_int_kind_to_generator  
     procedure :: process_id => sample_int_kind_process_id
     procedure :: sample_int_kind_initialize
     generic :: initialize => sample_int_kind_initialize  
     procedure :: sample_int_kind_generate_hit
     procedure :: mcgenerate_hit => sample_int_kind_mcgenerate_hit
     generic :: generate_hit => sample_int_kind_generate_hit
     procedure :: sample_int_kind_confirm_hit
     generic :: confirm_hit => sample_int_kind_confirm_hit    
     procedure :: analyse => sample_int_kind_analyse  
  end type sample_int_kind_t
  
  type, extends (ser_class_t) :: sample_inclusive_t
     integer :: n_alloc = 0
     integer(kind=i64) :: n_tries_sum = i_zero
     integer(kind=i64) :: n_over_sum = i_zero
     integer(kind=i64) :: n_hits_sum = i_zero
     type(sample_int_kind_t), dimension(:), allocatable :: int_kinds
   contains
     procedure :: write_to_marker => sample_inclusive_write_to_marker
     procedure :: read_from_marker => sample_inclusive_read_from_marker
     procedure :: print_to_unit => sample_inclusive_print_to_unit
     procedure, nopass :: get_type => sample_inclusive_get_type  
     procedure :: process_id => sample_inclusive_process_id 
     procedure :: initialize => sample_inclusive_initialize
     procedure :: finalize => sample_inclusive_finalize
     procedure :: generate_hit => sample_inclusive_generate_hit
     procedure :: mcgenerate_hit => sample_inclusive_mcgenerate_hit
     procedure :: confirm_hit => sample_inclusive_confirm_hit
     procedure :: sum_up => sample_inclusive_sum_up
     procedure :: analyse => sample_inclusive_analyse
     procedure :: to_generator => sample_inclusive_to_generator
     procedure :: allocate => sample_inclusive_allocate    
  end type sample_inclusive_t
  
  
contains

  subroutine sample_region_write_to_marker (this, marker, status)
    class(sample_region_t), intent(in) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status
    integer :: n
    call marker%mark_begin ("sample_region_t")
    call marker%mark ("n_hits", this%n_hits)
    call marker%mark ("n_alloc", this%n_alloc)
    call marker%mark ("lower_corner", this%corners(1:2,1))
    call marker%mark ("upper_corner", this%corners(1:2,2))
    if (allocated (this%hyp_hits)) then
       call marker%mark ("hyp_hits", this%hyp_hits (1:3,:this%n_hits))
    else
       call marker%mark_nothing ("hyp_hits")
    end if
    call marker%mark_end ("sample_region_t")
  end subroutine sample_region_write_to_marker
  
  subroutine sample_region_read_from_marker (this, marker, status)
    class(sample_region_t), intent(out) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status  
    integer :: n
    call marker%pick_begin ("sample_region_t", status=status)
    call marker%pick ("n_hits", this%n_hits, status)
    call marker%pick ("n_alloc", this%n_alloc, status)
    call marker%pick ("lower_corner", this%corners(1:2,1), status)
    call marker%pick ("upper_corner", this%corners(1:2,2), status)
    if (allocated (this%hyp_hits))  deallocate (this%hyp_hits)
    call marker%verify_nothing ("hyp_hits", status)
    if (.not. status == serialize_nothing) then
       allocate (this%hyp_hits (3,this%n_alloc))
       call marker%pick ("hyp_hits", this%hyp_hits (1:3,:this%n_hits), status)
    end if
    call marker%pick_end ("sample_region_t", status)
  end subroutine sample_region_read_from_marker
  
  subroutine sample_region_print_to_unit &
       (this, unit, parents, components, peers)
    class(sample_region_t), intent(in) :: this
    integer, intent(in) :: unit
    integer(dik), intent(in) :: parents, components, peers
    write (unit, "(1x,A)")      "components of sample_region_t"
    write (unit, "(3x,A,I10)")  "n_hits:           ", this%n_hits
    write (unit, "(3x,A,I10)")  "n_alloc:          ", this%n_alloc
    write (unit, "(3x,4(E20.10))") "corners:          ", this%corners
    if (allocated (this%hyp_hits) .and. this%n_hits > 0) then
       if (components > 0) then
          write (unit,"(3x,A)")  "hits:"
          print *, shape (this%hyp_hits)
          write (unit, "(3(e20.10))")  this%hyp_hits (1:3, this%n_hits)
       else
          write (unit, "(3x,A)")  "skipping hits."
       end if
    else
       write (unit, "(3x,A)")  "hits are not allocated."
    end if
  end subroutine sample_region_print_to_unit
  
  pure subroutine sample_region_get_type (type)
    character(:), allocatable, intent(out) :: type
    allocate (type, source="sample_region_t")
  end subroutine sample_region_get_type

  subroutine sample_region_initialize (this, n_alloc)
    class(sample_region_t), intent(out) :: this
    integer, intent(in) :: n_alloc
    if (allocated (this%hyp_hits))  deallocate (this%hyp_hits)
    allocate (this%hyp_hits (3,n_alloc))
    this%n_alloc = n_alloc
  end subroutine sample_region_initialize
  
  pure subroutine sample_region_generate_hit (this, rnd, area, hit)
    class(sample_region_t), intent(in) :: this
    integer, intent(in), dimension(2) :: rnd
    real(default), dimension(2), intent(out) :: hit
    real(default), intent(out) :: area
    call muli_mcint_generate_hit (rnd, this%corners, hit)
    area = this%area ()
  end subroutine sample_region_generate_hit
  
  subroutine sample_region_confirm_hit (this, hit)
    class(sample_region_t), intent(inout) :: this
    real(default), dimension(3), intent(in) :: hit
    ! print *,"sample_region_confirm_hit: ", this%n_hits, this%n_alloc, hit
    this%n_hits = this%n_hits + 1
    if (this%n_hits <= this%n_alloc) then
       this%hyp_hits (1:3, this%n_hits) = hit
    else
       print *, "sample_region_confirm_hit: Region is already full."
    end if
  end subroutine sample_region_confirm_hit
  
  subroutine sample_region_split (this, pos, dimX, n_alloc, lower, upper)
    class(sample_region_t), intent(in) :: this
    type(sample_region_t), intent(out) :: lower, upper
    real(default), dimension(3) :: hit
    real(default), intent(in) :: pos
    integer, intent(in) :: dimX, n_alloc
    integer :: n_hit    
    call lower%initialize (n_alloc)
    call upper%initialize (n_alloc)
    do n_hit = 1, this%n_hits
       hit = this%hyp_hits (1:3, n_hit)
       if (hit(dimX) < pos) then
          call lower%confirm_hit (hit)
       else
          call upper%confirm_hit (hit)
       end if
    end do
    lower%corners = this%corners
    upper%corners = this%corners
    if (dimX < 3) then
       lower%corners(dimX,2) = pos
       upper%corners(dimX,1) = pos
    end if
  end subroutine sample_region_split
  
  subroutine sample_region_write_hits (this, unit)
    class(sample_region_t), intent(in) :: this
    integer, intent(in) :: unit
    integer :: n
    do n = 1, this%n_hits
       write (unit, *)  this%hyp_hits (1:3,n)
    end do
  end subroutine sample_region_write_hits
  
  elemental logical function sample_region_is_full (this)
    class(sample_region_t), intent(in) :: this
    sample_region_is_full = this%n_alloc == this%n_hits
  end function sample_region_is_full
  
  subroutine sample_region_move_components (this, that)
    class(sample_region_t), intent(inout) :: this
    class(sample_region_t), intent(out) :: that
    that%n_alloc = this%n_alloc
    that%n_hits = this%n_hits
    that%corners = this%corners
    call move_alloc (this%hyp_hits, that%hyp_hits)
    this%n_alloc = 0
    this%n_hits = 0
  end subroutine sample_region_move_components
  
  elemental function sample_region_mean (this, dim)
    real(default) :: sample_region_mean
    class(sample_region_t), intent(in) :: this
    integer, intent(in) :: dim
    sample_region_mean = sum (this%hyp_hits (dim,1:this%n_hits)) / this%n_hits
  end function sample_region_mean
  
  elemental function sample_region_area (this)
    real(default) :: sample_region_area
    class(sample_region_t), intent(in) :: this
    sample_region_area = product (this%corners(1:2,2) - this%corners(1:2,1))
  end function sample_region_area
  
  elemental function sample_region_density (this)
    real(default) :: sample_region_density
    class(sample_region_t), intent(in) :: this
    sample_region_density = this%n_hits / this%area ()
  end function sample_region_density

  pure logical function sample_region_contains (this, hit)
    class(sample_region_t), intent(in) :: this
    real(default), intent(in), dimension(3) :: hit
    sample_region_contains = (this%corners(1,1) <= hit(1) .and. &
         hit(1) <= this%corners(1,2) .and. &
         this%corners(2,1) <= hit(2) .and. &
         hit(2) <= this%corners(2,2))
  end function sample_region_contains
  
  subroutine sample_region_to_generator (this)
    class(sample_region_t), intent(inout) :: this
    if (allocated (this%hyp_hits))  deallocate (this%hyp_hits)
    this%n_alloc = 0
  end subroutine sample_region_to_generator
  
  subroutine sample_2d_write_to_marker (this, marker, status)
    class(sample_2d_t), intent(in) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status
    integer :: n
    call marker%mark_begin ("sample_2d_t")
    call marker%mark ("n_regions", this%n_regions)
    call marker%mark ("n_alloc", this%n_alloc)
    call marker%mark ("n_hits", this%n_hits)
    call marker%mark ("range", this%range)
    if (this%n_regions > 0) then
       call marker%mark_instance_begin &
            (this%regions(1), name="sample_2d_t", shape=shape (this%regions))
       do n = 1, this%n_regions
          call sample_region_write_to_marker (this%regions(n), marker, status)
       end do
       call marker%mark_instance_end ()
    end if
    call marker%mark_end ("sample_2d_t")
  end subroutine sample_2d_write_to_marker
  
  subroutine sample_2d_read_from_marker (this, marker, status)
    class(sample_2d_t), intent(out) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status  
    integer :: n
    call marker%pick_begin ("sample_2d_t", status=status)
    call marker%pick ("n_regions", this%n_regions, status)
    call marker%pick ("n_alloc", this%n_alloc, status)
    call marker%pick ("n_hits", this%n_hits, status)
    call marker%pick ("range", this%range, status)
    if (this%n_regions > 0) then
       call marker%pick_begin ("regions", status=status)
       allocate (this%regions (this%n_regions))
       do n = 1, this%n_regions
          call sample_region_read_from_marker (this%regions(n), marker, status)
       end do
       call marker%pick_end ("regions", status)
    end if
    call marker%pick_end ("sample_2d_t", status)
  end subroutine sample_2d_read_from_marker
  
  subroutine sample_2d_print_to_unit (this, unit, parents, components, peers)
    class(sample_2d_t), intent(in) :: this
    integer, intent(in) :: unit
    integer(dik), intent(in) :: parents, components, peers
    integer :: n
    write (unit, "(1x,A)")           "components of sample_2d_t"
    write (unit, "(3x,A,I10)")       "n_regions:        ", this%n_regions
    write (unit, "(3x,A,I10)")       "n_alloc:          ", this%n_alloc
    write (unit, "(3x,A,2(E20.10))") "range:            ", this%range
    if (allocated (this%regions)) then
       if (components > 0) then
          write (unit, "(3x,A)")  "regions:"
          do n = 1, this%n_regions
             call this%regions(n)%print_to_unit &
                  (unit, parents, components-1, peers)
          end do
       else
          write (unit, "(3x,A)")  "skipping regions."
       end if
    else
       write (unit, "(3x,A)")  "regions are not allocated."
    end if
  end subroutine sample_2d_print_to_unit
  
  pure subroutine sample_2d_get_type (type)
    character(:), allocatable, intent(out) :: type
    allocate (type, source="sample_2d_t")
  end subroutine sample_2d_get_type
  
  subroutine sample_2d_initialize (this, n_alloc)
    class(sample_2d_t), intent(out) :: this
    integer, intent(in) :: n_alloc
    integer :: n
    if (allocated (this%regions))  deallocate (this%regions)
    allocate (this%regions (n_alloc))
    this%n_alloc = n_alloc
    this%n_regions = 1
    call this%regions(1)%initialize (n_alloc)
    ! do n = 1, n_alloc
    !    call this%regions(n)%initialize (n_alloc)
    ! end do
  end subroutine sample_2d_initialize
  
  pure logical function sample_2d_contains (this, pts2)
    class(sample_2d_t), intent(in) :: this
    real(default), intent(in) :: pts2
    sample_2d_contains = this%range(1) <= pts2 .and. pts2 <= this%range(2)
  end function sample_2d_contains
  
  pure subroutine sample_2d_generate_hit (this, rnd, boost, hit, region)
    class(sample_2d_t), intent(in) :: this
    integer, dimension(3), intent(in) :: rnd
    integer, intent(out) :: region
    integer :: n, sum
    real(default), dimension(2), intent(out) :: hit
    real(default), intent(out) :: boost
    if (0 < this%n_hits .and. this%n_hits < 10) then
       !!! this should be improved
       sum = modulo(rnd(1),this%n_hits) + 1  
       region = 0
       do while (sum > 0)
          region = region + 1
          sum = sum - this%regions(region)%n_hits
       end do
       call this%regions(region)%generate_hit (rnd(2:3), boost, hit)
       boost = boost * this%n_hits / this%regions(region)%n_hits
    else
       if (this%n_regions > 1) then
          !!! this should be improved
          region = modulo(rnd(1), this%n_regions) + 1 
          call this%regions(region)%generate_hit (rnd(2:3), boost, hit)
          boost = boost * this%n_regions
       else
          region = 1
          call this%regions(1)%generate_hit (rnd(2:3), boost, hit)
       end if
    end if
  end subroutine sample_2d_generate_hit

!  pure subroutine sample_2d_generate_hit (this, rnd, boost, hit, region)
!    class(sample_2d_t), intent(in) :: this
!    integer, dimension(3), intent(in) :: rnd
!    integer, intent(out) :: region
!    real(double), dimension(2), intent(out) :: hit
!    real(double), intent(out) :: boost
!    region = modulo(rnd(1), this%n_regions) + 1   !!! this should be improved
!    call this%regions(region)%generate_hit (rnd(2:3), boost, hit)
!    boost = boost * this%n_regions
!  end subroutine sample_2d_generate_hit
    
  subroutine sample_2d_confirm_hit (this, hit, region, full)
    class(sample_2d_t), intent(inout) :: this
    integer, intent(in) :: region
    real(default), dimension(3), intent(in) :: hit
    type(sample_region_t), allocatable :: old_region
    real(default), dimension(2) :: mean, var, diff, cm, cv, c
    integer :: n, n_alloc, dim 
    logical, intent(out) :: full
    this%n_hits = this%n_hits + 1
    if (region <= this%n_alloc) then
       full = .false.
       call this%regions(region)%confirm_hit (hit)
       n_alloc = this%regions(region)%n_alloc
       if (this%regions(region)%is_full()) then
          if (this%is_full()) then
             full = .true.
          else
             this%n_regions = this%n_regions + 1
             allocate (old_region)
             call this%regions(region)%move_components (old_region)
             mean = sum (old_region%hyp_hits(1:2,:), dim=2) / n_alloc
             var = 0
             do n = 1, n_alloc
                var = var + abs(mean-old_region%hyp_hits(1:2,n))
             end do
             var = var / n_alloc
             diff = old_region%corners(1:2,2) - old_region%corners(1:2,1)
             cm = abs ([0.5_default,0.5_default] - &
                  (old_region%corners(1:2,2) - mean) / diff)
             cv = abs(2*([0.25_default,0.25_default] - var / diff))
             c = max(cm,cv)
             if (c(1) < c(2)) then
                dim = 2
             else
                dim = 1
             end if
             call old_region%split (mean(dim), dim, this%n_alloc, &
                  this%regions(region), this%regions(this%n_regions))
          end if
       end if
    else
       write (*,*) "sample_2d_confirm_hit: Region ", region, &
            " not allocated."
    end if
  end subroutine sample_2d_confirm_hit
  
  recursive subroutine sample_2d_split (this, n_alloc, pos, lower, upper)
    class(sample_2d_t), intent(in) :: this
    integer, intent(in) :: n_alloc
    real(default), intent(in) :: pos
    type(sample_2d_t), intent(out) :: lower, upper
    integer :: n_r, n_h
    real(default), dimension(3) :: hit
    ! print *,"sample_2d_split: ", pos, this%range
    call lower%initialize (4*n_alloc)
    call upper%initialize (4*n_alloc)
    do n_r = this%n_regions, 1, -1
       do n_h = 1, this%regions(n_r)%n_hits
          hit = this%regions(n_r)%hyp_hits (1:3,n_h)
          if (hit(3) > pos) then
             call upper%push (hit)
          else
             call lower%push (hit)             
          end if
       end do
    end do
    lower%range = [this%range(1), pos]
    upper%range = [pos, this%range(2)]
  end subroutine sample_2d_split

!  subroutine sample_2d_split (this, n_alloc, pos, lower, upper)
!    class(sample_2d_t), intent(in) :: this
!    integer, intent(in) :: n_alloc
!    real(default), intent(in) :: pos
!    type(sample_2d_t), intent(out) :: lower, upper
!    integer :: n, n_hit
!    real(default), dimension(3) :: hit
!    allocate (lower%regions (n_alloc))
!    allocate (upper%regions (n_alloc))
!    !$OMP PARALLEL DO FIRSTPRIVATE (this, pos, n_alloc) SHARED (lower, upper)
!    do n = 1, this%n_regions
!      call sample_region_split (this%regions(n), pos, 3, n_alloc, &
!         lower%regions(n),upper%regions(n))
!    end do
!    !$OMP END PARALLEL DO
!    lower%n_regions = this%n_regions
!    upper%n_regions = this%n_regions
!    lower%n_alloc = n_alloc
!    upper%n_alloc = n_alloc
!    lower%range = [this%range(1), pos]
!    upper%range = [pos, this%range(2)]
!  end subroutine sample_2d_split
    
  subroutine sample_2d_push (this, hit)
    class(sample_2d_t), intent(inout) :: this
    real(default), dimension(3), intent(in) :: hit
    integer :: region
    logical :: full
    do region = 1, this%n_regions
       if (this%regions(region)%contains (hit)) then
          call this%confirm_hit (hit, region, full)
          ! call this%regions(region)%confirm_hit (hit)
          if (full) print *,"sample_2d_push: region is full now"
          exit
       end if
    end do
    if (region > this%n_regions) & 
         print *, "sample_2d_push: no region contains ", hit
  end subroutine sample_2d_push
  
  subroutine sample_2d_write_hits (this, unit)
    class(sample_2d_t), intent(in) :: this
    integer, intent(in) :: unit
    integer :: n
    do n = 1, this%n_regions
       call this%regions(n)%write_hits (unit)
    end do
  end subroutine sample_2d_write_hits
  
  elemental logical function sample_2d_is_full (this)
    class(sample_2d_t), intent(in) :: this
    sample_2d_is_full = this%n_alloc == this%n_regions
  end function sample_2d_is_full
  
  subroutine sample_2d_move_components (this, that)
    class(sample_2d_t), intent(inout) :: this
    class(sample_2d_t), intent(out) :: that
    that%n_alloc = this%n_alloc
    that%n_regions = this%n_regions
    that%n_hits = this%n_hits
    that%range = this%range
    call move_alloc (this%regions, that%regions)
    this%n_alloc = 0
    this%n_regions = 0
    this%n_hits = 0
    this%range = [zero,zero]
  end subroutine sample_2d_move_components
  
  elemental function sample_2d_thickness (this)
    class(sample_2d_t), intent(in) :: this
    real(default) :: sample_2d_thickness
    sample_2d_thickness = this%range(2) - this%range(1)
  end function sample_2d_thickness
  
  subroutine sample_2d_analyse (this, dir, file)
    class(sample_2d_t), intent(in) :: this
    character(*), intent(in) :: dir, file
    integer :: u
    real(default), dimension(1:2,0:100,0:100) :: grid
    integer, dimension(0:100,0:100) :: i_grid
    integer :: r, x, y
    integer, dimension(2,2) :: i
    call generate_unit (u)
    print *, "sample_2d_analyse: ", dir // "/" // file
    open (u, file=dir//"/"//file)
    do x = 0, 100
       do y = 0, 100
          grid(1:2,x,y) = [-one,-one]
       end do
    end do
    do r = 1, this%n_regions
       i = int(this%regions(r)%corners*1E2_default)
       do x = i(1,1), i(1,2)
          do y = i(2,1), i(2,2)
             i_grid(x,y) = this%regions(r)%n_hits
             grid(1,x,y) = one / this%regions(r)%area ()
             grid(2,x,y) = this%regions(r)%density ()
          end do
       end do
    end do
    do x = 0, 100
       do y = 0, 100
          write (u, *) x, y, i_grid(x,y), grid(1:2,x,y)
       end do
       write (u, *)
    end do
    close (u)
  end subroutine sample_2d_analyse
  
  subroutine sample_2d_to_generator (this)
    class(sample_2d_t), intent(inout) :: this
    integer :: region
    do region = 1, this%n_regions
       call this%regions(region)%to_generator ()
    end do
  end subroutine sample_2d_to_generator
  
  elemental function sample_2d_mean (this, dim) result (mean)
    class(sample_2d_t), intent(in) :: this
    integer, intent(in) :: dim
    real(default) :: mean
    integer :: region, hit
    mean = zero
    do region = 1, this%n_regions
       do hit = 1, this%regions(region)%n_hits
          mean = mean + this%regions(region)%hyp_hits (dim, hit)
       end do
    end do
    mean = mean / this%n_hits
  end function sample_2d_mean
  
  subroutine sample_3d_write_to_marker (this, marker, status)
    class(sample_3d_t), intent(in) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status 
    integer :: n
    call marker%mark_begin ("sample_3d_t")
    call marker%mark ("n_slices", this%n_slices)
    call marker%mark ("n_alloc", this%n_alloc)
    if (this%n_slices > 0) then
       call marker%mark_instance_begin &
            (this%slices(1), "slices", shape=shape(this%slices))
       do n = 1, this%n_slices
          call sample_2d_write_to_marker (this%slices(n), marker, status)
       end do
       call marker%mark_instance_end ()
    end if
    call marker%mark_end ("sample_3d_t")
  end subroutine sample_3d_write_to_marker
  
  subroutine sample_3d_read_from_marker (this, marker, status)
    class(sample_3d_t), intent(out) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status  
    integer :: n
    call marker%pick_begin ("sample_3d_t", status=status)
    call marker%pick ("n_slices", this%n_slices, status)
    call marker%pick ("n_alloc", this%n_alloc, status)
    if (this%n_slices > 0) then
       call marker%pick_instance_begin ("slices", status=status)
       allocate(this%slices (this%n_slices))
       do n = 1, this%n_slices
          call sample_2d_read_from_marker (this%slices(n), marker, status)
       end do
       call marker%pick_instance_end (status)
    end if
    call marker%pick_end ("sample_3d_t", status)
  end subroutine sample_3d_read_from_marker
  
  subroutine sample_3d_print_to_unit (this, unit, parents, components, peers)
    class(sample_3d_t), intent(in) :: this
    integer, intent(in) :: unit
    integer(dik), intent(in) :: parents, components, peers
    integer :: n
    write (unit, "(1x,A)")     "components of sample_3d_t"
    write (unit, "(3x,A,I10)") "n_slices:         ", this%n_slices
    write (unit, "(3x,A,I10)") "n_alloc:          ", this%n_alloc
    if (allocated (this%slices)) then
       if (components > 0) then
          do n = 1, this%n_slices
             call this%slices(n)%print_to_unit (unit, parents, components-1, peers)
          end do
       else
          write (unit, "(3x,A)")  "skipping slices."
       end if
    else
       write (unit, "(3x,A)")  "slices are not allocated."
    end if
  end subroutine sample_3d_print_to_unit
  
  pure subroutine sample_3d_get_type (type)
    character(:), allocatable, intent(out) :: type
    allocate (type, source="sample_3d_t")
  end subroutine sample_3d_get_type
  
  elemental function sample_3d_measure (this)
    real(default) :: sample_3d_measure
    class(sample_3d_t), intent(in) :: this
    sample_3d_measure = one
  end function sample_3d_measure
  
  subroutine sample_3d_to_generator(this)
    class(sample_3d_t), intent(inout)::this
    integer::slice
    do slice=1,this%n_slices
       call this%slices(slice)%to_generator()
    end do
  end subroutine sample_3d_to_generator
  
  subroutine sample_3d_initialize (this, n_alloc)
    class(sample_3d_t), intent(out) :: this
    integer, intent(in) :: n_alloc
    if (allocated (this%slices))  deallocate (this%slices)
    if (n_alloc > 0) then
       allocate (this%slices (n_alloc))
       this%n_alloc = n_alloc
       this%n_slices = 1
       call this%slices(1)%initialize (n_alloc)
    else
       this%n_alloc = 0
    end if
  end subroutine sample_3d_initialize
  
  pure subroutine sample_3d_generate_hit &
       (this, rnd, pts2, boost, hit, region, slice)
    class(sample_3d_t), intent(in) :: this
    integer, intent(in), dimension(3) :: rnd
    real(default), intent(in) :: pts2
    integer, intent(out) :: slice, region
    real(default), dimension(3), intent(out) :: hit
    real(default), intent(out) :: boost
    if (this%n_slices == 0) then
       call muli_mcint_generate_hit (rnd, unit_square, hit(1:2))
       boost = 1._default
       slice = 1
       region = 1
    else
       do slice = 1, this%n_slices
          if (this%slices(slice)%contains (pts2))  exit          
       end do
       call this%slices(slice)%generate_hit (rnd, boost, hit(1:2), region)
    end if
    hit(3) = pts2
  end subroutine sample_3d_generate_hit
  
  subroutine sample_3d_confirm_hit (this, hit, region, slice)
    class(sample_3d_t), intent(inout) :: this
    integer, intent(in) :: slice, region
    real(default), intent(in), dimension(3) :: hit
    type(sample_2d_t), allocatable :: old_slice
    integer :: n
    logical :: full
    if (this%n_alloc < slice) then
       print *, "sample_3d_confirm_hit: Slice ", slice, " not allocated."
    else
    ! if (.not. allocated (this%slices))  call this%initialize (2)
       call this%slices(slice)%confirm_hit (hit, region, full)
       if (full) then
          if (this%n_alloc == this%n_slices)  call this%enlarge ()
          this%n_slices = this%n_slices + 1
          allocate (old_slice)
          call this%slices(slice)%move_components (old_slice)
          call sample_2d_split (old_slice, this%n_alloc, &
               old_slice%mean(3), this%slices(slice), &
               this%slices(this%n_slices))
       end if
    end if
  end subroutine sample_3d_confirm_hit
  
  subroutine sample_3d_enlarge (this)
    class(sample_3d_t), intent(inout) :: this
    type(sample_2d_t), allocatable, dimension(:) :: old_slices
    integer :: n
    print *, "sample_3d_enlarge"
    call move_alloc (this%slices, old_slices)
    this%n_alloc = this%n_alloc * 2
    allocate (this%slices (this%n_alloc))
    do n = 1, size(old_slices)
       call old_slices(n)%move_components(this%slices(n))
    end do
  end subroutine sample_3d_enlarge
  
  subroutine sample_int_kind_write_to_marker (this, marker, status)
    class(sample_int_kind_t), intent(in) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status
    call marker%mark_begin ("sample_int_kind_t")
    call sample_3d_write_to_marker (this, marker, status)
    call marker%mark ("n_hits", this%n_hits)
    call marker%mark ("n_proc", this%n_proc)
    call marker%mark ("boost", this%overall_boost)
    if (this%n_hits > 0) then
       call marker%mark ("hits", this%hits)
    end if
    if (this%n_proc > 0) then
       call marker%mark ("processes", this%processes)
       call marker%mark ("weights", this%weights)
    end if
    call marker%mark_end ("sample_int_kind_t")
  end subroutine sample_int_kind_write_to_marker
  
  subroutine sample_int_kind_read_from_marker (this, marker, status)
    class(sample_int_kind_t), intent(out) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status  
    call marker%pick_begin ("sample_int_kind_t", status=status)
    call sample_3d_read_from_marker (this, marker, status)
    call marker%pick ("n_hits", this%n_hits, status)
    call marker%pick ("n_proc", this%n_proc, status)
    call marker%pick ("boost", this%overall_boost, status)
    if (this%n_hits > 0) then
       allocate (this%hits (this%n_hits))
       call marker%pick ("hits", this%hits, status)
    end if
    if (this%n_proc > 0) then
       allocate (this%processes (this%n_proc))
       call marker%pick ("processes", this%processes, status)
       allocate (this%weights (this%n_proc))
       call marker%pick ("weights", this%weights, status)
    end if
    call marker%pick_end ("sample_int_kind_t", status)
  end subroutine sample_int_kind_read_from_marker
  

  subroutine sample_int_kind_print_to_unit (this, unit, parents, components, peers)
    class(sample_int_kind_t), intent(in) :: this
    integer, intent(in) :: unit
    integer(dik), intent(in) :: parents, components, peers
    integer :: n
    if (parents > 0)  call sample_3d_print_to_unit &
         (this, unit, parents, components, peers)
    write (unit, "(1x,A)")       "components of sample_int_kind_t"
    write (unit, "(3x,A,I10)")   "n_hits:          ", this%n_hits
    write (unit, "(3x,A,I10)")   "n_proc:          ", this%n_proc
    write (unit, "(3x,A,E14.7)") "overall_boost:   ", this%overall_boost
    write (unit, "(3x,A)")       "hits:"
    write (unit, "(3x,10(I0,1x))")  this%hits(1:this%n_hits)
    write (unit, "(3x,A)")       "weights:"
    write (unit, "(3x,10(I0,1x))")  this%weights
    write (unit, "(3x,A)")  "processes:"
    write (unit, "(3x,2(I0,1x))") this%processes
  end subroutine sample_int_kind_print_to_unit
  
  pure subroutine sample_int_kind_get_type (type)
    character(:),allocatable, intent(out) :: type
    allocate (type, source="sample_int_kind_t")
  end subroutine sample_int_kind_get_type
  
  subroutine sample_int_kind_to_generator(this)
    class(sample_int_kind_t), intent(inout)::this
    integer::int_kind
    if (allocated(this%hits))deallocate(this%hits)
    call sample_3d_to_generator(this)
  end subroutine sample_int_kind_to_generator

  elemental integer function sample_int_kind_process_id (this, subprocess)
    class(sample_int_kind_t), intent(in) :: this
    integer, intent(in) :: subprocess
    sample_int_kind_process_id = this%processes(subprocess)
  end function sample_int_kind_process_id
  
  subroutine sample_int_kind_initialize (this, n_alloc, processes, overall_boost)
    class(sample_int_kind_t), intent(out) :: this
    integer, intent(in) :: n_alloc
    integer, intent(in), dimension(:) :: processes
    real(default), optional, intent(in) :: overall_boost
    integer :: s, n
    s = size(processes)
    call sample_3d_initialize (this, n_alloc)
    if (allocated (this%hits))  deallocate (this%hits)
    allocate (this%hits (n_alloc))
    if (allocated (this%weights))  deallocate (this%weights)
    allocate (this%weights(s))
    if (allocated (this%processes))  deallocate (this%processes)
    allocate (this%processes(s), source=processes)
    do n = 1, s
       this%weights(n) = 0
    end do
    this%n_alloc = n_alloc
    this%n_hits = 0
    this%n_proc = s
    if (present (overall_boost))  this%overall_boost = overall_boost
    this%overall_boost = this%overall_boost * this%n_proc
    ! print *, this%weights
  end subroutine sample_int_kind_initialize
  
  pure subroutine sample_int_kind_generate_hit &
       (this, rnd, pts2, boost, hit, region, slice, subprocess)
    class(sample_int_kind_t), intent(in) :: this
    integer, dimension(4), intent(in) :: rnd
    real(default), intent(in) :: pts2
    real(default), dimension(3), intent(out) :: hit
    integer, intent(out) :: region, slice, subprocess
    real(default), intent(out) :: boost
    integer :: n_n
    ! print *, rnd, pts2, boost, hit, region, slice, subprocess
    call sample_3d_generate_hit &
         (this, rnd(2:4), pts2, boost, hit, region, slice)
    n_n = modulo(rnd(1), this%n_hits + size(this%weights)) + 1
    if (n_n > this%n_hits) then
       subprocess = n_n - this%n_hits
    else
       subprocess = this%hits(n_n)
    end if
    boost = boost * this%overall_boost * (this%n_proc + this%n_hits) / &
         (this%n_proc * (this%weights(subprocess) + 1))
  end subroutine sample_int_kind_generate_hit
  
  subroutine sample_int_kind_mcgenerate_hit (this, pts2, mean, &
       integrand_kind, tao_rnd, process_id, cart_hit)
    class(sample_int_kind_t), intent(inout) :: this
    integer, intent(in) :: integrand_kind
    real(default), intent(in) :: pts2, mean
    type(tao_random_state), intent(inout) :: tao_rnd
    real(default), dimension(3), intent(out) :: cart_hit
    integer, intent(out) :: process_id
    real(default) :: boost
    integer :: region, slice, subprocess
    integer, dimension(4) :: i_rnd
    real(default) :: dddsigma, d_rnd
    real(default), dimension(3) :: hyp_hit
    MC: do 
       this%n_tries = this%n_tries + 1
       call tao_random_number (tao_rnd, i_rnd)
       call tao_random_number (tao_rnd, d_rnd)
       ! print *, pts2, mean, integrand_kind, process_id, cart_hit
       call this%generate_hit (i_rnd, pts2, boost, hyp_hit, region, &
            slice, subprocess)
       process_id = this%process_id(subprocess)
       call interactions_dddsigma_reg (process_id, integrand_kind, &
            hyp_hit, cart_hit, dddsigma)
       dddsigma = dddsigma * boost
       if (d_rnd*mean < dddsigma) then
          exit MC
       end if
    end do MC
    if (mean < dddsigma) then
       call this%confirm_hit (hyp_hit, region, slice, subprocess, .true.)
    else
       call this%confirm_hit (hyp_hit, region, slice, subprocess, .false.)
    end if
  end subroutine sample_int_kind_mcgenerate_hit
  
  subroutine sample_int_kind_confirm_hit &
       (this, hit, region, slice, subprocess, over)
    class(sample_int_kind_t), intent(inout) :: this
    real(default), dimension(3), intent(in) :: hit
    integer, intent(in) :: region, slice, subprocess
    integer, dimension(:), allocatable :: tmp_hits
    logical, optional, intent(in) :: over
    this%n_hits = this%n_hits + 1
    if (present(over)) then
       if (over) then
          this%n_over = this%n_over + 1
          this%overall_boost = this%overall_boost / 1.1_default
       else
          this%overall_boost = this%overall_boost * 1.0001_default
       end if
    end if
    if (0 < size(this%hits)) then
       if (this%n_hits > size(this%hits)) then
          call move_alloc (this%hits, tmp_hits)
          allocate (this%hits (2*size(tmp_hits)))
          this%hits (1:size(tmp_hits)) = tmp_hits
       end if
       this%hits(this%n_hits) = subprocess
    end if
    this%weights(subprocess) = this%weights(subprocess) + 1
    call sample_3d_confirm_hit (this, hit, region, slice)
  end subroutine sample_int_kind_confirm_hit
  
  subroutine sample_int_kind_analyse (this, dir, prefix)
    class(sample_int_kind_t), intent(in) :: this
    character(*), intent(in) :: dir, prefix
    integer :: slices_unit, subprocs_unit
    integer :: n, slice
    character(3) :: slice_name
    integer, dimension(:), allocatable :: int_a
    real(default), dimension(:), allocatable :: real_a
    call generate_unit (slices_unit)
    print *, "sample_int_kind_analyse: ", dir // "/" // prefix // &
         "slice_distribution.plot"
    open (slices_unit, file=dir // "/" // prefix // "slice_distribution.plot")
    call generate_unit (subprocs_unit)
    print *, "sample_int_kind_analyse: ", dir // "/" // prefix // &
         "subproc_distribution.plot"
    open (subprocs_unit, file=dir // "/" // prefix // &
         "subproc_distribution.plot")
    allocate (real_a (this%n_slices))
    allocate (int_a (this%n_slices))
    do n = 1, this%n_slices
       real_a(n) = this%slices(n)%range(1)
    end do
    call misc_sort (real_a, int_a)
    do n = 1, size (this%weights)
       if (this%n_hits > 0) then
          write (subprocs_unit, fmt=*) real(this%weights(n)), &
               real(this%weights(n)+1) / this%n_hits
       else
          write (subprocs_unit, fmt=*) 0, 0
       end if
    end do
    do n = 1, this%n_slices       
       slice = int_a (n)
       call integer_with_leading_zeros (n, 3, slice_name)
       call sample_2d_analyse (this%slices(slice), dir, prefix // &
            slice_name // ".plot")  
       print *, this%n_hits, this%slices(slice)%range(2) - &
            this%slices(slice)%range(1)
       if (this%n_hits > 0) then
          write (slices_unit, *)  this%slices(slice)%range(1), &
               this%slices(slice)%range(2), this%slices(slice)%n_hits, &
               real (this%slices(slice)%n_hits) / (this%n_hits * &
               (this%slices(slice)%range(2) - this%slices(slice)%range(1)))
       else
          write (slices_unit, *)  this%slices(slice)%range(1), &
               this%slices(slice)%range(2), this%slices(slice)%n_hits, zero
       end if
    end do
    write (slices_unit, *) one, zero, zero, zero
    close (slices_unit)
    close (subprocs_unit)
  end subroutine sample_int_kind_analyse
  
  subroutine sample_inclusive_write_to_marker (this, marker, status)
    class(sample_inclusive_t), intent(in) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status
    integer :: n
    call marker%mark_begin ("sample_inclusive_t")
    call marker%mark ("n_alloc", this%n_alloc)
    if (allocated (this%int_kinds)) then
       call marker%mark_begin (tag="int_kinds", shape=shape(this%int_kinds))
       do n = 1, size(this%int_kinds)
          call this%int_kinds(n)%write_to_marker (marker, status)
       end do
       call marker%mark_instance_end ()
    else
       call marker%mark_empty (tag="int_kinds", shape=[0])
    end if
    call marker%mark_end ("sample_inclusive_t")
  end subroutine sample_inclusive_write_to_marker
  
  subroutine sample_inclusive_read_from_marker (this, marker, status)
    class(sample_inclusive_t), intent(out) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status
    integer :: n
    integer, dimension(:), allocatable :: s
    call marker%pick_begin ("sample_inclusive_t", status=status)
    call marker%pick ("n_alloc", this%n_alloc, status)
    call marker%pick_begin ("int_kinds", shape=s, status=status)
    if (s(1) > 0) then
       do n = 1, size(this%int_kinds)
          call this%int_kinds(n)%read_from_marker (marker, status)
       end do
       call marker%pick_end ("int_kinds",status)
    end if
    call marker%pick_end ("sample_inclusive_t", status)
  end subroutine sample_inclusive_read_from_marker
  
  subroutine sample_inclusive_print_to_unit (this, unit, parents, components, peers)
    class(sample_inclusive_t), intent(in) :: this
    integer, intent(in) :: unit
    integer(dik), intent(in) :: parents, components, peers
    integer :: n
    write (unit, "(1x,A)")  "components of sample_inclusive_t"
    write (unit, "(3x,A,I10)")  "n_alloc:          ", this%n_alloc
    if (allocated (this%int_kinds)) then
       if (components > 0) then
          write (unit, "(3x,A)")  "int_kinds:"
          do n = 1, this%n_alloc
             call this%int_kinds(n)%print_to_unit &
                  (unit, parents, components-1, peers)
          end do
       else
          write (unit, "(3x,A)")  "skipping int_kinds."
       end if
    else
       write (unit, "(3x,A)")  "int_kinds are not allocated."
    end if
  end subroutine sample_inclusive_print_to_unit
  
  pure subroutine sample_inclusive_get_type (type)
    character(:), allocatable, intent(out) :: type
    allocate (type, source = "sample_inclusive_t")
  end subroutine sample_inclusive_get_type
  
  elemental integer function sample_inclusive_process_id &
       (this, subprocess, int_kind)
    class(sample_inclusive_t), intent(in) :: this
    integer, intent(in) :: subprocess, int_kind
    sample_inclusive_process_id = &
         this%int_kinds(int_kind)%processes (subprocess)
  end function sample_inclusive_process_id
  
  subroutine sample_inclusive_initialize &
       (this, n_alloc, sizes, processes, overall_boost)
    class(sample_inclusive_t), intent(out) :: this
    integer, intent(in) :: n_alloc
    integer, dimension(:), intent(in) :: sizes, processes
    real(default), optional, intent(in) :: overall_boost
    integer :: n, sum
    this%n_tries_sum = i_zero
    this%n_over_sum = 0
    this%n_alloc = size(sizes)
    if (allocated(this%int_kinds))  deallocate (this%int_kinds)
    allocate (this%int_kinds (this%n_alloc))
    sum = 0
    do n = 1, this%n_alloc
       call this%int_kinds(n)%initialize (n_alloc, &
            processes(sum+1:sum+sizes(n)), overall_boost)
       sum = sum + sizes(n)
    end do
  end subroutine sample_inclusive_initialize
  
  subroutine sample_inclusive_finalize (this)
    class(sample_inclusive_t), intent(inout) :: this
    deallocate (this%int_kinds)
    this%n_alloc = 0
  end subroutine sample_inclusive_finalize
  
  pure subroutine sample_inclusive_generate_hit &
       (this, rnd, pts2, int_kind, hit, region, boost, slice, process)
    class(sample_inclusive_t), intent(in) :: this
    integer, dimension(4), intent(in) :: rnd
    real(default), intent(in) :: pts2
    integer, intent(in) :: int_kind
    real(default), dimension(3), intent(out) :: hit
    integer, intent(out) :: region, slice, process
    real(default), intent(out) :: boost
    call this%int_kinds(int_kind)%generate_hit &
         (rnd, pts2, boost, hit, region, slice, process)
  end subroutine sample_inclusive_generate_hit
  
  subroutine sample_inclusive_mcgenerate_hit &
       (this, pts2, mean, integrand_kind, tao_rnd, process_id, cart_hit)
    class(sample_inclusive_t), intent(inout) :: this
    real(default), intent(in) :: pts2, mean
    integer, intent(in) :: integrand_kind
    type(tao_random_state), intent(inout) :: tao_rnd
    real(default), dimension(3), intent(out) :: cart_hit
    integer, intent(out) :: process_id
    ! print *, "sample_inclusive_mcgenerate_hit &
    !   (this,",pts2,mean,integrand_kind,process_id,cart_hit,")"
    ! print *, allocated (this%int_kinds)
    call sample_int_kind_mcgenerate_hit (this%int_kinds(integrand_kind), &
         pts2, mean, integrand_kind, tao_rnd, process_id, cart_hit)
  end subroutine sample_inclusive_mcgenerate_hit
  
  subroutine sample_inclusive_confirm_hit &
       (this, hit, int_kind, region, slice, process, over)
    class(sample_inclusive_t), intent(inout) :: this
    real(default), dimension(3), intent(in) :: hit
    integer, intent(in) :: int_kind, region, slice, process
    logical, optional, intent(in) :: over
    call this%int_kinds(int_kind)%confirm_hit &
         (hit, region, slice, process, over)
  end subroutine sample_inclusive_confirm_hit
  
  subroutine sample_inclusive_sum_up (this)
    class(sample_inclusive_t), intent(inout) :: this
    integer :: n
    this%n_tries_sum = i_zero
    this%n_hits_sum = i_zero
    this%n_over_sum = i_zero
    do n = 1, this%n_alloc
       this%n_tries_sum = this%n_tries_sum+this%int_kinds(n)%n_tries
       this%n_hits_sum = this%n_hits_sum+this%int_kinds(n)%n_hits
       this%n_over_sum = this%n_over_sum+this%int_kinds(n)%n_over
    end do
  end subroutine sample_inclusive_sum_up
  
  subroutine sample_inclusive_analyse (this, dir, subdirs)
    class(sample_inclusive_t), intent(in) :: this
    character(*), intent(in) :: dir
    logical, intent(in) :: subdirs
    integer :: inclusive_unit    
    integer :: n, n_hits
    character(2) :: sample_name
    call generate_unit (inclusive_unit)
    open (inclusive_unit, file = dir // "/int_kinds.plot")
    n_hits = 0
    do n = 1, size(this%int_kinds)
       n_hits = n_hits + this%int_kinds(n)%n_hits
    end do
    do n = 1, size(this%int_kinds)
       write (inclusive_unit, *)  n, real(this%int_kinds(n)%n_hits) / n_hits
       call integer_with_leading_zeros (n, 2, sample_name)
       if (subdirs) then
          call sample_int_kind_analyse (this%int_kinds(n), &
               dir // "/" // sample_name, "")
       else
          call sample_int_kind_analyse (this%int_kinds(n), &
               dir, sample_name // "_")
       end if
    end do
    close (inclusive_unit)
  end subroutine sample_inclusive_analyse
  
  subroutine sample_inclusive_to_generator (this)
    class(sample_inclusive_t), intent(inout) :: this
    integer :: int_kind
    do int_kind = 1, size(this%int_kinds)
       call this%int_kinds(int_kind)%to_generator ()
    end do
  end subroutine sample_inclusive_to_generator
  
  subroutine sample_inclusive_allocate (this, n_alloc)
    class(sample_inclusive_t), intent(out) :: this
    integer, intent(in) :: n_alloc
    allocate (this%int_kinds (n_alloc))
    this%n_alloc = n_alloc
  end subroutine sample_inclusive_allocate
  
  pure subroutine muli_mcint_generate_hit (rnd, corners, hit)
    real(default), dimension(2), intent(out) :: hit
    integer, intent(in), dimension(2) :: rnd
    real(default), dimension(2,2), intent(in) :: corners
    ! print *, hit
    ! print *, corners
    ! print *, (corners(1:2,2) - corners(1:2,1))
    hit = (rnd / max_d) * (corners(1:2,2) - corners(1:2,1)) + corners(1:2,1)
  end subroutine muli_mcint_generate_hit
  
  subroutine plot_pstvue3d (unit, corners, density)
    integer, intent(in) :: unit
    real(default), dimension(2,2), intent(in) :: corners
    real(default), intent(in) :: density
    real(default), dimension(2) :: width, mean
    real(default), dimension(3,3) :: plot
    width = (corners(:,2) - corners(:,1)) / two
    mean = (corners(:,1) + corners(:,2)) / two
    plot(1,1) = width(1)
    plot(2,1) = width(2)
    plot(3,1) = density / two
    plot(1,2) = mean(1)
    plot(2,2) = mean(2)
    plot(3,2) = density / two
    call log_color_code (density, plot(1:3,3))
    if (density > one) then                      
       write (unit, fmt='("\mybigcube{",F14.7,"}{",F14.7,"}{",F14.7,"} &
            & {",F14.7,"}{",F14.7,"}{",F14.7,"}{",F14.7,"}{",F14.7,"} &
            & {",F14.7,"}")') plot
       return
    end if
    write (unit, fmt='("\mycube{",F14.7,"}{",F14.7,"}{",F14.7,"} &
         & {",F14.7,"}{",F14.7,"}{",F14.7,"}{",F14.7,"}{",F14.7,"} &
         & {",F14.7,"}")') plot
  end subroutine plot_pstvue3d

  subroutine log_color_code (number, rgb)
    real(default), intent(in) :: number
    real(default), dimension(3), intent(out) :: rgb
    if (number < exp(-five)) then
       rgb = [zero, zero, exp(five)*number]
    else
       if (number < exp(-four)) then
          rgb = [zero, (number-exp(-five))/(exp(-four)-exp(-five)), one]
       else
          if (number < exp(-three)) then
             rgb = [zero, one, one-((number-exp(-four))/(exp(-three)-exp(-four)))]
          else
             if (number < exp(-two)) then
                rgb = [(number-exp(-three))/(exp(-two)-exp(-three)), one, zero]
             else
                if (number < exp(-one)) then                   
                   rgb = [one, one-(number-exp(-two))/(exp(-one)-exp(-two)), zero]
                else
                   if (number < one) then
                      rgb = [one, zero, (number-exp(-three))/(one-exp(-three))]
                   else                      
                      rgb = [exp(one), one, one] * exp(-number)
                      return
                   end if
                end if
             end if
          end if
       end if
    end if
  end subroutine log_color_code

  recursive subroutine misc_sort (in, out)
    real(default), dimension(:), intent(in) :: in
    integer, dimension(:), intent(out) :: out
    integer, dimension(:), allocatable :: tmp
    integer :: n, k, l, cut
    if (size(in) == 1) then
       out = [1]
    else
       if (size(in) == 2) then
          if (in(1) <= in(2)) then
             out = [1,2]
          else
             out = [2,1]
          end if
       else
          cut = size(in) / 2
          k = 1
          l = cut + 1
          allocate (tmp (size(in)))
          call misc_sort (in(1:cut), tmp(1:cut))
          call misc_sort (in(cut+1:), tmp(cut+1:))
          do n = cut + 1, size(in)
             tmp(n) = tmp(n) + cut
          end do
          do n = 1, size(in)
             if (k > cut) then
                out(n) = tmp(l)
                l = l + 1
             else
                if (l > size(tmp)) then
                   out(n) = tmp(k)
                   k = k + 1
                else
                   if (in(tmp(k)) < in(tmp(l))) then
                      out(n) = tmp(k)
                      k = k + 1
                   else
                      out(n) = tmp(l)
                      l = l + 1
                   end if
                end if
             end if
          end do
       end if
    end if
  end subroutine misc_sort
  

end module muli_mcint

