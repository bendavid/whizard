! WHIZARD 2.5.0 May 06 2017
!
! Copyright (C) 1999-2017 by
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

module muli
  use, intrinsic :: iso_fortran_env
  use kinds, only: default
  use constants
  use tao_random_numbers !NODEP!
  use muli_base
  use muli_momentum
  use muli_trapezium
  use muli_interactions
  use muli_dsigma
  use muli_mcint
  use muli_remnant

  implicit none
  private

  logical, parameter :: muli_default_modify_pdfs = .true.
  integer, parameter :: muli_default_lhapdf_member = 0
  character(*), parameter :: muli_default_lhapdf_file = "cteq6ll.LHpdf"


  public :: muli_t

  type, extends(qcd_2_2_class) :: qcd_2_2_t
     private
     integer :: process_id = -1
     integer :: integrand_id = -1
     integer, dimension(2) :: parton_ids = [0,0]
     integer, dimension(4) :: flow = [0,0,0,0]
     real(default), dimension(3) :: momentum_fractions = [-one, -one, -one]
     real(default), dimension(3) :: hyperbolic_fractions = [-one ,- one,- one]
   contains
     procedure :: write_to_marker => qcd_2_2_write_to_marker
     procedure :: read_from_marker => qcd_2_2_read_from_marker
     procedure :: print_to_unit => qcd_2_2_print_to_unit
     procedure, nopass :: get_type => qcd_2_2_get_type
     procedure :: get_process_id => qcd_2_2_get_process_id
     procedure :: get_integrand_id => qcd_2_2_get_integrand_id
     procedure :: get_diagram_kind => qcd_2_2_get_diagram_kind
     procedure :: get_diagram_color_kind => qcd_2_2_get_diagram_color_kind
     procedure :: get_io_kind => qcd_2_2_get_io_kind
     procedure :: get_lha_flavors => qcd_2_2_get_lha_flavors
     procedure :: get_pdg_flavors => qcd_2_2_get_pdg_flavors
     procedure :: get_parton_id => qcd_2_2_get_parton_id
     procedure :: get_parton_kinds => qcd_2_2_get_parton_kinds
     procedure :: get_pdf_int_kinds => qcd_2_2_get_pdf_int_kinds
     procedure :: get_momentum_boost => qcd_2_2_get_momentum_boost
     procedure :: get_hyperbolic_fractions => qcd_2_2_get_hyperbolic_fractions
     procedure :: get_remnant_momentum_fractions => &
          qcd_2_2_get_remnant_momentum_fractions
     procedure :: get_total_momentum_fractions => &
          qcd_2_2_get_total_momentum_fractions
     procedure :: get_color_flow => qcd_2_2_get_color_flow
     procedure :: get_color_correlations => qcd_2_2_get_color_correlations
     generic :: initialize => qcd_2_2_initialize
     procedure :: qcd_2_2_initialize
  end type qcd_2_2_t

  type, extends(qcd_2_2_t) :: muli_t
     real(default) :: GeV2_scale_cutoff
     logical :: modify_pdfs = muli_default_modify_pdfs
     !!! Pt chain status
     logical :: finished = .false.
     logical :: exceeded = .false.
     !!! Timers
     real(default) :: init_time = zero
     real(default) :: pt_time = zero
     real(default) :: partons_time = zero
     real(default) :: confirm_time = zero
     !!! Generator internals
     logical :: initialized = .false.
     logical :: initial_interaction_given = .false.
     real(default) :: mean = one
     real(default), dimension(0:16) :: start_integrals = &
          [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
     type(tao_random_state) :: tao_rnd
     type(muli_trapezium_tree_t) :: dsigma
     type(sample_inclusive_t) :: samples
     type(pp_remnant_t) :: beam
     !!! These pointers shall not be allocated, deallocated,
     !!!  serialized or deserialized explicitly.
     class(muli_trapezium_node_class_t), pointer :: node => null()
   contains
     procedure :: write_to_marker => muli_write_to_marker
     procedure :: read_from_marker => muli_read_from_marker
     procedure :: print_to_unit => muli_print_to_unit
     procedure, nopass :: get_type => muli_get_type
     generic :: initialize => muli_initialize
     procedure :: muli_initialize
     procedure :: apply_initial_interaction => muli_apply_initial_interaction
     procedure :: finalize => muli_finalize
     procedure :: stop_trainer => muli_stop_trainer
     procedure :: reset_timer => muli_reset_timer
     procedure :: restart => muli_restart
     procedure :: is_initialized => muli_is_initialized
     procedure :: is_initial_interaction_given => &
          muli_is_initial_interaction_given
     procedure :: is_finished => muli_is_finished
     procedure :: enable_remnant_pdf => muli_enable_remnant_pdf
     procedure :: disable_remnant_pdf => muli_disable_remnant_pdf
     procedure :: generate_gev2_pt2 => muli_generate_gev2_pt2
     procedure :: generate_partons => muli_generate_partons
     procedure :: generate_flow => muli_generate_flow
     procedure :: replace_parton => muli_replace_parton
     procedure :: get_parton_pdf => muli_get_parton_pdf
     procedure :: get_momentum_pdf => muli_get_momentum_pdf
     procedure :: print_timer => muli_print_timer
     procedure :: generate_samples => muli_generate_samples
     procedure :: fake_interaction => muli_fake_interaction
     procedure :: generate_next_scale => muli_generate_next_scale
     procedure :: confirm => muli_confirm
  end type muli_t


contains

  subroutine qcd_2_2_write_to_marker (this, marker, status)
    class(qcd_2_2_t), intent(in) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status
    call marker%mark_begin ("qcd_2_2_t")
    call this%mom_write_to_marker (marker, status)
    call marker%mark ("process_id", this%process_id)
    call marker%mark ("integrand_id", this%integrand_id)
    call marker%mark ("momentum_fractions", this%momentum_fractions)
    call marker%mark ("hyperbolic_fractions", this%hyperbolic_fractions)
    call marker%mark_end("qcd_2_2_t")
  end subroutine qcd_2_2_write_to_marker

  subroutine qcd_2_2_read_from_marker (this, marker, status)
    class(qcd_2_2_t), intent(out) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status
    call marker%pick_begin ("qcd_2_2_t", status=status)
    call this%mom_read_from_marker (marker, status)
    call marker%pick ("process_id", this%process_id, status)
    call marker%pick ("integrand_id", this%integrand_id, status)
    call marker%pick ("momentum_fractions", this%momentum_fractions, status)
    call marker%pick &
         ("hyperbolic_fractions", this%hyperbolic_fractions, status)
    call marker%pick_end ("qcd_2_2_t", status=status)
  end subroutine qcd_2_2_read_from_marker

  subroutine qcd_2_2_print_to_unit (this, unit, parents, components, peers)
    class(qcd_2_2_t), intent(in) :: this
    integer, intent(in) :: unit
    integer(dik), intent(in) :: parents, components, peers
    integer, dimension(2,4) :: flow
    integer :: index
    if (parents > i_zero) &
         call this%mom_print_to_unit (unit, parents-1, components, peers)
    write (unit, "(1x,A)")  "Components of qcd_2_2_t:"
    write (unit, "(3x,A,I3)")  "Process id is:       ", this%get_process_id ()
    write (unit, "(3x,A,I3)")  "Integrand id is:     ", this%get_integrand_id ()
    if (this%get_integrand_id () > 0) then
       write (unit, "(3x,A,4(I3))")  "LHA Flavors are:     ", &
            this%get_lha_flavors ()
       write (unit, "(3x,A,4(I3))")  "PDG Flavors are:     ", &
            this%get_pdg_flavors ()
       write (unit, "(3x,A,2(I3))")  "Parton kinds are:    ", &
            this%get_parton_kinds ()
       write (unit, "(3x,A,2(I3))")  "PDF int kinds are:   ", &
            this%get_pdf_int_kinds ()
       write (unit, "(3x,A,2(I3))")  "Diagram kind is:     ", &
            this%get_diagram_kind ()
    end if
    call this%get_color_correlations (1, index, flow)
    write (unit, "(3x,A,4(I0))")  "Color Permutations:  ", this%flow
    write (unit, "(3x,A)")  "Color Connections:"
    write (unit, &
         '("   (",I0,",",I0,")+(",I0,",",I0,")->(",I0,",",I0,&
         &")+(",I0,",",I0,")")') flow
    write (unit, "(3x,A,E14.7)")  "Evolution scale is:  ", &
         this%get_unit2_scale ()
    write (unit, "(3x,A,E14.7)")  "Momentum boost is:   ", &
         this%get_momentum_boost ()
    write (unit, "(3x,A,2(E14.7))")  "Remant momentum fractions are: ", &
         this%get_remnant_momentum_fractions ()
    write (unit, "(3x,A,2(E14.7))")  "Total momentum fractions are:  ", &
         this%get_total_momentum_fractions ()
  end subroutine qcd_2_2_print_to_unit

  pure subroutine qcd_2_2_get_type (type)
    character(:), allocatable, intent(out) :: type
    allocate (type, source="qcd_2_2_t")
  end subroutine qcd_2_2_get_type

  elemental function qcd_2_2_get_process_id (this) result (id)
    class(qcd_2_2_t), intent(in) :: this
    integer :: id
    id = this%process_id
  end function qcd_2_2_get_process_id

  elemental function qcd_2_2_get_integrand_id (this) result (id)
    class(qcd_2_2_t), intent(in) :: this
    integer :: id
    id = this%integrand_id
  end function qcd_2_2_get_integrand_id

  elemental function qcd_2_2_get_diagram_kind (this) result (kind)
    class(qcd_2_2_t), intent(in) :: this
    integer :: kind
    kind = valid_processes (6, this%process_id)
  end function qcd_2_2_get_diagram_kind

  pure function qcd_2_2_get_diagram_color_kind (this) result (kind)
    class(qcd_2_2_t), intent(in) :: this
    integer :: kind
    kind = valid_processes (6, this%process_id)
    if (kind == 1) then
       if (product (valid_processes (1:2,this%process_id)) > 0) then
          kind = 0
       end if
    end if
  end function qcd_2_2_get_diagram_color_kind

  elemental function qcd_2_2_get_io_kind (this) result (kind)
    class(qcd_2_2_t), intent(in) :: this
    integer :: kind
    kind = valid_processes (5, this%process_id)
  end function qcd_2_2_get_io_kind

  pure function qcd_2_2_get_lha_flavors (this) result (lha)
    class(qcd_2_2_t), intent(in) :: this
    integer, dimension(4) :: lha
    lha = valid_processes (1:4, this%process_id)
  end function qcd_2_2_get_lha_flavors

  pure function qcd_2_2_get_pdg_flavors (this) result (pdg)
    class(qcd_2_2_t), intent(in) :: this
    integer, dimension(4) :: pdg
    pdg = this%get_lha_flavors ()
    where (pdg == 0) pdg = 21
  end function qcd_2_2_get_pdg_flavors

  pure function qcd_2_2_get_parton_id (this, n) result (id)
    class(qcd_2_2_t), intent(in) :: this
    integer, intent(in) :: n
    integer :: id
    id = this%parton_ids (n)
  end function qcd_2_2_get_parton_id

  pure function qcd_2_2_get_parton_kinds (this) result (kinds)
    class(qcd_2_2_t), intent(in) :: this
    integer, dimension(2) :: kinds
    kinds = this%get_pdf_int_kinds ()
    kinds(1) = parton_kind_of_int_kind (kinds(1))
    kinds(2) = parton_kind_of_int_kind (kinds(2))
  end function qcd_2_2_get_parton_kinds

  pure function qcd_2_2_get_pdf_int_kinds (this) result (kinds)
    class(qcd_2_2_t), intent(in) :: this
    integer, dimension(2) :: kinds
    kinds = double_pdf_kinds (1:2, this%integrand_id)
  end function qcd_2_2_get_pdf_int_kinds

  elemental function qcd_2_2_get_momentum_boost (this) result (boost)
    class(qcd_2_2_t), intent(in) :: this
    real(default) :: boost
    boost = - one
    ! print('("qcd_2_2_get_momentum_boost: not yet implemented.")')
    ! boost = this%momentum_boost
  end function qcd_2_2_get_momentum_boost

  pure function qcd_2_2_get_hyperbolic_fractions (this) result (fractions)
    class(qcd_2_2_t), intent(in) :: this
    real(double), dimension(3) :: fractions
    fractions = this%hyperbolic_fractions
  end function qcd_2_2_get_hyperbolic_fractions

  pure function qcd_2_2_get_remnant_momentum_fractions &
       (this) result (fractions)
    class(qcd_2_2_t), intent(in) :: this
    real(default), dimension(2) :: fractions
    fractions = this%momentum_fractions(1:2)
  end function qcd_2_2_get_remnant_momentum_fractions

  pure function qcd_2_2_get_total_momentum_fractions &
       (this) result (fractions)
    class(qcd_2_2_t), intent(in) :: this
    real(default), dimension(2) :: fractions
    fractions = [-one, -one]
    ! fractions = this%hyperbolic_fractions(1:2) * &
    !    this%beam%get_proton_remnant_momentum_fractions()
  end function qcd_2_2_get_total_momentum_fractions

  pure function qcd_2_2_get_color_flow (this) result (flow)
    class(qcd_2_2_t), intent(in) :: this
    integer, dimension(4) :: flow
    flow = this%flow
  end function qcd_2_2_get_color_flow

  subroutine qcd_2_2_get_color_correlations &
       (this, start_index, final_index, flow)
    class(qcd_2_2_t), intent(in) :: this
    integer, intent(in) :: start_index
    integer, intent(out) :: final_index
    integer, dimension(2,4), intent(out) :: flow
    integer :: pos, f_end, f_beginning
    final_index = start_index
    !!! We set all flows to i_zero. i_zero means no connection.
    flow = reshape([0,0,0,0,0,0,0,0],[2,4])
    !!! look at all four possible ends of color lines
    do f_end = 1, 4
       !!! The beginning of of this potential line is stored in flow.
       !!! i_zero means no line.
       f_beginning = this%flow(f_end)
       !!! Is there a line beginning at f_beginning and ending at f_end?
       if (f_beginning > 0) then
          !!! yes it is. we get a new number for this new line
          final_index = final_index + 1
          !!! Is this line beginning in the initial state?
          if (f_beginning < 3) then
             !!! Yes it is. lets connect the color entry of f_begin.
             flow(1,f_beginning) = final_index
          else
             !!! No, it's the final state.
             !!! lets connect the anticolor entry of f_begin.
             flow(2,f_beginning) = final_index
          end if
          !!! Is this line ending in the final state?
          if (f_end > 2) then
             !!! Yes it is. lets connect the color entry of f_end.
             flow(1,f_end) = final_index
          else
             !!! No, it's the initial state.
             !!! Lets connect the anticolor entry of f_end.
             flow(2,f_end) = final_index
          end if
       end if
    end do
  end subroutine qcd_2_2_get_color_correlations

  subroutine qcd_2_2_initialize (this, gev2_s, process_id, &
       integrand_id, parton_ids, flow, hyp, cart)
    class(qcd_2_2_t), intent(out) :: this
    real(default), intent(in) :: gev2_s
    integer, intent(in) :: process_id, integrand_id
    integer, dimension(2), intent(in) :: parton_ids
    integer, dimension(4), intent(in) :: flow
    real(default), dimension(3), intent(in)::hyp
    real(default), dimension(3), intent(in), optional :: cart
    call this%initialize (gev2_s)
    this%process_id = process_id
    this%integrand_id = integrand_id
    this%parton_ids = parton_ids
    this%flow = flow
    this%hyperbolic_fractions = hyp
    if (present (cart)) then
       this%momentum_fractions = cart
    else
       this%momentum_fractions = h_to_c_param (hyp)
    end if
  end subroutine qcd_2_2_initialize

  subroutine muli_write_to_marker (this, marker, status)
    class(muli_t), intent(in) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status
    call marker%mark_begin ("muli_t")
    call qcd_2_2_write_to_marker (this, marker, status)
    call marker%mark ("modify_pdfs", this%modify_pdfs)
    call marker%mark ("initialized", this%initialized)
    call marker%mark &
         ("initial_interaction_given", this%initial_interaction_given)
    call marker%mark ("finished", this%finished)
    call marker%mark ("init_time", this%init_time)
    call marker%mark ("pt_time", this%pt_time)
    call marker%mark ("partons_time", this%partons_time)
    call marker%mark ("confirm_time", this%confirm_time)
    ! call marker%mark_instance (this%start_values, "start_values")
    call marker%mark_instance (this%dsigma, "dsigma")
    call marker%mark_instance (this%samples, "samples")
    call marker%mark_instance (this%beam, "beam")
    call marker%mark_end ("muli_t")
  end subroutine muli_write_to_marker

  subroutine muli_read_from_marker (this, marker, status)
    class(muli_t), intent(out) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status
    call marker%pick_begin ("muli_t", status=status)
    call qcd_2_2_read_from_marker (this, marker, status)
    call marker%pick ("modify_pdfs", this%modify_pdfs, status)
    call marker%pick ("initialized", this%initialized, status)
    call marker%pick &
         ("initial_interaction_given", this%initial_interaction_given, status)
    call marker%pick ("finished", this%finished, status)
    call marker%pick ("init_time", this%init_time, status)
    call marker%pick ("pt_time", this%pt_time, status)
    call marker%pick ("partons_time", this%partons_time, status)
    call marker%pick ("confirm_time", this%confirm_time, status)
    ! call marker%pick_instance &
    !    ("start_values", this%start_values, status=status)
    call marker%pick_instance ("dsigma", this%dsigma, status=status)
    call marker%pick_instance ("samples", this%samples, status=status)
    call marker%pick_instance ("beam", this%beam, status=status)
    call marker%pick_end ("muli_t", status)
  end subroutine muli_read_from_marker

  subroutine muli_print_to_unit (this, unit, parents, components, peers)
    class(muli_t), intent(in) :: this
    integer, intent(in) :: unit
    integer(dik), intent(in) :: parents, components, peers
    if (parents>0) &
         call qcd_2_2_print_to_unit (this, unit, parents-1, components, peers)
    write (unit, "(1x,A)")  "Components of muli_t :"
    write (unit, "(3x,A)")  "Model Parameters:"
    write (unit, "(3x,A,E20.10)") "GeV2_scale_cutoff : ", &
         this%GeV2_scale_cutoff
    write (unit, "(3x,A,L1)")  "Modify PDF        : ", this%modify_pdfs
    write (unit, "(3x,A)")  "PT Chain Status:"
    write (unit, "(3x,A,L1)")  "Initialized       : ", this%initialized
    write (unit, "(3x,A,L1)")  "initial_interaction_given: ", &
         this%initial_interaction_given
    write (unit, "(3x,A,L1)")  "Finished          : ", this%finished
    write (unit, "(3x,A,L1)")  "Exceeded          : ", this%exceeded
    write (unit, "(3x,A)")  "Generator Internals:"
    write (unit, "(3x,A,E20.10)")  "Mean Value        : ", this%mean
    if (components > i_zero) then
       write (unit, "(3x,A,16(E20.10))")  "Start Integrals   : ", &
            this%start_integrals(1:16)
       ! write (unit, "(3x,A)")  "start_values Component:"
       ! call this%start_values%print_to_unit &
       !    (unit, parents, components-1, peers)
       write (unit, "(3x,A)")  "dsigma Component:"
       call this%dsigma%print_to_unit (unit, parents, components-1, peers)
       write (unit, "(3x,A)")  "samples Component:"
       call this%samples%print_to_unit (unit, parents, components-1, peers)
       write (unit, "(3x,A)")  "beam Component:"
       call this%beam%print_to_unit (unit, parents, components-1, peers)
    else
       write (unit, "(3x,A)")  "Skipping Derived-Type Components."
    end if
    ! call print_comp_pointer (this%start_node, unit, i_zero, &
    !    min(components-1,i_one), i_zero, "start_node")
    ! call serialize_print_comp_pointer (this%node, unit, i_zero, &
    !    min(components-1,i_one), i_zero, "node")
  end subroutine muli_print_to_unit

  pure subroutine muli_get_type(type)
    character(:), allocatable, intent(out) :: type
    allocate (type, source="muli_t")
  end subroutine muli_get_type

  subroutine muli_initialize (this, GeV2_scale_cutoff, gev2_s, &
       muli_dir, random_seed)
    class(muli_t), intent(out) :: this
    real(kind=default), intent(in) :: gev2_s, GeV2_scale_cutoff
    character(*), intent(in) :: muli_dir
    integer, intent(in), optional :: random_seed
    real(double) :: time
    logical :: exist
    type(muli_dsigma_t) :: dsigma_aq
    character(3) :: lhapdf_member_c
    call cpu_time(time)
    this%init_time = this%init_time-time
    print *, "muli_initialize: The MULI modules are still not fully " &
         // "populated, so MULI might generate some dummy values instead " &
         // "of real Monte Carlo generated interactions."
    print *, "Given Parameters:"
    print *, "GeV2_scale_cutoff=", GeV2_scale_cutoff
    print *, "muli_dir=", muli_dir
    print *, "lhapdf_dir=", ""
    print *, "lhapdf_file=", muli_default_lhapdf_file
    print *, "lhapdf_member=", muli_default_lhapdf_member
    print *, ""
    call this%transverse_mom_t%initialize (gev2_s)
    call this%beam%initialize (muli_dir, lhapdf_dir="", &
         lhapdf_file=muli_default_lhapdf_file, &
         lhapdf_member=muli_default_lhapdf_member)
    this%GeV2_scale_cutoff = GeV2_scale_cutoff
    if (present(random_seed)) then
       call tao_random_create (this%tao_rnd, random_seed)
    else
       call tao_random_create (this%tao_rnd, 1)
    end if
    print *, "looking for previously generated root function..."
    call integer_with_leading_zeros (muli_default_lhapdf_member, 3, &
         lhapdf_member_c)
    inquire (file=muli_dir//"/dsigma_"//muli_default_lhapdf_file//".xml", &
         exist=exist)
    if (exist) then
       print *, "found. Starting deserialization..."
       call this%dsigma%deserialize &
            (name="dsigma_"//muli_default_lhapdf_file//"_"//lhapdf_member_c, &
            file=muli_dir//"/dsigma_"//muli_default_lhapdf_file//".xml")
       ! call this%dsigma%print_all ()
       print *, "done. Starting generation of plots..."
       call this%dsigma%gnuplot (muli_dir)
       print *, "done."
    else
       print *, &
            "No root function found. Starting generation of root function..."
       call dsigma_aq%generate (GeV2_scale_cutoff, gev2_s, this%dsigma)
       print *, "done. Starting serialization of root function..."
       call this%dsigma%serialize &
            (name="dsigma_"//muli_default_lhapdf_file//"_"//lhapdf_member_c, &
            file=muli_dir//"/dsigma_"//muli_default_lhapdf_file//".xml")
       print *, "done. Starting serialization of generator..."
       call dsigma_aq%serialize &
            (name="dsigma_aq_"//muli_default_lhapdf_file//"_" // &
            lhapdf_member_c, file=muli_dir//"/dsigma_aq_" // &
            muli_default_lhapdf_file//".xml")
       print *,"done. Starting generation of plots..."
       call this%dsigma%gnuplot (muli_dir)
       print *, "done."
    end if
    print *, ""
    print *, "looking for previously generated samples..."
    inquire (file=muli_dir//"/samples.xml", exist=exist)
    if (exist) then
       print *, "found. Starting deserialization..."
       call this%samples%deserialize ("samples",muli_dir//"/samples.xml")
    else
       print *,"No samples found. Starting with default initialization."
       call this%samples%initialize (4, int_sizes_all, int_all, 1E-2_default)
    end if
    call this%restart ()
    this%initialized = .true.
    call cpu_time (time)
    this%init_time = this%init_time + time
  end subroutine muli_initialize

  subroutine muli_apply_initial_interaction (this, GeV2_s, &
       x1, x2, pdg_f1, pdg_f2, n1, n2)
    class(muli_t), intent(inout) :: this
    real(default), intent(in) :: Gev2_s, x1, x2
    integer, intent(in):: pdg_f1, pdg_f2, n1, n2
    real(default) :: rnd1, rnd2, time
    if (this%initialized) then
       call cpu_time (time)
       this%init_time = this%init_time - time
       print *, "muli_apply_initial_interaction:"
       print *, "gev2_s=", gev2_s
       print *, "x1=", x1
       print *, "x2=", x2
       print *, "pdg_f1=", pdg_f1
       print *, "pdg_f2=", pdg_f2
       print *, "n1=", n1
       print *, "n2=", n2
       call tao_random_number (this%tao_rnd, rnd1)
       call tao_random_number (this%tao_rnd, rnd2)
       call cpu_time (time)
       this%init_time = this%init_time + time
       call this%beam%apply_initial_interaction &
                 (sqrt (gev2_s), x1, x2, pdg_f1, pdg_f2, n1, n2,&
                 !!! This is a hack: We should give the pt scale of the initial
                 !!! interaction. Unfortunately, we only know the invariant
                 !!! mass shat. shat/2 is the upper bound of pt, so we
                 !!! use it for now.
            sqrt(gev2_s) * x1 *x2 / 2D0, &
            rnd1, rnd2)
       this%initial_interaction_given = .true.
    else
       print *, &
            "muli_apply_initial_interaction: call muli_initialize first. STOP"
       stop
    end if
  end subroutine muli_apply_initial_interaction

  subroutine muli_finalize (this)
    class(muli_t), intent(inout) :: this
    print *, "muli_finalize"
    nullify (this%node)
    call this%dsigma%finalize ()
    call this%samples%finalize ()
    call this%beam%finalize ()
  end subroutine muli_finalize

  subroutine muli_stop_trainer (this)
    class(muli_t), intent(inout) :: this
    print *, "muli_stop_trainer: DUMMY!"
  end subroutine muli_stop_trainer

  subroutine muli_reset_timer (this)
    class(muli_t), intent(inout) :: this
    this%init_time = 0D0
    this%pt_time = 0D0
    this%partons_time = 0D0
    this%confirm_time = 0D0
  end subroutine muli_reset_timer

  subroutine muli_restart (this)
    class(muli_t), intent(inout) :: this
    call this%dsigma%get_rightmost (this%node)
    call this%beam%reset ()
    ! print *, associated(this%node)
    ! nullify (this%node)
    this%finished = .false.
    this%process_id = -1
    this%integrand_id = -1
    this%momentum_fractions = [-1D0,-1D0,1D0]
    this%hyperbolic_fractions = [-1D0,-1D0,1D0]
    ! this%start_values%process_id = -1
    ! this%start_values%integrand_id = -1
    ! this%start_values%momentum_fractions = [-1D0,-1D0,1D0]
    ! this%start_values%hyperbolic_fractions = [-1D0,-1D0,1D0]
    this%start_integrals = &
         [0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0]
  end subroutine muli_restart

  elemental function muli_is_initialized (this) result (res)
    logical :: res
    class(muli_t), intent(in) :: this
    res = this%initialized
  end function muli_is_initialized

  elemental function muli_is_initial_interaction_given (this) result (res)
    logical :: res
    class(muli_t), intent(in) :: this
    res = this%initial_interaction_given
  end function muli_is_initial_interaction_given

  elemental function muli_is_finished (this) result (res)
    logical :: res
    class(muli_t), intent(in) :: this
    res = this%finished
  end function muli_is_finished

  subroutine muli_enable_remnant_pdf (this)
    class(muli_t), intent(inout) :: this
    this%modify_pdfs = .true.
  end subroutine muli_enable_remnant_pdf

  subroutine muli_disable_remnant_pdf (this)
    class(muli_t), intent(inout) :: this
    this%modify_pdfs = .false.
  end subroutine muli_disable_remnant_pdf

  subroutine muli_generate_gev2_pt2 (this, gev2_start_scale, gev2_new_scale)
    class(muli_t), intent(inout) :: this
    real(kind=default), intent(in) :: gev2_start_scale
    real(kind=default), intent(out) :: gev2_new_scale
    real(double) :: time
    call cpu_time (time)
    this%pt_time = this%pt_time - time
    call this%set_gev2_scale (gev2_start_scale)
    this%start_integrals = this%node%approx_integral (this%get_unit_scale ())
    call this%generate_next_scale ()
    gev2_new_scale = this%get_gev2_scale ()
    call cpu_time (time)
    this%pt_time = this%pt_time + time
  end subroutine muli_generate_gev2_pt2

  subroutine muli_generate_partons (this, n1, n2, x_proton_1, x_proton_2, &
       pdg_f1, pdg_f2, pdg_f3, pdg_f4)
    class(muli_t), intent(inout) :: this
    integer, intent(in) :: n1, n2
    real(kind=default), intent(out) :: x_proton_1, x_proton_2
    integer, intent(out) :: pdg_f1, pdg_f2, pdg_f3, pdg_f4
    integer, dimension(4) :: pdg_f
    real(double) :: time
    ! print *, "muli_generate_partons: n1=", n1, " n2=", n2
    this%parton_ids(1) = n1
    this%parton_ids(2) = n2
    call cpu_time (time)
    this%partons_time = this%partons_time - time
    this%mean = this%node%approx_value_n (this%get_unit_scale(), &
         this%integrand_id)
    call this%samples%mcgenerate_hit (this%get_unit2_scale(), &
         this%mean, this%integrand_id, this%tao_rnd, this%process_id, &
         this%momentum_fractions)
    ! print *,"muli_generate_partons", this%momentum_fractions
    call this%generate_flow ()
    if (this%modify_pdfs) then
       call cpu_time (time)
       this%partons_time = this%partons_time + time
       this%confirm_time = this%confirm_time - time
       call this%beam%apply_interaction (this)
       call cpu_time (time)
       this%confirm_time = this%confirm_time + time
       this%partons_time = this%partons_time - time
    end if
    x_proton_1 = this%momentum_fractions(1)
    x_proton_2 = this%momentum_fractions(2)
    pdg_f = this%get_pdg_flavors ()
    pdg_f1 = pdg_f(1)
    pdg_f2 = pdg_f(2)
    pdg_f3 = pdg_f(3)
    pdg_f4 = pdg_f(4)
    call cpu_time (time)
    this%partons_time = this%partons_time - time
    call qcd_2_2_print_to_unit (this, output_unit, 100_dik, 100_dik, 100_dik)
  end subroutine muli_generate_partons

  subroutine muli_generate_flow(this)
    class(muli_t), intent(inout)::this
    integer::rnd
    integer::m,n
    logical, dimension(3)::t
    integer, dimension(4)::tmp_flow, tmp_array
    ! we initialize with zeros. a i_zero means no line ends here.
    this%flow=[0,0,0,0]
    ! we randomly pick a color flow
    call tao_random_number(this%tao_rnd,rnd)
    ! the third position of muli_flow_stats is the sum of all flow wheights of stratum diagram_kind.
    ! so we generate a random number 0 <= m < sum(weights)
    m=modulo(rnd,muli_flow_stats(3,this%get_diagram_color_kind()))
    ! lets visit all color flows of stratum diagram_kind. the first and second position of muli_flow_stats
    ! tells us the index of the first and the last valid color flow.
    do n=muli_flow_stats(1,this%get_diagram_color_kind()),muli_flow_stats(2,this%get_diagram_color_kind())
       ! now we remove the weight of flow n from our random number.
       m=m-muli_flows(0,n)
       ! this is how we pick a flow.
       if (m<0) then
          ! the actual flow
          this%flow=muli_flows(1:4,n)
          exit
       end if
    end do
    ! the diagram kind contains a primitive diagram and all diagramms which can be deriven by
    ! (1) global charge conjugation
    ! (2) permutation of the initial state particles
    ! (3) permutation of the final state particles
    ! lets see, what transformations we have got in our actual interaction.
    tmp_array = this%get_lha_flavors ()
    t = muli_get_state_transformations (this%get_diagram_color_kind (), &
         tmp_array)
    !     this%get_lha_flavors ())
    ! now we have to apply these transformations to our flow.
    ! (1) means: swap beginning and end of a line. flow is a permutation that maps
    ! ends to their beginnings, so we apply flow to itself:
!!$    print *,"(0)",this%flow
    if (t(1)) then
       tmp_flow=this%flow
       this%flow=[0,0,0,0]
       do n=1,4
          if (tmp_flow(n)>0)this%flow(tmp_flow(n))=n
       end do
!!$       print *,"(1)",this%flow
    end if
    if (t(2)) then
       ! we swap the particles 1 and 2
       tmp_flow(1)=this%flow(2)
       tmp_flow(2)=this%flow(1)
       tmp_flow(3:4)=this%flow(3:4)
!!$       print *,"(2)",tmp_flow
       ! we swap the beginnings assigned to particle 1 and 2
       where(tmp_flow==1)
          this%flow=2
       elsewhere(tmp_flow==2)
          this%flow=1
       elsewhere
          this%flow=tmp_flow
       end where
!!$       print *,"(2)",this%flow
    end if
    if (t(3)) then
       ! we swap the particles 3 and 4
       tmp_flow(1:2)=this%flow(1:2)
       tmp_flow(3)=this%flow(4)
       tmp_flow(4)=this%flow(3)
!!$       print *,"(3)",tmp_flow
       ! we swap the beginnings assigned to particle 3 and 4
       where(tmp_flow==3)
          this%flow=4
       elsewhere(tmp_flow==4)
          this%flow=3
       elsewhere
          this%flow=tmp_flow
       end where
!!$       print *,"(3)",this%flow
    end if
  end subroutine muli_generate_flow

  subroutine muli_replace_parton &
       (this, proton_id, old_id, new_id, pdg_f, x_proton, gev_scale)
    class(muli_t), intent(inout) :: this
    integer, intent(in) :: proton_id, old_id, new_id, pdg_f
    real(kind=default), intent(in) :: x_proton, gev_scale
    ! print *, "muli_replace_parton(", proton_id, old_id, new_id, &
    !    pdg_f, x_proton, gev_scale, ")"
    if (proton_id==1 .or. proton_id==2) then
       call this%beam%replace_parton &
            (proton_id, old_id, new_id, pdg_f, x_proton, gev_scale)
    else
       print *, "muli_replace_parton: proton_id must be 1 or 2, but ", &
            proton_id, " was given."
       stop
    end if
  end subroutine muli_replace_parton

  function muli_get_parton_pdf &
       (this, x_proton, gev2_scale, n, pdg_f) result (pdf)
    real(default) :: pdf
    class(muli_t), intent(in) :: this
    real(default), intent(in) :: x_proton, gev2_scale
    integer, intent(in) :: n, pdg_f
    call this%beam%parton_pdf (x_proton, gev2_scale, n, pdg_f, pdf)
  end function muli_get_parton_pdf

  function muli_get_momentum_pdf &
       (this, x_proton, gev2_scale, n, pdg_f) result (pdf)
    real(default) :: pdf
    class(muli_t), intent(in) :: this
    real(default), intent(in) :: x_proton, gev2_scale
    integer, intent(in) :: n, pdg_f
    call this%beam%momentum_pdf (x_proton, gev2_scale, n, pdg_f, pdf)
  end function muli_get_momentum_pdf

  subroutine muli_print_timer(this)
    class(muli_t), intent(in) :: this
    print ("(1x,A,E20.10)"), "Init time:    ", this%init_time
    print ("(1x,A,E20.10)"), "PT gen time:  ", this%pt_time
    print ("(1x,A,E20.10)"), "Partons time: ", this%partons_time
    print ("(1x,A,E20.10)"), "Confirm time: ", this%confirm_time
    print ("(1x,A,E20.10)"), "Overall time: ", &
         this%init_time + this%pt_time + this%partons_time + this%confirm_time
  end subroutine muli_print_timer

  subroutine muli_generate_samples &
       (this, n_total, n_print, integrand_kind, muli_dir, analyse)
    class(muli_t), intent(inout) :: this
    integer(dik), intent(in) :: n_total, n_print
    integer, intent(in) :: integrand_kind
    character(*), intent(in) :: muli_dir
    logical, intent(in) :: analyse
    integer(dik) :: n_inner

    class(muli_trapezium_node_class_t), pointer :: start_node => null()
    class(muli_trapezium_node_class_t), pointer, save :: s_node => null()
    class(muli_trapezium_node_class_t), pointer, save :: node => null()

    character(2) :: prefix
    integer, save :: t_slice, t_region, t_proc, t_subproc, t_max_n = 0
    integer(dik) :: n_t, n_p, n_m
    integer :: n, m, u, unit = 0
    integer(dik) :: n_tries = 0
    integer(dik) :: n_hits = 0
    integer(dik) :: n_over = 0
    integer(dik) :: n_miss = 0
    real(default), save, dimension(3) :: cart_hit
    integer, save, dimension(4) :: t_i_rnd
    ! integer, save, dimension(5) :: r_n_proc
    real(default), dimension(16) :: d_rnd
    real(default), save :: t_area, t_dddsigma, t_rnd, t_weight, t_arg
    real(default) :: mean = 0D0
    real(default) :: time = 0D0
    real(default) :: timepa = 0D0
    real(default) :: timept = 0D0
    real(default) :: timet = 0D0
    real(default) :: pts, s_pts = 1D0
    real(default) :: pts2 = 1D0
    real(default) :: rnd
    logical :: running
    character(3) :: num
    integer :: success = -1
    integer :: chain_length = 0
    integer :: int_kind
    integer :: process_id
    real(double), dimension(0:16) :: integral
    call this%print_parents ()
    n_tries = one
    n_inner = n_total / n_print
    n_t = i_zero
    PRINT: do while (n_t < n_total)
       call cpu_time (time)
       timet = - time
       n_p = i_zero
       INNER: do while (n_p < n_print)
          chain_length = 0
          ! print *,"new chain"
          call this%restart ()
          this%integrand_id = integrand_kind
          call cpu_time (time)
          timept = timept - time
          call this%generate_next_scale (integrand_kind)
          call cpu_time (time)
          timept = timept + time
          CHAIN: do while (.not. this%is_finished ())
             chain_length = chain_length + 1
             n_p = n_p + 1
             call this%confirm ()
             call cpu_time (time)
             timepa = timepa - time
             ! print *, this%get_unit2_scale ()
             call this%samples%mcgenerate_hit (this%get_unit2_scale(), &
                  this%mean, this%integrand_id, this%tao_rnd, this%process_id, &
                  this%momentum_fractions)
             call cpu_time (time)
             timepa = timepa + time
             timept = timept - time
             call this%generate_next_scale (integrand_kind)
             call cpu_time (time)
             timept = timept + time
          end do CHAIN
          ! print *, "chain length = ", chain_length
       end do INNER
       n_t = n_t + n_p
       call this%samples%sum_up ()
       call cpu_time (time)
       timet = timet + time
       print *, n_t, "/", n_total
       print *, "time: ", timet
       print *, "pt time: ", timept
       print *, "pa time: ", timepa
       print *, this%samples%n_tries_sum, this%samples%n_hits_sum, &
            this%samples%n_over_sum
       if (this%samples%n_hits_sum > 0) then
          print *, (this%samples%n_hits_sum * 10000) / &
               this%samples%n_tries_sum, (this%samples%n_over_sum * 10000) / &
               this%samples%n_hits_sum
       else
          print *, "no hits"
       end if
       ! print ('(7(I11," "),5(E14.7," "))'), n_p, n_print, n_tries, &
       !   n_hits,n_over, int((n_hits*1D3)/n_tries), &
       !   int((n_over*1D6)/n_tries), n_hits/real(n_over), time1, time2, &
       !   time3, this%samples%int_kinds(integrand_kind)%overall_boost
    end do print
    call integer_with_leading_zeros (integrand_kind, 2, prefix)
    if (analyse) then
       call this%samples%int_kinds(integrand_kind)%analyse &
            (muli_dir, prefix//"_")
       call this%samples%int_kinds(integrand_kind)%serialize &
            ("sample_int_kind_"//prefix, &
            muli_dir//"/sample_int_kind/"//prefix//".xml")
    end if
    call this%samples%int_kinds(integrand_kind)%serialize &
         ("sample_int_kind_"//prefix, &
         muli_dir//"/sample_int_kind/"//prefix//".xml")
  end subroutine muli_generate_samples

  subroutine muli_fake_interaction (this, GeV2_scale, x1, x2, &
       process_id, integrand_id, n1, n2, flow)
    class(muli_t), intent(inout) :: this
    real(default), intent(in) :: Gev2_scale, x1, x2
    integer, intent(in) :: process_id, integrand_id, n1, n2
    integer, dimension(4), intent(in), optional :: flow
    call this%set_gev2_scale (Gev2_scale)
    this%process_id = process_id
    this%integrand_id = integrand_id
    this%parton_ids = [n1, n2]
    if (present (flow)) then
       this%flow = flow
    else
       this%flow = [0,0,0,0]
    end if
    this%momentum_fractions = [x1, x2, this%get_unit2_scale()]
    call this%beam%apply_interaction (this)
    call this%beam%print_all ()
  end subroutine muli_fake_interaction

  subroutine muli_generate_next_scale (this, integrand_kind)
    class(muli_t), intent(inout) :: this
    integer, intent(in), optional :: integrand_kind
    real(default) :: pts, tmp_pts, rnd
    integer :: tmp_int_kind
    class(muli_trapezium_node_class_t), pointer :: tmp_node
      pts = - one
      if (present (integrand_kind)) then
         call tao_random_number (this%tao_rnd, rnd)
         call generate_single_pts (integrand_kind, &
              this%start_integrals(integrand_kind), &
              this%beam%get_pdf_int_weights &
              (double_pdf_kinds (1:2,integrand_kind)), rnd, this%dsigma, &
              pts, this%node)
      else
         do tmp_int_kind = 1, 16
            call tao_random_number (this%tao_rnd, rnd)
            call generate_single_pts (tmp_int_kind, &
                 this%start_integrals(tmp_int_kind), &
                 this%beam%get_pdf_int_weights &
                 (double_pdf_kinds(1:2,tmp_int_kind)), rnd, &
                 this%dsigma, tmp_pts, tmp_node)
            if (tmp_pts > pts) then
               pts = tmp_pts
               this%integrand_id = tmp_int_kind
               this%node => tmp_node
            end if
         end do
      end if
      if (pts > 0) then
         call this%set_unit_scale (pts)
      else
         this%finished = .true.
      end if
      ! print *, this%finished, this%integrand_id
  contains
    subroutine generate_single_pts &
         (int_kind, start_int, weight, rnd, int_tree, pts, node)
      integer, intent(in) :: int_kind
      real(default), intent(in) :: start_int, weight, rnd
      type(muli_trapezium_tree_t), intent(in) :: int_tree
      real(default), intent(out) :: pts
      class(muli_trapezium_node_class_t),pointer, intent(out) :: node
      real(default) :: arg
      ! print *, int_kind, start_int, weight, rnd
      if (weight > 0D0) then
         arg = start_int - log(rnd) / weight
         call int_tree%find_decreasing (arg, int_kind, node)
         if (node%get_l_integral(int_kind) > arg) then
            pts = node%approx_position_by_integral (int_kind, arg)
         else
            pts = -1D0
         end if
      else
         pts = -1D0
      end if
    end subroutine generate_single_pts
  end subroutine muli_generate_next_scale

  subroutine muli_confirm (this)
    class(muli_t), intent(inout) :: this
    this%mean = this%node%approx_value_n(this%get_unit_scale (), &
         this%integrand_id)
    this%start_integrals = this%node%approx_integral (this%get_unit_scale ())
  end subroutine muli_confirm


end module muli
