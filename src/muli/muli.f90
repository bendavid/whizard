!!! module: muli
!!! This code is part of my Ph.D studies.
!!! 
!!! Copyright (C) 2011 Hans-Werner Boschmann <boschmann@tp1.physik.uni-siegen.de>
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
!!! Latest Change: 2011-08-03 11:12:37 CEST(+0200)
!!! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!! This file contains the module "muli" which is the multiple parton 
!!! interactions interface module to WHIZARD. muli is supposed to run together
!!! with initial state radiation. Both share a momentum evolution variable and 
!!! compete for beam momentum, so the generation of this scale variable must
!!! be fully transparent to WHIZARD. That's why the corresponding procedures
!!! are here, while all other work like phase space integration, flavour
!!! generation and treatment of the beam remnant is put into muli_dsigma, 
!!! muli_mcint and muli_remnant respectively.

!!! qcd_2_2_type is a container class for properties of qcd 2->2 interactions.
!!! It holds a very condensed internal representation and offers a convenient
!!! set of TBP to query all aspects without the burden of the generator 
!!! internals.

!!! muli_type then is an extension of qcd_2_2_type that adds generator
!!! internals like random number generator, integrated cross-sections, the
!!! actual Monte Carlo generator for flavour genertation and beam remnants in
!!! tao_rnd, dsigma, samples and beam respectively.

module muli
  use muli_dsigma
  use muli_mcint
  use muli_remnant
  implicit none

  logical,parameter::muli_default_modify_pdfs=.true.
  integer,parameter::muli_default_lhapdf_member=0
  character(*),parameter::muli_default_lhapdf_file="cteq6ll.LHpdf"
  
  type,extends(qcd_2_2_class)::qcd_2_2_type
     private
     integer::process_id=-1
     integer::integrand_id=-1
     integer,dimension(2)::parton_ids=[0,0]
     integer,dimension(4)::flow=[0,0,0,0]
     real(kind=double),dimension(3)::momentum_fractions=[-1D0,-1D0,-1D0]
     real(kind=double),dimension(3)::hyperbolic_fractions=[-1D0,-1D0,-1D0]
   contains
     !overridden serializable_class procedures
     procedure,public::write_to_marker=>qcd_2_2_write_to_marker
     procedure,public::read_from_marker=>qcd_2_2_read_from_marker
     procedure,public::print_to_unit=>qcd_2_2_print_to_unit
     procedure,public,nopass::get_type=>qcd_2_2_get_type
     ! new type-bound-procedures
     procedure,public::get_process_id=>qcd_2_2_get_process_id
     procedure,public::get_integrand_id=>qcd_2_2_get_integrand_id
     procedure,public::get_diagram_kind=>qcd_2_2_get_diagram_kind
     procedure,public::get_diagram_color_kind=>qcd_2_2_get_diagram_color_kind
     procedure,public::get_io_kind=>qcd_2_2_get_io_kind
     procedure,public::get_lha_flavors=>qcd_2_2_get_lha_flavors
     procedure,public::get_pdg_flavors=>qcd_2_2_get_pdg_flavors
     procedure,public::get_parton_id=>qcd_2_2_get_parton_id
     procedure,public::get_parton_kinds=>qcd_2_2_get_parton_kinds
     procedure,public::get_pdf_int_kinds=>qcd_2_2_get_pdf_int_kinds
     procedure,public::get_momentum_boost=>qcd_2_2_get_momentum_boost
     procedure,public::get_hyperbolic_fractions=>qcd_2_2_get_hyperbolic_fractions
     procedure,public::get_remnant_momentum_fractions=>qcd_2_2_get_remnant_momentum_fractions
     procedure,public::get_total_momentum_fractions=>qcd_2_2_get_total_momentum_fractions
     procedure,public::get_color_flow=>qcd_2_2_get_color_flow
     procedure,public::get_color_correlations=>qcd_2_2_get_color_correlations
     procedure,public::qcd_2_2_initialize
     generic,public::initialize=>qcd_2_2_initialize
  end type qcd_2_2_type

  type,extends(qcd_2_2_type)::muli_type
!     private
     ! model parameters
     real(kind=double)::GeV2_scale_cutoff     
     logical::modify_pdfs=muli_default_modify_pdfs
     ! pt chain status
     logical::finished=.false.
     logical::exceeded=.false.
     ! timers
     real(kind=double)::init_time=0D0
     real(kind=double)::pt_time=0D0
     real(kind=double)::partons_time=0D0
     real(kind=double)::confirm_time=0D0
     ! generator internals
     logical::initialized=.false.
     logical::initial_interaction_given=.false.
     real(kind=double)::mean=1D0
     real(kind=double),dimension(0:16)::start_integrals=&
          [0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0]
     type(tao_random_state)::tao_rnd
     type(muli_trapezium_tree_type)::dsigma
     type(sample_inclusive_type)::samples
     type(pp_remnant_type)::beam
     ! these pointers shall not be allocated, deallocated, serialized or deserialized explicitly.
     class(muli_trapezium_node_class),pointer::node=>null()
   contains
     ! overridden serializable_class procedures
     procedure,public::write_to_marker=>muli_write_to_marker
     procedure,public::read_from_marker=>muli_read_from_marker
     procedure,public::print_to_unit=>muli_print_to_unit
     procedure,public,nopass::get_type=>muli_get_type
     ! init / final
     procedure,public::muli_initialize
     procedure,public::apply_initial_interaction=>muli_apply_initial_interaction
     procedure,public::finalize=>muli_finalize
     procedure,public::stop_trainer=>muli_stop_trainer
     procedure,public::reset_timer=>muli_reset_timer
     procedure,public::restart=>muli_restart
     generic,public::  initialize=>muli_initialize
     ! status query
     procedure,public::is_initialized=>muli_is_initialized
     procedure,public::is_initial_interaction_given=>muli_is_initial_interaction_given
     procedure,public::is_finished=>muli_is_finished
     ! user interface
     procedure,public::enable_remnant_pdf=>muli_enable_remnant_pdf
     procedure,public::disable_remnant_pdf=>muli_disable_remnant_pdf
     procedure,public::generate_gev2_pt2=>muli_generate_gev2_pt2
     procedure,public::generate_partons=>muli_generate_partons
     procedure,public::generate_flow=>muli_generate_flow
     procedure,public::replace_parton=>muli_replace_parton
     procedure,public::get_parton_pdf=>muli_get_parton_pdf
     procedure,public::get_momentum_pdf=>muli_get_momentum_pdf
     procedure,public::print_timer=>muli_print_timer
     procedure,public::generate_samples=>muli_generate_samples
     ! beam test
     procedure,public::fake_interaction=>muli_fake_interaction
     ! private procedures
     procedure,private::generate_next_scale=>muli_generate_next_scale
     procedure,private::confirm=>muli_confirm
  end type muli_type

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! type bound procedures for qcd_2_2_type !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  !overridden serializable_class procedures

  subroutine qcd_2_2_write_to_marker(this,marker,status)
    class(qcd_2_2_type),intent(in)::this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
    call marker%mark_begin("qcd_2_2_type")
    call transversal_momentum_write_to_marker(this,marker,status)
    call marker%mark("process_id",this%process_id)
    call marker%mark("integrand_id",this%integrand_id)
    call marker%mark("momentum_fractions",this%momentum_fractions)
    call marker%mark("hyperbolic_fractions",this%hyperbolic_fractions)
    call marker%mark_end("qcd_2_2_type")
  end subroutine qcd_2_2_write_to_marker

  subroutine qcd_2_2_read_from_marker(this,marker,status)
    class(qcd_2_2_type),intent(out)::this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
    call marker%pick_begin("qcd_2_2_type",status=status)
    call transversal_momentum_read_from_marker(this,marker,status)
    call marker%pick("process_id",this%process_id,status)
    call marker%pick("integrand_id",this%integrand_id,status)
    call marker%pick("momentum_fractions",this%momentum_fractions,status)
    call marker%pick("hyperbolic_fractions",this%hyperbolic_fractions,status)
    call marker%pick_end("qcd_2_2_type",status=status)
  end subroutine qcd_2_2_read_from_marker

  subroutine qcd_2_2_print_to_unit(this,unit,parents,components,peers)
    class(qcd_2_2_type),intent(in)::this
    integer,intent(in)::unit
    integer(kind=dik),intent(in)::parents,components,peers
    integer,dimension(2,4)::flow
    integer::index
    if(parents>zero)call transversal_momentum_print_to_unit(this,unit,parents-1,components,peers)
    write(unit,'("Components of qcd_2_2_type:")')
    write(unit,'("Process id is:       ",I3)')this%get_process_id()
    write(unit,'("Integrand id is:     ",I3)')this%get_integrand_id()
    if(this%get_integrand_id()>0)then
       write(unit,'("LHA Flavors are:     ",4(I3))')this%get_lha_flavors()
       write(unit,'("PDG Flavors are:     ",4(I3))')this%get_pdg_flavors()
       write(unit,'("Parton kinds are:    ",2(I3))')this%get_parton_kinds()
       write(unit,'("PDF int kinds are:   ",2(I3))')this%get_pdf_int_kinds()
       write(unit,'("Diagram kind is:     ",2(I3))')this%get_diagram_kind()
    end if
    call this%get_color_correlations(1,index,flow)
    write(unit,'("Color Permutations:  ",4(I0))')this%flow
    write(unit,'("Color Connections:")')
    write(unit,'("(",I0,",",I0,")+(",I0,",",I0,")->(",I0,",",I0,")+(",I0,",",I0,")")')flow
    write(unit,'("Evolution scale is:  ",E14.7)')this%get_unit2_scale()
    write(unit,'("Momentum boost is:   ",E14.7)')this%get_momentum_boost()
    write(unit,'("Remant momentum fractions are: ",2(E14.7))')this%get_remnant_momentum_fractions()
    write(unit,'("Total momentum fractions are:  ",2(E14.7))')this%get_total_momentum_fractions()
  end subroutine qcd_2_2_print_to_unit
  
  pure subroutine qcd_2_2_get_type(type)
    character(:),allocatable,intent(out)::type
    allocate(type,source="qcd_2_2_type")
  end subroutine qcd_2_2_get_type

  elemental function qcd_2_2_get_process_id(this) result(id)
    class(qcd_2_2_type),intent(in)::this
    integer::id
    id=this%process_id
  end function qcd_2_2_get_process_id

  elemental function qcd_2_2_get_integrand_id(this) result(id)
    class(qcd_2_2_type),intent(in)::this
    integer::id
    id=this%integrand_id
  end function qcd_2_2_get_integrand_id

  pure function qcd_2_2_get_lha_flavors(this) result(lha)
    class(qcd_2_2_type),intent(in)::this
    integer,dimension(4)::lha
    lha=valid_processes(1:4,this%process_id)
  end function qcd_2_2_get_lha_flavors

  pure function qcd_2_2_get_pdg_flavors(this) result(pdg)
    class(qcd_2_2_type),intent(in)::this
    integer,dimension(4)::pdg
    pdg=this%get_lha_flavors()
    where(pdg==0) pdg=21
  end function qcd_2_2_get_pdg_flavors

  pure function qcd_2_2_get_pdf_int_kinds(this) result(kinds)
    class(qcd_2_2_type),intent(in)::this
    integer,dimension(2)::kinds
    kinds=double_pdf_kinds(1:2,this%integrand_id)
  end function qcd_2_2_get_pdf_int_kinds

  elemental function qcd_2_2_get_parton_id(this,n) result(id)
    class(qcd_2_2_type),intent(in)::this
    integer,intent(in)::n
    integer::id
    id=this%parton_ids(n)
  end function qcd_2_2_get_parton_id

  pure function qcd_2_2_get_parton_kinds(this) result(kinds)
    class(qcd_2_2_type),intent(in)::this
    integer,dimension(2)::kinds
    kinds=this%get_pdf_int_kinds()
    kinds(1)=parton_kind_of_int_kind(kinds(1))
    kinds(2)=parton_kind_of_int_kind(kinds(2))
  end function qcd_2_2_get_parton_kinds

  elemental function qcd_2_2_get_io_kind(this) result(kind)
    class(qcd_2_2_type),intent(in)::this
    integer::kind
    kind=valid_processes(5,this%process_id)
  end function qcd_2_2_get_io_kind

  elemental function qcd_2_2_get_diagram_kind(this) result(kind)
    class(qcd_2_2_type),intent(in)::this
    integer::kind
    kind=valid_processes(6,this%process_id)
  end function qcd_2_2_get_diagram_kind

  elemental function qcd_2_2_get_diagram_color_kind(this) result(kind)
    ! This is one more hack. Before merging into the interleaved algorithm, muli has only cared for 
    ! summed cross sections, but not in specific color flows. So two different diagrams with equal 
    ! cross sections were summed up to diagram kind 1. 
    ! Now muli also generates color flows, so we must devide diagram kind 1 into diagram color kind 0
    ! and diagram color kind 1.
    class(qcd_2_2_type),intent(in)::this
    integer::kind
    kind=valid_processes(6,this%process_id)
    if(kind==1)then
       if(product(valid_processes(1:2,this%process_id))>0)then
          kind=0
       end if
    end if
  end function qcd_2_2_get_diagram_color_kind

  elemental function qcd_2_2_get_momentum_boost(this) result(boost)
    class(qcd_2_2_type),intent(in)::this
    real(kind=double)::boost
    boost=-1D0
!    print('("qcd_2_2_get_momentum_boost: not yet implemented.")')
!    boost=this%momentum_boost
  end function qcd_2_2_get_momentum_boost

  pure function qcd_2_2_get_hyperbolic_fractions(this) result(fractions)
    class(qcd_2_2_type),intent(in)::this
    real(kind=double),dimension(3)::fractions
    fractions=this%hyperbolic_fractions
  end function qcd_2_2_get_hyperbolic_fractions

  pure function qcd_2_2_get_remnant_momentum_fractions(this) result(fractions)
    class(qcd_2_2_type),intent(in)::this
    real(kind=double),dimension(2)::fractions
    fractions=this%momentum_fractions(1:2)
  end function qcd_2_2_get_remnant_momentum_fractions

  pure function qcd_2_2_get_total_momentum_fractions(this) result(fractions)
    class(qcd_2_2_type),intent(in)::this
    real(kind=double),dimension(2)::fractions
    fractions=[-1D0,-1D0]
!    fractions=this%hyperbolic_fractions(1:2)*this%beam%get_proton_remnant_momentum_fractions()
  end function qcd_2_2_get_total_momentum_fractions

  pure function qcd_2_2_get_color_flow(this) result(flow)
    class(qcd_2_2_type),intent(in)::this
    integer,dimension(4)::flow
    flow=this%flow
  end function qcd_2_2_get_color_flow

  subroutine qcd_2_2_get_color_correlations(this,start_index,final_index,flow)
    class(qcd_2_2_type),intent(in)::this
    integer,intent(in)::start_index
    integer,intent(out)::final_index
    integer,dimension(2,4),intent(out)::flow
    integer::pos,f_end,f_beginning
    final_index=start_index
    ! we set all flows to zero. zero means no connection.
    flow=reshape([0,0,0,0,0,0,0,0],[2,4])
    ! look at all four possible ends of color lines
    do f_end=1,4
       ! the beginning of of this potential line is stored in flow. zero means no line.
       f_beginning=this%flow(f_end)
       ! is there a line beginning at f_beginning and ending at f_end?
       if(f_beginning>0)then
          ! yes it is. we get a new number for this new line
          final_index=final_index+1
          ! is this line beginning in the initial state?
          if(f_beginning<3)then
             ! yes it is. lets connect the color entry of f_begin.            
             flow(1,f_beginning)=final_index
          else
             ! no, it's the final state. lets connect the anticolor entry of f_begin.
             flow(2,f_beginning)=final_index
          end if
          ! is this line ending in the final state?
          if(f_end>2)then
             ! yes it is. lets connect the color entry of f_end.
             flow(1,f_end)=final_index
          else
             ! no, it's the initial state. lets connect the anticolor entry of f_end.
             flow(2,f_end)=final_index
          end if
       end if
    end do
  end subroutine qcd_2_2_get_color_correlations

  subroutine qcd_2_2_initialize(this,gev2_s,process_id,integrand_id,parton_ids,flow,hyp,cart)
    class(qcd_2_2_type),intent(out)::this
    real(kind=double),intent(in)::gev2_s
    integer,intent(in)::process_id,integrand_id
    integer,dimension(2),intent(in)::parton_ids
    integer,dimension(4),intent(in)::flow
    real(kind=double),dimension(3),intent(in)::hyp
    real(kind=double),dimension(3),intent(in),optional::cart
    call this%initialize(gev2_s)
    this%process_id=process_id
    this%integrand_id=integrand_id
    this%parton_ids=parton_ids
    this%flow=flow
    this%hyperbolic_fractions=hyp
    if(present(cart))then
       this%momentum_fractions=cart
    else
       this%momentum_fractions=h_to_c_param(hyp)
    end if
  end subroutine qcd_2_2_initialize

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! type bound procedures for muli_type !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! overridden serializable_class procedures

  subroutine muli_write_to_marker(this,marker,status)
    class(muli_type),intent(in) :: this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
    call marker%mark_begin("muli_type")
    call qcd_2_2_write_to_marker(this,marker,status)
    call marker%mark("modify_pdfs",this%modify_pdfs)
    call marker%mark("initialized",this%initialized)
    call marker%mark("initial_interaction_given",this%initial_interaction_given)
    call marker%mark("finished",this%finished)
    call marker%mark("init_time",this%init_time)
    call marker%mark("pt_time",this%pt_time)
    call marker%mark("partons_time",this%partons_time)
    call marker%mark("confirm_time",this%confirm_time)
!    call marker%mark_instance(this%start_values,"start_values")
    call marker%mark_instance(this%dsigma,"dsigma")
    call marker%mark_instance(this%samples,"samples")
    call marker%mark_instance(this%beam,"beam")
    call marker%mark_end("muli_type")
  end subroutine muli_write_to_marker
  
  subroutine muli_read_from_marker(this,marker,status)
    class(muli_type),intent(out) :: this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
    call marker%pick_begin("muli_type",status=status)
    call qcd_2_2_read_from_marker(this,marker,status)
    call marker%pick("modify_pdfs",this%modify_pdfs,status)
    call marker%pick("initialized",this%initialized,status)
    call marker%pick("initial_interaction_given",this%initial_interaction_given,status)
    call marker%pick("finished",this%finished,status)
    call marker%pick("init_time",this%init_time,status)
    call marker%pick("pt_time",this%pt_time,status)
    call marker%pick("partons_time",this%partons_time,status)
    call marker%pick("confirm_time",this%confirm_time,status)
!    call marker%pick_instance("start_values",this%start_values,status=status)
    call marker%pick_instance("dsigma",this%dsigma,status=status)
    call marker%pick_instance("samples",this%samples,status=status)
    call marker%pick_instance("beam",this%beam,status=status)
    call marker%pick_end("muli_type",status)
  end subroutine muli_read_from_marker

  subroutine muli_print_to_unit(this,unit,parents,components,peers)
    class(muli_type),intent(in)::this
    integer,intent(in)::unit
    integer(kind=dik),intent(in)::parents,components,peers
    if(parents>0)call qcd_2_2_print_to_unit(this,unit,parents-1,components,peers)
    write(unit,fmt="(a)")"Components of muli_type :"
    write(unit,'("Model Parameters:")')
    write(unit,'("GeV2_scale_cutoff : ",E20.10)')this%GeV2_scale_cutoff
    write(unit,'("Modify PDF        : ",L1)')this%modify_pdfs
    write(unit,'("PT Chain Status:")')
    write(unit,'("Initialized       : ",L1)')this%initialized
    write(unit,'("initial_interaction_given: ",L1)')this%initial_interaction_given
    write(unit,'("Finished          : ",L1)')this%finished
    write(unit,'("Exceeded          : ",L1)')this%exceeded
    write(unit,'("Generator Internals:")')
    write(unit,'("Mean Value        : ",E20.10)')this%mean
    if(components>zero)then
       write(unit,'("Start Integrals   : ",16(E20.10))')this%start_integrals(1:16)
!       write(unit,'("start_values Component:")')
!       call this%start_values%print_to_unit(unit,parents,components-1,peers)
       write(unit,'("dsigma Component:")')
       call this%dsigma%print_to_unit(unit,parents,components-1,peers)
       write(unit,'("samples Component:")')
       call this%samples%print_to_unit(unit,parents,components-1,peers)
       write(unit,'("beam Component:")')
       call this%beam%print_to_unit(unit,parents,components-1,peers)
    else
       write(unit,'("Skipping Derived-Type Components.")')
    end if
!    call print_comp_pointer(this%start_node,unit,zero,min(components-1,one),zero,"start_node")
!    call serialize_print_comp_pointer(this%node,unit,zero,min(components-1,one),zero,"node")
  end subroutine muli_print_to_unit

  pure subroutine muli_get_type(type)
    character(:),allocatable,intent(out)::type
    allocate(type,source="muli_type")
  end subroutine muli_get_type

  ! user interface

  subroutine muli_apply_initial_interaction(this,&
       GeV2_s,&
       x1,&
       x2,&
       pdg_f1,&
       pdg_f2,&
       n1,&
       n2)
    class(muli_type),intent(inout)::this
    real(kind=default),intent(in)::Gev2_s,x1,x2
    integer,intent(in)::pdg_f1,pdg_f2,n1,n2
    real(kind=double)::rnd1,rnd2,time
    if(this%initialized)then
       call cpu_time(time)
       this%init_time=this%init_time-time
       print *,"muli_apply_initial_interaction:"
       print *,"gev2_s=",gev2_s
       print *,"x1=",x1
       print *,"x2=",x2
       print *,"pdg_f1=",pdg_f1
       print *,"pdg_f2=",pdg_f2
       print *,"n1=",n1
       print *,"n2=",n2
       call tao_random_number(this%tao_rnd,rnd1)
       call tao_random_number(this%tao_rnd,rnd2)
       call cpu_time(time)
       this%init_time=this%init_time+time
       call this%beam%apply_initial_interaction(sqrt(gev2_s),x1,x2,pdg_f1,pdg_f2,n1,n2,&
            ! This is a hack: We should give the pt scale of the initial interaction.
            ! Unfortunately, we only know the invarient mass shat. shat/2 is the upper
            ! bound of pt, so we use it for now.
            sqrt(gev2_s)*x1*x2/2D0,&
            rnd1,rnd2)
       this%initial_interaction_given=.true.
    else
       print *,"muli_apply_initial_interaction: call muli_initialize first. STOP"
       STOP
    end if
  end subroutine muli_apply_initial_interaction

  subroutine muli_initialize(this,&
       GeV2_scale_cutoff,&
       gev2_s,&              
       muli_dir,&
       random_seed)
    class(muli_type),intent(out)::this
    real(kind=default),intent(in)::gev2_s,GeV2_scale_cutoff
    character(*),intent(in)::muli_dir
    integer,intent(in),optional::random_seed
    real(kind=double)::time
    logical::exist
    type(muli_dsigma_type)::dsigma_aq
    character(3)::lhapdf_member_c
    call cpu_time(time)
    this%init_time=this%init_time-time
    print *,"muli_initialize: The MULI modules are still not fully populated, so MULI might &
         &generate some dummy values instead of real Monte Carlo generated interactions."
    print *,"Given Parameters:"
    print *,"GeV2_scale_cutoff=",GeV2_scale_cutoff
    print *,"muli_dir=",muli_dir
    print *,"lhapdf_dir=",""
    print *,"lhapdf_file=",muli_default_lhapdf_file
    print *,"lhapdf_member=",muli_default_lhapdf_member
    print *,""
    call transversal_momentum_initialize(this,gev2_s)
    call this%beam%initialize(&
         muli_dir,&
         lhapdf_dir="",&
         lhapdf_file=muli_default_lhapdf_file,&
         lhapdf_member=muli_default_lhapdf_member)
    this%GeV2_scale_cutoff=GeV2_scale_cutoff
    if(present(random_seed))then
       call tao_random_create(this%tao_rnd,random_seed)
    else
       call tao_random_create(this%tao_rnd,1)
    end if
    print *,"looking for previously generated root function..."
    call integer_with_leading_zeros(muli_default_lhapdf_member,3,lhapdf_member_c)
    inquire(file=muli_dir//"/dsigma_"//muli_default_lhapdf_file//".xml",exist=exist)
    if(exist)then
       print *,"found. Starting deserialization..."
       call this%dsigma%deserialize(&
            name="dsigma_"//muli_default_lhapdf_file//"_"//lhapdf_member_c,&
            file=muli_dir//"/dsigma_"//muli_default_lhapdf_file//".xml")
!       call this%dsigma%print_all()
       print *,"done. Starting generation of plots..."
       call this%dsigma%gnuplot(muli_dir)
       print *,"done."
    else
       print *,"No root function found. Starting generation of root function..."
       call dsigma_aq%generate(GeV2_scale_cutoff,gev2_s,this%dsigma)
       print *,"done. Starting serialization of root function..."
       call this%dsigma%serialize(& 
            name="dsigma_"//muli_default_lhapdf_file//"_"//lhapdf_member_c,&
            file=muli_dir//"/dsigma_"//muli_default_lhapdf_file//".xml")
       print *,"done. Starting serialization of generator..."
       call dsigma_aq%serialize(&
            name="dsigma_aq_"//muli_default_lhapdf_file//"_"//lhapdf_member_c,&
            file=muli_dir//"/dsigma_aq_"//muli_default_lhapdf_file//".xml")
       print *,"done. Starting generation of plots..."
       call this%dsigma%gnuplot(muli_dir)
       print *,"done."
    end if
    print *,""
    print *,"looking for previously generated samples..."
    inquire(file=muli_dir//"/samples.xml",exist=exist)
    if(exist)then
       print *,"found. Starting deserialization..."
       call this%samples%deserialize("samples",muli_dir//"/samples.xml")
    else
       print *,"No samples found. Starting with default initialization."
       call this%samples%initialize(4,int_sizes_all,int_all,1D-2)
    end if
    call this%restart()
    this%initialized=.true.
    call cpu_time(time)
    this%init_time=this%init_time+time
  end subroutine muli_initialize

  subroutine muli_finalize(this)
    class(muli_type),intent(inout)::this
    print *,"muli_finalize"
    nullify(this%node)
    call this%dsigma%finalize()
    call this%samples%finalize()
    call this%beam%finalize()
  end subroutine muli_finalize

  subroutine muli_stop_trainer(this)
    class(muli_type),intent(inout)::this
    print *,"muli_stop_trainer: DUMMY!"
  end subroutine muli_stop_trainer

  subroutine muli_reset_timer(this)
    class(muli_type),intent(inout)::this
    this%init_time=0D0
    this%pt_time=0D0
    this%partons_time=0D0
    this%confirm_time=0D0
  end subroutine muli_reset_timer

  subroutine muli_restart(this)
    class(muli_type),intent(inout)::this
    call this%dsigma%get_rightmost(this%node)
    call this%beam%reset()
!    print *,associated(this%node)
!    nullify(this%node)
    this%finished=.false.
    this%process_id=-1
    this%integrand_id=-1
    this%momentum_fractions=[-1D0,-1D0,1D0]
    this%hyperbolic_fractions=[-1D0,-1D0,1D0]
 !   this%start_values%process_id=-1
 !   this%start_values%integrand_id=-1
!    this%start_values%momentum_fractions=[-1D0,-1D0,1D0]
!    this%start_values%hyperbolic_fractions=[-1D0,-1D0,1D0]
    this%start_integrals=[0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0]
  end subroutine muli_restart

  elemental function muli_is_initialized(this) result(res)
    logical::res
    class(muli_type),intent(in) :: this
    res=this%initialized
  end function muli_is_initialized
  
  elemental function muli_is_initial_interaction_given(this) result(res)
    logical::res
    class(muli_type),intent(in) :: this
    res=this%initial_interaction_given
  end function muli_is_initial_interaction_given

  elemental function muli_is_finished(this) result(res)
    logical::res
    class(muli_type),intent(in) :: this
    res=this%finished
  end function muli_is_finished

  subroutine muli_enable_remnant_pdf(this)
    class(muli_type),intent(inout)::this
    this%modify_pdfs=.true.
  end subroutine muli_enable_remnant_pdf

  subroutine muli_disable_remnant_pdf(this)
    class(muli_type),intent(inout)::this
    this%modify_pdfs=.false.
  end subroutine muli_disable_remnant_pdf

  subroutine muli_generate_gev2_pt2(this,gev2_start_scale,gev2_new_scale)
    class(muli_type),intent(inout)::this
    real(kind=default),intent(in)::gev2_start_scale
    real(kind=default),intent(out)::gev2_new_scale
    real(kind=double)::time
    call cpu_time(time)
    this%pt_time=this%pt_time-time
    call this%set_gev2_scale(gev2_start_scale)
    this%start_integrals=this%node%approx_integral(this%get_unit_scale())
    call this%generate_next_scale()
    gev2_new_scale=this%get_gev2_scale()
    call cpu_time(time)
    this%pt_time=this%pt_time+time
  end subroutine muli_generate_gev2_pt2
  
  subroutine muli_generate_flow(this)
    class(muli_type),intent(inout)::this
    integer::rnd
    integer::m,n
    logical,dimension(3)::t
    integer,dimension(4)::tmp_flow
    ! we initialize with zeros. a zero means no line ends here.
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
       if(m<0)then
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
    t=muli_get_state_transformations(this%get_diagram_color_kind(),this%get_lha_flavors())
    ! now we have to apply these transformations to our flow.
    ! (1) means: swap beginning and end of a line. flow is a permutation that maps
    ! ends to their beginnings, so we apply flow to itself:
!!$    print *,"(0)",this%flow
    if(t(1))then
       tmp_flow=this%flow
       this%flow=[0,0,0,0]
       do n=1,4
          if(tmp_flow(n)>0)this%flow(tmp_flow(n))=n
       end do
!!$       print *,"(1)",this%flow
    end if
    if(t(2))then
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
    if(t(3))then
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

  subroutine muli_generate_partons(this,n1,n2,x_proton_1,x_proton_2,pdg_f1,pdg_f2,pdg_f3,pdg_f4)
    class(muli_type),intent(inout)::this
    integer,intent(in)::n1,n2
    real(kind=default),intent(out)::x_proton_1,x_proton_2
    integer,intent(out)::pdg_f1,pdg_f2,pdg_f3,pdg_f4
    integer,dimension(4)::pdg_f
    real(kind=double)::time
    !print *,"muli_generate_partons: n1=",n1," n2=",n2
    this%parton_ids(1)=n1
    this%parton_ids(2)=n2
    call cpu_time(time)
    this%partons_time=this%partons_time-time
    this%mean=this%node%approx_value_n(this%get_unit_scale(),this%integrand_id)
    call sample_inclusive_mcgenerate_hit(&
         this%samples,&
         this%get_unit2_scale(),&
         this%mean,&
         this%integrand_id,&
         this%tao_rnd,&
         this%process_id,&
         this%momentum_fractions)
    !print *,"muli_generate_partons",this%momentum_fractions
    call this%generate_flow()
    if(this%modify_pdfs)then
       call cpu_time(time)
       this%partons_time=this%partons_time+time
       this%confirm_time=this%confirm_time-time
       call this%beam%apply_interaction(this)
       call cpu_time(time)
       this%confirm_time=this%confirm_time+time
       this%partons_time=this%partons_time-time
    end if
    x_proton_1=this%momentum_fractions(1)
    x_proton_2=this%momentum_fractions(2)
    pdg_f=this%get_pdg_flavors()
    pdg_f1=pdg_f(1)
    pdg_f2=pdg_f(2)
    pdg_f3=pdg_f(3)
    pdg_f4=pdg_f(4)
    call cpu_time(time)
    this%partons_time=this%partons_time-time
    call qcd_2_2_print_to_unit(this,output_unit,100_dik,100_dik,100_dik)
  end subroutine muli_generate_partons

  subroutine muli_replace_parton(this,proton_id,old_id,new_id,pdg_f,x_proton,gev_scale)
    class(muli_type),intent(inout)::this
    integer,intent(in)::proton_id,old_id,new_id,pdg_f
    real(kind=default),intent(in)::x_proton,gev_scale
    !print *,"muli_replace_parton(",proton_id,old_id,new_id,pdg_f,x_proton,gev_scale,")"
    if(proton_id==1.or.proton_id==2)then
       call this%beam%replace_parton(proton_id,old_id,new_id,pdg_f,x_proton,gev_scale)
    else
       print *,"muli_replace_parton: proton_id must be 1 or 2, but ",proton_id," was given."
       STOP
    end if
  end subroutine muli_replace_parton

  function muli_get_momentum_pdf(this,x_proton,gev2_scale,n,pdg_f) result(pdf)
    real(kind=double)::pdf
    class(muli_type),intent(in)::this
    real(kind=double),intent(in)::x_proton,gev2_scale
    integer,intent(in)::n,pdg_f
    call this%beam%momentum_pdf(x_proton,gev2_scale,n,pdg_f,pdf)
  end function muli_get_momentum_pdf

  function muli_get_parton_pdf(this,x_proton,gev2_scale,n,pdg_f) result(pdf)
    real(kind=double)::pdf
    class(muli_type),intent(in)::this
    real(kind=double),intent(in)::x_proton,gev2_scale
    integer,intent(in)::n,pdg_f
    call this%beam%parton_pdf(x_proton,gev2_scale,n,pdg_f,pdf)
  end function muli_get_parton_pdf

  subroutine muli_print_timer(this)
    class(muli_type),intent(in) :: this
    print('("Init time:    ",E20.10)'),this%init_time
    print('("PT gen time:  ",E20.10)'),this%pt_time
    print('("Partons time: ",E20.10)'),this%partons_time
    print('("Confirm time: ",E20.10)'),this%confirm_time
    print('("Overall time: ",E20.10)'),this%init_time+this%pt_time+this%partons_time+this%confirm_time
  end subroutine muli_print_timer

  subroutine muli_generate_next_scale(this,integrand_kind)
    class(muli_type),intent(inout)::this
    integer,intent(in),optional::integrand_kind
    real(kind=double)::pts,tmp_pts,rnd
    integer::tmp_int_kind
    class(muli_trapezium_node_class),pointer::tmp_node
      pts=-1D0
      if(present(integrand_kind))then
         call tao_random_number(this%tao_rnd,rnd)
         call generate_single_pts(&
              integrand_kind,&
              this%start_integrals(integrand_kind),&
              this%beam%get_pdf_int_weights(double_pdf_kinds(1:2,integrand_kind)),&
              rnd,&
              this%dsigma,&
              pts,&
              this%node)
      else
         do tmp_int_kind=1,16
            call tao_random_number(this%tao_rnd,rnd)
            call generate_single_pts(&
                 tmp_int_kind,&
                 this%start_integrals(tmp_int_kind),&
                 this%beam%get_pdf_int_weights(double_pdf_kinds(1:2,tmp_int_kind)),&
                 rnd,&
                 this%dsigma,&
                 tmp_pts,&
                 tmp_node)
            if(tmp_pts>pts)then
               pts=tmp_pts
               this%integrand_id=tmp_int_kind
               this%node=>tmp_node
            end if
         end do
      end if
      if(pts>0)then
         call this%set_unit_scale(pts)
      else
         this%finished=.true.
      end if
!      print *,this%finished,this%integrand_id
  contains
    subroutine generate_single_pts(int_kind,start_int,weight,rnd,int_tree,pts,node)
      integer,intent(in)::int_kind
      real(kind=double),intent(in)::start_int,weight,rnd
      type(muli_trapezium_tree_type),intent(in)::int_tree
      real(kind=double),intent(out)::pts
      class(muli_trapezium_node_class),pointer,intent(out)::node
      real(kind=double)::arg
!      print *,int_kind,start_int,weight,rnd
      if(weight>0D0)then
         arg=start_int-log(rnd)/weight
         call int_tree%find_decreasing(arg,int_kind,node)
         if(node%get_l_integral(int_kind)>arg)then
            pts=node%approx_position_by_integral(int_kind,arg)
         else
            pts=-1D0
         end if
      else
         pts=-1D0
      end if
    end subroutine generate_single_pts
  end subroutine muli_generate_next_scale

  subroutine muli_confirm(this)
    class(muli_type),intent(inout) :: this
    this%mean=this%node%approx_value_n(this%get_unit_scale(),this%integrand_id)
    this%start_integrals=this%node%approx_integral(this%get_unit_scale())
  end subroutine muli_confirm

  subroutine muli_generate_samples(this,n_total,n_print,integrand_kind,muli_dir,analyse)
    class(muli_type),intent(inout)::this
    integer(kind=dik),intent(in)::n_total,n_print
    integer,intent(in)::integrand_kind
    character(*),intent(in)::muli_dir
    logical,intent(in)::analyse
    integer(kind=dik)::n_inner

    class(muli_trapezium_node_class),pointer::start_node=>null()
    class(muli_trapezium_node_class),pointer,save::s_node=>null()
    class(muli_trapezium_node_class),pointer,save::node=>null()   

    character(2)::prefix
    integer,save::t_slice,t_region,t_proc,t_subproc,t_max_n=0
    integer(kind=dik)::n_t,n_p,n_m
    integer::n,m,u,unit=0
    integer(kind=dik)::n_tries=0
    integer(kind=dik)::n_hits=0
    integer(kind=dik)::n_over=0
    integer(kind=dik)::n_miss=0
    real(kind=double),save,dimension(3)::cart_hit
    integer,save,dimension(4)::t_i_rnd
    !  integer,save,dimension(5)::r_n_proc
    real(kind=double),dimension(16)::d_rnd
    real(kind=double),save::t_area,t_dddsigma,t_rnd,t_weight,t_arg
    real(kind=double)::mean=0D0
    real(kind=double)::time=0D0
    real(kind=double)::timepa=0D0
    real(kind=double)::timept=0D0
    real(kind=double)::timet=0D0
    real(kind=double)::pts,s_pts=1D0
    real(kind=double)::pts2=1D0
    real(kind=double)::rnd
    logical::running
    character(3)::num
    integer::success=-1
    integer::chain_length=0
    integer::int_kind
    integer::process_id
    real(kind=double),dimension(0:16)::integral
    call this%print_parents()
    n_tries=one
    n_inner=n_total/n_print
    n_t=zero
    print:do while(n_t<n_total)
       call cpu_time(time)
       timet=-time
       n_p=zero
       inner:do while(n_p<n_print)
          chain_length=0
!          print *,"new chain"
          call this%restart()
          this%integrand_id=integrand_kind
          call cpu_time(time)
          timept=timept-time
          call this%generate_next_scale(integrand_kind)
          call cpu_time(time)
          timept=timept+time
          chain:do while(.not.this%is_finished())
             chain_length=chain_length+1
             n_p=n_p+1
             call this%confirm()
             call cpu_time(time)
             timepa=timepa-time
             !             print *,this%get_unit2_scale()
             call sample_inclusive_mcgenerate_hit(&
                  this%samples,&
                  this%get_unit2_scale(),&
                  this%mean,&
                  this%integrand_id,&
                  this%tao_rnd,&
                  this%process_id,&
                  this%momentum_fractions)
             call cpu_time(time)
             timepa=timepa+time
             timept=timept-time
             call this%generate_next_scale(integrand_kind)
             call cpu_time(time)
             timept=timept+time
          end do chain
!          print *,"chain length=",chain_length
       end do inner
       n_t=n_t+n_p
       call this%samples%sum_up()
       call cpu_time(time)
       timet=timet+time
       print *,n_t,"/",n_total
       print *,"time: ",timet
       print *,"pt time: ",timept
       print *,"pa time: ",timepa       
       print *,this%samples%n_tries_sum,this%samples%n_hits_sum,this%samples%n_over_sum
       if(this%samples%n_hits_sum>0)then
          print *,(this%samples%n_hits_sum*10000)/this%samples%n_tries_sum,&
               (this%samples%n_over_sum*10000)/this%samples%n_hits_sum
       else
          print *,"no hits"
       end if
!       print('(7(I11," "),5(E14.7," "))')&
!            ,n_p,n_print,n_tries,n_hits,n_over,&
!            int((n_hits*1D3)/n_tries),int((n_over*1D6)/n_tries),&
!            n_hits/real(n_over),time1,time2,time3,&
!            this%samples%int_kinds(integrand_kind)%overall_boost
    end do print
    call integer_with_leading_zeros(integrand_kind,2,prefix)
    if(analyse)then
       call this%samples%int_kinds(integrand_kind)%analyse(muli_dir,prefix//"_")
       call this%samples%int_kinds(integrand_kind)%serialize(&
            "sample_int_kind_"//prefix,&
            muli_dir//"/sample_int_kind/"//prefix//".xml")
    end if
    call this%samples%int_kinds(integrand_kind)%serialize(&
         "sample_int_kind_"//prefix,&
         muli_dir//"/sample_int_kind/"//prefix//".xml")
  end subroutine muli_generate_samples

  subroutine muli_fake_interaction(this,GeV2_scale,x1,x2,process_id,integrand_id,n1,n2,flow)
    class(muli_type),intent(inout)::this
    real(kind=double),intent(in)::Gev2_scale,x1,x2
    integer,intent(in)::process_id,integrand_id,n1,n2
    integer,dimension(4),intent(in),optional::flow
    call this%set_gev2_scale(Gev2_scale)
    this%process_id=process_id
    this%integrand_id=integrand_id
    this%parton_ids=[n1,n2]
    if(present(flow))then
       this%flow=flow
    else
       this%flow=[0,0,0,0]
    end if
    this%momentum_fractions=[x1,x2,this%get_unit2_scale()]
    call this%beam%apply_interaction(this)
    call this%beam%print_all()
  end subroutine muli_fake_interaction

!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  private procedures  !!
!!!!!!!!!!!!!!!!!!!!!!!!!!

end module muli
