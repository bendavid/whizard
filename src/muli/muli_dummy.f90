!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! Dummy replacement routines for muli
!!!!!! To be replaced by something like
!     write (0, "(A)")  "**************************************************************"
!     write (0, "(A)")  "**** Error: MPI has not been enabled, WHIZARD terminates ****"
!     write (0, "(A)")  "**************************************************************"
!     stop      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module muli
  use kinds
  use, intrinsic :: iso_fortran_env
  implicit none
  type::qcd_2_2_type
     private
     integer::process_id=-1
     integer::integrand_id=-1
     integer,dimension(2)::parton_ids=[0,0]
     integer,dimension(4)::flow=[0,0,0,0]
     real(kind=double),dimension(3)::momentum_fractions=[-1D0,-1D0,-1D0]
     real(kind=double),dimension(3)::hyperbolic_fractions=[-1D0,-1D0,-1D0]
  end type qcd_2_2_type
  type::muli_type
     real(kind=default)::GeV2_scale_cutoff
     logical::initialized=.false.
   contains
     procedure,public::initialize=>muli_initialize
     procedure,public::apply_initial_interaction=>muli_apply_initial_interaction
     procedure,public::is_initialized=>muli_is_initialized
     procedure,public::finalize=>muli_finalize
     procedure,public::generate_gev2_pt2=>muli_generate_gev2_pt2
     procedure,public::generate_partons=>muli_generate_partons
     procedure,public::replace_parton=>muli_replace_parton
     procedure,public::get_parton_pdf=>muli_get_parton_pdf
     procedure,public::get_momentum_pdf=>muli_get_momentum_pdf
     procedure,public::restart=>muli_restart
     procedure,public::get_color_correlations=>qcd_2_2_get_color_correlations
  end type muli_type
contains
  subroutine muli_initialize(this,GeV2_scale_cutoff,GeV2_s, &
       muli_dir, random_seed)
    class(muli_type),intent(out)::this
    real(kind=default),intent(in)::GeV2_scale_cutoff,GeV2_s
    character(*),intent(in)::muli_dir
    integer,intent(in),optional::random_seed
    this%GeV2_scale_cutoff=GeV2_scale_cutoff
  end subroutine muli_initialize
  subroutine muli_finalize(this)
    class(muli_type),intent(out)::this
  end subroutine muli_finalize
  elemental function muli_is_initialized(this) result(res)
    logical::res
    class(muli_type),intent(in) :: this
    res=this%initialized
  end function muli_is_initialized
  subroutine muli_generate_gev2_pt2(this,gev2_start_scale,gev2_new_scale)
    class(muli_type),intent(inout)::this
    real(kind=default),intent(in)::gev2_start_scale
    real(kind=default),intent(out)::gev2_new_scale
    gev2_new_scale=gev2_start_scale/1D2
    if(gev2_new_scale<this%GeV2_scale_cutoff)gev2_new_scale=0
  end subroutine muli_generate_gev2_pt2
  subroutine muli_generate_partons(this,n1,n2,x_proton_1,x_proton_2,pdg_f1,pdg_f2,pdg_f3,pdg_f4)
    class(muli_type),intent(inout)::this
    integer,intent(in)::n1,n2
    real(kind=default),intent(out)::x_proton_1,x_proton_2
    integer,intent(out)::pdg_f1,pdg_f2,pdg_f3,pdg_f4
    x_proton_1=1D-1
    x_proton_2=1D-1
    pdg_f1=21
    pdg_f2=21
    pdg_f3=21
    pdg_f4=21
  end subroutine muli_generate_partons
  subroutine muli_replace_parton(this,proton_id,old_id,new_id,pdg_f,x_proton,gev_scale)
    class(muli_type),intent(inout)::this
    integer,intent(in)::proton_id,old_id,new_id,pdg_f
    real(kind=default),intent(in)::x_proton,gev_scale
  end subroutine muli_replace_parton
  function muli_get_momentum_pdf(this,x_proton,gev2_scale,n,pdg_f) result(pdf)
    real(kind=double)::pdf
    class(muli_type),intent(in)::this
    real(kind=double),intent(in)::x_proton,gev2_scale
    integer,intent(in)::n,pdg_f
    pdf = 1._double
  end function muli_get_momentum_pdf
  function muli_get_parton_pdf(this,x_proton,gev2_scale,n,pdg_f) result(pdf)
    real(kind=double)::pdf
    class(muli_type),intent(in)::this
    real(kind=double),intent(in)::x_proton,gev2_scale
    integer,intent(in)::n,pdg_f
    pdf = 1._double
  end function muli_get_parton_pdf
  subroutine muli_restart(this)
    class(muli_type),intent(inout)::this
  end subroutine muli_restart
  subroutine muli_apply_initial_interaction(this,&
       GeV2_s,&
       x1,&
       x2,&
       pdg_f1,&
       pdg_f2,&
       n1,&
       n2)
    class(muli_type),intent(inout)::this
    real(kind=default),intent(in)::GeV2_s,x1,x2
    integer,intent(in)::pdg_f1,pdg_f2,n1,n2
  end subroutine muli_apply_initial_interaction
  subroutine qcd_2_2_get_color_correlations(this,start_index,final_index,flow)
    class(muli_type),intent(in)::this
    integer,intent(in)::start_index
    integer,intent(out)::final_index
    integer,dimension(2,4),intent(out)::flow
    integer::pos,f_end,f_beginning
    final_index=start_index
    flow=reshape([0,0,0,0,0,0,0,0],[2,4])
  end subroutine qcd_2_2_get_color_correlations
end module muli
