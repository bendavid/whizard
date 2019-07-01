module pythia_dummy
  public :: pyinit
  public :: pygive
  public :: pylist
  public :: pyevnt
  public :: pyp
  public :: upinit
contains
  subroutine pylist (i)
    integer, intent(in) :: i
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine pylist

  subroutine pyinit (frame, beam, target, win)
    character*(*), intent(in) ::  frame, beam, target
    double precision, intent(in) :: win
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine pyinit
  subroutine upinit
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine upinit
  subroutine pygive (chin)
    character chin*(*)
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine pygive
  subroutine pyevnt()
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine pyevnt
  subroutine pyexec()
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine pyexec
  function pyp(I,J)
    integer, intent(in) :: i,j
    double precision :: pyp
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end function pyp
end module pythia_dummy


module ckkw_pseudo_weights
  use kinds, only: default, double !NODEP!

  implicit none

  public :: ckkw_pseudo_shower_weights_t
  public :: ckkw_pseudo_shower_weights_write
  public :: ckkw_pseudo_shower_weights_init

  type :: ckkw_pseudo_shower_weights_t
     real(default) :: alphaS
     real(default), dimension(:), allocatable :: weights
     real(default), dimension(:,:), allocatable :: weights_by_type
  end type ckkw_pseudo_shower_weights_t

contains

  subroutine ckkw_pseudo_shower_weights_init (weights)
    type(ckkw_pseudo_shower_weights_t), intent(out) :: weights
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine ckkw_pseudo_shower_weights_init

  subroutine ckkw_pseudo_shower_weights_write (weights)
    type(ckkw_pseudo_shower_weights_t), intent(in) :: weights
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine ckkw_pseudo_shower_weights_write

end module ckkw_pseudo_weights


module shower_base
  use kinds, only: default !NODEP!
  use constants !NODEP!
  public :: shower_set_minenergy_timelike
  public :: shower_set_d_min_t
  public :: shower_set_d_nf
  public :: shower_set_d_running_alpha_s_fsr
  public :: shower_set_d_running_alpha_s_isr
  public :: shower_set_d_lambda_fsr
  public :: shower_set_d_lambda_isr
  public :: shower_set_d_constantalpha_s
  public :: shower_set_maxz_isr
  public :: shower_set_isr_pt_ordered
  public :: shower_set_isr_angular_ordered
  public :: shower_set_primordial_kt_width
  public :: shower_set_primordial_kt_cutoff
  public :: shower_set_tscalefactor_isr
  public :: shower_set_isr_only_onshell_emitted_partons
  public :: shower_set_pdf_set_and_type

  real(default), public :: D_Min_t = one
  integer, parameter, public :: STRF_NONE = 0
  integer, parameter, public :: STRF_LHAPDF6 = 1
  integer, parameter, public :: STRF_LHAPDF5 = 2
  integer, parameter, public :: STRF_PDF_BUILTIN = 3
  
contains
  subroutine shower_set_minenergy_timelike (input)
    real(default), intent(in) :: input
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine shower_set_minenergy_timelike
  subroutine shower_set_d_min_t (input)
    real(default), intent(in) :: input
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine shower_set_d_min_t
  subroutine shower_set_d_nf (input)
    integer, intent(in) :: input
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine shower_set_d_nf
  subroutine shower_set_isr_pt_ordered (input)
    logical, intent(in) :: input
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine shower_set_isr_pt_ordered
  subroutine shower_set_isr_angular_ordered (input)
    logical, intent(in) :: input
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine shower_set_isr_angular_ordered
  subroutine shower_set_d_lambda_fsr (input)
    real(default), intent(in) :: input
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine shower_set_d_lambda_fsr
  subroutine shower_set_d_lambda_isr (input)
    real(default), intent(in) :: input
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine shower_set_d_lambda_isr
  subroutine shower_set_d_running_alpha_s_fsr (input)
    logical, intent(in) :: input
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine shower_set_d_running_alpha_s_fsr
  subroutine shower_set_d_running_alpha_s_isr (input)
    logical, intent(in) :: input
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine shower_set_d_running_alpha_s_isr
  subroutine shower_set_d_constantalpha_s (input)
    real(default), intent(in) :: input
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine shower_set_d_constantalpha_s
  subroutine shower_set_maxz_isr (input)
    real(default), intent(in) :: input
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine shower_set_maxz_isr
  subroutine shower_set_primordial_kt_width (input)
    real(default), intent(in) :: input
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine shower_set_primordial_kt_width
  subroutine shower_set_primordial_kt_cutoff (input)
    real(default), intent(in) :: input
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine shower_set_primordial_kt_cutoff
  subroutine shower_set_tscalefactor_isr (input)
    real(default), intent(in) :: input
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine shower_set_tscalefactor_isr
  subroutine shower_set_isr_only_onshell_emitted_partons (input)
    logical, intent(in) :: input
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine shower_set_isr_only_onshell_emitted_partons

  subroutine shower_set_pdf_set_and_type(set,type)
    integer, intent(in) :: set, type
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine shower_set_pdf_set_and_type
end module shower_base

module shower_partons
  use kinds, only: default !NODEP!
  use constants !NODEP!
  use lorentz !NODEP!

  public :: parton_t
  public :: parton_pointer_t
  public :: parton_copy
  public :: parton_get_costheta
  public :: parton_get_costheta_correct
  public :: parton_get_costheta_motherfirst
  public :: parton_get_beta
  public :: parton_write
  public :: parton_is_final
  public :: parton_is_branched
  public :: parton_set_simulated
  public :: parton_is_simulated
  public :: parton_get_momentum
  public :: parton_set_momentum
  public :: parton_set_energy
  public :: parton_get_energy
  public :: parton_set_parent
  public :: parton_get_parent
  public :: parton_set_initial
  public :: parton_get_initial
  public :: parton_set_child
  public :: parton_get_child
  public :: parton_is_quark
  public :: parton_is_squark
  public :: parton_is_gluon
  public :: parton_is_gluino
  public :: parton_is_hadron
  public :: parton_is_colored
  public :: parton_p4square
  public :: parton_p3square
  public :: parton_p3abs
  public :: parton_mass
  public :: parton_mass_squared
  public :: P_prt_to_child1
  public :: thetabar
  public :: parton_apply_z
  public :: parton_apply_costheta
  public :: parton_apply_lorentztrafo
  public :: parton_apply_lorentztrafo_recursive
  public :: parton_generate_ps
  public :: parton_generate_ps_ini
  public :: parton_next_t_ana
  public :: parton_simulate_stept
  public :: maxzz

  type :: parton_t
     integer :: nr = 0
     integer :: type = 0
     type(vector4_t) :: momentum = vector4_null
     real(default) :: t  = zero
     real(default) :: scale = zero
     real(default) :: z = zero
     real(default) :: costheta = zero
     real(default) :: x = zero
     logical :: simulated = .false.
     logical :: belongstoFSR = .true.
     logical :: belongstointeraction = .false.
     type(parton_t), pointer :: parent => null ()
     type(parton_t), pointer :: child1 => null ()
     type(parton_t), pointer :: child2 => null ()
     type(parton_t), pointer :: initial => null ()
     integer :: c1 = 0, c2 = 0
     integer :: aux_pt = 0
     integer :: ckkwlabel = 0
     real(default) :: ckkwscale = zero
     integer :: ckkwtype = -1
     integer :: interactionnr = 0
  end type parton_t

  type :: parton_pointer_t
     type(parton_t), pointer :: p => null ()
  end type parton_pointer_t


contains
  subroutine parton_set_simulated (prt, sim)
    type(parton_t), intent(inout) :: prt
    logical, intent(in), optional :: sim
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine parton_set_simulated
  subroutine parton_set_momentum (prt, EE, ppx, ppy, ppz)
    type(parton_t), intent(inout) :: prt
    real(default), intent(in) :: EE, ppx, ppy, ppz
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine parton_set_momentum
  subroutine parton_set_initial (prt, initial)
    type(parton_t), intent(inout) :: prt
    type(parton_t), intent(in) , target :: initial
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine parton_set_initial
end module shower_partons

module shower_core
  use kinds, only: default
  use shower_base
  use shower_partons
  use ckkw_pseudo_weights
  use pythia_dummy
  use lhapdf

  public :: shower_interaction_t
  public :: shower_t
  public :: shower_get_final_partons
  public :: shower_interaction_get_shat
  public :: shower_interaction_get_s

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


  ! real(default), parameter :: alphasmax = one
  ! real(default), parameter :: xpdfmax = 10._default
  real(default), save :: alphasxpdfmax = 12._default


contains
    function shower_get_next_free_nr (shower) result(next_number)
      class(shower_t), intent(inout) :: shower
      integer :: next_number
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop
    end function shower_get_next_free_nr
    function shower_generate_next_isr_branching (shower) result (next_brancher)
      class(shower_t), intent(inout) :: shower
      type(parton_pointer_t) :: next_brancher
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop
    end function shower_generate_next_isr_branching
    function shower_generate_next_isr_branching_veto (shower) &
         result (next_brancher)
      class(shower_t), intent(inout) :: shower
      type(parton_pointer_t) :: next_brancher
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop
    end function shower_generate_next_isr_branching_veto
    subroutine shower_generate_fsr_for_partons_emitted_in_isr (shower)
      class(shower_t), intent(inout) :: shower
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop
    end subroutine shower_generate_fsr_for_partons_emitted_in_isr
    subroutine interaction_generate_primordial_kt (interaction)
      type(shower_interaction_t), intent(inout) :: interaction
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop
    end subroutine interaction_generate_primordial_kt
    subroutine shower_generate_primordial_kt (shower)
      class(shower_t), intent(inout) :: shower
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop
    end subroutine shower_generate_primordial_kt
    subroutine shower_interaction_generate_fsr_2ton (shower, interaction)
      class(shower_t), intent(inout) :: shower
      type(shower_interaction_t), intent(inout) :: interaction
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop
    end subroutine shower_interaction_generate_fsr_2ton
    subroutine shower_execute_next_isr_branching (shower, prtp)
      class(shower_t), intent(inout) :: shower
      type(parton_pointer_t), intent(inout) :: prtp
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop
    end subroutine shower_execute_next_isr_branching
    subroutine shower_boost_to_labframe (shower)
      class(shower_t), intent(inout) :: shower
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop
    end subroutine shower_boost_to_labframe
    subroutine shower_get_final_colored_ME_partons(shower, partons)
      class(shower_t), intent(in) :: shower
      type(parton_pointer_t), dimension(:), allocatable, intent(inout) :: &
           partons
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop
    end subroutine shower_get_final_colored_ME_partons
    subroutine shower_get_final_partons (shower, partons, include_remnants)
      type(shower_t), intent(in) :: shower
      type(parton_pointer_t), dimension(:), allocatable, intent(inout) :: partons
      logical, intent(in), optional :: include_remnants
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop
    end subroutine shower_get_final_partons
    subroutine shower_write (shower)
      class(shower_t), intent(inout) :: shower
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop
    end subroutine shower_write
    subroutine shower_sort_partons (shower)
      class(shower_t), intent(inout) :: shower
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop
    end subroutine shower_sort_partons
    subroutine shower_add_interaction_2ton (shower, partons)
      class(shower_t), intent(inout) :: shower
      type(parton_pointer_t), intent(inout), dimension(:), allocatable :: &
           partons
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop
    end subroutine shower_add_interaction_2ton
    subroutine shower_add_interaction_2ton_CKKW &
         (shower, partons, ckkw_pseudo_weights)
      class(shower_t), intent(inout) :: shower
      type(parton_pointer_t), intent(in), dimension(:), allocatable :: partons
      type(ckkw_pseudo_shower_weights_t), intent(in) :: ckkw_pseudo_weights
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop
    end subroutine shower_add_interaction_2ton_CKKW
    subroutine shower_simulate_no_isr_shower (shower)
      class(shower_t), intent(inout) :: shower
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop
    end subroutine shower_simulate_no_isr_shower
    subroutine shower_simulate_no_fsr_shower (shower)
      class(shower_t), intent(inout) :: shower
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop
    end subroutine shower_simulate_no_fsr_shower
    subroutine shower_update_beamremnants (shower)
      class(shower_t), intent(in) :: shower
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop
    end subroutine shower_update_beamremnants
    subroutine shower_set_next_color_nr (shower, index)
      class(shower_t), intent(in) :: shower
      integer, intent(in) :: index
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop
    end subroutine shower_set_next_color_nr
    subroutine shower_create (shower, pdf)
      class(shower_t), intent(inout) :: shower
      type(lhapdf_pdf_t), intent(in), target :: pdf
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop
    end subroutine shower_create
    subroutine shower_final (shower)
      class(shower_t), intent(inout) :: shower
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop
    end subroutine shower_final
    subroutine shower_write_lhef (shower, unit)
      class(shower_t), intent(in) :: shower
      integer, intent(in), optional :: unit
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop
    end subroutine shower_write_lhef
    function shower_interaction_get_s (interaction) result(s)
      type(shower_interaction_t), intent(in) :: interaction
      real(default) :: s
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop
    end function shower_interaction_get_s
    function shower_get_ISR_scale (shower) result (scale)
      class(shower_t), intent(in) :: shower
      real(default) :: scale
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop
    end function shower_get_ISR_scale
    function shower_get_next_color_nr (shower) result(next_color)
      class(shower_t), intent(inout) :: shower
      integer :: next_color
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop
    end function shower_get_next_color_nr
    subroutine shower_set_max_isr_scale (shower, newscale)
      class(shower_t), intent(inout) :: shower
      real(default), intent(in) :: newscale
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop
    end subroutine shower_set_max_isr_scale
    subroutine shower_add_child (shower, prt, child)
      class(shower_t), intent(inout) :: shower
      type(parton_t), intent(in) :: prt
      integer, intent(in) :: child
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop
    end subroutine shower_add_child
    subroutine shower_add_parent (shower, prt)
      class(shower_t), intent(inout) :: shower
      type(parton_t), intent(in) :: prt
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop
    end subroutine shower_add_parent
    subroutine shower_get_pdf (shower, mother, x, Q2, daughter)
      class(shower_t), intent(inout) :: shower
      integer, intent(in) :: mother, daughter
      real(default), intent(in) :: x, Q2
    end subroutine shower_get_pdf
    subroutine shower_get_xpdf (shower, mother, x, Q2, daughter)
      class(shower_t), intent(inout) :: shower
      integer, intent(in) :: mother, daughter
      real(default), intent(in) :: x, Q2
    end subroutine shower_get_xpdf    
    subroutine shower_pdf_func (shower, set, x, q2, f)
      class(shower_t), intent(inout) :: shower
      integer, intent(in) :: set
      real(default), intent(in) :: x, q2
      real(default), dimension(-6:6), intent(out) :: f
    end subroutine shower_pdf_func
end module shower_core

module shower_topythia
  use kinds, only: default !NODEP!
  use shower_base
  use shower_partons
  use shower_core
  public :: shower_converttopythia
contains
  subroutine shower_converttopythia (shower)
    type(shower_t), intent(in) :: shower
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
    end subroutine shower_converttopythia
end module shower_topythia

module mlm_matching
  use kinds, only: default !NODEP!
  use kinds, only: double !NODEP!
  use constants !NODEP!
  use lorentz !NODEP!

  public :: mlm_matching_data_t
  public :: mlm_matching_settings_t
  public :: mlm_matching_settings_write
  public :: mlm_matching_data_write
  public :: mlm_matching_data_final
  public :: mlm_matching_apply

  type :: mlm_matching_data_t
     logical :: is_hadron_collision = .false.
     type(vector4_t), dimension(:), allocatable, public :: P_ME
     type(vector4_t), dimension(:), allocatable, public :: P_PS
     type(vector4_t), dimension(:), allocatable, private :: JETS_ME
     type(vector4_t), dimension(:), allocatable, private :: JETS_PS
  end type mlm_matching_data_t

  type :: mlm_matching_settings_t
     real(default) :: mlm_Qcut_ME = one
     real(default) :: mlm_Qcut_PS = one
     real(default) :: mlm_ptmin, mlm_etamax, mlm_Rmin, mlm_Emin
     real(default) :: mlm_ETclusfactor = 0.2_default
     real(default) :: mlm_ETclusminE = five
     real(default) :: mlm_etaclusfactor = one
     real(default) :: mlm_Rclusfactor = one
     real(default) :: mlm_Eclusfactor = one
     integer :: kt_imode_hadronic = 4313
     integer :: kt_imode_leptonic = 1111
     integer :: mlm_nmaxMEjets = 0
  end type mlm_matching_settings_t


contains

  subroutine mlm_matching_settings_write (settings, unit)
    type(mlm_matching_settings_t), intent(in) :: settings
    integer, intent(in), optional :: unit
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine mlm_matching_settings_write

  subroutine mlm_matching_data_final (data)
    type(mlm_matching_data_t), intent(inout) :: data
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine mlm_matching_data_final

  subroutine mlm_matching_apply (data, settings, vetoed)
    type(mlm_matching_data_t), intent(inout) :: data
    type(mlm_matching_settings_t), intent(in) :: settings
    logical, intent(out) :: vetoed
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine mlm_matching_apply
end module mlm_matching



module ckkw_matching
  use kinds, only: default !NODEP!
  use kinds, only: double !NODEP!
  use shower_core
  use ckkw_pseudo_weights

  implicit none
  private

  public :: ckkw_matching_settings_t
  public :: ckkw_matching_apply

  type :: ckkw_matching_settings_t
     real(default) :: alphaS = 0.118_default
     real(default) :: Qmin = one
     integer :: n_max_jets = 0
  end type ckkw_matching_settings_t


contains

  subroutine ckkw_matching_apply (shower, settings, weights, veto)
    type(shower_t), intent(inout) :: shower
    type(ckkw_matching_settings_t), intent(in) :: settings
    type(ckkw_pseudo_shower_weights_t), intent(in) :: weights
    logical, intent(out) :: veto
    veto = .false.
    write (0, "(A)")  "**************************************************************"
    write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "**************************************************************"
    stop
  end subroutine ckkw_matching_apply

end module ckkw_matching
