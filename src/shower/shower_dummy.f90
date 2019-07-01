! Dummy replacement routines

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

module shower_basics_module
  use kinds, only: default
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
end module shower_basics_module

module shower_parton_module
  use kinds, only: default
  use lorentz !NODEP!
  public :: parton_t, parton_pointer_t
  type :: parton_t
!     private
     integer :: nr=0 
     integer :: typ=0  
     type(vector4_t) :: momentum = vector4_null
     real(default) :: t  = 0._default
     real(default) :: scale = 0._default  
     real(default) :: z = 0._default
     real(default) :: costheta = 0._default
     real(default) :: x=0._default
     logical :: simulated=.false.
     logical :: belongstoFSR=.true.
     logical :: belongstointeraction=.false.
     type(parton_t), pointer :: parent => null ()
     type(parton_t), pointer :: child1 => null ()
     type(parton_t), pointer :: child2 => null ()
     type(parton_t), pointer :: initial => null ()
     integer :: c1 = 0, c2 = 0
     integer :: aux_pt = 0             
     integer :: interactionnr = 0
  end type parton_t
  type :: parton_pointer_t
     type(parton_t), pointer :: p => null ()
  end type parton_pointer_t
end module shower_parton_module

module shower_module
  use kinds, only: default
  use shower_basics_module
  use shower_parton_module
  use pythia_dummy
  public :: shower_t
  public :: shower_get_next_free_nr
  public :: shower_generate_next_isr_branching
  public :: shower_generate_next_isr_branching_veto
  public :: shower_generate_fsr_for_partons_emitted_in_isr
  public :: interaction_generate_primordial_kt
  public :: shower_generate_primordial_kt
  public :: shower_interaction_generate_fsr2ton
  public :: shower_set_next_color_nr
  public :: shower_execute_next_isr_branching
  public :: shower_update_beamremnants
  public :: shower_add_interaction2ton
  public :: shower_simulate_no_isr_shower
  public :: shower_simulate_no_fsr_shower
  public :: shower_boost_to_labframe
  public :: shower_get_final_partons
  public :: shower_print
  public :: shower_create
  Public :: shower_final
  public :: shower_write_lhef
  type :: my_interaction_t
     type(parton_pointer_t) :: in1, in2
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
contains
    function shower_get_next_free_nr(shower) result(next_number)
      type(shower_t), intent(inout) :: shower
      integer :: next_number
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop      
    end function shower_get_next_free_nr
    function shower_generate_next_isr_branching (shower) result (next_brancher)
      type(shower_t), intent(inout) :: shower
      type(parton_pointer_t) :: next_brancher
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop      
    end function shower_generate_next_isr_branching
    function shower_generate_next_isr_branching_veto (shower) result (next_brancher)
      type(shower_t), intent(inout) :: shower
      type(parton_pointer_t) :: next_brancher
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop      
    end function shower_generate_next_isr_branching_veto
    subroutine shower_generate_fsr_for_partons_emitted_in_isr (shower)
      type(shower_t), intent(inout) :: shower
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop      
    end subroutine shower_generate_fsr_for_partons_emitted_in_isr
    subroutine interaction_generate_primordial_kt (interaction)
      type(my_interaction_t), intent(inout) :: interaction
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop            
    end subroutine interaction_generate_primordial_kt
    subroutine shower_generate_primordial_kt (shower)
      type(shower_t), intent(inout) :: shower
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop            
    end subroutine shower_generate_primordial_kt
    subroutine shower_interaction_generate_fsr2ton (shower, interaction)
      type(shower_t), intent(inout) :: shower
      type(my_interaction_t), intent(inout) :: interaction
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop      
    end subroutine shower_interaction_generate_fsr2ton
    subroutine shower_execute_next_isr_branching (shower, prtp)
      type(shower_t), intent(inout) :: shower
      type(parton_pointer_t), intent(inout) :: prtp
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop      
    end subroutine shower_execute_next_isr_branching
    subroutine shower_boost_to_labframe (shower)
      type(shower_t), intent(inout) :: shower
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop      
    end subroutine shower_boost_to_labframe
    subroutine shower_get_final_colored_ME_partons(shower, partons)
      type(shower_t), intent(in) :: shower
      type(parton_pointer_t), dimension(:), allocatable, intent(inout) :: partons
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
    subroutine shower_print (shower)
      type(shower_t), intent(inout) :: shower
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop      
    end subroutine shower_print
    subroutine shower_add_interaction2ton (shower, partons)
      type(shower_t), intent(inout) :: shower
      type(parton_pointer_t), intent(inout), dimension(:), allocatable :: partons
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop      
    end subroutine shower_add_interaction2ton
    subroutine shower_simulate_no_isr_shower (shower)
      type(shower_t), intent(inout) :: shower
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop      
    end subroutine shower_simulate_no_isr_shower
    subroutine shower_simulate_no_fsr_shower (shower)
      type(shower_t), intent(inout) :: shower
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop      
    end subroutine shower_simulate_no_fsr_shower
    subroutine shower_update_beamremnants (shower)
      type(shower_t), intent(in) :: shower
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop      
    end subroutine shower_update_beamremnants
    subroutine shower_set_next_color_nr (shower, index)
      type(shower_t), intent(in) :: shower
      integer, intent(in) :: index
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop      
    end subroutine shower_set_next_color_nr
    subroutine shower_create (shower)
      type(shower_t), intent(inout) :: shower
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop      
    end subroutine shower_create
    subroutine shower_final (shower)
      type(shower_t), intent(inout) :: shower
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop      
    end subroutine shower_final
    subroutine shower_write_lhef (shower, unit)
      type(shower_t), intent(in) :: shower
      integer, intent(in), optional :: unit
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop      
    end subroutine shower_write_lhef
end module shower_module

module shower_topythia_module
  use shower_module
  public :: shower_create
contains
  SUBROUTINE shower_converttopythia(shower)
    TYPE(shower_t), INTENT(in) :: shower
      write (0, "(A)")  "**************************************************************"
      write (0, "(A)")  "*** Error: Shower has not been enabled, WHIZARD terminates ***"
      write (0, "(A)")  "**************************************************************"
      stop      
    end subroutine shower_converttopythia
end module shower_topythia_module

module mlm_matching_module
  use kinds, only: default, double !NODEP!
  use lorentz !NODEP!

  public :: mlm_matching_data_t
  public :: mlm_matching_settings_t
  public :: mlm_matching_settings_write
  public :: mlm_matching_data_final
  public :: mlm_matching

  type :: mlm_matching_data_t
     logical :: is_hadron_collision = .false.
     ! the (colored) partons' momenta
     type(vector4_t), dimension(:), allocatable, public :: P_ME
     type(vector4_t), dimension(:), allocatable, public :: P_PS

     ! the jets' momenta
     type(vector4_t), dimension(:), allocatable, private :: JETS_ME
     type(vector4_t), dimension(:), allocatable, private :: JETS_PS
  end type mlm_matching_data_t

  type :: mlm_matching_settings_t
     real(kind=default) :: mlm_Qcut_ME = 1._default
     real(kind=default) :: mlm_Qcut_PS = 1._default
     real(kind=default) :: mlm_ptmin, mlm_etamax, mlm_Rmin, mlm_Emin
     real(kind=default) :: mlm_ETclusfactor = 0.2_default
     real(kind=default) :: mlm_ETclusminE = 5._default
     real(kind=default) :: mlm_etaclusfactor = 1._default
     real(kind=default) :: mlm_Rclusfactor = 1._default
     real(kind=default) :: mlm_Eclusfactor = 1._default

     integer :: kt_imode_hadronic = 4313
     integer :: kt_imode_leptonic = 1111
     integer :: mlm_nmaxMEjets = 0
  end type mlm_matching_settings_t

  contains

  subroutine mlm_matching_settings_write(mlm_matching_settings, unit)
    type(mlm_matching_settings_t), intent(in) :: mlm_matching_settings
    integer, intent(in), optional :: unit
    write (0, "(A)")  "****************************************************************"
    write (0, "(A)")  "*** Error: Matching has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "****************************************************************"
    stop      
  end subroutine mlm_matching_settings_write

  subroutine mlm_matching_data_final(mlm_matching_data)
    type(mlm_matching_data_t), intent(inout) :: mlm_matching_data
    write (0, "(A)")  "****************************************************************"
    write (0, "(A)")  "*** Error: Matching has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "****************************************************************"
    stop      
  end subroutine mlm_matching_data_final

subroutine mlm_matching(mlm_matching_data, mlm_matching_settings, vetoed)
    type(mlm_matching_data_t), intent(inout) :: mlm_matching_data
    type(mlm_matching_settings_t), intent(in) :: mlm_matching_settings
    logical, intent(out) :: vetoed
    write (0, "(A)")  "****************************************************************"
    write (0, "(A)")  "*** Error: Matching has not been enabled, WHIZARD terminates ***"
    write (0, "(A)")  "****************************************************************"
    stop      
  end subroutine mlm_matching
end module mlm_matching_module
