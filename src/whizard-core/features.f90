module features

  use string_utils, only: lower_case
  use system_dependencies, only: WHIZARD_VERSION
  use kinds, only: default
  use system_dependencies, only: openmp_is_active
  use system_dependencies, only: GOSAM_AVAILABLE
  use system_dependencies, only: OPENLOOPS_AVAILABLE
  use system_dependencies, only: LHAPDF5_AVAILABLE
  use system_dependencies, only: LHAPDF6_AVAILABLE  
  use system_dependencies, only: HOPPET_AVAILABLE
  use jets, only: fastjet_available
  use system_dependencies, only: PYTHIA6_AVAILABLE
  use system_dependencies, only: PYTHIA8_AVAILABLE
  use hepmc_interface, only: hepmc_is_available
  use lcio_interface, only: lcio_is_available
  use system_dependencies, only: EVENT_ANALYSIS

  implicit none
  private

  public :: print_features

contains
  
  subroutine print_features ()
    print "(A)", "WHIZARD " // WHIZARD_VERSION
    print "(A)", "Build configuration:"
    call print_check ("precision")
    print "(A)", "Optional features available in this build:"
    call print_check ("OpenMP")
    call print_check ("GoSam")
    call print_check ("OpenLoops")
    call print_check ("LHAPDF")
    call print_check ("HOPPET")
    call print_check ("fastjet")
    call print_check ("Pythia6")
    call print_check ("Pythia8")
    call print_check ("StdHEP")
    call print_check ("HepMC")
    call print_check ("LCIO")
    call print_check ("MetaPost")
  end subroutine print_features
  
  subroutine check (feature, recognized, result, help)
    character(*), intent(in) :: feature
    logical, intent(out) :: recognized
    character(*), intent(out) :: result, help
    recognized = .true.
    result = "no"
    select case (lower_case (trim (feature)))
    case ("precision")
       write (result, "(I0)")  precision (1._default)
       help = "significant decimals of real/complex numbers"
    case ("openmp")
       if (openmp_is_active ()) then
          result = "yes"
       end if
       help = "OpenMP parallel execution"
    case ("gosam")
       if (GOSAM_AVAILABLE) then
          result = "yes"
       end if
       help = "external NLO matrix element provider"
    case ("openloops")
       if (OPENLOOPS_AVAILABLE) then
          result = "yes"
       end if
       help = "external NLO matrix element provider"
    case ("lhapdf")
       if (LHAPDF5_AVAILABLE) then
          result = "v5"
       else if (LHAPDF6_AVAILABLE) then
          result = "v6"
       end if
       help = "PDF library"
    case ("hoppet")
       if (HOPPET_AVAILABLE) then
          result = "yes"
       end if
       help = "PDF evolution package"
    case ("fastjet")
       if (fastjet_available ()) then
          result = "yes"
       end if
       help = "jet-clustering package"
    case ("pythia6")
       if (PYTHIA6_AVAILABLE) then
          result = "yes"
       end if
       help = "direct access for shower/hadronization"
    case ("pythia8")
       if (PYTHIA8_AVAILABLE) then
          result = "yes"
       end if
       help = "direct access for shower/hadronization"
    case ("stdhep")
       result = "yes"
       help = "event I/O format"
    case ("hepmc")
       if (hepmc_is_available ()) then
          result = "yes"
       end if
       help = "event I/O format"
    case ("lcio")
       if (lcio_is_available ()) then
          result = "yes"
       end if
       help = "event I/O format"
    case ("metapost")
       result = EVENT_ANALYSIS
       help = "graphical event analysis via LaTeX/MetaPost"
    case default
       recognized = .false.
    end select
  end subroutine check
  
  subroutine print_check (feature)
    character(*), intent(in) :: feature
    character(16) :: f
    logical :: recognized
    character(10) :: result
    character(48) :: help
    call check (feature, recognized, result, help)
    if (.not. recognized) then
       result = "unknown"
       help = ""
    end if
    f = feature
    print "(2x,A,1x,A,'(',A,')')", f, result, trim (help)
  end subroutine print_check
    

end module features
