! WHIZARD 2.2.7 Aug 11 2015
! 
! Copyright (C) 1999-2015 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Christian Weiss <christian.weiss@desy.de>
!     and Hans-Werner Boschmann, Felix Braam, 
!     Sebastian Schmidt, Daniel Wiesler 
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

module radiation_generator_uti

  use iso_varying_string, string_t => varying_string
  use format_utils, only: write_separator
  use os_interface
  use pdg_arrays
  use models

  use radiation_generator

  implicit none
  private

  public :: radiation_generator_1
  public :: radiation_generator_2

contains

  subroutine radiation_generator_1 (u)
    integer, intent(in) :: u
    type(radiation_generator_t) :: generator
    type(pdg_array_t), dimension(:), allocatable :: pdg_in, pdg_out
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: radiation_model => null ()
    
    write (u, "(A)") "* Test output: radiation_generator_1"
    write (u, "(A)") "* Purpose: Create N+1-particle flavor structures from predefined N-particle flavor structures"
    write (u, "(A)") "* One additional strong coupling, no additional electroweak coupling"
    write (u, "(A)")
    write (u, "(A)") "* Loading radiation model: SM_rad.mdl"

    call syntax_model_file_init ()
    call os_data_init (os_data)
    call model_list%read_model &
       (var_str ("SM_rad"), var_str ("SM_rad.mdl"), &
        os_data, radiation_model)
    call generator%init_radiation_model (radiation_model)
    write (u, "(A)") "* Success"    

    allocate (pdg_in (2))
    pdg_in(1) = 11; pdg_in(2) = -11    
    
    write (u, "(A)") "* Start checking processes"
    call write_separator (u)    

    write (u, "(A)") "* Process 1: Quark-antiquark production"
    allocate (pdg_out(2))
    pdg_out(1) = 2; pdg_out(2) = -2
    call test_process (generator, pdg_in, pdg_out, u)
    deallocate (pdg_out)

    write (u, "(A)") "* Process 2: Quark-antiquark production with additional gluon"
    allocate (pdg_out(3))
    pdg_out(1) = 2; pdg_out(2) = -2; pdg_out(3) = 21 
    call test_process (generator, pdg_in, pdg_out, u)
    deallocate (pdg_out)

    write (u, "(A)") "* Process 3: Z + jets"
    allocate (pdg_out(3))
    pdg_out(1) = 2; pdg_out(2) = -2; pdg_out(3) = 23
    call test_process (generator, pdg_in, pdg_out, u)
    deallocate (pdg_out)
    
    write (u, "(A)") "* Process 4: Top Decay"
    allocate (pdg_out(4))
    pdg_out(1) = 24; pdg_out(2) = -24
    pdg_out(3) = 5; pdg_out(4) = -5
    call test_process (generator, pdg_in, pdg_out, u)
    deallocate (pdg_out)

    write (u, "(A)") "* Process 5: Production of four quarks"
    allocate (pdg_out(4))
    pdg_out(1) = 2; pdg_out(2) = -2;
    pdg_out(3) = 2; pdg_out(4) = -2
    call test_process (generator, pdg_in, pdg_out, u)
    deallocate (pdg_out); deallocate (pdg_in)

    write (u, "(A)") "* Process 6: Drell-Yan lepto-production"
    allocate (pdg_in (2)); allocate (pdg_out (2))
    pdg_in(1) = 2; pdg_in(2) = -2
    pdg_out(1) = 11; pdg_out(2) = -11
    call test_process (generator, pdg_in, pdg_out, u)
    deallocate (pdg_out); deallocate (pdg_in)

    write (u, "(A)") "* Process 7: WZ production at hadron-colliders"
    allocate (pdg_in (2)); allocate (pdg_out (2))
    pdg_in(1) = 1; pdg_in(2) = -2
    pdg_out(1) = -24; pdg_out(2) = 23
    call test_process (generator, pdg_in, pdg_out, u)
    deallocate (pdg_out); deallocate (pdg_in)

  contains
    subroutine test_process (generator, pdg_in, pdg_out, u)
      type(radiation_generator_t), intent(inout) :: generator
      type(pdg_array_t), dimension(:), intent(in) :: pdg_in, pdg_out
      integer, intent(in) :: u
      type(string_t), dimension(:), allocatable :: prt_strings_in
      type(string_t), dimension(:), allocatable :: prt_strings_out
      write (u, "(A)") "* Leading order: "
      write (u, "(A)", advance = 'no') '* Incoming: '
      call write_pdg_array (pdg_in, u)
      write (u, "(A)", advance = 'no') '* Outgoing: '
      call write_pdg_array (pdg_out, u)

      call generator%init (pdg_in, pdg_out, qcd = .true., qed = .false.)
      call generator%set_n (2, size(pdg_out), 0)
      call generator%set_constraints (.false., .false., .true., .true.)
      call generator%setup_if_table ()
      call generator%generate (prt_strings_in, prt_strings_out)
      write (u, "(A)") "* Additional radiation: "
      write (u, "(A)") "* Incoming: "
      call write_particle_string (prt_strings_in, u)
      write (u, "(A)") "* Outgoing: "
      call write_particle_string (prt_strings_out, u) 
      call write_separator(u)
    end subroutine test_process

  end subroutine radiation_generator_1

  subroutine radiation_generator_2 (u)
    integer, intent(in) :: u
    type(radiation_generator_t) :: generator
    type(pdg_array_t), dimension(:), allocatable :: pdg_in, pdg_out
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: radiation_model => null ()
    integer, parameter :: max_multiplicity = 10
    type(string_t), dimension(:), allocatable :: prt_last

    write (u, "(A)") "* Test output: radiation_generator_2"
    write (u, "(A)") "* Purpose: Test the repeated application of a radiation generator splitting"
    write (u, "(A)") "* Only Final state emissions! "
    write (u, "(A)") 
    write (u, "(A)") "* Loading radiation model: SM_rad.mdl"

    call syntax_model_file_init ()
    call os_data_init (os_data)
    call model_list%read_model &
       (var_str ("SM_rad"), var_str ("SM_rad.mdl"), &
        os_data, radiation_model)
    call generator%init_radiation_model (radiation_model)
    write (u, "(A)") "* Success"

    allocate (pdg_in (2))
    pdg_in(1) = 11; pdg_in(2) = -11
    allocate (pdg_out(2))
    pdg_out(1) = 2; pdg_out(2) = -2

    write (u, "(A)") "* Leading order"
    write (u, "(A)", advance = 'no') "* Incoming: "
    call write_pdg_array (pdg_in, u)
    write (u, "(A)", advance = 'no') "* Outgoing: "
    call write_pdg_array (pdg_out, u)

    call generator%init (pdg_in, pdg_out, qcd = .true., qed = .false.)
    call generator%set_n (2, 2, 0)
    call generator%set_constraints (.false., .false., .true., .true.)

    call write_separator (u)
    write (u, "(A)") "Generate higher-multiplicity states"
    write (u, "(A,I0)") "Desired multiplicity: ", max_multiplicity
    call generator%generate_multiple (max_multiplicity)    
    call generator%prt_queue%write (u)
    call write_separator (u)
    write (u, "(A,I0)") "Number of higher-multiplicity states: ", generator%prt_queue%n_lists

    write (u, "(A)") "Check that no particle state occurs twice or more"
    if (.not. generator%prt_queue%check_for_same_prt_strings()) then
       write (u, "(A)") "SUCCESS"
    else
       write (u, "(A)") "FAIL"
    end if
    call write_separator (u)
    write (u, "(A,I0,A)") "Check that there are ", max_multiplicity, " particles in the last entry:"
    call generator%prt_queue%get_last (prt_last)
    if (size (prt_last) == max_multiplicity) then
       write (u, "(A)") "SUCCESS"
    else
       write (u, "(A)") "FAIL"
    end if
  end subroutine radiation_generator_2


  subroutine write_pdg_array (pdg, u)
    use pdg_arrays
    type(pdg_array_t), dimension(:), intent(in) :: pdg
    integer, intent(in) :: u
    integer :: i
    do i = 1, size (pdg)
       call pdg(i)%write (u)
    end do
    write (u, "(A)")
  end subroutine write_pdg_array
 
  subroutine write_particle_string (prt, u)
    use iso_varying_string, string_t => varying_string
    type(string_t), dimension(:), intent(in) :: prt
    integer, intent(in) :: u
    integer :: i
    do i = 1, size (prt)
       write (u, "(A,1X)", advance = "no") char (prt(i))
    end do
    write (u, "(A)") 
  end subroutine write_particle_string


end module radiation_generator_uti
