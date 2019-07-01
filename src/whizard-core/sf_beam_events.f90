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

module sf_beam_events

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use unit_tests
  use diagnostics
  use physics_defs, only: ELECTRON
  use lorentz
  use pdg_arrays
  use model_data
  use flavors
  use helicities
  use colors
  use quantum_numbers
  use state_matrices
  use polarizations
  use interactions
  use sf_aux
  use sf_base

  implicit none
  private

  public :: beam_events_data_t
  public :: sf_beam_events_test

  type, extends(sf_data_t) :: beam_events_data_t
     private
     type(flavor_t), dimension(2) :: flv_in
     type(string_t) :: dir
     type(string_t) :: file
     integer :: unit = 0
     logical :: warn_eof = .true.
   contains
     procedure :: init => beam_events_data_init
     procedure :: is_generator => beam_events_data_is_generator
     procedure :: get_n_par => beam_events_data_get_n_par
     procedure :: get_pdg_out => beam_events_data_get_pdg_out
     procedure :: allocate_sf_int => beam_events_data_allocate_sf_int
     procedure :: write => beam_events_data_write
     procedure :: open => beam_events_data_open
     procedure :: close => beam_events_data_close
  end type beam_events_data_t

  type, extends (sf_int_t) :: beam_events_t
     type(beam_events_data_t), pointer :: data => null ()
     integer :: count = 0
   contains
     procedure :: type_string => beam_events_type_string
     procedure :: write => beam_events_write
     procedure :: init => beam_events_init
     procedure :: final => sf_beam_events_final
     procedure :: is_generator => beam_events_is_generator
     procedure :: generate_free => beam_events_generate_free
     procedure :: complete_kinematics => beam_events_complete_kinematics
     procedure :: inverse_kinematics => beam_events_inverse_kinematics
     procedure :: apply => beam_events_apply
  end type beam_events_t 
  

contains

  subroutine beam_events_data_init (data, model, pdg_in, dir, file, warn_eof)
    class(beam_events_data_t), intent(out) :: data
    class(model_data_t), intent(in), target :: model
    type(pdg_array_t), dimension(2), intent(in) :: pdg_in
    type(string_t), intent(in) :: dir
    type(string_t), intent(in) :: file
    logical, intent(in), optional :: warn_eof
    if (any (pdg_array_get_length (pdg_in) /= 1)) then
       call msg_fatal ("Beam events: incoming beam particles must be unique")
    end if
    call flavor_init (data%flv_in(1), pdg_array_get (pdg_in(1), 1), model)
    call flavor_init (data%flv_in(2), pdg_array_get (pdg_in(2), 1), model)
    data%dir = dir
    data%file = file
    if (present (warn_eof))  data%warn_eof = warn_eof
  end subroutine beam_events_data_init

  function beam_events_data_is_generator (data) result (flag)
    class(beam_events_data_t), intent(in) :: data
    logical :: flag
    flag = .true.
  end function beam_events_data_is_generator
  
  function beam_events_data_get_n_par (data) result (n)
    class(beam_events_data_t), intent(in) :: data
    integer :: n
    n = 2
  end function beam_events_data_get_n_par
  
  subroutine beam_events_data_get_pdg_out (data, pdg_out)
    class(beam_events_data_t), intent(in) :: data
    type(pdg_array_t), dimension(:), intent(inout) :: pdg_out
    integer :: i, n
    n = 2
    do i = 1, n
       pdg_out(i) = flavor_get_pdg (data%flv_in(i))
    end do
  end subroutine beam_events_data_get_pdg_out
  
  subroutine beam_events_data_allocate_sf_int (data, sf_int)
    class(beam_events_data_t), intent(in) :: data
    class(sf_int_t), intent(inout), allocatable :: sf_int
    allocate (beam_events_t :: sf_int)
  end subroutine beam_events_data_allocate_sf_int
  
  subroutine beam_events_data_write (data, unit, verbose) 
    class(beam_events_data_t), intent(in) :: data
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose        
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(1x,A)") "Beam-event file data:"
    write (u, "(3x,A,A,A,A)") "prt_in = ", &
         char (flavor_get_name (data%flv_in(1))), &
         ", ", char (flavor_get_name (data%flv_in(2)))    
    write (u, "(3x,A,A,A)") "file   = '", char (data%file), "'"
    write (u, "(3x,A,I0)")  "unit   = ", data%unit
    write (u, "(3x,A,L1)")  "warn   = ", data%warn_eof
  end subroutine beam_events_data_write

  subroutine beam_events_data_open (data)
    class(beam_events_data_t), intent(inout) :: data
    type(string_t) :: filename
    logical :: exist
    if (data%unit == 0) then
       filename = data%file
       if (filename == "") &
            call msg_fatal ("Beam events: $beam_events_file is not set")
       inquire (file = char (filename), exist = exist)
       if (exist) then
          call msg_message ("Beam events: reading from file '" &
               // char (data%file) // "'")
          data%unit = free_unit ()
          open (unit = data%unit, file = char (filename), action = "read", &
               status = "old")
       else
          filename = data%dir // "/" // data%file
          inquire (file = char (filename), exist = exist)
          if (exist) then
             call msg_message ("Beam events: reading from file '" &
                  // char (data%file) // "'")
             data%unit = free_unit ()
             open (unit = data%unit, file = char (filename), action = "read", &
                  status = "old")
          else
             call msg_fatal ("Beam events: file '" &
                  // char (data%file) // "' not found")
          end if
       end if
    else
       call msg_bug ("Beam events: file '" &
         // char (data%file) // "' is already open")
    end if
  end subroutine beam_events_data_open

  subroutine beam_events_data_close (data)
    class(beam_events_data_t), intent(inout) :: data
    if (data%unit /= 0) then
       close (data%unit)
       call msg_message ("Beam events: closed file '" &
         // char (data%file) // "'")
       data%unit = 0
    end if
  end subroutine beam_events_data_close

  function beam_events_type_string (object) result (string)
    class(beam_events_t), intent(in) :: object
    type(string_t) :: string
    if (associated (object%data)) then
       string = "Beam events: " // object%data%file 
    else
       string = "Beam events: [undefined]"
    end if
  end function beam_events_type_string
  
  subroutine beam_events_write (object, unit, testflag)
    class(beam_events_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u
    u = given_output_unit (unit)
    if (associated (object%data)) then
       call object%data%write (u)
       call object%base_write (u, testflag)
    else
       write (u, "(1x,A)")  "Beam events data: [undefined]"
    end if
  end subroutine beam_events_write
    
  subroutine beam_events_init (sf_int, data)
    class(beam_events_t), intent(out) :: sf_int
    class(sf_data_t), intent(in), target :: data
    real(default), dimension(2) :: m2
    real(default), dimension(0) :: mr2
    type(quantum_numbers_mask_t), dimension(4) :: mask
    integer, dimension(4) :: hel_lock
    type(quantum_numbers_t), dimension(4) :: qn_fc, qn_hel, qn
    type(polarization_t) :: pol1, pol2
    type(state_iterator_t) :: it_hel1, it_hel2
    integer :: i
    select type (data)
    type is (beam_events_data_t)
       m2 = flavor_get_mass (data%flv_in) ** 2
       hel_lock = [3, 4, 1, 2]
       mask = new_quantum_numbers_mask (.false., .false., .false.)
       call sf_int%base_init (mask, m2, mr2, m2, hel_lock = hel_lock)
       sf_int%data => data
       do i = 1, 2
          call quantum_numbers_init (qn_fc(i), &
               flv = data%flv_in(i), &
               col = color_from_flavor (data%flv_in(i)))
          call quantum_numbers_init (qn_fc(i+2), &
               flv = data%flv_in(i), &
               col = color_from_flavor (data%flv_in(i)))
       end do
       call polarization_init_generic (pol1, data%flv_in(1))
       call state_iterator_init (it_hel1, pol1%state)
       do while (state_iterator_is_valid (it_hel1))
          qn_hel(1:1) = state_iterator_get_quantum_numbers (it_hel1)
          qn_hel(3:3) = state_iterator_get_quantum_numbers (it_hel1)
          call polarization_init_generic (pol2, data%flv_in(2))
          call state_iterator_init (it_hel2, pol2%state)
          do while (state_iterator_is_valid (it_hel2))
             qn_hel(2:2) = state_iterator_get_quantum_numbers (it_hel2)
             qn_hel(4:4) = state_iterator_get_quantum_numbers (it_hel2)
             qn = qn_hel .merge. qn_fc
             call interaction_add_state (sf_int%interaction_t, qn)
             call state_iterator_advance (it_hel2)
          end do
          call polarization_final (pol2)
          call state_iterator_advance (it_hel1)
       end do
       call polarization_final (pol2)
       call interaction_freeze (sf_int%interaction_t)
       call sf_int%set_incoming ([1,2])
       call sf_int%set_outgoing ([3,4])
       call sf_int%data%open ()
       sf_int%status = SF_INITIAL
    end select
  end subroutine beam_events_init
    
  subroutine sf_beam_events_final (object)
    class(beam_events_t), intent(inout) :: object
    call object%data%close ()
    call interaction_final (object%interaction_t)
  end subroutine sf_beam_events_final

  function beam_events_is_generator (sf_int) result (flag)
    class(beam_events_t), intent(in) :: sf_int
    logical :: flag
    flag = sf_int%data%is_generator ()
  end function beam_events_is_generator
  
  recursive subroutine beam_events_generate_free (sf_int, r, rb,  x_free)
    class(beam_events_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(out) :: r, rb
    real(default), intent(inout) :: x_free
    integer :: iostat
    associate (data => sf_int%data)
      if (data%unit /= 0) then
         read (data%unit, fmt=*, iostat=iostat)  r
         if (iostat > 0) then
            write (msg_buffer, "(A,I0,A)") &
                 "Beam events: I/O error after reading ", sf_int%count, &
                 " events"
            call msg_fatal ()
         else if (iostat < 0) then
            if (sf_int%count == 0) then
               call msg_fatal ("Beam events: file is empty")
            else if (sf_int%data%warn_eof) then
               write (msg_buffer, "(A,I0,A)") &
                    "Beam events: End of file after reading ", sf_int%count, &
                    " events, rewinding"
               call msg_warning ()
            end if
            rewind (data%unit)
            sf_int%count = 0
            call sf_int%generate_free (r, rb, x_free)
         else
            sf_int%count = sf_int%count + 1
            rb = 1 - r
            x_free = x_free * product (r)
         end if
      else
         call msg_bug ("Beam events: file is not open for reading")
      end if
    end associate
  end subroutine beam_events_generate_free
    
  subroutine beam_events_complete_kinematics (sf_int, x, f, r, rb, map)
    class(beam_events_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(out) :: x
    real(default), intent(out) :: f
    real(default), dimension(:), intent(in) :: r
    real(default), dimension(:), intent(in) :: rb
    logical, intent(in) :: map
    if (map) then
       call msg_fatal ("Beam events: map flag not supported")
    else
       x = r
       f = 1
    end if
    call sf_int%reduce_momenta (x)
  end subroutine beam_events_complete_kinematics

  subroutine beam_events_inverse_kinematics &
       (sf_int, x, f, r, rb, map, set_momenta)
    class(beam_events_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(in) :: x
    real(default), intent(out) :: f
    real(default), dimension(:), intent(out) :: r
    real(default), dimension(:), intent(out) :: rb
    logical, intent(in) :: map
    logical, intent(in), optional :: set_momenta
    logical :: set_mom
    set_mom = .false.;  if (present (set_momenta))  set_mom = set_momenta
    if (map) then
       call msg_fatal ("Beam events: map flag not supported")
    else
       r = x
       f = 1
    end if
    rb = 1 - r
    if (set_mom) then
       call sf_int%reduce_momenta (x)
    end if
  end subroutine beam_events_inverse_kinematics

  subroutine beam_events_apply (sf_int, scale)
    class(beam_events_t), intent(inout) :: sf_int
    real(default), intent(in) :: scale
    real(default) :: f
    f = 1
    call interaction_set_matrix_element (sf_int%interaction_t, &
         cmplx (f, kind=default))        
    sf_int%status = SF_EVALUATED
  end subroutine beam_events_apply


  subroutine sf_beam_events_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (sf_beam_events_1, "sf_beam_events_1", &
         "structure function configuration", &
         u, results)
    call test (sf_beam_events_2, "sf_beam_events_2", &
         "generate event", &
         u, results)
  end subroutine sf_beam_events_test
  
  subroutine sf_beam_events_1 (u)
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(pdg_array_t), dimension(2) :: pdg_in
    type(pdg_array_t), dimension(2) :: pdg_out
    integer, dimension(:), allocatable :: pdg1, pdg2
    class(sf_data_t), allocatable :: data
    
    write (u, "(A)")  "* Test output: sf_beam_events_1"
    write (u, "(A)")  "*   Purpose: initialize and display &
         &beam-events structure function data"
    write (u, "(A)")
    
    call model%init_qed_test ()
    pdg_in(1) = ELECTRON
    pdg_in(2) = -ELECTRON

    allocate (beam_events_data_t :: data)
    select type (data)
    type is (beam_events_data_t)
       call data%init (model, pdg_in, var_str (""), var_str ("beam_events.dat"))
    end select

    call data%write (u)

    write (u, "(A)")

    write (u, "(1x,A)")  "Outgoing particle codes:"
    call data%get_pdg_out (pdg_out)
    pdg1 = pdg_out(1)
    pdg2 = pdg_out(2)
    write (u, "(2x,99(1x,I0))")  pdg1, pdg2

    call model%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_beam_events_1"

  end subroutine sf_beam_events_1

  subroutine sf_beam_events_2 (u)
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(flavor_t), dimension(2) :: flv
    type(pdg_array_t), dimension(2) :: pdg_in
    class(sf_data_t), allocatable, target :: data
    class(sf_int_t), allocatable :: sf_int
    type(vector4_t) :: k1, k2
    real(default) :: E
    real(default), dimension(:), allocatable :: r, rb, x
    real(default) :: x_free, f
    integer :: i
    
    write (u, "(A)")  "* Test output: sf_beam_events_2"
    write (u, "(A)")  "*   Purpose: initialize and display &
         &beam-events structure function data"
    write (u, "(A)")
    
    call model%init_qed_test ()
    call flavor_init (flv(1), ELECTRON, model)
    call flavor_init (flv(2), -ELECTRON, model)
    pdg_in(1) = ELECTRON
    pdg_in(2) = -ELECTRON

    call reset_interaction_counter ()
    
    allocate (beam_events_data_t :: data)
    select type (data)
    type is (beam_events_data_t)
       call data%init (model, pdg_in, &
            var_str (""), var_str ("test_beam_events.dat"))
    end select
       
    write (u, "(A)")  "* Initialize structure-function object"
    write (u, "(A)")
    
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)
    call sf_int%set_beam_index ([1,2])

    write (u, "(A)")  "* Initialize incoming momentum with E=500"
    write (u, "(A)")
    E = 250
    k1 = vector4_moving (E, sqrt (E**2 - flavor_get_mass (flv(1))**2), 3)
    k2 = vector4_moving (E,-sqrt (E**2 - flavor_get_mass (flv(2))**2), 3)
    call vector4_write (k1, u)
    call vector4_write (k2, u)
    call sf_int%seed_kinematics ([k1, k2])

    write (u, "(A)")
    write (u, "(A)")  "* Set dummy parameters and generate x."
    write (u, "(A)")

    allocate (r (data%get_n_par ()))
    allocate (rb(size (r)))
    allocate (x (size (r)))

    r  = 0
    rb = 0
    x_free = 1
    call sf_int%generate_free (r, rb, x_free)
    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)

    write (u, "(A,9(1x,F10.7))")  "r =", r
    write (u, "(A,9(1x,F10.7))")  "rb=", rb
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "f =", f
    write (u, "(A,9(1x,F10.7))")  "xf=", x_free
    select type (sf_int)
    type is (beam_events_t)     
       write (u, "(A,1x,I0)")  "count =", sf_int%count
    end select

    write (u, "(A)")
    write (u, "(A)")  "* Evaluate"
    write (u, "(A)")

    call sf_int%apply (scale = 0._default)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Generate more events, rewind"
    write (u, "(A)")

    select type (sf_int)
    type is (beam_events_t)     
       do i = 1, 3
          call sf_int%generate_free (r, rb, x_free)
          write (u, "(A,9(1x,F10.7))")  "r =", r
          write (u, "(A,1x,I0)")  "count =", sf_int%count
       end do
    end select

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call sf_int%final ()
    call model%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_beam_events_2"

  end subroutine sf_beam_events_2


end module sf_beam_events
