! WHIZARD 2.2.8 Nov 22 2015
! 
! Copyright (C) 1999-2015 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@t-online.de>
!     Bijan Chokoufe <bijan.chokoufe@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Soyoung Shim <soyoung.shim@desy.de>
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

module sf_beam_events

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use file_registries
  use diagnostics
  use lorentz
  use pdg_arrays
  use model_data
  use flavors
  use quantum_numbers
  use state_matrices
  use polarizations
  use sf_base

  implicit none
  private

  public :: beam_file_registry
  public :: beam_events_data_t
  public :: beam_events_t

  type, extends(sf_data_t) :: beam_events_data_t
     private
     type(flavor_t), dimension(2) :: flv_in
     type(string_t) :: dir
     type(string_t) :: file
     type(string_t) :: fqn
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
  

  type(file_registry_t), save :: beam_file_registry


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
    call data%flv_in(1)%init (pdg_array_get (pdg_in(1), 1), model)
    call data%flv_in(2)%init (pdg_array_get (pdg_in(2), 1), model)
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
       pdg_out(i) = data%flv_in(i)%get_pdg ()
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
         char (data%flv_in(1)%get_name ()), &
         ", ", char (data%flv_in(2)%get_name ())    
    write (u, "(3x,A,A,A)") "file   = '", char (data%file), "'"
    write (u, "(3x,A,I0)")  "unit   = ", data%unit
    write (u, "(3x,A,L1)")  "warn   = ", data%warn_eof
  end subroutine beam_events_data_write

  subroutine beam_events_data_open (data)
    class(beam_events_data_t), intent(inout) :: data
    logical :: exist
    if (data%unit == 0) then
       data%fqn = data%file
       if (data%fqn == "") &
            call msg_fatal ("Beam events: $beam_events_file is not set")
       inquire (file = char (data%fqn), exist = exist)
       if (.not. exist) then
          data%fqn = data%dir // "/" // data%file
          inquire (file = char (data%fqn), exist = exist)
          if (.not. exist) then
             data%fqn = ""
             call msg_fatal ("Beam events: file '" &
                  // char (data%file) // "' not found")
             return
          end if
       end if
       call msg_message ("Beam events: reading from file '" &
            // char (data%file) // "'")
       call beam_file_registry%open (data%fqn, data%unit)
    else
       call msg_bug ("Beam events: file '" &
         // char (data%file) // "' is already open")
    end if
  end subroutine beam_events_data_open

  subroutine beam_events_data_close (data)
    class(beam_events_data_t), intent(inout) :: data
    if (data%unit /= 0) then
       call beam_file_registry%close (data%fqn)
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
       m2 = data%flv_in%get_mass () ** 2
       hel_lock = [3, 4, 1, 2]
       mask = quantum_numbers_mask (.false., .false., .false.)
       call sf_int%base_init (mask, m2, mr2, m2, hel_lock = hel_lock)
       sf_int%data => data
       do i = 1, 2
          call qn_fc(i)%init ( &
               flv = data%flv_in(i), &
               col = color_from_flavor (data%flv_in(i)))
          call qn_fc(i+2)%init ( &
               flv = data%flv_in(i), &
               col = color_from_flavor (data%flv_in(i)))
       end do
       call polarization_init_generic (pol1, data%flv_in(1))
       call it_hel1%init (pol1%state)
       do while (it_hel1%is_valid ())
          qn_hel(1:1) = it_hel1%get_quantum_numbers ()
          qn_hel(3:3) = it_hel1%get_quantum_numbers ()
          call polarization_init_generic (pol2, data%flv_in(2))
          call it_hel2%init (pol2%state)
          do while (it_hel2%is_valid ())
             qn_hel(2:2) = it_hel2%get_quantum_numbers ()
             qn_hel(4:4) = it_hel2%get_quantum_numbers ()
             qn = qn_hel .merge. qn_fc
             call sf_int%add_state (qn)
             call it_hel2%advance ()
          end do
          call polarization_final (pol2)
          call it_hel1%advance ()
       end do
       call polarization_final (pol2)
       call sf_int%freeze ()
       call sf_int%set_incoming ([1,2])
       call sf_int%set_outgoing ([3,4])
       call sf_int%data%open ()
       sf_int%status = SF_INITIAL
    end select
  end subroutine beam_events_init
    
  subroutine sf_beam_events_final (object)
    class(beam_events_t), intent(inout) :: object
    call object%data%close ()
    call object%interaction_t%final ()
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
    call sf_int%set_matrix_element (cmplx (f, kind=default))        
    sf_int%status = SF_EVALUATED
  end subroutine beam_events_apply


end module sf_beam_events
