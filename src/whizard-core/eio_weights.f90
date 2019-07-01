! WHIZARD 2.2.0 May 18 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Christian Speckner <cnspeckn@googlemail.com> 
!     and  Fabian Bach, Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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

module eio_weights
  
  use kinds !NODEP!
  use file_utils !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use diagnostics !NODEP!
  use unit_tests

  use lorentz !NODEP!
  use variables
  use models
  use particles
  use beams
  use processes
  use events
  use eio_data
  use eio_base

  implicit none
  private

  public :: eio_weights_t
  public :: eio_weights_test

  type, extends (eio_t) :: eio_weights_t
     logical :: writing = .false.
     integer :: unit = 0
     logical :: pacify = .false.
   contains
     procedure :: set_parameters => eio_weights_set_parameters
     procedure :: write => eio_weights_write
     procedure :: final => eio_weights_final
     procedure :: init_out => eio_weights_init_out
     procedure :: init_in => eio_weights_init_in
     procedure :: switch_inout => eio_weights_switch_inout
     procedure :: output => eio_weights_output
     procedure :: input_i_prc => eio_weights_input_i_prc
     procedure :: input_event => eio_weights_input_event
  end type eio_weights_t
  

contains
  
  subroutine eio_weights_set_parameters (eio, pacify)
    class(eio_weights_t), intent(inout) :: eio
    logical, intent(in), optional :: pacify
    if (present (pacify))  eio%pacify = pacify
  end subroutine eio_weights_set_parameters
  
  subroutine eio_weights_write (object, unit)
    class(eio_weights_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, "(1x,A)")  "Weight stream:"
    if (object%writing) then
       write (u, "(3x,A,A)")  "Writing to file   = ", char (object%filename)
       write (u, "(3x,A,L1)") "Reduced I/O prec. = ", object%pacify 
    else
       write (u, "(3x,A)")  "[closed]"
    end if
  end subroutine eio_weights_write
  
  subroutine eio_weights_final (object)
    class(eio_weights_t), intent(inout) :: object
    if (object%writing) then
       write (msg_buffer, "(A,A,A)")  "Events: closing weight stream file '", &
            char (object%filename), "'"
       call msg_message ()
       close (object%unit)
       object%writing = .false.
    end if
  end subroutine eio_weights_final
  
  subroutine eio_weights_init_out &
       (eio, sample, process_ptr, data, success)
    class(eio_weights_t), intent(inout) :: eio
    type(string_t), intent(in) :: sample
    type(process_ptr_t), dimension(:), intent(in) :: process_ptr
    type(event_sample_data_t), intent(in), optional :: data
    logical, intent(out), optional :: success
    eio%filename = sample // ".weights.dat"
    eio%unit = free_unit ()
    write (msg_buffer, "(A,A,A)")  "Events: writing to weight stream file '", &
         char (eio%filename), "'"
    call msg_message ()
    eio%writing = .true.
    open (eio%unit, file = char (eio%filename), &
         action = "write", status = "replace")
    if (present (success))  success = .true.
  end subroutine eio_weights_init_out
    
  subroutine eio_weights_init_in &
       (eio, sample, process_ptr, data, success, extension)
    class(eio_weights_t), intent(inout) :: eio
    type(string_t), intent(in) :: sample
    type(string_t), intent(in), optional :: extension
    type(process_ptr_t), dimension(:), intent(in) :: process_ptr
    type(event_sample_data_t), intent(inout), optional :: data
    logical, intent(out), optional :: success
    call msg_bug ("Weight stream: event input not supported")
    if (present (success))  success = .false.
  end subroutine eio_weights_init_in
    
  subroutine eio_weights_switch_inout (eio, success)
    class(eio_weights_t), intent(inout) :: eio
    logical, intent(out), optional :: success
    call msg_bug ("Weight stream: in-out switch not supported")
    if (present (success))  success = .false.
  end subroutine eio_weights_switch_inout
  
  subroutine eio_weights_output (eio, event, i_prc, reading)
    class(eio_weights_t), intent(inout) :: eio
    type(event_t), intent(in), target :: event
    integer, intent(in) :: i_prc
    logical, intent(in), optional :: reading
    integer :: i_mci, i_term, n_alt, i
    real(default) :: weight, sqme_ref, sqme_prc
    if (eio%writing) then
       i_mci = event%get_i_mci ()
       i_term = event%get_i_term ()
       weight = event%weight_prc
       sqme_ref = event%sqme_ref
       sqme_prc = event%sqme_prc
       n_alt = event%n_alt
1      format (I0,3(1x,ES17.10),3(1x,I0))
2      format (I0,3(1x,ES15.8),3(1x,I0))       
       if (eio%pacify) then
          write (eio%unit, 2)  0, weight, sqme_prc, sqme_ref, &
               i_prc, i_mci, i_term
       else
          write (eio%unit, 1)  0, weight, sqme_prc, sqme_ref, &
               i_prc, i_mci, i_term
       end if
       do i = 1, n_alt
          weight = event%weight_alt(i)
          sqme_prc = event%sqme_alt(i)
          if (eio%pacify) then
             write (eio%unit, 2)  i, weight, sqme_prc
          else
             write (eio%unit, 1)  i, weight, sqme_prc          
          end if
       end do
    else
       call eio%write ()
       call msg_fatal ("Weight stream file is not open for writing")
    end if
  end subroutine eio_weights_output

  subroutine eio_weights_input_i_prc (eio, i_prc, iostat)
    class(eio_weights_t), intent(inout) :: eio
    integer, intent(out) :: i_prc
    integer, intent(out) :: iostat
    call msg_bug ("Weight stream: event input not supported")
    i_prc = 0
    iostat = 1
  end subroutine eio_weights_input_i_prc

  subroutine eio_weights_input_event (eio, event, iostat)
    class(eio_weights_t), intent(inout) :: eio
    type(event_t), intent(inout), target :: event
    integer, intent(out) :: iostat
    call msg_bug ("Weight stream: event input not supported")
    iostat = 1
  end subroutine eio_weights_input_event


  subroutine eio_weights_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (eio_weights_1, "eio_weights_1", &
         "read and write event contents", &
         u, results)
    call test (eio_weights_2, "eio_weights_2", &
         "multiple weights", &
         u, results)
  end subroutine eio_weights_test
  
  subroutine eio_weights_1 (u)
    integer, intent(in) :: u
    type(model_list_t) :: model_list
    type(event_t), allocatable, target :: event
    type(process_t), allocatable, target :: process
    type(process_ptr_t) :: process_ptr
    type(process_instance_t), allocatable, target :: process_instance
    class(eio_t), allocatable :: eio
    type(string_t) :: sample
    integer :: u_file
    character(80) :: buffer

    write (u, "(A)")  "* Test output: eio_weights_1"
    write (u, "(A)")  "*   Purpose: generate an event and write weight to file"
    write (u, "(A)")

    call syntax_model_file_init ()

    write (u, "(A)")  "* Initialize test process"
 
    allocate (process)
    process_ptr%ptr => process
    allocate (process_instance)
    call prepare_test_process (process, process_instance, model_list)
    call process_instance%setup_event_data ()
 
    allocate (event)
    call event%basic_init ()
    call event%connect (process_instance, process%get_model_ptr ())
    
    write (u, "(A)")
    write (u, "(A)")  "* Generate and write an event"
    write (u, "(A)")
 
    sample = "eio_weights_1"
 
    allocate (eio_weights_t :: eio)
    
    call eio%init_out (sample, [process_ptr])
    call event%generate (1, [0._default, 0._default])

    call eio%output (event, i_prc = 42)
    call eio%write (u)
    call eio%final ()

    write (u, "(A)")
    write (u, "(A)")  "* File contents: &
         &(weight, sqme(evt), sqme(prc), i_prc, i_mci, i_term)"
    write (u, "(A)")

    u_file = free_unit ()
    open (u_file, file = "eio_weights_1.weights.dat", &
         action = "read", status = "old")
    read (u_file, "(A)")  buffer
    write (u, "(A)") trim (buffer)
    close (u_file)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
 
    call event%final ()
    deallocate (event)
 
    call cleanup_test_process (process, process_instance)
    deallocate (process_instance)
    deallocate (process)

    call model_list%final ()
    call syntax_model_file_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: eio_weights_1"
    
  end subroutine eio_weights_1
  
  subroutine eio_weights_2 (u)
    integer, intent(in) :: u
    type(model_list_t) :: model_list
    type(var_list_t) :: var_list
    type(event_t), allocatable, target :: event
    type(process_t), allocatable, target :: process
    type(process_ptr_t) :: process_ptr
    type(process_instance_t), allocatable, target :: process_instance
    class(eio_t), allocatable :: eio
    type(string_t) :: sample
    integer :: u_file, i
    character(80) :: buffer

    write (u, "(A)")  "* Test output: eio_weights_2"
    write (u, "(A)")  "*   Purpose: generate an event and write weight to file"
    write (u, "(A)")

    call syntax_model_file_init ()

    write (u, "(A)")  "* Initialize test process"
 
    allocate (process)
    process_ptr%ptr => process
    allocate (process_instance)
    call prepare_test_process (process, process_instance, model_list)
    call process_instance%setup_event_data ()

    call var_list_append_log (var_list, var_str ("?unweighted"), .false., &
         intrinsic = .true.)
    call var_list_append_string (var_list, var_str ("$sample_normalization"), &
         var_str ("auto"), intrinsic = .true.)

    allocate (event)
    call event%basic_init (var_list, n_alt = 2)
    call event%connect (process_instance, process%get_model_ptr ())
    
    write (u, "(A)")
    write (u, "(A)")  "* Generate and write an event"
    write (u, "(A)")
 
    sample = "eio_weights_2"
 
    allocate (eio_weights_t :: eio)
    
    call eio%init_out (sample, [process_ptr])
    select type (eio)
    type is (eio_weights_t)
       call eio%set_parameters (pacify = .true.)
    end select
    call event%generate (1, [0._default, 0._default])
    call event%set (sqme_alt = [2._default, 3._default])
    call event%set (weight_alt = [2 * event%weight_prc, 3 * event%weight_prc])

    call eio%output (event, i_prc = 42)
    call eio%write (u)
    call eio%final ()

    write (u, "(A)") 
    write (u, "(A)")  "* File contents: &
         &(weight, sqme(evt), sqme(prc), i_prc, i_mci, i_term)"
    write (u, "(A)")

    u_file = free_unit ()
    open (u_file, file = "eio_weights_2.weights.dat", &
         action = "read", status = "old")
    do i = 1, 3
       read (u_file, "(A)")  buffer
       write (u, "(A)") trim (buffer)
    end do
    close (u_file)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
 
    call event%final ()
    deallocate (event)
 
    call cleanup_test_process (process, process_instance)
    deallocate (process_instance)
    deallocate (process)

    call model_list%final ()
    call syntax_model_file_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: eio_weights_2"
    
  end subroutine eio_weights_2
  

end module eio_weights
