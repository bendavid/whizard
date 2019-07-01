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

module eio_lhef
  
  use kinds !NODEP!
  use file_utils !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use diagnostics !NODEP!
  use unit_tests

  use xml
  use lorentz !NODEP!
  use models
  use particles
  use beams
  use processes
  use events
  use eio_data
  use eio_base
  use hep_common
  use hep_events

  implicit none
  private

  public :: eio_lhef_t
  public :: eio_lhef_test

  type, extends (eio_t) :: eio_lhef_t
     logical :: writing = .false.
     logical :: reading = .false.
     integer :: unit = 0
     type(event_sample_data_t) :: data
     type(cstream_t) :: cstream
     character(3) :: version = "1.0"
     logical :: keep_beams = .false.
     logical :: recover_beams = .true.
     logical :: unweighted = .true.
     logical :: write_sqme_ref = .false.
     logical :: write_sqme_prc = .false.
     logical :: write_sqme_alt = .false.
     integer :: n_alt = 0
     integer, dimension(:), allocatable :: proc_num_id
     integer :: i_weight_sqme = 0
     type(xml_tag_t) :: tag_lhef, tag_head, tag_init, tag_event
     type(xml_tag_t), allocatable :: tag_gen_n, tag_gen_v
     type(xml_tag_t), allocatable :: tag_generator, tag_xsecinfo
     type(xml_tag_t), allocatable :: tag_sqme_ref, tag_sqme_prc
     type(xml_tag_t), dimension(:), allocatable :: tag_sqme_alt, tag_wgts_alt
     type(xml_tag_t), allocatable :: tag_weight, tag_weightinfo, tag_weights
   contains
     procedure :: set_parameters => eio_lhef_set_parameters
     procedure :: write => eio_lhef_write
     procedure :: final => eio_lhef_final
     procedure :: common_init => eio_lhef_common_init
     procedure :: init_tags => eio_lhef_init_tags
     procedure :: init_out => eio_lhef_init_out
     procedure :: init_in => eio_lhef_init_in
     procedure :: merge_data => eio_merge_data
     procedure :: switch_inout => eio_lhef_switch_inout
     procedure :: split_out => eio_lhef_split_out
     procedure :: output => eio_lhef_output
     procedure :: input_i_prc => eio_lhef_input_i_prc
     procedure :: input_event => eio_lhef_input_event
     procedure :: write_header => eio_lhef_write_header
     procedure :: write_footer => eio_lhef_write_footer
     procedure :: read_header => eio_lhef_read_header
     procedure :: read_init_10 => eio_lhef_read_init_10
     procedure :: write_init_20 => eio_lhef_write_init_20
     procedure :: read_init_20 => eio_lhef_read_init_20
     procedure :: write_event_20 => eio_lhef_write_event_20
     procedure :: read_event_20 => eio_lhef_read_event_20
     procedure :: write_init_30 => eio_lhef_write_init_30
     procedure :: read_init_30 => eio_lhef_read_init_30
     procedure :: write_event_30 => eio_lhef_write_event_30
     procedure :: read_event_30 => eio_lhef_read_event_30
  end type eio_lhef_t
  

  interface str
     module procedure str_int, str_real
  end interface

contains
  
  subroutine eio_lhef_set_parameters (eio, keep_beams, recover_beams, version, &
       extension, &
       write_sqme_ref, write_sqme_prc, write_sqme_alt)
    class(eio_lhef_t), intent(inout) :: eio
    logical, intent(in), optional :: keep_beams
    logical, intent(in), optional :: recover_beams
    character(*), intent(in), optional :: version
    type(string_t), intent(in), optional :: extension
    logical, intent(in), optional :: write_sqme_ref
    logical, intent(in), optional :: write_sqme_prc
    logical, intent(in), optional :: write_sqme_alt
    if (present (keep_beams))  eio%keep_beams = keep_beams
    if (present (recover_beams))  eio%recover_beams = recover_beams
    if (present (version)) then
       select case (version)
       case ("1.0", "2.0", "3.0")
          eio%version = version
       case default
          call msg_error ("LHEF version " // version &
               // " is not supported.  Inserting 2.0")
          eio%version = "2.0"
       end select
    end if
    if (present (extension)) then
       eio%extension = extension
    else
       eio%extension = "lhe"
    end if
    if (present (write_sqme_ref))  eio%write_sqme_ref = write_sqme_ref
    if (present (write_sqme_prc))  eio%write_sqme_prc = write_sqme_prc
    if (present (write_sqme_alt))  eio%write_sqme_alt = write_sqme_alt
  end subroutine eio_lhef_set_parameters
  
  subroutine eio_lhef_write (object, unit)
    class(eio_lhef_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u, i
    u = output_unit (unit)
    write (u, "(1x,A)")  "LHEF event stream:"
    if (object%writing) then
       write (u, "(3x,A,A)")  "Writing to file   = ", char (object%filename)
    else if (object%reading) then
       write (u, "(3x,A,A)")  "Reading from file = ", char (object%filename)
    else
       write (u, "(3x,A)")  "[closed]"
    end if
    write (u, "(3x,A,L1)")    "Keep beams        = ", object%keep_beams
    write (u, "(3x,A,L1)")    "Recover beams     = ", object%recover_beams
    write (u, "(3x,A,A)")     "Version           = ", object%version
    write (u, "(3x,A,A,A)")     "File extension    = '", &
         char (object%extension), "'"
    if (allocated (object%proc_num_id)) then
       write (u, "(3x,A)")  "Numerical process IDs:"
       do i = 1, size (object%proc_num_id)
          write (u, "(5x,I0,': ',I0)")  i, object%proc_num_id(i)
       end do
    end if
  end subroutine eio_lhef_write
  
  subroutine eio_lhef_final (object)
    class(eio_lhef_t), intent(inout) :: object
    if (allocated (object%proc_num_id))  deallocate (object%proc_num_id)
    if (object%writing) then
       write (msg_buffer, "(A,A,A)")  "Events: closing LHEF file '", &
            char (object%filename), "'"
       call msg_message ()
       call object%write_footer ()
       close (object%unit)
       object%writing = .false.
    else if (object%reading) then
       write (msg_buffer, "(A,A,A)")  "Events: closing LHEF file '", &
            char (object%filename), "'"
       call msg_message ()
       call object%cstream%final ()
       object%reading = .false.
    end if
  end subroutine eio_lhef_final
  
  subroutine eio_lhef_common_init (eio, sample, data, extension)
    class(eio_lhef_t), intent(inout) :: eio
    type(string_t), intent(in) :: sample
    type(string_t), intent(in), optional :: extension
    type(event_sample_data_t), intent(in), optional :: data
    if (.not. present (data)) &
         call msg_bug ("LHEF initialization: missing data")
    eio%data = data
    if (data%n_beam /= 2) &
         call msg_fatal ("LHEF: defined for scattering processes only")
    eio%unweighted = data%unweighted
    if (eio%unweighted) then
       select case (data%norm_mode)
       case (NORM_UNIT)
       case default;  call msg_fatal &
            ("LHEF: normalization for unweighted events must be '1'")
       end select
    else
       select case (data%norm_mode)
       case (NORM_SIGMA)
       case default;  call msg_fatal &
            ("LHEF: normalization for weighted events must be 'sigma'")
       end select
    end if
    eio%n_alt = data%n_alt
    eio%sample = sample
    if (present (extension)) then
       eio%extension = extension
    else
       eio%extension = "lhe"
    end if
    call eio%set_filename ()
    eio%unit = free_unit ()
    call eio%init_tags (data)
    allocate (eio%proc_num_id (data%n_proc), source = data%proc_num_id)
  end subroutine eio_lhef_common_init
  
  subroutine eio_lhef_init_tags (eio, data)
    class(eio_lhef_t), intent(inout) :: eio
    type(event_sample_data_t), intent(in) :: data
    real(default), parameter :: pb_per_fb = 1.e-3_default
    integer :: i
    call eio%tag_lhef%init ( &
         var_str ("LesHouchesEvents"), &
         [xml_attribute (var_str ("version"), var_str (eio%version))], &
         .true.)
    call eio%tag_head%init ( &
         var_str ("header"), &
         .true.)
    call eio%tag_init%init ( &
         var_str ("init"), &
         .true.)
    call eio%tag_event%init (var_str ("event"), &
         .true.)
    select case (eio%version)
    case ("1.0")
       allocate (eio%tag_gen_n)
       call eio%tag_gen_n%init ( &
            var_str ("generator_name"), &
            .true.)
       allocate (eio%tag_gen_v)
       call eio%tag_gen_v%init ( &
            var_str ("generator_version"), &
            .true.)
    end select
    select case (eio%version)
    case ("2.0", "3.0")
       allocate (eio%tag_generator)
       call eio%tag_generator%init ( &
            var_str ("generator"), &
            [xml_attribute (var_str ("version"), var_str ("2.2.0"))], &
            .true.)
       allocate (eio%tag_xsecinfo)
       call eio%tag_xsecinfo%init ( &
            var_str ("xsecinfo"), &
            [xml_attribute (var_str ("neve"), str (data%n_evt)), &
             xml_attribute (var_str ("totxsec"), &
                            str (data%total_cross_section * pb_per_fb))])
    end select
    select case (eio%version)
    case ("2.0")
       allocate (eio%tag_weight)
       call eio%tag_weight%init (var_str ("weight"), &
            [xml_attribute (var_str ("name"))])
       if (eio%write_sqme_ref) then
          allocate (eio%tag_sqme_ref)
          call eio%tag_sqme_ref%init (var_str ("weight"), &
               [xml_attribute (var_str ("name"), var_str ("sqme_ref"))], &
               .true.)
       end if
       if (eio%write_sqme_prc) then
          allocate (eio%tag_sqme_prc)
          call eio%tag_sqme_prc%init (var_str ("weight"), &
               [xml_attribute (var_str ("name"), var_str ("sqme_prc"))], &
               .true.)
       end if
       if (eio%n_alt > 0) then
          if (eio%write_sqme_alt) then
             allocate (eio%tag_sqme_alt (1))
             call eio%tag_sqme_alt(1)%init (var_str ("weight"), &
                  [xml_attribute (var_str ("name"), var_str ("sqme_alt"))], &
                  .true.)
          end if
          allocate (eio%tag_wgts_alt (1))
          call eio%tag_wgts_alt(1)%init (var_str ("weight"), &
               [xml_attribute (var_str ("name"), var_str ("wgts_alt"))], &
               .true.)
       end if
    case ("3.0")
       if (eio%write_sqme_ref) then
          allocate (eio%tag_sqme_ref)
          call eio%tag_sqme_ref%init (var_str ("weightinfo"), &
               [xml_attribute (var_str ("name"), var_str ("sqme_ref"))])
       end if
       if (eio%write_sqme_prc) then
          allocate (eio%tag_sqme_prc)
          call eio%tag_sqme_prc%init (var_str ("weightinfo"), &
               [xml_attribute (var_str ("name"), var_str ("sqme_prc"))])
       end if
       if (eio%n_alt > 0) then
          if (eio%write_sqme_alt) then
             allocate (eio%tag_sqme_alt (eio%n_alt))
             do i = 1, eio%n_alt
                call eio%tag_sqme_alt(i)%init (var_str ("weightinfo"), &
                     [xml_attribute (var_str ("name"), &
                                     var_str ("sqme_alt") // str (i))])
             end do
          end if
          allocate (eio%tag_wgts_alt (eio%n_alt))
          do i = 1, eio%n_alt
             call eio%tag_wgts_alt(i)%init (var_str ("weightinfo"), &
                  [xml_attribute (var_str ("name"), &
                                  var_str ("wgts_alt") // str (i))])
          end do
       end if
       allocate (eio%tag_weightinfo)
       call eio%tag_weightinfo%init (var_str ("weightinfo"), &
            [xml_attribute (var_str ("name"))])
       allocate (eio%tag_weights)
       call eio%tag_weights%init (var_str ("weights"), .true.)
    end select
  end subroutine eio_lhef_init_tags
  
  subroutine eio_lhef_init_out (eio, sample, process_ptr, data, success)
    class(eio_lhef_t), intent(inout) :: eio
    type(string_t), intent(in) :: sample
    type(process_ptr_t), dimension(:), intent(in) :: process_ptr
    type(event_sample_data_t), intent(in), optional :: data
    logical, intent(out), optional :: success
    integer :: u, i
    if (.not. present (data)) &
         call msg_bug ("LHEF initialization: missing data")
    eio%sample = sample
    call eio%set_splitting (data)
    call eio%common_init (sample, data)
    write (msg_buffer, "(A,A,A)")  "Events: writing to LHEF file '", &
         char (eio%filename), "'"
    call msg_message ()
    eio%writing = .true.
    u = eio%unit
    open (u, file = char (eio%filename), &
         action = "write", status = "replace")
    call eio%write_header ()
    call heprup_init &
         (data%pdg_beam, &
          data%energy_beam, &
          n_processes = data%n_proc, &
          unweighted = data%unweighted, &
          negative_weights = data%negative_weights)             
    do i = 1, data%n_proc
       call heprup_set_process_parameters (i = i, &
            process_id = data%proc_num_id(i), &
            cross_section = data%cross_section(i), &
            error = data%error(i))
    end do
    call eio%tag_init%write (u);  write (u, *)
    call heprup_write_lhef (u)
    select case (eio%version)
    case ("2.0");  call eio%write_init_20 (data)
    case ("3.0");  call eio%write_init_30 (data)
    end select
    call eio%tag_init%close (u);  write (u, *)
    if (present (success))  success = .true.
  end subroutine eio_lhef_init_out
    
  subroutine eio_lhef_init_in &
       (eio, sample, process_ptr, data, success, extension)
    class(eio_lhef_t), intent(inout) :: eio
    type(string_t), intent(in) :: sample
    type(string_t), intent(in), optional :: extension
    type(process_ptr_t), dimension(:), intent(in) :: process_ptr
    type(event_sample_data_t), intent(inout), optional :: data
    logical, intent(out), optional :: success
    logical :: exist, ok, closing
    type(event_sample_data_t) :: data_file
    type(string_t) :: string
    integer :: u
    eio%split = .false.
    call eio%common_init (sample, data, extension)
    write (msg_buffer, "(A,A,A)")  "Events: reading from LHEF file '", &
         char (eio%filename), "'"
    call msg_message ()
    inquire (file = char (eio%filename), exist = exist)
    if (.not. exist)  call msg_fatal ("Events: LHEF file not found.")
    eio%reading = .true.
    u = eio%unit
    open (u, file = char (eio%filename), &
         action = "read", status = "old")
    call eio%cstream%init (u)
    call eio%read_header ()
    call eio%tag_init%read (eio%cstream, ok)
    if (.not. ok)  call err_init
    select case (eio%version)
    case ("1.0");  call eio%read_init_10 (data_file)
       call eio%tag_init%read_content (eio%cstream, string, closing)
       if (string /= "" .or. .not. closing)  call err_init
    case ("2.0");  call eio%read_init_20 (data_file)
    case ("3.0");  call eio%read_init_30 (data_file)
    end select
    call eio%merge_data (data, data_file)
    if (present (success))  success = .true.

  contains
    
    subroutine err_init
      call msg_fatal ("LHEF: syntax error in init tag")
    end subroutine err_init
      
  end subroutine eio_lhef_init_in
    
  subroutine eio_merge_data (eio, data, data_file)
    class(eio_lhef_t), intent(inout) :: eio
    type(event_sample_data_t), intent(inout) :: data
    type(event_sample_data_t), intent(in) :: data_file
    real, parameter :: tolerance = 1000 * epsilon (1._default)
    if (data%unweighted .neqv. data_file%unweighted)  call err_weights
    if (data%negative_weights .neqv. data_file%negative_weights) &
         call err_weights
    if (data%norm_mode /= data_file%norm_mode)  call err_norm
    if (data%n_beam /= data_file%n_beam)  call err_beams
    if (any (data%pdg_beam /= data_file%pdg_beam))  call err_beams
    if (any (abs ((data%energy_beam - data_file%energy_beam)) &
         > (data%energy_beam + data_file%energy_beam) * tolerance)) &
         call err_beams
    if (data%n_proc /= data_file%n_proc)  call err_proc
    if (any (data%proc_num_id /= data_file%proc_num_id))  call err_proc
    where (data%cross_section == 0)
       data%cross_section = data_file%cross_section
       data%error = data_file%error
    end where
    data%total_cross_section = sum (data%cross_section)
    if (data_file%n_evt > 0) then
       if (data%n_evt > 0 .and. data_file%n_evt /= data%n_evt)  call err_n_evt
       data%n_evt = data_file%n_evt
    end if
  contains
    subroutine err_weights
      call msg_fatal ("LHEF: mismatch in event weight properties")
    end subroutine err_weights
    subroutine err_norm
      call msg_fatal ("LHEF: mismatch in event normalization")
    end subroutine err_norm
    subroutine err_beams
      call msg_fatal ("LHEF: mismatch in beam properties")
    end subroutine err_beams
    subroutine err_proc
      call msg_fatal ("LHEF: mismatch in process definitions")
    end subroutine err_proc
    subroutine err_n_evt
      call msg_error ("LHEF: mismatch in specified number of events (ignored)")
    end subroutine err_n_evt
  end subroutine eio_merge_data
    
  subroutine eio_lhef_switch_inout (eio, success)
    class(eio_lhef_t), intent(inout) :: eio
    logical, intent(out), optional :: success
    call msg_bug ("LHEF: in-out switch not supported")
    if (present (success))  success = .false.
  end subroutine eio_lhef_switch_inout
  
  subroutine eio_lhef_split_out (eio)
    class(eio_lhef_t), intent(inout) :: eio
    integer :: u
    if (eio%split) then
       eio%split_index = eio%split_index + 1
       call eio%set_filename ()
       write (msg_buffer, "(A,A,A)")  "Events: writing to LHEF file '", &
            char (eio%filename), "'"
       call msg_message ()
       call eio%write_footer ()
       u = eio%unit
       close (u)
       open (u, file = char (eio%filename), &
            action = "write", status = "replace")
       call eio%write_header ()
       call eio%tag_init%write (u);  write (u, *)
       call heprup_write_lhef (u)
       select case (eio%version)
       case ("2.0");  call eio%write_init_20 (eio%data)
       case ("3.0");  call eio%write_init_30 (eio%data)
       end select
       call eio%tag_init%close (u);  write (u, *)
    end if
  end subroutine eio_lhef_split_out
  
  subroutine eio_lhef_output (eio, event, i_prc, reading)
    class(eio_lhef_t), intent(inout) :: eio
    type(event_t), intent(in), target :: event
    integer, intent(in) :: i_prc
    logical, intent(in), optional :: reading
    integer :: u
    u = output_unit (eio%unit);  if (u < 0)  return
    if (eio%writing) then
       call hepeup_from_event (event, &
            process_index = eio%proc_num_id (i_prc), &
            keep_beams = eio%keep_beams)
       write (u, '(A)') "<event>"
       call hepeup_write_lhef (eio%unit)
       select case (eio%version)
       case ("2.0");  call eio%write_event_20 (event)
       case ("3.0");  call eio%write_event_30 (event)
       end select
       write (u, '(A)') "</event>"
    else
       call eio%write ()
       call msg_fatal ("LHEF file is not open for writing")
    end if
  end subroutine eio_lhef_output

  subroutine eio_lhef_input_i_prc (eio, i_prc, iostat)
    class(eio_lhef_t), intent(inout) :: eio
    integer, intent(out) :: i_prc
    integer, intent(out) :: iostat
    integer :: i, proc_num_id
    type(string_t) :: s
    logical :: ok
    iostat = 0
    call eio%tag_lhef%read_content (eio%cstream, s, ok)
    if (ok) then
       if (s == "") then
          iostat = -1
       else
          call err_close
       end if
       return
    else
       call eio%cstream%revert_record (s)
    end if
    call eio%tag_event%read (eio%cstream, ok)
    if (.not. ok) then
       call err_evt1
       return
    end if
    call hepeup_read_lhef (eio%unit)
    call hepeup_get_event_parameters (proc_id = proc_num_id)
    i_prc = 0
    FIND_I_PRC: do i = 1, size (eio%proc_num_id)
       if (eio%proc_num_id(i) == proc_num_id) then
          i_prc = i
          exit FIND_I_PRC
       end if
    end do FIND_I_PRC
    if (i_prc == 0)  call err_index
  contains
    subroutine err_close
      call msg_error ("LHEF: reading events: syntax error in closing tag")
      iostat = 1
    end subroutine
    subroutine err_evt1
      call msg_error ("LHEF: reading events: invalid event tag, &
           &aborting read")
      iostat = 2
    end subroutine err_evt1
    subroutine err_index
      call msg_error ("LHEF: reading events: undefined process ID " &
           // char (str (proc_num_id)) // ", aborting read")
      iostat = 3
    end subroutine err_index
  end subroutine eio_lhef_input_i_prc

  subroutine eio_lhef_input_event (eio, event, iostat)
    class(eio_lhef_t), intent(inout) :: eio
    type(event_t), intent(inout), target :: event
    integer, intent(out) :: iostat
    type(particle_set_t), pointer :: pset
    type(string_t) :: s
    logical :: closing
    iostat = 0
    call event%reset ()
    call event%select (1, 1, 1)
    call hepeup_to_event (event, recover_beams = eio%recover_beams)
    if (associated (event%process)) then
       pset => event%get_particle_set_ptr ()
       call particle_set_set_model (pset, event%process%get_model_ptr ())
    end if
    select case (eio%version)
    case ("1.0")
       call eio%tag_event%read_content (eio%cstream, s, closing = closing)
       if (s /= "" .or. .not. closing)  call err_evt2
    case ("2.0");  call eio%read_event_20 (event)
    case ("3.0");  call eio%read_event_30 (event)
    end select
  contains
    subroutine err_evt2
      call msg_error ("LHEF: reading events: syntax error in event record, &
           &aborting read")
      iostat = 2
    end subroutine err_evt2

  end subroutine eio_lhef_input_event

  subroutine eio_lhef_write_header (eio)
    class(eio_lhef_t), intent(in) :: eio
    integer :: u
    u = output_unit (eio%unit);  if (u < 0)  return
    call eio%tag_lhef%write (u);  write (u, *)
    call eio%tag_head%write (u);  write (u, *)
    select case (eio%version)
    case ("1.0")
       write (u, "(2x)", advance = "no")
       call eio%tag_gen_n%write (var_str ("WHIZARD"), u)
       write (u, *)
       write (u, "(2x)", advance = "no")
       call eio%tag_gen_v%write (var_str ("2.2.0"), u)
       write (u, *)
    end select
    call eio%tag_head%close (u);  write (u, *)
  end subroutine eio_lhef_write_header

  subroutine eio_lhef_write_footer (eio)
    class(eio_lhef_t), intent(in) :: eio
    integer :: u
    u = output_unit (eio%unit);  if (u < 0)  return
    call eio%tag_lhef%close (u)
  end subroutine eio_lhef_write_footer

  subroutine eio_lhef_read_header (eio)
    class(eio_lhef_t), intent(inout) :: eio
    logical :: success, closing
    type(string_t) :: content
    call eio%tag_lhef%read (eio%cstream, success)
    if (.not. success .or. .not. eio%tag_lhef%has_content)  call err_lhef
    if (eio%tag_lhef%get_attribute (1) /= eio%version)  call err_version
    call eio%tag_head%read (eio%cstream, success)
    if (.not. success)  call err_header
    if (eio%tag_head%has_content) then
       SKIP_HEADER_CONTENT: do
          call eio%tag_head%read_content (eio%cstream, content, closing)
          if (closing)  exit SKIP_HEADER_CONTENT
       end do SKIP_HEADER_CONTENT
    end if
  contains
    subroutine err_lhef
      call msg_fatal ("LHEF: LesHouchesEvents tag absent or corrupted")
    end subroutine err_lhef
    subroutine err_header
      call msg_fatal ("LHEF: header tag absent or corrupted")
    end subroutine err_header
    subroutine err_version
       call msg_error ("LHEF: version mismatch: expected " &
            // eio%version // ", found " &
            // char (eio%tag_lhef%get_attribute (1)))
    end subroutine err_version
  end subroutine eio_lhef_read_header

  subroutine eio_lhef_read_init_10 (eio, data)
    class(eio_lhef_t), intent(in) :: eio
    type(event_sample_data_t), intent(out) :: data
    integer :: n_proc, i
    call heprup_read_lhef (eio%unit)
    call heprup_get_run_parameters (n_processes = n_proc)
    call data%init (n_proc)
    data%n_beam = 2
    call heprup_get_run_parameters ( &
         unweighted = data%unweighted, &
         negative_weights = data%negative_weights, &
         beam_pdg = data%pdg_beam, &
         beam_energy = data%energy_beam)
    if (data%unweighted) then
       data%norm_mode = NORM_UNIT
    else
       data%norm_mode = NORM_SIGMA
    end if
    do i = 1, n_proc
       call heprup_get_process_parameters (i, &
            process_id = data%proc_num_id(i), &
            cross_section = data%cross_section(i), &
            error = data%error(i))
    end do
  end subroutine eio_lhef_read_init_10
  
  subroutine eio_lhef_write_init_20 (eio, data)
    class(eio_lhef_t), intent(in) :: eio
    type(event_sample_data_t), intent(in) :: data
    integer :: u
    u = eio%unit
    call eio%tag_generator%write (u)
    write (u, "(A)", advance="no")  "WHIZARD"
    call eiO%tag_generator%close (u);  write (u, *)
    call eio%tag_xsecinfo%write (u);  write (u, *)
  end subroutine eio_lhef_write_init_20
    
  subroutine eio_lhef_read_init_20 (eio, data)
    class(eio_lhef_t), intent(inout) :: eio
    type(event_sample_data_t), intent(out) :: data
    real(default), parameter :: pb_per_fb = 1.e-3_default
    type(string_t) :: content
    logical :: found, closing
    call eio_lhef_read_init_10 (eio, data)
    SCAN_INIT_TAGS: do
       call eio%tag_generator%read (eio%cstream, found)
       if (found) then
          if (.not. eio%tag_generator%has_content)  call err_generator
          call eio%tag_generator%read_content (eio%cstream, content, closing)
          call msg_message ("LHEF: Event file has been generated by " &
               // char (content) // " " &
               // char (eio%tag_generator%get_attribute (1)))
          cycle SCAN_INIT_TAGS
       end if
       call eio%tag_xsecinfo%read (eio%cstream, found)
       if (found) then
          if (eio%tag_xsecinfo%has_content)  call err_xsecinfo
          cycle SCAN_INIT_TAGS
       end if
       call eio%tag_init%read_content (eio%cstream, content, closing)
       if (closing) then
          if (content /= "")  call err_init
          exit SCAN_INIT_TAGS
       end if
    end do SCAN_INIT_TAGS
    data%n_evt = &
         read_ival (eio%tag_xsecinfo%get_attribute (1))
    data%total_cross_section = &
         read_rval (eio%tag_xsecinfo%get_attribute (2)) / pb_per_fb
  contains
    subroutine err_generator
      call msg_fatal ("LHEF: invalid generator tag")
    end subroutine err_generator
    subroutine err_xsecinfo
      call msg_fatal ("LHEF: invalid xsecinfo tag")
    end subroutine err_xsecinfo
    subroutine err_init
      call msg_fatal ("LHEF: syntax error after init tag")
    end subroutine err_init
  end subroutine eio_lhef_read_init_20
  
  subroutine eio_lhef_write_event_20 (eio, event)
    class(eio_lhef_t), intent(in) :: eio
    type(event_t), intent(in) :: event
    type(string_t) :: s
    integer :: i, u
    u = eio%unit
    if (eio%write_sqme_ref) then
       s = str (event%sqme_ref)
       call eio%tag_sqme_ref%write (s, u);  write (u, *)
    end if
    if (eio%write_sqme_prc) then
       s = str (event%sqme_prc)
       call eio%tag_sqme_prc%write (s, u);  write (u, *)
    end if
    if (eio%n_alt > 0) then
       if (eio%write_sqme_alt) then
          s = str (event%sqme_alt(1))
          do i = 2, eio%n_alt
             s = s // " " // str (event%sqme_alt(i));  write (u, *)
          end do
          call eio%tag_sqme_alt(1)%write (s, u)
       end if
       s = str (event%weight_alt(1))
       do i = 2, eio%n_alt
          s = s // " " // str (event%weight_alt(i));  write (u, *)
       end do
       call eio%tag_wgts_alt(1)%write (s, u)
    end if
  end subroutine eio_lhef_write_event_20
    
  subroutine eio_lhef_read_event_20 (eio, event)
    class(eio_lhef_t), intent(inout) :: eio
    type(event_t), intent(inout) :: event
    type(string_t) :: content
    logical :: found, closing
    SCAN_EVENT_TAGS: do
       call eio%tag_weight%read (eio%cstream, found)
       if (found) then
          if (.not. eio%tag_weight%has_content)  call err_weight
          call eio%tag_weight%read_content (eio%cstream, content, closing)
          if (.not. closing)  call err_weight
          if (eio%tag_weight%get_attribute (1) == "sqme_prc") then
             call event%set (sqme_ref = read_rval (content))
          end if
          cycle SCAN_EVENT_TAGS
       end if
       call eio%tag_event%read_content (eio%cstream, content, closing)
       if (closing) then
          if (content /= "")  call err_event
          exit SCAN_EVENT_TAGS
       end if
    end do SCAN_EVENT_TAGS
  contains
    subroutine err_weight
      call msg_fatal ("LHEF: invalid weight tag in event record")
    end subroutine err_weight
    subroutine err_event
      call msg_fatal ("LHEF: syntax error after event tag")
    end subroutine err_event
  end subroutine eio_lhef_read_event_20
    
  subroutine eio_lhef_write_init_30 (eio, data)
    class(eio_lhef_t), intent(in) :: eio
    type(event_sample_data_t), intent(in) :: data
    real(default), parameter :: pb_per_fb = 1.e-3_default
    integer :: u, i
    u = output_unit (eio%unit)
    call eio%tag_generator%write (u)
    write (u, "(A)", advance="no")  "WHIZARD"
    call eiO%tag_generator%close (u);  write (u, *)
    call eio%tag_xsecinfo%write (u);  write (u, *)
    if (eio%write_sqme_ref) then
       call eio%tag_sqme_ref%write (u);  write (u, *)
    end if
    if (eio%write_sqme_prc) then
       call eio%tag_sqme_prc%write (u);  write (u, *)
    end if
    if (eio%write_sqme_alt) then
       do i = 1, eio%n_alt
          call eio%tag_sqme_alt(i)%write (u);  write (u, *)
       end do
    end if
    do i = 1, eio%n_alt
       call eio%tag_wgts_alt(i)%write (u);  write (u, *)
    end do
  end subroutine eio_lhef_write_init_30
    
  subroutine eio_lhef_read_init_30 (eio, data)
    class(eio_lhef_t), intent(inout) :: eio
    type(event_sample_data_t), intent(out) :: data
    real(default), parameter :: pb_per_fb = 1.e-3_default
    type(string_t) :: content
    logical :: found, closing
    integer :: n_weightinfo
    call eio_lhef_read_init_10 (eio, data)
    n_weightinfo = 0
    eio%i_weight_sqme = 0
    SCAN_INIT_TAGS: do
       call eio%tag_generator%read (eio%cstream, found)
       if (found) then
          if (.not. eio%tag_generator%has_content)  call err_generator
          call eio%tag_generator%read_content (eio%cstream, content, closing)
          call msg_message ("LHEF: Event file has been generated by " &
               // char (content) // " " &
               // char (eio%tag_generator%get_attribute (1)))
          cycle SCAN_INIT_TAGS
       end if
       call eio%tag_xsecinfo%read (eio%cstream, found)
       if (found) then
          if (eio%tag_xsecinfo%has_content)  call err_xsecinfo
          cycle SCAN_INIT_TAGS
       end if
       call eio%tag_weightinfo%read (eio%cstream, found)
       if (found) then
          if (eio%tag_weightinfo%has_content)  call err_xsecinfo
          n_weightinfo = n_weightinfo + 1
          if (eio%tag_weightinfo%get_attribute (1) == "sqme_prc") then
             eio%i_weight_sqme = n_weightinfo
          end if
          cycle SCAN_INIT_TAGS
       end if
       call eio%tag_init%read_content (eio%cstream, content, closing)
       if (closing) then
          if (content /= "")  call err_init
          exit SCAN_INIT_TAGS
       end if
    end do SCAN_INIT_TAGS
    data%n_evt = &
         read_ival (eio%tag_xsecinfo%get_attribute (1))
    data%total_cross_section = &
         read_rval (eio%tag_xsecinfo%get_attribute (2)) / pb_per_fb
  contains
    subroutine err_generator
      call msg_fatal ("LHEF: invalid generator tag")
    end subroutine err_generator
    subroutine err_xsecinfo
      call msg_fatal ("LHEF: invalid xsecinfo tag")
    end subroutine err_xsecinfo
    subroutine err_init
      call msg_fatal ("LHEF: syntax error after init tag")
    end subroutine err_init
  end subroutine eio_lhef_read_init_30
  
  subroutine eio_lhef_write_event_30 (eio, event)
    class(eio_lhef_t), intent(in) :: eio
    type(event_t), intent(in) :: event
    type(string_t) :: s
    integer :: u, i
    u = eio%unit
    s = ""
    if (eio%write_sqme_ref) then
       s = s // str (event%sqme_ref) // " "
    end if
    if (eio%write_sqme_prc) then
       s = s // str (event%sqme_prc) // " "
    end if
    if (eio%n_alt > 0) then
       if (eio%write_sqme_alt) then
          s = s // str (event%sqme_alt(1)) // " "
          do i = 2, eio%n_alt
             s = s // str (event%sqme_alt(i)) // " "
          end do
       end if
       s = s // str (event%weight_alt(1)) // " "
       do i = 2, eio%n_alt
          s = s // str (event%weight_alt(i)) // " "
       end do
    end if
    if (len_trim (s) > 0) then
       call eio%tag_weights%write (trim (s), u);  write (u, *)
    end if
  end subroutine eio_lhef_write_event_30
    
  subroutine eio_lhef_read_event_30 (eio, event)
    class(eio_lhef_t), intent(inout) :: eio
    type(event_t), intent(inout) :: event
    type(string_t) :: content, string
    logical :: found, closing
    integer :: i
    SCAN_EVENT_TAGS: do
       call eio%tag_weights%read (eio%cstream, found)
       if (found) then
          if (.not. eio%tag_weights%has_content)  call err_weights
          call eio%tag_weights%read_content (eio%cstream, content, closing)
          if (.not. closing)  call err_weights
          if (eio%i_weight_sqme > 0) then
             SCAN_WEIGHTS: do i = 1, eio%i_weight_sqme
                call split (content, string, " ")
                content = adjustl (content)
                if (i == eio%i_weight_sqme) then
                   call event%set (sqme_ref = read_rval (string))
                   exit SCAN_WEIGHTS
                end if
             end do SCAN_WEIGHTS
          end if
          cycle SCAN_EVENT_TAGS
       end if
       call eio%tag_event%read_content (eio%cstream, content, closing)
       if (closing) then
          if (content /= "")  call err_event
          exit SCAN_EVENT_TAGS
       end if
    end do SCAN_EVENT_TAGS
  contains
    subroutine err_weights
      call msg_fatal ("LHEF: invalid weights tag in event record")
    end subroutine err_weights
    subroutine err_event
      call msg_fatal ("LHEF: syntax error after event tag")
    end subroutine err_event
  end subroutine eio_lhef_read_event_30
    
  function str_int (i) result (s)
    integer, intent(in) :: i
    type(string_t) :: s
    character(32) :: buffer
    write (buffer, "(I0)")  i
    s = var_str (trim (buffer))
  end function str_int
  
  function str_real (x) result (s)
    real(default), intent(in) :: x
    type(string_t) :: s
    character(32) :: buffer
    write (buffer, "(ES17.10)")  x
    s = var_str (trim (adjustl (buffer)))
  end function str_real
  
  function read_rval (s) result (rval)
    type(string_t), intent(in) :: s
    real(default) :: rval
    character(80) :: buffer
    buffer = s
    read (buffer, *)  rval
  end function read_rval
    
  function read_ival (s) result (ival)
    type(string_t), intent(in) :: s
    integer :: ival
    character(80) :: buffer
    buffer = s
    read (buffer, *)  ival
  end function read_ival
    

  subroutine eio_lhef_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (eio_lhef_1, "eio_lhef_1", &
         "write version 1.0", &
         u, results)
    call test (eio_lhef_2, "eio_lhef_2", &
         "write version 2.0", &
         u, results)
    call test (eio_lhef_3, "eio_lhef_3", &
         "write version 3.0", &
         u, results)
    call test (eio_lhef_4, "eio_lhef_4", &
         "read version 1.0", &
         u, results)
    call test (eio_lhef_5, "eio_lhef_5", &
         "read version 2.0", &
         u, results)
    call test (eio_lhef_6, "eio_lhef_6", &
         "read version 3.0", &
         u, results)
  end subroutine eio_lhef_test
  
  subroutine eio_lhef_1 (u)
    integer, intent(in) :: u
    type(model_list_t) :: model_list
    type(event_t), allocatable, target :: event
    type(process_t), allocatable, target :: process
    type(process_ptr_t) :: process_ptr
    type(process_instance_t), allocatable, target :: process_instance
    type(event_sample_data_t) :: data
    class(eio_t), allocatable :: eio
    type(string_t) :: sample
    integer :: u_file, iostat
    character(80) :: buffer

    write (u, "(A)")  "* Test output: eio_lhef_1"
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
    
    call data%init (1)
    data%n_evt = 1
    data%n_beam = 2
    data%unweighted = .true.
    data%norm_mode = NORM_UNIT
    data%pdg_beam = 25
    data%energy_beam = 500
    data%proc_num_id = [42]
    data%cross_section(1) = 100
    data%error(1) = 1
    data%total_cross_section = sum (data%cross_section)

    write (u, "(A)")
    write (u, "(A)")  "* Generate and write an event"
    write (u, "(A)")
 
    sample = "eio_lhef_1"
    
    allocate (eio_lhef_t :: eio)
    select type (eio)
    type is (eio_lhef_t)
       call eio%set_parameters ()
    end select
    
    call eio%init_out (sample, [process_ptr], data)
    call event%generate (1, [0._default, 0._default])

    call eio%output (event, i_prc = 1)
    call eio%write (u)
    call eio%final ()

    write (u, "(A)")
    write (u, "(A)")  "* File contents:"
    write (u, "(A)")

    u_file = free_unit ()
    open (u_file, file = char (sample // "." // eio%extension), &
         action = "read", status = "old")
    do
       read (u_file, "(A)", iostat = iostat)  buffer
       if (buffer(1:21) == "  <generator_version>")  buffer = "[...]"
       if (iostat /= 0)  exit
       write (u, "(A)") trim (buffer)
    end do
    close (u_file)
    
    write (u, "(A)")
    write (u, "(A)")  "* Reset data"
    write (u, "(A)")
 
    deallocate (eio)
    allocate (eio_lhef_t :: eio)
    select type (eio)
    type is (eio_lhef_t)
       call eio%set_parameters ()
    end select
    
    select type (eio)
    type is (eio_lhef_t)
       call eio%set_parameters (keep_beams = .true.)
    end select
    call eio%write (u)

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
    write (u, "(A)")  "* Test output end: eio_lhef_1"
    
  end subroutine eio_lhef_1
  
  subroutine eio_lhef_2 (u)
    integer, intent(in) :: u
    type(model_list_t) :: model_list
    type(event_t), allocatable, target :: event
    type(process_t), allocatable, target :: process
    type(process_ptr_t) :: process_ptr
    type(process_instance_t), allocatable, target :: process_instance
    type(event_sample_data_t) :: data
    class(eio_t), allocatable :: eio
    type(string_t) :: sample
    integer :: u_file, iostat
    character(80) :: buffer

    write (u, "(A)")  "* Test output: eio_lhef_2"
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
    
    call data%init (1)
    data%unweighted = .false.
    data%norm_mode = NORM_SIGMA
    data%n_evt = 1
    data%n_beam = 2
    data%pdg_beam = 25
    data%energy_beam = 500
    data%proc_num_id = [42]
    data%cross_section(1) = 100
    data%error(1) = 1
    data%total_cross_section = sum (data%cross_section)

    write (u, "(A)")
    write (u, "(A)")  "* Generate and write an event"
    write (u, "(A)")
 
    sample = "eio_lhef_2"
    
    allocate (eio_lhef_t :: eio)
    select type (eio)
    type is (eio_lhef_t)
       call eio%set_parameters (version = "2.0", write_sqme_prc = .true.)
    end select
    
    call eio%init_out (sample, [process_ptr], data)
    call event%generate (1, [0._default, 0._default])


    call eio%output (event, i_prc = 1)
    call eio%write (u)
    call eio%final ()

    write (u, "(A)")
    write (u, "(A)")  "* File contents:"
    write (u, "(A)")

    u_file = free_unit ()
    open (u_file, file = char (sample // "." // eio%extension), &
         action = "read", status = "old")
    do
       read (u_file, "(A)", iostat = iostat)  buffer
       if (buffer(1:10) == "<generator")  buffer = "[...]"
       if (iostat /= 0)  exit
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
    write (u, "(A)")  "* Test output end: eio_lhef_2"
    
  end subroutine eio_lhef_2
  
  subroutine eio_lhef_3 (u)
    integer, intent(in) :: u
    type(model_list_t) :: model_list
    type(event_t), allocatable, target :: event
    type(process_t), allocatable, target :: process
    type(process_ptr_t) :: process_ptr
    type(process_instance_t), allocatable, target :: process_instance
    type(event_sample_data_t) :: data
    class(eio_t), allocatable :: eio
    type(string_t) :: sample
    integer :: u_file, iostat
    character(80) :: buffer

    write (u, "(A)")  "* Test output: eio_lhef_3"
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
    
    call data%init (1)
    data%unweighted = .false.
    data%norm_mode = NORM_SIGMA
    data%n_evt = 1
    data%n_beam = 2
    data%pdg_beam = 25
    data%energy_beam = 500
    data%proc_num_id = [42]
    data%cross_section(1) = 100
    data%error(1) = 1
    data%total_cross_section = sum (data%cross_section)

    write (u, "(A)")
    write (u, "(A)")  "* Generate and write an event"
    write (u, "(A)")
 
    sample = "eio_lhef_3"
    
    allocate (eio_lhef_t :: eio)
    select type (eio)
    type is (eio_lhef_t)
       call eio%set_parameters (version = "3.0", write_sqme_prc = .true.)
    end select
    
    call eio%init_out (sample, [process_ptr], data)
    call event%generate (1, [0._default, 0._default])


    call eio%output (event, i_prc = 1)
    call eio%write (u)
    call eio%final ()

    write (u, "(A)")
    write (u, "(A)")  "* File contents:"
    write (u, "(A)")

    u_file = free_unit ()
    open (u_file, file = char (sample // ".lhe"), &
         action = "read", status = "old")
    do
       read (u_file, "(A)", iostat = iostat)  buffer
       if (buffer(1:10) == "<generator")  buffer = "[...]"
       if (iostat /= 0)  exit
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
    write (u, "(A)")  "* Test output end: eio_lhef_3"
    
  end subroutine eio_lhef_3
  
  subroutine eio_lhef_4 (u)
    integer, intent(in) :: u
    type(model_list_t) :: model_list
    type(event_t), allocatable, target :: event
    type(process_t), allocatable, target :: process
    type(process_ptr_t) :: process_ptr
    type(process_instance_t), allocatable, target :: process_instance
    type(event_sample_data_t) :: data
    class(eio_t), allocatable :: eio
    type(string_t) :: sample
    integer :: u_file, iostat, i_prc

    write (u, "(A)")  "* Test output: eio_lhef_4"
    write (u, "(A)")  "*   Purpose: read a LHEF 1.0 file"
    write (u, "(A)")

    write (u, "(A)")  "* Write a LHEF data file"
    write (u, "(A)")
 
    u_file = free_unit ()
    sample = "eio_lhef_4"
    open (u_file, file = char (sample // ".lhe"), &
         status = "replace", action = "readwrite")
    
    write (u_file, "(A)")  '<LesHouchesEvents version="1.0">'
    write (u_file, "(A)")  '<header>'
    write (u_file, "(A)")  '  <arbitrary_tag opt="foo">content</arbitrary_tag>'
    write (u_file, "(A)")  '  Text'
    write (u_file, "(A)")  '  <another_tag />'
    write (u_file, "(A)")  '</header>'
    write (u_file, "(A)")  '<init>'
    write (u_file, "(A)")  ' 25 25  5.0000000000E+02  5.0000000000E+02 &
         & -1 -1 -1 -1 3 1'
    write (u_file, "(A)")  '  1.0000000000E-01  1.0000000000E-03 &
         & 1.0000000000E+00 42'
    write (u_file, "(A)")  '</init>'
    write (u_file, "(A)")  '<event>'
    write (u_file, "(A)")  ' 4 42  3.0574068604E+08  1.0000000000E+03 &
         & -1.0000000000E+00 -1.0000000000E+00'
    write (u_file, "(A)")  ' 25 -1 0 0 0 0  0.0000000000E+00  0.0000000000E+00 &
         & 4.8412291828E+02  5.0000000000E+02  1.2500000000E+02 &
         & 0.0000000000E+00  9.0000000000E+00'
    write (u_file, "(A)")  ' 25 -1 0 0 0 0  0.0000000000E+00  0.0000000000E+00 &
         &-4.8412291828E+02  5.0000000000E+02  1.2500000000E+02 &
         & 0.0000000000E+00  9.0000000000E+00'
    write (u_file, "(A)")  ' 25 1 1 2 0 0 -1.4960220911E+02 -4.6042825611E+02 &
         & 0.0000000000E+00  5.0000000000E+02  1.2500000000E+02 &
         & 0.0000000000E+00  9.0000000000E+00'
    write (u_file, "(A)")  ' 25 1 1 2 0 0  1.4960220911E+02  4.6042825611E+02 &
         & 0.0000000000E+00  5.0000000000E+02  1.2500000000E+02 &
         & 0.0000000000E+00  9.0000000000E+00'
    write (u_file, "(A)")  '</event>'
    write (u_file, "(A)")  '</LesHouchesEvents>'
    close (u_file)
    

    write (u, "(A)")  "* Initialize test process"
    write (u, "(A)")

    call syntax_model_file_init ()
 
    allocate (process)
    process_ptr%ptr => process
    allocate (process_instance)
    call prepare_test_process (process, process_instance, model_list)
    call process_instance%setup_event_data ()
 
    allocate (event)
    call event%basic_init ()
    call event%connect (process_instance, process%get_model_ptr ())

    allocate (eio_lhef_t :: eio)
    select type (eio)
    type is (eio_lhef_t)
       call eio%set_parameters (recover_beams = .false.)
    end select
    
    call data%init (1)
    data%n_beam = 2
    data%unweighted = .true.
    data%norm_mode = NORM_UNIT
    data%pdg_beam = 25
    data%energy_beam = 500
    data%proc_num_id = [42]
    data%norm_mode = NORM_UNIT
    call data%write (u)
    write (u, *)

    write (u, "(A)")  "* Initialize and read header"
    write (u, "(A)")

    call eio%init_in (sample, [process_ptr], data)
    call eio%write (u)
    
    write (u, *)
    
    select type (eio)
    type is (eio_lhef_t)
       call eio%tag_lhef%write (u);  write (u, *)
    end select

    write (u, *)
    call data%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Read event"
    write (u, "(A)")

    call eio%input_i_prc (i_prc, iostat)
    
    select type (eio)
    type is (eio_lhef_t)
       write (u, "(A,I0,A,I0)")  "Found process #", i_prc, &
            " with ID = ", eio%proc_num_id(i_prc)
    end select
   
    call eio%input_event (event, iostat)

    call event%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Read closing"
    write (u, "(A)")

    call eio%input_i_prc (i_prc, iostat)
    write (u, "(A,I0)")  "iostat = ", iostat
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
 
    call eio%final ()
    call event%final ()
    deallocate (event)
 
    call cleanup_test_process (process, process_instance)
    deallocate (process_instance)
    deallocate (process)
    
    call model_list%final ()
    call syntax_model_file_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: eio_lhef_4"
    
  end subroutine eio_lhef_4
  
  subroutine eio_lhef_5 (u)
    integer, intent(in) :: u
    type(model_list_t) :: model_list
    type(event_t), allocatable, target :: event
    type(process_t), allocatable, target :: process
    type(process_ptr_t) :: process_ptr
    type(process_instance_t), allocatable, target :: process_instance
    type(event_sample_data_t) :: data
    class(eio_t), allocatable :: eio
    type(string_t) :: sample
    integer :: u_file, iostat, i_prc

    write (u, "(A)")  "* Test output: eio_lhef_5"
    write (u, "(A)")  "*   Purpose: read a LHEF 2.0 file"
    write (u, "(A)")

    write (u, "(A)")  "* Write a LHEF data file"
    write (u, "(A)")
 
    u_file = free_unit ()
    sample = "eio_lhef_5"
    open (u_file, file = char (sample // ".lhe"), &
         status = "replace", action = "readwrite")
    
    write (u_file, "(A)")  '<LesHouchesEvents version="2.0">'
    write (u_file, "(A)")  '<header>'
    write (u_file, "(A)")  '</header>'
    write (u_file, "(A)")  '<init>'
    write (u_file, "(A)")  ' 25 25  5.0000000000E+02  5.0000000000E+02 &
         &-1 -1 -1 -1 4 1'
    write (u_file, "(A)")  '  1.0000000000E-01  1.0000000000E-03 &
         & 0.0000000000E+00 42'
    write (u_file, "(A)")  '<generator version="2.2.0">WHIZARD&
         &</generator>'
    write (u_file, "(A)")  '<xsecinfo neve="1" totxsec="1.0000000000E-01" />'
    write (u_file, "(A)")  '</init>'
    write (u_file, "(A)")  '<event>'
    write (u_file, "(A)")  ' 4 42  3.0574068604E+08  1.0000000000E+03 &
         &-1.0000000000E+00 -1.0000000000E+00'
    write (u_file, "(A)")  ' 25 -1 0 0 0 0  0.0000000000E+00 &
         & 0.0000000000E+00  4.8412291828E+02  5.0000000000E+02 &
         & 1.2500000000E+02  0.0000000000E+00  9.0000000000E+00'
    write (u_file, "(A)")  ' 25 -1 0 0 0 0  0.0000000000E+00 &
         & 0.0000000000E+00 -4.8412291828E+02  5.0000000000E+02 &
         & 1.2500000000E+02  0.0000000000E+00  9.0000000000E+00'
    write (u_file, "(A)")  ' 25 1 1 2 0 0 -1.4960220911E+02 &
         &-4.6042825611E+02  0.0000000000E+00  5.0000000000E+02 &
         & 1.2500000000E+02  0.0000000000E+00  9.0000000000E+00'
    write (u_file, "(A)")  ' 25 1 1 2 0 0  1.4960220911E+02 &
         & 4.6042825611E+02  0.0000000000E+00  5.0000000000E+02 &
         & 1.2500000000E+02  0.0000000000E+00  9.0000000000E+00'
    write (u_file, "(A)")  '<weight name="sqme_prc">1.0000000000E+00</weight>'
    write (u_file, "(A)")  '</event>'
    write (u_file, "(A)")  '</LesHouchesEvents>'
    close (u_file)

    write (u, "(A)")  "* Initialize test process"
    write (u, "(A)")

    call syntax_model_file_init ()
 
    allocate (process)
    process_ptr%ptr => process
    allocate (process_instance)
    call prepare_test_process (process, process_instance, model_list)
    call process_instance%setup_event_data ()
 
    allocate (event)
    call event%basic_init ()
    call event%connect (process_instance, process%get_model_ptr ())

    allocate (eio_lhef_t :: eio)
    select type (eio)
    type is (eio_lhef_t)
       call eio%set_parameters (version = "2.0", recover_beams = .false.)
    end select
    
    call data%init (1)
    data%unweighted = .false.
    data%norm_mode = NORM_SIGMA
    data%n_beam = 2
    data%pdg_beam = 25
    data%energy_beam = 500
    data%proc_num_id = [42]
    call data%write (u)
    write (u, *)

    write (u, "(A)")  "* Initialize and read header"
    write (u, "(A)")

    call eio%init_in (sample, [process_ptr], data)
    call eio%write (u)
    
    write (u, *)
    
    select type (eio)
    type is (eio_lhef_t)
       call eio%tag_lhef%write (u);  write (u, *)
    end select

    write (u, *)
    call data%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Read event"
    write (u, "(A)")

    call eio%input_i_prc (i_prc, iostat)
    
    select type (eio)
    type is (eio_lhef_t)
       write (u, "(A,I0,A,I0)")  "Found process #", i_prc, &
            " with ID = ", eio%proc_num_id(i_prc)
    end select
   
    call eio%input_event (event, iostat)

    call event%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Read closing"
    write (u, "(A)")

    call eio%input_i_prc (i_prc, iostat)
    write (u, "(A,I0)")  "iostat = ", iostat
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
 
    call eio%final ()
    call event%final ()
    deallocate (event)
 
    call cleanup_test_process (process, process_instance)
    deallocate (process_instance)
    deallocate (process)
    
    call model_list%final ()
    call syntax_model_file_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: eio_lhef_5"
    
  end subroutine eio_lhef_5
  
  subroutine eio_lhef_6 (u)
    integer, intent(in) :: u
    type(model_list_t) :: model_list
    type(event_t), allocatable, target :: event
    type(process_t), allocatable, target :: process
    type(process_ptr_t) :: process_ptr
    type(process_instance_t), allocatable, target :: process_instance
    type(event_sample_data_t) :: data
    class(eio_t), allocatable :: eio
    type(string_t) :: sample
    integer :: u_file, iostat, i_prc

    write (u, "(A)")  "* Test output: eio_lhef_6"
    write (u, "(A)")  "*   Purpose: read a LHEF 3.0 file"
    write (u, "(A)")

    write (u, "(A)")  "* Write a LHEF data file"
    write (u, "(A)")
 
    u_file = free_unit ()
    sample = "eio_lhef_6"
    open (u_file, file = char (sample // ".lhe"), &
         status = "replace", action = "readwrite")
    
    write (u_file, "(A)")  '<LesHouchesEvents version="3.0">'
    write (u_file, "(A)")  '<header>'
    write (u_file, "(A)")  '</header>'
    write (u_file, "(A)")  '<init>'
    write (u_file, "(A)")  ' 25 25  5.0000000000E+02  5.0000000000E+02 &
         &-1 -1 -1 -1 4 1'
    write (u_file, "(A)")  '  1.0000000000E-01  1.0000000000E-03 &
         & 0.0000000000E+00 42'
    write (u_file, "(A)")  '<generator version="2.2.0">WHIZARD&
         &</generator>'
    write (u_file, "(A)")  '<xsecinfo neve="1" totxsec="1.0000000000E-01" />'
    write (u_file, "(A)")  '<weightinfo name="sqme_prc" />'
    write (u_file, "(A)")  '</init>'
    write (u_file, "(A)")  '<event>'
    write (u_file, "(A)")  ' 4 42  3.0574068604E+08  1.0000000000E+03 &
         &-1.0000000000E+00 -1.0000000000E+00'
    write (u_file, "(A)")  ' 25 -1 0 0 0 0  0.0000000000E+00 &
         & 0.0000000000E+00  4.8412291828E+02  5.0000000000E+02 &
         & 1.2500000000E+02  0.0000000000E+00  9.0000000000E+00'
    write (u_file, "(A)")  ' 25 -1 0 0 0 0  0.0000000000E+00 &
         & 0.0000000000E+00 -4.8412291828E+02  5.0000000000E+02 &
         & 1.2500000000E+02  0.0000000000E+00  9.0000000000E+00'
    write (u_file, "(A)")  ' 25 1 1 2 0 0 -1.4960220911E+02 &
         &-4.6042825611E+02  0.0000000000E+00  5.0000000000E+02 &
         & 1.2500000000E+02  0.0000000000E+00  9.0000000000E+00'
    write (u_file, "(A)")  ' 25 1 1 2 0 0  1.4960220911E+02 &
         & 4.6042825611E+02  0.0000000000E+00  5.0000000000E+02 &
         & 1.2500000000E+02  0.0000000000E+00  9.0000000000E+00'
    write (u_file, "(A)")  '<weights>1.0000000000E+00</weights>'
    write (u_file, "(A)")  '</event>'
    write (u_file, "(A)")  '</LesHouchesEvents>'
    close (u_file)

    write (u, "(A)")  "* Initialize test process"
    write (u, "(A)")

    call syntax_model_file_init ()
 
    allocate (process)
    process_ptr%ptr => process
    allocate (process_instance)
    call prepare_test_process (process, process_instance, model_list)
    call process_instance%setup_event_data ()
 
    allocate (event)
    call event%basic_init ()
    call event%connect (process_instance, process%get_model_ptr ())

    allocate (eio_lhef_t :: eio)
    select type (eio)
    type is (eio_lhef_t)
       call eio%set_parameters (version = "3.0", recover_beams = .false.)
    end select
    
    call data%init (1)
    data%unweighted = .false.
    data%norm_mode = NORM_SIGMA
    data%n_beam = 2
    data%pdg_beam = 25
    data%energy_beam = 500
    data%proc_num_id = [42]
    call data%write (u)
    write (u, *)

    write (u, "(A)")  "* Initialize and read header"
    write (u, "(A)")

    call eio%init_in (sample, [process_ptr], data)
    call eio%write (u)
    
    write (u, *)
    
    select type (eio)
    type is (eio_lhef_t)
       call eio%tag_lhef%write (u);  write (u, *)
    end select

    write (u, *)
    call data%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Read event"
    write (u, "(A)")

    call eio%input_i_prc (i_prc, iostat)
    
    select type (eio)
    type is (eio_lhef_t)
       write (u, "(A,I0,A,I0)")  "Found process #", i_prc, &
            " with ID = ", eio%proc_num_id(i_prc)
    end select
   
    call eio%input_event (event, iostat)

    call event%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Read closing"
    write (u, "(A)")

    call eio%input_i_prc (i_prc, iostat)
    write (u, "(A,I0)")  "iostat = ", iostat
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
 
    call eio%final ()
    call event%final ()
    deallocate (event)
 
    call cleanup_test_process (process, process_instance)
    deallocate (process_instance)
    deallocate (process)
    
    call model_list%final ()
    call syntax_model_file_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: eio_lhef_6"
    
  end subroutine eio_lhef_6
  

end module eio_lhef
