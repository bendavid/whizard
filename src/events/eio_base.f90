! WHIZARD 2.3.0 July 21 2016
! 
! Copyright (C) 1999-2016 by 
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

module eio_base
  
  use kinds, only: i64
  use iso_varying_string, string_t => varying_string
  use io_units
  use diagnostics
  use model_data
  use event_base
  use eio_data

  implicit none
  private

  public :: eio_t

  type, abstract :: eio_t
     type(string_t) :: sample
     type(string_t) :: extension
     type(string_t) :: filename
     logical :: has_file = .false.
     logical :: split = .false.
     integer :: split_n_evt = 0
     integer :: split_n_kbytes = 0
     integer :: split_index = 0
     integer :: split_count = 0
     class(model_data_t), pointer :: fallback_model => null ()
   contains
     procedure (eio_write), deferred :: write
     procedure (eio_final), deferred :: final
     procedure :: set_splitting => eio_set_splitting
     procedure :: update_split_count => eio_update_split_count
     procedure :: set_filename => eio_set_filename
     procedure :: set_fallback_model => eio_set_fallback_model
     procedure (eio_init_out), deferred :: init_out
     procedure (eio_init_in), deferred :: init_in
     procedure (eio_switch_inout), deferred :: switch_inout
     procedure :: split_out => eio_split_out
     procedure :: file_size_kbytes => eio_file_size_kbytes
     procedure (eio_output), deferred :: output
     procedure (eio_input_i_prc), deferred :: input_i_prc
     procedure (eio_input_event), deferred :: input_event
     procedure (eio_skip), deferred :: skip
  end type eio_t
  

  abstract interface
     subroutine eio_write (object, unit)
       import
       class(eio_t), intent(in) :: object
       integer, intent(in), optional :: unit
     end subroutine eio_write
  end interface
  
  abstract interface
     subroutine eio_final (object)
       import
       class(eio_t), intent(inout) :: object
     end subroutine eio_final
  end interface
  
  abstract interface
     subroutine eio_init_out (eio, sample, data, success, extension)
       import
       class(eio_t), intent(inout) :: eio
       type(string_t), intent(in) :: sample
       type(event_sample_data_t), intent(in), optional :: data
       logical, intent(out), optional :: success
       type(string_t), intent(in), optional :: extension       
     end subroutine eio_init_out
  end interface

  abstract interface
     subroutine eio_init_in (eio, sample, data, success, extension)
       import
       class(eio_t), intent(inout) :: eio
       type(string_t), intent(in) :: sample
       type(event_sample_data_t), intent(inout), optional :: data
       logical, intent(out), optional :: success
       type(string_t), intent(in), optional :: extension
     end subroutine eio_init_in
  end interface

  abstract interface
     subroutine eio_switch_inout (eio, success)
       import
       class(eio_t), intent(inout) :: eio
       logical, intent(out), optional :: success
     end subroutine eio_switch_inout
  end interface

  abstract interface
     subroutine eio_output (eio, event, i_prc, reading, passed, pacify)
       import
       class(eio_t), intent(inout) :: eio
       class(generic_event_t), intent(in), target :: event
       integer, intent(in) :: i_prc
       logical, intent(in), optional :: reading, passed, pacify
     end subroutine eio_output
  end interface
  
  abstract interface
     subroutine eio_input_i_prc (eio, i_prc, iostat)
       import
       class(eio_t), intent(inout) :: eio
       integer, intent(out) :: i_prc
       integer, intent(out) :: iostat
     end subroutine eio_input_i_prc
  end interface
  
  abstract interface
     subroutine eio_input_event (eio, event, iostat)
       import
       class(eio_t), intent(inout) :: eio
       class(generic_event_t), intent(inout), target :: event
       integer, intent(out) :: iostat
     end subroutine eio_input_event
  end interface
  
  abstract interface
     subroutine eio_skip (eio, iostat)
       import
       class(eio_t), intent(inout) :: eio
       integer, intent(out) :: iostat
     end subroutine eio_skip
  end interface


contains
  
  subroutine eio_set_splitting (eio, data)
    class(eio_t), intent(inout) :: eio
    type(event_sample_data_t), intent(in) :: data
    eio%split = data%split_n_evt > 0 .or. data%split_n_kbytes > 0
    if (eio%split) then
       eio%split_n_evt = data%split_n_evt
       eio%split_n_kbytes = data%split_n_kbytes
       eio%split_index = data%split_index
       eio%split_count = 0
    end if
  end subroutine eio_set_splitting
    
  subroutine eio_update_split_count (eio, increased)
    class(eio_t), intent(inout) :: eio
    logical, intent(out) :: increased
    integer :: split_count_old
    if (eio%split_n_kbytes > 0) then
       split_count_old = eio%split_count
       eio%split_count = eio%file_size_kbytes () / eio%split_n_kbytes
       increased = eio%split_count > split_count_old
    end if
  end subroutine eio_update_split_count
  
  subroutine eio_set_filename (eio)
    class(eio_t), intent(inout) :: eio
    character(32) :: buffer
    if (eio%split) then
       write (buffer, "(I0,'.')")  eio%split_index
       eio%filename = eio%sample // "." // trim (buffer) // eio%extension
       eio%has_file = .true.
    else
       eio%filename = eio%sample // "." // eio%extension
       eio%has_file = .true.
    end if    
  end subroutine eio_set_filename

  subroutine eio_set_fallback_model (eio, model)
    class(eio_t), intent(inout) :: eio
    class(model_data_t), intent(in), target :: model
    eio%fallback_model => model
  end subroutine eio_set_fallback_model
  
  subroutine eio_split_out (eio)
    class(eio_t), intent(inout) :: eio
  end subroutine eio_split_out

  function eio_file_size_kbytes (eio) result (kbytes)
    class(eio_t), intent(in) :: eio
    integer :: kbytes
    integer(i64) :: bytes
    if (eio%has_file) then
       inquire (file = char (eio%filename), size = bytes)
       if (bytes > 0) then
          kbytes = bytes / 1024
       else
          kbytes = 0
       end if
    else
       kbytes = 0
    end if
  end function eio_file_size_kbytes
  

end module eio_base
