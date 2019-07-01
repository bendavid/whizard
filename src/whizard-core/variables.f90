! WHIZARD 2.1.1 September 18 2012
! 
! Copyright (C) 1999-2012 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     Christian Speckner <christian.speckner@physik.uni-freiburg.de>
!     with contributions by Sebastian Schmidt, Daniel Wiesler, Felix Braam
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

module variables

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use lorentz !NODEP!
  use pdg_arrays
  use subevents

  implicit none
  private

  public :: var_entry_t
  public :: obs_unary_int
  public :: obs_unary_real
  public :: obs_binary_int
  public :: obs_binary_real
  public :: var_entry_init_log
  public :: var_entry_init_int
  public :: var_entry_init_real
  public :: var_entry_init_cmplx
  public :: var_entry_init_pdg_array
  public :: var_entry_init_subevt
  public :: var_entry_init_string
  public :: var_entry_init_log_ptr
  public :: var_entry_init_int_ptr
  public :: var_entry_init_real_ptr
  public :: var_entry_init_cmplx_ptr
  public :: var_entry_init_pdg_array_ptr
  public :: var_entry_init_subevt_ptr
  public :: var_entry_init_string_ptr
  public :: var_entry_final
  public :: var_entry_write
  public :: var_entry_get_name
  public :: var_entry_get_type
  public :: var_entry_is_defined
  public :: var_entry_is_locked
  public :: var_entry_is_intrinsic
  public :: var_entry_is_copy
  public :: var_entry_is_known
  public :: var_entry_get_lval
  public :: var_entry_get_ival
  public :: var_entry_get_rval
  public :: var_entry_get_cval
  public :: var_entry_get_aval
  public :: var_entry_get_pval
  public :: var_entry_get_sval
  public :: var_entry_get_known_ptr
  public :: var_entry_get_lval_ptr
  public :: var_entry_get_ival_ptr
  public :: var_entry_get_rval_ptr
  public :: var_entry_get_cval_ptr
  public :: var_entry_get_aval_ptr
  public :: var_entry_get_pval_ptr
  public :: var_entry_get_sval_ptr
  public :: var_entry_get_prt1_ptr
  public :: var_entry_get_prt2_ptr
  public :: var_entry_assign_obs1_int_ptr
  public :: var_entry_assign_obs1_real_ptr
  public :: var_entry_assign_obs2_int_ptr
  public :: var_entry_assign_obs2_real_ptr
  public :: var_entry_set_log
  public :: var_entry_set_int
  public :: var_entry_set_real
  public :: var_entry_set_cmplx
  public :: var_entry_set_pdg_array
  public :: var_entry_set_subevt
  public :: var_entry_set_string
  public :: var_list_t
  public :: var_list_link
  public :: var_list_append_log
  public :: var_list_append_int
  public :: var_list_append_real
  public :: var_list_append_cmplx
  public :: var_list_append_subevt
  public :: var_list_append_pdg_array
  public :: var_list_append_string
  public :: var_list_append_log_ptr
  public :: var_list_append_int_ptr
  public :: var_list_append_real_ptr
  public :: var_list_append_cmplx_ptr
  public :: var_list_append_pdg_array_ptr
  public :: var_list_append_subevt_ptr
  public :: var_list_append_string_ptr
  public :: var_list_final
  public :: var_list_write
  public :: var_list_write_var
  public :: var_list_get_next_ptr
  public :: var_list_get_var_ptr
  public :: var_list_get_type 
  public :: var_list_exists
  public :: var_list_is_intrinsic 
  public :: var_list_is_known 
  public :: var_list_is_locked 
  public :: var_list_get_lval
  public :: var_list_get_ival
  public :: var_list_get_rval
  public :: var_list_get_cval
  public :: var_list_get_pval
  public :: var_list_get_aval
  public :: var_list_get_sval
  public :: var_list_init_num_id
  public :: var_list_init_process_results
  public :: var_list_set_observables_unary
  public :: var_list_set_observables_binary
  public :: event_vars_t
  public :: event_vars_write
  public :: event_vars_write_raw
  public :: event_vars_read_raw
  public :: var_list_append_event_vars
  public :: var_list_set_log
  public :: var_list_set_int
  public :: var_list_set_real
  public :: var_list_set_cmplx
  public :: var_list_set_subevt
  public :: var_list_set_pdg_array
  public :: var_list_set_string
  public :: var_list_init_copy
  public :: var_list_init_copies
  public :: var_list_set_original_pointer
  public :: var_list_synchronize
  public :: var_list_restore
  public :: var_list_undefine
  public :: var_list_init_snapshot
  public :: var_list_check_user_var

  integer, parameter, public :: V_NONE = 0, V_LOG = 1, V_INT = 2, V_REAL = 3
  integer, parameter, public :: V_CMPLX = 4, V_SEV = 5, V_PDG = 6, V_STR = 7
  integer, parameter, public :: V_OBS1_INT = 11, V_OBS2_INT = 12
  integer, parameter, public :: V_OBS1_REAL = 21, V_OBS2_REAL = 22
  integer, parameter, public :: V_UOBS1_INT = 31, V_UOBS2_INT = 32
  integer, parameter, public :: V_UOBS1_REAL = 41, V_UOBS2_REAL = 42


  type :: var_entry_t
     private
     integer :: type = V_NONE
     type(string_t) :: name
     logical :: is_allocated = .false.
     logical :: is_defined = .false.
     logical :: is_locked = .false.
     logical :: is_copy = .false.
     type(var_entry_t), pointer :: original => null ()
     logical :: is_intrinsic = .false.
     logical :: is_user_var = .false.
     logical, pointer :: is_known => null ()
     logical,           pointer :: lval => null ()
     integer,           pointer :: ival => null ()
     real(default),     pointer :: rval => null ()
     complex(default), pointer :: cval => null ()
     type(subevt_t),  pointer :: pval => null ()
     type(pdg_array_t), pointer :: aval => null ()
     type(string_t),    pointer :: sval => null ()
     procedure(obs_unary_int),   nopass, pointer :: obs1_int  => null ()
     procedure(obs_unary_real),  nopass, pointer :: obs1_real => null ()
     procedure(obs_binary_int),  nopass, pointer :: obs2_int  => null ()
     procedure(obs_binary_real), nopass, pointer :: obs2_real => null ()
     type(prt_t), pointer :: prt1 => null ()
     type(prt_t), pointer :: prt2 => null ()
     type(var_entry_t), pointer :: next => null ()
  end type var_entry_t

  type :: var_list_t
     private
     type(var_entry_t), pointer :: first => null ()
     type(var_entry_t), pointer :: last => null ()
     type(var_list_t), pointer :: next => null ()
  end type var_list_t

  type :: event_vars_t
     integer :: event_index = 0
     integer :: process_index = 0
     integer :: process_num_id = 0
     type(string_t) :: process_id
     integer :: n_in = 0
     integer :: n_out = 0
     integer :: n_tot = 0
     real(default) :: sqrts = 0
     real(default) :: sqrts_hat = 0
     real(default) :: sqme = 0
     real(default) :: sqme_ref = 0
     real(default) :: weight = 0
     real(default) :: excess = 0
  end type event_vars_t


  abstract interface
     function obs_unary_int (prt1) result (ival)
       import
       integer :: ival
       type(prt_t), intent(in) :: prt1
     end function obs_unary_int
  end interface
  abstract interface
     function obs_unary_real (prt1) result (rval)
       import
       real(default) :: rval
       type(prt_t), intent(in) :: prt1
     end function obs_unary_real
  end interface
  abstract interface
     function obs_binary_int (prt1, prt2) result (ival)
       import
       integer :: ival
       type(prt_t), intent(in) :: prt1, prt2
     end function obs_binary_int
  end interface
  abstract interface
     function obs_binary_real (prt1, prt2) result (rval)
       import
       real(default) :: rval
       type(prt_t), intent(in) :: prt1, prt2
     end function obs_binary_real
  end interface

  interface var_list_append_log
     module procedure var_list_append_log_s
     module procedure var_list_append_log_c
  end interface
  interface var_list_append_int
     module procedure var_list_append_int_s
     module procedure var_list_append_int_c
  end interface
  interface var_list_append_real
     module procedure var_list_append_real_s
     module procedure var_list_append_real_c
  end interface
  interface var_list_append_cmplx
     module procedure var_list_append_cmplx_s
     module procedure var_list_append_cmplx_c
  end interface
  interface var_list_append_subevt
     module procedure var_list_append_subevt_s
     module procedure var_list_append_subevt_c
  end interface
  interface var_list_append_pdg_array
     module procedure var_list_append_pdg_array_s
     module procedure var_list_append_pdg_array_c
  end interface
  interface var_list_append_string
     module procedure var_list_append_string_s
     module procedure var_list_append_string_c
  end interface
  interface var_list_is_known 
     module procedure var_list_is_known_s
     module procedure var_list_is_known_c
  end interface
  interface var_list_get_lval
     module procedure var_list_get_lval_s
     module procedure var_list_get_lval_c
  end interface
  interface var_list_get_ival
     module procedure var_list_get_ival_s
     module procedure var_list_get_ival_c
  end interface
  interface var_list_get_rval
     module procedure var_list_get_rval_s
     module procedure var_list_get_rval_c
  end interface
  interface var_list_get_cval
     module procedure var_list_get_cval_s
     module procedure var_list_get_cval_c
  end interface
  interface var_list_get_pval
     module procedure var_list_get_pval_s
     module procedure var_list_get_pval_c
  end interface
  interface var_list_get_aval
     module procedure var_list_get_aval_s
     module procedure var_list_get_aval_c
  end interface
  interface var_list_get_sval
     module procedure var_list_get_sval_s
     module procedure var_list_get_sval_c
  end interface


contains

  subroutine var_entry_init_log (var, name, lval, intrinsic, user)
    type(var_entry_t), intent(out) :: var
    type(string_t), intent(in) :: name
    logical, intent(in), optional :: lval
    logical, intent(in), optional :: intrinsic, user
    var%name = name
    var%type = V_LOG
    allocate (var%lval, var%is_known)
    if (present (lval)) then
       var%lval = lval
       var%is_defined = .true.
       var%is_known = .true.
    else
       var%is_known = .false.
    end if
    if (present (intrinsic))  var%is_intrinsic = intrinsic
    if (present (user))  var%is_user_var = user
    var%is_allocated = .true.
  end subroutine var_entry_init_log

  subroutine var_entry_init_int (var, name, ival, intrinsic, user)
    type(var_entry_t), intent(out) :: var
    type(string_t), intent(in) :: name
    integer, intent(in), optional :: ival
    logical, intent(in), optional :: intrinsic, user
    var%name = name
    var%type = V_INT
    allocate (var%ival, var%is_known)
    if (present (ival)) then
       var%ival = ival
       var%is_defined = .true.
       var%is_known = .true.
    else
       var%is_known = .false.
    end if
    if (present (intrinsic))  var%is_intrinsic = intrinsic
    if (present (user))  var%is_user_var = user
    var%is_allocated = .true.
  end subroutine var_entry_init_int

  subroutine var_entry_init_real (var, name, rval, intrinsic, user)
    type(var_entry_t), intent(out) :: var
    type(string_t), intent(in) :: name
    real(default), intent(in), optional :: rval
    logical, intent(in), optional :: intrinsic, user
    var%name = name
    var%type = V_REAL
    allocate (var%rval, var%is_known)
    if (present (rval)) then
       var%rval = rval
       var%is_defined = .true.
       var%is_known = .true.
    else
       var%is_known = .false.
    end if
    if (present (intrinsic))  var%is_intrinsic = intrinsic
    if (present (user))  var%is_user_var = user
    var%is_allocated = .true.
  end subroutine var_entry_init_real
  
  subroutine var_entry_init_cmplx (var, name, cval, intrinsic, user)
    type(var_entry_t), intent(out) :: var
    type(string_t), intent(in) :: name
    complex(default), intent(in), optional :: cval
    logical, intent(in), optional :: intrinsic, user
    var%name = name
    var%type = V_CMPLX
    allocate (var%cval, var%is_known)
    if (present (cval)) then
       var%cval = cval
       var%is_defined = .true.
       var%is_known = .true.
    else
       var%is_known = .false.
    end if
    if (present (intrinsic))  var%is_intrinsic = intrinsic
    if (present (user))  var%is_user_var = user
    var%is_allocated = .true.
  end subroutine var_entry_init_cmplx

  subroutine var_entry_init_subevt (var, name, pval, intrinsic, user)
    type(var_entry_t), intent(out) :: var
    type(string_t), intent(in) :: name
    type(subevt_t), intent(in), optional :: pval
    logical, intent(in), optional :: intrinsic, user
    var%name = name
    var%type = V_SEV
    allocate (var%pval, var%is_known)
    if (present (pval)) then
       var%pval = pval
       var%is_defined = .true.
       var%is_known = .true.
    else
       var%is_known = .false.
    end if
    if (present (intrinsic))  var%is_intrinsic = intrinsic
    if (present (user))  var%is_user_var = user
    var%is_allocated = .true.
  end subroutine var_entry_init_subevt

  subroutine var_entry_init_pdg_array (var, name, aval, intrinsic, user)
    type(var_entry_t), intent(out) :: var
    type(string_t), intent(in) :: name
    type(pdg_array_t), intent(in), optional :: aval
    logical, intent(in), optional :: intrinsic, user
    var%name = name
    var%type = V_PDG
    allocate (var%aval, var%is_known)
    if (present (aval)) then
       var%aval = aval
       var%is_defined = .true.
       var%is_known = .true.
    else
       var%is_known = .false.
    end if
    if (present (intrinsic))  var%is_intrinsic = intrinsic
    if (present (user))  var%is_user_var = user
    var%is_allocated = .true.
  end subroutine var_entry_init_pdg_array

  subroutine var_entry_init_string (var, name, sval, intrinsic, user)
    type(var_entry_t), intent(out) :: var
    type(string_t), intent(in) :: name
    type(string_t), intent(in), optional :: sval
    logical, intent(in), optional :: intrinsic, user
    var%name = name
    var%type = V_STR
    allocate (var%sval, var%is_known)
    if (present (sval)) then
       var%sval = sval
       var%is_defined = .true.
       var%is_known = .true.
    else
       var%is_known = .false.
    end if
    if (present (intrinsic))  var%is_intrinsic = intrinsic
    if (present (user))  var%is_user_var = user
    var%is_allocated = .true.
  end subroutine var_entry_init_string

  subroutine var_entry_init_log_ptr (var, name, lval, is_known, intrinsic)
    type(var_entry_t), intent(out) :: var
    type(string_t), intent(in) :: name
    logical, intent(in), target :: lval
    logical, intent(in), target :: is_known
    logical, intent(in), optional :: intrinsic
    var%name = name
    var%type = V_LOG
    var%lval => lval
    var%is_known => is_known
    if (present (intrinsic))  var%is_intrinsic = intrinsic
    var%is_defined = .true.
  end subroutine var_entry_init_log_ptr

  subroutine var_entry_init_int_ptr (var, name, ival, is_known, intrinsic)
    type(var_entry_t), intent(out) :: var
    type(string_t), intent(in) :: name
    integer, intent(in), target :: ival
    logical, intent(in), target :: is_known
    logical, intent(in), optional :: intrinsic
    var%name = name
    var%type = V_INT
    var%ival => ival
    var%is_known => is_known
    if (present (intrinsic))  var%is_intrinsic = intrinsic
    var%is_defined = .true.
  end subroutine var_entry_init_int_ptr

  subroutine var_entry_init_real_ptr (var, name, rval, is_known, intrinsic)
    type(var_entry_t), intent(out) :: var
    type(string_t), intent(in) :: name
    real(default), intent(in), target :: rval
    logical, intent(in), target :: is_known
    logical, intent(in), optional :: intrinsic
    var%name = name
    var%type = V_REAL
    var%rval => rval
    var%is_known => is_known
    if (present (intrinsic))  var%is_intrinsic = intrinsic
    var%is_defined = .true.
  end subroutine var_entry_init_real_ptr
 
  subroutine var_entry_init_cmplx_ptr (var, name, cval, is_known, intrinsic)
    type(var_entry_t), intent(out) :: var
    type(string_t), intent(in) :: name
    complex(default), intent(in), target :: cval
    logical, intent(in), target :: is_known
    logical, intent(in), optional :: intrinsic
    var%name = name
    var%type = V_CMPLX
    var%cval => cval
    var%is_known => is_known
    if (present (intrinsic))  var%is_intrinsic = intrinsic
    var%is_defined = .true.
  end subroutine var_entry_init_cmplx_ptr

  subroutine var_entry_init_pdg_array_ptr (var, name, aval, is_known, intrinsic)
    type(var_entry_t), intent(out) :: var
    type(string_t), intent(in) :: name
    type(pdg_array_t), intent(in), target :: aval
    logical, intent(in), target :: is_known
    logical, intent(in), optional :: intrinsic
    var%name = name
    var%type = V_PDG
    var%aval => aval
    var%is_known => is_known
    if (present (intrinsic))  var%is_intrinsic = intrinsic
    var%is_defined = .true.
  end subroutine var_entry_init_pdg_array_ptr

  subroutine var_entry_init_subevt_ptr (var, name, pval, is_known, intrinsic)
    type(var_entry_t), intent(out) :: var
    type(string_t), intent(in) :: name
    type(subevt_t), intent(in), target :: pval
    logical, intent(in), target :: is_known
    logical, intent(in), optional :: intrinsic
    var%name = name
    var%type = V_SEV
    var%pval => pval
    var%is_known => is_known
    if (present (intrinsic))  var%is_intrinsic = intrinsic
    var%is_defined = .true.
  end subroutine var_entry_init_subevt_ptr

  subroutine var_entry_init_string_ptr (var, name, sval, is_known, intrinsic)
    type(var_entry_t), intent(out) :: var
    type(string_t), intent(in) :: name
    type(string_t), intent(in), target :: sval
    logical, intent(in), target :: is_known
    logical, intent(in), optional :: intrinsic
    var%name = name
    var%type = V_STR
    var%sval => sval
    var%is_known => is_known
    if (present (intrinsic))  var%is_intrinsic = intrinsic
    var%is_defined = .true.
  end subroutine var_entry_init_string_ptr

  subroutine var_entry_init_obs (var, name, type, prt1, prt2)
    type(var_entry_t), intent(out) :: var
    type(string_t), intent(in) :: name
    integer, intent(in) :: type
    type(prt_t), intent(in), target :: prt1
    type(prt_t), intent(in), optional, target :: prt2
    var%type = type
    var%name = name
    var%prt1 => prt1
    if (present (prt2))  var%prt2 => prt2
    var%is_intrinsic = .true.
    var%is_defined = .true.
  end subroutine var_entry_init_obs

  subroutine var_entry_undefine (var)
    type(var_entry_t), intent(inout) :: var
    var%is_defined = .not. var%is_user_var
    var%is_known = var%is_defined .and. var%is_known
  end subroutine var_entry_undefine

  subroutine var_entry_lock (var, locked)
    type(var_entry_t), intent(inout) :: var
    logical, intent(in), optional :: locked
    if (present (locked)) then
       var%is_locked = locked
    else
       var%is_locked = .true.
    end if
  end subroutine var_entry_lock

  subroutine var_entry_final (var)
    type(var_entry_t), intent(inout) :: var
    if (var%is_allocated) then
       select case (var%type)
       case (V_LOG); deallocate (var%lval)
       case (V_INT); deallocate (var%ival)
       case (V_REAL);deallocate (var%rval)
       case (V_CMPLX);deallocate (var%cval)
       case (V_SEV); deallocate (var%pval)
       case (V_PDG); deallocate (var%aval)
       case (V_STR); deallocate (var%sval)
       end select
       deallocate (var%is_known)
       var%is_allocated = .false.
       var%is_defined = .false.
    end if
  end subroutine var_entry_final

  recursive subroutine var_entry_write (var, unit, model_name, show_ptr, &
       intrinsic)
    type(var_entry_t), intent(in) :: var
    integer, intent(in), optional :: unit
    type(string_t), intent(in), optional :: model_name
    logical, intent(in), optional :: show_ptr
    logical, intent(in), optional :: intrinsic
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    if (present (intrinsic)) then
       if (var%is_intrinsic .neqv. intrinsic)  return
    end if
    if (.not. var%is_defined) then
       write (u, "(A,1x)", advance="no")  "[undefined]"
    end if
    if (.not. var%is_intrinsic) then
       write (u, "(A,1x)", advance="no")  "[user variable]"
    end if
    if (associated (var%original)) then
       if (present (model_name)) then
          write (u, "(A,A)", advance="no")  char(model_name), "."
       end if
    end if
    write (u, "(A)", advance="no")  char (var%name)
    if (var%is_locked)  write (u, "(A)", advance="no")  "*"
    if (var%is_allocated) then
       write (u, "(A)", advance="no")  " = "
    else if (var%type /= V_NONE) then
       write (u, "(A)", advance="no")  " => "
    end if
    select case (var%type)
    case (V_NONE); write (u, *)
    case (V_LOG)
       if (var%is_known) then
          if (var%lval) then
             write (u, "(A)")  "true"
          else
             write (u, "(A)")  "false"
          end if
       else
          write (u, "(A)")  "[unknown logical]"
       end if
    case (V_INT)
       if (var%is_known) then
          write (u, *)  var%ival
       else
          write (u, "(A)")  "[unknown integer]"
       end if
    case (V_REAL)
       if (var%is_known) then
          write (u, *)  var%rval
       else
          write (u, "(A)")  "[unknown real]"
       end if
    case (V_CMPLX)
       if (var%is_known) then
          write (u, *)  char (cmplx2string (var%cval))
       else
          write (u, "(A)")  "[unknown complex]"
       end if
    case (V_SEV)
       if (var%is_known) then
          call subevt_write (var%pval, unit, prefix="       ")
       else
          write (u, "(A)")  "[unknown subevent]"
       end if
    case (V_PDG)
       if (var%is_known) then
          call pdg_array_write (var%aval, u);  write (u, *)
       else
          write (u, "(A)")  "[unknown PDG array]"
       end if
    case (V_STR)
       if (var%is_known) then
          write (u, "(A)")  '"' // char (var%sval) // '"'
       else
          write (u, "(A)")  "[unknown string]"
       end if
    case (V_OBS1_INT);  write (u, *) "[int] = unary observable"
    case (V_OBS2_INT);  write (u, *) "[int] = binary observable"
    case (V_OBS1_REAL); write (u, *) "[real] = unary observable"
    case (V_OBS2_REAL); write (u, *) "[real] = binary observable"
    case (V_UOBS1_INT);  write (u, *) "[int] = unary user observable"
    case (V_UOBS2_INT);  write (u, *) "[int] = binary user observable"
    case (V_UOBS1_REAL); write (u, *) "[real] = unary user observable"
    case (V_UOBS2_REAL); write (u, *) "[real] = binary user observable"
    end select
    if (present (show_ptr)) then
       if (show_ptr .and. var%is_copy .and. associated (var%original)) then
          write (u, "('  => ')", advance="no")
          call var_entry_write (var%original, unit)
       end if
    end if
  end subroutine var_entry_write

  function var_entry_get_name (var) result (name)
    type(string_t) :: name
    type(var_entry_t), intent(in) :: var
    name = var%name
  end function var_entry_get_name

  function var_entry_get_type (var) result (type)
    integer :: type
    type(var_entry_t), intent(in) :: var
    type = var%type
  end function var_entry_get_type

  function var_entry_is_defined (var) result (defined)
    logical :: defined
    type(var_entry_t), intent(in) :: var
    defined = var%is_defined
  end function var_entry_is_defined

  function var_entry_is_locked (var) result (locked)
    logical :: locked
    type(var_entry_t), intent(in) :: var
    locked = var%is_locked
  end function var_entry_is_locked

  function var_entry_is_intrinsic (var) result (flag)
    logical :: flag
    type(var_entry_t), intent(in) :: var
    flag = var%is_intrinsic
  end function var_entry_is_intrinsic

  function var_entry_is_copy (var) result (flag)
    logical :: flag
    type(var_entry_t), intent(in) :: var
    flag = var%is_copy
  end function var_entry_is_copy

  function var_entry_is_known (var) result (flag)
    logical :: flag
    type(var_entry_t), intent(in) :: var
    flag = var%is_known
  end function var_entry_is_known

  function var_entry_get_lval (var) result (lval)
    logical :: lval
    type(var_entry_t), intent(in) :: var
    lval = var%lval
  end function var_entry_get_lval

  function var_entry_get_ival (var) result (ival)
    integer :: ival
    type(var_entry_t), intent(in) :: var
    ival = var%ival
  end function var_entry_get_ival

  function var_entry_get_rval (var) result (rval)
    real(default) :: rval
    type(var_entry_t), intent(in) :: var
    rval = var%rval
  end function var_entry_get_rval
  
  function var_entry_get_cval (var) result (cval)
    complex(default) :: cval
    type(var_entry_t), intent(in) :: var
    cval = var%cval
  end function var_entry_get_cval

  function var_entry_get_aval (var) result (aval)
    type(pdg_array_t) :: aval
    type(var_entry_t), intent(in) :: var
    aval = var%aval
  end function var_entry_get_aval

  function var_entry_get_pval (var) result (pval)
    type(subevt_t) :: pval
    type(var_entry_t), intent(in) :: var
    pval = var%pval
  end function var_entry_get_pval

  function var_entry_get_sval (var) result (sval)
    type(string_t) :: sval
    type(var_entry_t), intent(in) :: var
    sval = var%sval
  end function var_entry_get_sval

  function var_entry_get_known_ptr (var) result (ptr)
    logical, pointer :: ptr
    type(var_entry_t), intent(in), target :: var
    ptr => var%is_known
  end function var_entry_get_known_ptr

  function var_entry_get_lval_ptr (var) result (ptr)
    logical, pointer :: ptr
    type(var_entry_t), intent(in), target :: var
    ptr => var%lval
  end function var_entry_get_lval_ptr

  function var_entry_get_ival_ptr (var) result (ptr)
    integer, pointer :: ptr
    type(var_entry_t), intent(in), target :: var
    ptr => var%ival
  end function var_entry_get_ival_ptr

  function var_entry_get_rval_ptr (var) result (ptr)
    real(default), pointer :: ptr
    type(var_entry_t), intent(in), target :: var
    ptr => var%rval
  end function var_entry_get_rval_ptr
  
  function var_entry_get_cval_ptr (var) result (ptr)
    complex(default), pointer :: ptr
    type(var_entry_t), intent(in), target :: var
    ptr => var%cval
  end function var_entry_get_cval_ptr

  function var_entry_get_pval_ptr (var) result (ptr)
    type(subevt_t), pointer :: ptr
    type(var_entry_t), intent(in), target :: var
    ptr => var%pval
  end function var_entry_get_pval_ptr

  function var_entry_get_aval_ptr (var) result (ptr)
    type(pdg_array_t), pointer :: ptr
    type(var_entry_t), intent(in), target :: var
    ptr => var%aval
  end function var_entry_get_aval_ptr

  function var_entry_get_sval_ptr (var) result (ptr)
    type(string_t), pointer :: ptr
    type(var_entry_t), intent(in), target :: var
    ptr => var%sval
  end function var_entry_get_sval_ptr

  function var_entry_get_prt1_ptr (var) result (ptr)
    type(prt_t), pointer :: ptr
    type(var_entry_t), intent(in), target :: var
    ptr => var%prt1
  end function var_entry_get_prt1_ptr

  function var_entry_get_prt2_ptr (var) result (ptr)
    type(prt_t), pointer :: ptr
    type(var_entry_t), intent(in), target :: var
    ptr => var%prt2
  end function var_entry_get_prt2_ptr

  subroutine var_entry_assign_obs1_int_ptr (ptr, var)
    procedure(obs_unary_int), pointer :: ptr
    type(var_entry_t), intent(in), target :: var
    ptr => var%obs1_int
  end subroutine var_entry_assign_obs1_int_ptr

  subroutine var_entry_assign_obs1_real_ptr (ptr, var)
    procedure(obs_unary_real), pointer :: ptr
    type(var_entry_t), intent(in), target :: var
    ptr => var%obs1_real
  end subroutine var_entry_assign_obs1_real_ptr

  subroutine var_entry_assign_obs2_int_ptr (ptr, var)
    procedure(obs_binary_int), pointer :: ptr
    type(var_entry_t), intent(in), target :: var
    ptr => var%obs2_int
  end subroutine var_entry_assign_obs2_int_ptr

  subroutine var_entry_assign_obs2_real_ptr (ptr, var)
    procedure(obs_binary_real), pointer :: ptr
    type(var_entry_t), intent(in), target :: var
    ptr => var%obs2_real
  end subroutine var_entry_assign_obs2_real_ptr

  subroutine var_entry_clear_value (var)
    type(var_entry_t), intent(inout) :: var
    var%is_known = .false.
  end subroutine var_entry_clear_value

  recursive subroutine var_entry_set_log &
       (var, lval, is_known, verbose, model_name)
    type(var_entry_t), intent(inout) :: var
    logical, intent(in) :: lval
    logical, intent(in) :: is_known
    logical, intent(in), optional :: verbose
    type(string_t), intent(in), optional :: model_name
    integer :: u
    u = logfile_unit ()
    var%lval = lval
    var%is_known = is_known
    var%is_defined = .true.
    if (associated (var%original)) then
       call var_entry_set_log (var%original, lval, is_known)
    end if
    if (present (verbose)) then
       if (verbose) then
          call var_entry_write (var, model_name=model_name)
          call var_entry_write (var, model_name=model_name, unit=u)
          if (u >= 0) flush (u)
       end if
    end if
  end subroutine var_entry_set_log

  recursive subroutine var_entry_set_int &
       (var, ival, is_known, verbose, model_name)
    type(var_entry_t), intent(inout) :: var
    integer, intent(in) :: ival
    logical, intent(in) :: is_known
    logical, intent(in), optional :: verbose
    type(string_t), intent(in), optional :: model_name
    integer :: u
    u = logfile_unit ()
    var%ival = ival
    var%is_known = is_known
    var%is_defined = .true.
    if (associated (var%original)) then
       call var_entry_set_int (var%original, ival, is_known)
    end if
    if (present (verbose)) then
       if (verbose) then
          call var_entry_write (var, model_name=model_name)
          call var_entry_write (var, model_name=model_name, unit=u)
          if (u >= 0) flush (u)
       end if
    end if
  end subroutine var_entry_set_int

  recursive subroutine var_entry_set_real &
       (var, rval, is_known, verbose, model_name)
    type(var_entry_t), intent(inout) :: var
    real(default), intent(in) :: rval
    logical, intent(in) :: is_known
    logical, intent(in), optional :: verbose
    type(string_t), intent(in), optional :: model_name
    integer :: u
    u = logfile_unit ()
    var%rval = rval
    var%is_known = is_known
    var%is_defined = .true.
    if (associated (var%original)) then
       call var_entry_set_real (var%original, rval, is_known)
    end if
    if (present (verbose)) then
       if (verbose) then
          call var_entry_write (var, model_name=model_name)
          call var_entry_write (var, model_name=model_name, unit=u)
          if (u >= 0) flush (u)
       end if
    end if
  end subroutine var_entry_set_real
  
  recursive subroutine var_entry_set_cmplx &
       (var, cval, is_known, verbose, model_name)
    type(var_entry_t), intent(inout) :: var
    complex(default), intent(in) :: cval
    logical, intent(in) :: is_known
    logical, intent(in), optional :: verbose
    type(string_t), intent(in), optional :: model_name
    integer :: u
    u = logfile_unit ()
    var%cval = cval
    var%is_known = is_known
    var%is_defined = .true.
    if (associated (var%original)) then
       call var_entry_set_cmplx (var%original, cval, is_known)
    end if
    if (present (verbose)) then
       if (verbose) then
          call var_entry_write (var, model_name=model_name)
          call var_entry_write (var, model_name=model_name, unit=u)
          if (u >= 0) flush (u)
       end if
    end if
  end subroutine var_entry_set_cmplx

  recursive subroutine var_entry_set_pdg_array &
       (var, aval, is_known, verbose, model_name)
    type(var_entry_t), intent(inout) :: var
    type(pdg_array_t), intent(in) :: aval
    logical, intent(in) :: is_known
    logical, intent(in), optional :: verbose
    type(string_t), intent(in), optional :: model_name
    integer :: u
    u = logfile_unit ()
    var%aval = aval
    var%is_known = is_known
    var%is_defined = .true.
    if (associated (var%original)) then
       call var_entry_set_pdg_array (var%original, aval, is_known)
    end if
    if (present (verbose)) then
       if (verbose) then
          call var_entry_write (var, model_name=model_name)
          call var_entry_write (var, model_name=model_name, unit=u)
          if (u >= 0) flush (u)
       end if
    end if
  end subroutine var_entry_set_pdg_array

  recursive subroutine var_entry_set_subevt &
       (var, pval, is_known, verbose, model_name)
    type(var_entry_t), intent(inout) :: var
    type(subevt_t), intent(in) :: pval
    logical, intent(in) :: is_known
    logical, intent(in), optional :: verbose
    type(string_t), intent(in), optional :: model_name
    integer :: u
    u = logfile_unit ()
    var%pval = pval
    var%is_known = is_known
    var%is_defined = .true.
    if (associated (var%original)) then
       call var_entry_set_subevt (var%original, pval, is_known)
    end if
    if (present (verbose)) then
       if (verbose) then
          call var_entry_write (var, model_name=model_name)
          call var_entry_write (var, model_name=model_name, unit=u)
          if (u >= 0) flush (u)
       end if
    end if
  end subroutine var_entry_set_subevt

  recursive subroutine var_entry_set_string &
       (var, sval, is_known, verbose, model_name)
    type(var_entry_t), intent(inout) :: var
    type(string_t), intent(in) :: sval
    logical, intent(in) :: is_known
    logical, intent(in), optional :: verbose
    type(string_t), intent(in), optional :: model_name
    integer :: u
    u = logfile_unit ()
    var%sval = sval
    var%is_known = is_known
    var%is_defined = .true.
    if (associated (var%original)) then
       call var_entry_set_string (var%original, sval, is_known)
    end if
    if (present (verbose)) then
       if (verbose) then
          call var_entry_write (var, model_name=model_name)
          call var_entry_write (var, model_name=model_name, unit=u)
          if (u >= 0) flush (u)
       end if
    end if
  end subroutine var_entry_set_string

  subroutine var_entry_init_copy (var, original, user)
    type(var_entry_t), intent(out) :: var
    type(var_entry_t), intent(in), target :: original
    logical, intent(in), optional :: user
    type(string_t) :: name
    logical :: intrinsic
    name = var_entry_get_name (original)
    intrinsic = original%is_intrinsic
    select case (original%type)
    case (V_LOG)
       call var_entry_init_log (var, name, intrinsic=intrinsic, user=user)
    case (V_INT)
       call var_entry_init_int (var, name, intrinsic=intrinsic, user=user)
    case (V_REAL)
       call var_entry_init_real (var, name, intrinsic=intrinsic, user=user)
    case (V_CMPLX)
       call var_entry_init_cmplx (var, name, intrinsic=intrinsic, user=user)
    case (V_SEV)
       call var_entry_init_subevt (var, name, intrinsic=intrinsic, user=user)
    case (V_PDG)
       call var_entry_init_pdg_array (var, name, intrinsic=intrinsic, user=user)
    case (V_STR)
       call var_entry_init_string (var, name, intrinsic=intrinsic, user=user)
    end select
    var%is_copy = .true.
  end subroutine var_entry_init_copy

  subroutine var_entry_clear_original_pointer (var)
    type(var_entry_t), intent(inout) :: var
    var%original => null ()
  end subroutine var_entry_clear_original_pointer

  subroutine var_entry_set_original_pointer (var, original)
    type(var_entry_t), intent(inout) :: var
    type(var_entry_t), intent(in), target :: original
    type(string_t) :: name
    type(var_entry_t), pointer :: next
    if (var_entry_is_locked (original)) then
!        next => var%next
!        name = var_entry_get_name (original)
!        select case (original%type)
!        case (V_LOG);  call var_entry_init_log_ptr (var, name, &
!             original%lval, original%is_known)
!        case (V_INT);  call var_entry_init_int_ptr (var, name, &
!             original%ival, original%is_known)
!        case (V_REAL); call var_entry_init_real_ptr (var, name, &
!             original%rval, original%is_known)
!        case (V_CMPLX); call var_entry_init_cmplx_ptr (var, name, &
!             original%cval, original%is_known)
!        case (V_SEV);  call var_entry_init_subevt_ptr (var, name, &
!             original%pval, original%is_known)
!        case (V_PDG);  call var_entry_init_pdg_array_ptr (var, name, &
!             original%aval, original%is_known)
!        case (V_STR);  call var_entry_init_string_ptr (var, name, &
!             original%sval, original%is_known)
!        end select
!        var%next => next
       call var_entry_lock (var)
!     else
!        var%original => original
    end if
    var%original => original
  end subroutine var_entry_set_original_pointer

  subroutine var_entry_synchronize (var)
    type(var_entry_t), intent(inout) :: var
    if (associated (var%original)) then
       var%is_defined = var%original%is_defined
       var%is_known = var%original%is_known
       if (var%original%is_known) then
          select case (var%type)
          case (V_LOG);  var%lval = var%original%lval
          case (V_INT);  var%ival = var%original%ival
          case (V_REAL); var%rval = var%original%rval
          case (V_CMPLX); var%cval = var%original%cval
          case (V_SEV);  var%pval = var%original%pval
          case (V_PDG);  var%aval = var%original%aval
          case (V_STR);  var%sval = var%original%sval
          end select
       end if
    end if
  end subroutine var_entry_synchronize

  subroutine var_entry_restore (var)
    type(var_entry_t), intent(inout) :: var
    !!! ifort 11.1 rev5 chokes over the intent(in)
    !!! type(var_entry_t), intent(in) :: var    
    if (associated (var%original)) then
       if (var%is_known) then
          select case (var%type)
          case (V_LOG);  var%original%lval = var%lval
          case (V_INT);  var%original%ival = var%ival
          case (V_REAL); var%original%rval = var%rval
          case (V_CMPLX); var%original%cval = var%cval
          case (V_SEV);  var%original%pval = var%pval
          case (V_PDG);  var%original%aval = var%aval
          case (V_STR);  var%original%sval = var%sval
          end select
       end if
    end if
  end subroutine var_entry_restore
       
  subroutine var_list_link (var_list, next)
    type(var_list_t), intent(inout) :: var_list
    type(var_list_t), intent(in), target :: next
    var_list%next => next
  end subroutine var_list_link

  subroutine var_list_append (var_list, var, verbose)
    type(var_list_t), intent(inout) :: var_list
    type(var_entry_t), intent(in), target :: var
    logical, intent(in), optional :: verbose
    if (associated (var_list%last)) then
       var_list%last%next => var
    else
       var_list%first => var
    end if
    var_list%last => var
    if (present (verbose)) then
       if (verbose)  call var_entry_write (var)
    end if
  end subroutine var_list_append

  subroutine var_list_append_log_s &
       (var_list, name, lval, locked, verbose, intrinsic, user)
    type(var_list_t), intent(inout) :: var_list
    type(string_t), intent(in) :: name
    logical, intent(in), optional :: lval
    logical, intent(in), optional :: locked, verbose, intrinsic, user
    type(var_entry_t), pointer :: var
    allocate (var)
    call var_entry_init_log (var, name, lval, intrinsic, user)
    if (present (locked))  call var_entry_lock (var, locked)
    call var_list_append (var_list, var, verbose)
  end subroutine var_list_append_log_s

  subroutine var_list_append_int_s &
       (var_list, name, ival, locked, verbose, intrinsic, user)
    type(var_list_t), intent(inout) :: var_list
    type(string_t), intent(in) :: name
    integer, intent(in), optional :: ival
    logical, intent(in), optional :: locked, verbose, intrinsic, user
    type(var_entry_t), pointer :: var
    allocate (var)
    call var_entry_init_int (var, name, ival, intrinsic, user)
    if (present (locked))  call var_entry_lock (var, locked)
    call var_list_append (var_list, var, verbose)
  end subroutine var_list_append_int_s

  subroutine var_list_append_real_s &
       (var_list, name, rval, locked, verbose, intrinsic, user)
    type(var_list_t), intent(inout) :: var_list
    type(string_t), intent(in) :: name
    real(default), intent(in), optional :: rval
    logical, intent(in), optional :: locked, verbose, intrinsic, user
    type(var_entry_t), pointer :: var
    allocate (var)
    call var_entry_init_real (var, name, rval, intrinsic, user)
    if (present (locked))  call var_entry_lock (var, locked)
    call var_list_append (var_list, var, verbose)
  end subroutine var_list_append_real_s
  
  subroutine var_list_append_cmplx_s &
       (var_list, name, cval, locked, verbose, intrinsic, user)
    type(var_list_t), intent(inout) :: var_list
    type(string_t), intent(in) :: name
    complex(default), intent(in), optional :: cval
    logical, intent(in), optional :: locked, verbose, intrinsic, user
    type(var_entry_t), pointer :: var
    allocate (var)
    call var_entry_init_cmplx (var, name, cval, intrinsic, user)
    if (present (locked))  call var_entry_lock (var, locked)
    call var_list_append (var_list, var, verbose)
  end subroutine var_list_append_cmplx_s

  subroutine var_list_append_subevt_s &
       (var_list, name, pval, locked, verbose, intrinsic, user)
    type(var_list_t), intent(inout) :: var_list
    type(string_t), intent(in) :: name
    type(subevt_t), intent(in), optional :: pval
    logical, intent(in), optional :: locked, verbose, intrinsic, user
    type(var_entry_t), pointer :: var
    allocate (var)
    call var_entry_init_subevt (var, name, pval, intrinsic, user)
    if (present (locked))  call var_entry_lock (var, locked)
    call var_list_append (var_list, var, verbose)
  end subroutine var_list_append_subevt_s

  subroutine var_list_append_pdg_array_s &
       (var_list, name, aval, locked, verbose, intrinsic, user)
    type(var_list_t), intent(inout) :: var_list
    type(string_t), intent(in) :: name
    type(pdg_array_t), intent(in), optional :: aval
    logical, intent(in), optional :: locked, verbose, intrinsic, user
    type(var_entry_t), pointer :: var
    allocate (var)
    call var_entry_init_pdg_array (var, name, aval, intrinsic, user)
    if (present (locked))  call var_entry_lock (var, locked)
    call var_list_append (var_list, var, verbose)
  end subroutine var_list_append_pdg_array_s

  subroutine var_list_append_string_s &
       (var_list, name, sval, locked, verbose, intrinsic, user)
    type(var_list_t), intent(inout) :: var_list
    type(string_t), intent(in) :: name
    type(string_t), intent(in), optional :: sval
    logical, intent(in), optional :: locked, verbose, intrinsic, user
    type(var_entry_t), pointer :: var
    allocate (var)
    call var_entry_init_string (var, name, sval, intrinsic, user)
    if (present (locked))  call var_entry_lock (var, locked)
    call var_list_append (var_list, var, verbose)
  end subroutine var_list_append_string_s

  subroutine var_list_append_log_c &
       (var_list, name, lval, locked, verbose, intrinsic, user)
    type(var_list_t), intent(inout) :: var_list
    character(*), intent(in) :: name
    logical, intent(in), optional :: lval
    logical, intent(in), optional :: locked, verbose, intrinsic, user
    call var_list_append_log_s &
         (var_list, var_str (name), lval, locked, verbose, intrinsic, user)
  end subroutine var_list_append_log_c

  subroutine var_list_append_int_c &
       (var_list, name, ival, locked, verbose, intrinsic, user)
    type(var_list_t), intent(inout) :: var_list
    character(*), intent(in) :: name
    integer, intent(in), optional :: ival
    logical, intent(in), optional :: locked, verbose, intrinsic, user
    call var_list_append_int_s &
         (var_list, var_str (name), ival, locked, verbose, intrinsic, user)
  end subroutine var_list_append_int_c

  subroutine var_list_append_real_c &
       (var_list, name, rval, locked, verbose, intrinsic, user)
    type(var_list_t), intent(inout) :: var_list
    character(*), intent(in) :: name
    real(default), intent(in), optional :: rval
    logical, intent(in), optional :: locked, verbose, intrinsic, user
    call var_list_append_real_s &
         (var_list, var_str (name), rval, locked, verbose, intrinsic, user)
  end subroutine var_list_append_real_c
  
  subroutine var_list_append_cmplx_c &
       (var_list, name, cval, locked, verbose, intrinsic, user)
    type(var_list_t), intent(inout) :: var_list
    character(*), intent(in) :: name
    complex(default), intent(in), optional :: cval
    logical, intent(in), optional :: locked, verbose, intrinsic, user
    call var_list_append_cmplx_s &
         (var_list, var_str (name), cval, locked, verbose, intrinsic, user)
  end subroutine var_list_append_cmplx_c

  subroutine var_list_append_subevt_c &
       (var_list, name, pval, locked, verbose, intrinsic, user)
    type(var_list_t), intent(inout) :: var_list
    character(*), intent(in) :: name
    type(subevt_t), intent(in), optional :: pval
    logical, intent(in), optional :: locked, verbose, intrinsic, user
    call var_list_append_subevt_s &
         (var_list, var_str (name), pval, locked, verbose, intrinsic, user)
  end subroutine var_list_append_subevt_c

  subroutine var_list_append_pdg_array_c &
       (var_list, name, aval, locked, verbose, intrinsic, user)
    type(var_list_t), intent(inout) :: var_list
    character(*), intent(in) :: name
    type(pdg_array_t), intent(in), optional :: aval
    logical, intent(in), optional :: locked, verbose, intrinsic, user
    call var_list_append_pdg_array_s &
         (var_list, var_str (name), aval, locked, verbose, intrinsic, user)
  end subroutine var_list_append_pdg_array_c

  subroutine var_list_append_string_c &
       (var_list, name, sval, locked, verbose, intrinsic, user)
    type(var_list_t), intent(inout) :: var_list
    character(*), intent(in) :: name
    character(*), intent(in), optional :: sval
    logical, intent(in), optional :: locked, verbose, intrinsic, user
    if (present (sval)) then
       call var_list_append_string_s &
            (var_list, var_str (name), var_str (sval), &
            locked, verbose, intrinsic, user)
    else
       call var_list_append_string_s &
            (var_list, var_str (name), &
            locked=locked, verbose=verbose, intrinsic=intrinsic, user=user)
    end if
  end subroutine var_list_append_string_c

  subroutine var_list_append_log_ptr &
       (var_list, name, lval, is_known, locked, verbose, intrinsic)
    type(var_list_t), intent(inout) :: var_list
    type(string_t), intent(in) :: name
    logical, intent(in), target :: lval
    logical, intent(in), target :: is_known
    logical, intent(in), optional :: locked, verbose, intrinsic
    type(var_entry_t), pointer :: var
    allocate (var)
    call var_entry_init_log_ptr (var, name, lval, is_known, intrinsic)
    if (present (locked))  call var_entry_lock (var, locked)
    call var_list_append (var_list, var, verbose)
  end subroutine var_list_append_log_ptr

  subroutine var_list_append_int_ptr &
       (var_list, name, ival, is_known, locked, verbose, intrinsic)
    type(var_list_t), intent(inout) :: var_list
    type(string_t), intent(in) :: name
    integer, intent(in), target :: ival
    logical, intent(in), target :: is_known
    logical, intent(in), optional :: locked, verbose, intrinsic
    type(var_entry_t), pointer :: var
    allocate (var)
    call var_entry_init_int_ptr (var, name, ival, is_known, intrinsic)
    if (present (locked))  call var_entry_lock (var, locked)
    call var_list_append (var_list, var, verbose)
  end subroutine var_list_append_int_ptr

  subroutine var_list_append_real_ptr &
       (var_list, name, rval, is_known, locked, verbose, intrinsic)
    type(var_list_t), intent(inout) :: var_list
    type(string_t), intent(in) :: name
    real(default), intent(in), target :: rval
    logical, intent(in), target :: is_known
    logical, intent(in), optional :: locked, verbose, intrinsic
    type(var_entry_t), pointer :: var
    allocate (var)
    call var_entry_init_real_ptr (var, name, rval, is_known, intrinsic)
    if (present (locked))  call var_entry_lock (var, locked)
    call var_list_append (var_list, var, verbose)
  end subroutine var_list_append_real_ptr

  subroutine var_list_append_cmplx_ptr &
       (var_list, name, cval, is_known, locked, verbose, intrinsic)
    type(var_list_t), intent(inout) :: var_list
    type(string_t), intent(in) :: name
    complex(default), intent(in), target :: cval
    logical, intent(in), target :: is_known
    logical, intent(in), optional :: locked, verbose, intrinsic
    type(var_entry_t), pointer :: var
    allocate (var)
    call var_entry_init_cmplx_ptr (var, name, cval, is_known, intrinsic)
    if (present (locked))  call var_entry_lock (var, locked)
    call var_list_append (var_list, var, verbose)
  end subroutine var_list_append_cmplx_ptr
    
  subroutine var_list_append_pdg_array_ptr &
       (var_list, name, aval, is_known, locked, verbose, intrinsic)
    type(var_list_t), intent(inout) :: var_list
    type(string_t), intent(in) :: name
    type(pdg_array_t), intent(in), target :: aval
    logical, intent(in), target :: is_known
    logical, intent(in), optional :: locked, verbose, intrinsic
    type(var_entry_t), pointer :: var
    allocate (var)
    call var_entry_init_pdg_array_ptr (var, name, aval, is_known, intrinsic)
    if (present (locked))  call var_entry_lock (var, locked)
    call var_list_append (var_list, var, verbose)
  end subroutine var_list_append_pdg_array_ptr

  subroutine var_list_append_subevt_ptr &
       (var_list, name, pval, is_known, locked, verbose, intrinsic)
    type(var_list_t), intent(inout) :: var_list
    type(string_t), intent(in) :: name
    type(subevt_t), intent(in), target :: pval
    logical, intent(in), target :: is_known
    logical, intent(in), optional :: locked, verbose, intrinsic
    type(var_entry_t), pointer :: var
    allocate (var)
    call var_entry_init_subevt_ptr (var, name, pval, is_known, intrinsic)
    if (present (locked))  call var_entry_lock (var, locked)
    call var_list_append (var_list, var, verbose)
  end subroutine var_list_append_subevt_ptr

  subroutine var_list_append_string_ptr &
       (var_list, name, sval, is_known, locked, verbose, intrinsic)
    type(var_list_t), intent(inout) :: var_list
    type(string_t), intent(in) :: name
    type(string_t), intent(in), target :: sval
    logical, intent(in), target :: is_known
    logical, intent(in), optional :: locked, verbose, intrinsic
    type(var_entry_t), pointer :: var
    allocate (var)
    call var_entry_init_string_ptr (var, name, sval, is_known, intrinsic)
    if (present (locked))  call var_entry_lock (var, locked)
    call var_list_append (var_list, var, verbose)
  end subroutine var_list_append_string_ptr

  subroutine var_list_final (var_list)
    type(var_list_t), intent(inout) :: var_list
    type(var_entry_t), pointer :: var
    var_list%last => null ()
    do while (associated (var_list%first))
       var => var_list%first
       var_list%first => var%next
       call var_entry_final (var)
       deallocate (var)
    end do
  end subroutine var_list_final

  recursive subroutine var_list_write &
       (var_list, unit, follow_link, only_type, prefix, model_name, show_ptr, &
        intrinsic)
    type(var_list_t), intent(in), target :: var_list
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: follow_link
    integer, intent(in), optional :: only_type
    character(*), intent(in), optional :: prefix
    type(string_t), intent(in), optional :: model_name
    logical, intent(in), optional :: show_ptr
    logical, intent(in), optional :: intrinsic
    type(var_entry_t), pointer :: var
    integer :: u, length
    logical :: write_this, write_next
    u = output_unit (unit);  if (u < 0)  return
    if (present (prefix))  length = len (prefix)
    var => var_list%first
    if (associated (var)) then
       do while (associated (var))
          if (present (only_type)) then
             write_this = only_type == var%type
          else
             write_this = .true.
          end if
          if (write_this .and. present (prefix)) then
             if (prefix /= extract (var%name, 1, length)) &
                  write_this = .false.
          end if
          if (write_this) then
             call var_entry_write &
                  (var, unit, model_name = model_name, show_ptr = show_ptr, &
                   intrinsic=intrinsic)
          end if
          var => var%next
       end do
    end if
    write_next = associated (var_list%next)
    if (present (follow_link)) &
         write_next = write_next .and. follow_link
    if (write_next) then
       call var_list_write (var_list%next, &
            unit, follow_link, only_type, prefix, model_name, show_ptr, &
            intrinsic)
    end if
  end subroutine var_list_write

  recursive subroutine var_list_write_var &
       (var_list, name, unit, type, follow_link, model_name, show_ptr)
    type(var_list_t), intent(in), target :: var_list
    type(string_t), intent(in) :: name
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: type
    logical, intent(in), optional :: follow_link
    type(string_t), intent(in), optional :: model_name
    logical, intent(in), optional :: show_ptr
    type(var_entry_t), pointer :: var
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    var => var_list_get_var_ptr &
         (var_list, name, type, follow_link=follow_link, defined=.true.)
    if (associated (var)) then
       call var_entry_write &
            (var, unit, model_name = model_name, show_ptr = show_ptr)
    else
       write (u, "(A)")  char (name) // " = [undefined]"
    end if
  end subroutine var_list_write_var

  function var_list_get_next_ptr (var_list) result (next_ptr)
    type(var_list_t), pointer :: next_ptr
    type(var_list_t), intent(in) :: var_list
    next_ptr => var_list%next
  end function var_list_get_next_ptr

  recursive function var_list_get_var_ptr &
       (var_list, name, type, follow_link, defined) result (var)
    type(var_entry_t), pointer :: var
    type(var_list_t), intent(in), target :: var_list
    type(string_t), intent(in) :: name
    integer, intent(in), optional :: type
    logical, intent(in), optional :: follow_link, defined
    logical :: ignore_undef, search_next
    ignore_undef = .true.;  if (present (defined))  ignore_undef = .not. defined
    var => var_list%first
    if (present (type)) then
       do while (associated (var))
          if (var%type == type) then
             if (var%name == name) then
                if (ignore_undef .or. var%is_defined)  return
             end if
          end if
          var => var%next
       end do
    else
       do while (associated (var))
          if (var%name == name) then
             if (ignore_undef .or. var%is_defined)  return
          end if
          var => var%next
       end do
    end if
    search_next = associated (var_list%next)
    if (present (follow_link)) &
         search_next = search_next .and. follow_link
    if (search_next) &
         var => var_list_get_var_ptr &
              (var_list%next, name, type, defined=defined)
  end function var_list_get_var_ptr

  function var_list_get_type (var_list, name, follow_link) result (type)
    integer :: type
    type(string_t), intent(in) :: name
    type(var_list_t), intent(in), target :: var_list
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, follow_link=follow_link)
    if (associated (var)) then
       type = var%type
    else
       type = V_NONE
    end if
  end function var_list_get_type

  function var_list_exists (var_list, name, follow_link) result (flag)
    logical :: flag
    type(string_t), intent(in) :: name
    type(var_list_t), intent(in), target :: var_list
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, follow_link=follow_link)
    flag = associated (var)
  end function var_list_exists

  function var_list_is_intrinsic (var_list, name, follow_link) result (flag)
    logical :: flag
    type(string_t), intent(in) :: name
    type(var_list_t), intent(in), target :: var_list
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, follow_link=follow_link)
    if (associated (var)) then
       flag = var%is_intrinsic
    else
       flag = .false.
    end if
  end function var_list_is_intrinsic

  function var_list_is_known_s (var_list, name, follow_link) result (flag)
    logical :: flag
    type(string_t), intent(in) :: name
    type(var_list_t), intent(in), target :: var_list
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, follow_link=follow_link)
    if (associated (var)) then
       flag = var%is_known
    else
       flag = .false.
    end if
  end function var_list_is_known_s

  function var_list_is_known_c (var_list, name, follow_link) result (flag)
    logical :: flag
    character(*), intent(in) :: name
    type(var_list_t), intent(in), target :: var_list
    logical, intent(in), optional :: follow_link
    flag = var_list_is_known_s (var_list, var_str (name), follow_link)
  end function var_list_is_known_c

  function var_list_is_locked (var_list, name, follow_link) result (flag)
    logical :: flag
    type(string_t), intent(in) :: name
    type(var_list_t), intent(in), target :: var_list
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, follow_link=follow_link)
    if (associated (var)) then
       flag = var_entry_is_locked (var)
    else
       flag = .false.
    end if
  end function var_list_is_locked

  function var_list_get_lval_s (var_list, name, follow_link) result (lval)
    logical :: lval
    type(string_t), intent(in) :: name
    type(var_list_t), intent(in), target :: var_list
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr &
         (var_list, name, V_LOG, follow_link, defined=.true.)
    if (associated (var)) then
       if (var_has_value (var)) then
          lval = var%lval
       else
          lval = .false.
       end if
    else
       lval = .false.
    end if
  end function var_list_get_lval_s
  
  function var_list_get_ival_s (var_list, name, follow_link) result (ival)
    integer :: ival
    type(string_t), intent(in) :: name
    type(var_list_t), intent(in), target :: var_list
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr &
         (var_list, name, V_INT, follow_link, defined=.true.)
    if (associated (var)) then
       if (var_has_value (var)) then
          ival = var%ival
       else
          ival = 0
       end if
    else
       ival = 0
    end if
  end function var_list_get_ival_s
  
  function var_list_get_rval_s (var_list, name, follow_link) result (rval)
    real(default) :: rval
    type(string_t), intent(in) :: name
    type(var_list_t), intent(in), target :: var_list
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr &
         (var_list, name, V_REAL, follow_link, defined=.true.)
    if (associated (var)) then
       if (var_has_value (var)) then
          rval = var%rval
       else
          rval = 0
       end if
    else
       rval = 0
    end if
  end function var_list_get_rval_s
    
  function var_list_get_cval_s (var_list, name, follow_link) result (cval)
    complex(default) :: cval
    type(string_t), intent(in) :: name
    type(var_list_t), intent(in), target :: var_list
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr &
         (var_list, name, V_CMPLX, follow_link, defined=.true.)
    if (associated (var)) then
       if (var_has_value (var)) then
          cval = var%cval
       else
          cval = 0
       end if
    else
       cval = 0
    end if
  end function var_list_get_cval_s

  function var_list_get_aval_s (var_list, name, follow_link) result (aval)
    type(pdg_array_t) :: aval
    type(string_t), intent(in) :: name
    type(var_list_t), intent(in), target :: var_list
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr &
         (var_list, name, V_PDG, follow_link, defined=.true.)
    if (associated (var)) then
       if (var_has_value (var)) then
          aval = var%aval
       end if
    end if    
  end function var_list_get_aval_s
  
  function var_list_get_pval_s (var_list, name, follow_link) result (pval)
    type(subevt_t) :: pval
    type(string_t), intent(in) :: name
    type(var_list_t), intent(in), target :: var_list
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr &
         (var_list, name, V_SEV, follow_link, defined=.true.)
    if (associated (var)) then
       if (var_has_value (var)) then
          pval = var%pval
       end if
    end if
  end function var_list_get_pval_s
  
  function var_list_get_sval_s (var_list, name, follow_link) result (sval)
    type(string_t) :: sval
    type(string_t), intent(in) :: name
    type(var_list_t), intent(in), target :: var_list
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr &
         (var_list, name, V_STR, follow_link, defined=.true.)
    if (associated (var)) then
       if (var_has_value (var)) then
          sval = var%sval
       else
          sval = ""
       end if
    else
       sval = ""
    end if
  end function var_list_get_sval_s
  
  function var_list_get_lval_c (var_list, name, follow_link) result (lval)
    logical :: lval
    character(*), intent(in) :: name
    type(var_list_t), intent(in), target :: var_list
    logical, intent(in), optional :: follow_link
    lval = var_list_get_lval_s (var_list, var_str (name), follow_link)
  end function var_list_get_lval_c
  
  function var_list_get_ival_c (var_list, name, follow_link) result (ival)
    integer :: ival
    character(*), intent(in) :: name
    type(var_list_t), intent(in), target :: var_list
    logical, intent(in), optional :: follow_link
    ival = var_list_get_ival_s (var_list, var_str (name), follow_link)
  end function var_list_get_ival_c
  
  function var_list_get_rval_c (var_list, name, follow_link) result (rval)
    real(default) :: rval
    character(*), intent(in) :: name
    type(var_list_t), intent(in), target :: var_list
    logical, intent(in), optional :: follow_link
    rval = var_list_get_rval_s (var_list, var_str (name), follow_link)
  end function var_list_get_rval_c
    
  function var_list_get_cval_c (var_list, name, follow_link) result (cval)
    complex(default) :: cval
    character(*), intent(in) :: name
    type(var_list_t), intent(in), target :: var_list
    logical, intent(in), optional :: follow_link
    cval = var_list_get_cval_s (var_list, var_str (name), follow_link)
  end function var_list_get_cval_c

  function var_list_get_aval_c (var_list, name, follow_link) result (aval)
    type(pdg_array_t) :: aval
    character(*), intent(in) :: name
    type(var_list_t), intent(in), target :: var_list
    logical, intent(in), optional :: follow_link
    aval = var_list_get_aval_s (var_list, var_str (name), follow_link)
  end function var_list_get_aval_c
  
  function var_list_get_pval_c (var_list, name, follow_link) result (pval)
    type(subevt_t) :: pval
    character(*), intent(in) :: name
    type(var_list_t), intent(in), target :: var_list
    logical, intent(in), optional :: follow_link
    pval = var_list_get_pval_s (var_list, var_str (name), follow_link)
  end function var_list_get_pval_c
  
  function var_list_get_sval_c (var_list, name, follow_link) result (sval)
    type(string_t) :: sval
    character(*), intent(in) :: name
    type(var_list_t), intent(in), target :: var_list
    logical, intent(in), optional :: follow_link
    sval = var_list_get_sval_s (var_list, var_str (name), follow_link)
  end function var_list_get_sval_c
  
  function var_has_value (var) result (valid)
    logical :: valid
    type(var_entry_t), pointer :: var
    if (associated (var)) then
       if (var%is_known) then
          valid = .true.
       else
          call msg_error ("The value of variable '" // char (var%name) &
               // "' is unknown but must be known at this point.")
          valid = .false.
       end if
    else
       call msg_error ("Variable '" // char (var%name) &
            // "' is undefined but must have a known value at this point.")
       valid = .false.
    end if
  end function var_has_value

  subroutine var_list_init_num_id (var_list, proc_id, num_id)
    type(var_list_t), intent(inout) :: var_list
    type(string_t), intent(in) :: proc_id
    integer, intent(in), optional :: num_id
    call var_list_set_procvar_int (var_list, proc_id, &
         var_str ("num_id"), num_id)
  end subroutine var_list_init_num_id

  subroutine var_list_init_process_results (var_list, proc_id, &
       n_calls, integral, error, accuracy, chi2, efficiency)
    type(var_list_t), intent(inout) :: var_list
    type(string_t), intent(in) :: proc_id
    integer, intent(in), optional :: n_calls
    real(default), intent(in), optional :: integral, error, accuracy
    real(default), intent(in), optional :: chi2, efficiency
    call var_list_set_procvar_int (var_list, proc_id, &
         var_str ("n_calls"), n_calls)
    call var_list_set_procvar_real (var_list, proc_id, &
         var_str ("integral"), integral)
    call var_list_set_procvar_real (var_list, proc_id, &
         var_str ("error"), error)
    call var_list_set_procvar_real (var_list, proc_id, &
         var_str ("accuracy"), accuracy)
    call var_list_set_procvar_real (var_list, proc_id, &
         var_str ("chi2"), chi2)
    call var_list_set_procvar_real (var_list, proc_id, &
         var_str ("efficiency"), efficiency)
  end subroutine var_list_init_process_results

  subroutine var_list_set_procvar_int (var_list, proc_id, name, ival)
    type(var_list_t), intent(inout) :: var_list
    type(string_t), intent(in) :: proc_id
    type(string_t), intent(in) :: name
    integer, intent(in), optional :: ival
    type(string_t) :: var_name
    type(var_entry_t), pointer :: var
    var_name = name // "(" // proc_id // ")"
    var => var_list_get_var_ptr (var_list, var_name)
    if (.not. associated (var)) then
       call var_list_append_int (var_list, var_name, ival, intrinsic=.true.)
    else if (present (ival)) then
       call var_list_set_int (var_list, var_name, ival, is_known=.true.)
    end if
  end subroutine var_list_set_procvar_int

  subroutine var_list_set_procvar_real (var_list, proc_id, name, rval)
    type(var_list_t), intent(inout) :: var_list
    type(string_t), intent(in) :: proc_id
    type(string_t), intent(in) :: name
    real(default), intent(in), optional :: rval
    type(string_t) :: var_name
    type(var_entry_t), pointer :: var
    var_name = name // "(" // proc_id // ")"
    var => var_list_get_var_ptr (var_list, var_name)
    if (.not. associated (var)) then
       call var_list_append_real (var_list, var_name, rval, intrinsic=.true.)
    else if (present (rval)) then
       call var_list_set_real (var_list, var_name, rval, is_known=.true.)
    end if
  end subroutine var_list_set_procvar_real

  subroutine var_list_set_obs (var_list, name, type, var, prt1, prt2)
    type(var_list_t), intent(inout) :: var_list
    type(string_t), intent(in) :: name
    integer, intent(in) :: type
    type(var_entry_t), pointer :: var
    type(prt_t), intent(in), target :: prt1
    type(prt_t), intent(in), optional, target :: prt2
    allocate (var)
    call var_entry_init_obs (var, name, type, prt1, prt2)
    call var_list_append (var_list, var)
  end subroutine var_list_set_obs

  subroutine var_list_set_observables_unary (var_list, prt1)
    type(var_list_t), intent(inout) :: var_list
    type(prt_t), intent(in), target :: prt1
    type(var_entry_t), pointer :: var
    call var_list_set_obs &
         (var_list, var_str ("PDG"), V_OBS1_INT, var, prt1)
    var% obs1_int => obs_pdg1
    call var_list_set_obs &
         (var_list, var_str ("Hel"), V_OBS1_INT, var, prt1)
    var% obs1_int => obs_helicity1
    call var_list_set_obs &
         (var_list, var_str ("M"), V_OBS1_REAL, var, prt1)
    var% obs1_real => obs_signed_mass1
    call var_list_set_obs &
         (var_list, var_str ("M2"), V_OBS1_REAL, var, prt1)
    var% obs1_real => obs_mass_squared1
    call var_list_set_obs &
         (var_list, var_str ("E"), V_OBS1_REAL, var, prt1)
    var% obs1_real => obs_energy1
    call var_list_set_obs &
         (var_list, var_str ("Px"), V_OBS1_REAL, var, prt1)
    var% obs1_real => obs_px1
    call var_list_set_obs &
         (var_list, var_str ("Py"), V_OBS1_REAL, var, prt1)
    var% obs1_real => obs_py1
    call var_list_set_obs &
         (var_list, var_str ("Pz"), V_OBS1_REAL, var, prt1)
    var% obs1_real => obs_pz1
    call var_list_set_obs &
         (var_list, var_str ("P"), V_OBS1_REAL, var, prt1)
    var% obs1_real => obs_p1
    call var_list_set_obs &
         (var_list, var_str ("Pl"), V_OBS1_REAL, var, prt1)
    var% obs1_real => obs_pl1
    call var_list_set_obs &
         (var_list, var_str ("Pt"), V_OBS1_REAL, var, prt1)
    var% obs1_real => obs_pt1
    call var_list_set_obs &
         (var_list, var_str ("Theta"), V_OBS1_REAL, var, prt1)
    var% obs1_real => obs_theta1
    call var_list_set_obs &
         (var_list, var_str ("Phi"), V_OBS1_REAL, var, prt1)
    var% obs1_real => obs_phi1
    call var_list_set_obs &
         (var_list, var_str ("Rap"), V_OBS1_REAL, var, prt1)
    var% obs1_real => obs_rap1
    call var_list_set_obs &
         (var_list, var_str ("Eta"), V_OBS1_REAL, var, prt1)
    var% obs1_real => obs_eta1
    call var_list_set_obs &
         (var_list, var_str ("Theta_RF"), V_OBS1_REAL, var, prt1)
    var% obs1_real => obs_theta_rf1
    call var_list_set_obs &
         (var_list, var_str ("Dist"), V_OBS1_REAL, var, prt1)
    var% obs1_real => obs_dist1
    call var_list_set_obs &
         (var_list, var_str ("_User_obs_real"), V_UOBS1_REAL, var, prt1)
    call var_list_set_obs &
         (var_list, var_str ("_User_obs_int"), V_UOBS1_INT, var, prt1)
  end subroutine var_list_set_observables_unary

  subroutine var_list_set_observables_binary (var_list, prt1, prt2)
    type(var_list_t), intent(inout) :: var_list
    type(prt_t), intent(in), target :: prt1
    type(prt_t), intent(in), optional, target :: prt2
    type(var_entry_t), pointer :: var
    call var_list_set_obs &
         (var_list, var_str ("PDG"), V_OBS2_INT, var, prt1, prt2)
    var% obs2_int => obs_pdg2
    call var_list_set_obs &
         (var_list, var_str ("Hel"), V_OBS2_INT, var, prt1, prt2)
    var% obs2_int => obs_helicity2
    call var_list_set_obs &
         (var_list, var_str ("M"), V_OBS2_REAL, var, prt1, prt2)
    var% obs2_real => obs_signed_mass2
    call var_list_set_obs &
         (var_list, var_str ("M2"), V_OBS2_REAL, var, prt1, prt2)
    var% obs2_real => obs_mass_squared2
    call var_list_set_obs &
         (var_list, var_str ("E"), V_OBS2_REAL, var, prt1, prt2)
    var% obs2_real => obs_energy2
    call var_list_set_obs &
         (var_list, var_str ("Px"), V_OBS2_REAL, var, prt1, prt2)
    var% obs2_real => obs_px2
    call var_list_set_obs &
         (var_list, var_str ("Py"), V_OBS2_REAL, var, prt1, prt2)
    var% obs2_real => obs_py2
    call var_list_set_obs &
         (var_list, var_str ("Pz"), V_OBS2_REAL, var, prt1, prt2)
    var% obs2_real => obs_pz2
    call var_list_set_obs &
         (var_list, var_str ("P"), V_OBS2_REAL, var, prt1, prt2)
    var% obs2_real => obs_p2
    call var_list_set_obs &
         (var_list, var_str ("Pl"), V_OBS2_REAL, var, prt1, prt2)
    var% obs2_real => obs_pl2
    call var_list_set_obs &
         (var_list, var_str ("Pt"), V_OBS2_REAL, var, prt1, prt2)
    var% obs2_real => obs_pt2
    call var_list_set_obs &
         (var_list, var_str ("Theta"), V_OBS2_REAL, var, prt1, prt2)
    var% obs2_real => obs_theta2
    call var_list_set_obs &
         (var_list, var_str ("Phi"), V_OBS2_REAL, var, prt1, prt2)
    var% obs2_real => obs_phi2
    call var_list_set_obs &
         (var_list, var_str ("Rap"), V_OBS2_REAL, var, prt1, prt2)
    var% obs2_real => obs_rap2
    call var_list_set_obs &
         (var_list, var_str ("Eta"), V_OBS2_REAL, var, prt1, prt2)
    var% obs2_real => obs_eta2
    call var_list_set_obs &
         (var_list, var_str ("Theta_RF"), V_OBS2_REAL, var, prt1, prt2)
    var% obs2_real => obs_theta_rf2
    call var_list_set_obs &
         (var_list, var_str ("Dist"), V_OBS2_REAL, var, prt1, prt2)
    var% obs2_real => obs_dist2
    call var_list_set_obs &
         (var_list, var_str ("kT"), V_OBS2_REAL, var, prt1, prt2)
    var% obs2_real => obs_ktmeasure
    call var_list_set_obs &
         (var_list, var_str ("_User_obs_real"), V_UOBS2_REAL, var, prt1, prt2)
    call var_list_set_obs &
         (var_list, var_str ("_User_obs_int"), V_UOBS2_INT, var, prt1, prt2)
  end subroutine var_list_set_observables_binary

  function string_is_observable_id (string) result (flag)
    logical :: flag
    type(string_t), intent(in) :: string
    select case (char (string))
    case ("PDG", "Hel", "M", "M2", "E", "Px", "Py", "Pz", "P", "Pl", "Pt", &
         "Theta", "Phi", "Rap", "Eta", "Theta_RF", "Dist", "kT")
       flag = .true.
    case default
       flag = .false.
    end select
  end function string_is_observable_id

  integer function obs_pdg1 (prt1) result (pdg)
    type(prt_t), intent(in) :: prt1
    pdg = prt_get_pdg (prt1)
  end function obs_pdg1

  integer function obs_helicity1 (prt1) result (h)
    type(prt_t), intent(in) :: prt1
    if (prt_is_polarized (prt1)) then
       h = prt_get_helicity (prt1)
    else
       h = -9
    end if
  end function obs_helicity1

  real(default) function obs_mass_squared1 (prt1) result (p2)
    type(prt_t), intent(in) :: prt1
    p2 = prt_get_msq (prt1)
  end function obs_mass_squared1

  real(default) function obs_signed_mass1 (prt1) result (m)
    type(prt_t), intent(in) :: prt1
    real(default) :: msq
    msq = prt_get_msq (prt1)
    m = sign (sqrt (abs (msq)), msq)
  end function obs_signed_mass1

  real(default) function obs_energy1 (prt1) result (e)
    type(prt_t), intent(in) :: prt1
    e = energy (prt_get_momentum (prt1))
  end function obs_energy1

  real(default) function obs_px1 (prt1) result (p)
    type(prt_t), intent(in) :: prt1
    p = vector4_get_component (prt_get_momentum (prt1), 1)
  end function obs_px1

  real(default) function obs_py1 (prt1) result (p)
    type(prt_t), intent(in) :: prt1
    p = vector4_get_component (prt_get_momentum (prt1), 2)
  end function obs_py1

  real(default) function obs_pz1 (prt1) result (p)
    type(prt_t), intent(in) :: prt1
    p = vector4_get_component (prt_get_momentum (prt1), 3)
  end function obs_pz1

  real(default) function obs_p1 (prt1) result (p)
    type(prt_t), intent(in) :: prt1
    p = space_part_norm (prt_get_momentum (prt1))
  end function obs_p1

  real(default) function obs_pl1 (prt1) result (p)
    type(prt_t), intent(in) :: prt1
    p = longitudinal_part (prt_get_momentum (prt1))
  end function obs_pl1

  real(default) function obs_pt1 (prt1) result (p)
    type(prt_t), intent(in) :: prt1
    p = transverse_part (prt_get_momentum (prt1))
  end function obs_pt1

  real(default) function obs_theta1 (prt1) result (p)
    type(prt_t), intent(in) :: prt1
    p = polar_angle (prt_get_momentum (prt1))
  end function obs_theta1

  real(default) function obs_phi1 (prt1) result (p)
    type(prt_t), intent(in) :: prt1
    p = azimuthal_angle (prt_get_momentum (prt1))
  end function obs_phi1

  real(default) function obs_rap1 (prt1) result (p)
    type(prt_t), intent(in) :: prt1
    p = rapidity (prt_get_momentum (prt1))
  end function obs_rap1

  real(default) function obs_eta1 (prt1) result (p)
    type(prt_t), intent(in) :: prt1
    p = pseudorapidity (prt_get_momentum (prt1))
  end function obs_eta1

  real(default) function obs_theta_rf1 (prt1) result (dist)
    type(prt_t), intent(in) :: prt1
    call msg_fatal (" 'Theta_RF' is undefined as unary observable")
    dist = 0
  end function obs_theta_rf1

  real(default) function obs_dist1 (prt1) result (dist)
    type(prt_t), intent(in) :: prt1
    call msg_fatal (" 'Dist' is undefined as unary observable")
    dist = 0
  end function obs_dist1

  integer function obs_pdg2 (prt1, prt2) result (pdg)
    type(prt_t), intent(in) :: prt1, prt2
    call msg_fatal (" PDG_Code is undefined as binary observable")
    pdg = 0
  end function obs_pdg2

  integer function obs_helicity2 (prt1, prt2) result (h)
    type(prt_t), intent(in) :: prt1, prt2
    call msg_fatal (" Helicity is undefined as binary observable")
    h = 0
  end function obs_helicity2

  real(default) function obs_mass_squared2 (prt1, prt2) result (p2)
    type(prt_t), intent(in) :: prt1, prt2
    type(prt_t) :: prt
    call prt_init_combine (prt, prt1, prt2)
    p2 = prt_get_msq (prt)
  end function obs_mass_squared2

  real(default) function obs_signed_mass2 (prt1, prt2) result (m)
    type(prt_t), intent(in) :: prt1, prt2
    type(prt_t) :: prt
    real(default) :: msq
    call prt_init_combine (prt, prt1, prt2)
    msq = prt_get_msq (prt)
    m = sign (sqrt (abs (msq)), msq)
  end function obs_signed_mass2

  real(default) function obs_energy2 (prt1, prt2) result (e)
    type(prt_t), intent(in) :: prt1, prt2
    type(prt_t) :: prt
    call prt_init_combine (prt, prt1, prt2)
    e = energy (prt_get_momentum (prt))
  end function obs_energy2

  real(default) function obs_px2 (prt1, prt2) result (p)
    type(prt_t), intent(in) :: prt1, prt2
    type(prt_t) :: prt
    call prt_init_combine (prt, prt1, prt2)
    p = vector4_get_component (prt_get_momentum (prt), 1)
  end function obs_px2

  real(default) function obs_py2 (prt1, prt2) result (p)
    type(prt_t), intent(in) :: prt1, prt2
    type(prt_t) :: prt
    call prt_init_combine (prt, prt1, prt2)
    p = vector4_get_component (prt_get_momentum (prt), 2)
  end function obs_py2

  real(default) function obs_pz2 (prt1, prt2) result (p)
    type(prt_t), intent(in) :: prt1, prt2
    type(prt_t) :: prt
    call prt_init_combine (prt, prt1, prt2)
    p = vector4_get_component (prt_get_momentum (prt), 3)
  end function obs_pz2

  real(default) function obs_p2 (prt1, prt2) result (p)
    type(prt_t), intent(in) :: prt1, prt2
    type(prt_t) :: prt
    call prt_init_combine (prt, prt1, prt2)
    p = space_part_norm (prt_get_momentum (prt))
  end function obs_p2

  real(default) function obs_pl2 (prt1, prt2) result (p)
    type(prt_t), intent(in) :: prt1, prt2
    type(prt_t) :: prt
    call prt_init_combine (prt, prt1, prt2)
    p = longitudinal_part (prt_get_momentum (prt))
  end function obs_pl2

  real(default) function obs_pt2 (prt1, prt2) result (p)
    type(prt_t), intent(in) :: prt1, prt2
    type(prt_t) :: prt
    call prt_init_combine (prt, prt1, prt2)
    p = transverse_part (prt_get_momentum (prt))
  end function obs_pt2

  real(default) function obs_theta2 (prt1, prt2) result (p)
    type(prt_t), intent(in) :: prt1, prt2
    p = enclosed_angle (prt_get_momentum (prt1), prt_get_momentum (prt2))
  end function obs_theta2

  real(default) function obs_phi2 (prt1, prt2) result (p)
    type(prt_t), intent(in) :: prt1, prt2
    type(prt_t) :: prt
    call prt_init_combine (prt, prt1, prt2)
    p = azimuthal_distance (prt_get_momentum (prt1), prt_get_momentum (prt2))
  end function obs_phi2

  real(default) function obs_rap2 (prt1, prt2) result (p)
    type(prt_t), intent(in) :: prt1, prt2
    p = rapidity_distance &
         (prt_get_momentum (prt1), prt_get_momentum (prt2))
  end function obs_rap2

  real(default) function obs_eta2 (prt1, prt2) result (p)
    type(prt_t), intent(in) :: prt1, prt2
    type(prt_t) :: prt
    call prt_init_combine (prt, prt1, prt2)
    p = pseudorapidity_distance &
         (prt_get_momentum (prt1), prt_get_momentum (prt2))
  end function obs_eta2

  real(default) function obs_theta_rf2 (prt1, prt2) result (theta)
    type(prt_t), intent(in) :: prt1, prt2
    theta = enclosed_angle_rest_frame &
         (prt_get_momentum (prt1), prt_get_momentum (prt2))
  end function obs_theta_rf2

  real(default) function obs_dist2 (prt1, prt2) result (dist)
    type(prt_t), intent(in) :: prt1, prt2
    dist = eta_phi_distance &
         (prt_get_momentum (prt1), prt_get_momentum (prt2))
  end function obs_dist2

  real(default) function obs_ktmeasure (prt1, prt2) result (kt)
    type(prt_t), intent(in) :: prt1, prt2
    real (default) :: q2, e1, e2
!   normalized scale to one for now!
    q2 = 1
    e1 = energy (prt_get_momentum (prt1))
    e2 = energy (prt_get_momentum (prt2))
    kt = (2/q2) * min(e1**2,e2**2) *  &
         (1 - enclosed_angle_ct(prt_get_momentum (prt1), &
         prt_get_momentum (prt2)))
  end function obs_ktmeasure

  subroutine event_vars_write (vars, unit)
    type(event_vars_t), intent(in) :: vars
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, *)  "Event index          = ", vars%event_index
    write (u, *)  "Process index        = ", vars%process_index
    write (u, *)  "Numerical process ID = ", vars%process_num_id
    write (u, *)  "Process ID           = ", char (vars%process_id)
    write (u, *)  "Process n_in         = ", vars%n_in
    write (u, *)  "Process n_out        = ", vars%n_out
    write (u, *)  "Process n_tot        = ", vars%n_tot
    write (u, *)  "Event sqrts_hat      = ", vars%sqrts_hat
    write (u, *)  "Event sqme           = ", vars%sqme
    write (u, *)  "Event sqme(ref)      = ", vars%sqme_ref
    write (u, *)  "Event weight         = ", vars%weight
    write (u, *)  "Event excess weight  = ", vars%excess
  end subroutine event_vars_write

  subroutine event_vars_write_raw (vars, u, version)
    type(event_vars_t), intent(in) :: vars
    integer, intent(in) :: u
    integer, intent(in) :: version
    write (u)  vars%event_index
    write (u)  vars%process_index
    select case (version)
    case (3:)
       write (u)  vars%n_in
       write (u)  vars%n_out
       write (u)  vars%n_tot
       write (u)  vars%sqrts
       write (u)  vars%sqrts_hat
    end select
    write (u)  vars%sqme
    write (u)  vars%sqme_ref
    write (u)  vars%weight
    write (u)  vars%excess
  end subroutine event_vars_write_raw

  subroutine event_vars_read_raw (vars, u, iostat, version)
    type(event_vars_t), intent(out) :: vars
    integer, intent(in) :: u
    integer, intent(out) :: iostat
    integer, intent(in) :: version
    read (u, iostat=iostat)  vars%event_index
    if (iostat /= 0) return
    read (u, iostat=iostat)  vars%process_index
    if (iostat /= 0) return
    select case (version)
    case (3:)
       read (u, iostat=iostat)  vars%n_in
       read (u, iostat=iostat)  vars%n_out
       read (u, iostat=iostat)  vars%n_tot
       read (u, iostat=iostat)  vars%sqrts
       read (u, iostat=iostat)  vars%sqrts_hat
    end select
    if (iostat /= 0) return
    read (u, iostat=iostat)  vars%sqme
    if (iostat /= 0) return
    read (u, iostat=iostat)  vars%sqme_ref
    if (iostat /= 0) return
    read (u, iostat=iostat)  vars%weight
    if (iostat /= 0) return
    read (u, iostat=iostat)  vars%excess
  end subroutine event_vars_read_raw

  subroutine var_list_append_event_vars (var_list, event_vars)
    type(var_list_t), intent(inout) :: var_list
    type(event_vars_t), intent(in), target :: event_vars
    logical, target, save :: known = .true.
    call var_list_append_int_ptr (var_list, &
         var_str ("event_index"), event_vars%event_index, &
         is_known = known, locked = .true., intrinsic = .true.)
    call var_list_append_int_ptr (var_list, &
         var_str ("process_index"), event_vars%process_index, &
         is_known = known, locked = .true., intrinsic = .true.)
    call var_list_append_int_ptr (var_list, &
         var_str ("process_num_id"), event_vars%process_num_id, &
         is_known = known, locked = .true., intrinsic = .true.)
    call var_list_append_string_ptr (var_list, &
         var_str ("$process_id"), event_vars%process_id, &     ! $
         is_known = known, locked = .true., intrinsic = .true.)
    call var_list_append_int_ptr (var_list, &
         var_str ("n_in"), event_vars%n_in, &
         is_known = known, locked = .true., intrinsic = .true.)
    call var_list_append_int_ptr (var_list, &
         var_str ("n_out"), event_vars%n_out, &
         is_known = known, locked = .true., intrinsic = .true.)
    call var_list_append_int_ptr (var_list, &
         var_str ("n_tot"), event_vars%n_tot, &
         is_known = known, locked = .true., intrinsic = .true.)
    call var_list_append_real_ptr (var_list, &
         var_str ("sqrts"), event_vars%sqrts, &
         is_known = known, locked = .true., intrinsic = .true.)
    call var_list_append_real_ptr (var_list, &
         var_str ("sqrts_hat"), event_vars%sqrts_hat, &
         is_known = known, locked = .true., intrinsic = .true.)
    call var_list_append_real_ptr (var_list, &
         var_str ("sqme"), event_vars%sqme, &
         is_known = known, locked = .true., intrinsic = .true.)
    call var_list_append_real_ptr (var_list, &
         var_str ("sqme_ref"), event_vars%sqme, &
         is_known = known, locked = .true., intrinsic = .true.)
    call var_list_append_real_ptr (var_list, &
         var_str ("event_weight"), event_vars%weight, &
         is_known = known, locked = .true., intrinsic = .true.)
    call var_list_append_real_ptr (var_list, &
         var_str ("event_excess_weight"), event_vars%excess, &
         is_known = known, locked = .true., intrinsic = .true.)
  end subroutine var_list_append_event_vars

  subroutine var_list_set_log &
       (var_list, name, lval, is_known, ignore, verbose, model_name)
    type(var_list_t), intent(inout), target :: var_list
    type(string_t), intent(in) :: name
    logical, intent(in) :: lval
    logical, intent(in) :: is_known
    logical, intent(in), optional :: ignore, verbose
    type(string_t), intent(in), optional :: model_name
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, V_LOG)
    if (associated (var)) then
       if (.not. var_entry_is_locked (var)) then
          select case (var%type)
          case (V_LOG)
             call var_entry_set_log (var, lval, is_known, verbose, model_name)
          case default
             call var_mismatch_error (name)
          end select
       else
          call var_locked_error (name)
       end if
    else
       call var_missing_error (name, ignore)
    end if
  end subroutine var_list_set_log
          
  subroutine var_list_set_int &
       (var_list, name, ival, is_known, ignore, verbose, model_name)
    type(var_list_t), intent(inout), target :: var_list
    type(string_t), intent(in) :: name
    integer, intent(in) :: ival
    logical, intent(in) :: is_known
    logical, intent(in), optional :: ignore, verbose
    type(string_t), intent(in), optional :: model_name
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, V_INT)
    if (associated (var)) then 
       if (.not. var_entry_is_locked (var)) then
          select case (var%type)
          case (V_INT)
             call var_entry_set_int (var, ival, is_known, verbose, model_name)
          case default
             call var_mismatch_error (name)
          end select
       else
          call var_locked_error (name)
       end if
    else
       call var_missing_error (name, ignore)
    end if
  end subroutine var_list_set_int
          
  subroutine var_list_set_real &
       (var_list, name, rval, is_known, ignore, verbose, model_name)
    type(var_list_t), intent(inout), target :: var_list
    type(string_t), intent(in) :: name
    real(default), intent(in) :: rval
    logical, intent(in) :: is_known
    logical, intent(in), optional :: ignore, verbose
    type(string_t), intent(in), optional :: model_name
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, V_REAL)
    if (associated (var)) then
       if (.not. var_entry_is_locked (var)) then
          select case (var%type)
          case (V_REAL)
             call var_entry_set_real (var, rval, is_known, verbose, model_name)
          case default
             call var_mismatch_error (name)
          end select
       else
          call var_locked_error (name)
       end if
    else
       call var_missing_error (name, ignore)
    end if
  end subroutine var_list_set_real
          
  subroutine var_list_set_cmplx &
       (var_list, name, cval, is_known, ignore, verbose, model_name)
    type(var_list_t), intent(inout), target :: var_list
    type(string_t), intent(in) :: name
    complex(default), intent(in) :: cval
    logical, intent(in) :: is_known
    logical, intent(in), optional :: ignore, verbose
    type(string_t), intent(in), optional :: model_name
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, V_CMPLX)
    if (associated (var)) then
       if (.not. var_entry_is_locked (var)) then
          select case (var%type)
          case (V_CMPLX)
             call var_entry_set_cmplx (var, cval, is_known, verbose, model_name)
          case default
             call var_mismatch_error (name)
          end select
       else
          call var_locked_error (name)
       end if
    else
       call var_missing_error (name, ignore)
    end if
  end subroutine var_list_set_cmplx
          
  subroutine var_list_set_pdg_array &
       (var_list, name, aval, is_known, ignore, verbose, model_name)
    type(var_list_t), intent(inout), target :: var_list
    type(string_t), intent(in) :: name
    type(pdg_array_t), intent(in) :: aval
    logical, intent(in) :: is_known
    logical, intent(in), optional :: ignore, verbose
    type(string_t), intent(in), optional :: model_name
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, V_PDG)
    if (associated (var)) then
       if (.not. var_entry_is_locked (var)) then
          select case (var%type)
          case (V_PDG)
             call var_entry_set_pdg_array &
                  (var, aval, is_known, verbose, model_name)
          case default
             call var_mismatch_error (name)
          end select
       else
          call var_locked_error (name)
       end if
    else
       call var_missing_error (name, ignore)
    end if
  end subroutine var_list_set_pdg_array
          
  subroutine var_list_set_subevt &
       (var_list, name, pval, is_known, ignore, verbose, model_name)
    type(var_list_t), intent(inout), target :: var_list
    type(string_t), intent(in) :: name
    type(subevt_t), intent(in) :: pval
    logical, intent(in) :: is_known
    logical, intent(in), optional :: ignore, verbose
    type(string_t), intent(in), optional :: model_name
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, V_SEV)
    if (associated (var)) then
       if (.not. var_entry_is_locked (var)) then
          select case (var%type)
          case (V_SEV)
             call var_entry_set_subevt &
                  (var, pval, is_known, verbose, model_name)
          case default
             call var_mismatch_error (name)
          end select
       else
          call var_locked_error (name)
       end if
    else
       call var_missing_error (name, ignore)
    end if
  end subroutine var_list_set_subevt
          
  subroutine var_list_set_string &
       (var_list, name, sval, is_known, ignore, verbose, model_name)
    type(var_list_t), intent(inout), target :: var_list
    type(string_t), intent(in) :: name
    type(string_t), intent(in) :: sval
    logical, intent(in) :: is_known
    logical, intent(in), optional :: ignore, verbose
    type(string_t), intent(in), optional :: model_name
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, V_STR)
    if (associated (var)) then
       if (.not. var_entry_is_locked (var)) then
          select case (var%type)
          case (V_STR)
             call var_entry_set_string &
                  (var, sval, is_known, verbose, model_name)
          case default
             call var_mismatch_error (name)
          end select
       else
          call var_locked_error (name)
       end if
    else
       call var_missing_error (name, ignore)
    end if
  end subroutine var_list_set_string
          
  subroutine var_mismatch_error (name)
    type(string_t), intent(in) :: name
    call msg_fatal ("Type mismatch for variable '" // char (name) // "'")
  end subroutine var_mismatch_error

  subroutine var_locked_error (name)
    type(string_t), intent(in) :: name
    call msg_error ("Variable '" // char (name) // "' is not user-definable")
  end subroutine var_locked_error

  subroutine var_missing_error (name, ignore)
    type(string_t), intent(in) :: name
    logical, intent(in), optional :: ignore
    logical :: error
    if (present (ignore)) then
       error = .not. ignore
    else
       error = .true.
    end if
    if (error) then
       call msg_fatal ("Variable '" // char (name) // "' has not been declared")
    end if
  end subroutine var_missing_error

  subroutine var_list_init_copy (var_list, model_var, user)
    type(var_list_t), intent(inout), target :: var_list
    type(var_entry_t), intent(in), target :: model_var
    logical, intent(in), optional :: user
    type(var_entry_t), pointer :: var
    if (.not. var_list_exists &
         (var_list, model_var%name, follow_link = .false.)) then
       allocate (var)
       call var_entry_init_copy (var, model_var, user)
       call var_list_append (var_list, var)
    end if
  end subroutine var_list_init_copy

  subroutine var_list_init_copies (var_list, model_vars, derived_only)
    type(var_list_t), intent(inout), target :: var_list
    type(var_list_t), intent(in) :: model_vars
    logical, intent(in), optional :: derived_only
    type(var_entry_t), pointer :: model_var, var
    type(string_t) :: name
    logical :: copy_all, locked, derived
    integer :: type
    copy_all = .true.
    if (present (derived_only))  copy_all = .not. derived_only
    model_var => model_vars%first
    do while (associated (model_var))
       name = var_entry_get_name (model_var)
       type = var_entry_get_type (model_var)
       locked = var_entry_is_locked (model_var)
       derived = type == V_REAL .and. locked
       if (copy_all .or. derived) then
          var => var_list_get_var_ptr &
               (var_list, name, type, follow_link = .false.)
          if (associated (var)) then
             call var_entry_clear_value (var)
          else
             allocate (var)
             call var_entry_init_copy (var, model_var)
             call var_list_append (var_list, var)
          end if
       end if
       model_var => model_var%next
    end do
  end subroutine var_list_init_copies

  subroutine var_list_clear_original_pointers (var_list)
    type(var_list_t), intent(inout) :: var_list
    type(var_entry_t), pointer :: var
    var => var_list%first
    do while (associated (var))
       call var_entry_clear_original_pointer (var)
       var => var%next
    end do
  end subroutine var_list_clear_original_pointers

  subroutine var_list_set_original_pointer (var_list, name, model_vars)
    type(var_list_t), intent(inout) :: var_list
    type(string_t), intent(in) :: name
    type(var_list_t), intent(in), target :: model_vars
    type(var_entry_t), pointer :: var, model_var
    integer :: type
    model_var => var_list_get_var_ptr (model_vars, name)
    if (associated (model_var)) then
       type = var_entry_get_type (model_var)
       var => var_list_get_var_ptr (var_list, name, type, follow_link=.false.)
       if (associated (var)) then
          call var_entry_set_original_pointer (var, model_var)
       end if
    end if
  end subroutine var_list_set_original_pointer

  subroutine var_list_set_original_pointers (var_list, model_vars)
    type(var_list_t), intent(inout) :: var_list
    type(var_list_t), intent(in), target :: model_vars
    type(var_entry_t), pointer :: var, model_var
    type(string_t) :: name
    integer :: type
    model_var => model_vars%first
    do while (associated (model_var))
       name = var_entry_get_name (model_var)
       type = var_entry_get_type (model_var)
       var => var_list_get_var_ptr (var_list, name, type, follow_link=.false.)
       if (associated (var)) then
          call var_entry_set_original_pointer (var, model_var)
       end if
       model_var => model_var%next
    end do
  end subroutine var_list_set_original_pointers

  subroutine var_list_synchronize (var_list, model_vars, reset_pointers)
    type(var_list_t), intent(inout) :: var_list
    type(var_list_t), intent(in), target :: model_vars
    logical, intent(in), optional :: reset_pointers
    type(var_entry_t), pointer :: var
    if (present (reset_pointers)) then
       if (reset_pointers) then
          call var_list_clear_original_pointers (var_list)
          call var_list_set_original_pointers (var_list, model_vars)
       end if
    end if
    var => var_list%first
    do while (associated (var))
       call var_entry_synchronize (var)
       var => var%next
    end do
  end subroutine var_list_synchronize

  recursive subroutine var_list_restore (var_list)
    type(var_list_t), intent(inout) :: var_list
    type(var_entry_t), pointer :: var
    var => var_list%first
    do while (associated (var))
       call var_entry_restore (var)
       var => var%next
    end do
  end subroutine var_list_restore

  recursive subroutine var_list_undefine (var_list, follow_link)
    type(var_list_t), intent(inout) :: var_list
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    logical :: rec
    rec = .true.;  if (present (follow_link))  rec = follow_link
    var => var_list%first
    do while (associated (var))
       call var_entry_undefine (var)
       var => var%next
    end do
    if (rec .and. associated (var_list%next)) then
       call var_list_undefine (var_list%next, follow_link=follow_link)
    end if
  end subroutine var_list_undefine

  recursive subroutine var_list_init_snapshot (var_list, vars_in, follow_link)
    type(var_list_t), intent(out) :: var_list
    type(var_list_t), intent(in) :: vars_in
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var, var_in
    type(var_list_t), pointer :: var_list_next
    logical :: rec
    rec = .true.;  if (present (follow_link))  rec = follow_link
    var_in => vars_in%first
    do while (associated (var_in))
       allocate (var)
       call var_entry_init_copy (var, var_in)
       call var_entry_set_original_pointer (var, var_in)
       call var_entry_synchronize (var)
       call var_entry_clear_original_pointer (var)
       call var_list_append (var_list, var)
       var_in => var_in%next
    end do
    if (rec .and. associated (vars_in%next)) then
       allocate (var_list_next)
       call var_list_init_snapshot (var_list_next, vars_in%next)
       call var_list_link (var_list, var_list_next)
    end if
  end subroutine var_list_init_snapshot

  subroutine var_list_check_user_var (var_list, name, type, new)
    type(var_list_t), intent(in), target :: var_list
    type(string_t), intent(in) :: name
    integer, intent(inout) :: type
    logical, intent(in) :: new
    type(var_entry_t), pointer :: var
    if (string_is_observable_id (name)) then
       call msg_error ("Variable name '" // char (name) &
            // "' is reserved for an observable")
       type = V_NONE
       return
    end if
    if (string_is_integer_result_var (name))  type = V_INT
    var => var_list_get_var_ptr (var_list, name)
    if (associated (var)) then
       if (type == V_NONE) then
          type = var_entry_get_type (var)
       end if
       if (var_entry_is_locked (var)) then
          call msg_error ("Variable '" // char (name) &
               // "' is not user-definable")
          type = V_NONE
          return
       else if (new) then
          if (var_entry_is_intrinsic (var)) then
             call msg_error ("Intrinsic variable '" &
                  // char (name) // "' redeclared")
             type = V_NONE
             return
          end if
          if (var_entry_get_type (var) /= type) then
             call msg_error ("Variable '" // char (name) // "' " &
                  // "redeclared with different type")
             type = V_NONE
             return
          end if
       end if
    else
       if (string_is_result_var (name)) then
          call msg_error ("Result variable '" // char (name) // "' " &
               // "set without prior integration")
          type = V_NONE
          return
       else if (string_is_num_id (name)) then
          call msg_error ("Numeric process ID '" // char (name) // "' " &
               // "set without process declaration")
          type = V_NONE
          return
       else if (.not. new) then
          call msg_error ("Variable '" // char (name) // "' " &
               // "set without declaration")
          type = V_NONE
          return
       end if
    end if
  end subroutine var_list_check_user_var

  function string_is_integer_result_var (string) result (flag)
    logical :: flag
    type(string_t), intent(in) :: string
    type(string_t) :: buffer, name, separator
    buffer = string
    call split (buffer, name, "(", separator=separator)  ! ")"
    if (separator == "(") then
       select case (char (name))
       case ("num_id", "n_calls")
          flag = .true.
       case default
          flag = .false.
       end select
    else
       flag = .false.
    end if
  end function string_is_integer_result_var

  function string_is_result_var (string) result (flag)
    logical :: flag
    type(string_t), intent(in) :: string
    type(string_t) :: buffer, name, separator
    buffer = string
    call split (buffer, name, "(", separator=separator)  ! ")"
    if (separator == "(") then
       select case (char (name))
       case ("n_calls", "integral", "error", "accuracy", "chi2", "efficiency")
          flag = .true.
       case default
          flag = .false.
       end select
    else
       flag = .false.
    end if
  end function string_is_result_var

  function string_is_num_id (string) result (flag)
    logical :: flag
    type(string_t), intent(in) :: string
    type(string_t) :: buffer, name, separator
    buffer = string
    call split (buffer, name, "(", separator=separator)  ! ")"
    if (separator == "(") then
       select case (char (name))
       case ("num_id")
          flag = .true.
       case default
          flag = .false.
       end select
    else
       flag = .false.
    end if
  end function string_is_num_id


end module variables
