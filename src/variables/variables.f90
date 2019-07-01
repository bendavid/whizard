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

module variables

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use format_utils, only: pac_fmt
  use format_defs, only: FMT_14, FMT_19
  use constants, only: eps0
  use diagnostics
  use pdg_arrays
  use subevents
  
  use var_base

  implicit none
  private

  public :: obs_unary_int
  public :: obs_unary_real
  public :: obs_binary_int
  public :: obs_binary_real
  public :: var_list_t
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
  public :: var_list_write
  public :: var_list_write_var
  public :: var_list_get_var_ptr
  public :: var_list_set_procvar_int
  public :: var_list_set_procvar_real
  public :: var_list_append_obs1_iptr
  public :: var_list_append_obs2_iptr
  public :: var_list_append_obs1_rptr
  public :: var_list_append_obs2_rptr
  public :: var_list_append_uobs_int
  public :: var_list_append_uobs_real
  public :: var_list_import
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

  type, extends (vars_t) :: var_list_t
     private
     type(var_entry_t), pointer :: first => null ()
     type(var_entry_t), pointer :: last => null ()
     type(var_list_t), pointer :: next => null ()
   contains
     procedure :: link => var_list_link
     procedure :: final => var_list_final
     procedure :: write => var_list_write
     procedure :: get_type => var_list_get_type
     procedure :: contains => var_list_exists
     procedure :: is_intrinsic => var_list_is_intrinsic
     procedure :: is_known => var_list_is_known
     procedure :: is_locked => var_list_is_locked
     procedure :: get_var_properties => var_list_get_var_properties
     procedure :: get_lval => var_list_get_lval
     procedure :: get_ival => var_list_get_ival
     procedure :: get_rval => var_list_get_rval
     procedure :: get_cval => var_list_get_cval
     procedure :: get_pval => var_list_get_pval
     procedure :: get_aval => var_list_get_aval
     procedure :: get_sval => var_list_get_sval
     procedure :: get_lptr => var_list_get_lptr
     procedure :: get_iptr => var_list_get_iptr
     procedure :: get_rptr => var_list_get_rptr
     procedure :: get_cptr => var_list_get_cptr
     procedure :: get_aptr => var_list_get_aptr
     procedure :: get_pptr => var_list_get_pptr
     procedure :: get_sptr => var_list_get_sptr
     procedure :: get_obs1_iptr => var_list_get_obs1_iptr
     procedure :: get_obs2_iptr => var_list_get_obs2_iptr
     procedure :: get_obs1_rptr => var_list_get_obs1_rptr
     procedure :: get_obs2_rptr => var_list_get_obs2_rptr
     procedure :: unset => var_list_clear
     procedure :: set_ival => var_list_set_ival
     procedure :: set_rval => var_list_set_rval
     procedure :: set_cval => var_list_set_cval
     procedure :: set_lval => var_list_set_lval
     procedure :: set_sval => var_list_set_sval
     procedure :: set_log => var_list_set_log
     procedure :: set_int => var_list_set_int
     procedure :: set_real => var_list_set_real
     procedure :: set_cmplx => var_list_set_cmplx
     procedure :: set_subevt => var_list_set_subevt
     procedure :: set_pdg_array => var_list_set_pdg_array
     procedure :: set_string => var_list_set_string
  end type var_list_t


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

  subroutine var_entry_clear (var)
    type(var_entry_t), intent(inout) :: var
    var%is_known = .false.
  end subroutine var_entry_clear

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

  recursive subroutine var_entry_write (var, unit, model_name, &
       intrinsic, pacified)
    type(var_entry_t), intent(in) :: var
    integer, intent(in), optional :: unit
    type(string_t), intent(in), optional :: model_name
    logical, intent(in), optional :: intrinsic
    logical, intent(in), optional :: pacified
    logical :: num_pac
    real(default) :: rval
    complex(default) :: cval
    integer :: u    
    character(len=7) :: fmt   
    call pac_fmt (fmt, FMT_19, FMT_14, pacified)
    u = given_output_unit (unit);  if (u < 0)  return
    if (present (intrinsic)) then
       if (var%is_intrinsic .neqv. intrinsic)  return
    end if
    if (.not. var%is_defined) then
       write (u, "(A,1x)", advance="no")  "[undefined]"
    end if
    if (.not. var%is_intrinsic) then
       write (u, "(A,1x)", advance="no")  "[user variable]"
    end if
    num_pac = .false.; if (present (pacified))  num_pac = pacified
    if (present (model_name)) then
       write (u, "(A,A)", advance="no")  char(model_name), "."
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
          write (u, "(I0)")  var%ival
       else
          write (u, "(A)")  "[unknown integer]"
       end if
    case (V_REAL)
       if (var%is_known) then
          rval = var%rval
          if (num_pac) then
             call pacify (rval, 10 * eps0)
          end if
          write (u, "(" // fmt // ")")  rval          
       else
          write (u, "(A)")  "[unknown real]"
       end if
    case (V_CMPLX)
       if (var%is_known) then
          cval = var%cval
          if (num_pac) then
             call pacify (cval, 10 * eps0)
          end if
          write (u, "('('," // fmt // ",','," // fmt // ",')')")  cval 
       else
          write (u, "(A)")  "[unknown complex]"
       end if
    case (V_SEV)
       if (var%is_known) then
          call subevt_write (var%pval, unit, prefix="       ", &
               pacified = pacified)
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

  function var_entry_is_locked (var, force) result (locked)
    logical :: locked
    type(var_entry_t), intent(in) :: var
    logical, intent(in), optional :: force
    if (present (force)) then
       if (force) then
          locked = .false.;  return
       end if
    end if
    locked = var%is_locked
  end function var_entry_is_locked

  function var_entry_is_intrinsic (var) result (flag)
    logical :: flag
    type(var_entry_t), intent(in) :: var
    flag = var%is_intrinsic
  end function var_entry_is_intrinsic

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
    if (present (verbose)) then
       if (verbose) then
          call var_entry_write (var, model_name=model_name)
          call var_entry_write (var, model_name=model_name, unit=u)
          if (u >= 0) flush (u)
       end if
    end if
  end subroutine var_entry_set_int

  recursive subroutine var_entry_set_real &
       (var, rval, is_known, verbose, model_name, pacified)
    type(var_entry_t), intent(inout) :: var
    real(default), intent(in) :: rval
    logical, intent(in) :: is_known
    logical, intent(in), optional :: verbose, pacified
    type(string_t), intent(in), optional :: model_name
    integer :: u
    u = logfile_unit ()
    var%rval = rval
    var%is_known = is_known
    var%is_defined = .true.
    if (present (verbose)) then
       if (verbose) then
          call var_entry_write &
               (var, model_name=model_name, pacified = pacified)
          call var_entry_write &
               (var, model_name=model_name, unit=u, pacified = pacified)
          if (u >= 0) flush (u)
       end if
    end if
  end subroutine var_entry_set_real
  
  recursive subroutine var_entry_set_cmplx &
       (var, cval, is_known, verbose, model_name, pacified)
    type(var_entry_t), intent(inout) :: var
    complex(default), intent(in) :: cval
    logical, intent(in) :: is_known
    logical, intent(in), optional :: verbose, pacified
    type(string_t), intent(in), optional :: model_name
    integer :: u
    u = logfile_unit ()
    var%cval = cval
    var%is_known = is_known
    var%is_defined = .true.
    if (present (verbose)) then
       if (verbose) then
          call var_entry_write &
               (var, model_name=model_name, pacified = pacified)
          call var_entry_write &
               (var, model_name=model_name, unit=u, pacified = pacified)
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
  end subroutine var_entry_init_copy

  subroutine var_entry_copy_value (var, original)
    type(var_entry_t), intent(inout) :: var
    type(var_entry_t), intent(in), target :: original
    if (var_entry_is_known (original)) then
       select case (original%type)
       case (V_LOG)
          call var_entry_set_log (var, var_entry_get_lval (original), .true.)
       case (V_INT)
          call var_entry_set_int (var, var_entry_get_ival (original), .true.)
       case (V_REAL)
          call var_entry_set_real (var, var_entry_get_rval (original), .true.)
       case (V_CMPLX)
          call var_entry_set_cmplx (var, var_entry_get_cval (original), .true.)
       case (V_SEV)
          call var_entry_set_subevt (var, var_entry_get_pval (original), .true.)
       case (V_PDG)
          call var_entry_set_pdg_array (var, var_entry_get_aval (original), .true.)
       case (V_STR)
          call var_entry_set_string (var, var_entry_get_sval (original), .true.)
       end select
    else
       call var_entry_clear (var)
    end if
  end subroutine var_entry_copy_value

  subroutine var_list_link (vars, target_vars)
    class(var_list_t), intent(inout) :: vars
    class(vars_t), intent(in), target :: target_vars
    select type (target_vars)
    type is (var_list_t)
       vars%next => target_vars
    class default
       call msg_bug ("var_list_link: unsupported target type")
    end select
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

  recursive subroutine var_list_final (vars, follow_link)
    class(var_list_t), intent(inout) :: vars
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    vars%last => null ()
    do while (associated (vars%first))
       var => vars%first
       vars%first => var%next
       call var_entry_final (var)
       deallocate (var)
    end do
    if (present (follow_link)) then
       if (follow_link) then
          if (associated (vars%next)) then
             call vars%next%final (follow_link)
             deallocate (vars%next)
          end if
       end if
    end if
  end subroutine var_list_final

  recursive subroutine var_list_write &
       (var_list, unit, follow_link, only_type, prefix, model_name, &
        intrinsic, pacified)
    class(var_list_t), intent(in), target :: var_list
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: follow_link
    integer, intent(in), optional :: only_type
    character(*), intent(in), optional :: prefix
    type(string_t), intent(in), optional :: model_name
!     logical, intent(in), optional :: show_ptr
    logical, intent(in), optional :: intrinsic
    logical, intent(in), optional :: pacified
    type(var_entry_t), pointer :: var
    integer :: u, length
    logical :: write_this, write_next
    u = given_output_unit (unit);  if (u < 0)  return
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
                  (var, unit, model_name = model_name, &
                   intrinsic = intrinsic, pacified = pacified)
          end if
          var => var%next
       end do
    end if
    if (present (follow_link)) then
       write_next = follow_link .and. associated (var_list%next)
    else
       write_next = associated (var_list%next)
    end if
    if (write_next) then
       call var_list_write (var_list%next, &
            unit, follow_link, only_type, prefix, model_name, &
            intrinsic, pacified)
    end if
  end subroutine var_list_write

  recursive subroutine var_list_write_var &
       (var_list, name, unit, type, follow_link, &
       model_name, pacified)
    type(var_list_t), intent(in), target :: var_list
    type(string_t), intent(in) :: name
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: type
    logical, intent(in), optional :: follow_link
    type(string_t), intent(in), optional :: model_name
    logical, intent(in), optional :: pacified
    type(var_entry_t), pointer :: var
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    var => var_list_get_var_ptr &
         (var_list, name, type, follow_link=follow_link, defined=.true.)
    if (associated (var)) then
       call var_entry_write &
            (var, unit, model_name = model_name, &
            pacified = pacified)
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
    class(var_list_t), intent(in), target :: var_list
    type(string_t), intent(in) :: name
    logical, intent(in), optional :: follow_link
    integer :: type
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, follow_link=follow_link)
    if (associated (var)) then
       type = var%type
    else
       type = V_NONE
    end if
  end function var_list_get_type

  function var_list_exists (vars, name, follow_link) result (lval)
    logical :: lval
    type(string_t), intent(in) :: name
    class(var_list_t), intent(in) :: vars
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (vars, name, follow_link=follow_link)
    lval = associated (var)
  end function var_list_exists

  function var_list_is_intrinsic (vars, name, follow_link) result (lval)
    logical :: lval
    type(string_t), intent(in) :: name
    class(var_list_t), intent(in) :: vars
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (vars, name, follow_link=follow_link)
    if (associated (var)) then
       lval = var%is_intrinsic
    else
       lval = .false.
    end if
  end function var_list_is_intrinsic

  function var_list_is_known (vars, name, follow_link) result (lval)
    logical :: lval
    type(string_t), intent(in) :: name
    class(var_list_t), intent(in) :: vars
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (vars, name, follow_link=follow_link)
    if (associated (var)) then
       lval = var%is_known
    else
       lval = .false.
    end if
  end function var_list_is_known

  function var_list_is_locked (vars, name, follow_link) result (lval)
    logical :: lval
    type(string_t), intent(in) :: name
    class(var_list_t), intent(in) :: vars
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (vars, name, follow_link=follow_link)
    if (associated (var)) then
       lval = var_entry_is_locked (var)
    else
       lval = .false.
    end if
  end function var_list_is_locked

  subroutine var_list_get_var_properties (vars, name, req_type, follow_link, &
       type, is_defined, is_known, is_locked)
    class(var_list_t), intent(in) :: vars
    type(string_t), intent(in) :: name
    integer, intent(in), optional :: req_type
    logical, intent(in), optional :: follow_link
    integer, intent(out), optional :: type
    logical, intent(out), optional :: is_defined, is_known, is_locked
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr &
         (vars, name, type=req_type, follow_link=follow_link)
    if (associated (var)) then
       if (present (type))  type = var_entry_get_type (var)
       if (present (is_defined))  is_defined = var_entry_is_defined (var)
       if (present (is_known))  is_known = var_entry_is_known (var)
       if (present (is_locked))  is_locked = var_entry_is_locked (var)
    else
       if (present (type))  type = V_NONE
       if (present (is_defined))  is_defined = .false.
       if (present (is_known))  is_known = .false.
       if (present (is_locked))  is_locked = .false.
    end if
  end subroutine var_list_get_var_properties
    
  function var_list_get_lval (vars, name, follow_link) result (lval)
    logical :: lval
    type(string_t), intent(in) :: name
    class(var_list_t), intent(in) :: vars
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr &
         (vars, name, V_LOG, follow_link, defined=.true.)
    if (associated (var)) then
       if (var_has_value (var)) then
          lval = var%lval
       else
          lval = .false.
       end if
    else
       lval = .false.
    end if
  end function var_list_get_lval
  
  function var_list_get_ival (vars, name, follow_link) result (ival)
    integer :: ival
    type(string_t), intent(in) :: name
    class(var_list_t), intent(in) :: vars
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr &
         (vars, name, V_INT, follow_link, defined=.true.)
    if (associated (var)) then
       if (var_has_value (var)) then
          ival = var%ival
       else
          ival = 0
       end if
    else
       ival = 0
    end if
  end function var_list_get_ival
  
  function var_list_get_rval (vars, name, follow_link) result (rval)
    real(default) :: rval
    type(string_t), intent(in) :: name
    class(var_list_t), intent(in) :: vars
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr &
         (vars, name, V_REAL, follow_link, defined=.true.)
    if (associated (var)) then
       if (var_has_value (var)) then
          rval = var%rval
       else
          rval = 0
       end if
    else
       rval = 0
    end if
  end function var_list_get_rval
    
  function var_list_get_cval (vars, name, follow_link) result (cval)
    complex(default) :: cval
    type(string_t), intent(in) :: name
    class(var_list_t), intent(in) :: vars
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr &
         (vars, name, V_CMPLX, follow_link, defined=.true.)
    if (associated (var)) then
       if (var_has_value (var)) then
          cval = var%cval
       else
          cval = 0
       end if
    else
       cval = 0
    end if
  end function var_list_get_cval

  function var_list_get_aval (vars, name, follow_link) result (aval)
    type(pdg_array_t) :: aval
    type(string_t), intent(in) :: name
    class(var_list_t), intent(in) :: vars
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr &
         (vars, name, V_PDG, follow_link, defined=.true.)
    if (associated (var)) then
       if (var_has_value (var)) then
          aval = var%aval
       end if
    end if    
  end function var_list_get_aval
  
  function var_list_get_pval (vars, name, follow_link) result (pval)
    type(subevt_t) :: pval
    type(string_t), intent(in) :: name
    class(var_list_t), intent(in) :: vars
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr &
         (vars, name, V_SEV, follow_link, defined=.true.)
    if (associated (var)) then
       if (var_has_value (var)) then
          pval = var%pval
       end if
    end if
  end function var_list_get_pval
  
  function var_list_get_sval (vars, name, follow_link) result (sval)
    type(string_t) :: sval
    type(string_t), intent(in) :: name
    class(var_list_t), intent(in) :: vars
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr &
         (vars, name, V_STR, follow_link, defined=.true.)
    if (associated (var)) then
       if (var_has_value (var)) then
          sval = var%sval
       else
          sval = ""
       end if
    else
       sval = ""
    end if
  end function var_list_get_sval
  
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

  subroutine var_list_get_lptr (var_list, name, lptr, known)
    class(var_list_t), intent(in) :: var_list
    type(string_t), intent(in) :: name
    logical, pointer, intent(out) :: lptr
    logical, pointer, intent(out), optional :: known
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, V_LOG)
    if (associated (var)) then
       lptr => var_entry_get_lval_ptr (var)
       if (present (known))  known => var_entry_get_known_ptr (var)
    else
       lptr => null ()
       if (present (known))  known => null ()
    end if
  end subroutine var_list_get_lptr
    
  subroutine var_list_get_iptr (var_list, name, iptr, known)
    class(var_list_t), intent(in) :: var_list
    type(string_t), intent(in) :: name
    integer, pointer, intent(out) :: iptr
    logical, pointer, intent(out), optional :: known
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, V_INT)
    if (associated (var)) then
       iptr => var_entry_get_ival_ptr (var)
       if (present (known))  known => var_entry_get_known_ptr (var)
    else
       iptr => null ()
       if (present (known))  known => null ()
    end if
  end subroutine var_list_get_iptr
    
  subroutine var_list_get_rptr (var_list, name, rptr, known)
    class(var_list_t), intent(in) :: var_list
    type(string_t), intent(in) :: name
    real(default), pointer, intent(out) :: rptr
    logical, pointer, intent(out), optional :: known
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, V_REAL)
    if (associated (var)) then
       rptr => var_entry_get_rval_ptr (var)
       if (present (known))  known => var_entry_get_known_ptr (var)
    else
       rptr => null ()
       if (present (known))  known => null ()
    end if
  end subroutine var_list_get_rptr
    
  subroutine var_list_get_cptr (var_list, name, cptr, known)
    class(var_list_t), intent(in) :: var_list
    type(string_t), intent(in) :: name
    complex(default), pointer, intent(out) :: cptr
    logical, pointer, intent(out), optional :: known
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, V_CMPLX)
    if (associated (var)) then
       cptr => var_entry_get_cval_ptr (var)
       if (present (known))  known => var_entry_get_known_ptr (var)
    else
       cptr => null ()
       if (present (known))  known => null ()
    end if
  end subroutine var_list_get_cptr
    
  subroutine var_list_get_aptr (var_list, name, aptr, known)
    class(var_list_t), intent(in) :: var_list
    type(string_t), intent(in) :: name
    type(pdg_array_t), pointer, intent(out) :: aptr
    logical, pointer, intent(out), optional :: known
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, V_PDG)
    if (associated (var)) then
       aptr => var_entry_get_aval_ptr (var)
       if (present (known))  known => var_entry_get_known_ptr (var)
    else
       aptr => null ()
       if (present (known))  known => null ()
    end if
  end subroutine var_list_get_aptr
    
  subroutine var_list_get_pptr (var_list, name, pptr, known)
    class(var_list_t), intent(in) :: var_list
    type(string_t), intent(in) :: name
    type(subevt_t), pointer, intent(out) :: pptr
    logical, pointer, intent(out), optional :: known
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, V_SEV)
    if (associated (var)) then
       pptr => var_entry_get_pval_ptr (var)
       if (present (known))  known => var_entry_get_known_ptr (var)
    else
       pptr => null ()
       if (present (known))  known => null ()
    end if
  end subroutine var_list_get_pptr
    
  subroutine var_list_get_sptr (var_list, name, sptr, known)
    class(var_list_t), intent(in) :: var_list
    type(string_t), intent(in) :: name
    type(string_t), pointer, intent(out) :: sptr
    logical, pointer, intent(out), optional :: known
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, V_STR)
    if (associated (var)) then
       sptr => var_entry_get_sval_ptr (var)
       if (present (known))  known => var_entry_get_known_ptr (var)
    else
       sptr => null ()
       if (present (known))  known => null ()
    end if
  end subroutine var_list_get_sptr
    
  subroutine var_list_get_obs1_iptr (var_list, name, obs1_iptr, p1)
    class(var_list_t), intent(in) :: var_list
    type(string_t), intent(in) :: name
    procedure(obs_unary_int), pointer, intent(out) :: obs1_iptr
    type(prt_t), pointer, intent(out) :: p1
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, V_OBS1_INT)
    if (associated (var)) then
       call var_entry_assign_obs1_int_ptr (obs1_iptr, var)
       p1 => var_entry_get_prt1_ptr (var)
    else
       obs1_iptr => null ()
       p1 => null ()
    end if
  end subroutine var_list_get_obs1_iptr
  
  subroutine var_list_get_obs2_iptr (var_list, name, obs2_iptr, p1, p2)
    class(var_list_t), intent(in) :: var_list
    type(string_t), intent(in) :: name
    procedure(obs_binary_int), pointer, intent(out) :: obs2_iptr
    type(prt_t), pointer, intent(out) :: p1, p2
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, V_OBS2_INT)
    if (associated (var)) then
       call var_entry_assign_obs2_int_ptr (obs2_iptr, var)
       p1 => var_entry_get_prt1_ptr (var)
       p2 => var_entry_get_prt2_ptr (var)
    else
       obs2_iptr => null ()
       p1 => null ()
       p2 => null ()
    end if
  end subroutine var_list_get_obs2_iptr
  
  subroutine var_list_get_obs1_rptr (var_list, name, obs1_rptr, p1)
    class(var_list_t), intent(in) :: var_list
    type(string_t), intent(in) :: name
    procedure(obs_unary_real), pointer, intent(out) :: obs1_rptr
    type(prt_t), pointer, intent(out) :: p1
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, V_OBS1_REAL)
    if (associated (var)) then
       call var_entry_assign_obs1_real_ptr (obs1_rptr, var)
       p1 => var_entry_get_prt1_ptr (var)
    else
       obs1_rptr => null ()
       p1 => null ()
    end if
  end subroutine var_list_get_obs1_rptr
  
  subroutine var_list_get_obs2_rptr (var_list, name, obs2_rptr, p1, p2)
    class(var_list_t), intent(in) :: var_list
    type(string_t), intent(in) :: name
    procedure(obs_binary_real), pointer, intent(out) :: obs2_rptr
    type(prt_t), pointer, intent(out) :: p1, p2
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, V_OBS2_REAL)
    if (associated (var)) then
       call var_entry_assign_obs2_real_ptr (obs2_rptr, var)
       p1 => var_entry_get_prt1_ptr (var)
       p2 => var_entry_get_prt2_ptr (var)
    else
       obs2_rptr => null ()
       p1 => null ()
       p2 => null ()
    end if
  end subroutine var_list_get_obs2_rptr
  
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
       call var_list%set_int (var_name, ival, is_known=.true.)
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
       call var_list%set_real (var_name, rval, is_known=.true.)
    end if
  end subroutine var_list_set_procvar_real

  subroutine var_list_append_obs1_iptr (var_list, name, obs1_iptr, p1)
    type(var_list_t), intent(inout) :: var_list
    type(string_t), intent(in) :: name
    procedure(obs_unary_int) :: obs1_iptr
    type(prt_t), intent(in), target :: p1
    type(var_entry_t), pointer :: var
    allocate (var)
    call var_entry_init_obs (var, name, V_OBS1_INT, p1)
    var%obs1_int => obs1_iptr
    call var_list_append (var_list, var)
  end subroutine var_list_append_obs1_iptr
  
  subroutine var_list_append_obs2_iptr (var_list, name, obs2_iptr, p1, p2)
    type(var_list_t), intent(inout) :: var_list
    type(string_t), intent(in) :: name
    procedure(obs_binary_int) :: obs2_iptr
    type(prt_t), intent(in), target :: p1, p2
    type(var_entry_t), pointer :: var
    allocate (var)
    call var_entry_init_obs (var, name, V_OBS2_INT, p1, p2)
    var%obs2_int => obs2_iptr
    call var_list_append (var_list, var)
  end subroutine var_list_append_obs2_iptr
  
  subroutine var_list_append_obs1_rptr (var_list, name, obs1_rptr, p1)
    type(var_list_t), intent(inout) :: var_list
    type(string_t), intent(in) :: name
    procedure(obs_unary_real) :: obs1_rptr
    type(prt_t), intent(in), target :: p1
    type(var_entry_t), pointer :: var
    allocate (var)
    call var_entry_init_obs (var, name, V_OBS1_REAL, p1)
    var%obs1_real => obs1_rptr
    call var_list_append (var_list, var)
  end subroutine var_list_append_obs1_rptr
  
  subroutine var_list_append_obs2_rptr (var_list, name, obs2_rptr, p1, p2)
    type(var_list_t), intent(inout) :: var_list
    type(string_t), intent(in) :: name
    procedure(obs_binary_real) :: obs2_rptr
    type(prt_t), intent(in), target :: p1, p2
    type(var_entry_t), pointer :: var
    allocate (var)
    call var_entry_init_obs (var, name, V_OBS2_REAL, p1, p2)
    var%obs2_real => obs2_rptr
    call var_list_append (var_list, var)
  end subroutine var_list_append_obs2_rptr
  
  subroutine var_list_append_uobs_int (var_list, name, p1, p2)
    type(var_list_t), intent(inout) :: var_list
    type(string_t), intent(in) :: name
    type(prt_t), intent(in), target :: p1
    type(prt_t), intent(in), target, optional :: p2
    type(var_entry_t), pointer :: var
    allocate (var)
    if (present (p2)) then
       call var_entry_init_obs (var, name, V_UOBS2_INT, p1, p2)
    else
       call var_entry_init_obs (var, name, V_UOBS1_INT, p1)
    end if
    call var_list_append (var_list, var)
  end subroutine var_list_append_uobs_int
  
  subroutine var_list_append_uobs_real (var_list, name, p1, p2)
    type(var_list_t), intent(inout) :: var_list
    type(string_t), intent(in) :: name
    type(prt_t), intent(in), target :: p1
    type(prt_t), intent(in), target, optional :: p2
    type(var_entry_t), pointer :: var
    allocate (var)
    if (present (p2)) then
       call var_entry_init_obs (var, name, V_UOBS2_REAL, p1, p2)
    else
       call var_entry_init_obs (var, name, V_UOBS1_REAL, p1)
    end if
    call var_list_append (var_list, var)
  end subroutine var_list_append_uobs_real
  
  subroutine var_list_clear (vars, name, follow_link)
    class(var_list_t), intent(inout) :: vars
    type(string_t), intent(in) :: name
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (vars, name, follow_link=follow_link)
    if (associated (var)) then
       call var_entry_clear (var)
    end if
  end subroutine var_list_clear
  
  subroutine var_list_set_ival (vars, name, ival, follow_link)
    class(var_list_t), intent(inout) :: vars
    type(string_t), intent(in) :: name
    integer, intent(in) :: ival
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (vars, name, follow_link=follow_link)
    if (associated (var)) then
       call var_entry_set_int (var, ival, is_known=.true.)
    end if
  end subroutine var_list_set_ival
  
  subroutine var_list_set_rval (vars, name, rval, follow_link)
    class(var_list_t), intent(inout) :: vars
    type(string_t), intent(in) :: name
    real(default), intent(in) :: rval
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (vars, name, follow_link=follow_link)
    if (associated (var)) then
       call var_entry_set_real (var, rval, is_known=.true.)
    end if
  end subroutine var_list_set_rval
  
  subroutine var_list_set_cval (vars, name, cval, follow_link)
    class(var_list_t), intent(inout) :: vars
    type(string_t), intent(in) :: name
    complex(default), intent(in) :: cval
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (vars, name, follow_link=follow_link)
    if (associated (var)) then
       call var_entry_set_cmplx (var, cval, is_known=.true.)
    end if
  end subroutine var_list_set_cval
  
  subroutine var_list_set_lval (vars, name, lval, follow_link)
    class(var_list_t), intent(inout) :: vars
    type(string_t), intent(in) :: name
    logical, intent(in) :: lval
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (vars, name, follow_link=follow_link)
    if (associated (var)) then
       call var_entry_set_log (var, lval, is_known=.true.)
    end if
  end subroutine var_list_set_lval
  
  subroutine var_list_set_sval (vars, name, sval, follow_link)
    class(var_list_t), intent(inout) :: vars
    type(string_t), intent(in) :: name
    type(string_t), intent(in) :: sval
    logical, intent(in), optional :: follow_link
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (vars, name, follow_link=follow_link)
    if (associated (var)) then
       call var_entry_set_string (var, sval, is_known=.true.)
    end if
  end subroutine var_list_set_sval
  
  subroutine var_list_set_log &
       (var_list, name, lval, is_known, ignore, force, verbose, model_name)
    class(var_list_t), intent(inout), target :: var_list
    type(string_t), intent(in) :: name
    logical, intent(in) :: lval
    logical, intent(in) :: is_known
    logical, intent(in), optional :: ignore, force, verbose
    type(string_t), intent(in), optional :: model_name
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, V_LOG)
    if (associated (var)) then
       if (.not. var_entry_is_locked (var, force)) then
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
       (var_list, name, ival, is_known, ignore, force, verbose, model_name)
    class(var_list_t), intent(inout), target :: var_list
    type(string_t), intent(in) :: name
    integer, intent(in) :: ival
    logical, intent(in) :: is_known
    logical, intent(in), optional :: ignore, force, verbose
    type(string_t), intent(in), optional :: model_name
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, V_INT)
    if (associated (var)) then 
       if (.not. var_entry_is_locked (var, force)) then
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
       (var_list, name, rval, is_known, ignore, force, &
        verbose, model_name, pacified)
    class(var_list_t), intent(inout), target :: var_list
    type(string_t), intent(in) :: name
    real(default), intent(in) :: rval
    logical, intent(in) :: is_known
    logical, intent(in), optional :: ignore, force, verbose, pacified
    type(string_t), intent(in), optional :: model_name
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, V_REAL)
    if (associated (var)) then
       if (.not. var_entry_is_locked (var, force)) then
          select case (var%type)
          case (V_REAL)
             call var_entry_set_real &
                  (var, rval, is_known, verbose, model_name, pacified)
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
       (var_list, name, cval, is_known, ignore, force, &
        verbose, model_name, pacified)
    class(var_list_t), intent(inout), target :: var_list
    type(string_t), intent(in) :: name
    complex(default), intent(in) :: cval
    logical, intent(in) :: is_known
    logical, intent(in), optional :: ignore, force, verbose, pacified
    type(string_t), intent(in), optional :: model_name
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, V_CMPLX)
    if (associated (var)) then
       if (.not. var_entry_is_locked (var, force)) then
          select case (var%type)
          case (V_CMPLX)
             call var_entry_set_cmplx &
                  (var, cval, is_known, verbose, model_name, pacified)
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
       (var_list, name, aval, is_known, ignore, force, verbose, model_name)
    class(var_list_t), intent(inout), target :: var_list
    type(string_t), intent(in) :: name
    type(pdg_array_t), intent(in) :: aval
    logical, intent(in) :: is_known
    logical, intent(in), optional :: ignore, force, verbose
    type(string_t), intent(in), optional :: model_name
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, V_PDG)
    if (associated (var)) then
       if (.not. var_entry_is_locked (var, force)) then
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
       (var_list, name, pval, is_known, ignore, force, verbose, model_name)
    class(var_list_t), intent(inout), target :: var_list
    type(string_t), intent(in) :: name
    type(subevt_t), intent(in) :: pval
    logical, intent(in) :: is_known
    logical, intent(in), optional :: ignore, force, verbose
    type(string_t), intent(in), optional :: model_name
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, V_SEV)
    if (associated (var)) then
       if (.not. var_entry_is_locked (var, force)) then
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
       (var_list, name, sval, is_known, ignore, force, verbose, model_name)
    class(var_list_t), intent(inout), target :: var_list
    type(string_t), intent(in) :: name
    type(string_t), intent(in) :: sval
    logical, intent(in) :: is_known
    logical, intent(in), optional :: ignore, force, verbose
    type(string_t), intent(in), optional :: model_name
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name, V_STR)
    if (associated (var)) then
       if (.not. var_entry_is_locked (var, force)) then
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

  subroutine var_list_import (var_list, src_list)
    type(var_list_t), intent(inout) :: var_list
    type(var_list_t), intent(in) :: src_list
    type(var_entry_t), pointer :: var, src
    var => var_list%first
    do while (associated (var))
       src => var_list_get_var_ptr (src_list, var%name)
       if (associated (src)) then
          call var_entry_copy_value (var, src)
       end if
       var => var%next
    end do
  end subroutine var_list_import
          
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
       call var_entry_copy_value (var, var_in)
       call var_list_append (var_list, var)
       var_in => var_in%next
    end do
    if (rec .and. associated (vars_in%next)) then
       allocate (var_list_next)
       call var_list_init_snapshot (var_list_next, vars_in%next)
       call var_list%link (var_list_next)
    end if
  end subroutine var_list_init_snapshot

  subroutine var_list_check_user_var (var_list, name, type, new)
    type(var_list_t), intent(in), target :: var_list
    type(string_t), intent(in) :: name
    integer, intent(inout) :: type
    logical, intent(in) :: new
    type(var_entry_t), pointer :: var
    var => var_list_get_var_ptr (var_list, name)
    if (associated (var)) then
       if (type == V_NONE) then
          type = var_entry_get_type (var)
       end if
       if (var_entry_is_locked (var)) then
          call msg_fatal ("Variable '" // char (name) &
               // "' is not user-definable")
          type = V_NONE
          return
       else if (new) then
          if (var_entry_is_intrinsic (var)) then
             call msg_fatal ("Intrinsic variable '" &
                  // char (name) // "' redeclared")
             type = V_NONE
             return
          end if
          if (var_entry_get_type (var) /= type) then
             call msg_fatal ("Variable '" // char (name) // "' " &
                  // "redeclared with different type")
             type = V_NONE
             return
          end if
       end if
    end if
  end subroutine var_list_check_user_var


end module variables
