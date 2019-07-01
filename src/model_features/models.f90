! WHIZARD 2.2.6 May 02 2015
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

module models

  use, intrinsic :: iso_c_binding !NODEP!
  
  use kinds, only: default
  use kinds, only: i8, i32
  use kinds, only: c_default_float
  use iso_varying_string, string_t => varying_string
  use format_defs, only: FMT_19
  use io_units
  use unit_tests
  use diagnostics
  use md5
  use os_interface
  use physics_defs, only: UNDEFINED, SCALAR, SPINOR
  use model_data
  
  use ifiles
  use syntax_rules
  use lexers
  use parser
  use pdg_arrays
  use variables
  use expr_base
  use eval_trees

  implicit none
  private

  public :: model_t
  public :: syntax_model_file_init
  public :: syntax_model_file_final
  public :: syntax_model_file_write
  public :: model_list_t
!   public :: model_pointer_to_instance
!   public :: model_pointer_delete_instance
  public :: models_test

  integer, parameter :: PAR_NONE = 0
  integer, parameter :: PAR_INDEPENDENT = 1, PAR_DERIVED = 2
  integer, parameter :: PAR_EXTERNAL = 3


  type :: parameter_t
     private
     integer :: type  = PAR_NONE
     class(modelpar_data_t), pointer :: data => null ()
     type(parse_node_t), pointer :: pn => null ()
     class(expr_t), allocatable :: expr
   contains
     procedure :: init_independent_value => parameter_init_independent_value
     procedure :: init_independent => parameter_init_independent
     procedure :: init_derived => parameter_init_derived
     procedure :: init_external => parameter_init_external
     procedure :: final => parameter_final
     procedure :: reset_derived => parameter_reset_derived
     procedure :: write => parameter_write
     procedure :: show => parameter_show
  end type parameter_t

  type, extends (model_data_t) :: model_t
     private
     character(32) :: md5sum = ""
     type(parameter_t), dimension(:), allocatable :: par
     integer :: max_par_name_length = 0
     integer :: max_field_name_length = 0
     type(var_list_t) :: var_list
     type(string_t) :: dlname
     procedure(model_init_external_parameters), nopass, pointer :: &
          init_external_parameters => null ()
     type(dlaccess_t) :: dlaccess
     type(parse_tree_t) :: parse_tree
   contains
     generic :: init => model_init
     procedure, private :: model_init
     procedure, private :: basic_init => model_basic_init
     procedure :: final => model_final
     procedure :: write => model_write
     procedure :: show => model_show
     procedure :: show_fields => model_show_fields
     procedure :: show_stable => model_show_stable
     procedure :: show_unstable => model_show_unstable
     procedure :: show_polarized => model_show_polarized
     procedure :: show_unpolarized => model_show_unpolarized
     procedure :: get_md5sum => model_get_md5sum
     procedure, private :: &
          set_parameter_constant => model_set_parameter_constant
     procedure, private :: &
          set_parameter_parse_node => model_set_parameter_parse_node
     procedure, private :: &
          set_parameter_external => model_set_parameter_external
     procedure, private :: copy_parameter => model_copy_parameter
     procedure :: update_parameters => model_parameters_update
     procedure, private :: init_field => model_init_field
     procedure, private :: copy_field => model_copy_field
     procedure :: write_var_list => model_write_var_list
     procedure :: link_var_list => model_link_var_list
     procedure :: var_exists => model_var_exists
     procedure :: var_is_locked => model_var_is_locked
     procedure :: set_real => model_var_set_real
     procedure :: get_rval => model_var_get_rval
     procedure :: get_var_list_ptr => model_get_var_list_ptr
     procedure :: read => model_read
     procedure, private :: read_parameter => model_read_parameter
     procedure, private :: read_derived => model_read_derived
     procedure, private :: read_external => model_read_external
     procedure, private :: read_field => model_read_field
     procedure, private :: read_vertex => model_read_vertex
     procedure, private :: append_field_vars => model_append_field_vars
     procedure :: init_instance => model_copy
  end type model_t

  type, extends (model_t) :: model_entry_t
     type(model_entry_t), pointer :: next => null ()
  end type model_entry_t

  type :: model_list_t
     type(model_entry_t), pointer :: first => null ()
     type(model_entry_t), pointer :: last => null ()
     type(model_list_t), pointer :: context => null ()
   contains
     procedure :: write => model_list_write
     procedure :: link => model_list_link
     procedure, private :: import => model_list_import
     procedure, private :: add => model_list_add
     procedure :: read_model => model_list_read_model
     procedure :: append_copy => model_list_append_copy
     procedure :: model_exists => model_list_model_exists
     procedure :: get_model_ptr => model_list_get_model_ptr
     procedure :: final => model_list_final
  end type model_list_t


  abstract interface
     subroutine model_init_external_parameters (par) bind (C)
       import
       real(c_default_float), dimension(*), intent(inout) :: par
     end subroutine model_init_external_parameters
  end interface


  type(syntax_t), target, save :: syntax_model_file


contains

  subroutine parameter_init_independent_value (par, par_data, name, value)
    class(parameter_t), intent(out) :: par
    class(modelpar_data_t), intent(in), target :: par_data
    type(string_t), intent(in) :: name
    real(default), intent(in) :: value
    par%type = PAR_INDEPENDENT
    par%data => par_data
    call par%data%init (name, value)
  end subroutine parameter_init_independent_value

  subroutine parameter_init_independent (par, par_data, name, pn)
    class(parameter_t), intent(out) :: par
    class(modelpar_data_t), intent(in), target :: par_data
    type(string_t), intent(in) :: name
    type(parse_node_t), intent(in), target :: pn
    par%type = PAR_INDEPENDENT
    par%pn => pn
    allocate (eval_tree_t :: par%expr)
    select type (expr => par%expr)
    type is (eval_tree_t)
       call expr%init_numeric_value (pn)
    end select
    par%data => par_data
    call par%data%init (name, par%expr%get_real ())
  end subroutine parameter_init_independent

  subroutine parameter_init_derived (par, par_data, name, pn, var_list)
    class(parameter_t), intent(out) :: par
    class(modelpar_data_t), intent(in), target :: par_data
    type(string_t), intent(in) :: name
    type(parse_node_t), intent(in), target :: pn
    type(var_list_t), intent(in), target :: var_list
    par%type = PAR_DERIVED
    par%pn => pn
    allocate (eval_tree_t :: par%expr)
    select type (expr => par%expr)
    type is (eval_tree_t)
       call expr%init_expr (pn, var_list=var_list)
    end select
    par%data => par_data
    call par%expr%evaluate ()
    call par%data%init (name, par%expr%get_real ())
  end subroutine parameter_init_derived

  subroutine parameter_init_external (par, par_data, name)
    class(parameter_t), intent(out) :: par
    class(modelpar_data_t), intent(in), target :: par_data
    type(string_t), intent(in) :: name
    par%type = PAR_EXTERNAL
    par%data => par_data
    call par%data%init (name, 0._default)
  end subroutine parameter_init_external

  subroutine parameter_final (par)
    class(parameter_t), intent(inout) :: par
    if (allocated (par%expr)) then
       call par%expr%final ()
    end if
  end subroutine parameter_final

  subroutine parameter_reset_derived (par)
    class(parameter_t), intent(inout) :: par
    select case (par%type)
    case (PAR_DERIVED)
       call par%expr%evaluate ()
       par%data = par%expr%get_real ()
    end select
  end subroutine parameter_reset_derived

  subroutine parameter_write (par, unit, write_defs)
    class(parameter_t), intent(in) :: par
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: write_defs
    logical :: defs
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    defs = .false.;  if (present (write_defs))  defs = write_defs
    select case (par%type)
    case (PAR_INDEPENDENT)
       write (u, "(3x,A)", advance="no")  "parameter"
    case (PAR_DERIVED)
       write (u, "(3x,A)", advance="no")  "derived"
    case (PAR_EXTERNAL)
       write (u, "(3x,A)", advance="no")  "external"
    end select
    call par%data%write (u)
    select case (par%type)
    case (PAR_DERIVED)
       if (defs) then
          call par%expr%write (unit)
       else
          write (u, *)
       end if
    case default
       write (u, *)
    end select
  end subroutine parameter_write

  subroutine parameter_show (par, l, u, partype)
    class(parameter_t), intent(in) :: par
    integer, intent(in) :: l, u
    integer, intent(in) :: partype
    if (par%type == partype) then
       call par%data%show (l, u)
    end if
  end subroutine parameter_show
    
  subroutine model_init &
       (model, name, libname, os_data, n_par, n_prt, n_vtx)
    class(model_t), intent(inout) :: model
    type(string_t), intent(in) :: name, libname
    type(os_data_t), intent(in) :: os_data
    integer, intent(in) :: n_par, n_prt, n_vtx
    type(c_funptr) :: c_fptr
    type(string_t) :: libpath
    call model%basic_init (name, n_par, n_prt, n_vtx)
    if (libname /= "") then
       if (.not. os_data%use_testfiles) then
          libpath = os_data%whizard_models_libpath_local
          model%dlname = os_get_dlname ( &
            libpath // "/" // libname, os_data, ignore=.true.)
       end if
       if (model%dlname == "") then
          libpath = os_data%whizard_models_libpath
          model%dlname = os_get_dlname (libpath // "/" // libname, os_data)
       end if
    else
       model%dlname = ""
    end if
    if (model%dlname /= "") then
       if (.not. dlaccess_is_open (model%dlaccess)) then
          if (logging) &
               call msg_message ("Loading model auxiliary library '" &
               // char (libpath) // "/" // char (model%dlname) // "'")
          call dlaccess_init (model%dlaccess, os_data%whizard_models_libpath, &
               model%dlname, os_data)
          if (dlaccess_has_error (model%dlaccess)) then
             call msg_message (char (dlaccess_get_error (model%dlaccess)))
             call msg_fatal ("Loading model auxiliary library '" &
                  // char (model%dlname) // "' failed")
             return
          end if
          c_fptr = dlaccess_get_c_funptr (model%dlaccess, &
               var_str ("init_external_parameters"))
          if (dlaccess_has_error (model%dlaccess)) then
             call msg_message (char (dlaccess_get_error (model%dlaccess)))
             call msg_fatal ("Loading function from auxiliary library '" &
                  // char (model%dlname) // "' failed")
             return
          end if
          call c_f_procpointer (c_fptr, model% init_external_parameters)
       end if
    end if
  end subroutine model_init

  subroutine model_basic_init (model, name, n_par, n_prt, n_vtx)
    class(model_t), intent(inout) :: model
    type(string_t), intent(in) :: name
    integer, intent(in) :: n_par, n_prt, n_vtx
    allocate (model%par (n_par))
    call model%model_data_t%init (name, n_par, 0, n_prt, n_vtx)
  end subroutine model_basic_init
    
  subroutine model_final (model)
    class(model_t), intent(inout) :: model
    integer :: i
    if (allocated (model%par)) then
       do i = 1, size (model%par)
          call model%par(i)%final ()
       end do
    end if
    call model%var_list%final (follow_link=.false.)
    if (model%dlname /= "")  call dlaccess_final (model%dlaccess)
    call parse_tree_final (model%parse_tree)
    call model%model_data_t%final ()
  end subroutine model_final

  subroutine model_write (model, unit, verbose, &
       show_md5sum, show_variables, show_parameters, &
       show_particles, show_vertices)
    class(model_t), intent(in) :: model
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose
    logical, intent(in), optional :: show_md5sum
    logical, intent(in), optional :: show_variables
    logical, intent(in), optional :: show_parameters
    logical, intent(in), optional :: show_particles
    logical, intent(in), optional :: show_vertices
    logical :: verb, show_md5, show_par, show_var
    integer :: u, i
    u = given_output_unit (unit);  if (u < 0)  return
    verb = .false.;  if (present (verbose))  verb = verbose
    show_md5 = .true.;  if (present (show_md5sum)) &
         show_md5 = show_md5sum
    show_par = .true.;  if (present (show_parameters)) &
         show_par = show_parameters
    show_var = verb;  if (present (show_variables)) &
         show_var = show_variables
    write (u, "(A,A,A)") 'model "', char (model%get_name ()), '"'
    if (show_md5 .and. model%md5sum /= "") &
         write (u, "(1x,A,A,A)") "! md5sum = '", model%md5sum, "'"
    if (show_par) then
       write (u, *)
       do i = 1, size (model%par)
          call model%par(i)%write (u, write_defs=verbose)
       end do
    end if
    call model%model_data_t%write (unit, verbose, &
         show_md5sum, show_variables, &
         show_parameters=.false., &
         show_particles=show_particles, &
         show_vertices=show_vertices)
    if (show_var) then
       write (u, *)
       call var_list_write (model%var_list, unit, follow_link=.false.)
    end if
  end subroutine model_write

  subroutine model_show (model, unit)
    class(model_t), intent(in) :: model
    integer, intent(in), optional :: unit
    integer :: i, u, l
    u = given_output_unit (unit)
    write (u, "(A,1x,A)")  "Model:", char (model%get_name ())
    l = model%max_field_name_length
    call model%show_fields (l, u)
    l = model%max_par_name_length
    if (any (model%par%type == PAR_INDEPENDENT)) then
       write (u, "(2x,A)")  "Independent parameters:"
       do i = 1, size (model%par)
          call model%par(i)%show (l, u, PAR_INDEPENDENT)
       end do
    end if
    if (any (model%par%type == PAR_DERIVED)) then
       write (u, "(2x,A)")  "Derived parameters:"
       do i = 1, size (model%par)
          call model%par(i)%show (l, u, PAR_DERIVED)
       end do
    end if
    if (any (model%par%type == PAR_EXTERNAL)) then
       write (u, "(2x,A)")  "External parameters:"
       do i = 1, size (model%par)
          call model%par(i)%show (l, u, PAR_EXTERNAL)
       end do
    end if
  end subroutine model_show

  subroutine model_show_fields (model, l, unit)
    class(model_t), intent(in), target :: model
    integer, intent(in) :: l
    integer, intent(in), optional :: unit
    type(field_data_t), pointer :: field
    integer :: u, i
    u = given_output_unit (unit)
    write (u, "(2x,A)")  "Particles:"
    do i = 1, model%get_n_field ()
       field => model%get_field_ptr_by_index (i)
       call field%show (l, u)
    end do
  end subroutine model_show_fields
  
  subroutine model_show_stable (model, unit)
    class(model_t), intent(in), target :: model
    integer, intent(in), optional :: unit
    type(field_data_t), pointer :: field
    integer :: u, i
    u = given_output_unit (unit)
    write (u, "(A,1x)", advance="no")  "Stable particles:"
    do i = 1, model%get_n_field ()
       field => model%get_field_ptr_by_index (i)
       if (field%is_stable (.false.)) then
          write (u, "(1x,A)", advance="no")  char (field%get_name (.false.))
       end if
       if (field%has_antiparticle ()) then
          if (field%is_stable (.true.)) then
             write (u, "(1x,A)", advance="no")  char (field%get_name (.true.))
          end if
       end if
    end do
    write (u, *)
  end subroutine model_show_stable
  
  subroutine model_show_unstable (model, unit)
    class(model_t), intent(in), target :: model
    integer, intent(in), optional :: unit
    type(field_data_t), pointer :: field
    integer :: u, i
    u = given_output_unit (unit)
    write (u, "(A,1x)", advance="no")  "Unstable particles:"
    do i = 1, model%get_n_field ()
       field => model%get_field_ptr_by_index (i)
       if (.not. field%is_stable (.false.)) then
          write (u, "(1x,A)", advance="no")  char (field%get_name (.false.))
       end if
       if (field%has_antiparticle ()) then
          if (.not. field%is_stable (.true.)) then
             write (u, "(1x,A)", advance="no")  char (field%get_name (.true.))
          end if
       end if
    end do
    write (u, *)
  end subroutine model_show_unstable
  
  subroutine model_show_polarized (model, unit)
    class(model_t), intent(in), target :: model
    integer, intent(in), optional :: unit
    type(field_data_t), pointer :: field
    integer :: u, i
    u = given_output_unit (unit)
    write (u, "(A,1x)", advance="no")  "Polarized particles:"
    do i = 1, model%get_n_field ()
       field => model%get_field_ptr_by_index (i)
       if (field%is_polarized (.false.)) then
          write (u, "(1x,A)", advance="no") char (field%get_name (.false.))
       end if
       if (field%has_antiparticle ()) then
          if (field%is_polarized (.true.)) then
             write (u, "(1x,A)", advance="no") char (field%get_name (.true.))
          end if
       end if
    end do
    write (u, *)
  end subroutine model_show_polarized
  
  subroutine model_show_unpolarized (model, unit)
    class(model_t), intent(in), target :: model
    integer, intent(in), optional :: unit
    type(field_data_t), pointer :: field
    integer :: u, i
    u = given_output_unit (unit)
    write (u, "(A,1x)", advance="no")  "Unpolarized particles:"
    do i = 1, model%get_n_field ()
       field => model%get_field_ptr_by_index (i)
       if (.not. field%is_polarized (.false.)) then
          write (u, "(1x,A)", advance="no") &
               char (field%get_name (.false.))
       end if
       if (field%has_antiparticle ()) then
          if (.not. field%is_polarized (.true.)) then
             write (u, "(1x,A)", advance="no") char (field%get_name (.true.))
          end if
       end if
    end do
    write (u, *)
  end subroutine model_show_unpolarized
  
  function model_get_md5sum (model) result (md5sum)
    character(32) :: md5sum
    class(model_t), intent(in) :: model
    md5sum = model%md5sum
  end function model_get_md5sum

  subroutine model_set_parameter_constant (model, i, name, value)
    class(model_t), intent(inout), target :: model
    integer, intent(in) :: i
    type(string_t), intent(in) :: name
    real(default), intent(in) :: value
    logical, save, target :: known = .true.
    class(modelpar_data_t), pointer :: par_data
    real(default), pointer :: value_ptr
    par_data => model%get_par_real_ptr (i)
    call model%par(i)%init_independent_value (par_data, name, value)
    value_ptr => par_data%get_real_ptr ()
    call var_list_append_real_ptr (model%var_list, &
         name, value_ptr, &
         is_known=known, intrinsic=.true.)
    model%max_par_name_length = max (model%max_par_name_length, len (name))
  end subroutine model_set_parameter_constant

  subroutine model_set_parameter_parse_node (model, i, name, pn, constant)
    class(model_t), intent(inout), target :: model
    integer, intent(in) :: i
    type(string_t), intent(in) :: name
    type(parse_node_t), intent(in), target :: pn
    logical, intent(in) :: constant
    logical, save, target :: known = .true.
    class(modelpar_data_t), pointer :: par_data
    real(default), pointer :: value_ptr
    par_data => model%get_par_real_ptr (i)
    if (constant) then
       call model%par(i)%init_independent (par_data, name, pn)
    else
       call model%par(i)%init_derived (par_data, name, pn, model%var_list)
    end if
    value_ptr => par_data%get_real_ptr ()
    call var_list_append_real_ptr (model%var_list, &
         name, value_ptr, &
         is_known=known, locked=.not.constant, intrinsic=.true.)
    model%max_par_name_length = max (model%max_par_name_length, len (name))
  end subroutine model_set_parameter_parse_node

  subroutine model_set_parameter_external (model, i, name)
    class(model_t), intent(inout), target :: model
    integer, intent(in) :: i
    type(string_t), intent(in) :: name
    logical, save, target :: known = .true.
    class(modelpar_data_t), pointer :: par_data
    real(default), pointer :: value_ptr
    par_data => model%get_par_real_ptr (i)
    call model%par(i)%init_external (par_data, name)
    value_ptr => par_data%get_real_ptr ()
    call var_list_append_real_ptr (model%var_list, &
         name, value_ptr, &
         is_known=known, locked=.true., intrinsic=.true.)
    model%max_par_name_length = max (model%max_par_name_length, len (name))
  end subroutine model_set_parameter_external

  subroutine model_copy_parameter (model, i, par)
    class(model_t), intent(inout), target :: model
    integer, intent(in) :: i
    type(parameter_t), intent(in) :: par
    type(string_t) :: name
    real(default) :: value
    name = par%data%get_name ()
    select case (par%type)
    case (PAR_INDEPENDENT)
       if (associated (par%pn)) then
          call model%set_parameter_parse_node (i, name, par%pn, &
               constant = .true.)
       else
          value = par%data%get_real ()
          call model%set_parameter_constant (i, name, value)
       end if
    case (PAR_DERIVED)
       call model%set_parameter_parse_node (i, name, par%pn, &
            constant = .false.)
    case (PAR_EXTERNAL)
       call model%set_parameter_external (i, name)
    end select
  end subroutine model_copy_parameter
  
  subroutine model_parameters_update (model)
    class(model_t), intent(inout) :: model
    integer :: i
    real(default), dimension(:), allocatable :: par
    do i = 1, size (model%par)
       call model%par(i)%reset_derived ()
    end do
    if (associated (model%init_external_parameters)) then
       allocate (par (model%get_n_real ()))
       call model%real_parameters_to_c_array (par)
       call model%init_external_parameters (par)
       call model%real_parameters_from_c_array (par)
    end if
  end subroutine model_parameters_update

  subroutine model_init_field (model, i, longname, pdg)
    class(model_t), intent(inout), target :: model
    integer, intent(in) :: i
    type(string_t), intent(in) :: longname
    integer, intent(in) :: pdg
    type(field_data_t), pointer :: field
    field => model%get_field_ptr_by_index (i)
    call field%init (longname, pdg)
  end subroutine model_init_field
    
  subroutine model_copy_field (model, i, name_src)
    class(model_t), intent(inout), target :: model
    integer, intent(in) :: i
    type(string_t), intent(in) :: name_src
    type(field_data_t), pointer :: field_src, field
    field_src => model%get_field_ptr (name_src)
    field => model%get_field_ptr_by_index (i)
    call field%copy_from (field_src)
  end subroutine model_copy_field

  subroutine model_write_var_list (model, unit, follow_link)
    class(model_t), intent(in) :: model
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: follow_link
    call var_list_write (model%var_list, unit, follow_link)
  end subroutine model_write_var_list
  
  subroutine model_link_var_list (model, var_list)
    class(model_t), intent(inout) :: model
    type(var_list_t), intent(in), target :: var_list
    call model%var_list%link (var_list)
  end subroutine model_link_var_list

  function model_var_exists (model, name) result (flag)
    class(model_t), intent(in) :: model
    type(string_t), intent(in) :: name
    logical :: flag
    flag = model%var_list%contains (name, follow_link=.false.)
  end function model_var_exists
  
  function model_var_is_locked (model, name) result (flag)
    class(model_t), intent(in) :: model
    type(string_t), intent(in) :: name
    logical :: flag
    flag = model%var_list%is_locked (name, follow_link=.false.)
  end function model_var_is_locked
  
  subroutine model_var_set_real (model, name, rval, verbose, pacified)
    class(model_t), intent(inout) :: model
    type(string_t), intent(in) :: name
    real(default), intent(in) :: rval
    logical, intent(in), optional :: verbose, pacified
    call var_list_set_real (model%var_list, name, rval, &
         is_known=.true., ignore=.false., &
         verbose=verbose, model_name=model%get_name (), pacified=pacified)
    call model%update_parameters ()
  end subroutine model_var_set_real
    
  function model_var_get_rval (model, name) result (rval)
    class(model_t), intent(in) :: model
    type(string_t), intent(in) :: name
    real(default) :: rval
    rval = model%var_list%get_rval (name, follow_link=.false.)
  end function model_var_get_rval
  
  function model_get_var_list_ptr (model) result (var_list)
    type(var_list_t), pointer :: var_list
    class(model_t), intent(in), target :: model
    var_list => model%var_list
  end function model_get_var_list_ptr

  subroutine define_model_file_syntax (ifile)
    type(ifile_t), intent(inout) :: ifile
    call ifile_append (ifile, "SEQ model_def = model_name_def " // &
         "parameters derived_pars external_pars particles vertices")
    call ifile_append (ifile, "SEQ model_name_def = model model_name")
    call ifile_append (ifile, "KEY model")
    call ifile_append (ifile, "QUO model_name = '""'...'""'")
    call ifile_append (ifile, "SEQ parameters = parameter_def*")
    call ifile_append (ifile, "SEQ parameter_def = parameter par_name " // &
         "'=' any_real_value")
    call ifile_append (ifile, "ALT any_real_value = " &
         // "neg_real_value | pos_real_value | real_value")
    call ifile_append (ifile, "SEQ neg_real_value = '-' real_value")
    call ifile_append (ifile, "SEQ pos_real_value = '+' real_value")
    call ifile_append (ifile, "KEY parameter")
    call ifile_append (ifile, "IDE par_name")
    ! call ifile_append (ifile, "KEY '='")          !!! Key already exists
    call ifile_append (ifile, "SEQ derived_pars = derived_def*")
    call ifile_append (ifile, "SEQ derived_def = derived par_name " // &
         "'=' expr")
    call ifile_append (ifile, "KEY derived")
    call ifile_append (ifile, "SEQ external_pars = external_def*")
    call ifile_append (ifile, "SEQ external_def = external par_name")
    call ifile_append (ifile, "KEY external")
    call ifile_append (ifile, "SEQ particles = particle_def*")
    call ifile_append (ifile, "SEQ particle_def = particle prt_longname " // &
         "prt_pdg prt_details")
    call ifile_append (ifile, "KEY particle")
    call ifile_append (ifile, "IDE prt_longname")
    call ifile_append (ifile, "INT prt_pdg")
    call ifile_append (ifile, "ALT prt_details = prt_src | prt_properties")
    call ifile_append (ifile, "SEQ prt_src = like prt_longname prt_properties")
    call ifile_append (ifile, "KEY like")
    call ifile_append (ifile, "SEQ prt_properties = prt_property*")
    call ifile_append (ifile, "ALT prt_property = " // & 
         "parton | invisible | gauge | left | right | " // &
         "prt_name | prt_anti | prt_tex_name | prt_tex_anti | " // &
         "prt_spin | prt_isospin | prt_charge | " // &
         "prt_color | prt_mass | prt_width")
    call ifile_append (ifile, "KEY parton")
    call ifile_append (ifile, "KEY invisible")
    call ifile_append (ifile, "KEY gauge")
    call ifile_append (ifile, "KEY left")
    call ifile_append (ifile, "KEY right")
    call ifile_append (ifile, "SEQ prt_name = name name_def+")
    call ifile_append (ifile, "SEQ prt_anti = anti name_def+")
    call ifile_append (ifile, "SEQ prt_tex_name = tex_name name_def")
    call ifile_append (ifile, "SEQ prt_tex_anti = tex_anti name_def")
    call ifile_append (ifile, "KEY name")
    call ifile_append (ifile, "KEY anti")
    call ifile_append (ifile, "KEY tex_name")
    call ifile_append (ifile, "KEY tex_anti")
    call ifile_append (ifile, "ALT name_def = name_string | name_id")
    call ifile_append (ifile, "QUO name_string = '""'...'""'")
    call ifile_append (ifile, "IDE name_id")
    call ifile_append (ifile, "SEQ prt_spin = spin frac")
    call ifile_append (ifile, "KEY spin")
    call ifile_append (ifile, "SEQ prt_isospin = isospin frac")
    call ifile_append (ifile, "KEY isospin")
    call ifile_append (ifile, "SEQ prt_charge = charge frac")
    call ifile_append (ifile, "KEY charge")
    call ifile_append (ifile, "SEQ prt_color = color integer_literal")
    call ifile_append (ifile, "KEY color")
    call ifile_append (ifile, "SEQ prt_mass = mass par_name")
    call ifile_append (ifile, "KEY mass")
    call ifile_append (ifile, "SEQ prt_width = width par_name")
    call ifile_append (ifile, "KEY width")
    call ifile_append (ifile, "SEQ vertices = vertex_def*")
    call ifile_append (ifile, "SEQ vertex_def = vertex name_def+")
    call ifile_append (ifile, "KEY vertex")
    call define_expr_syntax (ifile, particles=.false., analysis=.false.)
  end subroutine define_model_file_syntax

  subroutine syntax_model_file_init ()
    type(ifile_t) :: ifile
    call define_model_file_syntax (ifile)
    call syntax_init (syntax_model_file, ifile)
    call ifile_final (ifile)
  end subroutine syntax_model_file_init

  subroutine lexer_init_model_file (lexer)
    type(lexer_t), intent(out) :: lexer
    call lexer_init (lexer, &
         comment_chars = "#!", &
         quote_chars = '"{', &
         quote_match = '"}', &
         single_chars = ":()", &
         special_class = [ "+-*/^", "<>=  " ] , &
         keyword_list = syntax_get_keyword_list_ptr (syntax_model_file))
  end subroutine lexer_init_model_file

  subroutine syntax_model_file_final ()
    call syntax_final (syntax_model_file)
  end subroutine syntax_model_file_final

  subroutine syntax_model_file_write (unit)
    integer, intent(in), optional :: unit
    call syntax_write (syntax_model_file, unit)
  end subroutine syntax_model_file_write

  subroutine model_read (model, filename, os_data, exist)
    class(model_t), intent(out), target :: model
    type(string_t), intent(in) :: filename
    type(os_data_t), intent(in) :: os_data
    logical, intent(out), optional :: exist
    type(string_t) :: file
    type(stream_t), target :: stream
    type(lexer_t) :: lexer
    integer :: unit
    character(32) :: model_md5sum
    type(parse_node_t), pointer :: nd_model_def, nd_model_name_def
    type(parse_node_t), pointer :: nd_parameters, nd_derived_pars
    type(parse_node_t), pointer :: nd_external_pars
    type(parse_node_t), pointer :: nd_particles, nd_vertices
    type(string_t) :: model_name, lib_name
    integer :: n_par, n_der, n_ext, n_prt, n_vtx
    real(c_default_float), dimension(:), allocatable :: par
    integer :: i
    type(parse_node_t), pointer :: nd_par_def
    type(parse_node_t), pointer :: nd_der_def
    type(parse_node_t), pointer :: nd_ext_def
    type(parse_node_t), pointer :: nd_prt
    type(parse_node_t), pointer :: nd_vtx
    logical :: model_exist
    file = filename
    inquire (file=char(file), exist=model_exist)
    if ((.not. model_exist) .and. (.not. os_data%use_testfiles)) then
       file = os_data%whizard_modelpath_local // "/" // filename
       inquire (file = char (file), exist = model_exist)
    end if
    if (.not. model_exist) then
       file = os_data%whizard_modelpath // "/" // filename
       inquire (file = char (file), exist = model_exist)
    end if
    if (.not. model_exist) then
       call msg_fatal ("Model file '" // char (filename) // "' not found")
       if (present (exist))  exist = .false.
       return
    end if
    if (present (exist))  exist = .true.
    if (logging) call msg_message ("Reading model file '" // char (file) // "'")
    call lexer_init_model_file (lexer)
    unit = free_unit ()
    open (file=char(file), unit=unit, action="read", status="old")
    model_md5sum = md5sum (unit)
    close (unit)
    call stream_init (stream, char (file))
    call lexer_assign_stream (lexer, stream)
    call parse_tree_init (model%parse_tree, syntax_model_file, lexer)
    call stream_final (stream)
    call lexer_final (lexer)
    nd_model_def => parse_tree_get_root_ptr (model%parse_tree)
    nd_model_name_def => parse_node_get_sub_ptr (nd_model_def)
    model_name = parse_node_get_string &
         (parse_node_get_sub_ptr (nd_model_name_def, 2))
    nd_parameters => parse_node_get_next_ptr (nd_model_name_def)
    if (associated (nd_parameters)) then
       if (parse_node_get_rule_key (nd_parameters) == "parameters") then
          n_par = parse_node_get_n_sub (nd_parameters)
          nd_par_def => parse_node_get_sub_ptr (nd_parameters)
          nd_derived_pars => parse_node_get_next_ptr (nd_parameters)
       else
          n_par = 0
          nd_derived_pars => nd_parameters
          nd_parameters => null ()
       end if
    else
       n_par = 0
       nd_derived_pars => null ()
    end if
    if (associated (nd_derived_pars)) then
       if (parse_node_get_rule_key (nd_derived_pars) == "derived_pars") then
          n_der = parse_node_get_n_sub (nd_derived_pars)
          nd_der_def => parse_node_get_sub_ptr (nd_derived_pars)
          nd_external_pars => parse_node_get_next_ptr (nd_derived_pars)
       else
          n_der = 0
          nd_external_pars => nd_derived_pars
          nd_derived_pars => null ()
       end if
    else
       n_der = 0
       nd_external_pars => null ()
    end if
    if (associated (nd_external_pars)) then
       if (parse_node_get_rule_key (nd_external_pars) == "external_pars") then
          n_ext = parse_node_get_n_sub (nd_external_pars)
          lib_name = "external." // model_name
          nd_ext_def => parse_node_get_sub_ptr (nd_external_pars)
          nd_particles => parse_node_get_next_ptr (nd_external_pars)
       else
          n_ext = 0
          lib_name = ""
          nd_particles => nd_external_pars
          nd_external_pars => null ()
       end if
    else
       n_ext = 0
       lib_name = ""
       nd_particles => null ()
    end if
    if (associated (nd_particles)) then
       if (parse_node_get_rule_key (nd_particles) == "particles") then
          n_prt = parse_node_get_n_sub (nd_particles)
          nd_prt => parse_node_get_sub_ptr (nd_particles)
          nd_vertices => parse_node_get_next_ptr (nd_particles)
       else
          n_prt = 0
          nd_vertices => nd_particles
          nd_particles => null ()
       end if
    else
       n_prt = 0
       nd_vertices => null ()
    end if
    if (associated (nd_vertices)) then
       n_vtx = parse_node_get_n_sub (nd_vertices)
       nd_vtx => parse_node_get_sub_ptr (nd_vertices)
    else
       n_vtx = 0
    end if
    call model%init (model_name, lib_name, os_data, &
         n_par + n_der + n_ext, n_prt, n_vtx)
    model%md5sum = model_md5sum
    do i = 1, n_par
       call model%read_parameter (i, nd_par_def)
       nd_par_def => parse_node_get_next_ptr (nd_par_def)
    end do
    do i = n_par + 1, n_par + n_der
       call model%read_derived (i, nd_der_def)
       nd_der_def => parse_node_get_next_ptr (nd_der_def)
    end do
    do i = n_par + n_der + 1, n_par + n_der + n_ext
       call model%read_external (i, nd_ext_def)
       nd_ext_def => parse_node_get_next_ptr (nd_ext_def)
    end do
    if (associated (model% init_external_parameters)) then
       allocate (par (model%get_n_real ()))
       call model%real_parameters_to_c_array (par)
       call model%init_external_parameters (par)
       call model%real_parameters_from_c_array (par)
    end if
    do i = 1, n_prt
       call model%read_field (i, nd_prt)
       nd_prt => parse_node_get_next_ptr (nd_prt)
    end do
    do i = 1, n_vtx
       call model%read_vertex (i, nd_vtx)
       nd_vtx => parse_node_get_next_ptr (nd_vtx)
    end do
    call model%freeze_vertices ()
    call model%append_field_vars ()
  end subroutine model_read

  subroutine model_read_parameter (model, i, node)
    class(model_t), intent(inout), target :: model
    integer, intent(in) :: i
    type(parse_node_t), intent(in), target :: node
    type(parse_node_t), pointer :: node_name, node_val
    type(string_t) :: name
    node_name => parse_node_get_sub_ptr (node, 2)
    name = parse_node_get_string (node_name)
    node_val => parse_node_get_next_ptr (node_name, 2)
    call model%set_parameter_parse_node (i, name, node_val, constant=.true.)
  end subroutine model_read_parameter

  subroutine model_read_derived (model, i, node)
    class(model_t), intent(inout), target :: model
    integer, intent(in) :: i
    type(parse_node_t), intent(in), target :: node
    type(string_t) :: name
    type(parse_node_t), pointer :: pn_expr
    name = parse_node_get_string (parse_node_get_sub_ptr (node, 2))
    pn_expr => parse_node_get_sub_ptr (node, 4)
    call model%set_parameter_parse_node (i, name, pn_expr, constant=.false.)
  end subroutine model_read_derived

  subroutine model_read_external (model, i, node)
    class(model_t), intent(inout), target :: model
    integer, intent(in) :: i
    type(parse_node_t), intent(in), target :: node
    type(string_t) :: name
    name = parse_node_get_string (parse_node_get_sub_ptr (node, 2))
    call model%set_parameter_external (i, name)
  end subroutine model_read_external

  subroutine model_read_field (model, i, node)
    class(model_t), intent(inout), target :: model
    integer, intent(in) :: i
    type(parse_node_t), intent(in) :: node
    type(parse_node_t), pointer :: nd_src, nd_props, nd_prop
    type(string_t) :: longname
    integer :: pdg
    type(string_t) :: name_src
    type(string_t), dimension(:), allocatable :: name
    type(field_data_t), pointer :: field, field_src
    longname = parse_node_get_string (parse_node_get_sub_ptr (node, 2))
    pdg = parse_node_get_integer (parse_node_get_sub_ptr (node, 3)) 
    field => model%get_field_ptr_by_index (i)
    call field%init (longname, pdg)
    nd_src => parse_node_get_sub_ptr (node, 4)
    if (associated (nd_src)) then
       if (parse_node_get_rule_key (nd_src) == "prt_src") then
          name_src = parse_node_get_string (parse_node_get_sub_ptr (nd_src, 2))
          field_src => model%get_field_ptr (name_src, check=.true.)
          call field%copy_from (field_src)
          nd_props => parse_node_get_sub_ptr (nd_src, 3)
       else
          nd_props => nd_src
       end if
       nd_prop => parse_node_get_sub_ptr (nd_props)
       do while (associated (nd_prop))
          select case (char (parse_node_get_rule_key (nd_prop)))
          case ("invisible")
             call field%set (is_visible=.false.)
          case ("parton")
             call field%set (is_parton=.true.)
          case ("gauge")
             call field%set (is_gauge=.true.)
          case ("left")
             call field%set (is_left_handed=.true.)
          case ("right")
             call field%set (is_right_handed=.true.)
          case ("prt_name")
             call read_names (nd_prop, name)
             call field%set (name=name)
          case ("prt_anti")
             call read_names (nd_prop, name)
             call field%set (anti=name)
          case ("prt_tex_name")
             call field%set ( &
                  tex_name = parse_node_get_string &
                  (parse_node_get_sub_ptr (nd_prop, 2)))
          case ("prt_tex_anti")
             call field%set ( &
                  tex_anti = parse_node_get_string &
                  (parse_node_get_sub_ptr (nd_prop, 2)))
          case ("prt_spin")
             call field%set ( &
                  spin_type = read_frac &
                  (parse_node_get_sub_ptr (nd_prop, 2), 2))
          case ("prt_isospin")
             call field%set ( &
                  isospin_type = read_frac &
                  (parse_node_get_sub_ptr (nd_prop, 2), 2))
          case ("prt_charge")
             call field%set ( &
                  charge_type = read_frac &
                  (parse_node_get_sub_ptr (nd_prop, 2), 3))
          case ("prt_color")
             call field%set ( &
                  color_type = parse_node_get_integer &
                  (parse_node_get_sub_ptr (nd_prop, 2)))
          case ("prt_mass")
             call field%set ( &
                  mass_data = model%get_par_data_ptr &
                  (parse_node_get_string &
                  (parse_node_get_sub_ptr (nd_prop, 2))))
          case ("prt_width")
             call field%set ( &
                  width_data = model%get_par_data_ptr &
                  (parse_node_get_string &
                  (parse_node_get_sub_ptr (nd_prop, 2))))
          case default
             call msg_bug (" Unknown particle property '" &
                  // char (parse_node_get_rule_key (nd_prop)) // "'")
          end select
          if (allocated (name))  deallocate (name)
          nd_prop => parse_node_get_next_ptr (nd_prop)
       end do
    end if
    call field%freeze ()
  end subroutine model_read_field

  subroutine model_read_vertex (model, i, node)
    class(model_t), intent(inout) :: model
    integer, intent(in) :: i
    type(parse_node_t), intent(in) :: node
    type(string_t), dimension(:), allocatable :: name
    call read_names (node, name)
    call model%set_vertex (i, name)
  end subroutine model_read_vertex

  subroutine read_names (node, name)
    type(parse_node_t), intent(in) :: node
    type(string_t), dimension(:), allocatable, intent(inout) :: name
    type(parse_node_t), pointer :: nd_name
    integer :: n_names, i
    n_names = parse_node_get_n_sub (node) - 1
    allocate (name (n_names))
    nd_name => parse_node_get_sub_ptr (node, 2)
    do i = 1, n_names
       name(i) = parse_node_get_string (nd_name)
       nd_name => parse_node_get_next_ptr (nd_name)
    end do
  end subroutine read_names

  function read_frac (nd_frac, base) result (qn_type)
    integer :: qn_type
    type(parse_node_t), intent(in) :: nd_frac
    integer, intent(in) :: base
    type(parse_node_t), pointer :: nd_num, nd_den
    integer :: num, den
    nd_num => parse_node_get_sub_ptr (nd_frac)
    nd_den => parse_node_get_next_ptr (nd_num)
    select case (char (parse_node_get_rule_key (nd_num)))
    case ("integer_literal")
       num = parse_node_get_integer (nd_num)
    case ("neg_int")
       num = - parse_node_get_integer (parse_node_get_sub_ptr (nd_num, 2))
    case ("pos_int")
       num = parse_node_get_integer (parse_node_get_sub_ptr (nd_num, 2))
    case default
       call parse_tree_bug (nd_num, "int|neg_int|pos_int")
    end select
    if (associated (nd_den)) then
       den = parse_node_get_integer (parse_node_get_sub_ptr (nd_den, 2))
    else
       den = 1
    end if
    if (den == 1) then
       qn_type = sign (1 + abs (num) * base, num)
    else if (den == base) then
       qn_type = sign (abs (num) + 1, num)
    else
       call parse_node_write_rec (nd_frac)
       call msg_fatal (" Fractional quantum number: wrong denominator")
    end if
  end function read_frac

  subroutine model_append_field_vars (model)
    class(model_t), intent(inout) :: model
    type(pdg_array_t) :: aval
    type(field_data_t), dimension(:), pointer :: field_array
    type(field_data_t), pointer :: field
    type(string_t) :: name
    type(string_t), dimension(:), allocatable :: name_array
    integer, dimension(:), allocatable :: pdg
    logical, dimension(:), allocatable :: mask
    integer :: i, j
    field_array => model%get_field_array_ptr ()
    aval = UNDEFINED
    call var_list_append_pdg_array &
         (model%var_list, var_str ("particle"), &
          aval, locked = .true., intrinsic=.true.)
    do i = 1, size (field_array)
       aval = field_array(i)%get_pdg ()
       name = field_array(i)%get_longname ()
       call var_list_append_pdg_array &
            (model%var_list, name, aval, locked=.true., intrinsic=.true.)
       call field_array(i)%get_name_array (.false., name_array)
       do j = 1, size (name_array)
          call var_list_append_pdg_array &
               (model%var_list, name_array(j), &
               aval, locked=.true., intrinsic=.true.)
       end do
       model%max_field_name_length = &
            max (model%max_field_name_length, len (name_array(1)))
       aval = - field_array(i)%get_pdg ()
       call field_array(i)%get_name_array (.true., name_array)
       do j = 1, size (name_array)
          call var_list_append_pdg_array &
               (model%var_list, name_array(j), &
               aval, locked=.true., intrinsic=.true.)
       end do
       if (size (name_array) > 0) then
          model%max_field_name_length = &
               max (model%max_field_name_length, len (name_array(1)))
       end if
    end do
    call model%get_all_pdg (pdg)
    allocate (mask (size (pdg)))
    do i = 1, size (pdg)
       field => model%get_field_ptr (pdg(i))
       mask(i) = field%get_charge_type () /= 1
    end do
    aval = pack (pdg, mask)
    call var_list_append_pdg_array &
         (model%var_list, var_str ("charged"), &
          aval, locked = .true., intrinsic=.true.)
    do i = 1, size (pdg)
       field => model%get_field_ptr (pdg(i))
       mask(i) = field%get_color_type () /= 1
    end do
    aval = pack (pdg, mask)
    call var_list_append_pdg_array &
         (model%var_list, var_str ("colored"), &
          aval, locked = .true., intrinsic=.true.)
  end subroutine model_append_field_vars
    
  recursive subroutine model_list_write (object, unit, verbose, follow_link)
    class(model_list_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose
    logical, intent(in), optional :: follow_link
    type(model_entry_t), pointer :: current
    logical :: rec
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    rec = .true.;  if (present (follow_link))  rec = follow_link
    if (rec .and. associated (object%context)) then
       call object%context%write (unit, verbose, follow_link)
    end if
    current => object%first
    if (associated (current)) then
       do while (associated (current))
          call current%write (unit, verbose)
          current => current%next
          if (associated (current))  write (u, *)
       end do
    end if
  end subroutine model_list_write

  subroutine model_list_link (model_list, context)
    class(model_list_t), intent(inout) :: model_list
    type(model_list_t), intent(in), target :: context
    model_list%context => context
  end subroutine model_list_link
  
  subroutine model_list_import (model_list, current, model)
    class(model_list_t), intent(inout) :: model_list
    type(model_entry_t), pointer, intent(inout) :: current
    type(model_t), optional, pointer, intent(out) :: model
    if (associated (current)) then
       if (associated (model_list%first)) then
          model_list%last%next => current
       else
          model_list%first => current
       end if
       model_list%last => current
       if (present (model))  model => current%model_t
       current => null ()
    end if
  end subroutine model_list_import
       
  subroutine model_list_add (model_list, &
       name, os_data, n_par, n_prt, n_vtx, model)
    class(model_list_t), intent(inout) :: model_list
    type(string_t), intent(in) :: name
    type(os_data_t), intent(in) :: os_data
    integer, intent(in) :: n_par, n_prt, n_vtx
    type(model_t), pointer :: model
    type(model_entry_t), pointer :: current
    if (model_list%model_exists (name, follow_link=.false.)) then
       model => null ()
    else
       allocate (current)
       call current%init (name, var_str (""), os_data, &
            n_par, n_prt, n_vtx)
       call model_list%import (current, model)
    end if
  end subroutine model_list_add

  subroutine model_list_read_model (model_list, name, filename, os_data, model)
    class(model_list_t), intent(inout), target :: model_list
    type(string_t), intent(in) :: name, filename
    type(os_data_t), intent(in) :: os_data
    type(model_t), pointer, intent(inout) :: model
    class(model_list_t), pointer :: global_model_list
    type(model_entry_t), pointer :: current
    logical :: exist
    if (.not. model_list%model_exists (name, follow_link=.true.)) then
       allocate (current)
       call current%read (filename, os_data, exist)
       if (.not. exist)  return
       if (current%get_name () /= name) then
          call msg_fatal ("Model file '" // char (filename) // &
               "' contains model '" // char (current%get_name ()) // &
               "' instead of '" // char (name) // "'")
          call current%final ();  deallocate (current)
          return
       end if
       global_model_list => model_list
       do while (associated (global_model_list%context))
          global_model_list => global_model_list%context
       end do
       call global_model_list%import (current, model)
    else
       model => model_list%get_model_ptr (name)
    end if
  end subroutine model_list_read_model

  subroutine model_list_append_copy (model_list, orig, model)
    class(model_list_t), intent(inout) :: model_list
    type(model_t), intent(in), target :: orig
    type(model_t), intent(out), pointer, optional :: model
    type(model_entry_t), pointer :: copy
    allocate (copy)
    call copy%init_instance (orig)
    call model_list%import (copy, model)
  end subroutine model_list_append_copy
    
  recursive function model_list_model_exists (model_list, name, follow_link) &
       result (exists)
    class(model_list_t), intent(in) :: model_list
    logical :: exists
    type(string_t), intent(in) :: name
    logical, intent(in), optional :: follow_link
    type(model_entry_t), pointer :: current
    logical :: rec
    rec = .true.;  if (present (follow_link))  rec = follow_link
    current => model_list%first
    do while (associated (current))
       if (current%get_name () == name) then
          exists = .true.
          return
       end if
       current => current%next
    end do
    if (rec .and. associated (model_list%context)) then
       exists = model_list%context%model_exists (name, follow_link)
    else
       exists = .false.
    end if
  end function model_list_model_exists

  recursive function model_list_get_model_ptr (model_list, name, follow_link) &
       result (model)
    class(model_list_t), intent(in) :: model_list
    type(model_t), pointer :: model
    type(string_t), intent(in) :: name
    logical, intent(in), optional :: follow_link
    type(model_entry_t), pointer :: current
    logical :: rec
    rec = .true.;  if (present (follow_link))  rec = follow_link
    current => model_list%first
    do while (associated (current))
       if (current%get_name () == name) then
          model => current%model_t
          return
       end if
       current => current%next
    end do
    if (rec .and. associated (model_list%context)) then
       model => model_list%context%get_model_ptr (name, follow_link)
    else
       model => null ()
    end if
  end function model_list_get_model_ptr

  subroutine model_list_final (model_list)
    class(model_list_t), intent(inout) :: model_list
    type(model_entry_t), pointer :: current
    model_list%last => null ()
    do while (associated (model_list%first))
       current => model_list%first
       model_list%first => model_list%first%next
       call current%final ()
       deallocate (current)
    end do
  end subroutine model_list_final

  subroutine model_copy (model, orig)
    class(model_t), intent(out), target :: model
    type(model_t), intent(in) :: orig
    integer :: n_par, n_prt, n_vtx
    integer :: i
    n_par = size (orig%par)
    n_prt = orig%get_n_field ()
    n_vtx = orig%get_n_vtx ()
    call model%basic_init (orig%get_name (), n_par, n_prt, n_vtx)
    model%md5sum = orig%md5sum
    do i = 1, n_par
       call model%copy_parameter (i, orig%par(i))
    end do
    model%init_external_parameters => orig%init_external_parameters
    call model%copy_from (orig)
    model%max_par_name_length = orig%max_par_name_length
    call model%append_field_vars ()
  end subroutine model_copy
  
!   subroutine model_pointer_to_instance (model)
!     type(model_t), pointer, intent(inout) :: model
!     type(model_t), pointer :: model_tmp
!     model_tmp => model
!     allocate (model)
!     call model%init_instance (model_tmp)
!   end subroutine model_pointer_to_instance
    
!   subroutine model_pointer_delete_instance (model)
!     type(model_t), pointer, intent(inout) :: model
!     if (associated (model)) then
!        call model%final ()
!        deallocate (model)
!     end if
!   end subroutine model_pointer_delete_instance
    

  subroutine models_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (models_1, "models_1", &
         "construct model", &
         u, results)
    call test (models_2, "models_2", &
         "read model", &
         u, results)
    call test (models_3, "models_3", &
         "model instance", &
         u, results)
    call test (models_4, "models_4", &
         "handle decays and polarization", &
         u, results)
    call test (models_5, "models_5", &
         "handle parameters", &
         u, results)
  end subroutine models_test

  subroutine models_1 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: model
    type(string_t) :: model_name
    type(string_t) :: x_longname
    type(string_t), dimension(2) :: parname
    type(string_t), dimension(2) :: x_name
    type(string_t), dimension(1) :: x_anti
    type(string_t) :: x_tex_name, x_tex_anti
    type(string_t) :: y_longname
    type(string_t), dimension(2) :: y_name
    type(string_t) :: y_tex_name
    type(field_data_t), pointer :: field

    write (u, "(A)")  "* Test output: models_1"
    write (u, "(A)")  "*   Purpose: create a model"
    write (u, *)

    model_name = "Test model"
    call model_list%add (model_name, os_data, 2, 2, 3, model)
    parname(1) = "mx"
    parname(2) = "coup"
    call model%set_parameter_constant (1, parname(1), 10._default)
    call model%set_parameter_constant (2, parname(2), 1.3_default)
    x_longname = "X_LEPTON"
    x_name(1) = "X"
    x_name(2) = "x"
    x_anti(1) = "Xbar"
    x_tex_name = "X^+"
    x_tex_anti = "X^-"
    field => model%get_field_ptr_by_index (1)
    call field%init (x_longname, 99)
    call field%set ( &
         .true., .false., .false., .false., .false., &
         name=x_name, anti=x_anti, tex_name=x_tex_name, tex_anti=x_tex_anti, &
         spin_type=SPINOR, isospin_type=-3, charge_type=2, &
         mass_data=model%get_par_data_ptr (parname(1)))
    y_longname = "Y_COLORON"
    y_name(1) = "Y"
    y_name(2) = "yc"
    y_tex_name = "Y^0"
    field => model%get_field_ptr_by_index (2)
    call field%init (y_longname, 97)
    call field%set ( &
          .false., .false., .true., .false., .false., &
          name=y_name, tex_name=y_tex_name, &
          spin_type=SCALAR, isospin_type=2, charge_type=1, color_type=8)
    call model%set_vertex (1, [99, 99, 99])
    call model%set_vertex (2, [99, 99, 99, 99])
    call model%set_vertex (3, [99, 97, 99])
    call model_list%write (u)

    call model_list%final ()

    write (u, *)
    write (u, "(A)")  "* Test output end: models_1"

  end subroutine models_1

  subroutine models_2 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: model

    write (u, "(A)")  "* Test output: models_2"
    write (u, "(A)")  "*   Purpose: read a model from file"
    write (u, *)

    call syntax_model_file_init ()
    call os_data_init (os_data)

    call model_list%read_model (var_str ("Test"), var_str ("Test.mdl"), &
         os_data, model)
    call model_list%write (u)
    
    write (u, *)
    write (u, "(A)")  "* Variable list"
    write (u, *)
    
    call var_list_write (model%var_list, u)

    write (u, *)
    write (u, "(A)")  "* Cleanup"
    
    call model_list%final ()
    call syntax_model_file_final ()

    write (u, *)
    write (u, "(A)")  "* Test output end: models_2"

  end subroutine models_2

  subroutine models_3 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: model
    type(model_t), pointer :: instance

    write (u, "(A)")  "* Test output: models_3"
    write (u, "(A)")  "*   Purpose: create a model instance"
    write (u, *)

    call syntax_model_file_init ()
    call os_data_init (os_data)

    call model_list%read_model (var_str ("Test"), var_str ("Test.mdl"), &
         os_data, model)
    allocate (instance)
    call instance%init_instance (model)
    
    call model%write (u)
    
    write (u, *)
    write (u, "(A)")  "* Variable list"
    write (u, *)
    
    call var_list_write (instance%var_list, u)

    write (u, *)
    write (u, "(A)")  "* Cleanup"
    
    call instance%final ()
    deallocate (instance)
    
    call model_list%final ()
    call syntax_model_file_final ()

    write (u, *)
    write (u, "(A)")  "* Test output end: models_3"

  end subroutine models_3

  subroutine models_4 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: model, model_instance
    character(32) :: md5sum

    write (u, "(A)")  "* Test output: models_4"
    write (u, "(A)")  "*   Purpose: set and unset decays and polarization"
    write (u, *)

    call syntax_model_file_init ()
    call os_data_init (os_data)

    write (u, "(A)")  "* Read model from file"

    call model_list%read_model (var_str ("Test"), var_str ("Test.mdl"), &
         os_data, model)

    md5sum = model%get_parameters_md5sum ()
    write (u, *)
    write (u, "(1x,3A)")  "MD5 sum (parameters) = '", md5sum, "'"

    write (u, *)
    write (u, "(A)")  "* Set particle decays and polarization"
    write (u, *)

    call model%set_unstable (25, [var_str ("dec1"), var_str ("dec2")])
    call model%set_polarized (6)
    call model%set_unstable (-6, [var_str ("fdec")])

    call model%write (u)

    md5sum = model%get_parameters_md5sum ()
    write (u, *)
    write (u, "(1x,3A)")  "MD5 sum (parameters) = '", md5sum, "'"

    write (u, *)
    write (u, "(A)")  "* Create a model instance"

    allocate (model_instance)
    call model_instance%init_instance (model)

    write (u, *)
    write (u, "(A)")  "* Revert particle decays and polarization"
    write (u, *)

    call model%set_stable (25)
    call model%set_unpolarized (6)
    call model%set_stable (-6)

    call model%write (u)
    
    md5sum = model%get_parameters_md5sum ()
    write (u, *)
    write (u, "(1x,3A)")  "MD5 sum (parameters) = '", md5sum, "'"

    write (u, *)
    write (u, "(A)")  "* Show the model instance"
    write (u, *)

    call model_instance%write (u)

    md5sum = model_instance%get_parameters_md5sum ()
    write (u, *)
    write (u, "(1x,3A)")  "MD5 sum (parameters) = '", md5sum, "'"

    write (u, *)
    write (u, "(A)")  "* Cleanup"
    
    call model_instance%final ()
    deallocate (model_instance)
    call model_list%final ()
    call syntax_model_file_final ()

    write (u, *)
    write (u, "(A)")  "* Test output end: models_4"

  end subroutine models_4

  subroutine models_5 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: model, model_instance
    character(32) :: md5sum

    write (u, "(A)")  "* Test output: models_5"
    write (u, "(A)")  "*   Purpose: access and modify model variables"
    write (u, *)

    call syntax_model_file_init ()
    call os_data_init (os_data)

    write (u, "(A)")  "* Read model from file"

    call model_list%read_model (var_str ("Test"), var_str ("Test.mdl"), &
         os_data, model)

    write (u, *)

    call model%write (u, &
         show_md5sum = .true., &
         show_variables = .true., &
         show_parameters = .true., &
         show_particles = .false., &
         show_vertices = .false.)

    write (u, *)
    write (u, "(A)")  "* Check parameter status"
    write (u, *)

    write (u, "(1x,A,L1)") "xy exists = ", model%var_exists (var_str ("xx"))
    write (u, "(1x,A,L1)") "ff exists = ", model%var_exists (var_str ("ff"))
    write (u, "(1x,A,L1)") "mf exists = ", model%var_exists (var_str ("mf"))
    write (u, "(1x,A,L1)") "ff locked = ", model%var_is_locked (var_str ("ff"))
    write (u, "(1x,A,L1)") "mf locked = ", model%var_is_locked (var_str ("mf"))

    write (u, *)
    write (u, "(1x,A,F6.2)") "ff = ", model%get_rval (var_str ("ff"))
    write (u, "(1x,A,F6.2)") "mf = ", model%get_rval (var_str ("mf"))

    write (u, *)
    write (u, "(A)")  "* Modify parameter"
    write (u, *)
    
    call model%set_real (var_str ("ff"), 1._default)

    call model%write (u, &
         show_md5sum = .true., &
         show_variables = .true., &
         show_parameters = .true., &
         show_particles = .false., &
         show_vertices = .false.)

    write (u, *)
    write (u, "(A)")  "* Cleanup"
    
    call model_list%final ()
    call syntax_model_file_final ()

    write (u, *)
    write (u, "(A)")  "* Test output end: models_5"

  end subroutine models_5


end module models
