! WHIZARD 2.4.0 Nov 28 2016
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
!     So Young Shim <soyoung.shim@desy.de>
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

module models_uti

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use physics_defs, only: SCALAR, SPINOR
  use os_interface
  use model_data
  use variables

  use models

  implicit none
  private

  public :: models_1
  public :: models_2
  public :: models_3
  public :: models_4
  public :: models_5
  public :: models_6
  public :: models_7

contains

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
    type(var_list_t), pointer :: var_list
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

    var_list => model%get_var_list_ptr ()
    call var_list%write (u)

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
    type(var_list_t), pointer :: var_list
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

    var_list => instance%get_var_list_ptr ()
    call var_list%write (u)

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

  subroutine models_6 (u)
    integer, intent(in) :: u
    integer :: um
    character(80) :: buffer
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(var_list_t), pointer :: var_list
    type(model_t), pointer :: model

    write (u, "(A)")  "* Test output: models_6"
    write (u, "(A)")  "*   Purpose: read a model from file &
         &with non-canonical parameter ordering"
    write (u, *)

    open (newunit=um, file="Test6.mdl", status="replace", action="readwrite")
    write (um, "(A)")  'model "Test6"'
    write (um, "(A)")  '   parameter a =  1.000000000000E+00'
    write (um, "(A)")  '   derived   b =  2 * a'
    write (um, "(A)")  '   parameter c =  3.000000000000E+00'
    write (um, "(A)")  '   unused    d'

    rewind (um)
    do
       read (um, "(A)", end=1)  buffer
       write (u, "(A)")  trim (buffer)
    end do
1   continue
    close (um)

    call syntax_model_file_init ()
    call os_data_init (os_data)

    call model_list%read_model (var_str ("Test6"), var_str ("Test6.mdl"), &
         os_data, model)

    write (u, *)
    write (u, "(A)")  "* Variable list"
    write (u, *)

    var_list => model%get_var_list_ptr ()
    call var_list%write (u)

    write (u, *)
    write (u, "(A)")  "* Cleanup"

    call model_list%final ()
    call syntax_model_file_final ()

    write (u, *)
    write (u, "(A)")  "* Test output end: models_6"

  end subroutine models_6

  subroutine models_7 (u)
    integer, intent(in) :: u
    integer :: um
    character(80) :: buffer
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(var_list_t), pointer :: var_list
    type(model_t), pointer :: model

    write (u, "(A)")  "* Test output: models_7"
    write (u, "(A)")  "*   Purpose: read a model from file &
         &with scheme selection"
    write (u, *)

    open (newunit=um, file="Test7.mdl", status="replace", action="readwrite")
    write (um, "(A)")  'model "Test7"'
    write (um, "(A)")  '  schemes = "foo", "bar", "gee"'
    write (um, "(A)")  ''
    write (um, "(A)")  '  select scheme'
    write (um, "(A)")  '  scheme "foo"'
    write (um, "(A)")  '    parameter a = 1'
    write (um, "(A)")  '    derived   b = 2 * a'
    write (um, "(A)")  '  scheme other'
    write (um, "(A)")  '    parameter b = 4'
    write (um, "(A)")  '    derived   a = b / 2'
    write (um, "(A)")  '  end select'
    write (um, "(A)")  ''
    write (um, "(A)")  '  parameter c = 3'
    write (um, "(A)")  ''
    write (um, "(A)")  '  select scheme'
    write (um, "(A)")  '  scheme "foo", "gee"'
    write (um, "(A)")  '    derived   d = b + c'
    write (um, "(A)")  '  scheme other'
    write (um, "(A)")  '    unused    d'
    write (um, "(A)")  '  end select'

    rewind (um)
    do
       read (um, "(A)", end=1)  buffer
       write (u, "(A)")  trim (buffer)
    end do
1   continue
    close (um)

    call syntax_model_file_init ()
    call os_data_init (os_data)

    write (u, *)
    write (u, "(A)")  "* Model output, default scheme (= foo)"
    write (u, *)

    call model_list%read_model (var_str ("Test7"), var_str ("Test7.mdl"), &
         os_data, model)
    call model%write (u, show_md5sum=.false.)
    call show_var_list ()
    call show_par_array ()

    call model_list%final ()

    write (u, *)
    write (u, "(A)")  "* Model output, scheme foo"
    write (u, *)

    call model_list%read_model (var_str ("Test7"), var_str ("Test7.mdl"), &
         os_data, model, scheme = var_str ("foo"))
    call model%write (u, show_md5sum=.false.)
    call show_var_list ()
    call show_par_array ()

    call model_list%final ()

    write (u, *)
    write (u, "(A)")  "* Model output, scheme bar"
    write (u, *)

    call model_list%read_model (var_str ("Test7"), var_str ("Test7.mdl"), &
         os_data, model, scheme = var_str ("bar"))
    call model%write (u, show_md5sum=.false.)
    call show_var_list ()
    call show_par_array ()

    call model_list%final ()

    write (u, *)
    write (u, "(A)")  "* Model output, scheme gee"
    write (u, *)

    call model_list%read_model (var_str ("Test7"), var_str ("Test7.mdl"), &
         os_data, model, scheme = var_str ("gee"))
    call model%write (u, show_md5sum=.false.)
    call show_var_list ()
    call show_par_array ()

    write (u, *)
    write (u, "(A)")  "* Cleanup"

    call model_list%final ()
    call syntax_model_file_final ()

    write (u, *)
    write (u, "(A)")  "* Test output end: models_7"

  contains

    subroutine show_var_list ()
      write (u, *)
      write (u, "(A)")  "* Variable list"
      write (u, *)
      var_list => model%get_var_list_ptr ()
      call var_list%write (u)
    end subroutine show_var_list

    subroutine show_par_array ()
      real(default), dimension(:), allocatable :: par
      integer :: n
      write (u, *)
      write (u, "(A)")  "* Parameter array"
      write (u, *)
      n = model%get_n_real ()
      allocate (par (n))
      call model%real_parameters_to_array (par)
      write (u, 1)  par
1     format (1X,F6.3)
    end subroutine show_par_array

  end subroutine models_7


end module models_uti
