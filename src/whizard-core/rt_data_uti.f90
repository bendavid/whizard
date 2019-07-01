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

module rt_data_uti

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use format_defs, only: FMT_19
  use ifiles
  use lexers
  use parser
  use flavors
  use variables
  use eval_trees
  use models
  use prclib_stacks

  use rt_data

  implicit none
  private

  public :: rt_data_1
  public :: rt_data_2
  public :: rt_data_3
  public :: rt_data_4
  public :: rt_data_5
  public :: rt_data_6
  public :: rt_data_7
  public :: rt_data_8
  public :: rt_data_9

contains

  subroutine fix_system_dependencies (global)
    class(rt_data_t), intent(inout), target :: global
    type(var_list_t), pointer :: var_list
    
    var_list => global%get_var_list_ptr ()
    call var_list%set_log (var_str ("?omega_openmp"), &
         .false., is_known = .true., force=.true.) 
    call var_list%set_log (var_str ("?openmp_is_active"), &
         .false., is_known = .true., force=.true.)
    call var_list%set_int (var_str ("openmp_num_threads_default"), &
         1, is_known = .true., force=.true.)
    call var_list%set_int (var_str ("openmp_num_threads"), &
         1, is_known = .true., force=.true.)        
    call var_list%set_int (var_str ("real_range"), &
         307, is_known = .true., force=.true.)
    call var_list%set_int (var_str ("real_precision"), &
         15, is_known = .true., force=.true.)    
    call var_list%set_real (var_str ("real_epsilon"), &
         1.e-16_default, is_known = .true., force=.true.)
    call var_list%set_real (var_str ("real_tiny"), &
         1.e-300_default, is_known = .true., force=.true.)     
    
    global%os_data%fc = "Fortran-compiler"
    global%os_data%fcflags = "Fortran-flags"
        
  end subroutine fix_system_dependencies
  
  function is_stable (pdg, global) result (flag)
    integer, intent(in) :: pdg
    type(rt_data_t), intent(in) :: global
    logical :: flag
    type(flavor_t) :: flv
    call flv%init (pdg, global%model)
    flag = flv%is_stable ()
  end function is_stable
   
  function is_polarized (pdg, global) result (flag)
    integer, intent(in) :: pdg
    type(rt_data_t), intent(in) :: global
    logical :: flag
    type(flavor_t) :: flv
    call flv%init (pdg, global%model)
    flag = flv%is_polarized ()
  end function is_polarized
    

  subroutine rt_data_1 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: global
    
    write (u, "(A)")  "* Test output: rt_data_1"
    write (u, "(A)")  "*   Purpose: initialize global runtime data"
    write (u, "(A)")

    call global%global_init (logfile = var_str ("rt_data.log"))
    call fix_system_dependencies (global)

    call global%set_int (var_str ("seed"), 0, is_known=.true.)            

    call global%it_list%init ([2, 3], [5000, 20000])

    call global%write (u)

    call global%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: rt_data_1"
    
  end subroutine rt_data_1
  
  subroutine rt_data_2 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: global
    type(flavor_t), dimension(2) :: flv
    type(string_t) :: cut_expr_text
    type(ifile_t) :: ifile
    type(stream_t) :: stream
    type(parse_tree_t) :: parse_tree
    
    write (u, "(A)")  "* Test output: rt_data_2"
    write (u, "(A)")  "*   Purpose: initialize global runtime data &
         &and fill contents"
    write (u, "(A)")

    call syntax_model_file_init ()

    call global%global_init ()
    call fix_system_dependencies (global)

    call global%select_model (var_str ("Test"))

    call global%set_real (var_str ("sqrts"), &
         1000._default, is_known = .true.)
    call global%set_int (var_str ("seed"), &
         0, is_known=.true.)        
    call flv%init ([25,25], global%model)
    
    call global%set_string (var_str ("$run_id"), &
         var_str ("run1"), is_known = .true.)
    call global%set_real (var_str ("luminosity"), &
         33._default, is_known = .true.)
    
    call syntax_pexpr_init ()
    cut_expr_text = "all Pt > 100 [s]"
    call ifile_append (ifile, cut_expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_lexpr (parse_tree, stream, .true.)
    global%pn%cuts_lexpr => parse_tree%get_root_ptr ()
    
    allocate (global%sample_fmt (2))
    global%sample_fmt(1) = "foo_fmt"
    global%sample_fmt(2) = "bar_fmt"
    
    call global%write (u)

    call parse_tree_final (parse_tree)
    call stream_final (stream)
    call ifile_final (ifile)
    call syntax_pexpr_final ()

    call global%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: rt_data_2"
    
  end subroutine rt_data_2
  
  subroutine rt_data_3 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: global, local
    type(flavor_t), dimension(2) :: flv
    type(string_t) :: cut_expr_text
    type(ifile_t) :: ifile
    type(stream_t) :: stream
    type(parse_tree_t) :: parse_tree
    type(prclib_entry_t), pointer :: lib
    
    write (u, "(A)")  "* Test output: rt_data_3"
    write (u, "(A)")  "*   Purpose: initialize global runtime data &
         &and fill contents;"
    write (u, "(A)")  "*            copy to local block and back"
    write (u, "(A)")

    write (u, "(A)")  "* Init global data"
    write (u, "(A)")

    call syntax_model_file_init ()

    call global%global_init ()
    call fix_system_dependencies (global)

    call global%set_int (var_str ("seed"), &
         0, is_known=.true.)        

    call global%select_model (var_str ("Test"))

    call global%set_real (var_str ("sqrts"),&
         1000._default, is_known = .true.)
    call flv%init ([25,25], global%model)
    
    call global%beam_structure%init_sf (flv%get_name (), [1])
    call global%beam_structure%set_sf (1, 1, var_str ("pdf_builtin"))

    call global%set_string (var_str ("$run_id"), &
         var_str ("run1"), is_known = .true.)
    call global%set_real (var_str ("luminosity"), &
         33._default, is_known = .true.)
    
    call syntax_pexpr_init ()
    cut_expr_text = "all Pt > 100 [s]"
    call ifile_append (ifile, cut_expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_lexpr (parse_tree, stream, .true.)
    global%pn%cuts_lexpr => parse_tree%get_root_ptr ()
    
    allocate (global%sample_fmt (2))
    global%sample_fmt(1) = "foo_fmt"
    global%sample_fmt(2) = "bar_fmt"

    allocate (lib)
    call lib%init (var_str ("library_1"))
    call global%add_prclib (lib)

    write (u, "(A)")  "* Init and modify local data"
    write (u, "(A)")

    call local%local_init (global)
    call local%append_string (var_str ("$integration_method"), intrinsic=.true.)
    call local%append_string (var_str ("$phs_method"), intrinsic=.true.)

    call local%activate ()

    write (u, "(1x,A,L1)")  "model associated   = ", associated (local%model)
    write (u, "(1x,A,L1)")  "library associated = ", associated (local%prclib)
    write (u, *)

    call local%model_set_real (var_str ("ms"), 150._default)
    call local%set_string (var_str ("$integration_method"), &
         var_str ("midpoint"), is_known = .true.)
    call local%set_string (var_str ("$phs_method"), &
         var_str ("single"), is_known = .true.)

    local%os_data%fc = "Local compiler"
    
    allocate (lib)
    call lib%init (var_str ("library_2"))
    call local%add_prclib (lib)

    call local%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Restore global data"
    write (u, "(A)")
    
    call local%deactivate (global)

    write (u, "(1x,A,L1)")  "model associated   = ", associated (global%model)
    write (u, "(1x,A,L1)")  "library associated = ", associated (global%prclib)
    write (u, *)

    call global%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call parse_tree_final (parse_tree)
    call stream_final (stream)
    call ifile_final (ifile)
    call syntax_pexpr_final ()

    call global%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: rt_data_3"
    
  end subroutine rt_data_3
  
  subroutine rt_data_4 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: global
    
    type(string_t), dimension(0) :: empty_string_array

    write (u, "(A)")  "* Test output: rt_data_4"
    write (u, "(A)")  "*   Purpose: display selected variables"
    write (u, "(A)")

    call global%global_init ()

    write (u, "(A)")  "* No variables:"
    write (u, "(A)")

    call global%write_vars (u, empty_string_array)

    write (u, "(A)")  "* Two variables:"
    write (u, "(A)")

    call global%write_vars (u, &
         [var_str ("?unweighted"), var_str ("$phs_method")])
    
    write (u, "(A)")
    write (u, "(A)")  "* Display whole record with selected variables"
    write (u, "(A)")

    call global%write (u, &
         vars = [var_str ("?unweighted"), var_str ("$phs_method")])

    call global%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: rt_data_4"
    
  end subroutine rt_data_4
  
  subroutine rt_data_5 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: global
    
    write (u, "(A)")  "* Test output: rt_data_5"
    write (u, "(A)")  "*   Purpose: display parts of rt data"
    write (u, "(A)")

    call global%global_init ()
    call global%write_libraries (u)

    write (u, "(A)")

    call global%write_beams (u)

    write (u, "(A)")

    call global%write_process_stack (u)

    call global%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: rt_data_5"
    
  end subroutine rt_data_5
  
  subroutine rt_data_6 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: global, local
    type(var_list_t), pointer :: model_vars
    type(string_t) :: var_name

    write (u, "(A)")  "* Test output: rt_data_6"
    write (u, "(A)")  "*   Purpose: apply and keep local modifications to model"
    write (u, "(A)")

    call syntax_model_file_init ()

    call global%global_init ()
    call global%select_model (var_str ("Test"))
    
    write (u, "(A)")  "* Original model"
    write (u, "(A)")

    call global%write_model_list (u)
    write (u, *)
    write (u, "(A,L1)")  "s is stable    = ", is_stable (25, global)
    write (u, "(A,L1)")  "f is polarized = ", is_polarized (6, global)

    write (u, *)

    var_name = "ff"

    write (u, "(A)", advance="no")  "Global model variable: "
    model_vars => global%model%get_var_list_ptr ()
    call var_list_write_var (model_vars, var_name, u)

    write (u, "(A)")
    write (u, "(A)")  "* Apply local modifications: unstable"
    write (u, "(A)")

    call local%local_init (global)
    call local%activate ()

    call local%model_set_real (var_name, 0.4_default)
    call local%modify_particle (25, stable = .false., decay = [var_str ("d1")])
    call local%modify_particle (6, stable = .false., &
         decay = [var_str ("f1")], isotropic_decay = .true.)
    call local%modify_particle (-6, stable = .false., &
         decay = [var_str ("f2"), var_str ("f3")], diagonal_decay = .true.)

    call local%model%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Further modifications"
    write (u, "(A)")

    call local%modify_particle (6, stable = .false., &
         decay = [var_str ("f1")], &
         diagonal_decay = .true., isotropic_decay = .false.)
    call local%modify_particle (-6, stable = .false., &
         decay = [var_str ("f2"), var_str ("f3")], &
         diagonal_decay = .false., isotropic_decay = .true.)
    call local%model%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Further modifications: f stable but polarized"
    write (u, "(A)")

    call local%modify_particle (6, stable = .true., polarized = .true.)
    call local%modify_particle (-6, stable = .true.)
    call local%model%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Global model"
    write (u, "(A)")

    call global%model%write (u)
    write (u, *)
    write (u, "(A,L1)")  "s is stable    = ", is_stable (25, global)
    write (u, "(A,L1)")  "f is polarized = ", is_polarized (6, global)

    write (u, "(A)")
    write (u, "(A)")  "* Local model"
    write (u, "(A)")

    call local%model%write (u)
    write (u, *)
    write (u, "(A,L1)")  "s is stable    = ", is_stable (25, local)
    write (u, "(A,L1)")  "f is polarized = ", is_polarized (6, local)

    write (u, *)

    write (u, "(A)", advance="no")  "Global model variable: "
    model_vars => global%model%get_var_list_ptr ()
    call var_list_write_var (model_vars, var_name, u)

    write (u, "(A)", advance="no")  "Local model variable: "
    call var_list_write_var (local%model%get_var_list_ptr (), &
         var_name, u)

    write (u, "(A)")
    write (u, "(A)")  "* Restore global"

    call local%deactivate (global, keep_local = .true.)

    write (u, "(A)")
    write (u, "(A)")  "* Global model"
    write (u, "(A)")

    call global%model%write (u)
    write (u, *)
    write (u, "(A,L1)")  "s is stable    = ", is_stable (25, global)
    write (u, "(A,L1)")  "f is polarized = ", is_polarized (6, global)

    write (u, "(A)")
    write (u, "(A)")  "* Local model"
    write (u, "(A)")

    call local%model%write (u)
    write (u, *)
    write (u, "(A,L1)")  "s is stable    = ", is_stable (25, local)
    write (u, "(A,L1)")  "f is polarized = ", is_polarized (6, local)

    write (u, *)

    write (u, "(A)", advance="no")  "Global model variable: "
    model_vars => global%model%get_var_list_ptr ()
    call var_list_write_var (model_vars, var_name, u)

    write (u, "(A)", advance="no")  "Local model variable: "
    call var_list_write_var (local%model%get_var_list_ptr (), &
         var_name, u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call local%model%final ()
    deallocate (local%model)
    
    call global%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: rt_data_6"
    
  end subroutine rt_data_6
  
  subroutine rt_data_7 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: global

    write (u, "(A)")  "* Test output: rt_data_7"
    write (u, "(A)")  "*   Purpose: set and access result variables"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize process variables"
    write (u, "(A)")

    call global%global_init ()
    call global%process_stack%init_result_vars (var_str ("testproc"))
    
    call var_list_write_var (global%var_list, &
         var_str ("integral(testproc)"), u)
    call var_list_write_var (global%var_list, &
         var_str ("error(testproc)"), u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call global%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: rt_data_7"
    
  end subroutine rt_data_7
  
  subroutine rt_data_8 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: global

    write (u, "(A)")  "* Test output: rt_data_8"
    write (u, "(A)")  "*   Purpose: get correct collision energy"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize"
    write (u, "(A)")

    call global%global_init ()

    write (u, "(A)")  "* Set sqrts"
    write (u, "(A)")

    call global%set_real (var_str ("sqrts"), &
         1000._default, is_known = .true.)
    write (u, "(1x,A," // FMT_19 // ")")  "sqrts =", global%get_sqrts ()

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call global%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: rt_data_8"
    
  end subroutine rt_data_8
  
  subroutine rt_data_9 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: global, local
    type(var_list_t), pointer :: var_list

    write (u, "(A)")  "* Test output: rt_data_9"
    write (u, "(A)")  "*   Purpose: handle local variables"
    write (u, "(A)")

    call syntax_model_file_init ()

    write (u, "(A)")  "* Initialize global record and set some variables"
    write (u, "(A)")

    call global%global_init ()
    call global%select_model (var_str ("Test"))
    
    call global%set_real (var_str ("sqrts"), 17._default, is_known = .true.)
    call global%set_real (var_str ("luminosity"), 2._default, is_known = .true.)
    call global%model_set_real (var_str ("ff"), 0.5_default)
    call global%model_set_real (var_str ("gy"), 1.2_default)

    var_list => global%get_var_list_ptr ()

    call var_list_write_var (var_list, var_str ("sqrts"), u)
    call var_list_write_var (var_list, var_str ("luminosity"), u)
    call var_list_write_var (var_list, var_str ("ff"), u)
    call var_list_write_var (var_list, var_str ("gy"), u)
    call var_list_write_var (var_list, var_str ("mf"), u)
    call var_list_write_var (var_list, var_str ("x"), u)

    write (u, "(A)")
    
    write (u, "(1x,A,1x,F5.2)")  "sqrts      = ", &
         global%get_rval (var_str ("sqrts"))
    write (u, "(1x,A,1x,F5.2)")  "luminosity = ", &
         global%get_rval (var_str ("luminosity"))
    write (u, "(1x,A,1x,F5.2)")  "ff         = ", &
         global%get_rval (var_str ("ff"))
    write (u, "(1x,A,1x,F5.2)")  "gy         = ", &
         global%get_rval (var_str ("gy"))
    write (u, "(1x,A,1x,F5.2)")  "mf         = ", &
         global%get_rval (var_str ("mf"))
    write (u, "(1x,A,1x,F5.2)")  "x          = ", &
         global%get_rval (var_str ("x"))

    write (u, "(A)")
    write (u, "(A)")  "* Create local record with local variables"
    write (u, "(A)")

    call local%local_init (global)

    call local%append_real (var_str ("luminosity"), intrinsic = .true.)
    call local%append_real (var_str ("x"), user = .true.)

    call local%activate ()

    var_list => local%get_var_list_ptr ()

    call var_list_write_var (var_list, var_str ("sqrts"), u)
    call var_list_write_var (var_list, var_str ("luminosity"), u)
    call var_list_write_var (var_list, var_str ("ff"), u)
    call var_list_write_var (var_list, var_str ("gy"), u)
    call var_list_write_var (var_list, var_str ("mf"), u)
    call var_list_write_var (var_list, var_str ("x"), u)

    write (u, "(A)")
    
    write (u, "(1x,A,1x,F5.2)")  "sqrts      = ", &
         local%get_rval (var_str ("sqrts"))
    write (u, "(1x,A,1x,F5.2)")  "luminosity = ", &
         local%get_rval (var_str ("luminosity"))
    write (u, "(1x,A,1x,F5.2)")  "ff         = ", &
         local%get_rval (var_str ("ff"))
    write (u, "(1x,A,1x,F5.2)")  "gy         = ", &
         local%get_rval (var_str ("gy"))
    write (u, "(1x,A,1x,F5.2)")  "mf         = ", &
         local%get_rval (var_str ("mf"))
    write (u, "(1x,A,1x,F5.2)")  "x          = ", &
         local%get_rval (var_str ("x"))

    write (u, "(A)")
    write (u, "(A)")  "* Modify some local variables"
    write (u, "(A)")

    call local%set_real (var_str ("luminosity"), 42._default, is_known=.true.)
    call local%set_real (var_str ("x"), 6.66_default, is_known=.true.)
    call local%model_set_real (var_str ("ff"), 0.7_default)

    var_list => local%get_var_list_ptr ()

    call var_list_write_var (var_list, var_str ("sqrts"), u)
    call var_list_write_var (var_list, var_str ("luminosity"), u)
    call var_list_write_var (var_list, var_str ("ff"), u)
    call var_list_write_var (var_list, var_str ("gy"), u)
    call var_list_write_var (var_list, var_str ("mf"), u)
    call var_list_write_var (var_list, var_str ("x"), u)

    write (u, "(A)")
    
    write (u, "(1x,A,1x,F5.2)")  "sqrts      = ", &
         local%get_rval (var_str ("sqrts"))
    write (u, "(1x,A,1x,F5.2)")  "luminosity = ", &
         local%get_rval (var_str ("luminosity"))
    write (u, "(1x,A,1x,F5.2)")  "ff         = ", &
         local%get_rval (var_str ("ff"))
    write (u, "(1x,A,1x,F5.2)")  "gy         = ", &
         local%get_rval (var_str ("gy"))
    write (u, "(1x,A,1x,F5.2)")  "mf         = ", &
         local%get_rval (var_str ("mf"))
    write (u, "(1x,A,1x,F5.2)")  "x          = ", &
         local%get_rval (var_str ("x"))

    write (u, "(A)")
    write (u, "(A)")  "* Restore globals"
    write (u, "(A)")

    call local%deactivate (global)
    
    var_list => global%get_var_list_ptr ()

    call var_list_write_var (var_list, var_str ("sqrts"), u)
    call var_list_write_var (var_list, var_str ("luminosity"), u)
    call var_list_write_var (var_list, var_str ("ff"), u)
    call var_list_write_var (var_list, var_str ("gy"), u)
    call var_list_write_var (var_list, var_str ("mf"), u)
    call var_list_write_var (var_list, var_str ("x"), u)

    write (u, "(A)")
    
    write (u, "(1x,A,1x,F5.2)")  "sqrts      = ", &
         global%get_rval (var_str ("sqrts"))
    write (u, "(1x,A,1x,F5.2)")  "luminosity = ", &
         global%get_rval (var_str ("luminosity"))
    write (u, "(1x,A,1x,F5.2)")  "ff         = ", &
         global%get_rval (var_str ("ff"))
    write (u, "(1x,A,1x,F5.2)")  "gy         = ", &
         global%get_rval (var_str ("gy"))
    write (u, "(1x,A,1x,F5.2)")  "mf         = ", &
         global%get_rval (var_str ("mf"))
    write (u, "(1x,A,1x,F5.2)")  "x          = ", &
         global%get_rval (var_str ("x"))

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call local%local_final ()

    call global%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: rt_data_9"
    
  end subroutine rt_data_9
  

end module rt_data_uti
