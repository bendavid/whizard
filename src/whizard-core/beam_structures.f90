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

module beam_structures
  
  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use limits, only: FMT_19 !NODEP!
  use diagnostics !NODEP!
  use lorentz !NODEP!
  use unit_tests
  
  use variables
  use polarizations

  implicit none
  private

  public :: beam_structure_t
  public :: beam_structures_test

  type :: beam_structure_entry_t
     logical :: is_valid = .false.
     type(string_t) :: name
   contains
     procedure :: to_string => beam_structure_entry_to_string
  end type beam_structure_entry_t
  
  type :: beam_structure_record_t
     type(beam_structure_entry_t), dimension(:), allocatable :: entry
  end type beam_structure_record_t

  type :: beam_structure_t
     private
     integer :: n_beam = 0
     type(string_t), dimension(:), allocatable :: prt
     type(beam_structure_record_t), dimension(:), allocatable :: record
     type(smatrix_t), dimension(:), allocatable :: smatrix
     real(default), dimension(:), allocatable :: pol_f
     real(default), dimension(:), allocatable :: p
     real(default), dimension(:), allocatable :: theta
     real(default), dimension(:), allocatable :: phi
   contains
     procedure :: final_sf => beam_structure_final_sf
     procedure :: write => beam_structure_write
     procedure :: to_string => beam_structure_to_string
     procedure :: init_sf => beam_structure_init_sf
     procedure :: set_sf => beam_structure_set_sf
     procedure :: expand => beam_structure_expand
     procedure :: final_pol => beam_structure_final_pol
     procedure :: init_pol => beam_structure_init_pol
     procedure :: set_smatrix => beam_structure_set_smatrix
     procedure :: init_smatrix => beam_structure_init_smatrix
     procedure :: set_sentry => beam_structure_set_sentry
     procedure :: set_pol_f => beam_structure_set_pol_f
     procedure :: final_mom => beam_structure_final_mom
     procedure :: set_momentum => beam_structure_set_momentum
     procedure :: set_theta => beam_structure_set_theta
     procedure :: set_phi => beam_structure_set_phi
     procedure :: is_set => beam_structure_is_set
     procedure :: get_n_beam => beam_structure_get_n_beam
     procedure :: get_prt => beam_structure_get_prt
     procedure :: get_n_record => beam_structure_get_n_record
     procedure :: get_i_entry => beam_structure_get_i_entry
     procedure :: get_name => beam_structure_get_name
     procedure :: contains => beam_structure_contains
     procedure :: polarized => beam_structure_polarized
     procedure :: get_smatrix => beam_structure_get_smatrix
     procedure :: get_pol_f => beam_structure_get_pol_f
     procedure :: asymmetric => beam_structure_asymmetric
     procedure :: get_momenta => beam_structure_get_momenta
  end type beam_structure_t
  

  abstract interface
     function strfun_mode_fun (name) result (n)
       import
       type(string_t), intent(in) :: name
       integer :: n
     end function strfun_mode_fun
  end interface
  

contains
  
  function beam_structure_entry_to_string (object) result (string)
    class(beam_structure_entry_t), intent(in) :: object
    type(string_t) :: string
    if (object%is_valid) then
       string = object%name
    else
       string = "none"
    end if
  end function beam_structure_entry_to_string

  subroutine beam_structure_final_sf (object)
    class(beam_structure_t), intent(inout) :: object
    if (allocated (object%prt))  deallocate (object%prt)
    if (allocated (object%record))  deallocate (object%record)
    object%n_beam = 0
  end subroutine beam_structure_final_sf
  
  subroutine beam_structure_write (object, unit)
    class(beam_structure_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u, i
    u = output_unit (unit)
    write (u, "(1x,A,A)")  "Beam structure: ", char (object%to_string ())
    if (allocated (object%smatrix)) then
       do i = 1, size (object%smatrix)
          write (u, "(3x,A,I0,A)") "polarization (beam ", i, "):"
          call object%smatrix(i)%write (u, indent=2)
       end do
    end if
    if (allocated (object%pol_f)) then
       write (u, "(3x,A,F10.7,:,',',F10.7)")  "polarization degree =", &
            object%pol_f
    end if
    if (allocated (object%p)) then
       write (u, "(3x,A," // FMT_19 // ",:,','," // FMT_19 // &
            ")")  "momentum =", object%p
    end if
    if (allocated (object%theta)) then
       write (u, "(3x,A," // FMT_19 // ",:,','," // FMT_19 // &
            ")")  "angle th =", object%theta
    end if
    if (allocated (object%phi)) then
       write (u, "(3x,A," // FMT_19 // ",:,','," // FMT_19 // &
            ")")  "angle ph =", object%phi
    end if
  end subroutine beam_structure_write
  
  function beam_structure_to_string (object, sf_only) result (string)
    class(beam_structure_t), intent(in) :: object
    logical, intent(in), optional :: sf_only
    type(string_t) :: string
    integer :: i, j
    logical :: with_beams
    with_beams = .true.;  if (present (sf_only))  with_beams = .not. sf_only
    select case (object%n_beam)
    case (1)
       if (with_beams) then
          string = object%prt(1)
       else
          string = ""
       end if
    case (2)
       if (with_beams) then
          string = object%prt(1) // ", " // object%prt(2)
       else
          string = ""
       end if
       if (allocated (object%record)) then
          if (size (object%record) > 0) then
             if (with_beams)  string = string // " => "
             do i = 1, size (object%record)
                if (i > 1)  string = string // " => "
                do j = 1, size (object%record(i)%entry)
                   if (j > 1)  string = string // ", "
                   string = string // object%record(i)%entry(j)%to_string ()
                end do
             end do
          end if
       end if
    case default
       string = "[any particles]"
    end select
  end function beam_structure_to_string
    
  subroutine beam_structure_init_sf (beam_structure, prt, dim_array)
    class(beam_structure_t), intent(inout) :: beam_structure
    type(string_t), dimension(:), intent(in) :: prt
    integer, dimension(:), intent(in), optional :: dim_array
    integer :: i
    call beam_structure%final_sf ()
    beam_structure%n_beam = size (prt)
    allocate (beam_structure%prt (size (prt)))
    beam_structure%prt = prt
    if (present (dim_array)) then
       allocate (beam_structure%record (size (dim_array)))
       do i = 1, size (dim_array)
          allocate (beam_structure%record(i)%entry (dim_array(i)))
       end do
    else
       allocate (beam_structure%record (0))
    end if
  end subroutine beam_structure_init_sf
    
  subroutine beam_structure_set_sf (beam_structure, i, j, name)
    class(beam_structure_t), intent(inout) :: beam_structure
    integer, intent(in) :: i, j
    type(string_t), intent(in) :: name
    associate (entry => beam_structure%record(i)%entry(j))
      entry%name = name
      entry%is_valid = .true.
    end associate
  end subroutine beam_structure_set_sf

  subroutine beam_structure_expand (beam_structure, strfun_mode)
    class(beam_structure_t), intent(inout) :: beam_structure
    procedure(strfun_mode_fun) :: strfun_mode
    type(beam_structure_record_t), dimension(:), allocatable :: new
    integer :: n_record, i, j
    if (.not. allocated (beam_structure%record))  return
    do i = 1, size (beam_structure%record)
       associate (entry => beam_structure%record(i)%entry)
         do j = 1, size (entry)
            select case (strfun_mode (entry(j)%name))
            case (0);  entry(j)%is_valid = .false.
            end select
         end do
       end associate
    end do
    n_record = 0
    do i = 1, size (beam_structure%record)
       associate (entry => beam_structure%record(i)%entry)
         select case (size (entry))
         case (1)
            if (entry(1)%is_valid) then
               select case (strfun_mode (entry(1)%name))
               case (1);  n_record = n_record + 2
               case (2);  n_record = n_record + 1
               end select
            end if
         case (2)
            do j = 1, 2
               if (entry(j)%is_valid) then
                  select case (strfun_mode (entry(j)%name))
                  case (1);  n_record = n_record + 1
                  case (2)
                     call beam_structure%write ()
                     call msg_fatal ("Pair spectrum used as &
                          &single-particle structure function")
                  end select
               end if
            end do
         end select
       end associate
    end do
    allocate (new (n_record))
    n_record = 0
    do i = 1, size (beam_structure%record)
       associate (entry => beam_structure%record(i)%entry)
         select case (size (entry))
         case (1)
            if (entry(1)%is_valid) then
               select case (strfun_mode (entry(1)%name))
               case (1)
                  n_record = n_record + 1
                  allocate (new(n_record)%entry (2))
                  new(n_record)%entry(1) = entry(1)
                  n_record = n_record + 1
                  allocate (new(n_record)%entry (2))
                  new(n_record)%entry(2) = entry(1)
               case (2)
                  n_record = n_record + 1
                  allocate (new(n_record)%entry (1))
                  new(n_record)%entry(1) = entry(1)
               end select
            end if
         case (2)
            do j = 1, 2
               if (entry(j)%is_valid) then
                  n_record = n_record + 1
                  allocate (new(n_record)%entry (2))
                  new(n_record)%entry(j) = entry(j)
               end if
            end do
         end select
       end associate
    end do
    call move_alloc (from = new, to = beam_structure%record)
  end subroutine beam_structure_expand
    
  subroutine beam_structure_final_pol (beam_structure)
    class(beam_structure_t), intent(inout) :: beam_structure
    if (allocated (beam_structure%smatrix))  deallocate (beam_structure%smatrix)
    if (allocated (beam_structure%pol_f))  deallocate (beam_structure%pol_f)
  end subroutine beam_structure_final_pol
    
  subroutine beam_structure_init_pol (beam_structure, n)
    class(beam_structure_t), intent(inout) :: beam_structure
    integer, intent(in) :: n
    if (allocated (beam_structure%smatrix))  deallocate (beam_structure%smatrix)
    allocate (beam_structure%smatrix (n))
    if (.not. allocated (beam_structure%pol_f)) &
         allocate (beam_structure%pol_f (n), source = 1._default)
  end subroutine beam_structure_init_pol
    
  subroutine beam_structure_set_smatrix (beam_structure, i, smatrix)
    class(beam_structure_t), intent(inout) :: beam_structure
    integer, intent(in) :: i
    type(smatrix_t), intent(in) :: smatrix
    beam_structure%smatrix(i) = smatrix
  end subroutine beam_structure_set_smatrix
  
  subroutine beam_structure_init_smatrix (beam_structure, i, n_entry)
    class(beam_structure_t), intent(inout) :: beam_structure
    integer, intent(in) :: i
    integer, intent(in) :: n_entry
    call beam_structure%smatrix(i)%init (2, n_entry)
  end subroutine beam_structure_init_smatrix
  
  subroutine beam_structure_set_sentry &
       (beam_structure, i, i_entry, index, value)
    class(beam_structure_t), intent(inout) :: beam_structure
    integer, intent(in) :: i
    integer, intent(in) :: i_entry
    integer, dimension(:), intent(in) :: index
    complex(default), intent(in) :: value
    call beam_structure%smatrix(i)%set_entry (i_entry, index, value)
  end subroutine beam_structure_set_sentry
  
  subroutine beam_structure_set_pol_f (beam_structure, f)
    class(beam_structure_t), intent(inout) :: beam_structure
    real(default), dimension(:), intent(in) :: f
    if (allocated (beam_structure%pol_f))  deallocate (beam_structure%pol_f)
    allocate (beam_structure%pol_f (size (f)), source = f)
  end subroutine beam_structure_set_pol_f
    
  subroutine beam_structure_final_mom (beam_structure)
    class(beam_structure_t), intent(inout) :: beam_structure
    if (allocated (beam_structure%p))  deallocate (beam_structure%p)
    if (allocated (beam_structure%theta))  deallocate (beam_structure%theta)
    if (allocated (beam_structure%phi))  deallocate (beam_structure%phi)
  end subroutine beam_structure_final_mom

  subroutine beam_structure_set_momentum (beam_structure, p)
    class(beam_structure_t), intent(inout) :: beam_structure
    real(default), dimension(:), intent(in) :: p
    if (allocated (beam_structure%p))  deallocate (beam_structure%p)
    allocate (beam_structure%p (size (p)), source = p)
  end subroutine beam_structure_set_momentum
    
  subroutine beam_structure_set_theta (beam_structure, theta)
    class(beam_structure_t), intent(inout) :: beam_structure
    real(default), dimension(:), intent(in) :: theta
    if (allocated (beam_structure%theta))  deallocate (beam_structure%theta)
    allocate (beam_structure%theta (size (theta)), source = theta)
  end subroutine beam_structure_set_theta
    
  subroutine beam_structure_set_phi (beam_structure, phi)
    class(beam_structure_t), intent(inout) :: beam_structure
    real(default), dimension(:), intent(in) :: phi
    if (allocated (beam_structure%phi))  deallocate (beam_structure%phi)
    allocate (beam_structure%phi (size (phi)), source = phi)
  end subroutine beam_structure_set_phi
    
  function beam_structure_is_set (beam_structure) result (flag)
    class(beam_structure_t), intent(in) :: beam_structure
    logical :: flag
    flag = beam_structure%n_beam > 0 .or. beam_structure%asymmetric ()
  end function beam_structure_is_set

  function beam_structure_get_n_beam (beam_structure) result (n)
    class(beam_structure_t), intent(in) :: beam_structure
    integer :: n
    n = beam_structure%n_beam
  end function beam_structure_get_n_beam

  function beam_structure_get_prt (beam_structure) result (prt)
    class(beam_structure_t), intent(in) :: beam_structure
    type(string_t), dimension(:), allocatable :: prt
    allocate (prt (size (beam_structure%prt)))
    prt = beam_structure%prt
  end function beam_structure_get_prt

  function beam_structure_get_n_record (beam_structure) result (n)
    class(beam_structure_t), intent(in) :: beam_structure
    integer :: n
    if (allocated (beam_structure%record)) then
       n = size (beam_structure%record)
    else
       n = 0
    end if
  end function beam_structure_get_n_record
  
  function beam_structure_get_i_entry (beam_structure, i) result (i_entry)
    class(beam_structure_t), intent(in) :: beam_structure
    integer, intent(in) :: i
    integer, dimension(:), allocatable :: i_entry
    associate (record => beam_structure%record(i))
      select case (size (record%entry))
      case (1)
         if (record%entry(1)%is_valid) then
            allocate (i_entry (2), source = [1, 2])
         else
            allocate (i_entry (0))
         end if
      case (2)
         if (all (record%entry%is_valid)) then
            allocate (i_entry (2), source = [1, 2])
         else if (record%entry(1)%is_valid) then
            allocate (i_entry (1), source = [1])
         else if (record%entry(2)%is_valid) then
            allocate (i_entry (1), source = [2])
         else
            allocate (i_entry (0))
         end if
      end select
    end associate
  end function beam_structure_get_i_entry
  
  function beam_structure_get_name (beam_structure, i) result (name)
    class(beam_structure_t), intent(in) :: beam_structure
    integer, intent(in) :: i
    type(string_t) :: name
    associate (record => beam_structure%record(i))
      if (record%entry(1)%is_valid) then
         name = record%entry(1)%name
      else if (size (record%entry) == 2) then
         name = record%entry(2)%name
      end if
    end associate
  end function beam_structure_get_name
  
  function beam_structure_contains (beam_structure, name) result (flag)
    class(beam_structure_t), intent(in) :: beam_structure
    character(*), intent(in) :: name
    logical :: flag
    integer :: i, j
    flag = .false.
    if (allocated (beam_structure%record)) then
       do i = 1, size (beam_structure%record)
          do j = 1, size (beam_structure%record(i)%entry)
             flag = beam_structure%record(i)%entry(j)%name == name
             if (flag)  return
          end do
       end do
    end if
  end function beam_structure_contains

  function beam_structure_polarized (beam_structure) result (flag)
    class(beam_structure_t), intent(in) :: beam_structure
    logical :: flag
    flag = allocated (beam_structure%smatrix)
  end function beam_structure_polarized
  
  function beam_structure_get_smatrix (beam_structure) result (smatrix)
    class(beam_structure_t), intent(in) :: beam_structure
    type(smatrix_t), dimension(:), allocatable :: smatrix
    allocate (smatrix (size (beam_structure%smatrix)), &
         source = beam_structure%smatrix)
  end function beam_structure_get_smatrix
  
  function beam_structure_get_pol_f (beam_structure) result (pol_f)
    class(beam_structure_t), intent(in) :: beam_structure
    real(default), dimension(:), allocatable :: pol_f
    allocate (pol_f (size (beam_structure%pol_f)), &
         source = beam_structure%pol_f)
  end function beam_structure_get_pol_f
  
  function beam_structure_asymmetric (beam_structure) result (flag)
    class(beam_structure_t), intent(in) :: beam_structure
    logical :: flag
    flag = allocated (beam_structure%p) &
         .or. allocated (beam_structure%theta) &
         .or. allocated (beam_structure%phi)
  end function beam_structure_asymmetric
  
  function beam_structure_get_momenta (beam_structure) result (p)
    class(beam_structure_t), intent(in) :: beam_structure
    type(vector3_t), dimension(:), allocatable :: p
    real(default), dimension(:), allocatable :: theta, phi
    integer :: n, i
    if (allocated (beam_structure%p)) then
       n = size (beam_structure%p)
       if (allocated (beam_structure%theta)) then
          if (size (beam_structure%theta) == n) then
             allocate (theta (n), source = beam_structure%theta)
          else
             call msg_fatal ("Beam structure: mismatch in momentum vs. &
                  &angle theta specification")
          end if
       else
          allocate (theta (n), source = 0._default)
       end if
       if (allocated (beam_structure%phi)) then
          if (size (beam_structure%phi) == n) then
             allocate (phi (n), source = beam_structure%phi)
          else
             call msg_fatal ("Beam structure: mismatch in momentum vs. &
                  &angle phi specification")
          end if
       else
          allocate (phi (n), source = 0._default)
       end if
       allocate (p (n))
       do i = 1, n
          p(i) = beam_structure%p(i) * vector3_moving ([ &
               sin (theta(i)) * cos (phi(i)), &
               sin (theta(i)) * sin (phi(i)), &
               cos (theta(i))])
       end do
       if (n == 2)  p(2) = - p(2)
    else
       call msg_fatal ("Beam structure: angle theta/phi specified but &
            &momentum/a p undefined")
    end if
  end function beam_structure_get_momenta
    

  subroutine beam_structures_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (beam_structures_1, "beam_structures_1", &
         "empty beam structure record", &
         u, results)
    call test (beam_structures_2, "beam_structures_2", &
         "beam structure records", &
         u, results)
    call test (beam_structures_3, "beam_structures_3", &
         "beam structure expansion", &
         u, results)
    call test (beam_structures_4, "beam_structures_4", &
         "beam structure contents", &
         u, results)
    call test (beam_structures_5, "beam_structures_5", &
         "polarization", &
         u, results)
    call test (beam_structures_6, "beam_structures_6", &
         "momenta", &
         u, results)
end subroutine beam_structures_test
  
  subroutine beam_structures_1 (u)
    integer, intent(in) :: u
    type(beam_structure_t) :: beam_structure
    
    write (u, "(A)")  "* Test output: beam_structures_1"
    write (u, "(A)")  "*   Purpose: display empty beam structure record"
    write (u, "(A)")

    call beam_structure%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: beam_structures_1"
    
  end subroutine beam_structures_1
  
  subroutine beam_structures_2 (u)
    integer, intent(in) :: u
    type(beam_structure_t) :: beam_structure
    integer, dimension(0) :: empty_array
    type(string_t) :: s
    
    write (u, "(A)")  "* Test output: beam_structures_2"
    write (u, "(A)")  "*   Purpose: setup beam structure records"
    write (u, "(A)")

    s = "s"

    call beam_structure%init_sf ([s], empty_array)
    call beam_structure%write (u)
    
    write (u, "(A)")

    call beam_structure%init_sf ([s, s], [1])
    call beam_structure%set_sf (1, 1, var_str ("a"))
    call beam_structure%write (u)
    
    write (u, "(A)")

    call beam_structure%init_sf ([s, s], [2])
    call beam_structure%set_sf (1, 1, var_str ("a"))
    call beam_structure%set_sf (1, 2, var_str ("b"))
    call beam_structure%write (u)
    
    write (u, "(A)")

    call beam_structure%init_sf ([s, s], [2, 1])
    call beam_structure%set_sf (1, 1, var_str ("a"))
    call beam_structure%set_sf (1, 2, var_str ("b"))
    call beam_structure%set_sf (2, 1, var_str ("c"))
    call beam_structure%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: beam_structures_2"
    
  end subroutine beam_structures_2
  
  function test_strfun_mode (name) result (n)
    type(string_t), intent(in) :: name
    integer :: n
    select case (char (name))
    case ("a");  n = 2
    case ("b");  n = 1
    case default;  n = 0
    end select
  end function test_strfun_mode

  subroutine beam_structures_3 (u)
    integer, intent(in) :: u
    type(beam_structure_t) :: beam_structure
    type(string_t) :: s
    
    write (u, "(A)")  "* Test output: beam_structures_3"
    write (u, "(A)")  "*   Purpose: expand beam structure records"
    write (u, "(A)")

    s = "s"

    write (u, "(A)")  "* Pair spectrum (keep as-is)"
    write (u, "(A)")

    call beam_structure%init_sf ([s, s], [1])
    call beam_structure%set_sf (1, 1, var_str ("a"))
    call beam_structure%write (u)

    write (u, "(A)")

    call beam_structure%expand (test_strfun_mode)
    call beam_structure%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Structure function pair (expand)"
    write (u, "(A)")

    call beam_structure%init_sf ([s, s], [2])
    call beam_structure%set_sf (1, 1, var_str ("b"))
    call beam_structure%set_sf (1, 2, var_str ("b"))
    call beam_structure%write (u)

    write (u, "(A)")

    call beam_structure%expand (test_strfun_mode)
    call beam_structure%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Structure function (separate and expand)"
    write (u, "(A)")

    call beam_structure%init_sf ([s, s], [1])
    call beam_structure%set_sf (1, 1, var_str ("b"))
    call beam_structure%write (u)

    write (u, "(A)")

    call beam_structure%expand (test_strfun_mode)
    call beam_structure%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Combination"
    write (u, "(A)")

    call beam_structure%init_sf ([s, s], [1, 1])
    call beam_structure%set_sf (1, 1, var_str ("a"))
    call beam_structure%set_sf (2, 1, var_str ("b"))
    call beam_structure%write (u)

    write (u, "(A)")

    call beam_structure%expand (test_strfun_mode)
    call beam_structure%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: beam_structures_3"
    
  end subroutine beam_structures_3
  
  subroutine beam_structures_4 (u)
    integer, intent(in) :: u
    type(beam_structure_t) :: beam_structure
    type(string_t) :: s
    type(string_t), dimension(2) :: prt
    integer :: i
    
    write (u, "(A)")  "* Test output: beam_structures_4"
    write (u, "(A)")  "*   Purpose: check the API"
    write (u, "(A)")

    s = "s"

    write (u, "(A)")  "* Structure-function combination"
    write (u, "(A)")

    call beam_structure%init_sf ([s, s], [1, 2, 2])
    call beam_structure%set_sf (1, 1, var_str ("a"))
    call beam_structure%set_sf (2, 1, var_str ("b"))
    call beam_structure%set_sf (3, 2, var_str ("c"))
    call beam_structure%write (u)

    write (u, *)
    write (u, "(1x,A,I0)")  "n_beam = ", beam_structure%get_n_beam ()
    prt = beam_structure%get_prt ()
    write (u, "(1x,A,2(1x,A))")  "prt =", char (prt(1)), char (prt(2))
    
    write (u, *)
    write (u, "(1x,A,I0)")  "n_record = ", beam_structure%get_n_record ()

    do i = 1, 3
       write (u, "(A)")
       write (u, "(1x,A,I0,A,A)")  "name(", i, ") = ", &
            char (beam_structure%get_name (i))
       write (u, "(1x,A,I0,A,2(1x,I0))")  "i_entry(", i, ") =", &
            beam_structure%get_i_entry (i)
    end do
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: beam_structures_4"
    
  end subroutine beam_structures_4
  
  subroutine beam_structures_5 (u)
    integer, intent(in) :: u
    type(beam_structure_t) :: beam_structure
    integer, dimension(0) :: empty_array
    type(string_t) :: s
    
    write (u, "(A)")  "* Test output: beam_structures_5"
    write (u, "(A)")  "*   Purpose: setup polarization in beam structure records"
    write (u, "(A)")

    s = "s"

    call beam_structure%init_sf ([s], empty_array)
    call beam_structure%init_pol (1)
    call beam_structure%init_smatrix (1, 1)
    call beam_structure%set_sentry (1, 1, [0,0], (1._default, 0._default))
    call beam_structure%set_pol_f ([0.5_default])
    call beam_structure%write (u)
    
    
    write (u, "(A)")
    call beam_structure%final_sf ()
    call beam_structure%final_pol ()

    call beam_structure%init_sf ([s, s], [1])
    call beam_structure%set_sf (1, 1, var_str ("a"))
    call beam_structure%init_pol (2)
    call beam_structure%init_smatrix (1, 2)
    call beam_structure%set_sentry (1, 1, [-1,1], (0.5_default,-0.5_default))
    call beam_structure%set_sentry (1, 2, [ 1,1], (1._default, 0._default))
    call beam_structure%init_smatrix (2, 0)
    call beam_structure%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: beam_structures_5"
    
  end subroutine beam_structures_5
  
  subroutine beam_structures_6 (u)
    integer, intent(in) :: u
    type(beam_structure_t) :: beam_structure
    integer, dimension(0) :: empty_array
    type(string_t) :: s
    
    write (u, "(A)")  "* Test output: beam_structures_6"
    write (u, "(A)")  "*   Purpose: setup momenta in beam structure records"
    write (u, "(A)")

    s = "s"

    call beam_structure%init_sf ([s], empty_array)
    call beam_structure%set_momentum ([500._default])
    call beam_structure%write (u)
    
    
    write (u, "(A)")
    call beam_structure%final_sf ()
    call beam_structure%final_mom ()

    call beam_structure%init_sf ([s, s], [1])
    call beam_structure%set_momentum ([500._default, 700._default])
    call beam_structure%set_theta ([0._default, 0.1_default])
    call beam_structure%set_phi ([0._default, 1.51_default])
    call beam_structure%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: beam_structures_6"
    
  end subroutine beam_structures_6
  

end module beam_structures
