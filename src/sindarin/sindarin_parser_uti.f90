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

module sindarin_parser_uti
  
    use iso_varying_string, string_t => varying_string
    use io_units
    use lexers
    use syntax_rules
    use parser
    use codes
    use object_base
    use object_builder
    use sindarin_parser

  implicit none
  private

  public :: sindarin_parser_1
  public :: sindarin_parser_2
  public :: sindarin_parser_3
  public :: sindarin_parser_4
  public :: sindarin_parser_5
  public :: sindarin_parser_6
  public :: sindarin_parser_7

contains
  
  subroutine sindarin_parser_1 (u)
    integer, intent(in) :: u

    write (u, "(A)")  "* Test output: sindarin_parser_1"
    write (u, "(A)")  "*   Purpose: build syntax table"
    write (u, "(A)")      
    
    call syntax_sindarin_init ()
    call syntax_sindarin_write (u)
    call syntax_sindarin_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sindarin_parser_1"
    
  end subroutine sindarin_parser_1

  subroutine sindarin_parser_2 (u)
    integer, intent(in) :: u
    type(repository_t) :: repository
    type(string_t), dimension(:), allocatable :: name
    integer :: i

    write (u, "(A)")  "* Test output: sindarin_parser_2"
    write (u, "(A)")  "*   Purpose: build Sindarin repository"
    write (u, "(A)")      
    
    call make_sindarin_repository (repository)
    call repository%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Name array"
    write (u, "(A)")
    
    call repository%get_prototype_names (name)
    do i = 1, size (name)
       write (u, "(I0,1x,A)")  i, char (name (i))
    end do

    call repository%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sindarin_parser_2"
    
  end subroutine sindarin_parser_2

  subroutine sindarin_parser_3 (u)
    integer, intent(in) :: u
    type(lexer_t) :: lexer
    type(stream_t), target :: stream
    type(parse_tree_t) :: parse_tree
    type(sindarin_decoder_t) :: decoder
    type(repository_t), allocatable :: repository
    type(string_t), dimension(:), allocatable :: prototype_names
    type(object_builder_t) :: builder
    type(code_t) :: code
    class(object_t), pointer :: main
    logical :: success
    integer :: u_sin, u_pt, u_code, iostat
    character(80) :: buffer

    write (u, "(A)")  "* Test output: sindarin_parser_3"
    write (u, "(A)")  "*   Purpose: parse a simple script"
    write (u, "(A)")      
    
    call syntax_sindarin_init ()

    allocate (repository)
    call make_sindarin_repository (repository)

    write (u, "(A)")  "* Create script"
    write (u, "(A)")      

    u_sin = free_unit ()
    open (u_sin, status="scratch")

    write (u_sin, "(A)")  "logical a"
    write (u_sin, "(A)")  "logical b = true"
    write (u_sin, "(A)")  "a = b and not false"          ! = true 
    write (u_sin, "(A)")  "b = (a or true) and (not b)"  ! = false
    write (u_sin, "(A)")  "a = true and true and true"   ! = true
    
    rewind (u_sin)
    do
       read (u_sin, "(A)", end=1)  buffer
       write (u, "(A)") trim (buffer)
    end do
1   continue
    
    rewind (u_sin)

    write (u, "(A)")      
    write (u, "(A)")  "* Parse script"
    write (u, "(A)")      

    call lexer%init ( &
         comment_chars = "", &
         quote_chars = '', &
         quote_match = '', &
         single_chars = "()", &
         special_class = [ "=" ] , &
         keyword_list = syntax_get_keyword_list_ptr (syntax_sindarin))

    call stream%init (u_sin)
    call lexer%assign_stream (stream)

    call parse_tree%parse (syntax_sindarin, lexer)

    write (u, "(A)")  "* Setup decoder (see 'sindarin_parser_3.pt.dat')"
    write (u, "(A)")
    
    call repository%get_prototype_names (prototype_names)

    call decoder%init (parse_tree, prototype_names)

    u_pt = free_unit ()
    open (u_pt, file="sindarin_parser_3.pt.dat", &
         status="replace", action="readwrite")
    call decoder%write (u_pt)
    close (u_pt)

    write (u, "(A)")  "* Decode (see 'sindarin_parser_3.code.dat')"
    write (u, "(A)")
    
    u_code = free_unit ()
    open (u_code, file="sindarin_parser_3.code.dat", &
         status="replace", action="readwrite")
    
    do
       call decoder%decode (code, success)
       if (.not. success)  exit
       call code%write (u_code)
    end do
    
    write (u, "(A)")  "* Create object tree"
    write (u, "(A)")
    
    rewind (u_code)

    call builder%import_repository (repository)
    call builder%init_empty ()

    do
       call code%read (u_code, iostat=iostat)
       if (iostat /= 0)  exit
       call builder%build (code, success)
       if (.not. success)  exit
    end do
    close (u_code)

    call builder%export (main)
    
    call main%write (u)
    call remove_object (main)

    write (u, "(A)")      
    write (u, "(A)")  "* Cleanup"
    
    call builder%final ()
    call parse_tree%final ()

    close (u_sin)
    close (u_code)
    call stream%final ()
    call lexer%final ()

    call syntax_sindarin_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sindarin_parser_3"
    
  end subroutine sindarin_parser_3

  subroutine sindarin_parser_4 (u)
    integer, intent(in) :: u
    type(lexer_t) :: lexer
    type(stream_t), target :: stream
    type(parse_tree_t) :: parse_tree
    type(sindarin_decoder_t) :: decoder
    type(repository_t), allocatable :: repository
    type(string_t), dimension(:), allocatable :: prototype_names
    type(object_builder_t) :: builder
    type(code_t) :: code
    class(object_t), pointer :: main
    logical :: success
    integer :: u_sin, u_pt, u_code, iostat
    character(80) :: buffer

    write (u, "(A)")  "* Test output: sindarin_parser_4"
    write (u, "(A)")  "*   Purpose: parse a simple script"
    write (u, "(A)")      
    
    call syntax_sindarin_init ()

    allocate (repository)
    call make_sindarin_repository (repository)

    write (u, "(A)")  "* Create script"
    write (u, "(A)")      

    u_sin = free_unit ()
    open (u_sin, status="scratch")
    
    write (u_sin, "(A)")  "integer a"
    write (u_sin, "(A)")  "integer b = -42"
    write (u_sin, "(A)")  "a = b / (7 - 1)"      ! = -7 
    write (u_sin, "(A)")  "b = -b * 2 + a"       ! = 77
    write (u_sin, "(A)")  "a = 1 + 2 - 3"        ! = 0
    
    rewind (u_sin)
    do
       read (u_sin, "(A)", end=1)  buffer
       write (u, "(A)") trim (buffer)
    end do
1   continue
    
    rewind (u_sin)

    write (u, "(A)")      
    write (u, "(A)")  "* Parse script"
    write (u, "(A)")      

    call lexer%init ( &
         comment_chars = "", &
         quote_chars = '', &
         quote_match = '', &
         single_chars = "()", &
         special_class = [ &
         "=   " , &
         "+-*/" &
         ] , &
         keyword_list = syntax_get_keyword_list_ptr (syntax_sindarin))

    call stream%init (u_sin)
    call lexer%assign_stream (stream)

    call parse_tree%parse (syntax_sindarin, lexer)

    write (u, "(A)")  "* Setup decoder (see 'sindarin_parser_4.pt.dat')"
    write (u, "(A)")
    
    call repository%get_prototype_names (prototype_names)

    call decoder%init (parse_tree, prototype_names)

    u_pt = free_unit ()
    open (u_pt, file="sindarin_parser_4.pt.dat", &
         status="replace", action="readwrite")
    call decoder%write (u_pt)
    close (u_pt)

    write (u, "(A)")  "* Decode (see 'sindarin_parser_4.code.dat')"
    write (u, "(A)")
    
    u_code = free_unit ()
    open (u_code, file="sindarin_parser_4.code.dat", &
         status="replace", action="readwrite")
    
    do
       call decoder%decode (code, success)
       if (.not. success)  exit
       call code%write (u_code)
    end do
    
    write (u, "(A)")  "* Create object tree"
    write (u, "(A)")
    
    rewind (u_code)

    call builder%import_repository (repository)
    call builder%init_empty ()

    do
       call code%read (u_code, iostat=iostat)
       if (iostat /= 0)  exit
       call builder%build (code, success)
       if (.not. success)  exit
    end do
    close (u_code)

    call builder%export (main)
    
    call main%write (u)
    call remove_object (main)

    write (u, "(A)")      
    write (u, "(A)")  "* Cleanup"
    
    call builder%final ()
    call parse_tree%final ()

    close (u_sin)
    close (u_code)
    call stream%final ()
    call lexer%final ()

    call syntax_sindarin_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sindarin_parser_4"
    
  end subroutine sindarin_parser_4

  subroutine sindarin_parser_5 (u)
    integer, intent(in) :: u
    type(lexer_t) :: lexer
    type(stream_t), target :: stream
    type(parse_tree_t) :: parse_tree
    type(sindarin_decoder_t) :: decoder
    type(repository_t), allocatable :: repository
    type(string_t), dimension(:), allocatable :: prototype_names
    type(object_builder_t) :: builder
    type(code_t) :: code
    class(object_t), pointer :: main
    logical :: success
    integer :: u_sin, u_pt, u_code, iostat
    character(80) :: buffer

    write (u, "(A)")  "* Test output: sindarin_parser_5"
    write (u, "(A)")  "*   Purpose: parse a simple script"
    write (u, "(A)")      
    
    call syntax_sindarin_init ()

    allocate (repository)
    call make_sindarin_repository (repository)

    write (u, "(A)")  "* Create script"
    write (u, "(A)")      

    u_sin = free_unit ()
    open (u_sin, status="scratch")
    
    write (u_sin, "(A)")  "logical a"
    write (u_sin, "(A)")  "integer i = 42"
    write (u_sin, "(A)")  "a = i == 42"        ! = true
    write (u_sin, "(A)")  "a = 13 < i <= 42"   ! = true
    write (u_sin, "(A)")  "a = a and i < 0"    ! = false
    
    rewind (u_sin)
    do
       read (u_sin, "(A)", end=1)  buffer
       write (u, "(A)") trim (buffer)
    end do
1   continue
    
    rewind (u_sin)

    write (u, "(A)")      
    write (u, "(A)")  "* Parse script"
    write (u, "(A)")      

    call lexer%init ( &
         comment_chars = "", &
         quote_chars = '', &
         quote_match = '', &
         single_chars = "()", &
         special_class = [ &
         "=<> " , &
         "+-*/" &
         ] , &
         keyword_list = syntax_get_keyword_list_ptr (syntax_sindarin))

    call stream%init (u_sin)
    call lexer%assign_stream (stream)

    call parse_tree%parse (syntax_sindarin, lexer)

    write (u, "(A)")  "* Setup decoder (see 'sindarin_parser_5.pt.dat')"
    write (u, "(A)")
    
    call repository%get_prototype_names (prototype_names)

    call decoder%init (parse_tree, prototype_names)

    u_pt = free_unit ()
    open (u_pt, file="sindarin_parser_5.pt.dat", &
         status="replace", action="readwrite")
    call decoder%write (u_pt)
    close (u_pt)

    write (u, "(A)")  "* Decode (see 'sindarin_parser_5.code.dat')"
    write (u, "(A)")
    
    u_code = free_unit ()
    open (u_code, file="sindarin_parser_5.code.dat", &
         status="replace", action="readwrite")
    
    do
       call decoder%decode (code, success)
       if (.not. success)  exit
       call code%write (u_code)
    end do
    
    write (u, "(A)")  "* Create object tree"
    write (u, "(A)")
    
    rewind (u_code)

    call builder%import_repository (repository)
    call builder%init_empty ()

    do
       call code%read (u_code, iostat=iostat)
       if (iostat /= 0)  exit
       call builder%build (code, success)
       if (.not. success)  exit
    end do
    close (u_code)

    call builder%export (main)
    
    call main%write (u)
    call remove_object (main)

    write (u, "(A)")      
    write (u, "(A)")  "* Cleanup"
    
    call builder%final ()
    call parse_tree%final ()

    close (u_sin)
    close (u_code)
    call stream%final ()
    call lexer%final ()

    call syntax_sindarin_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sindarin_parser_5"
    
  end subroutine sindarin_parser_5

  subroutine sindarin_parser_6 (u)
    integer, intent(in) :: u
    type(lexer_t) :: lexer
    type(stream_t), target :: stream
    type(parse_tree_t) :: parse_tree
    type(sindarin_decoder_t) :: decoder
    type(repository_t), allocatable :: repository
    type(string_t), dimension(:), allocatable :: prototype_names
    type(object_builder_t) :: builder
    type(code_t) :: code
    class(object_t), pointer :: main
    logical :: success
    integer :: u_sin, u_pt, u_code, iostat
    character(80) :: buffer

    write (u, "(A)")  "* Test output: sindarin_parser_6"
    write (u, "(A)")  "*   Purpose: parse a simple script"
    write (u, "(A)")      
    
    call syntax_sindarin_init ()

    allocate (repository)
    call make_sindarin_repository (repository)

    write (u, "(A)")  "* Create script"
    write (u, "(A)")      

    u_sin = free_unit ()
    open (u_sin, status="scratch")
    
    write (u_sin, "(A)")  "integer i = if true then 22 endif"
    write (u_sin, "(A)")  "integer j = if false then 23 else 34 endif"
    write (u_sin, "(A)")  "integer k = if false then 24 elsif true then 35&
         & endif"
    write (u_sin, "(A)")  "integer l = if false then 25 elsif false then 36&
         & elsif false then 45 else 56 endif"
    
    rewind (u_sin)
    do
       read (u_sin, "(A)", end=1)  buffer
       write (u, "(A)") trim (buffer)
    end do
1   continue
    
    rewind (u_sin)

    write (u, "(A)")      
    write (u, "(A)")  "* Parse script"
    write (u, "(A)")      

    call lexer%init ( &
         comment_chars = "", &
         quote_chars = '', &
         quote_match = '', &
         single_chars = "()", &
         special_class = [ &
         "=<> " , &
         "+-*/" &
         ] , &
         keyword_list = syntax_get_keyword_list_ptr (syntax_sindarin))

    call stream%init (u_sin)
    call lexer%assign_stream (stream)

    call parse_tree%parse (syntax_sindarin, lexer)

    write (u, "(A)")  "* Setup decoder (see 'sindarin_parser_6.pt.dat')"
    write (u, "(A)")
    
    call repository%get_prototype_names (prototype_names)

    call decoder%init (parse_tree, prototype_names)

    u_pt = free_unit ()
    open (u_pt, file="sindarin_parser_6.pt.dat", &
         status="replace", action="readwrite")
    call decoder%write (u_pt)
    close (u_pt)

    write (u, "(A)")  "* Decode (see 'sindarin_parser_6.code.dat')"
    write (u, "(A)")
    
    u_code = free_unit ()
    open (u_code, file="sindarin_parser_6.code.dat", &
         status="replace", action="readwrite")
    
    do
       call decoder%decode (code, success)
       if (.not. success)  exit
       call code%write (u_code)
    end do
    
    write (u, "(A)")  "* Create object tree"
    write (u, "(A)")
    
    rewind (u_code)

    call builder%import_repository (repository)
    call builder%init_empty ()

    do
       call code%read (u_code, iostat=iostat)
       if (iostat /= 0)  exit
       call builder%build (code, success)
       if (.not. success)  exit
    end do
    close (u_code)

    call builder%export (main)
    
    call main%write (u)
    call remove_object (main)

    write (u, "(A)")      
    write (u, "(A)")  "* Cleanup"
    
    call builder%final ()
    call parse_tree%final ()

    close (u_sin)
    close (u_code)
    call stream%final ()
    call lexer%final ()

    call syntax_sindarin_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sindarin_parser_6"
    
  end subroutine sindarin_parser_6

  subroutine sindarin_parser_7 (u)
    integer, intent(in) :: u
    type(lexer_t) :: lexer
    type(stream_t), target :: stream
    type(parse_tree_t) :: parse_tree
    type(sindarin_decoder_t) :: decoder
    type(repository_t), allocatable :: repository
    type(string_t), dimension(:), allocatable :: prototype_names
    type(object_builder_t) :: builder
    type(code_t) :: code
    class(object_t), pointer :: main
    logical :: success
    integer :: u_sin, u_pt, u_code, iostat
    character(80) :: buffer

    write (u, "(A)")  "* Test output: sindarin_parser_7"
    write (u, "(A)")  "*   Purpose: parse a simple script"
    write (u, "(A)")      
    
    call syntax_sindarin_init ()

    allocate (repository)
    call make_sindarin_repository (repository)

    write (u, "(A)")  "* Create script"
    write (u, "(A)")      

    u_sin = free_unit ()
    open (u_sin, status="scratch")
    
    write (u_sin, "(A)")  "a = 1, 2, 3"          
    write (u_sin, "(A)")  "b = 1:2, 3"   
    write (u_sin, "(A)")  "c = 1, 2:3"
    write (u_sin, "(A)")  "d = (1, 2):3"
    write (u_sin, "(A)")  "e = 1, 2 => 3, 4:5, 6 => 7"
    
    rewind (u_sin)
    do
       read (u_sin, "(A)", end=1)  buffer
       write (u, "(A)") trim (buffer)
    end do
1   continue
    
    rewind (u_sin)

    write (u, "(A)")      
    write (u, "(A)")  "* Parse script"
    write (u, "(A)")      

    call lexer%init ( &
         comment_chars = "", &
         quote_chars = '', &
         quote_match = '', &
         single_chars = "(),:", &
         special_class = [ &
         "=<> " , &
         "+-*/" &
         ] , &
         keyword_list = syntax_get_keyword_list_ptr (syntax_sindarin))

    call stream%init (u_sin)
    call lexer%assign_stream (stream)

    call parse_tree%parse (syntax_sindarin, lexer)

    write (u, "(A)")  "* Setup decoder (see 'sindarin_parser_7.pt.dat')"
    write (u, "(A)")
    
    call repository%get_prototype_names (prototype_names)

    call decoder%init (parse_tree, prototype_names)

    u_pt = free_unit ()
    open (u_pt, file="sindarin_parser_7.pt.dat", &
         status="replace", action="readwrite")
    call decoder%write (u_pt)
    close (u_pt)

    write (u, "(A)")  "* Decode (see 'sindarin_parser_7.code.dat')"
    write (u, "(A)")
    
    u_code = free_unit ()
    open (u_code, file="sindarin_parser_7.code.dat", &
         status="replace", action="readwrite")
    
    do
       call decoder%decode (code, success)
       if (.not. success)  exit
       call code%write (u_code)
    end do
    
    write (u, "(A)")  "* Create object tree"
    write (u, "(A)")
    
    rewind (u_code)

    call builder%import_repository (repository)
    call builder%init_empty ()

    do
       call code%read (u_code, iostat=iostat)
       if (iostat /= 0)  exit
       call builder%build (code, success)
       if (.not. success)  exit
    end do
    close (u_code)

    call builder%export (main)
    
    call main%write (u)
    call remove_object (main)

    write (u, "(A)")      
    write (u, "(A)")  "* Cleanup"
    
    call builder%final ()
    call parse_tree%final ()

    close (u_sin)
    close (u_code)
    call stream%final ()
    call lexer%final ()

    call syntax_sindarin_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sindarin_parser_7"
    
  end subroutine sindarin_parser_7


end module sindarin_parser_uti
