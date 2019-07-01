! WHIZARD 2.2.4 Feb 06 2015
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

module xml

  use iso_varying_string, string_t => varying_string
  use io_units
  use unit_tests
  use system_defs, only: BLANK, TAB
  use diagnostics
  use ifiles
  use lexers

  implicit none
  private

  public :: cstream_t
  public :: xml_attribute
  public :: xml_tag_t
  public :: xml_test

  type, extends (stream_t) :: cstream_t
     logical :: cache_is_empty = .true.
     type(string_t) :: cache
   contains
     generic :: init => init_filename, init_unit, init_string, &
          init_ifile, init_line
     procedure :: init_filename => cstream_init_filename
     procedure :: init_unit => cstream_init_unit
     procedure :: init_string => cstream_init_string
     procedure :: init_ifile => cstream_init_ifile
     procedure :: init_line => cstream_init_line
     procedure :: final => cstream_final
     procedure :: get_record => cstream_get_record
     procedure :: revert_record => cstream_revert_record
  end type cstream_t
  
  type :: attribute_t
     type(string_t) :: name
     type(string_t) :: value
     logical :: known = .false.
   contains
     procedure :: write => attribute_write
     procedure :: set_value => attribute_set_value
     procedure :: get_value => attribute_get_value     
  end type attribute_t
  
  type :: xml_tag_t
     type(string_t) :: name
     type(attribute_t), dimension(:), allocatable :: attribute
     logical :: has_content = .false.
   contains
     generic :: init => init_no_attributes
     procedure :: init_no_attributes => tag_init_no_attributes
     generic :: init => init_with_attributes
     procedure :: init_with_attributes => tag_init_with_attributes
     procedure :: set_attribute => tag_set_attribute
     procedure :: get_attribute => tag_get_attribute
     generic :: write => write_without_content
     procedure :: write_without_content => tag_write
     procedure :: close => tag_close
     generic :: write => write_with_content
     procedure :: write_with_content => tag_write_with_content
     procedure :: read => tag_read
     procedure :: read_attribute => tag_read_attribute
     procedure :: read_content => tag_read_content
  end type xml_tag_t
  

contains

  subroutine cstream_init_filename (cstream, filename)
    class(cstream_t), intent(out) :: cstream
    character(*), intent(in) :: filename
    call stream_init (cstream%stream_t, filename)
  end subroutine cstream_init_filename

  subroutine cstream_init_unit (cstream, unit)
    class(cstream_t), intent(out) :: cstream
    integer, intent(in) :: unit
    call stream_init (cstream%stream_t, unit)
  end subroutine cstream_init_unit
    
  subroutine cstream_init_string (cstream, string)
    class(cstream_t), intent(out) :: cstream
    type(string_t), intent(in) :: string
    call stream_init (cstream%stream_t, string)
  end subroutine cstream_init_string
    
  subroutine cstream_init_ifile (cstream, ifile)
    class(cstream_t), intent(out) :: cstream
    type(ifile_t), intent(in) :: ifile
    call stream_init (cstream%stream_t, ifile)
  end subroutine cstream_init_ifile
    
  subroutine cstream_init_line (cstream, line)
    class(cstream_t), intent(out) :: cstream
    type(line_p), intent(in) :: line
    call stream_init (cstream%stream_t, line)
  end subroutine cstream_init_line
    
  subroutine cstream_final (cstream)
    class(cstream_t), intent(inout) :: cstream
    cstream%cache_is_empty = .true.
    call stream_final (cstream%stream_t)
  end subroutine cstream_final
  
  subroutine cstream_get_record (cstream, string, iostat)
    class(cstream_t), intent(inout) :: cstream
    type(string_t), intent(out) :: string
    integer, intent(out) :: iostat
    if (cstream%cache_is_empty) then
       call stream_get_record (cstream%stream_t, string, iostat)
    else
       string = cstream%cache
       cstream%cache_is_empty = .true.
       iostat = 0
    end if
  end subroutine cstream_get_record
  
  subroutine cstream_revert_record (cstream, string)
    class(cstream_t), intent(inout) :: cstream
    type(string_t), intent(in) :: string
    if (cstream%cache_is_empty) then
       cstream%cache = string
       cstream%cache_is_empty = .false.
    else
       call msg_bug ("CStream: attempt to revert twice")
    end if
  end subroutine cstream_revert_record
  
  subroutine attribute_write (object, unit)
    class(attribute_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "(A,'=')", advance = "no")  char (object%name)
    if (object%known) then
       write (u, "(A,A,A)", advance = "no")  '"', char (object%value), '"'
    else
       write (u, "('?')", advance = "no")
    end if
  end subroutine attribute_write
    
  function xml_attribute (name, value) result (attribute)
    type(string_t), intent(in) :: name
    type(string_t), intent(in), optional :: value
    type(attribute_t) :: attribute
    attribute%name = name
    if (present (value)) then
       attribute%value = value
       attribute%known = .true.
    else
       attribute%known = .false.
    end if
  end function xml_attribute

  subroutine attribute_set_value (attribute, value)
    class(attribute_t), intent(inout) :: attribute
    type(string_t), intent(in) :: value
    attribute%value = value
    attribute%known = .true.
  end subroutine attribute_set_value
  
  function attribute_get_value (attribute) result (value)
    class(attribute_t), intent(in) :: attribute
    type(string_t) :: value
    if (attribute%known) then
       value = attribute%value
    else
       value = "?"
    end if
  end function attribute_get_value
  
  subroutine tag_init_no_attributes (tag, name, has_content)
    class(xml_tag_t), intent(out) :: tag
    type(string_t), intent(in) :: name
    logical, intent(in), optional :: has_content
    tag%name = name
    allocate (tag%attribute (0))
    if (present (has_content))  tag%has_content = has_content
  end subroutine tag_init_no_attributes
  
  subroutine tag_init_with_attributes (tag, name, attribute, has_content)
    class(xml_tag_t), intent(out) :: tag
    type(string_t), intent(in) :: name
    type(attribute_t), dimension(:), intent(in) :: attribute
    logical, intent(in), optional :: has_content
    tag%name = name
    allocate (tag%attribute (size (attribute)))
    tag%attribute = attribute
    if (present (has_content))  tag%has_content = has_content
  end subroutine tag_init_with_attributes
  
  subroutine tag_set_attribute (tag, i, value)
    class(xml_tag_t), intent(inout) :: tag
    integer, intent(in) :: i
    type(string_t), intent(in) :: value
    call tag%attribute(i)%set_value (value)
  end subroutine tag_set_attribute
  
  function tag_get_attribute (tag, i) result (value)
    class(xml_tag_t), intent(in) :: tag
    integer, intent(in) :: i
    type(string_t) :: value
    value = tag%attribute(i)%get_value ()
  end function tag_get_attribute
  
  subroutine tag_write (tag, unit)
    class(xml_tag_t), intent(in) :: tag
    integer, intent(in), optional :: unit
    integer :: u, i
    u = given_output_unit (unit)
    write (u, "('<',A)", advance = "no")  char (tag%name)
    do i = 1, size (tag%attribute)
       write (u, "(1x)", advance = "no")
       call tag%attribute(i)%write (u)
    end do
    if (tag%has_content) then
       write (u, "('>')", advance = "no")
    else
       write (u, "(' />')", advance = "no")
    end if
  end subroutine tag_write
  
  subroutine tag_close (tag, unit)
    class(xml_tag_t), intent(in) :: tag
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "('</',A,'>')", advance = "no")  char (tag%name)
  end subroutine tag_close
    
  subroutine tag_write_with_content (tag, content, unit)
    class(xml_tag_t), intent(in) :: tag
    type(string_t), intent(in) :: content
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    call tag%write (u)
    write (u, "(A)", advance = "no")  char (content)
    call tag%close (u)
  end subroutine tag_write_with_content
  
  subroutine tag_read (tag, cstream, success)
    class(xml_tag_t), intent(inout) :: tag
    type(cstream_t), intent(inout) :: cstream
    logical, intent(out) :: success
    type(string_t) :: string
    integer :: iostat, p1, p2
    character(2), parameter :: WS = BLANK // TAB
    logical :: done

    ! Skip comments and blank lines
    FIND_NON_COMMENT: do
       FIND_NONEMPTY_RECORD: do
          call cstream%get_record (string, iostat)
          if (iostat /= 0)  call err_io ()
          p1 = verify (string, WS)
          if (p1 > 0)  exit FIND_NONEMPTY_RECORD
       end do FIND_NONEMPTY_RECORD

       ! Look for comment beginning
       p2 = p1 + 3
       if (extract (string, p1, p2) /= "<!--")  exit FIND_NON_COMMENT
       
       ! Look for comment end, then restart
       string = extract (string, p2 + 1)
       FIND_COMMENT_END: do
          do p1 = 1, len (string) - 2
             p2 = p1 + 2
             if (extract (string, p1, p2) == "-->") then

                ! Return trailing text to the stream
                string = extract (string, p2 + 1)
                if (string /= "")  call cstream%revert_record (string)
                exit FIND_COMMENT_END

             end if
          end do
          call cstream%get_record (string, iostat)
          if (iostat /= 0)  call err_io ()
       end do FIND_COMMENT_END
    end do FIND_NON_COMMENT

    ! Look for opening <
    p2 = p1
    if (extract (string, p1, p2) /= "<") then
       call cstream%revert_record (string)
       success = .false.;  return
    else

       ! Look for tag name
       string = extract (string, p2 + 1)
       p1 = verify (string, WS);  if (p1 == 0)  call err_incomplete ()
       p2 = p1 + len (tag%name) - 1
       if (extract (string, p1, p2) /= tag%name) then
          call cstream%revert_record ("<" // string)
          success = .false.;  return
       else

          ! Look for attributes
          string = extract (string, p2 + 1)
          READ_ATTRIBUTES: do
             call tag%read_attribute (string, done)
             if (done)  exit READ_ATTRIBUTES
          end do READ_ATTRIBUTES

          ! Look for closing >
          p1 = verify (string, WS);  if (p1 == 0)  call err_incomplete ()
          p2 = p1
          if (extract (string, p1, p1) == ">") then
             tag%has_content = .true.
          else

             ! Look for closing />
             p2 = p1 + 1
             if (extract (string, p1, p2) /= "/>")  call err_incomplete ()
          end if

          ! Return trailing text to the stream
          string = extract (string, p2 + 1) 
          if (string /= "")  call cstream%revert_record (string)
          success = .true.

       end if
    end if

  contains

    subroutine err_io ()
      select case (iostat)
      case (:-1)
         call msg_fatal ("XML: Error reading tag '" // char (tag%name) &
              // "': end of file")
      case (1:)
         call msg_fatal ("XML: Error reading tag '" // char (tag%name) &
              // "': I/O error")
      end select
      success = .false.
    end subroutine err_io
    
    subroutine err_incomplete ()
      call msg_fatal ("XML: Error reading tag '" // char (tag%name) &
           // "': tag incomplete")
      success = .false.
    end subroutine err_incomplete
    
  end subroutine tag_read

  subroutine tag_read_attribute (tag, string, done)
    class(xml_tag_t), intent(inout) :: tag
    type(string_t), intent(inout) :: string
    logical, intent(out) :: done
    character(2), parameter :: WS = BLANK // TAB
    type(string_t) :: name, value
    integer :: p1, p2, i
    
    p1 = verify (string, WS);  if (p1 == 0)  call err ()
    p2 = p1

    ! Look for first terminating '>' or '/>'
    if (extract (string, p1, p2) == ">") then
       done = .true.
    else
       p2 = p1 + 1
       if (extract (string, p1, p2) == "/>") then
          done = .true.
       else

          ! Look for '='
          p2 = scan (string, '=')
          if (p2 == 0)  call err ()
          name = trim (extract (string, p1, p2 - 1))
          
          ! Look for '"'
          string = extract (string, p2 + 1)
          p1 = verify (string, WS);  if (p1 == 0)  call err ()
          p2 = p1
          if (extract (string, p1, p2) /= '"')  call err ()

          ! Look for matching '"' and get value
          string = extract (string, p2 + 1)
          p1 = 1
          p2 = scan (string, '"')
          if (p2 == 0)  call err ()
          value = extract (string, p1, p2 - 1)

          SCAN_KNOWN_ATTRIBUTES: do i = 1, size (tag%attribute)
             if (name == tag%attribute(i)%name) then
                call tag%attribute(i)%set_value (value)
                exit SCAN_KNOWN_ATTRIBUTES
             end if
          end do SCAN_KNOWN_ATTRIBUTES
          
          string = extract (string, p2 + 1)
          done = .false.
       end if
    end if

  contains
    
    subroutine err ()
      call msg_fatal ("XML: Error reading attributes of '" // char (tag%name) &
           // "': syntax error")
    end subroutine err
    
  end subroutine tag_read_attribute
    
  subroutine tag_read_content (tag, cstream, content, closing)
    class(xml_tag_t), intent(in) :: tag
    type(cstream_t), intent(inout) :: cstream
    type(string_t), intent(out) :: content
    type(string_t) :: string
    logical, intent(out) :: closing
    integer :: iostat
    integer :: p0, p1, p2
    character(2), parameter :: WS = BLANK // TAB
    call cstream%get_record (content, iostat)
    if (iostat /= 0)  call err_io ()
    closing = .false.
    FIND_CLOSING: do p0 = 1, len (content) - 1

       ! Look for terminating </
       p1 = p0
       p2 = p1 + 1
       if (extract (content, p1, p2) == "</") then

          ! Look for closing tag name
          string = extract (content, p2 + 1)
          p1 = verify (string, WS);  if (p1 == 0)  call err_incomplete ()
          p2 = p1 + len (tag%name) - 1
          if (extract (string, p1, p2) == tag%name) then
             
             ! Tag name matches: look for final >
             string = extract (string, p2 + 1)
             p1 = verify (string, WS);  if (p1 == 0)  call err_incomplete ()
             p2 = p1
             if (extract (string, p1, p2) /= ">")  call err_incomplete ()

             ! Return trailing text to the stream
             string = extract (string, p2 + 1)
             if (string /= "")  call cstream%revert_record (string)
             content = extract (content, 1, p0 -1)
             closing = .true.
             exit FIND_CLOSING

          end if
       end if
    end do FIND_CLOSING
    
  contains
    
    subroutine err_io ()
      select case (iostat)
      case (:-1)
         call msg_fatal ("XML: Error reading content of '" // char (tag%name) &
              // "': end of file")
      case (1:)
         call msg_fatal ("XML: Error reading content of '" // char (tag%name) &
              // "': I/O error")
      end select
      closing = .false.
    end subroutine err_io
    
    subroutine err_incomplete ()
      call msg_fatal ("XML: Error reading content '" // char (tag%name) &
           // "': closing tag incomplete")
      closing = .false.
    end subroutine err_incomplete
    
  end subroutine tag_read_content
          

  subroutine xml_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (xml_1, "xml_1", &
         "basic I/O", &
         u, results)
    call test (xml_2, "xml_2", &
         "optional tag", &
         u, results)
    call test (xml_3, "xml_3", &
         "content", &
         u, results)
    call test (xml_4, "xml_4", &
         "attributes", &
         u, results)
end subroutine xml_test
  
  subroutine show (u_tmp, u)
    integer, intent(in) :: u_tmp, u
    character (80) :: buffer
    integer :: iostat
    write (u, "(A)")  "File content:"
    rewind (u_tmp)
    do
       read (u_tmp, "(A)", iostat = iostat)  buffer
       if (iostat /= 0)  exit
       write (u, "(A)")  trim (buffer)
    end do
    rewind (u_tmp)
  end subroutine show

  subroutine xml_1 (u)
    integer, intent(in) :: u
    type(xml_tag_t), allocatable :: tag
    integer :: u_tmp
    type(cstream_t) :: cstream
    logical :: success
    
    write (u, "(A)")  "* Test output: xml_1"
    write (u, "(A)")  "*   Purpose: write and read tag"
    write (u, "(A)")
    
    write (u, "(A)")  "* Empty tag"
    write (u, *)

    u_tmp = free_unit ()
    open (u_tmp, status = "scratch", action = "readwrite")

    allocate (tag)
    call tag%init (var_str ("tagname"))
    call tag%write (u_tmp)
    write (u_tmp, *)
    deallocate (tag)

    call show (u_tmp, u)

    write (u, *)
    write (u, "(A)")  "Result from read:"
    call cstream%init (u_tmp)
    allocate (tag)
    call tag%init (var_str ("tagname"))
    call tag%read (cstream, success)
    call tag%write (u)
    write (u, *)
    write (u, "(A,L1)")  "success = ", success
    deallocate (tag)
    call cstream%final ()
    
    write (u, *)
    write (u, "(A)")  "* Tag with preceding blank lines"
    write (u, *)

    u_tmp = free_unit ()
    open (u_tmp, status = "scratch", action = "readwrite")

    allocate (tag)
    call tag%init (var_str ("tagname"))
    write (u_tmp, *)
    write (u_tmp, "(A)")  "    "
    write (u_tmp, "(2x)", advance = "no")
    call tag%write (u_tmp)
    write (u_tmp, *)
    deallocate (tag)

    call show (u_tmp, u)

    write (u, *)
    write (u, "(A)")  "Result from read:"
    call cstream%init (u_tmp)
    allocate (tag)
    call tag%init (var_str ("tagname"))
    call tag%read (cstream, success)
    call tag%write (u)
    write (u, *)
    write (u, "(A,L1)")  "success = ", success
    deallocate (tag)
    call cstream%final ()
    
    write (u, *)
    write (u, "(A)")  "* Tag with preceding comments"
    write (u, *)

    u_tmp = free_unit ()
    open (u_tmp, status = "scratch", action = "readwrite")

    allocate (tag)
    call tag%init (var_str ("tagname"))
    write (u_tmp, "(A)")  "<!-- comment -->"
    write (u_tmp, *)
    write (u_tmp, "(A)")  "<!-- multiline"
    write (u_tmp, "(A)")  "     comment -->"
    call tag%write (u_tmp)
    write (u_tmp, *)
    deallocate (tag)

    call show (u_tmp, u)

    write (u, *)
    write (u, "(A)")  "Result from read:"
    call cstream%init (u_tmp)
    allocate (tag)
    call tag%init (var_str ("tagname"))
    call tag%read (cstream, success)
    call tag%write (u)
    write (u, *)
    write (u, "(A,L1)")  "success = ", success
    deallocate (tag)
    call cstream%final ()
    
    write (u, *)
    write (u, "(A)")  "* Tag with name mismatch"
    write (u, *)

    u_tmp = free_unit ()
    open (u_tmp, status = "scratch", action = "readwrite")

    allocate (tag)
    call tag%init (var_str ("wrongname"))
    call tag%write (u_tmp)
    write (u_tmp, *)
    deallocate (tag)

    call show (u_tmp, u)

    write (u, *)
    write (u, "(A)")  "Result from read:"
    call cstream%init (u_tmp)
    allocate (tag)
    call tag%init (var_str ("tagname"))
    call tag%read (cstream, success)
    call tag%write (u)
    write (u, *)
    write (u, "(A,L1)")  "success = ", success
    deallocate (tag)
    call cstream%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: xml_1"

  end subroutine xml_1

  subroutine xml_2 (u)
    integer, intent(in) :: u
    type(xml_tag_t), allocatable :: tag1, tag2
    integer :: u_tmp
    type(cstream_t) :: cstream
    logical :: success

    write (u, "(A)")  "* Test output: xml_2"
    write (u, "(A)")  "*   Purpose: handle optional tag"
    write (u, "(A)")
    
    write (u, "(A)")  "* Optional tag present"
    write (u, *)
    
    u_tmp = free_unit ()
    open (u_tmp, status = "scratch", action = "readwrite")

    allocate (tag1)
    call tag1%init (var_str ("option"))
    call tag1%write (u_tmp)
    write (u_tmp, *)
    allocate (tag2)
    call tag2%init (var_str ("tagname"))
    call tag2%write (u_tmp)
    write (u_tmp, *)
    deallocate (tag1, tag2)

    call show (u_tmp, u)

    write (u, *)
    write (u, "(A)")  "Result from read:"
    call cstream%init (u_tmp)
    allocate (tag1)
    call tag1%init (var_str ("option"))
    call tag1%read (cstream, success)
    call tag1%write (u)
    write (u, *)
    write (u, "(A,L1)")  "success = ", success
    write (u, *)
    allocate (tag2)
    call tag2%init (var_str ("tagname"))
    call tag2%read (cstream, success)
    call tag2%write (u)
    write (u, *)
    write (u, "(A,L1)")  "success = ", success
    deallocate (tag1, tag2)
    call cstream%final ()
    
    write (u, *)
    write (u, "(A)")  "* Optional tag absent"
    write (u, *)
    
    u_tmp = free_unit ()
    open (u_tmp, status = "scratch", action = "readwrite")

    allocate (tag2)
    call tag2%init (var_str ("tagname"))
    call tag2%write (u_tmp)
    write (u_tmp, *)
    deallocate (tag2)

    call show (u_tmp, u)

    write (u, *)
    write (u, "(A)")  "Result from read:"
    call cstream%init (u_tmp)
    allocate (tag1)
    call tag1%init (var_str ("option"))
    call tag1%read (cstream, success)
    call tag1%write (u)
    write (u, *)
    write (u, "(A,L1)")  "success = ", success
    write (u, *)
    allocate (tag2)
    call tag2%init (var_str ("tagname"))
    call tag2%read (cstream, success)
    call tag2%write (u)
    write (u, *)
    write (u, "(A,L1)")  "success = ", success
    deallocate (tag1, tag2)
    call cstream%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: xml_2"

  end subroutine xml_2

  subroutine xml_3 (u)
    integer, intent(in) :: u
    type(xml_tag_t), allocatable :: tag
    integer :: u_tmp
    type(cstream_t) :: cstream
    logical :: success, closing
    type(string_t) :: content

    write (u, "(A)")  "* Test output: xml_3"
    write (u, "(A)")  "*   Purpose: handle tag with content"
    write (u, "(A)")
    
    write (u, "(A)")  "* Tag without content"
    write (u, *)
    
    u_tmp = free_unit ()
    open (u_tmp, status = "scratch", action = "readwrite")

    allocate (tag)
    call tag%init (var_str ("tagname"))
    call tag%write (u_tmp)
    write (u_tmp, *)
    deallocate (tag)

    call show (u_tmp, u)

    write (u, *)
    write (u, "(A)")  "Result from read:"
    call cstream%init (u_tmp)
    allocate (tag)
    call tag%init (var_str ("tagname"))
    call tag%read (cstream, success)
    call tag%write (u)
    write (u, *)
    write (u, "(A,L1)")  "success = ", success
    write (u, "(A,L1)")  "content = ", tag%has_content
    write (u, *)
    deallocate (tag)
    call cstream%final ()
    
    write (u, "(A)")  "* Tag with content"
    write (u, *)
    
    u_tmp = free_unit ()
    open (u_tmp, status = "scratch", action = "readwrite")

    allocate (tag)
    call tag%init (var_str ("tagname"), has_content = .true.)
    call tag%write (var_str ("Content text"), u_tmp)
    write (u_tmp, *)
    deallocate (tag)

    call show (u_tmp, u)

    write (u, *)
    write (u, "(A)")  "Result from read:"
    call cstream%init (u_tmp)
    allocate (tag)
    call tag%init (var_str ("tagname"))
    call tag%read (cstream, success)
    call tag%read_content (cstream, content, closing)
    call tag%write (u)
    write (u, "(A)", advance = "no")  char (content)
    call tag%close (u)
    write (u, *)
    write (u, "(A,L1)")  "success = ", success
    write (u, "(A,L1)")  "content = ", tag%has_content
    write (u, "(A,L1)")  "closing = ", closing
    deallocate (tag)
    call cstream%final ()
    
    write (u, *)
    write (u, "(A)")  "* Tag with multiline content"
    write (u, *)
    
    u_tmp = free_unit ()
    open (u_tmp, status = "scratch", action = "readwrite")

    allocate (tag)
    call tag%init (var_str ("tagname"), has_content = .true.)
    call tag%write (u_tmp)
    write (u_tmp, *)
    write (u_tmp, "(A)")  "Line 1"
    write (u_tmp, "(A)")  "Line 2"
    call tag%close (u_tmp)
    write (u_tmp, *)
    deallocate (tag)

    call show (u_tmp, u)

    write (u, *)
    write (u, "(A)")  "Result from read:"
    call cstream%init (u_tmp)
    allocate (tag)
    call tag%init (var_str ("tagname"))
    call tag%read (cstream, success)
    call tag%write (u)
    write (u, *)
    do
       call tag%read_content (cstream, content, closing)
       if (closing)  exit
       write (u, "(A)")  char (content)
    end do
    call tag%close (u)
    write (u, *)
    write (u, "(A,L1)")  "success = ", success
    write (u, "(A,L1)")  "content = ", tag%has_content
    deallocate (tag)
    call cstream%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: xml_3"

  end subroutine xml_3

  subroutine xml_4 (u)
    integer, intent(in) :: u
    type(xml_tag_t), allocatable :: tag
    integer :: u_tmp
    type(cstream_t) :: cstream
    logical :: success
    
    write (u, "(A)")  "* Test output: xml_4"
    write (u, "(A)")  "*   Purpose: handle tag with attributes"
    write (u, "(A)")
    
    write (u, "(A)")  "* Tag with one mandatory and one optional attribute,"
    write (u, "(A)")  "* unknown attribute ignored"
    write (u, *)

    u_tmp = free_unit ()
    open (u_tmp, status = "scratch", action = "readwrite")

    allocate (tag)
    call tag%init (var_str ("tagname"), &
         [xml_attribute (var_str ("a1"), var_str ("foo")), &
          xml_attribute (var_str ("a3"), var_str ("gee"))])
    call tag%write (u_tmp)
    deallocate (tag)

    call show (u_tmp, u)

    write (u, *)
    write (u, "(A)")  "Result from read:"
    call cstream%init (u_tmp)
    allocate (tag)
    call tag%init (var_str ("tagname"), &
         [xml_attribute (var_str ("a1")), &
          xml_attribute (var_str ("a2"), var_str ("bar"))])
    call tag%read (cstream, success)
    call tag%write (u)
    write (u, *)
    deallocate (tag)
    call cstream%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: xml_4"

  end subroutine xml_4


end module xml
