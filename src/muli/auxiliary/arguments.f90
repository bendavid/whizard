!!! module: arguments_module
!!! This code is part of my Ph.D studies.
!!! 
!!! Copyright (C) 2011 Hans-Werner Boschmann <boschmann@tp1.physik.uni-siegen.de>
!!! 
!!! This program is free software; you can redistribute it and/or modify it
!!! under the terms of the GNU General Public License as published by the Free 
!!! Software Foundation; either version 3 of the License, or (at your option) 
!!! any later version.
!!! 
!!! This program is distributed in the hope that it will be useful, but WITHOUT
!!! ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
!!! FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
!!! more details.
!!! 
!!! You should have received a copy of the GNU General Public License along
!!! with this program; if not, see <http://www.gnu.org/licenses/>.
!!! 
!!! Latest Change: 2011-06-29 16:11:41 CEST(+0200)
!!! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module arguments
  use,intrinsic::iso_fortran_env
  use kinds
  implicit none

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Intrinsic Type Module Component Declaration !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  integer,private,parameter::indent_len=30
  logical::arguments_stop_at_unrecognized_argument=.true.
  logical::arguments_stop_at_invalid_argument=.true.
  logical::arguments_stop_at_unrecognized_option=.true.
  logical::arguments_stop_at_invalid_option=.true.
  logical::arguments_revert_invalid_to_default=.true.

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Derived Type Definition !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  type::string_filo_stack_type
     character(:),allocatable::value
     class(string_filo_stack_type),pointer::next=>null()
   contains
     procedure::push_string=>string_filo_stack_push_string
     procedure::push_filo_stack=>string_filo_stack_push_filo_stack
     procedure::finalize=>string_filo_stack_finalize
     procedure::contains=>string_filo_stack_contains
     procedure::write_contents=>string_filo_stack_write_contents
     procedure::write_tail=>string_filo_stack_write_tail
     procedure::assign=>string_filo_stack_assign
     generic::push=>push_string,push_filo_stack
  end type string_filo_stack_type

  type,extends(string_filo_stack_type)::string_list_type
     class(string_filo_stack_type),pointer::last=>null()
   contains
     procedure::push_string=>string_list_push_string
     procedure::push_filo_stack=>string_list_push_filo_stack
     procedure::initialize=>string_list_initialize
     procedure::finalize=>string_list_finalize
     procedure::string_list_append_string
     procedure::string_list_append_filo_stack
     procedure::string_list_append_list
     procedure::get_rightmost=>string_list_get_rightmost
     generic::append=>string_list_append_string,string_list_append_filo_stack
  end type string_list_type

  type,abstract :: argument_class
     private
     character::short_form=""
     character(:),allocatable::long_form,named_option
     class(string_list_type),pointer::description=>null()
     integer::long_form_length=0
     integer::named_option_length=0
     logical::is_given_comp=.false.
     logical::is_default=.true.
     logical::is_valid=.true.
     logical::with_option=.false.
     logical::has_short_form=.false.
     logical::has_long_form=.false.
   contains
     procedure::is_given=>argument_is_given
     procedure::get_a_form=>argument_get_a_form
     procedure::compare_short=>argument_compare_short
     procedure::compare_long=>argument_compare_long
     procedure::argument_initialize
     procedure::argument_finalize
     procedure::write_description=>argument_write_description
     procedure(read_option_interface),deferred::read_option
     procedure::write_to_unit=>argument_write_to_unit
  end type argument_class

  type::argument_list_type
     class(argument_list_type),pointer::next=>null()
     class(argument_class),pointer::argument=>null()
   contains
     procedure::push=>argument_list_push
     procedure::finalize=>argument_list_finalize
     procedure::process=>argument_list_process
     procedure::process_long=>argument_list_process_long
     procedure::process_short=>argument_list_process_short
     procedure::write_description=>argument_list_write_description
  end type argument_list_type

  type,extends(argument_class) :: switch_argument_type
     logical::default_value=.true.
     logical::actual_value=.true.
     class(switch_argument_type),pointer::disable_arg=>null()
   contains
     procedure::read_option=>switch_argument_read_option
     procedure::switch_argument_initialize
     procedure::compare_short=>switch_argument_compare_short
     procedure::compare_long=>switch_argument_compare_long
     procedure::negates=>switch_argument_negates
     generic::initialize=>argument_initialize,switch_argument_initialize
  end type switch_argument_type

  type,extends(argument_class) :: real_argument_type
     real(kind=double)::default_value=0D0
     real(kind=double)::actual_value=0D0
     real(kind=double)::min_value=0D0
     real(kind=double)::max_value=1D0
     real(kind=double),dimension(:),allocatable::value_range
   contains
     procedure::get_actual_value=>real_argument_get_actual_value
     procedure::read_option=>real_argument_read_option
     procedure::write_description=>real_argument_write_description
     procedure::real_argument_initialize
     generic::initialize=>argument_initialize,real_argument_initialize
  end type real_argument_type

  type,extends(argument_class) :: integer_argument_type
     private
     integer(kind=i64)::default_value
     integer(kind=i64)::actual_value
     integer(kind=i64)::min_value
     integer(kind=i64)::max_value
     integer(kind=i64),dimension(:),allocatable::value_range
   contains
     procedure::get_actual_value=>integer_argument_get_actual_value
     procedure::read_option=>integer_argument_read_option
     procedure::write_description=>integer_argument_write_description
     procedure::integer_argument_initialize
     generic::initialize=>argument_initialize,integer_argument_initialize
  end type integer_argument_type

  type,extends(argument_class) :: string_argument_type
     private
     integer::actual_length=0
     integer::default_length=0
     character(:),allocatable::actual_value
     type(string_filo_stack_type)::value_range
   contains
     procedure::get_default_value=>string_argument_get_default_value
     procedure::get_actual_value=>string_argument_get_actual_value
     procedure::assign=>string_argument_assign
     procedure::push=>string_argument_push
     procedure::read_option=>string_argument_read_option
     procedure::string_argument_initialize
     procedure::finalize=>string_argument_finalize
     procedure::write_description=>string_argument_write_description
     generic::initialize=>string_argument_initialize
  end type string_argument_type

  type,extends(argument_class) :: plain_argument_type
   contains
     procedure::read_option=>plain_argument_read_option
     generic::initialize=>argument_initialize
  end type plain_argument_type

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Interface Definition !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!

  interface
     subroutine read_option_interface(this,index)
       import argument_class
       class(argument_class),intent(inout)::this
       integer,intent(inout)::index
     end subroutine read_option_interface
  end interface

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Module Procedure Definition !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Type Bound Procedures for argument_list_type !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine argument_list_push(this,argument)
    class(argument_list_type),intent(inout)::this
    class(argument_class),intent(in),target::argument
    class(argument_list_type),pointer::new_list
!    print *,"argument_list_push(this,",argument%get_a_form()
    if(associated(this%argument))then
       allocate(new_list)
       new_list%argument=>argument
       new_list%next=>this%next
       this%next=>new_list
    else
       this%argument=>argument
    end if
  end subroutine argument_list_push
  
  recursive subroutine argument_list_finalize(this)
    class(argument_list_type),intent(out)::this
    if(associated(this%next))call this%next%finalize()
    deallocate(this%next)
    nullify(this%argument)
  end subroutine argument_list_finalize

  subroutine argument_list_process(this)
    class(argument_list_type),intent(inout)::this
    character(2)::arg_kind
    integer::arg_length,arg_index,short_index
    character(:),allocatable::long_arg
    character::short_arg
    arg_index=0
    arg_loop:do
       arg_index=arg_index+1
       call get_command_argument(arg_index,arg_kind,arg_length)
       if(arg_kind(1:1)=="-")then
          if(arg_kind(2:2)=="-")then
             if(arg_length>2)then
                allocate(character(arg_length)::long_arg)
                call get_command_argument(arg_index,long_arg,arg_length)
!                print *, "process_long ",long_arg
                call this%process_long(long_arg(3:arg_length),arg_index)
                deallocate(long_arg)
             else
                print *,arg_kind,"XD is no valid argument."
                if(arguments_stop_at_invalid_argument)stop
             end if
          else
             if(arg_length>1)then
                allocate(character(arg_length)::long_arg)
                call get_command_argument(arg_index,long_arg,arg_length)
                short:do short_index=2,arg_length
                   short_arg=long_arg(short_index:short_index)
!                   print *, "process_short ",short_arg
                   call this%process_short(short_arg,arg_index)
                end do short
             else
                print *,arg_kind," is no valid argument."
                if(arguments_stop_at_invalid_argument)stop
             end if
          end if
       else
          exit
       end if
    end do arg_loop
  end subroutine argument_list_process

  recursive subroutine argument_list_process_short(this,short,index)
    class(argument_list_type),intent(inout)::this
    character,intent(in)::short
    integer,intent(inout)::index
    logical::match
    if(associated(this%argument))then
       call this%argument%compare_short(short,index,match)
       if(.not.match)then
          if(associated(this%next))then
             call this%next%process_short(short,index)
          else
             print *,"-",short," is no recognized argument."
             if(arguments_stop_at_unrecognized_argument)stop
          end if
       end if
    else
       print *,"argument_list_process_short: No Argument assigned."
    end if
  end subroutine argument_list_process_short

  recursive subroutine argument_list_process_long(this,long,index)
    class(argument_list_type),intent(inout)::this
    character(*),intent(in)::long
    integer,intent(inout)::index
    logical::match
    if(associated(this%argument))then
       call this%argument%compare_long(long,index,match)
       if(.not.match)then
          if(associated(this%next))then
             call this%next%process_long(long,index)
          else
             print *,"--",long," is no recognized argument."
             if(arguments_stop_at_unrecognized_argument)stop
          end if
       end if
    else
       print *,"argument_list_process_long: No Argument assigned."
    end if
  end subroutine argument_list_process_long

  recursive subroutine argument_list_write_description(this,unit)
    class(argument_list_type),intent(in)::this
    integer,intent(in)::unit
    if(associated(this%argument))call this%argument%write_description(unit)
    if(associated(this%next))call this%next%write_description(unit)
  end subroutine argument_list_write_description

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Type Bound Procedures for string_filo_stack_type !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine string_filo_stack_assign(this,string)
    class(string_filo_stack_type),intent(inout)::this
    character(*),intent(in)::string
    if(allocated(this%value))deallocate(this%value)
    allocate(character(len(string))::this%value)
    this%value=string
  end subroutine string_filo_stack_assign

  subroutine string_filo_stack_push_string(this,string)
    class(string_filo_stack_type),intent(inout)::this
    character(*),intent(in)::string
    class(string_filo_stack_type),pointer::new_entry
    allocate(new_entry)
    call new_entry%assign(string)
    new_entry%next=>this%next
    this%next=>new_entry
  end subroutine string_filo_stack_push_string
  
  subroutine string_filo_stack_push_filo_stack(this,stack)
    class(string_filo_stack_type),intent(inout)::this
    class(string_filo_stack_type),intent(inout),target::stack
    stack%next=>this%next
    this%next=>stack
  end subroutine string_filo_stack_push_filo_stack
 
  recursive subroutine string_filo_stack_finalize(this)
    class(string_filo_stack_type),intent(out)::this
    if(associated(this%next))then
       call this%next%finalize
       deallocate(this%next)
    end if
    if(allocated(this%value))deallocate(this%value)
  end subroutine string_filo_stack_finalize
    
  recursive subroutine string_filo_stack_contains(this,string,success)
    class(string_filo_stack_type),intent(in)::this
    character(*),intent(in)::string
    logical,intent(out)::success
    if(this%value==string)then
       success=.true.
    else
       if(associated(this%next))then
          call this%next%contains(string,success)
       else
          success=.false.
       end if
    end if
  end subroutine string_filo_stack_contains

  subroutine string_filo_stack_write_tail(this,unit,separator)
    class(string_filo_stack_type),intent(in)::this
    integer,intent(in)::unit
    character(*),intent(in),optional::separator
    if(associated(this%next))call string_filo_stack_write_contents(this%next,unit,separator)
  end subroutine string_filo_stack_write_tail

  recursive subroutine string_filo_stack_write_contents(this,unit,separator)
    class(string_filo_stack_type),intent(in)::this
    integer,intent(in)::unit
    character(*),intent(in),optional::separator    
    if(allocated(this%value))write(unit,fmt=("(a)"),ADVANCE="NO")this%value
    if(associated(this%next))then
       if(present(separator))then
          write(unit,fmt=("(a)"),ADVANCE="NO")separator
       else
          write(unit,fmt=*)""
       end if
       call this%next%write_contents(unit,separator)
    end if
  end subroutine string_filo_stack_write_contents

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Type Bound Procedures for string_list_type !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine string_list_set_last(this)
    class(string_list_type),intent(inout),target::this
    if(.not.associated(this%next))then
       this%last=>this
    else
       if(.not.associated(this%last))this%last=>this
       do while(associated(this%last%next))
          this%last=>this%last%next
       end do
    end if
  end subroutine string_list_set_last

  subroutine string_list_push_string(this,string)
    class(string_list_type),intent(inout),target::this
    character(*),intent(in)::string
    call string_filo_stack_push_string(this,string)
    call string_list_set_last(this)
  end subroutine string_list_push_string

  subroutine string_list_push_filo_stack(this,stack)
    class(string_list_type),intent(inout),target::this
    class(string_filo_stack_type),intent(inout),target::stack
    class(string_filo_stack_type),pointer::thisp
    thisp=>this
    call string_filo_stack_push_filo_stack(this,stack)
    call string_list_set_last(this)
  end subroutine string_list_push_filo_stack

  subroutine string_list_initialize(this,string)
    class(string_list_type),intent(out),target::this
    character(*),intent(in),optional::string
!    print *,"string_list_initialize"
!    print *,associated(this%last)
    this%last=>this
    if(allocated(this%value))deallocate(this%value)
    if(present(string))call this%assign(string)
!    print *,associated(this%last)
  end subroutine string_list_initialize

  subroutine string_list_finalize(this)
    class(string_list_type),intent(out),target::this
    call string_filo_stack_finalize(this)
    nullify(this%last)
  end subroutine string_list_finalize

  subroutine string_list_append_string(this,string)
    class(string_list_type),target,intent(inout)::this
    character(*),intent(in)::string
!    print *,string
!    print *,associated(this%last)
    allocate(this%last%next)
    this%last=>this%last%next
    call this%last%assign(string)
  end subroutine string_list_append_string

  subroutine string_list_append_filo_stack(this,stack)
    class(string_list_type),intent(inout)::this
    class(string_filo_stack_type),intent(in),target::stack
    this%last%next=>stack
    do while(associated(this%last%next))
       this%last=>this%last%next
    end do
  end subroutine string_list_append_filo_stack

  subroutine string_list_append_list(this,list)
    class(string_list_type),intent(inout)::this,list
    if(allocated(list%value))call this%append(list%value)
    call this%append(list%next)
  end subroutine string_list_append_list

  subroutine string_list_get_rightmost(this,string)
    class(string_list_type),intent(inout)::this
    character(*),intent(out)::string
    if(allocated(this%last%value))string=this%last%value
  end subroutine string_list_get_rightmost

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Type Bound Procedures for argument_class !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  elemental function argument_is_given(this)
    logical::argument_is_given
    class(argument_class),intent(in)::this
    argument_is_given=this%is_given_comp
  end function argument_is_given
  
  function argument_get_a_form(this) result(form)
    class(argument_class),intent(in)::this
    character(max(1,this%long_form_length))::form
    if(allocated(this%long_form))then
       form=this%long_form
    else
       form=this%short_form
    end if
  end function argument_get_a_form

  subroutine argument_initialize(this,arg_list,short,long,description,description_list,named_option)
    class(argument_class),intent(inout),target::this
    character,intent(in),optional::short
    character(*),intent(in),optional::long,description,named_option
    type(string_list_type),intent(inout),optional,target::description_list
    class(argument_list_type),optional,intent(inout)::arg_list
    if(present(long).or.present(short))then
       if(present(short))then
          this%short_form=short
          this%has_short_form=.true.
       else
          this%short_form=" "
          this%has_short_form=.false.
       end if
       if(allocated(this%long_form))deallocate(this%long_form)
       if(present(long))then
          this%long_form_length=len(long)
          allocate(this%long_form,source=long)
          this%has_long_form=.true.
       else
          this%long_form_length=0
          this%has_long_form=.false.
       end if
       allocate(this%description)
       call this%description%initialize()
       if(present(arg_list))call arg_list%push(this)
       if(present(description_list))then
          call this%description%push(description_list)
       else
          if(present(description))then
             call this%description%append(description)
          end if
       end if
       if(allocated(this%named_option))deallocate(this%named_option)
       if(present(named_option))then
          allocate(this%named_option,source=named_option)
          this%named_option_length=len(named_option)
       else
          this%named_option_length=0
       end if
    else
       print *,"argument_initialize: Neither short form nor long form given for argument. Stop."
       stop
    end if
  end subroutine argument_initialize
  
  subroutine argument_finalize(this)
    class(argument_class),intent(out)::this
    call this%description%finalize()
    deallocate(this%description)
  end subroutine argument_finalize
  
  subroutine argument_compare_short(this,short,index,match)
    class(argument_class),intent(inout)::this
    character,intent(in)::short
    integer,intent(inout)::index
    logical,intent(out)::match
    if(this%short_form==short)then
       match=.true.
       this%is_given_comp=.true.
       call this%read_option(index)
    else
       match=.false.
    end if
  end subroutine argument_compare_short

  subroutine argument_compare_long(this,long,index,match)
    class(argument_class),intent(inout)::this
    character(*),intent(in)::long
    integer,intent(inout)::index
    logical,intent(out)::match
    if(allocated(this%long_form))then
       if(this%long_form==long)then
          match=.true.
          this%is_given_comp=.true.
          call this%read_option(index)
       else
          match=.false.
       end if
    end if
  end subroutine argument_compare_long

  subroutine argument_write_description(this,unit)
    class(argument_class),intent(in)::this
    integer,intent(in)::unit
    integer::length
    length=6
    write(unit,fmt=("(a2)"),ADVANCE="NO")"  "
    if(this%has_short_form)then       
       write(unit,fmt=("(a1,a1)"),ADVANCE="NO")"-",this%short_form
       if(this%has_long_form)then
          write(unit,fmt=("(a)"),ADVANCE="NO")", "
       else
          write(unit,fmt=("(a3)"),ADVANCE="NO")"   "
       end if
    else
       write(unit,fmt=("(a)"),ADVANCE="NO")"    "
    end if
    if(this%has_long_form)then
       write(unit,fmt=("(a,a,a)"),ADVANCE="NO")"--",this%long_form," "
       length=length+this%long_form_length+3
    end if
    if(allocated(this%named_option))then
       write(unit,fmt=("(a,a)"),ADVANCE="NO")this%named_option(1:this%named_option_length)," "
       length=length+this%named_option_length+1
    end if
       if(length>=indent_len)then
          write(unit,fmt=*)""
          write(unit,fmt=("(a)"),ADVANCE="NO")repeat(" ",indent_len)
       else
          write(unit,fmt=("(a)"),ADVANCE="NO")repeat(" ",indent_len-length)
       end if
       call this%description%write_tail(unit,new_line(" ")//repeat(" ",indent_len))
    write(unit,fmt=*)""
  end subroutine argument_write_description

  subroutine argument_write_to_unit(this,unit)
    class(argument_class),intent(in)::this
    integer,intent(in)::unit
    write(unit,fmt=*)"short form:         ",this%short_form
    if(allocated(this%long_form))then
       write(unit,fmt=*)"long form:          ",this%long_form
    else
       write(unit,fmt=*)"long form:          ","not allocated."
    end if
    write(unit,fmt=*)"long form length:   ",this%long_form_length
    write(unit,fmt=*)"named option length:",this%named_option_length
    write(unit,fmt=*)"is given:           ",this%is_given_comp
    write(unit,fmt=*)"is default:         ",this%is_default
    write(unit,fmt=*)"is valid:           ",this%is_valid
    write(unit,fmt=*)"with option:        ",this%with_option
    write(unit,fmt=*)"has short form:     ",this%has_short_form
    write(unit,fmt=*)"has long form:      ",this%has_long_form
  end subroutine argument_write_to_unit

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Type Bound Procedures for real_argument_type !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  pure function real_argument_get_actual_value(this)
    real(kind=double)::real_argument_get_actual_value
    class(real_argument_type),intent(in)::this
    real_argument_get_actual_value=this%actual_value
  end function real_argument_get_actual_value
  
  subroutine real_argument_read_option(this,index)
    class(real_argument_type),intent(inout)::this
    integer,intent(inout)::index
    integer::n,length,iostat
    character(:),allocatable::option
    index=index+1
    call get_command_argument(index,length=length)
    allocate(character(length)::option)
    call get_command_argument(index,option)
    read(option,fmt=*,iostat=iostat)this%actual_value
    if(.not.iostat==0)then
       print *,"real_argument_read_option: could not parse option ",option," of argument ",this%get_a_form()
       if(arguments_stop_at_unrecognized_option)stop
       if(arguments_revert_invalid_to_default)then
          this%actual_value=this%default_value
       else
          this%is_valid=.false.
       end if
    end if
    if((this%actual_value<this%min_value).or.(this%actual_value>this%max_value))then
       print *,"real_argument_read_option: Option ",option," of argument ",this%get_a_form()," is out of range"
       print *,"Range is: [",this%min_value,":",this%max_value,"]"
       if(arguments_stop_at_invalid_option)stop
       if(arguments_revert_invalid_to_default)then
          this%actual_value=this%default_value
       else
          this%is_valid=.false.
       end if
    end if
    if(allocated(this%value_range))then
       this%is_valid=.false.
       do n=1,size(this%value_range)
          if(this%actual_value==this%value_range(n))then
             this%is_valid=.true.
             exit
          end if
       end do
       if(.not.this%is_valid)then
          print *,"Value ",this%actual_value," for argument ",this%get_a_form()," is invalid."
          print *,"Valid values are:"
          print *,this%value_range
          print *,"Default value is:"
          print *,this%default_value
          if(arguments_revert_invalid_to_default)then
             this%actual_value=this%default_value
          else
             this%is_valid=.false.
          end if
       end if
    end if
    if(this%actual_value==this%default_value)then
       this%is_default=.true.
    else
       this%is_default=.false.
    end if
    this%is_given_comp=.true.
  end subroutine real_argument_read_option

  subroutine real_argument_write_description(this,unit)
    class(real_argument_type),intent(in)::this
    integer,intent(in)::unit
    integer::i
    call argument_write_description(this,unit)
    if(allocated(this%value_range))then
       write(unit,("(a,a,a)"),ADVANCE="NO")repeat(" ",indent_len),this%named_option(1:this%named_option_length)," is one of: "
       write(unit,fmt='("{",E22.16)',ADVANCE="NO")this%value_range(1)
       do i=2,size(this%value_range)
          write(unit,fmt='(",",E22.16)',ADVANCE="NO")this%value_range(i)
       end do
       write(unit,fmt='("}")')
    else
       write(unit,("(a,a,a)"),ADVANCE="NO")repeat(" ",indent_len),this%named_option(1:this%named_option_length)," is in: "
       write(unit,fmt="(a,E22.16,a,E22.16,a)")"[",this%min_value,",",this%max_value,"]"
    end if
  end subroutine real_argument_write_description

  subroutine real_argument_initialize(this,value,min,max,arg_list,short,long,description,description_list,named_option,range)
    class(real_argument_type),intent(inout),target::this
    real(kind=double),intent(in)::value
    real(kind=double),optional,intent(in)::min,max
    class(argument_list_type),optional,intent(inout)::arg_list
    real(kind=double),dimension(:),intent(in),optional::range
    character,intent(in),optional::short
    character(*),intent(in),optional::long,description,named_option
    type(string_list_type),intent(inout),optional::description_list
    character(24)::default_char
    call argument_initialize(this,arg_list,short,long,description,description_list,named_option)
    this%default_value=value
    if(present(min))then
       this%min_value=min
    else
       this%min_value=-huge(1)
    end if
    if(present(max))then
       this%max_value=max
    else
       this%max_value=huge(1)
    end if
    if(this%min_value>this%max_value)then
       print *,"real_argument_initialize: min value is greater then max value. Stop."
       stop
    end if
    if((this%default_value<this%min_value).or.(this%default_value>this%max_value))then
       print *,"real_argument_initialize: default value is not in range. Stop."
       stop
    end if
    if(allocated(this%value_range))deallocate(this%value_range)
    if(present(range))allocate(this%value_range(size(range)),source=range)
    this%actual_value=this%default_value
    this%is_default=.true.
    this%is_valid=.true.
    write(default_char,'(E22.16)')this%default_value
    call this%description%append("Default value is "//trim(default_char)//".")
  end subroutine real_argument_initialize

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Type Bound Procedures for integer_argument_type !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  pure function integer_argument_get_actual_value(this)
    integer(kind=i64)::integer_argument_get_actual_value
    class(integer_argument_type),intent(in)::this
    integer_argument_get_actual_value=this%actual_value
  end function integer_argument_get_actual_value
  
  subroutine integer_argument_read_option(this,index)
    class(integer_argument_type),intent(inout)::this
    integer,intent(inout)::index
    integer::n,length,iostat
    character(:),allocatable::option
    index=index+1
    call get_command_argument(index,length=length)
    allocate(character(length)::option)
    call get_command_argument(index,option)
    read(option,fmt=*,iostat=iostat)this%actual_value
    if(.not.iostat==0)then
       print *,"integer_argument_read_option: could not parse option ",option," of argument ",this%get_a_form()
       if(arguments_stop_at_unrecognized_option)stop
       if(arguments_revert_invalid_to_default)then
          this%actual_value=this%default_value
       else
          this%is_valid=.false.
       end if
    end if
    if((this%actual_value<this%min_value).or.(this%actual_value>this%max_value))then
       print *,"integer_argument_read_option: Option ",option," of argument ",this%get_a_form()," is out of range"
       print *,"Range is: [",this%min_value,":",this%max_value,"]"
       if(arguments_stop_at_invalid_option)stop
       if(arguments_revert_invalid_to_default)then
          this%actual_value=this%default_value
       else
          this%is_valid=.false.
       end if
    end if
    if(allocated(this%value_range))then
       this%is_valid=.false.
       do n=1,size(this%value_range)
          if(this%actual_value==this%value_range(n))then
             this%is_valid=.true.
             exit
          end if
       end do
       if(.not.this%is_valid)then
          print *,"Value ",this%actual_value," for argument ",this%get_a_form()," is invalid."
          print *,"Valid values are:"
          print *,this%value_range
          print *,"Default value is:"
          print *,this%default_value
          if(arguments_revert_invalid_to_default)then
             this%actual_value=this%default_value
          else
             this%is_valid=.false.
          end if
       end if
    end if
    if(this%actual_value==this%default_value)then
       this%is_default=.true.
    else
       this%is_default=.false.
    end if
    this%is_given_comp=.true.
  end subroutine integer_argument_read_option

  subroutine integer_argument_write_description(this,unit)
    class(integer_argument_type),intent(in)::this
    integer,intent(in)::unit
    integer::i
    call argument_write_description(this,unit)
    if(allocated(this%value_range))then
       write(unit,("(a,a,a)"),ADVANCE="NO")repeat(" ",indent_len),this%named_option(1:this%named_option_length)," is one of: "
       write(unit,fmt='("{",I0)',ADVANCE="NO")this%value_range(1)
       do i=2,size(this%value_range)
          write(unit,fmt='(",",I0)',ADVANCE="NO")this%value_range(i)
       end do
       write(unit,fmt='("}")')
    else
       write(unit,("(a,a,a)"),ADVANCE="NO")repeat(" ",indent_len),this%named_option(1:this%named_option_length)," is in: "
       write(unit,fmt="(a,I0,a,I0,a)")"[",this%min_value,",",this%max_value,"]"
    end if
  end subroutine integer_argument_write_description

  subroutine integer_argument_initialize(this,value,min,max,arg_list,short,long,description,description_list,named_option,range)
    class(integer_argument_type),intent(inout),target::this
    integer(kind=i64),intent(in)::value
    integer(kind=i64),optional,intent(in)::min,max
    class(argument_list_type),optional,intent(inout)::arg_list
    integer(kind=i64),dimension(:),intent(in),optional::range
    character,intent(in),optional::short
    character(*),intent(in),optional::long,description,named_option
    type(string_list_type),intent(inout),optional::description_list
    character(12)::default_char
    call argument_initialize(this,arg_list,short,long,description,description_list,named_option)
    this%default_value=value
    if(present(min))then
       this%min_value=min
    else
       this%min_value=-huge(1)
    end if
    if(present(max))then
       this%max_value=max
    else
       this%max_value=huge(1)
    end if
    if(this%min_value>this%max_value)then
       print *,"integer_argument_initialize: min value is greater then max value. Stop."
       stop
    end if
    if((this%default_value<this%min_value).or.(this%default_value>this%max_value))then
       print *,"integer_argument_initialize: default value is not in range. Stop."
       stop
    end if
    if(allocated(this%value_range))deallocate(this%value_range)
    if(present(range))allocate(this%value_range(size(range)),source=range)
    this%actual_value=this%default_value
    this%is_default=.true.
    this%is_valid=.true.
    write(default_char,fmt='(I0)')this%default_value
    call this%description%append("Default value is "//trim(default_char)//".")
  end subroutine integer_argument_initialize

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Type Bound Procedures for string_argument_type !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  pure function string_argument_get_actual_value(this) result(def)
    class(string_argument_type),intent(in)::this
    character(this%actual_length)::def
    if(allocated(this%actual_value))then
       def=this%actual_value
    else
       def=""
    end if
  end function string_argument_get_actual_value

  pure function string_argument_get_default_value(this) result(def)
    class(string_argument_type),intent(in)::this
    character(this%default_length)::def
    if(allocated(this%value_range%value))then
       def=this%value_range%value
    else
       def=""
    end if
  end function string_argument_get_default_value

  subroutine string_argument_assign(this,string)
    class(string_argument_type),intent(inout)::this
    character(*),intent(in)::string
    if(allocated(this%actual_value))deallocate(this%actual_value)
    allocate(this%actual_value,source=string)
    this%actual_length=len(string)
  end subroutine string_argument_assign

  subroutine string_argument_push(this,string)
    class(string_argument_type),intent(inout)::this
    character(*),intent(in)::string
    call this%value_range%push(string)
  end subroutine string_argument_push

  subroutine string_argument_finalize(this)
    class(string_argument_type),intent(inout)::this
    call this%value_range%finalize()
  end subroutine string_argument_finalize
  
  subroutine string_argument_write_description(this,unit)
    class(string_argument_type),intent(in)::this
    integer,intent(in)::unit
!    print *,"hallo"
    call argument_write_description(this,unit)
    if(associated(this%value_range%next))then
       write(unit,("(a,a,a)"),ADVANCE="NO")repeat(" ",indent_len),this%named_option(1:this%named_option_length)," is one of: "
       call this%value_range%write_contents(unit,", ")
       write(unit,fmt=*)""
    end if
  end subroutine string_argument_write_description

  subroutine string_argument_read_option(this,index)
    class(string_argument_type),intent(inout)::this
    integer,intent(inout)::index
    integer::n,length,iostat
    character(:),allocatable::option
    index=index+1
    call get_command_argument(index,length=length)
    allocate(character(length)::option)
    call get_command_argument(index,option)
    if(associated(this%value_range%next))then
       call this%value_range%contains(option,this%is_valid)
    else
       this%is_valid=.true.
    end if
    if(this%is_valid)then
       call this%assign(option)
    else
       print *,"Value ",option," for argument ",this%short_form," is invalid."
       print *,"Valid values are:"
       call this%value_range%write_contents(output_unit)
       if(arguments_stop_at_invalid_option)stop
       if(arguments_revert_invalid_to_default)call this%assign(this%get_default_value())
    end if
    if(this%actual_value==this%value_range%value)then
       this%is_default=.true.
    else
       this%is_default=.false.
    end if
    this%is_given_comp=.true.
  end subroutine string_argument_read_option

  subroutine string_argument_initialize(this,value,arg_list,short,long,description,description_list,named_option)
    class(string_argument_type),intent(inout),target::this
    character(*),intent(in)::value
    class(argument_list_type),optional,intent(inout)::arg_list
    character,intent(in),optional::short
    character(*),intent(in),optional::long,description,named_option
    type(string_list_type),intent(inout),optional::description_list
    if(present(named_option))then
       call argument_initialize(this,arg_list,short,long,description,description_list,named_option)
    else
       call argument_initialize(this,arg_list,short,long,description,description_list,"<string>")
    end if
    call this%value_range%assign(value)
    this%default_length=len(value)
    call this%assign(value)
    this%is_default=.true.
    this%is_valid=.true.
    call this%description%append("Default value is '"//value//"'.")
  end subroutine string_argument_initialize

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Type Bound Procedures for switch_argument_type !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine switch_argument_read_option(this,index)
    class(switch_argument_type),intent(inout)::this
    integer,intent(inout)::index
  end subroutine switch_argument_read_option

  subroutine switch_argument_negates(this,negative)
    class(switch_argument_type),intent(inout),target::this,negative
    if(this%default_value.eqv.negative%default_value)then
       print *,"switch_argument_negates: Cannot assign negation: Both arguments have got the same default value."
    else
       this%disable_arg=>negative
       negative%disable_arg=>this
       call this%description%append("Negates "//negative%get_a_form())
       call negative%description%append("Negates "//this%get_a_form())
    end if
  end subroutine switch_argument_negates

  subroutine switch_argument_initialize(this,value,arg_list,short,long,description,description_list)
    class(switch_argument_type),intent(inout),target::this
    logical,intent(in)::value
    class(argument_list_type),optional,intent(inout)::arg_list
    character,intent(in),optional::short
    character(*),intent(in),optional::long,description
    type(string_list_type),intent(inout),optional::description_list
    call argument_initialize(this,arg_list,short,long,description,description_list)
    this%default_value=value
    this%actual_value=value
    this%is_default=.true.
    this%is_valid=.true.
    if(this%default_value)then
       call this%description%append("Default value is TRUE.")
    else
       call this%description%append("Default value is FALSE.")
    end if
  end subroutine switch_argument_initialize

  subroutine switch_argument_compare_short(this,short,index,match)
    class(switch_argument_type),intent(inout)::this
    character,intent(in)::short
    integer,intent(inout)::index
    logical,intent(out)::match
    call argument_compare_short(this,short,index,match)
    if(match)then
       this%actual_value=this%default_value
       this%is_default=.true.
       if(associated(this%disable_arg))then
          this%disable_arg%actual_value=.not.this%default_value
          this%disable_arg%is_default=.false.
       end if
    end if
  end subroutine switch_argument_compare_short

  subroutine switch_argument_compare_long(this,long,index,match)
    class(switch_argument_type),intent(inout)::this
    character(*),intent(in)::long
    integer,intent(inout)::index
    logical,intent(out)::match
    call argument_compare_long(this,long,index,match)
    if(match)then
       this%actual_value=this%default_value
       this%is_default=.true.
       if(associated(this%disable_arg))then
          this%disable_arg%actual_value=.not.this%default_value
          this%disable_arg%is_default=.false.
       end if
    end if
  end subroutine switch_argument_compare_long

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Type Bound Procedures for plain_argument_type !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine plain_argument_read_option(this,index)
    class(plain_argument_type),intent(inout)::this
    integer,intent(inout)::index
  end subroutine plain_argument_read_option
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Non Type Bound Procedures !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine arguments_setup(un_arg,in_arg,un_opt,in_opt,revert)
    logical,intent(in),optional::un_arg,in_arg,un_opt,in_opt,revert
    if(present(un_arg))arguments_stop_at_unrecognized_argument=un_arg
    if(present(in_arg))arguments_stop_at_invalid_argument=in_arg
    if(present(un_opt))arguments_stop_at_unrecognized_option=un_opt
    if(present(in_opt))arguments_stop_at_invalid_option=in_opt
    if(present(revert))arguments_revert_invalid_to_default=revert
  end subroutine arguments_setup

end module arguments
