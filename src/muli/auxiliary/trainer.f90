program trainer
  use arguments
  use muli
  implicit none
  type(muli_type)::mi
  type(sample_inclusive_type)::sample
  type(sample_int_kind_type),dimension(:),allocatable::int_kinds
  call initialize()
contains
  subroutine initialize()
    type(argument_list_type)::args
    type(integer_argument_type),target::int_kind,random_seed,pdf_set,n_total,n_print
    type(real_argument_type),target::gev_cme,gev_cutoff
    type(string_argument_type),target::pdf_file,muli_dir
    type(plain_argument_type),target::help_arg
    type(switch_argument_type),target::dynamic_remnant,collect,analyse
    character(2)::num
    integer::n
    call help_arg%initialize(args,"h","help","Print this Message and exit.")
    call random_seed%initialize(1_i64,1_i64,(2_i64)**30,args,long="random_seed",named_option="<NUMBER>", description="When given, tao random numbers are initialized with <NUMBER>.")
    call int_kind%initialize(1_i64,1_i64,16_i64,args,long="int_kind",named_option="<NUMBER>", description="The integrand kind stratus to train.")
    call n_total%initialize(1000000_i64,1_i64,huge(1_i64),args,long="n_total",named_option="<NUMBER>", description="Total number of pt chains.")
    call n_print%initialize(1000_i64,1_i64,huge(1_i64),args,long="n_print",named_option="<NUMBER>", description="Total number of output lines.")
    call pdf_set%initialize(1_i64,1_i64,huge(1_i64),args,long="pdf_set",named_option="<NUMBER>", description="The pdf set member.")
    call pdf_file%initialize("cteq5l.LHgrid",args,long="pdf_file",named_option="<FILE>")
    call muli_dir%initialize("/tmp",args,long="muli_dir",named_option="<DIR>",description="Place to read and write generator information.")
    call gev_cme%initialize(14D3,1D0,huge(1D0),long="cme",named_option="<NUMBER>",description="Initial center of mass energy in GeV")
    call gev_cutoff%initialize(1D0,0D0,huge(1D0),long="cutoff",named_option="<NUMBER>",description="Lower limit of pt in GeV")
    call collect%initialize(.false.,args,long="collect",description="Do not generate interactions but merge strati to one single file.")
    call analyse%initialize(.false.,args,long="analyse",description="Generate some Plots.")
    call dynamic_remnant%initialize(.false.,args,long="dynamic_remnant",description="When given, remnants will be altered after every interaction.")
    call pdf_file%push("cteq61.LHpdf")
    call pdf_file%push("GSG961.LHgrid")
    call args%process()
    if(help_arg%is_given())then
       call args%write_description(output_unit)
       stop
    end if
    if(collect%is_given())then
       allocate(int_kinds(16))
       do n=1,16
          call integer_with_leading_zeros(n,2,num)
          call int_kinds(n)%deserialize("sample_int_kind_"//num,muli_dir%get_actual_value()//"/sample_int_kind/"//num//".xml")
       end do
    else
       call mi%initialize(&
            gev_cutoff%get_actual_value()**2,&
            gev_cme%get_actual_value()**2,&
            muli_dir%get_actual_value(),&
!            pdf_file%get_actual_value(),&
!            int(pdf_set%get_actual_value(),kind=i32),&
            int(random_seed%get_actual_value(),kind=i32))
       if(dynamic_remnant%is_given())then
          call mi%enable_remnant_pdf()
       else
          call mi%disable_remnant_pdf()
       end if
       call mi%generate_samples(&
            n_total%get_actual_value(),&
            n_print%get_actual_value(),&
            int(int_kind%get_actual_value(),kind=i32),&
            muli_dir%get_actual_value(),&
            analyse%is_given())
    end if
  end subroutine initialize
end program trainer
