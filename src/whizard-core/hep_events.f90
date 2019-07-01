! WHIZARD 2.2.2 July 6 2014
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

module hep_events
  
  use kinds !NODEP!
  use file_utils !NODEP!
   use diagnostics !NODEP!

  use models
  use particles
  use processes
  use hep_common
  use events

  implicit none
  private

  public :: hepeup_from_event
  public :: hepeup_to_event
  public :: hepevt_from_event

contains
  
  subroutine hepeup_from_event (event, keep_beams, process_index)
    type(event_t), intent(in), target :: event
    logical, intent(in), optional :: keep_beams
    integer, intent(in), optional :: process_index
    type(particle_set_t), pointer :: particle_set
    real(default) :: scale, alpha_qcd
    if (event%has_particle_set ()) then
       particle_set => event%get_particle_set_ptr ()
       call hepeup_from_particle_set (particle_set, keep_beams)
       if (present (process_index)) &
            call hepeup_set_event_parameters (proc_id = process_index)
       scale = event%get_fac_scale ()
       if (scale /= 0) &
            call hepeup_set_event_parameters (scale = scale)
       alpha_qcd = event%get_alpha_s ()       
       if (alpha_qcd /= 0) &
            call hepeup_set_event_parameters (alpha_qcd = alpha_qcd)
       if (event%weight_prc_is_known) then
          call hepeup_set_event_parameters (weight = event%weight_prc)
       else
          call msg_bug ("HEPEUP: process weight is unknown")
       end if
    else
       call msg_bug ("HEPEUP: event incomplete")
    end if
  end subroutine hepeup_from_event

  subroutine hepeup_to_event &
       (event, fallback_model, process_index, recover_beams)
    type(event_t), intent(inout), target :: event
    type(model_t), intent(in), target :: fallback_model
    integer, intent(out), optional :: process_index
    logical, intent(in), optional :: recover_beams
    type(process_t), pointer :: process
    type(model_t), pointer :: model
    real(default) :: weight, scale, alpha_qcd
    type(particle_set_t) :: particle_set
    process => event%get_process_ptr ()
    model => process%get_model_ptr ()
    call hepeup_to_particle_set &
         (particle_set, recover_beams, model, fallback_model)
    call event%set_particle_set_hard_proc (particle_set)
    call particle_set_final (particle_set)
    if (present (process_index)) then
       call hepeup_get_event_parameters (proc_id = process_index)
    end if
    call hepeup_get_event_parameters (weight = weight, &
         scale = scale, alpha_qcd = alpha_qcd)
    call event%set (weight_ref = weight)
!!! Not implemented yet:
!     if (scale > 0)  call event%set_scales (scale)
!     if (alpha_qcd > 0)  call event%set_alpha_qcd (alpha_qcd)
  end subroutine hepeup_to_event

  subroutine hepevt_from_event (event, i_evt, keep_beams)
    type(event_t), intent(in), target :: event
    integer, intent(in), optional :: i_evt
    logical, intent(in), optional :: keep_beams  
    type(particle_set_t), pointer :: particle_set
    if (event%has_particle_set ()) then
       particle_set => event%get_particle_set_ptr ()
       call hepevt_from_particle_set (particle_set, keep_beams)
       if (event%weight_prc_is_known .and. event%sqme_prc_is_known) then
          call hepevt_set_event_parameters ( &
               weight = event%weight_prc, &
               function_value = event%sqme_prc)
       else
          call msg_bug ("HEPEVT: event weight and/or sqme unknown")
       end if
       if (present (i_evt)) &
            call hepevt_set_event_parameters (i_evt = i_evt)
    else
       call msg_bug ("HEPEVT: event incomplete")
    end if
  end subroutine hepevt_from_event


end module hep_events
