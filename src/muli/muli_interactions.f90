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

module muli_interactions

  use kinds, only: default, double
  use constants
  use muli_momentum

  implicit none
  private  

  public :: muli_get_state_transformations
  public :: h_to_c_param
  public :: c_to_h_param
  public :: h_to_c_param_def
  public :: h_to_c_ort
  public :: c_to_h_ort
  public :: h_to_c_ort_def
  public :: c_to_h_ort_def
  public :: h_to_c_noparam
  public :: c_to_h_noparam
  public :: c_to_h_param_def
  public :: h_to_c_smooth
  public :: c_to_h_smooth
  public :: h_to_c_smooth_def
  public :: voxel_h_to_c_ort
  public :: voxel_c_to_h_ort
  public :: voxel_h_to_c_noparam
  public :: voxel_c_to_h_noparam
  public :: voxel_h_to_c_param
  public :: voxel_c_to_h_param
  public :: voxel_h_to_c_smooth 
  public :: voxel_c_to_h_smooth
  public :: voxel_h_to_c_ort_def
  public :: voxel_c_to_h_ort_def
  public :: voxel_h_to_c_param_def 
  public :: voxel_c_to_h_param_def
  public :: voxel_h_to_c_smooth_def
  public :: voxel_c_to_h_smooth_def
  public :: denom_cart
  public :: denom_ort
  public :: denom_param
  public :: denom_param_reg
  public :: denom_smooth
  public :: denom_smooth_reg
  public :: denom_cart_save
  public :: denom_ort_save
  public :: denom_param_save
  public :: denom_smooth_save
  public :: denom_cart_cuba_int
  public :: denom_ort_cuba_int
  public :: denom_param_cuba_int
  public :: denom_smooth_cuba_int
  public :: coordinates_hcd_cart
  public :: coordinates_hcd_ort
  public :: coordinates_hcd_param
  public :: coordinates_hcd_param_reg
  public :: coordinates_hcd_smooth
  public :: coordinates_hcd_smooth_reg
  public :: interactions_dddsigma_reg
  public :: pdf_in_in_kind
  public :: ps_io_pol
  public :: interactions_dddsigma
  public :: interactions_dddsigma_print
  public :: interactions_dddsigma_cart 
  public :: cuba_gg_me_smooth
  public :: cuba_gg_me_param
  public :: cuba_gg_me_ort
  public :: cuba_gg_me_cart
  public :: interactions_proton_proton_integrand_generic_17_reg
  public :: interactions_proton_proton_integrand_param_17_reg
  public :: interactions_proton_proton_integrand_smooth_17_reg

  character(len=2), dimension(-6:6), parameter :: integer_parton_names = &
       ["-6", "-5", "-4", "-3", "-2", "-1", "00", &
        "+1", "+2", "+3", "+4", "+5", "+6" ]
  character, dimension(-6:6),parameter :: traditional_parton_names = &
       ["T", "B", "C", "S", "U", "D", "g", "d", "u", "s", "c", "b", "t"]    
  real(default), dimension(1:4,1:5), parameter :: &
       phase_space_coefficients_in = reshape (source = &
       [ 6144, -4608,  +384,    0, &
         6144, -5120,  +384,    0, &
         6144, -2048,  +128, -576, &
        13824, -9600, +1056,    0, &
        31104,-19872, +2160, +486 ], shape=[4,5])
  integer, parameter :: hadron_A_kind =  2212 
  integer, parameter :: hadron_B_kind = -2212 
  integer, dimension(4), parameter, public :: &
       parton_kind_of_int_kind = [1, 1, 2, 2]
  real(default), parameter :: b_sigma_tot_all = 100 !mb PDG
  real(default), parameter :: &
       b_sigma_tot_nd = 0.5*b_sigma_tot_all !!! PRD 49 n5 1994
  real(default), parameter, public :: &
       gev_cme_tot = 14000 ! total center of mass energie
  real(default), parameter :: gev2_cme_tot = gev_cme_tot**2 !!! s
  real(default), parameter :: gev_pt_max = gev_cme_tot/2D0
  real(default), parameter :: gev2_pt_max = gev2_cme_tot/4D0
  !model parameters
  real(default), parameter :: gev_pt_min = 8E-1_default
  real(default), parameter :: gev2_pt_min = gev_pt_min**2
  real(default), parameter :: pts_min = gev_pt_min / gev_pt_max
  real(default), parameter :: pts2_min = gev2_pt_min / gev2_pt_max
  real(default), parameter :: gev_p_t_0 = 2.0
  real(default), parameter :: gev2_p_t_0 = gev_p_t_0**2
  real(default), parameter :: norm_p_t_0 = gev_p_t_0 / gev_pt_max
  real(default), parameter :: norm2_p_t_0 = gev2_p_t_0 / gev2_pt_max
  !mathematical constants
  real(default), parameter, public :: euler = exp(one)
  !physical constants
  real(default), parameter :: gev2_mbarn = 0.389379304_default
  real(default), parameter :: const_pref = pi * gev2_mbarn / &
       (gev2_cme_tot * b_sigma_tot_nd)

  integer, parameter, public :: LHA_FLAVOR_AT = -6
  integer, parameter, public :: LHA_FLAVOR_AB = -5
  integer, parameter, public :: LHA_FLAVOR_AC = -4
  integer, parameter, public :: LHA_FLAVOR_AS = -3
  integer, parameter, public :: LHA_FLAVOR_AU = -2
  integer, parameter, public :: LHA_FLAVOR_AD = -1
  integer, parameter, public :: LHA_FLAVOR_G = 0
  integer, parameter, public :: LHA_FLAVOR_D = 1
  integer, parameter, public :: LHA_FLAVOR_U = 2
  integer, parameter, public :: LHA_FLAVOR_S = 3
  integer, parameter, public :: LHA_FLAVOR_C = 4
  integer, parameter, public :: LHA_FLAVOR_B = 5
  integer, parameter, public :: LHA_FLAVOR_T = 6

  integer, parameter, public :: PDG_FLAVOR_AT = -6
  integer, parameter, public :: PDG_FLAVOR_AB = -5
  integer, parameter, public :: PDG_FLAVOR_AC = -4
  integer, parameter, public :: PDG_FLAVOR_AS = -3
  integer, parameter, public :: PDG_FLAVOR_AU = -2
  integer, parameter, public :: PDG_FLAVOR_AD = -1
  integer, parameter, public :: PDG_FLAVOR_G = 21
  integer, parameter, public :: PDG_FLAVOR_D = 1
  integer, parameter, public :: PDG_FLAVOR_U = 2
  integer, parameter, public :: PDG_FLAVOR_S = 3
  integer, parameter, public :: PDG_FLAVOR_C = 4
  integer, parameter, public :: PDG_FLAVOR_B = 5
  integer, parameter, public :: PDG_FLAVOR_T = 6

  integer, parameter, public :: PARTON_SEA = 1
  integer, parameter, public :: PARTON_VALENCE = 2
  integer, parameter, public :: PARTON_SEA_AND_VALENCE = 3
  integer, parameter, public :: PARTON_TWIN = 4
  integer, parameter, public :: PARTON_SEA_AND_TWIN = 5
  integer, parameter, public :: PARTON_VALENCE_AND_TWIN = 6
  integer, parameter, public :: PARTON_ALL = 7

  integer, parameter, public :: PDF_UNDEFINED = 0
  integer, parameter, public :: PDF_GLUON = 1
  integer, parameter, public :: PDF_SEA = 2
  integer, parameter, public :: PDF_VALENCE_DOWN = 3
  integer, parameter, public :: PDF_VALENCE_UP = 4
  integer, parameter, public :: PDF_TWIN = 5  

  real(default), dimension(1:4,1:8),parameter :: &
       phase_space_coefficients_inout = reshape(source=[ &
       3072,  -2304,  +192,    0,  & 
       6144,  -5120,  +384,    0,  &
       0,         0,   192,  -96,  &
       3072,  -2048,  +192,  -96,  & 
       0,      2048, -2176, +576,  & 
       0,       288,  -306,  +81,  &
       6912,  -4800,  +528,    0,  &
       31104,-23328, +5832, -486], &
       shape=[4,8])  
  
  integer, dimension(1:4,0:8), parameter :: inout_signatures = &
       reshape (source = [ &
        1, 1, 1, 1, &   !1a
       -1, 1,-1, 1, &   !1b
        1, 1, 1, 1, &   !2
        1,-1, 1,-1, &   !3
        1,-1, 1,-1, &   !4
        1,-1, 0, 0, &   !5
        0, 0, 1,-1, &   !6
        1, 0, 1, 0, &   !7
        0, 0, 0, 0  ], &
        shape = [4,9])
  
  integer, dimension(6,-234:234), save, public :: valid_processes
  data valid_processes (:,-234)  / -6,  -6,  -6,  -6,   2,   2 /
  data valid_processes (:,-233)  / -6,  -5,  -6,  -5,   1,   1 /
  data valid_processes (:,-232)  / -6,  -5,  -5,  -6,   1,   1 /
  data valid_processes (:,-231)  / -6,  -4,  -6,  -4,   1,   1 /
  data valid_processes (:,-230)  / -6,  -4,  -4,  -6,   1,   1 /
  data valid_processes (:,-229)  / -6,  -3,  -6,  -3,   1,   1 /
  data valid_processes (:,-228)  / -6,  -3,  -3,  -6,   1,   1 /
  data valid_processes (:,-227)  / -6,  -2,  -6,  -2,   1,   1 /
  data valid_processes (:,-226)  / -6,  -2,  -2,  -6,   1,   1 /
  data valid_processes (:,-225)  / -6,  -1,  -6,  -1,   1,   1 /
  data valid_processes (:,-224)  / -6,  -1,  -1,  -6,   1,   1 /
  data valid_processes (:,-223)  / -6,   0,  -6,   0,   4,   7 /
  data valid_processes (:,-222)  / -6,   0,   0,  -6,   4,   7 /
  data valid_processes (:,-221)  / -6,   1,  -6,   1,   1,   1 /
  data valid_processes (:,-220)  / -6,   1,   1,  -6,   1,   1 /
  data valid_processes (:,-219)  / -6,   2,  -6,   2,   1,   1 /
  data valid_processes (:,-218)  / -6,   2,   2,  -6,   1,   1 /
  data valid_processes (:,-217)  / -6,   3,  -6,   3,   1,   1 /
  data valid_processes (:,-216)  / -6,   3,   3,  -6,   1,   1 /
  data valid_processes (:,-215)  / -6,   4,  -6,   4,   1,   1 /
  data valid_processes (:,-214)  / -6,   4,   4,  -6,   1,   1 /
  data valid_processes (:,-213)  / -6,   5,  -6,   5,   1,   1 /
  data valid_processes (:,-212)  / -6,   5,   5,  -6,   1,   1 /
  data valid_processes (:,-211)  / -6,   6,  -6,   6,   3,   4 /
  data valid_processes (:,-210)  / -6,   6,  -5,   5,   3,   3 /
  data valid_processes (:,-209)  / -6,   6,  -4,   4,   3,   3 /
  data valid_processes (:,-208)  / -6,   6,  -3,   3,   3,   3 /
  data valid_processes (:,-207)  / -6,   6,  -2,   2,   3,   3 /
  data valid_processes (:,-206)  / -6,   6,  -1,   1,   3,   3 /
  data valid_processes (:,-205)  / -6,   6,   0,   0,   3,   5 /
  data valid_processes (:,-204)  / -6,   6,   1,  -1,   3,   3 /
  data valid_processes (:,-203)  / -6,   6,   2,  -2,   3,   3 /
  data valid_processes (:,-202)  / -6,   6,   3,  -3,   3,   3 /
  data valid_processes (:,-201)  / -6,   6,   4,  -4,   3,   3 /
  data valid_processes (:,-200)  / -6,   6,   5,  -5,   3,   3 /
  data valid_processes (:,-199)  / -6,   6,   6,  -6,   3,   4 /
  data valid_processes (:,-198)  / -5,  -6,  -6,  -5,   1,   1 /
  data valid_processes (:,-197)  / -5,  -6,  -5,  -6,   1,   1 /
  data valid_processes (:,-196)  / -5,  -5,  -5,  -5,   2,   2 /
  data valid_processes (:,-195)  / -5,  -4,  -5,  -4,   1,   1 /
  data valid_processes (:,-194)  / -5,  -4,  -4,  -5,   1,   1 /
  data valid_processes (:,-193)  / -5,  -3,  -5,  -3,   1,   1 /
  data valid_processes (:,-192)  / -5,  -3,  -3,  -5,   1,   1 /
  data valid_processes (:,-191)  / -5,  -2,  -5,  -2,   1,   1 /
  data valid_processes (:,-190)  / -5,  -2,  -2,  -5,   1,   1 /
  data valid_processes (:,-189)  / -5,  -1,  -5,  -1,   1,   1 /
  data valid_processes (:,-188)  / -5,  -1,  -1,  -5,   1,   1 /
  data valid_processes (:,-187)  / -5,   0,  -5,   0,   4,   7 /
  data valid_processes (:,-186)  / -5,   0,   0,  -5,   4,   7 /
  data valid_processes (:,-185)  / -5,   1,  -5,   1,   1,   1 /
  data valid_processes (:,-184)  / -5,   1,   1,  -5,   1,   1 /
  data valid_processes (:,-183)  / -5,   2,  -5,   2,   1,   1 /
  data valid_processes (:,-182)  / -5,   2,   2,  -5,   1,   1 /
  data valid_processes (:,-181)  / -5,   3,  -5,   3,   1,   1 /
  data valid_processes (:,-180)  / -5,   3,   3,  -5,   1,   1 /
  data valid_processes (:,-179)  / -5,   4,  -5,   4,   1,   1 /
  data valid_processes (:,-178)  / -5,   4,   4,  -5,   1,   1 /
  data valid_processes (:,-177)  / -5,   5,  -6,   6,   3,   3 /
  data valid_processes (:,-176)  / -5,   5,  -5,   5,   3,   4 /
  data valid_processes (:,-175)  / -5,   5,  -4,   4,   3,   3 /
  data valid_processes (:,-174)  / -5,   5,  -3,   3,   3,   3 /
  data valid_processes (:,-173)  / -5,   5,  -2,   2,   3,   3 /
  data valid_processes (:,-172)  / -5,   5,  -1,   1,   3,   3 /
  data valid_processes (:,-171)  / -5,   5,   0,   0,   3,   5 /
  data valid_processes (:,-170)  / -5,   5,   1,  -1,   3,   3 /
  data valid_processes (:,-169)  / -5,   5,   2,  -2,   3,   3 /
  data valid_processes (:,-168)  / -5,   5,   3,  -3,   3,   3 /
  data valid_processes (:,-167)  / -5,   5,   4,  -4,   3,   3 /
  data valid_processes (:,-166)  / -5,   5,   5,  -5,   3,   4 /
  data valid_processes (:,-165)  / -5,   5,   6,  -6,   3,   3 /
  data valid_processes (:,-164)  / -5,   6,  -5,   6,   1,   1 /
  data valid_processes (:,-163)  / -5,   6,   6,  -5,   1,   1 /
  data valid_processes (:,-162)  / -4,  -6,  -6,  -4,   1,   1 /
  data valid_processes (:,-161)  / -4,  -6,  -4,  -6,   1,   1 /
  data valid_processes (:,-160)  / -4,  -5,  -5,  -4,   1,   1 /
  data valid_processes (:,-159)  / -4,  -5,  -4,  -5,   1,   1 /
  data valid_processes (:,-158)  / -4,  -4,  -4,  -4,   2,   2 /
  data valid_processes (:,-157)  / -4,  -3,  -4,  -3,   1,   1 /
  data valid_processes (:,-156)  / -4,  -3,  -3,  -4,   1,   1 /
  data valid_processes (:,-155)  / -4,  -2,  -4,  -2,   1,   1 /
  data valid_processes (:,-154)  / -4,  -2,  -2,  -4,   1,   1 /
  data valid_processes (:,-153)  / -4,  -1,  -4,  -1,   1,   1 /
  data valid_processes (:,-152)  / -4,  -1,  -1,  -4,   1,   1 /
  data valid_processes (:,-151)  / -4,   0,  -4,   0,   4,   7 /
  data valid_processes (:,-150)  / -4,   0,   0,  -4,   4,   7 /
  data valid_processes (:,-149)  / -4,   1,  -4,   1,   1,   1 /
  data valid_processes (:,-148)  / -4,   1,   1,  -4,   1,   1 /
  data valid_processes (:,-147)  / -4,   2,  -4,   2,   1,   1 /
  data valid_processes (:,-146)  / -4,   2,   2,  -4,   1,   1 /
  data valid_processes (:,-145)  / -4,   3,  -4,   3,   1,   1 /
  data valid_processes (:,-144)  / -4,   3,   3,  -4,   1,   1 /
  data valid_processes (:,-143)  / -4,   4,  -6,   6,   3,   3 /
  data valid_processes (:,-142)  / -4,   4,  -5,   5,   3,   3 /
  data valid_processes (:,-141)  / -4,   4,  -4,   4,   3,   4 /
  data valid_processes (:,-140)  / -4,   4,  -3,   3,   3,   3 /
  data valid_processes (:,-139)  / -4,   4,  -2,   2,   3,   3 /
  data valid_processes (:,-138)  / -4,   4,  -1,   1,   3,   3 /
  data valid_processes (:,-137)  / -4,   4,   0,   0,   3,   5 /
  data valid_processes (:,-136)  / -4,   4,   1,  -1,   3,   3 /
  data valid_processes (:,-135)  / -4,   4,   2,  -2,   3,   3 /
  data valid_processes (:,-134)  / -4,   4,   3,  -3,   3,   3 /
  data valid_processes (:,-133)  / -4,   4,   4,  -4,   3,   4 /
  data valid_processes (:,-132)  / -4,   4,   5,  -5,   3,   3 /
  data valid_processes (:,-131)  / -4,   4,   6,  -6,   3,   3 /
  data valid_processes (:,-130)  / -4,   5,  -4,   5,   1,   1 /
  data valid_processes (:,-129)  / -4,   5,   5,  -4,   1,   1 /
  data valid_processes (:,-128)  / -4,   6,  -4,   6,   1,   1 /
  data valid_processes (:,-127)  / -4,   6,   6,  -4,   1,   1 /
  data valid_processes (:,-126)  / -3,  -6,  -6,  -3,   1,   1 /
  data valid_processes (:,-125)  / -3,  -6,  -3,  -6,   1,   1 /
  data valid_processes (:,-124)  / -3,  -5,  -5,  -3,   1,   1 /
  data valid_processes (:,-123)  / -3,  -5,  -3,  -5,   1,   1 /
  data valid_processes (:,-122)  / -3,  -4,  -4,  -3,   1,   1 /
  data valid_processes (:,-121)  / -3,  -4,  -3,  -4,   1,   1 /
  data valid_processes (:,-120)  / -3,  -3,  -3,  -3,   2,   2 /
  data valid_processes (:,-119)  / -3,  -2,  -3,  -2,   1,   1 /
  data valid_processes (:,-118)  / -3,  -2,  -2,  -3,   1,   1 /
  data valid_processes (:,-117)  / -3,  -1,  -3,  -1,   1,   1 /
  data valid_processes (:,-116)  / -3,  -1,  -1,  -3,   1,   1 /
  data valid_processes (:,-115)  / -3,   0,  -3,   0,   4,   7 /
  data valid_processes (:,-114)  / -3,   0,   0,  -3,   4,   7 /
  data valid_processes (:,-113)  / -3,   1,  -3,   1,   1,   1 /
  data valid_processes (:,-112)  / -3,   1,   1,  -3,   1,   1 /
  data valid_processes (:,-111)  / -3,   2,  -3,   2,   1,   1 /
  data valid_processes (:,-110)  / -3,   2,   2,  -3,   1,   1 /
  data valid_processes (:,-109)  / -3,   3,  -6,   6,   3,   3 /
  data valid_processes (:,-108)  / -3,   3,  -5,   5,   3,   3 /
  data valid_processes (:,-107)  / -3,   3,  -4,   4,   3,   3 /
  data valid_processes (:,-106)  / -3,   3,  -3,   3,   3,   4 /
  data valid_processes (:,-105)  / -3,   3,  -2,   2,   3,   3 /
  data valid_processes (:,-104)  / -3,   3,  -1,   1,   3,   3 /
  data valid_processes (:,-103)  / -3,   3,   0,   0,   3,   5 /
  data valid_processes (:,-102)  / -3,   3,   1,  -1,   3,   3 /
  data valid_processes (:,-101)  / -3,   3,   2,  -2,   3,   3 /
  data valid_processes (:,-100)  / -3,   3,   3,  -3,   3,   4 /
  data valid_processes (:, -99)  / -3,   3,   4,  -4,   3,   3 /
  data valid_processes (:, -98)  / -3,   3,   5,  -5,   3,   3 /
  data valid_processes (:, -97)  / -3,   3,   6,  -6,   3,   3 /
  data valid_processes (:, -96)  / -3,   4,  -3,   4,   1,   1 /
  data valid_processes (:, -95)  / -3,   4,   4,  -3,   1,   1 /
  data valid_processes (:, -94)  / -3,   5,  -3,   5,   1,   1 /
  data valid_processes (:, -93)  / -3,   5,   5,  -3,   1,   1 /
  data valid_processes (:, -92)  / -3,   6,  -3,   6,   1,   1 /
  data valid_processes (:, -91)  / -3,   6,   6,  -3,   1,   1 /
  data valid_processes (:, -90)  / -2,  -6,  -6,  -2,   1,   1 /
  data valid_processes (:, -89)  / -2,  -6,  -2,  -6,   1,   1 /
  data valid_processes (:, -88)  / -2,  -5,  -5,  -2,   1,   1 /
  data valid_processes (:, -87)  / -2,  -5,  -2,  -5,   1,   1 /
  data valid_processes (:, -86)  / -2,  -4,  -4,  -2,   1,   1 /
  data valid_processes (:, -85)  / -2,  -4,  -2,  -4,   1,   1 /
  data valid_processes (:, -84)  / -2,  -3,  -3,  -2,   1,   1 /
  data valid_processes (:, -83)  / -2,  -3,  -2,  -3,   1,   1 /
  data valid_processes (:, -82)  / -2,  -2,  -2,  -2,   2,   2 /
  data valid_processes (:, -81)  / -2,  -1,  -2,  -1,   1,   1 /
  data valid_processes (:, -80)  / -2,  -1,  -1,  -2,   1,   1 /
  data valid_processes (:, -79)  / -2,   0,  -2,   0,   4,   7 /
  data valid_processes (:, -78)  / -2,   0,   0,  -2,   4,   7 /
  data valid_processes (:, -77)  / -2,   1,  -2,   1,   1,   1 /
  data valid_processes (:, -76)  / -2,   1,   1,  -2,   1,   1 /
  data valid_processes (:, -75)  / -2,   2,  -6,   6,   3,   3 /
  data valid_processes (:, -74)  / -2,   2,  -5,   5,   3,   3 /
  data valid_processes (:, -73)  / -2,   2,  -4,   4,   3,   3 /
  data valid_processes (:, -72)  / -2,   2,  -3,   3,   3,   3 /
  data valid_processes (:, -71)  / -2,   2,  -2,   2,   3,   4 /
  data valid_processes (:, -70)  / -2,   2,  -1,   1,   3,   3 /
  data valid_processes (:, -69)  / -2,   2,   0,   0,   3,   5 /
  data valid_processes (:, -68)  / -2,   2,   1,  -1,   3,   3 /
  data valid_processes (:, -67)  / -2,   2,   2,  -2,   3,   4 /
  data valid_processes (:, -66)  / -2,   2,   3,  -3,   3,   3 /
  data valid_processes (:, -65)  / -2,   2,   4,  -4,   3,   3 /
  data valid_processes (:, -64)  / -2,   2,   5,  -5,   3,   3 /
  data valid_processes (:, -63)  / -2,   2,   6,  -6,   3,   3 /
  data valid_processes (:, -62)  / -2,   3,  -2,   3,   1,   1 /
  data valid_processes (:, -61)  / -2,   3,   3,  -2,   1,   1 /
  data valid_processes (:, -60)  / -2,   4,  -2,   4,   1,   1 /
  data valid_processes (:, -59)  / -2,   4,   4,  -2,   1,   1 /
  data valid_processes (:, -58)  / -2,   5,  -2,   5,   1,   1 /
  data valid_processes (:, -57)  / -2,   5,   5,  -2,   1,   1 /
  data valid_processes (:, -56)  / -2,   6,  -2,   6,   1,   1 /
  data valid_processes (:, -55)  / -2,   6,   6,  -2,   1,   1 /
  data valid_processes (:, -54)  / -1,  -6,  -6,  -1,   1,   1 /
  data valid_processes (:, -53)  / -1,  -6,  -1,  -6,   1,   1 /
  data valid_processes (:, -52)  / -1,  -5,  -5,  -1,   1,   1 /
  data valid_processes (:, -51)  / -1,  -5,  -1,  -5,   1,   1 /
  data valid_processes (:, -50)  / -1,  -4,  -4,  -1,   1,   1 /
  data valid_processes (:, -49)  / -1,  -4,  -1,  -4,   1,   1 /
  data valid_processes (:, -48)  / -1,  -3,  -3,  -1,   1,   1 /
  data valid_processes (:, -47)  / -1,  -3,  -1,  -3,   1,   1 /
  data valid_processes (:, -46)  / -1,  -2,  -2,  -1,   1,   1 /
  data valid_processes (:, -45)  / -1,  -2,  -1,  -2,   1,   1 /
  data valid_processes (:, -44)  / -1,  -1,  -1,  -1,   2,   2 /
  data valid_processes (:, -43)  / -1,   0,  -1,   0,   4,   7 /
  data valid_processes (:, -42)  / -1,   0,   0,  -1,   4,   7 /
  data valid_processes (:, -41)  / -1,   1,  -6,   6,   3,   3 /
  data valid_processes (:, -40)  / -1,   1,  -5,   5,   3,   3 /
  data valid_processes (:, -39)  / -1,   1,  -4,   4,   3,   3 /
  data valid_processes (:, -38)  / -1,   1,  -3,   3,   3,   3 /
  data valid_processes (:, -37)  / -1,   1,  -2,   2,   3,   3 /
  data valid_processes (:, -36)  / -1,   1,  -1,   1,   3,   4 /
  data valid_processes (:, -35)  / -1,   1,   0,   0,   3,   5 /
  data valid_processes (:, -34)  / -1,   1,   1,  -1,   3,   4 /
  data valid_processes (:, -33)  / -1,   1,   2,  -2,   3,   3 /
  data valid_processes (:, -32)  / -1,   1,   3,  -3,   3,   3 /
  data valid_processes (:, -31)  / -1,   1,   4,  -4,   3,   3 /
  data valid_processes (:, -30)  / -1,   1,   5,  -5,   3,   3 /
  data valid_processes (:, -29)  / -1,   1,   6,  -6,   3,   3 /
  data valid_processes (:, -28)  / -1,   2,  -1,   2,   1,   1 /
  data valid_processes (:, -27)  / -1,   2,   2,  -1,   1,   1 /
  data valid_processes (:, -26)  / -1,   3,  -1,   3,   1,   1 /
  data valid_processes (:, -25)  / -1,   3,   3,  -1,   1,   1 /
  data valid_processes (:, -24)  / -1,   4,  -1,   4,   1,   1 /
  data valid_processes (:, -23)  / -1,   4,   4,  -1,   1,   1 /
  data valid_processes (:, -22)  / -1,   5,  -1,   5,   1,   1 /
  data valid_processes (:, -21)  / -1,   5,   5,  -1,   1,   1 /
  data valid_processes (:, -20)  / -1,   6,  -1,   6,   1,   1 /
  data valid_processes (:, -19)  / -1,   6,   6,  -1,   1,   1 /
  data valid_processes (:, -18)  /  0,  -6,  -6,   0,   4,   7 /
  data valid_processes (:, -17)  /  0,  -6,   0,  -6,   4,   7 /
  data valid_processes (:, -16)  /  0,  -5,  -5,   0,   4,   7 /
  data valid_processes (:, -15)  /  0,  -5,   0,  -5,   4,   7 /
  data valid_processes (:, -14)  /  0,  -4,  -4,   0,   4,   7 /
  data valid_processes (:, -13)  /  0,  -4,   0,  -4,   4,   7 /
  data valid_processes (:, -12)  /  0,  -3,  -3,   0,   4,   7 /
  data valid_processes (:, -11)  /  0,  -3,   0,  -3,   4,   7 /
  data valid_processes (:, -10)  /  0,  -2,  -2,   0,   4,   7 /
  data valid_processes (:,  -9)  /  0,  -2,   0,  -2,   4,   7 /
  data valid_processes (:,  -8)  /  0,  -1,  -1,   0,   4,   7 /
  data valid_processes (:,  -7)  /  0,  -1,   0,  -1,   4,   7 /
  data valid_processes (:,  -6)  /  0,   0,  -6,   6,   5,   6 /
  data valid_processes (:,  -5)  /  0,   0,  -5,   5,   5,   6 /
  data valid_processes (:,  -4)  /  0,   0,  -4,   4,   5,   6 /
  data valid_processes (:,  -3)  /  0,   0,  -3,   3,   5,   6 /
  data valid_processes (:,  -2)  /  0,   0,  -2,   2,   5,   6 /
  data valid_processes (:,  -1)  /  0,   0,  -1,   1,   5,   6 /
  data valid_processes (:,   0)  /  0,   0,   0,   0,   5,   8 /
  data valid_processes (:,   1)  /  0,   0,   1,  -1,   5,   6 /
  data valid_processes (:,   2)  /  0,   0,   2,  -2,   5,   6 /
  data valid_processes (:,   3)  /  0,   0,   3,  -3,   5,   6 /
  data valid_processes (:,   4)  /  0,   0,   4,  -4,   5,   6 /
  data valid_processes (:,   5)  /  0,   0,   5,  -5,   5,   6 /
  data valid_processes (:,   6)  /  0,   0,   6,  -6,   5,   6 /
  data valid_processes (:,   7)  /  0,   1,   0,   1,   4,   7 /
  data valid_processes (:,   8)  /  0,   1,   1,   0,   4,   7 /
  data valid_processes (:,   9)  /  0,   2,   0,   2,   4,   7 /
  data valid_processes (:,  10)  /  0,   2,   2,   0,   4,   7 /
  data valid_processes (:,  11)  /  0,   3,   0,   3,   4,   7 /
  data valid_processes (:,  12)  /  0,   3,   3,   0,   4,   7 /
  data valid_processes (:,  13)  /  0,   4,   0,   4,   4,   7 /
  data valid_processes (:,  14)  /  0,   4,   4,   0,   4,   7 /
  data valid_processes (:,  15)  /  0,   5,   0,   5,   4,   7 /
  data valid_processes (:,  16)  /  0,   5,   5,   0,   4,   7 /
  data valid_processes (:,  17)  /  0,   6,   0,   6,   4,   7 /
  data valid_processes (:,  18)  /  0,   6,   6,   0,   4,   7 /
  data valid_processes (:,  19)  /  1,  -6,  -6,   1,   1,   1 /
  data valid_processes (:,  20)  /  1,  -6,   1,  -6,   1,   1 /
  data valid_processes (:,  21)  /  1,  -5,  -5,   1,   1,   1 /
  data valid_processes (:,  22)  /  1,  -5,   1,  -5,   1,   1 /
  data valid_processes (:,  23)  /  1,  -4,  -4,   1,   1,   1 /
  data valid_processes (:,  24)  /  1,  -4,   1,  -4,   1,   1 /
  data valid_processes (:,  25)  /  1,  -3,  -3,   1,   1,   1 /
  data valid_processes (:,  26)  /  1,  -3,   1,  -3,   1,   1 /
  data valid_processes (:,  27)  /  1,  -2,  -2,   1,   1,   1 /
  data valid_processes (:,  28)  /  1,  -2,   1,  -2,   1,   1 /
  data valid_processes (:,  29)  /  1,  -1,  -6,   6,   3,   3 /
  data valid_processes (:,  30)  /  1,  -1,  -5,   5,   3,   3 /
  data valid_processes (:,  31)  /  1,  -1,  -4,   4,   3,   3 /
  data valid_processes (:,  32)  /  1,  -1,  -3,   3,   3,   3 /
  data valid_processes (:,  33)  /  1,  -1,  -2,   2,   3,   3 /
  data valid_processes (:,  34)  /  1,  -1,  -1,   1,   3,   4 /
  data valid_processes (:,  35)  /  1,  -1,   0,   0,   3,   5 /
  data valid_processes (:,  36)  /  1,  -1,   1,  -1,   3,   4 /
  data valid_processes (:,  37)  /  1,  -1,   2,  -2,   3,   3 /
  data valid_processes (:,  38)  /  1,  -1,   3,  -3,   3,   3 /
  data valid_processes (:,  39)  /  1,  -1,   4,  -4,   3,   3 /
  data valid_processes (:,  40)  /  1,  -1,   5,  -5,   3,   3 /
  data valid_processes (:,  41)  /  1,  -1,   6,  -6,   3,   3 /
  data valid_processes (:,  42)  /  1,   0,   0,   1,   4,   7 /
  data valid_processes (:,  43)  /  1,   0,   1,   0,   4,   7 /
  data valid_processes (:,  44)  /  1,   1,   1,   1,   2,   2 /
  data valid_processes (:,  45)  /  1,   2,   1,   2,   1,   1 /
  data valid_processes (:,  46)  /  1,   2,   2,   1,   1,   1 /
  data valid_processes (:,  47)  /  1,   3,   1,   3,   1,   1 /
  data valid_processes (:,  48)  /  1,   3,   3,   1,   1,   1 /
  data valid_processes (:,  49)  /  1,   4,   1,   4,   1,   1 /
  data valid_processes (:,  50)  /  1,   4,   4,   1,   1,   1 /
  data valid_processes (:,  51)  /  1,   5,   1,   5,   1,   1 /
  data valid_processes (:,  52)  /  1,   5,   5,   1,   1,   1 /
  data valid_processes (:,  53)  /  1,   6,   1,   6,   1,   1 /
  data valid_processes (:,  54)  /  1,   6,   6,   1,   1,   1 /
  data valid_processes (:,  55)  /  2,  -6,  -6,   2,   1,   1 /
  data valid_processes (:,  56)  /  2,  -6,   2,  -6,   1,   1 /
  data valid_processes (:,  57)  /  2,  -5,  -5,   2,   1,   1 /
  data valid_processes (:,  58)  /  2,  -5,   2,  -5,   1,   1 /
  data valid_processes (:,  59)  /  2,  -4,  -4,   2,   1,   1 /
  data valid_processes (:,  60)  /  2,  -4,   2,  -4,   1,   1 /
  data valid_processes (:,  61)  /  2,  -3,  -3,   2,   1,   1 /
  data valid_processes (:,  62)  /  2,  -3,   2,  -3,   1,   1 /
  data valid_processes (:,  63)  /  2,  -2,  -6,   6,   3,   3 /
  data valid_processes (:,  64)  /  2,  -2,  -5,   5,   3,   3 /
  data valid_processes (:,  65)  /  2,  -2,  -4,   4,   3,   3 /
  data valid_processes (:,  66)  /  2,  -2,  -3,   3,   3,   3 /
  data valid_processes (:,  67)  /  2,  -2,  -2,   2,   3,   4 /
  data valid_processes (:,  68)  /  2,  -2,  -1,   1,   3,   3 /
  data valid_processes (:,  69)  /  2,  -2,   0,   0,   3,   5 /
  data valid_processes (:,  70)  /  2,  -2,   1,  -1,   3,   3 /
  data valid_processes (:,  71)  /  2,  -2,   2,  -2,   3,   4 /
  data valid_processes (:,  72)  /  2,  -2,   3,  -3,   3,   3 /
  data valid_processes (:,  73)  /  2,  -2,   4,  -4,   3,   3 /
  data valid_processes (:,  74)  /  2,  -2,   5,  -5,   3,   3 /
  data valid_processes (:,  75)  /  2,  -2,   6,  -6,   3,   3 /
  data valid_processes (:,  76)  /  2,  -1,  -1,   2,   1,   1 /
  data valid_processes (:,  77)  /  2,  -1,   2,  -1,   1,   1 /
  data valid_processes (:,  78)  /  2,   0,   0,   2,   4,   7 /
  data valid_processes (:,  79)  /  2,   0,   2,   0,   4,   7 /
  data valid_processes (:,  80)  /  2,   1,   1,   2,   1,   1 /
  data valid_processes (:,  81)  /  2,   1,   2,   1,   1,   1 /
  data valid_processes (:,  82)  /  2,   2,   2,   2,   2,   2 /
  data valid_processes (:,  83)  /  2,   3,   2,   3,   1,   1 /
  data valid_processes (:,  84)  /  2,   3,   3,   2,   1,   1 /
  data valid_processes (:,  85)  /  2,   4,   2,   4,   1,   1 /
  data valid_processes (:,  86)  /  2,   4,   4,   2,   1,   1 /
  data valid_processes (:,  87)  /  2,   5,   2,   5,   1,   1 /
  data valid_processes (:,  88)  /  2,   5,   5,   2,   1,   1 /
  data valid_processes (:,  89)  /  2,   6,   2,   6,   1,   1 /
  data valid_processes (:,  90)  /  2,   6,   6,   2,   1,   1 /
  data valid_processes (:,  91)  /  3,  -6,  -6,   3,   1,   1 /
  data valid_processes (:,  92)  /  3,  -6,   3,  -6,   1,   1 /
  data valid_processes (:,  93)  /  3,  -5,  -5,   3,   1,   1 /
  data valid_processes (:,  94)  /  3,  -5,   3,  -5,   1,   1 /
  data valid_processes (:,  95)  /  3,  -4,  -4,   3,   1,   1 /
  data valid_processes (:,  96)  /  3,  -4,   3,  -4,   1,   1 /
  data valid_processes (:,  97)  /  3,  -3,  -6,   6,   3,   3 /
  data valid_processes (:,  98)  /  3,  -3,  -5,   5,   3,   3 /
  data valid_processes (:,  99)  /  3,  -3,  -4,   4,   3,   3 /
  data valid_processes (:, 100)  /  3,  -3,  -3,   3,   3,   4 /
  data valid_processes (:, 101)  /  3,  -3,  -2,   2,   3,   3 /
  data valid_processes (:, 102)  /  3,  -3,  -1,   1,   3,   3 /
  data valid_processes (:, 103)  /  3,  -3,   0,   0,   3,   5 /
  data valid_processes (:, 104)  /  3,  -3,   1,  -1,   3,   3 /
  data valid_processes (:, 105)  /  3,  -3,   2,  -2,   3,   3 /
  data valid_processes (:, 106)  /  3,  -3,   3,  -3,   3,   4 /
  data valid_processes (:, 107)  /  3,  -3,   4,  -4,   3,   3 /
  data valid_processes (:, 108)  /  3,  -3,   5,  -5,   3,   3 /
  data valid_processes (:, 109)  /  3,  -3,   6,  -6,   3,   3 /
  data valid_processes (:, 110)  /  3,  -2,  -2,   3,   1,   1 /
  data valid_processes (:, 111)  /  3,  -2,   3,  -2,   1,   1 /
  data valid_processes (:, 112)  /  3,  -1,  -1,   3,   1,   1 /
  data valid_processes (:, 113)  /  3,  -1,   3,  -1,   1,   1 /
  data valid_processes (:, 114)  /  3,   0,   0,   3,   4,   7 /
  data valid_processes (:, 115)  /  3,   0,   3,   0,   4,   7 /
  data valid_processes (:, 116)  /  3,   1,   1,   3,   1,   1 /
  data valid_processes (:, 117)  /  3,   1,   3,   1,   1,   1 /
  data valid_processes (:, 118)  /  3,   2,   2,   3,   1,   1 /
  data valid_processes (:, 119)  /  3,   2,   3,   2,   1,   1 /
  data valid_processes (:, 120)  /  3,   3,   3,   3,   2,   2 /
  data valid_processes (:, 121)  /  3,   4,   3,   4,   1,   1 /
  data valid_processes (:, 122)  /  3,   4,   4,   3,   1,   1 /
  data valid_processes (:, 123)  /  3,   5,   3,   5,   1,   1 /
  data valid_processes (:, 124)  /  3,   5,   5,   3,   1,   1 /
  data valid_processes (:, 125)  /  3,   6,   3,   6,   1,   1 /
  data valid_processes (:, 126)  /  3,   6,   6,   3,   1,   1 /
  data valid_processes (:, 127)  /  4,  -6,  -6,   4,   1,   1 /
  data valid_processes (:, 128)  /  4,  -6,   4,  -6,   1,   1 /
  data valid_processes (:, 129)  /  4,  -5,  -5,   4,   1,   1 /
  data valid_processes (:, 130)  /  4,  -5,   4,  -5,   1,   1 /
  data valid_processes (:, 131)  /  4,  -4,  -6,   6,   3,   3 /
  data valid_processes (:, 132)  /  4,  -4,  -5,   5,   3,   3 /
  data valid_processes (:, 133)  /  4,  -4,  -4,   4,   3,   4 /
  data valid_processes (:, 134)  /  4,  -4,  -3,   3,   3,   3 /
  data valid_processes (:, 135)  /  4,  -4,  -2,   2,   3,   3 /
  data valid_processes (:, 136)  /  4,  -4,  -1,   1,   3,   3 /
  data valid_processes (:, 137)  /  4,  -4,   0,   0,   3,   5 /
  data valid_processes (:, 138)  /  4,  -4,   1,  -1,   3,   3 /
  data valid_processes (:, 139)  /  4,  -4,   2,  -2,   3,   3 /
  data valid_processes (:, 140)  /  4,  -4,   3,  -3,   3,   3 /
  data valid_processes (:, 141)  /  4,  -4,   4,  -4,   3,   4 /
  data valid_processes (:, 142)  /  4,  -4,   5,  -5,   3,   3 /
  data valid_processes (:, 143)  /  4,  -4,   6,  -6,   3,   3 /
  data valid_processes (:, 144)  /  4,  -3,  -3,   4,   1,   1 /
  data valid_processes (:, 145)  /  4,  -3,   4,  -3,   1,   1 /
  data valid_processes (:, 146)  /  4,  -2,  -2,   4,   1,   1 /
  data valid_processes (:, 147)  /  4,  -2,   4,  -2,   1,   1 /
  data valid_processes (:, 148)  /  4,  -1,  -1,   4,   1,   1 /
  data valid_processes (:, 149)  /  4,  -1,   4,  -1,   1,   1 /
  data valid_processes (:, 150)  /  4,   0,   0,   4,   4,   7 /
  data valid_processes (:, 151)  /  4,   0,   4,   0,   4,   7 /
  data valid_processes (:, 152)  /  4,   1,   1,   4,   1,   1 /
  data valid_processes (:, 153)  /  4,   1,   4,   1,   1,   1 /
  data valid_processes (:, 154)  /  4,   2,   2,   4,   1,   1 /
  data valid_processes (:, 155)  /  4,   2,   4,   2,   1,   1 /
  data valid_processes (:, 156)  /  4,   3,   3,   4,   1,   1 /
  data valid_processes (:, 157)  /  4,   3,   4,   3,   1,   1 /
  data valid_processes (:, 158)  /  4,   4,   4,   4,   2,   2 /
  data valid_processes (:, 159)  /  4,   5,   4,   5,   1,   1 /
  data valid_processes (:, 160)  /  4,   5,   5,   4,   1,   1 /
  data valid_processes (:, 161)  /  4,   6,   4,   6,   1,   1 /
  data valid_processes (:, 162)  /  4,   6,   6,   4,   1,   1 /
  data valid_processes (:, 163)  /  5,  -6,  -6,   5,   1,   1 /
  data valid_processes (:, 164)  /  5,  -6,   5,  -6,   1,   1 /
  data valid_processes (:, 165)  /  5,  -5,  -6,   6,   3,   3 /
  data valid_processes (:, 166)  /  5,  -5,  -5,   5,   3,   4 /
  data valid_processes (:, 167)  /  5,  -5,  -4,   4,   3,   3 /
  data valid_processes (:, 168)  /  5,  -5,  -3,   3,   3,   3 /
  data valid_processes (:, 169)  /  5,  -5,  -2,   2,   3,   3 /
  data valid_processes (:, 170)  /  5,  -5,  -1,   1,   3,   3 /
  data valid_processes (:, 171)  /  5,  -5,   0,   0,   3,   5 /
  data valid_processes (:, 172)  /  5,  -5,   1,  -1,   3,   3 /
  data valid_processes (:, 173)  /  5,  -5,   2,  -2,   3,   3 /
  data valid_processes (:, 174)  /  5,  -5,   3,  -3,   3,   3 /
  data valid_processes (:, 175)  /  5,  -5,   4,  -4,   3,   3 /
  data valid_processes (:, 176)  /  5,  -5,   5,  -5,   3,   4 /
  data valid_processes (:, 177)  /  5,  -5,   6,  -6,   3,   3 /
  data valid_processes (:, 178)  /  5,  -4,  -4,   5,   1,   1 /
  data valid_processes (:, 179)  /  5,  -4,   5,  -4,   1,   1 /
  data valid_processes (:, 180)  /  5,  -3,  -3,   5,   1,   1 /
  data valid_processes (:, 181)  /  5,  -3,   5,  -3,   1,   1 /
  data valid_processes (:, 182)  /  5,  -2,  -2,   5,   1,   1 /
  data valid_processes (:, 183)  /  5,  -2,   5,  -2,   1,   1 /
  data valid_processes (:, 184)  /  5,  -1,  -1,   5,   1,   1 /
  data valid_processes (:, 185)  /  5,  -1,   5,  -1,   1,   1 /
  data valid_processes (:, 186)  /  5,   0,   0,   5,   4,   7 /
  data valid_processes (:, 187)  /  5,   0,   5,   0,   4,   7 /
  data valid_processes (:, 188)  /  5,   1,   1,   5,   1,   1 /
  data valid_processes (:, 189)  /  5,   1,   5,   1,   1,   1 /
  data valid_processes (:, 190)  /  5,   2,   2,   5,   1,   1 /
  data valid_processes (:, 191)  /  5,   2,   5,   2,   1,   1 /
  data valid_processes (:, 192)  /  5,   3,   3,   5,   1,   1 /
  data valid_processes (:, 193)  /  5,   3,   5,   3,   1,   1 /
  data valid_processes (:, 194)  /  5,   4,   4,   5,   1,   1 /
  data valid_processes (:, 195)  /  5,   4,   5,   4,   1,   1 /
  data valid_processes (:, 196)  /  5,   5,   5,   5,   2,   2 /
  data valid_processes (:, 197)  /  5,   6,   5,   6,   1,   1 /
  data valid_processes (:, 198)  /  5,   6,   6,   5,   1,   1 /
  data valid_processes (:, 199)  /  6,  -6,  -6,   6,   3,   4 /
  data valid_processes (:, 200)  /  6,  -6,  -5,   5,   3,   3 /
  data valid_processes (:, 201)  /  6,  -6,  -4,   4,   3,   3 /
  data valid_processes (:, 202)  /  6,  -6,  -3,   3,   3,   3 /
  data valid_processes (:, 203)  /  6,  -6,  -2,   2,   3,   3 /
  data valid_processes (:, 204)  /  6,  -6,  -1,   1,   3,   3 /
  data valid_processes (:, 205)  /  6,  -6,   0,   0,   3,   5 /
  data valid_processes (:, 206)  /  6,  -6,   1,  -1,   3,   3 /
  data valid_processes (:, 207)  /  6,  -6,   2,  -2,   3,   3 /
  data valid_processes (:, 208)  /  6,  -6,   3,  -3,   3,   3 /
  data valid_processes (:, 209)  /  6,  -6,   4,  -4,   3,   3 /
  data valid_processes (:, 210)  /  6,  -6,   5,  -5,   3,   3 /
  data valid_processes (:, 211)  /  6,  -6,   6,  -6,   3,   4 /
  data valid_processes (:, 212)  /  6,  -5,  -5,   6,   1,   1 /
  data valid_processes (:, 213)  /  6,  -5,   6,  -5,   1,   1 /
  data valid_processes (:, 214)  /  6,  -4,  -4,   6,   1,   1 /
  data valid_processes (:, 215)  /  6,  -4,   6,  -4,   1,   1 /
  data valid_processes (:, 216)  /  6,  -3,  -3,   6,   1,   1 /
  data valid_processes (:, 217)  /  6,  -3,   6,  -3,   1,   1 /
  data valid_processes (:, 218)  /  6,  -2,  -2,   6,   1,   1 /
  data valid_processes (:, 219)  /  6,  -2,   6,  -2,   1,   1 /
  data valid_processes (:, 220)  /  6,  -1,  -1,   6,   1,   1 /
  data valid_processes (:, 221)  /  6,  -1,   6,  -1,   1,   1 /
  data valid_processes (:, 222)  /  6,   0,   0,   6,   4,   7 /
  data valid_processes (:, 223)  /  6,   0,   6,   0,   4,   7 /
  data valid_processes (:, 224)  /  6,   1,   1,   6,   1,   1 /
  data valid_processes (:, 225)  /  6,   1,   6,   1,   1,   1 /
  data valid_processes (:, 226)  /  6,   2,   2,   6,   1,   1 /
  data valid_processes (:, 227)  /  6,   2,   6,   2,   1,   1 /
  data valid_processes (:, 228)  /  6,   3,   3,   6,   1,   1 /
  data valid_processes (:, 229)  /  6,   3,   6,   3,   1,   1 /
  data valid_processes (:, 230)  /  6,   4,   4,   6,   1,   1 /
  data valid_processes (:, 231)  /  6,   4,   6,   4,   1,   1 /
  data valid_processes (:, 232)  /  6,   5,   5,   6,   1,   1 /
  data valid_processes (:, 233)  /  6,   5,   6,   5,   1,   1 /
  data valid_processes (:, 234)  /  6,   6,   6,   6,   2,   2 /
  
  integer, dimension(2,0:16), parameter, public :: &
       double_pdf_kinds = reshape ( [ &
       0, 0, &
       1, 1, &
       1, 2, &
       1, 3, &
       1, 4, &
       2, 1, &
       2, 2, &
       2, 3, &
       2, 4, &
       3, 1, &
       3, 2, &
       3, 3, &
       3, 4, &
       4, 1, &
       4, 2, &
       4, 3, &
       4, 4], [2, 17])
  
  integer, parameter, dimension(371), public :: int_all = [ &
         -6,  -5,  -4,  -3,  -2,  -1,   0,  1 ,   2, &
          3,   4,   5,   6, -14, -13, -12, -11, -10, &
          9,  -8,  -7,   7,   8,   9,  10,  11,  12, &
         13,  14,   7,   8,   9,  10,-151,-150,-115, &
       -114, -79, -78, -43, -42,  42,  43,  78,  79, & 
        114, 115, 150, 151,-158,-157,-156,-155,-154, &
       -153,-152,-149,-148,-147,-146,-145,-144,-143, &
       -142,-141,-140,-139,-138,-137,-136,-135,-134, &
       -133,-132,-131,-122,-121,-120,-119,-118,-117, &
       -116,-113,-112,-111,-110,-109,-108,-107,-106, &
       -105,-104,-103,-102,-101,-100, -99, -98, -97, &
        -96, -95, -86, -85, -84, -83, -82, -81, -80, &
        -77, -76, -75, -74, -73, -72, -71, -70, -69, &
        -68, -67, -66, -65, -64, -63, -62, -61, -60, &
        -59, -50, -49, -48, -47, -46, -45, -44, -41, &
        -40, -39, -38, -37, -36, -35, -34, -33, -32, &
        -31, -30, -29, -28, -27, -26, -25, -24, -23, &
         23,  24,  25,  26,  27,  28,  29,  30,  31, &
         32,  33,  34,  35,  36,  37,  38,  39,  40, &
         41,  44,  45,  46,  47,  48,  49,  50,  59, &
         60,  61,  62,  63,  64,  65,  66,  67,  68, &
         69,  70,  71,  72,  73,  74,  75,  76,  77, &
         80,  81,  82,  83,  84,  85,  86,  95,  96, &
         97,  98,  99, 100, 101, 102, 103, 104, 105, &
        106, 107, 108, 109, 110, 111, 112, 113, 116, &
        117, 118, 119, 120, 121, 122, 131, 132, 133, &
        134, 135, 136, 137, 138, 139, 140, 141, 142, &
        143, 144, 145, 146, 147, 148, 149, 152, 153, &
        154, 155, 156, 157, 158,-149,-148,-113,-112, &
        -77, -76, -41, -40, -39, -38, -37, -36, -35, &
        -34, -33, -32, -31, -30, -29,  44,  80,  81, &
        116, 117, 152, 153,-147,-146,-111,-110, -75, &
        -74, -73, -72, -71, -70, -69, -68, -67, -66, &
        -65, -64, -63, -28, -27,  45,  46,  82, 118, &
        119, 154, 155,  42,  43,  23,  24,  25,  26, &
         27,  28,  29,  30,  31,  32,  33,  34,  35, &
         36,  37,  38,  39,  40,  41,  44,  45,  46, &
         47,  48,  49,  50,  44,  45,  46,  78,  79, &
         59,  60,  61,  62,  63,  64,  65,  66,  67, &
         68,  69,  70,  71,  72,  73,  74,  75,  76, &
         77,  80,  81,  82,  83,  84,  85,  86,  80, &
         81,  82 ]
  
  integer, parameter, dimension(16), public :: int_sizes_all = &
       [13, 16, 2, 2, 16, 208, 26, 26, 2, 26, 1, 2, 2, 26, 2, 1]
  
  integer, parameter, dimension(3,0:8), public :: muli_flow_stats = &
       reshape( [ &
           1,  2,  4,   &
           3,  4,  4,   &
           5,  6,  8,   &
           7,  8,  4,   &
           9, 10,  8,   &
          11, 16, 16,   &
          17, 22, 16,   &
          23, 28, 16,   &
          29, 52, 96 ], &
          [3,9])
  
  integer, parameter, dimension(0:4,52), public :: muli_flows = &
       reshape( [ &
       3, 0, 0, 1, 2, &   !1a
       1, 0, 0, 2, 1, &
       1, 2, 0, 0, 3, &   !1b
       3, 3, 0, 0, 2, &
       4, 0, 0, 1, 2, &   !2
       4, 0, 0, 2, 1, &
       3, 2, 0, 0, 3, &   !3
       1, 3, 0, 0, 2, &
       4, 2, 0, 0, 3, &   !4
       4, 3, 0, 0, 2, &
       4, 0, 1, 3, 4, &   !5
       4, 0, 1, 4, 3, &
       2, 0, 3, 1, 4, &
       2, 0, 4, 1, 3, &
       2, 0, 3, 4, 1, &
       2, 0, 4, 3, 1, &
       4, 1, 2, 4, 0, &    !6
       2, 1, 4, 2, 0, &
       4, 2, 1, 4, 0, &
       2, 4, 1, 2, 0, &
       2, 2, 4, 1, 0, &
       2, 4, 2, 1, 0, &
       2, 0, 1, 2, 4, &    !7
       2, 0, 1, 4, 2, &
       4, 0, 2, 1, 4, &
       4, 0, 4, 1, 2, &
       2, 0, 2, 4, 1, &
       2, 0, 4, 2, 1, &
       9, 1, 2, 3, 4, &    !8
       5, 1, 2, 4, 3, &
       5, 1, 3, 2, 4, &
       3, 1, 4, 2, 3, &
       3, 1, 3, 4, 2, &
       5, 1, 4, 3, 2, &
       5, 2, 1, 3, 4, &
       5, 2, 1, 4, 3, &
       3, 3, 1, 2, 4, &
       3, 4, 1, 2, 3, &
       3, 3, 1, 4, 2, &
       3, 4, 1, 3, 2, &
       3, 2, 3, 1, 4, &
       3, 2, 4, 1, 3, &
       5, 3, 2, 1, 4, &
       3, 4, 2, 1, 3, &
       5, 3, 4, 1, 2, &
       3, 4, 3, 1, 2, &
       3, 2, 3, 4, 1, &
       3, 2, 4, 3, 1, &
       3, 3, 2, 4, 1, &
       5, 4, 2, 3, 1, &
       3, 3, 4, 2, 1, &
       5, 4, 3, 2, 1], [5, 52])

  real(default) :: pts2_scale  

  abstract interface
     function trafo_in (in) 
       use kinds !NODEP!
       real(default), dimension(3) :: trafo_in
       real(default), dimension(3), intent(in) :: in
     end function trafo_in
  end interface
  abstract interface
     pure function coord_scalar_in (hyp)
       use kinds !NODEP!
       real(default) :: coord_scalar_in
       real(double), dimension(3), intent(in) :: hyp
     end function coord_scalar_in
  end interface
  abstract interface
     subroutine coord_hcd_in (hyp, cart, denom)
       use kinds !NODEP!
       real(default), dimension(3), intent(in) :: hyp
       real(default), dimension(3), intent(out) :: cart
       real(default), intent(out) :: denom
     end subroutine coord_hcd_in
  end interface
  interface
     pure function alphaspdf (Q)
       use kinds !NODEP!
       real(double) :: alphaspdf
       real(double), intent(in) :: Q
     end function alphaspdf
  end interface
  interface
     pure subroutine evolvepdf (x, q, f)
       use kinds !NODEP!
       real(double), intent(in) :: x, q
       real(double), intent(out), dimension(-6:6) :: f
     end subroutine evolvepdf
  end interface  
  

contains
        
  pure function muli_get_state_transformations &
       (inout_kind, lha_flavors) result (transformations)
    integer, intent(in) :: inout_kind
    integer, dimension(4), intent(in) :: lha_flavors
    integer, dimension(4) :: signature
    logical, dimension(3) :: transformations
    where (lha_flavors > 0)
       signature = 1
    elsewhere (lha_flavors < 0)
       signature = -1
    elsewhere
       signature = 0
    end where
    ! print *,"inout_kind=",inout_kind
    ! print *,"lha_flavors=",lha_flavors
    ! print *,"signature",signature
    if ((sum(inout_signatures(1:2,inout_kind)) == sum(signature(1:2))) .and. &
        (sum(inout_signatures(3:4,inout_kind)) == sum(signature(3:4)))) then
           transformations(1) = .false.
        else
           transformations(1) = .true.
           signature = -signature
        end if
    if (all (inout_signatures(1:2,inout_kind) == signature(1:2))) then
       transformations(2) = .false.
    else
       transformations(2) = .true.
    end if
    if (all(inout_signatures(3:4,inout_kind) == signature(3:4))) then
       transformations(3) = .false.
    else
       transformations(3) = .true.
    end if
    ! print *,"signature",signature
    ! print *,"transformations=",transformations
  end function muli_get_state_transformations
  
  pure function h_to_c_param (hyp)
    real(default), dimension(3) :: h_to_c_param
    real(default), dimension(3), intent(in) :: hyp
    h_to_c_param = [sqrt (sqrt ((((hyp(1)**4) * (one-hyp(3))) + &
         hyp(3))**2 + (((hyp(2)-(5E-1_default))**3)*4)**2) - &
         ((hyp(2)-(5E-1_default))**3)*4), &
         sqrt (sqrt ((((hyp(1)**4)*(one-hyp(3))) + hyp(3))**2 + &
         (((hyp(2)-(5E-1_default))**3)*4)**2) + &
         ((hyp(2)-(5E-1_default))**3)*4), hyp(3)]
  end function h_to_c_param
  
  pure function c_to_h_param (cart)
    real(default), dimension(3) :: c_to_h_param
    real(default), dimension(3), intent(in)::cart
    c_to_h_param= [ (((cart(1)*cart(2)) - cart(3)) / &
         (one - cart(3)))**(1/four), (one + sign(abs((cart(2)**2) - &
         (cart(1)**2))**(1/three), cart(2) - cart(1))) / two, cart(3) ]
  end function c_to_h_param
  
  pure function h_to_c_param_def (hyp)
    real(default), dimension(3) :: h_to_c_param_def
    real(default), dimension(3), intent(in) :: hyp
    h_to_c_param_def = h_to_c_param ([hyp(1), hyp(2), pts2_scale])
  end function h_to_c_param_def
    
  pure function h_to_c_ort (hyp)
    real(default), dimension(3) :: h_to_c_ort
    real(default), dimension(3), intent(in) :: hyp
    h_to_c_ort = [sqrt (sqrt (((hyp(1) * (one - hyp(3))) + hyp(3))**2 + &
         (hyp(2) - (5E-1_default))**2) - (hyp(2) - (5E-1_default))), &
         sqrt (sqrt (((hyp(1) * (one - hyp(3))) + hyp(3))**2 + &
         (hyp(2)-(5E-1_default))**2) + (hyp(2) - (5E-1_default))), hyp(3)]
  end function h_to_c_ort
  
  pure function c_to_h_ort (cart)
    real(default), dimension(3) :: c_to_h_ort
    real(default), dimension(3), intent(in) :: cart
    c_to_h_ort = [ (cart(3) - (cart(1)*cart(2))) / (cart(3) - one), &
         (one - cart(1)**2 + cart(2)**2) / two, cart(3)]
  end function c_to_h_ort
  
  pure function h_to_c_ort_def (hyp)
    real(default), dimension(3) :: h_to_c_ort_def
    real(default), dimension(3), intent(in) :: hyp
    h_to_c_ort_def = h_to_c_ort ([hyp(1), hyp(2), pts2_scale])
  end function h_to_c_ort_def
  
  pure function c_to_h_ort_def (cart)
    real(default), dimension(3) :: c_to_h_ort_def
    real(default), dimension(3), intent(in) :: cart
    c_to_h_ort_def = c_to_h_ort ([ cart(1), cart(2), pts2_scale])
  end function c_to_h_ort_def
    
  pure function h_to_c_noparam (hyp)
    real(default), dimension(2) :: h_to_c_noparam
    real(default), dimension(2), intent(in) :: hyp
    h_to_c_noparam = [sqrt (sqrt (hyp(1)**8 + (((hyp(2) - &
         (5E-1_default))**3)*4)**2) - ((hyp(2)-(5E-1_default))**3)*4), &
         sqrt (sqrt (hyp(1)**8 + (((hyp(2)-(5E-1_default))**3)*4)**2) + &
         ((hyp(2)-(5E-1_default))**3)*4)]
  end function h_to_c_noparam

  pure function c_to_h_noparam (cart)
    real(default), dimension(2) :: c_to_h_noparam
    real(default), dimension(2), intent(in) :: cart
    c_to_h_noparam = [sqrt (sqrt (cart(1)*cart(2))), &
         (one + sign(abs((cart(2)**2) - (cart(1)**2))**(one/three), &
         cart(2)-cart(1)))/two]
  end function c_to_h_noparam

  pure function c_to_h_param_def (cart)
    real(default), dimension(3) :: c_to_h_param_def
    real(default), dimension(3), intent(in) :: cart
    if (product (cart(1:2)) >= pts2_scale) then
       c_to_h_param_def = c_to_h_param ([cart(1), cart(2), pts2_scale])
    else
       c_to_h_param_def = [-one, -one, -one]
    end if
  end function c_to_h_param_def
  
  pure function h_to_c_smooth (hyp)
    real(default), dimension(3) :: h_to_c_smooth
    real(default), dimension(3), intent(in) :: hyp
    real(default) :: h2
    h2 = (((hyp(2) - 5E-1_default)**3) * 4._default + hyp(2)-5E-1_default) &
         / two
    h_to_c_smooth = &
        [sqrt (sqrt((((hyp(1)**4)*(one-hyp(3)))+hyp(3))**2+h2**2) - h2), &
         sqrt (sqrt((((hyp(1)**4)*(one-hyp(3)))+hyp(3))**2+h2**2) + h2), &
         hyp(3)]
  end function h_to_c_smooth
  
  pure function c_to_h_smooth (cart)
    real(default), dimension(3) :: c_to_h_smooth
    real(default), dimension(3), intent(in) :: cart
    c_to_h_smooth = &
         [((product (cart(1:2)) - cart(3)) / (one - cart(3)))**(1/four), &
          (three-three**(two/3) / (-9._default * cart(1)**2 + &
           9._default * cart(2)**2 + sqrt (three + 81._default * &
           (cart(1)**2 - cart(2)**2)**2))**(one/three)&
         + 3**(one/3)*(-9._default * cart(1)**2 + 9._default*cart(2)**2 &
         + sqrt(three + 81._default*(cart(1)**2&
         - cart(2)**2)**2))**(one/3))/6._default,cart(3)]
  end function c_to_h_smooth

  pure function h_to_c_smooth_def (hyp)
    real(default), dimension(3) :: h_to_c_smooth_def
    real(default), dimension(3), intent(in) :: hyp
    h_to_c_smooth_def = h_to_c_smooth ([hyp(1), hyp(2), pts2_scale])
  end function h_to_c_smooth_def
  
  pure function c_to_h_smooth_def (cart)
    real(default), dimension(3)::c_to_h_smooth_def
    real(default), dimension(3), intent(in) :: cart
    if (product (cart(1:2)) >= pts2_scale) then
       c_to_h_smooth_def = c_to_h_smooth ([cart(1), cart(2), pts2_scale])
    else
       c_to_h_smooth_def = [-one, -one, -one]
    end if
  end function c_to_h_smooth_def

  pure function voxel_h_to_c_ort (hyp)
    real(default) :: voxel_h_to_c_ort
    real(default), dimension(3), intent(in) :: hyp
    real(default) :: T, TH1
    T = one - hyp(3)
    TH1 = T * (one - hyp(1))
    voxel_h_to_c_ort = sqrt (T**2 / (five - four*(one-hyp(2))*hyp(2) - &    
         four*(two-TH1)*TH1))
  end function voxel_h_to_c_ort

  pure function voxel_c_to_h_ort(cart)
    real(default) :: voxel_c_to_h_ort
    real(default), dimension(3), intent(in) :: cart
    real(default) :: P
    P = product (cart(1:2))
    if (P > cart(3)) then
       voxel_c_to_h_ort = (cart(1)**2 + cart(2)**2) / (one -cart(3))
    else
       voxel_c_to_h_ort = zero
    end if
  end function voxel_c_to_h_ort

  pure function voxel_h_to_c_noparam (hyp)
    real(default) :: voxel_h_to_c_noparam
    real(default), dimension(3), intent(in) :: hyp
    voxel_h_to_c_noparam = 12._default * sqrt ((hyp(1)**6 * &
         (one - two*hyp(2))**4) / (4*hyp(1)**8 + (one - two*hyp(2))**6))
  end function voxel_h_to_c_noparam

  pure function voxel_c_to_h_noparam (cart)
    real(default) :: voxel_c_to_h_noparam
    real(default), dimension(3), intent(in) :: cart
    real(default) :: P
    voxel_c_to_h_noparam = (cart(1)**2 + cart(2)**2) / (12._default * &
         (cart(1)*cart(2))**(three/four) * &
         (cart(2)**2 + cart(1)**2)**(two/three))
  end function voxel_c_to_h_noparam

  pure function voxel_h_to_c_param (hyp)
    real(default) :: voxel_h_to_c_param
    real(default), dimension(3), intent(in) :: hyp
    voxel_h_to_c_param = 12*Sqrt((hyp(1)**6 * &
         (one - 2._default*hyp(2))**4 * (hyp(3) - one)**2) / &
         ((one - two * hyp(2))**6 + four * &
         (hyp(3)-(hyp(1)**4*(hyp(3)-one)))**2))
  end function voxel_h_to_c_param

  pure function voxel_c_to_h_param (cart)
    real(default)::voxel_c_to_h_param
    real(default), dimension(3), intent(in) :: cart
    real(default) :: P, T, CP, CM
    P = product (cart(1:2))
    if (P > cart(3)) then
       P = P - cart(3)
       CP = cart(1)**2 + cart(2)**2
       CM = abs(cart(2)**2 - cart(1)**2)
       T = 1 - cart(3)
       voxel_c_to_h_param = (Cp*sqrt(sqrt(P/T))) / (12*Cm**(two/three)*P)
    else
       voxel_c_to_h_param = zero
    end if
  end function voxel_c_to_h_param

  pure function voxel_h_to_c_smooth (hyp)
    real(default) :: voxel_h_to_c_smooth
    real(default), dimension(3), intent(in) :: hyp
    real(default) :: T
    T = one - hyp(3)
    voxel_h_to_c_smooth = 8._default * (hyp(1)**3 * (one + three * &
         (hyp(2) - one)*hyp(2))*T) / sqrt ((one - two*hyp(2) * (two + &
         hyp(2)*(two*hyp(2)-three)))**2 + &
         four * (one + (hyp(1)**4 - one)*T)**2) 
  end function voxel_h_to_c_smooth

  pure function voxel_c_to_h_smooth (cart)
    real(default) :: voxel_c_to_h_smooth
    real(default), dimension(3), intent(in) :: cart
    real(default) :: P, S, T, CM, CP
    P = product (cart(1:2))
    if (P > cart(3)) then
       P = P - cart(3)
       CP = cart(1)**2 + cart(2)**2
       CM = cart(2)**2 - cart(1)**2
       T = 1 - cart(3)
       S = sqrt(three + 81._default*cm**2)
       voxel_c_to_h_smooth = (three**(one/three) * Cp*(three**(one/three) + &
            (9._default*Cm + S)**(two/three)) * sqrt (sqrt (P/T))) / &
            (four * P * S * (9._default * Cm + S)**(one/three))
    else
       voxel_c_to_h_smooth = zero
    end if
end function voxel_c_to_h_smooth

  pure function voxel_h_to_c_ort_def (hyp)
    real(default) :: voxel_h_to_c_ort_def
    real(default), dimension(3), intent(in) :: hyp
    voxel_h_to_c_ort_def = voxel_h_to_c_ort (hyp)
  end function voxel_h_to_c_ort_def

  pure function voxel_c_to_h_ort_def (cart)
    real(default) :: voxel_c_to_h_ort_def
    real(default), dimension(3), intent(in) :: cart
    voxel_c_to_h_ort_def = voxel_c_to_h_ort (cart)
  end function voxel_c_to_h_ort_def

  pure function voxel_h_to_c_param_def (hyp)
    real(default) :: voxel_h_to_c_param_def
    real(default), dimension(3), intent(in) :: hyp
    voxel_h_to_c_param_def = voxel_h_to_c_param (hyp)
  end function voxel_h_to_c_param_def

  pure function voxel_c_to_h_param_def (cart)
    real(default) :: voxel_c_to_h_param_def
    real(default), dimension(3), intent(in) :: cart
    voxel_c_to_h_param_def = voxel_c_to_h_param (cart)
  end function voxel_c_to_h_param_def

  pure function voxel_h_to_c_smooth_def (hyp)
    real(default) :: voxel_h_to_c_smooth_def
    real(default), dimension(3), intent(in) :: hyp
    voxel_h_to_c_smooth_def = voxel_h_to_c_smooth (hyp)
  end function voxel_h_to_c_smooth_def

  pure function voxel_c_to_h_smooth_def (cart)
    real(default) :: voxel_c_to_h_smooth_def
    real(default), dimension(3), intent(in) :: cart
    voxel_c_to_h_smooth_def = voxel_c_to_h_smooth (cart)
  end function voxel_c_to_h_smooth_def

  pure function denom_cart (cart)
    real(default) :: denom_cart
    real(default), dimension(3), intent(in) :: cart
    denom_cart = 1._default / (864._default * sqrt (cart(3)**3 * &
         (1._default - cart(3) / product(cart(1:2)))))
  end function denom_cart

  pure function denom_ort (hyp)
    real(default) :: denom_ort
    real(default), dimension(3), intent(in) :: hyp
    real(default) :: Y, P
    Y = (one - two * hyp(2))**2
    P = one - hyp(3)
    if (hyp(1) > zero .and. hyp(3) > zero) then
       denom_ort = sqrt ((P + (-1 + Hyp(1))*P**2) / &
            (746496*hyp(1)*hyp(3)**3 * (4*(1 + (-1 + hyp(1))*P)**2 + Y)))
    else
       denom_ort = zero
    end if
  end function denom_ort
  
  pure function denom_param (hyp)
    real(default) :: denom_param
    real(default), dimension(3), intent(in) :: hyp
    real(default) :: X, Y, P
    X = hyp(1)**4
    Y = 1._default - 2._default * hyp(2)
    P = 1._default - hyp(3)
    if (hyp(3) > 0._default) then
       denom_param = sqrt ((P * (1+P*(X-1)) * Sqrt(X)*Y**4) / &
            (5184*(4*(1+P*(X-1))**2+Y**6)*hyp(3)**3))
    else
       denom_param = zero
    end if
  end function denom_param

  pure function denom_param_reg (hyp)
    real(default) :: denom_param_reg
    real(default), dimension(3), intent(in) :: hyp
    real(default) :: X, Y, P
    X = hyp(1)**4
    Y = one - two * hyp(2)
    P = one - hyp(3)
    if (hyp(3) > zero) then
       denom_param_reg = sqrt ((P*(1+P*(X-1)) * Sqrt(X)*Y**4) / &
            (5184*(4*(1+P*(X-1))**2+Y**6) * (hyp(3) + norm2_p_t_0)**3))
    else
       denom_param_reg = zero
    end if
  end function denom_param_reg

  pure function denom_smooth (hyp)
    real(default) :: denom_smooth
    real(default), dimension(3), intent(in) :: hyp
    real(default) :: X, Y, P
    X = hyp(1)**2
    Y = (one - two * hyp(2))**2
    P = one - hyp(3)
    if (hyp(3) > zero) then
       denom_smooth = sqrt ((P * X * (one + P*(-one + X**2)) * &
            (1 + three*Y)**2)/(46656*hyp(3)**3 &
            *(16*(1 + P*(-1 + X**2))**2 + Y + 2*Y**2 + Y**3)))
    else
       denom_smooth = zero
    end if
  end function denom_smooth

  pure function denom_smooth_reg (hyp)
    real(default) :: denom_smooth_reg
    real(default), dimension(3), intent(in) :: hyp
    real(default) :: X, Y, P
    X = hyp(1)**2
    Y = (one - two * hyp(2))**2
    P = one - hyp(3)
    if (hyp(3) > zero) then
       denom_smooth_reg = sqrt ((P * X * (1 + P*(-1 + X**2)) * &
            (1 + 3*Y)**2)/(46656*(hyp(3) + norm2_p_t_0)**3 * &
            (16 * (1 + P*(-1 + X**2))**2 + Y + 2*Y**2 + Y**3)))
    else
       denom_smooth_reg = zero
    end if
  end function denom_smooth_reg
  
   pure function denom_cart_save (cart)
    real(default) :: denom_cart_save
    real(default), dimension(3), intent(in) :: cart
    if (product(cart(1:2)) > cart(3)) then
       denom_cart_save = denom_cart (cart)
    else
       denom_cart_save = zero
    end if
  end function denom_cart_save

  pure function denom_ort_save (hyp)
    real(default) :: denom_ort_save
    real(default), dimension(3), intent(in) :: hyp
    real(default) :: Y, Z, W
    real(default), dimension(3) :: cart
    cart = h_to_c_ort (hyp)
    if (cart(1) > one .or. cart(2) > one) then
       denom_ort_save = zero
    else
       denom_ort_save = denom_ort (hyp)
    end if
  end function denom_ort_save

  pure function denom_param_save (hyp)
    real(default) :: denom_param_save
    real(default), dimension(3), intent(in) :: hyp
    real(default) :: Y, Z, W
    real(default), dimension(3) :: cart
    cart=h_to_c_param (hyp)
    if (cart(1) > one .or. cart(2) > one) then
       denom_param_save = zero
    else
       denom_param_save = denom_param (hyp)
    end if
  end function denom_param_save

  pure function denom_smooth_save (hyp)
    real(default) :: denom_smooth_save
    real(default), dimension(3), intent(in) :: hyp
    real(default) :: Y, Z, W
    real(default), dimension(3) :: cart
    cart = h_to_c_smooth (hyp)
    if (cart(1) > one .or. cart(2) > one) then
       denom_smooth_save = zero
    else
       denom_smooth_save = denom_smooth (hyp)
    end if
  end function denom_smooth_save

  subroutine denom_cart_cuba_int (d_cart, cart, d_denom, denom, pt2s)
    real(default), dimension(3), intent(in) :: cart
    real(default), dimension(1), intent(out) :: denom
    real(default), intent(in) :: pt2s
    integer, intent(in) :: d_cart, d_denom
    denom(1) = denom_cart_save ([cart(1), cart(2), pt2s])
  end subroutine denom_cart_cuba_int

  subroutine denom_ort_cuba_int (d_hyp, hyp, d_denom, denom, pt2s)
    real(default), dimension(3), intent(in) :: hyp
    real(default), dimension(1), intent(out) :: denom
    real(default), intent(in) :: pt2s
    integer, intent(in) :: d_hyp, d_denom
    denom(1) = denom_ort_save ([hyp(1), hyp(2), pt2s])
  end subroutine denom_ort_cuba_int

  subroutine denom_param_cuba_int (d_hyp, hyp, d_denom, denom, pt2s)
    real(default), dimension(3), intent(in) :: hyp
    real(default), dimension(1), intent(out) :: denom
    real(default), intent(in) :: pt2s
    integer, intent(in) :: d_hyp, d_denom
    denom(1) = denom_param_save ([hyp(1), hyp(2), pt2s])
  end subroutine denom_param_cuba_int

  subroutine denom_smooth_cuba_int (d_hyp, hyp, d_denom, denom, pt2s)
    real(default), dimension(3), intent(in) :: hyp
    real(default), dimension(1), intent(out) :: denom
    real(default), intent(in) :: pt2s
    integer, intent(in) :: d_hyp, d_denom
    denom(1) = denom_smooth_save ([hyp(1), hyp(2), pt2s])
  end subroutine denom_smooth_cuba_int

  subroutine coordinates_hcd_cart (hyp, cart, denom)
    real(default), dimension(3), intent(in) :: hyp
    real(default), dimension(3), intent(out) :: cart
    real(default), intent(out) :: denom
    cart = hyp
    denom = denom_cart_save (cart)
  end subroutine coordinates_hcd_cart

  subroutine coordinates_hcd_ort (hyp, cart, denom)
    real(default), dimension(3), intent(in) :: hyp
    real(default), dimension(3), intent(out) :: cart
    real(default), intent(out)::denom
    cart = h_to_c_ort (hyp)
    denom = denom_ort (hyp)
  end subroutine coordinates_hcd_ort

  subroutine coordinates_hcd_param (hyp, cart, denom)
    real(default), dimension(3), intent(in) :: hyp
    real(default), dimension(3), intent(out) :: cart
    real(default), intent(out) :: denom
    cart = h_to_c_param (hyp)
    denom = denom_param (hyp)
  end subroutine coordinates_hcd_param

  subroutine coordinates_hcd_param_reg (hyp, cart, denom)
    real(default), dimension(3), intent(in) :: hyp
    real(default), dimension(3), intent(out) :: cart
    real(default), intent(out) :: denom
    cart = h_to_c_param (hyp)
    denom = denom_param_reg (hyp)
  end subroutine coordinates_hcd_param_reg

  subroutine coordinates_hcd_smooth (hyp, cart, denom)
    real(default), dimension(3), intent(in) :: hyp
    real(default), dimension(3), intent(out) :: cart
    real(default), intent(out) :: denom
    cart = h_to_c_smooth (hyp)
    denom = denom_smooth (hyp)
  end subroutine coordinates_hcd_smooth
  
  subroutine coordinates_hcd_smooth_reg (hyp, cart, denom)
    real(default), dimension(3), intent(in) :: hyp
    real(default), dimension(3), intent(out) :: cart
    real(default), intent(out) :: denom
    cart = h_to_c_smooth (hyp)
    denom = denom_smooth_reg (hyp)
  end subroutine coordinates_hcd_smooth_reg

  pure subroutine interactions_dddsigma_reg &
       (process_id, double_pdf_id, hyp, cart, dddsigma)
    real(default), intent(out) :: dddsigma
    integer, intent(in) :: process_id, double_pdf_id
    real(default), dimension(3), intent(in) :: hyp
    real(default), dimension(3), intent(out) :: cart
    real(default) :: a, pt2shat, gev_pt, gev2_pt
    cart = h_to_c_param (hyp)
    a = product (cart(1:2))
    if (cart(1) <= 1D0 .and. cart(2) <= 1D0) then
       pt2shat = hyp(3) / a
       gev_pt = sqrt(hyp(3)) * gev_pt_max
       gev2_pt = hyp(3) * gev2_pt_max
       ! print *,process_id,pt2shat
       dddsigma = &
            const_pref &
            * alphasPDF (dble (sqrt (gev2_pt+gev2_p_t_0)))**2 &
            * ps_io_pol (process_id, pt2shat) &
            * pdf_in_in_kind &
               (process_id, double_pdf_id, cart(1), cart(2), gev_pt) &
            * denom_param_reg (hyp) / a
    else
       dddsigma = zero
    end if
  end subroutine interactions_dddsigma_reg
  
  pure function pdf_in_in_kind (process_id, double_pdf_id, c1, c2, gev_pt)
    real(default) :: pdf_in_in_kind
    real(default), intent(in) :: c1, c2, gev_pt
    integer, intent(in) :: process_id, double_pdf_id
    real(default) :: pdf1, pdf2
    call single_pdf (valid_processes(1, process_id), &
         double_pdf_kinds(1, double_pdf_id), c1, gev_pt, pdf1)
    call single_pdf (valid_processes(2, process_id), &
         double_pdf_kinds(2, double_pdf_id), c2, gev_pt, pdf2)
    pdf_in_in_kind = pdf1 * pdf2
  contains
    pure subroutine single_pdf (flavor, pdf_kind, c, gev_pt, pdf)
      integer, intent(in) :: flavor, pdf_kind
      real(default), intent(in) :: c, gev_pt
      real(default), intent(out) :: pdf
      real(double), dimension(-6:6) :: lha_pdf
      call evolvePDF (dble (c), dble (gev_pt), lha_pdf)
      select case (pdf_kind)
      case (1)
         pdf = lha_pdf (0)
      case (2)
         if (flavor==1 .or. flavor==2) then
            pdf = lha_pdf (-flavor)
         else
            pdf = lha_pdf (flavor)
         end if
      case (3)
         pdf = lha_pdf(1) - lha_pdf(-1)
      case (4)
         pdf = lha_pdf(2) - lha_pdf(-2)
      end select
    end subroutine single_pdf
  end function pdf_in_in_kind
  
  elemental function ps_io_pol (process_io_id, pt2shat)
    real(default) :: ps_io_pol
    integer, intent(in) :: process_io_id
    real(default), intent(in) :: pt2shat
    ps_io_pol = dot_product([1._default, pt2shat, pt2shat**2, pt2shat**3], &
         phase_space_coefficients_inout (1:4, &
         valid_processes (6, process_io_id)))
  end function ps_io_pol

  pure subroutine interactions_dddsigma &
       (process_id, double_pdf_id, hyp, cart, dddsigma)
    real(default), intent(out) :: dddsigma
    integer, intent(in) :: process_id, double_pdf_id
    real(default), dimension(3), intent(in) :: hyp
    real(default), dimension(3), intent(out) :: cart
    real(default) :: a, pt2shat, gev_pt
    cart = h_to_c_param (hyp)
    a = product (cart(1:2))
    if (cart(1) <= 1._default .and. cart(2) <= 1._default) then
       pt2shat = hyp(3) / a
       gev_pt = sqrt(hyp(3)) * gev_pt_max
       ! print *,process_id,pt2shat
       dddsigma = const_pref * &
            alphasPDF (dble (gev_pt))**2 * &
            ps_io_pol (process_id, pt2shat) * &
            pdf_in_in_kind &
               (process_id, double_pdf_id, cart(1), cart(2), gev_pt) * &
            denom_param(hyp) / a
    else
       dddsigma = zero
    end if
  end subroutine interactions_dddsigma

  subroutine interactions_dddsigma_print &
       (process_id, double_pdf_id, hyp, cart, dddsigma)
    real(default), intent(out) :: dddsigma
    integer, intent(in) :: process_id, double_pdf_id
    real(default), dimension(3), intent(in) :: hyp
    real(default), dimension(3), intent(out) :: cart
    real(default) :: a, pt2shat, gev_pt
    cart = h_to_c_param (hyp)
    a = product (cart(1:2))
    if (cart(1) <= 1._default .and. cart(2) <= 1._default) then
       pt2shat = hyp(3) / a
       gev_pt=sqrt(hyp(3))*gev_pt_max
       ! print *,process_id,pt2shat
       dddsigma = const_pref * &
            ! alphasPDF(dble (gev_pt))**2 * &
            ps_io_pol (process_id, pt2shat) * &
            pdf_in_in_kind &
               (process_id, double_pdf_id, cart(1), cart(2), gev_pt) * &
            denom_param (hyp) / a
    else
       dddsigma = zero
    end if
    write(11, *) dddsigma, pt2shat, &
         pdf_in_in_kind (process_id, double_pdf_id, cart(1), cart(2), &
         gev_pt), ps_io_pol (process_id, pt2shat), const_pref, &
         denom_param(hyp), a
    flush(11)
  end subroutine interactions_dddsigma_print

  pure subroutine interactions_dddsigma_cart &
       (process_id, double_pdf_id, cart, dddsigma)
    real(default), intent(out) :: dddsigma
    integer, intent(in) :: process_id, double_pdf_id
    real(default), dimension(3), intent(in) :: cart
    real(default) :: a, pt2shat, gev_pt
    a = product (cart(1:2))
    if (cart(1) <= one .and. cart(2) <= one) then
       pt2shat = cart(3) / a
       gev_pt = sqrt(cart(3)) * gev_pt_max
       ! print *,process_id,pt2shat
       dddsigma = const_pref * &
            alphasPDF (dble (gev_pt))**2 * &
            ps_io_pol (process_id, pt2shat) * &
            pdf_in_in_kind &
               (process_id, double_pdf_id, cart(1), cart(2), gev_pt) * &
            denom_cart (cart) / a
    else
       dddsigma = zero
    end if
  end subroutine interactions_dddsigma_cart

  subroutine cuba_gg_me_smooth (d_hyp, hyp, d_me, me, pt2s)
    integer, intent(in) :: d_hyp, d_me
    real(default), dimension(d_hyp), intent(in) :: hyp
    real(default), dimension(1), intent(out) :: me
    real(default), dimension(3) :: cart
    real(default), intent(in) :: pt2s
    real(default) :: p, p2
    if (d_hyp == 3) then
       p = hyp(3)
       p2 = hyp(3)**2
    else
       if (d_hyp == 2) then
          p = sqrt (pt2s)
          p2 = pt2s
       end if
    end if
    cart = h_to_c_smooth ([hyp(1), hyp(2), p2])
    if (p > pts_min .and. product (cart(1:2)) > p2) then
       me(1) = const_pref * &
            alphasPDF (dble (p*gev_pt_max))**2 * &
            ps_io_pol (109, p2) * &
            pdf_in_in_kind (109, 11, cart(1), cart(2), p2) * &
            denom_smooth ([hyp(1), hyp(2), p2]) / product (cart(1:2))
    else
       me(1) = zero
    end if
  end subroutine cuba_gg_me_smooth

  subroutine cuba_gg_me_param (d_hyp, hyp, d_me, me, pt2s)
    integer, intent(in)::d_hyp,d_me
    real(default), dimension(d_hyp), intent(in) :: hyp
    real(default), dimension(1), intent(out) :: me
    real(default), dimension(3) :: cart
    real(default), intent(in) :: pt2s
    real(default) :: p, p2
    if (d_hyp == 3) then
       p = hyp(3)
       p2 = hyp(3)**2
    else
       if (d_hyp == 2) then
          p = sqrt (pt2s)
          p2 = pt2s
       end if
    end if
    cart = h_to_c_param ([hyp(1), hyp(2), p2])
    if (p>pts_min .and. product (cart(1:2))>p2) then
       me(1) = const_pref * &
            alphasPDF(dble (p*gev_pt_max))**2 * &
            ps_io_pol (109, p2) * &
            pdf_in_in_kind (109, 11, cart(1), cart(2), p2) * &
            denom_param ([hyp(1), hyp(2), p2]) / product(cart(1:2))
    else
       me(1) = zero
    end if
  end subroutine cuba_gg_me_param

  subroutine cuba_gg_me_ort (d_hyp, hyp, d_me, me, pt2s)
    integer, intent(in) :: d_hyp, d_me
    real(default), dimension(d_hyp), intent(in) :: hyp
    real(default), dimension(1), intent(out) :: me
    real(default), dimension(3) :: cart
    real(default), intent(in) :: pt2s
    real(default) :: p, p2
    if (d_hyp == 3) then
       p = hyp(3)
       p2 = hyp(3)**2
    else
       if (d_hyp == 2) then
          p = sqrt(pt2s)
          p2 = pt2s
       end if
    end if
    cart = h_to_c_ort ([hyp(1), cart(2), p2])
    if (p > pts_min .and. product (cart(1:2)) > p2) then
       me(1) = const_pref * &
            alphasPDF(dble (p*gev_pt_max))**2 * &
            ps_io_pol (109, p2) * &
            pdf_in_in_kind (109, 11, cart(1), cart(2), p2) * &
            denom_ort ([hyp(1), hyp(2), p2]) / product (cart(1:2))
    else
       me(1) = zero
    end if
  end subroutine cuba_gg_me_ort

  subroutine cuba_gg_me_cart (d_cart, cart, d_me, me, pt2s)
    integer, intent(in) :: d_cart, d_me
    real(default), dimension(d_cart), intent(in) :: cart
    real(default), dimension(1), intent(out) :: me
    real(default), intent(in) :: pt2s
    real(default) :: a, p, p2
    if (d_cart == 3) then
       p = cart(3)
       p2 = cart(3)**2
    else
       if (d_cart == 2) then
          p = sqrt (pt2s)
          p2 = pt2s
       end if
    end if
    a = product (cart(1:2))
    if (p > pts_min .and. a > p2) then
       me(1) = const_pref * &
            alphasPDF (dble (p*gev_pt_max))**2 * &
            ps_io_pol (109, p2) * &
            pdf_in_in_kind (109, 11, cart(1), cart(2), p2) * &
            denom_cart ([cart(1), cart(2), p2]) / a
    else
       me(1) = zero
    end if
  end subroutine cuba_gg_me_cart

  subroutine interactions_proton_proton_integrand_generic_17_reg & 
       (hyp_2, trafo, f, pt)
    real(default), dimension(2), intent(in) :: hyp_2
    procedure(coord_hcd_in) :: trafo
    real(default), dimension(17), intent(out) :: f
    class(transverse_mom_t), intent(in) :: pt
    real(default), dimension(3) :: cart, hyp_3
    real(default), dimension(5) :: psin
    real(double), dimension(-6:6) :: c_dble, d_dble
    real(default), dimension(-6:6) :: c, d
    real(default) :: gev_pt, gev2_pt, pts, pt2s, pt2shat, a, &
         pdf_seaquark_seaquark, pdf_seaquark_gluon, pdf_gluon_gluon, &
         pdf_up_seaquark, pdf_up_gluon, pdf_down_seaquark, pdf_down_gluon, &
         v1u, v1d, v2u, v2d, denom
        
    pts = pt%get_unit_scale()
    pt2s = pt%get_unit2_scale()
    gev_pt = pt%get_gev_scale()
    gev2_pt = pt%get_gev2_scale()

    hyp_3(1:2) = hyp_2
    hyp_3(3) = pt2s
    call trafo (hyp_3, cart, denom)
    a = product (cart(1:2))
    if (cart(1) <= one .and. cart(2) <= one .and. a > pt2s) then
       pt2shat = pt2s / a
       ! phase space polynom
       psin = matmul ([one, pt2shat, pt2shat**2, pt2shat**3], &
            phase_space_coefficients_in)
       ! pdf
       call evolvepdf (dble (cart(1)), dble (gev_pt), c_dble)
       call evolvepdf (dble (cart(2)), dble (gev_pt), d_dble)
       c = c_dble
       d = d_dble
       ! c = [1,1,1,1,1,1,1,1,1,1,1,1,1]*1D0
       ! d = c
       v1d = c(1) - c(-1)
       v1u = c(2) - c(-2)
       v2d = d(1) - d(-1)
       v2u = d(2) - d(-2)
       c(1) = c(-1)
       c(2) = c(-2)
       d(1) = d(-1)
       d(2) = d(-2)
       f(1) = zero
       !!! gluon_gluon
       f( 2) = (c(0)*d(0)) * psin(5)
            !!! type5
       !!! gluon_seaquark
       f( 3) = (c(0)*d(-4) + c(0)*d(-3) + c(0)*d(-2) + c(0)*d(-1) + &
                c(0)*d(1)  + c(0)*d(2)  + c(0)*d(3)  + c(0)*d(4)) * psin(4)
            !!! type4
       !!! gluon_down
       f( 4) = (c(0)*v2d) * psin(4)
            !!! type4
       !!! gluon_up
       f( 5) = (c(0)*v2u) * psin(4)
            !!! type4
       !!! seaquark_gluon
       f( 6) = (c(-4)*d(0) + c(-3)*d(0) + c(-2)*d(0) + c(-1)*d(0) + &
                c(1)*d(0)  + c(2)*d(0)  + c(3)*d(0)  + c(4)*d(0)) * psin(4)
            !!! type4
       !!! seaquark_seaquark
       f( 7) = &
            !!! type1
               (c(-4)*d(-3) + c(-4)*d(-2) + c(-4)*d(-1) + c(-4)*d( 1) + &
                c(-4)*d( 2) + c(-4)*d( 3) + c(-3)*d(-4) + c(-3)*d(-2) + &
                c(-3)*d(-1) + c(-3)*d( 1) + c(-3)*d( 2) + c(-3)*d( 4) + &
                c(-2)*d(-4) + c(-2)*d(-3) + c(-2)*d(-1) + c(-2)*d( 1) + &
                c(-2)*d( 3) + c(-2)*d( 4) + c(-1)*d(-4) + c(-1)*d(-3) + &
                c(-1)*d(-2) + c(-1)*d( 2) + c(-1)*d( 3) + c(-1)*d( 4) + &
                c( 1)*d(-4) + c( 1)*d(-3) + c( 1)*d(-2) + c( 1)*d( 2) + &
                c( 1)*d( 3) + c( 1)*d( 4) + c( 2)*d(-4) + c( 2)*d(-3) + &
                c( 2)*d(-1) + c( 2)*d( 1) + c( 2)*d( 3) + c( 2)*d( 4) + &
                c( 3)*d(-4) + c( 3)*d(-2) + c( 3)*d(-1) + c( 3)*d( 1) + &
                c( 3)*d( 2) + c( 3)*d( 4) + c( 4)*d(-3) + c( 4)*d(-2) + &
                c( 4)*d(-1) + c( 4)*d( 1) + c( 4)*d( 2) + c( 4)*d( 3)) * &
                psin(1) + &
            !!! type2
               (c(-4)*d(-4) + c(-3)*d(-3) + c(-2)*d(-2) + c(-1)*d(-1) + &
                c( 4)*d( 4) + c( 3)*d( 3) + c(2)*d( 2)  + c(1)*d( 1)) * &
                psin(2) + &
            !!! type3
               (c(-4)*d( 4) + c(-3)*d( 3) + c(-2)*d( 2) + c(-1)*d( 1) + &
                c( 4)*d(-4) + c( 3)*d(-3) + c(2)*d(-2)  + c(1)*d(-1)) * &
                psin(3)
       !!! seaquark_down
       f( 8) = &
            !!! type1
               (c(-4)*v2d + c(-3)*v2d + c(-2)*v2d + c( 2)*v2d + &
                c( 3)*v2d + c( 4)*v2d) * psin(1) + &
            !!! type2
                c( 1)*v2d * psin(2) + &
            !!! type3
                c(-1)*v2d * psin(3)
       !!! seaquark_up
       f( 9) = &
            !!! type1
               (c(-4)*v2u + c(-3)*v2u + c(-1)*v2u + c( 1)*v2u + &
                c( 3)*v2u + c( 4)*v2u) * psin(1) + &
            !!! type2
                c(2)*v2u * psin(2) + &
            !!! type3
                c(-2)*v2u * psin(3)
       !!! down_gluon
       f(10) = (v1d*d( 0)) * psin(4)
            !!! type4
       !!! down_seaquark
       f(11) = &
            !!! type1
               (v1d*d(-4) + v1d*d(-3) + v1d*d(-2) + v1d*d( 2) + &
                v1d*d( 3) + v1d*d( 4)) * psin(1) + &
            !!! type2
                v1d*d( 1) * psin(2) + &
            !!! type3
                v1d*d(-1) * psin(3)
       !!! down_down
       f(12) = v1d*v2d * psin(2)
       !!! down_up
       f(13) = v1d*v2u * psin(1)
       !!! up_gluon
       f(14) = (v1u*d(0)) * psin(4)
            !!! type4
       !!! up_seaquark
       f(15) = &
            !!! type1
               (v1u*d(-4) + v1u*d(-3) + v1u*d(-1) + v1u*d( 1) + &
                v1u*d( 3) + v1u*d( 4)) * psin(1) + &
            !!! type2
                v1u*d(2) * psin(2) + &
            !!! type3
                v1u*d(-2) * psin(3)
       !!! up_down
       f(16) = v1u * v2d * psin(1)
       !!! up_up
       f(17) = v1u * v2u * psin(2)
       f=f * const_pref &
           * alphasPDF (dble (sqrt(gev2_pt+gev2_p_t_0)))**2 &
           * denom / a
       ! print *, const_pref, alphasPDF(gev_pt)**2, denom_smooth (hyp), a
    else
       f = [zero, zero, zero, zero, zero, zero, zero, zero, zero, &
            zero, zero, zero, zero, zero, zero, zero, zero]
    end if
    ! print *, pt2shat, c(0)*d(0), psin(5), const_pref, &
    !     alphasPDF(gev_pt)**2, denom, a
  end subroutine interactions_proton_proton_integrand_generic_17_reg

  !  subroutine coordinates_proton_proton_integrand_cart_11 &
  !       (d_hyp, hyp_2, d_f, f)
  !    integer, intent(in) :: d_hyp, d_f
  !    real(default), dimension(2), intent(in) :: hyp_2
  !    real(default), dimension(11), intent(out) :: f
  !    call coordinates_proton_proton_integrand_generic_11 &
  !       (hyp_2, coordinates_hcd_cart, f)
  !    ! write (51,*) hyp_2, momentum_get_pts_scale(), f
  !  end subroutine coordinates_proton_proton_integrand_cart_11

  !  subroutine coordinates_proton_proton_integrand_ort_11 &
  !       (d_hyp, hyp_2, d_f, f)
  !    integer, intent(in) :: d_hyp, d_f
  !    real(default), dimension(2), intent(in) :: hyp_2
  !    real(default), dimension(11), intent(out) :: f
  !    call coordinates_proton_proton_integrand_generic_11 &
  !       (hyp_2, coordinates_hcd_ort, f)
  !    ! write (52,*) hyp_2, momentum_get_pts_scale(), f
  !  end subroutine coordinates_proton_proton_integrand_ort_11

  !  subroutine coordinates_proton_proton_integrand_param_11 &
  !       (d_hyp, hyp_2, d_f, f)
  !    integer, intent(in) :: d_hyp, d_f
  !    real(default), dimension(2), intent(in) :: hyp_2
  !    real(default), dimension(11), intent(out) :: f
  !    call coordinates_proton_proton_integrand_generic_11 &
  !       (hyp_2, coordinates_hcd_param, f)
  !    ! write(53,*) hyp_2, momentum_get_pts_scale(), f
  !  end subroutine coordinates_proton_proton_integrand_param_11

  !  subroutine coordinates_proton_proton_integrand_smooth_11 &
  !       (d_hyp, hyp_2, d_f, f)
  !    integer, intent(in)::d_hyp,d_f
  !    real(default), dimension(2), intent(in) :: hyp_2
  !    real(default), dimension(11), intent(out) :: f
  !    call coordinates_proton_proton_integrand_generic_11 (hyp_2, &
  !       coordinates_hcd_smooth, f)
  !    ! write (54,*) hyp_2, momentum_get_pts_scale(), f
  !  end subroutine coordinates_proton_proton_integrand_smooth_11

  subroutine interactions_proton_proton_integrand_param_17_reg &
       (d_hyp, hyp_2, d_f, f, pt)
    integer, intent(in) :: d_hyp, d_f
    real(default), dimension(2), intent(in) :: hyp_2
    real(default), dimension(17), intent(out) :: f
    class(transverse_mom_t), intent(in) :: pt
    call interactions_proton_proton_integrand_generic_17_reg &
         (hyp_2, coordinates_hcd_param_reg, f, pt)
    ! write (53,*)  hyp_2,momentum_get_pts_scale(),f
  end subroutine interactions_proton_proton_integrand_param_17_reg
  
  subroutine interactions_proton_proton_integrand_smooth_17_reg &
       (d_hyp, hyp_2, d_f, f, pt)
    integer, intent(in) :: d_hyp, d_f
    real(default), dimension(2), intent(in) :: hyp_2
    real(default), dimension(17), intent(out) :: f
    class(transverse_mom_t), intent(in) :: pt
    call interactions_proton_proton_integrand_generic_17_reg &
         (hyp_2, coordinates_hcd_smooth_reg, f, pt)
    ! write (53,*)hyp_2,momentum_get_pts_scale(), f
  end subroutine interactions_proton_proton_integrand_smooth_17_reg
  
  
end module muli_interactions

