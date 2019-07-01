!!! module: muli_interactions
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
!!! Latest Change: 2011-06-09 13:50:58 CEST(+0200)
!!! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!! This file contains the module "muli_interactions". The allowed interactions
!!! and their cross sections are defined here. Additionaly, some coordinate
!!! transformations which annihilate divergencies of the cross sections are
!!! defined. Since the phase space border is hyperbolic, this transformations
!!! are also hyperbolic. That's why all interactions are named x_cart for
!!! cartesian or x_hyp for hyperbolic to avoid confusion.

module muli_interactions
  use kinds!NODEP!
  use muli_momentum
  implicit none
  !process parameters
  integer,parameter::hadron_A_kind=2212  ! Proton
  integer,parameter::hadron_B_kind=-2212 ! Anti Proton
  integer,dimension(4),parameter::parton_kind_of_int_kind=[1,1,2,2]
  real(kind=double), parameter :: b_sigma_tot_all = 100 !mb PDG
  real(kind=double), parameter :: b_sigma_tot_nd = 0.5*b_sigma_tot_all !phys.rev.d v49 n5 1994
  real(kind=double), parameter :: gev_cme_tot = 14000 !total center of mass energie
  real(kind=double), parameter :: gev2_cme_tot = gev_cme_tot**2 !s
  real(kind=double), parameter :: gev_pt_max = gev_cme_tot/2D0
  real(kind=double), parameter :: gev2_pt_max = gev2_cme_tot/4D0
  !model parameters
  real(kind=double), parameter :: gev_pt_min = 8D-1
  real(kind=double), parameter :: gev2_pt_min = gev_pt_min**2
  real(kind=double), parameter :: pts_min = gev_pt_min/gev_pt_max
  real(kind=double), parameter :: pts2_min = gev2_pt_min/gev2_pt_max
  real(kind=double), parameter :: gev_p_t_0 = 2.0
  real(kind=double), parameter :: gev2_p_t_0 = gev_p_t_0**2
  real(kind=double), parameter :: norm_p_t_0 = gev_p_t_0/gev_pt_max
  real(kind=double), parameter :: norm2_p_t_0 = gev2_p_t_0/gev2_pt_max
  !mathematical constants
  real(kind=double),private,parameter :: pi = 3.14159265
  real(kind=double),parameter         :: euler = exp(1D0)
  !physical constants
  real(kind=double), parameter :: gev2_mbarn = 0.389379304D0
  real(kind=double), parameter :: const_pref=pi*gev2_mbarn/(gev2_cme_tot*b_sigma_tot_nd)
    !parton kind parameters
  integer,parameter,public::lha_flavor_at=-6
  integer,parameter,public::lha_flavor_ab=-5
  integer,parameter,public::lha_flavor_ac=-4
  integer,parameter,public::lha_flavor_as=-3
  integer,parameter,public::lha_flavor_au=-2
  integer,parameter,public::lha_flavor_ad=-1
  integer,parameter,public::lha_flavor_g=0
  integer,parameter,public::lha_flavor_d=1
  integer,parameter,public::lha_flavor_u=2
  integer,parameter,public::lha_flavor_s=3
  integer,parameter,public::lha_flavor_c=4
  integer,parameter,public::lha_flavor_b=5
  integer,parameter,public::lha_flavor_t=6
  integer,parameter,public::pdg_flavor_at=-6
  integer,parameter,public::pdg_flavor_ab=-5
  integer,parameter,public::pdg_flavor_ac=-4
  integer,parameter,public::pdg_flavor_as=-3
  integer,parameter,public::pdg_flavor_au=-2
  integer,parameter,public::pdg_flavor_ad=-1
  integer,parameter,public::pdg_flavor_g=21
  integer,parameter,public::pdg_flavor_d=1
  integer,parameter,public::pdg_flavor_u=2
  integer,parameter,public::pdg_flavor_s=3
  integer,parameter,public::pdg_flavor_c=4
  integer,parameter,public::pdg_flavor_b=5
  integer,parameter,public::pdg_flavor_t=6
  integer,parameter,public::parton_kind_sea=1
  integer,parameter,public::parton_kind_valence=2
  integer,parameter,public::parton_kind_sea_and_valence=3
  integer,parameter,public::parton_kind_twin=4
  integer,parameter,public::parton_kind_sea_and_twin=5
  integer,parameter,public::parton_kind_valence_and_twin=6
  integer,parameter,public::parton_kind_all=7
  integer,parameter,public::pdf_int_kind_undef=0
  integer,parameter,public::pdf_int_kind_gluon=1
  integer,parameter,public::pdf_int_kind_sea=2
  integer,parameter,public::pdf_int_kind_val_down=3
  integer,parameter,public::pdf_int_kind_val_up=4
  integer,parameter,public::pdf_int_kind_twin=5
  character(len=2),dimension(-6:6),parameter :: integer_parton_names = &
       &["-6","-5","-4","-3","-2","-1","00","+1","+2","+3","+4","+5","+6"]
  character,dimension(-6:6),parameter :: traditional_parton_names = &
       &["T","B","C","S","U","D","g","d","u","s","c","b","t"]
  !ps polynom coefficients
  ! evolution variable is pt2s/(x1*x2)
  real(kind=double),dimension(1:4,1:5),parameter :: phase_space_coefficients_in = reshape(source=[&
       & 6144D0, -4608D0,  +384D0,    0D0,&
       & 6144D0, -5120D0,  +384D0,    0D0,&
       & 6144D0, -2048D0,  +128D0, -576D0,&
       &13824D0, -9600D0, +1056D0,    0D0,&
       &31104D0,-19872D0, +2160D0, +486D0],&
       &shape=[4,5])

  ! evolution variable is pt2s/(x1*x2)
  real(kind=double),dimension(1:4,1:8),parameter :: phase_space_coefficients_inout = reshape(source=[&
       &3072,  -2304,  +192,    0, &
       &6144,  -5120,  +384,    0, &
       &0,         0,   192,  -96, &
       &3072,  -2048,  +192,  -96, & 
       &0,      2048, -2176, +576, & 
       &0,       288,  -306,  +81, &
       &6912,  -4800,  +528,    0, &
       &31104,-23328, +5832, -486],&
       &shape=[4,8])

  integer,dimension(1:4,0:8),parameter :: inout_signatures = reshape(source=[&
        1, 1, 1, 1,&!1a
       -1, 1,-1, 1,&!1b
        1, 1, 1, 1,&!2
        1,-1, 1,-1,&!3
        1,-1, 1,-1,&!4
        1,-1, 0, 0,&!5
        0, 0, 1,-1,&!6
        1, 0, 1, 0,&!7
        0, 0, 0, 0],&
        shape=[4,9])

  integer,dimension(6,-234:234),parameter::valid_processes=reshape([&
 -6,  -6,  -6,  -6,   2,   2,&!-234
 -6,  -5,  -6,  -5,   1,   1,&!-233
 -6,  -5,  -5,  -6,   1,   1,&!-232
 -6,  -4,  -6,  -4,   1,   1,&!-231
 -6,  -4,  -4,  -6,   1,   1,&!-230
 -6,  -3,  -6,  -3,   1,   1,&!-229
 -6,  -3,  -3,  -6,   1,   1,&!-228
 -6,  -2,  -6,  -2,   1,   1,&!-227
 -6,  -2,  -2,  -6,   1,   1,&!-226
 -6,  -1,  -6,  -1,   1,   1,&!-225
 -6,  -1,  -1,  -6,   1,   1,&!-224
 -6,   0,  -6,   0,   4,   7,&!-223
 -6,   0,   0,  -6,   4,   7,&!-222
 -6,   1,  -6,   1,   1,   1,&!-221
 -6,   1,   1,  -6,   1,   1,&!-220
 -6,   2,  -6,   2,   1,   1,&!-219
 -6,   2,   2,  -6,   1,   1,&!-218
 -6,   3,  -6,   3,   1,   1,&!-217
 -6,   3,   3,  -6,   1,   1,&!-216
 -6,   4,  -6,   4,   1,   1,&!-215
 -6,   4,   4,  -6,   1,   1,&!-214
 -6,   5,  -6,   5,   1,   1,&!-213
 -6,   5,   5,  -6,   1,   1,&!-212
 -6,   6,  -6,   6,   3,   4,&!-211
 -6,   6,  -5,   5,   3,   3,&!-210
 -6,   6,  -4,   4,   3,   3,&!-209
 -6,   6,  -3,   3,   3,   3,&!-208
 -6,   6,  -2,   2,   3,   3,&!-207
 -6,   6,  -1,   1,   3,   3,&!-206
 -6,   6,   0,   0,   3,   5,&!-205
 -6,   6,   1,  -1,   3,   3,&!-204
 -6,   6,   2,  -2,   3,   3,&!-203
 -6,   6,   3,  -3,   3,   3,&!-202
 -6,   6,   4,  -4,   3,   3,&!-201
 -6,   6,   5,  -5,   3,   3,&!-200
 -6,   6,   6,  -6,   3,   4,&!-199
 -5,  -6,  -6,  -5,   1,   1,&!-198
 -5,  -6,  -5,  -6,   1,   1,&!-197
 -5,  -5,  -5,  -5,   2,   2,&!-196
 -5,  -4,  -5,  -4,   1,   1,&!-195
 -5,  -4,  -4,  -5,   1,   1,&!-194
 -5,  -3,  -5,  -3,   1,   1,&!-193
 -5,  -3,  -3,  -5,   1,   1,&!-192
 -5,  -2,  -5,  -2,   1,   1,&!-191
 -5,  -2,  -2,  -5,   1,   1,&!-190
 -5,  -1,  -5,  -1,   1,   1,&!-189
 -5,  -1,  -1,  -5,   1,   1,&!-188
 -5,   0,  -5,   0,   4,   7,&!-187
 -5,   0,   0,  -5,   4,   7,&!-186
 -5,   1,  -5,   1,   1,   1,&!-185
 -5,   1,   1,  -5,   1,   1,&!-184
 -5,   2,  -5,   2,   1,   1,&!-183
 -5,   2,   2,  -5,   1,   1,&!-182
 -5,   3,  -5,   3,   1,   1,&!-181
 -5,   3,   3,  -5,   1,   1,&!-180
 -5,   4,  -5,   4,   1,   1,&!-179
 -5,   4,   4,  -5,   1,   1,&!-178
 -5,   5,  -6,   6,   3,   3,&!-177
 -5,   5,  -5,   5,   3,   4,&!-176
 -5,   5,  -4,   4,   3,   3,&!-175
 -5,   5,  -3,   3,   3,   3,&!-174
 -5,   5,  -2,   2,   3,   3,&!-173
 -5,   5,  -1,   1,   3,   3,&!-172
 -5,   5,   0,   0,   3,   5,&!-171
 -5,   5,   1,  -1,   3,   3,&!-170
 -5,   5,   2,  -2,   3,   3,&!-169
 -5,   5,   3,  -3,   3,   3,&!-168
 -5,   5,   4,  -4,   3,   3,&!-167
 -5,   5,   5,  -5,   3,   4,&!-166
 -5,   5,   6,  -6,   3,   3,&!-165
 -5,   6,  -5,   6,   1,   1,&!-164
 -5,   6,   6,  -5,   1,   1,&!-163
 -4,  -6,  -6,  -4,   1,   1,&!-162
 -4,  -6,  -4,  -6,   1,   1,&!-161
 -4,  -5,  -5,  -4,   1,   1,&!-160
 -4,  -5,  -4,  -5,   1,   1,&!-159
 -4,  -4,  -4,  -4,   2,   2,&!-158
 -4,  -3,  -4,  -3,   1,   1,&!-157
 -4,  -3,  -3,  -4,   1,   1,&!-156
 -4,  -2,  -4,  -2,   1,   1,&!-155
 -4,  -2,  -2,  -4,   1,   1,&!-154
 -4,  -1,  -4,  -1,   1,   1,&!-153
 -4,  -1,  -1,  -4,   1,   1,&!-152
 -4,   0,  -4,   0,   4,   7,&!-151
 -4,   0,   0,  -4,   4,   7,&!-150
 -4,   1,  -4,   1,   1,   1,&!-149
 -4,   1,   1,  -4,   1,   1,&!-148
 -4,   2,  -4,   2,   1,   1,&!-147
 -4,   2,   2,  -4,   1,   1,&!-146
 -4,   3,  -4,   3,   1,   1,&!-145
 -4,   3,   3,  -4,   1,   1,&!-144
 -4,   4,  -6,   6,   3,   3,&!-143
 -4,   4,  -5,   5,   3,   3,&!-142
 -4,   4,  -4,   4,   3,   4,&!-141
 -4,   4,  -3,   3,   3,   3,&!-140
 -4,   4,  -2,   2,   3,   3,&!-139
 -4,   4,  -1,   1,   3,   3,&!-138
 -4,   4,   0,   0,   3,   5,&!-137
 -4,   4,   1,  -1,   3,   3,&!-136
 -4,   4,   2,  -2,   3,   3,&!-135
 -4,   4,   3,  -3,   3,   3,&!-134
 -4,   4,   4,  -4,   3,   4,&!-133
 -4,   4,   5,  -5,   3,   3,&!-132
 -4,   4,   6,  -6,   3,   3,&!-131
 -4,   5,  -4,   5,   1,   1,&!-130
 -4,   5,   5,  -4,   1,   1,&!-129
 -4,   6,  -4,   6,   1,   1,&!-128
 -4,   6,   6,  -4,   1,   1,&!-127
 -3,  -6,  -6,  -3,   1,   1,&!-126
 -3,  -6,  -3,  -6,   1,   1,&!-125
 -3,  -5,  -5,  -3,   1,   1,&!-124
 -3,  -5,  -3,  -5,   1,   1,&!-123
 -3,  -4,  -4,  -3,   1,   1,&!-122
 -3,  -4,  -3,  -4,   1,   1,&!-121
 -3,  -3,  -3,  -3,   2,   2,&!-120
 -3,  -2,  -3,  -2,   1,   1,&!-119
 -3,  -2,  -2,  -3,   1,   1,&!-118
 -3,  -1,  -3,  -1,   1,   1,&!-117
 -3,  -1,  -1,  -3,   1,   1,&!-116
 -3,   0,  -3,   0,   4,   7,&!-115
 -3,   0,   0,  -3,   4,   7,&!-114
 -3,   1,  -3,   1,   1,   1,&!-113
 -3,   1,   1,  -3,   1,   1,&!-112
 -3,   2,  -3,   2,   1,   1,&!-111
 -3,   2,   2,  -3,   1,   1,&!-110
 -3,   3,  -6,   6,   3,   3,&!-109
 -3,   3,  -5,   5,   3,   3,&!-108
 -3,   3,  -4,   4,   3,   3,&!-107
 -3,   3,  -3,   3,   3,   4,&!-106
 -3,   3,  -2,   2,   3,   3,&!-105
 -3,   3,  -1,   1,   3,   3,&!-104
 -3,   3,   0,   0,   3,   5,&!-103
 -3,   3,   1,  -1,   3,   3,&!-102
 -3,   3,   2,  -2,   3,   3,&!-101
 -3,   3,   3,  -3,   3,   4,&!-100
 -3,   3,   4,  -4,   3,   3,&! -99
 -3,   3,   5,  -5,   3,   3,&! -98
 -3,   3,   6,  -6,   3,   3,&! -97
 -3,   4,  -3,   4,   1,   1,&! -96
 -3,   4,   4,  -3,   1,   1,&! -95
 -3,   5,  -3,   5,   1,   1,&! -94
 -3,   5,   5,  -3,   1,   1,&! -93
 -3,   6,  -3,   6,   1,   1,&! -92
 -3,   6,   6,  -3,   1,   1,&! -91
 -2,  -6,  -6,  -2,   1,   1,&! -90
 -2,  -6,  -2,  -6,   1,   1,&! -89
 -2,  -5,  -5,  -2,   1,   1,&! -88
 -2,  -5,  -2,  -5,   1,   1,&! -87
 -2,  -4,  -4,  -2,   1,   1,&! -86
 -2,  -4,  -2,  -4,   1,   1,&! -85
 -2,  -3,  -3,  -2,   1,   1,&! -84
 -2,  -3,  -2,  -3,   1,   1,&! -83
 -2,  -2,  -2,  -2,   2,   2,&! -82
 -2,  -1,  -2,  -1,   1,   1,&! -81
 -2,  -1,  -1,  -2,   1,   1,&! -80
 -2,   0,  -2,   0,   4,   7,&! -79
 -2,   0,   0,  -2,   4,   7,&! -78
 -2,   1,  -2,   1,   1,   1,&! -77
 -2,   1,   1,  -2,   1,   1,&! -76
 -2,   2,  -6,   6,   3,   3,&! -75
 -2,   2,  -5,   5,   3,   3,&! -74
 -2,   2,  -4,   4,   3,   3,&! -73
 -2,   2,  -3,   3,   3,   3,&! -72
 -2,   2,  -2,   2,   3,   4,&! -71
 -2,   2,  -1,   1,   3,   3,&! -70
 -2,   2,   0,   0,   3,   5,&! -69
 -2,   2,   1,  -1,   3,   3,&! -68
 -2,   2,   2,  -2,   3,   4,&! -67
 -2,   2,   3,  -3,   3,   3,&! -66
 -2,   2,   4,  -4,   3,   3,&! -65
 -2,   2,   5,  -5,   3,   3,&! -64
 -2,   2,   6,  -6,   3,   3,&! -63
 -2,   3,  -2,   3,   1,   1,&! -62
 -2,   3,   3,  -2,   1,   1,&! -61
 -2,   4,  -2,   4,   1,   1,&! -60
 -2,   4,   4,  -2,   1,   1,&! -59
 -2,   5,  -2,   5,   1,   1,&! -58
 -2,   5,   5,  -2,   1,   1,&! -57
 -2,   6,  -2,   6,   1,   1,&! -56
 -2,   6,   6,  -2,   1,   1,&! -55
 -1,  -6,  -6,  -1,   1,   1,&! -54
 -1,  -6,  -1,  -6,   1,   1,&! -53
 -1,  -5,  -5,  -1,   1,   1,&! -52
 -1,  -5,  -1,  -5,   1,   1,&! -51
 -1,  -4,  -4,  -1,   1,   1,&! -50
 -1,  -4,  -1,  -4,   1,   1,&! -49
 -1,  -3,  -3,  -1,   1,   1,&! -48
 -1,  -3,  -1,  -3,   1,   1,&! -47
 -1,  -2,  -2,  -1,   1,   1,&! -46
 -1,  -2,  -1,  -2,   1,   1,&! -45
 -1,  -1,  -1,  -1,   2,   2,&! -44
 -1,   0,  -1,   0,   4,   7,&! -43
 -1,   0,   0,  -1,   4,   7,&! -42
 -1,   1,  -6,   6,   3,   3,&! -41
 -1,   1,  -5,   5,   3,   3,&! -40
 -1,   1,  -4,   4,   3,   3,&! -39
 -1,   1,  -3,   3,   3,   3,&! -38
 -1,   1,  -2,   2,   3,   3,&! -37
 -1,   1,  -1,   1,   3,   4,&! -36
 -1,   1,   0,   0,   3,   5,&! -35
 -1,   1,   1,  -1,   3,   4,&! -34
 -1,   1,   2,  -2,   3,   3,&! -33
 -1,   1,   3,  -3,   3,   3,&! -32
 -1,   1,   4,  -4,   3,   3,&! -31
 -1,   1,   5,  -5,   3,   3,&! -30
 -1,   1,   6,  -6,   3,   3,&! -29
 -1,   2,  -1,   2,   1,   1,&! -28
 -1,   2,   2,  -1,   1,   1,&! -27
 -1,   3,  -1,   3,   1,   1,&! -26
 -1,   3,   3,  -1,   1,   1,&! -25
 -1,   4,  -1,   4,   1,   1,&! -24
 -1,   4,   4,  -1,   1,   1,&! -23
 -1,   5,  -1,   5,   1,   1,&! -22
 -1,   5,   5,  -1,   1,   1,&! -21
 -1,   6,  -1,   6,   1,   1,&! -20
 -1,   6,   6,  -1,   1,   1,&! -19
  0,  -6,  -6,   0,   4,   7,&! -18
  0,  -6,   0,  -6,   4,   7,&! -17
  0,  -5,  -5,   0,   4,   7,&! -16
  0,  -5,   0,  -5,   4,   7,&! -15
  0,  -4,  -4,   0,   4,   7,&! -14
  0,  -4,   0,  -4,   4,   7,&! -13
  0,  -3,  -3,   0,   4,   7,&! -12
  0,  -3,   0,  -3,   4,   7,&! -11
  0,  -2,  -2,   0,   4,   7,&! -10
  0,  -2,   0,  -2,   4,   7,&!  -9
  0,  -1,  -1,   0,   4,   7,&!  -8
  0,  -1,   0,  -1,   4,   7,&!  -7
  0,   0,  -6,   6,   5,   6,&!  -6
  0,   0,  -5,   5,   5,   6,&!  -5
  0,   0,  -4,   4,   5,   6,&!  -4
  0,   0,  -3,   3,   5,   6,&!  -3
  0,   0,  -2,   2,   5,   6,&!  -2
  0,   0,  -1,   1,   5,   6,&!  -1
  0,   0,   0,   0,   5,   8,&!   0
  0,   0,   1,  -1,   5,   6,&!   1
  0,   0,   2,  -2,   5,   6,&!   2
  0,   0,   3,  -3,   5,   6,&!   3
  0,   0,   4,  -4,   5,   6,&!   4
  0,   0,   5,  -5,   5,   6,&!   5
  0,   0,   6,  -6,   5,   6,&!   6
  0,   1,   0,   1,   4,   7,&!   7
  0,   1,   1,   0,   4,   7,&!   8
  0,   2,   0,   2,   4,   7,&!   9
  0,   2,   2,   0,   4,   7,&!  10
  0,   3,   0,   3,   4,   7,&!  11
  0,   3,   3,   0,   4,   7,&!  12
  0,   4,   0,   4,   4,   7,&!  13
  0,   4,   4,   0,   4,   7,&!  14
  0,   5,   0,   5,   4,   7,&!  15
  0,   5,   5,   0,   4,   7,&!  16
  0,   6,   0,   6,   4,   7,&!  17
  0,   6,   6,   0,   4,   7,&!  18
  1,  -6,  -6,   1,   1,   1,&!  19
  1,  -6,   1,  -6,   1,   1,&!  20
  1,  -5,  -5,   1,   1,   1,&!  21
  1,  -5,   1,  -5,   1,   1,&!  22
  1,  -4,  -4,   1,   1,   1,&!  23
  1,  -4,   1,  -4,   1,   1,&!  24
  1,  -3,  -3,   1,   1,   1,&!  25
  1,  -3,   1,  -3,   1,   1,&!  26
  1,  -2,  -2,   1,   1,   1,&!  27
  1,  -2,   1,  -2,   1,   1,&!  28
  1,  -1,  -6,   6,   3,   3,&!  29
  1,  -1,  -5,   5,   3,   3,&!  30
  1,  -1,  -4,   4,   3,   3,&!  31
  1,  -1,  -3,   3,   3,   3,&!  32
  1,  -1,  -2,   2,   3,   3,&!  33
  1,  -1,  -1,   1,   3,   4,&!  34
  1,  -1,   0,   0,   3,   5,&!  35
  1,  -1,   1,  -1,   3,   4,&!  36
  1,  -1,   2,  -2,   3,   3,&!  37
  1,  -1,   3,  -3,   3,   3,&!  38
  1,  -1,   4,  -4,   3,   3,&!  39
  1,  -1,   5,  -5,   3,   3,&!  40
  1,  -1,   6,  -6,   3,   3,&!  41
  1,   0,   0,   1,   4,   7,&!  42
  1,   0,   1,   0,   4,   7,&!  43
  1,   1,   1,   1,   2,   2,&!  44
  1,   2,   1,   2,   1,   1,&!  45
  1,   2,   2,   1,   1,   1,&!  46
  1,   3,   1,   3,   1,   1,&!  47
  1,   3,   3,   1,   1,   1,&!  48
  1,   4,   1,   4,   1,   1,&!  49
  1,   4,   4,   1,   1,   1,&!  50
  1,   5,   1,   5,   1,   1,&!  51
  1,   5,   5,   1,   1,   1,&!  52
  1,   6,   1,   6,   1,   1,&!  53
  1,   6,   6,   1,   1,   1,&!  54
  2,  -6,  -6,   2,   1,   1,&!  55
  2,  -6,   2,  -6,   1,   1,&!  56
  2,  -5,  -5,   2,   1,   1,&!  57
  2,  -5,   2,  -5,   1,   1,&!  58
  2,  -4,  -4,   2,   1,   1,&!  59
  2,  -4,   2,  -4,   1,   1,&!  60
  2,  -3,  -3,   2,   1,   1,&!  61
  2,  -3,   2,  -3,   1,   1,&!  62
  2,  -2,  -6,   6,   3,   3,&!  63
  2,  -2,  -5,   5,   3,   3,&!  64
  2,  -2,  -4,   4,   3,   3,&!  65
  2,  -2,  -3,   3,   3,   3,&!  66
  2,  -2,  -2,   2,   3,   4,&!  67
  2,  -2,  -1,   1,   3,   3,&!  68
  2,  -2,   0,   0,   3,   5,&!  69
  2,  -2,   1,  -1,   3,   3,&!  70
  2,  -2,   2,  -2,   3,   4,&!  71
  2,  -2,   3,  -3,   3,   3,&!  72
  2,  -2,   4,  -4,   3,   3,&!  73
  2,  -2,   5,  -5,   3,   3,&!  74
  2,  -2,   6,  -6,   3,   3,&!  75
  2,  -1,  -1,   2,   1,   1,&!  76
  2,  -1,   2,  -1,   1,   1,&!  77
  2,   0,   0,   2,   4,   7,&!  78
  2,   0,   2,   0,   4,   7,&!  79
  2,   1,   1,   2,   1,   1,&!  80
  2,   1,   2,   1,   1,   1,&!  81
  2,   2,   2,   2,   2,   2,&!  82
  2,   3,   2,   3,   1,   1,&!  83
  2,   3,   3,   2,   1,   1,&!  84
  2,   4,   2,   4,   1,   1,&!  85
  2,   4,   4,   2,   1,   1,&!  86
  2,   5,   2,   5,   1,   1,&!  87
  2,   5,   5,   2,   1,   1,&!  88
  2,   6,   2,   6,   1,   1,&!  89
  2,   6,   6,   2,   1,   1,&!  90
  3,  -6,  -6,   3,   1,   1,&!  91
  3,  -6,   3,  -6,   1,   1,&!  92
  3,  -5,  -5,   3,   1,   1,&!  93
  3,  -5,   3,  -5,   1,   1,&!  94
  3,  -4,  -4,   3,   1,   1,&!  95
  3,  -4,   3,  -4,   1,   1,&!  96
  3,  -3,  -6,   6,   3,   3,&!  97
  3,  -3,  -5,   5,   3,   3,&!  98
  3,  -3,  -4,   4,   3,   3,&!  99
  3,  -3,  -3,   3,   3,   4,&! 100
  3,  -3,  -2,   2,   3,   3,&! 101
  3,  -3,  -1,   1,   3,   3,&! 102
  3,  -3,   0,   0,   3,   5,&! 103
  3,  -3,   1,  -1,   3,   3,&! 104
  3,  -3,   2,  -2,   3,   3,&! 105
  3,  -3,   3,  -3,   3,   4,&! 106
  3,  -3,   4,  -4,   3,   3,&! 107
  3,  -3,   5,  -5,   3,   3,&! 108
  3,  -3,   6,  -6,   3,   3,&! 109
  3,  -2,  -2,   3,   1,   1,&! 110
  3,  -2,   3,  -2,   1,   1,&! 111
  3,  -1,  -1,   3,   1,   1,&! 112
  3,  -1,   3,  -1,   1,   1,&! 113
  3,   0,   0,   3,   4,   7,&! 114
  3,   0,   3,   0,   4,   7,&! 115
  3,   1,   1,   3,   1,   1,&! 116
  3,   1,   3,   1,   1,   1,&! 117
  3,   2,   2,   3,   1,   1,&! 118
  3,   2,   3,   2,   1,   1,&! 119
  3,   3,   3,   3,   2,   2,&! 120
  3,   4,   3,   4,   1,   1,&! 121
  3,   4,   4,   3,   1,   1,&! 122
  3,   5,   3,   5,   1,   1,&! 123
  3,   5,   5,   3,   1,   1,&! 124
  3,   6,   3,   6,   1,   1,&! 125
  3,   6,   6,   3,   1,   1,&! 126
  4,  -6,  -6,   4,   1,   1,&! 127
  4,  -6,   4,  -6,   1,   1,&! 128
  4,  -5,  -5,   4,   1,   1,&! 129
  4,  -5,   4,  -5,   1,   1,&! 130
  4,  -4,  -6,   6,   3,   3,&! 131
  4,  -4,  -5,   5,   3,   3,&! 132
  4,  -4,  -4,   4,   3,   4,&! 133
  4,  -4,  -3,   3,   3,   3,&! 134
  4,  -4,  -2,   2,   3,   3,&! 135
  4,  -4,  -1,   1,   3,   3,&! 136
  4,  -4,   0,   0,   3,   5,&! 137
  4,  -4,   1,  -1,   3,   3,&! 138
  4,  -4,   2,  -2,   3,   3,&! 139
  4,  -4,   3,  -3,   3,   3,&! 140
  4,  -4,   4,  -4,   3,   4,&! 141
  4,  -4,   5,  -5,   3,   3,&! 142
  4,  -4,   6,  -6,   3,   3,&! 143
  4,  -3,  -3,   4,   1,   1,&! 144
  4,  -3,   4,  -3,   1,   1,&! 145
  4,  -2,  -2,   4,   1,   1,&! 146
  4,  -2,   4,  -2,   1,   1,&! 147
  4,  -1,  -1,   4,   1,   1,&! 148
  4,  -1,   4,  -1,   1,   1,&! 149
  4,   0,   0,   4,   4,   7,&! 150
  4,   0,   4,   0,   4,   7,&! 151
  4,   1,   1,   4,   1,   1,&! 152
  4,   1,   4,   1,   1,   1,&! 153
  4,   2,   2,   4,   1,   1,&! 154
  4,   2,   4,   2,   1,   1,&! 155
  4,   3,   3,   4,   1,   1,&! 156
  4,   3,   4,   3,   1,   1,&! 157
  4,   4,   4,   4,   2,   2,&! 158
  4,   5,   4,   5,   1,   1,&! 159
  4,   5,   5,   4,   1,   1,&! 160
  4,   6,   4,   6,   1,   1,&! 161
  4,   6,   6,   4,   1,   1,&! 162
  5,  -6,  -6,   5,   1,   1,&! 163
  5,  -6,   5,  -6,   1,   1,&! 164
  5,  -5,  -6,   6,   3,   3,&! 165
  5,  -5,  -5,   5,   3,   4,&! 166
  5,  -5,  -4,   4,   3,   3,&! 167
  5,  -5,  -3,   3,   3,   3,&! 168
  5,  -5,  -2,   2,   3,   3,&! 169
  5,  -5,  -1,   1,   3,   3,&! 170
  5,  -5,   0,   0,   3,   5,&! 171
  5,  -5,   1,  -1,   3,   3,&! 172
  5,  -5,   2,  -2,   3,   3,&! 173
  5,  -5,   3,  -3,   3,   3,&! 174
  5,  -5,   4,  -4,   3,   3,&! 175
  5,  -5,   5,  -5,   3,   4,&! 176
  5,  -5,   6,  -6,   3,   3,&! 177
  5,  -4,  -4,   5,   1,   1,&! 178
  5,  -4,   5,  -4,   1,   1,&! 179
  5,  -3,  -3,   5,   1,   1,&! 180
  5,  -3,   5,  -3,   1,   1,&! 181
  5,  -2,  -2,   5,   1,   1,&! 182
  5,  -2,   5,  -2,   1,   1,&! 183
  5,  -1,  -1,   5,   1,   1,&! 184
  5,  -1,   5,  -1,   1,   1,&! 185
  5,   0,   0,   5,   4,   7,&! 186
  5,   0,   5,   0,   4,   7,&! 187
  5,   1,   1,   5,   1,   1,&! 188
  5,   1,   5,   1,   1,   1,&! 189
  5,   2,   2,   5,   1,   1,&! 190
  5,   2,   5,   2,   1,   1,&! 191
  5,   3,   3,   5,   1,   1,&! 192
  5,   3,   5,   3,   1,   1,&! 193
  5,   4,   4,   5,   1,   1,&! 194
  5,   4,   5,   4,   1,   1,&! 195
  5,   5,   5,   5,   2,   2,&! 196
  5,   6,   5,   6,   1,   1,&! 197
  5,   6,   6,   5,   1,   1,&! 198
  6,  -6,  -6,   6,   3,   4,&! 199
  6,  -6,  -5,   5,   3,   3,&! 200
  6,  -6,  -4,   4,   3,   3,&! 201
  6,  -6,  -3,   3,   3,   3,&! 202
  6,  -6,  -2,   2,   3,   3,&! 203
  6,  -6,  -1,   1,   3,   3,&! 204
  6,  -6,   0,   0,   3,   5,&! 205
  6,  -6,   1,  -1,   3,   3,&! 206
  6,  -6,   2,  -2,   3,   3,&! 207
  6,  -6,   3,  -3,   3,   3,&! 208
  6,  -6,   4,  -4,   3,   3,&! 209
  6,  -6,   5,  -5,   3,   3,&! 210
  6,  -6,   6,  -6,   3,   4,&! 211
  6,  -5,  -5,   6,   1,   1,&! 212
  6,  -5,   6,  -5,   1,   1,&! 213
  6,  -4,  -4,   6,   1,   1,&! 214
  6,  -4,   6,  -4,   1,   1,&! 215
  6,  -3,  -3,   6,   1,   1,&! 216
  6,  -3,   6,  -3,   1,   1,&! 217
  6,  -2,  -2,   6,   1,   1,&! 218
  6,  -2,   6,  -2,   1,   1,&! 219
  6,  -1,  -1,   6,   1,   1,&! 220
  6,  -1,   6,  -1,   1,   1,&! 221
  6,   0,   0,   6,   4,   7,&! 222
  6,   0,   6,   0,   4,   7,&! 223
  6,   1,   1,   6,   1,   1,&! 224
  6,   1,   6,   1,   1,   1,&! 225
  6,   2,   2,   6,   1,   1,&! 226
  6,   2,   6,   2,   1,   1,&! 227
  6,   3,   3,   6,   1,   1,&! 228
  6,   3,   6,   3,   1,   1,&! 229
  6,   4,   4,   6,   1,   1,&! 230
  6,   4,   6,   4,   1,   1,&! 231
  6,   5,   5,   6,   1,   1,&! 232
  6,   5,   6,   5,   1,   1,&! 233
  6,   6,   6,   6,   2,   2&! 234
],[6,469])

  integer,dimension(2,0:16),parameter::double_pdf_kinds=reshape([&
       &0,0,&
       &1,1,&
       &1,2,&
       &1,3,&
       &1,4,&
       &2,1,&
       &2,2,&
       &2,3,&
       &2,4,&
       &3,1,&
       &3,2,&
       &3,3,&
       &3,4,&
       &4,1,&
       &4,2,&
       &4,3,&
       &4,4&
       &],[2,17])

  integer,parameter,dimension(371)::int_all=[&
       &  -6,   -5,   -4,   -3,   -2,   -1,    0,    1,    2,    3,    4,    5,    6,  -14,  -13,  -12,  -11,  -10,&
       &  -9,   -8,   -7,    7,    8,    9,   10,   11,   12,   13,   14,    7,    8,    9,   10, -151, -150, -115,&
       &-114,  -79,  -78,  -43,  -42,   42,   43,   78,   79,  114,  115,  150,  151, -158, -157, -156, -155, -154,&
       &-153, -152, -149, -148, -147, -146, -145, -144, -143, -142, -141, -140, -139, -138, -137, -136, -135, -134,&
       &-133, -132, -131, -122, -121, -120, -119, -118, -117, -116, -113, -112, -111, -110, -109, -108, -107, -106,&
       &-105, -104, -103, -102, -101, -100,  -99,  -98,  -97,  -96,  -95,  -86,  -85,  -84,  -83,  -82,  -81,  -80,&
       & -77,  -76,  -75,  -74,  -73,  -72,  -71,  -70,  -69,  -68,  -67,  -66,  -65,  -64,  -63,  -62,  -61,  -60,&
       & -59,  -50,  -49,  -48,  -47,  -46,  -45,  -44,  -41,  -40,  -39,  -38,  -37,  -36,  -35,  -34,  -33,  -32,&
       & -31,  -30,  -29,  -28,  -27,  -26,  -25,  -24,  -23,   23,   24,   25,   26,   27,   28,   29,   30,   31,&
       &  32,   33,   34,   35,   36,   37,   38,   39,   40,   41,   44,   45,   46,   47,   48,   49,   50,   59,&
       &  60,   61,   62,   63,   64,   65,   66,   67,   68,   69,   70,   71,   72,   73,   74,   75,   76,   77,&
       &  80,   81,   82,   83,   84,   85,   86,   95,   96,   97,   98,   99,  100,  101,  102,  103,  104,  105,&
       & 106,  107,  108,  109,  110,  111,  112,  113,  116,  117,  118,  119,  120,  121,  122,  131,  132,  133,&
       & 134,  135,  136,  137,  138,  139,  140,  141,  142,  143,  144,  145,  146,  147,  148,  149,  152,  153,&
       & 154,  155,  156,  157,  158, -149, -148, -113, -112,  -77,  -76,  -41,  -40,  -39,  -38,  -37,  -36,  -35,&
       & -34,  -33,  -32,  -31,  -30,  -29,   44,   80,   81,  116,  117,  152,  153, -147, -146, -111, -110,  -75,&
       & -74,  -73,  -72,  -71,  -70,  -69,  -68,  -67,  -66,  -65,  -64,  -63,  -28,  -27,   45,   46,   82,  118,&
       & 119,  154,  155,   42,   43,   23,   24,   25,   26,   27,   28,   29,   30,   31,   32,   33,   34,   35,&
       &  36,   37,   38,   39,   40,   41,   44,   45,   46,   47,   48,   49,   50,   44,   45,   46,   78,   79,&
       &  59,   60,   61,   62,   63,   64,   65,   66,   67,   68,   69,   70,   71,   72,   73,   74,   75,   76,&
       &  77,   80,   81,   82,   83,   84,   85,   86,   80,   81,   82]

  integer,parameter,dimension(16)::int_sizes_all=[13,16,2,2,16,208,26,26,2,26,1,2,2,26,2,1]

  integer,parameter,dimension(3,0:8)::muli_flow_stats=&
       reshape([&
        1, 2,4,&
        3, 4,4,&
        5, 6,8,&
        7, 8,4,&
        9,10,8,&
       11,16,16,&
       17,22,16,&
       23,28,16,&
       29,52,96],&
       [3,9])

  integer,parameter,dimension(0:4,52)::muli_flows=&
       reshape([&
       3,0,0,1,2,&!1a
       1,0,0,2,1,&
       1,2,0,0,3,&!1b
       3,3,0,0,2,&
       4,0,0,1,2,&!2
       4,0,0,2,1,&
       3,2,0,0,3,&!3
       1,3,0,0,2,&
       4,2,0,0,3,&!4
       4,3,0,0,2,&
       4,0,1,3,4,&!5
       4,0,1,4,3,&
       2,0,3,1,4,&
       2,0,4,1,3,&
       2,0,3,4,1,&
       2,0,4,3,1,&
       4,1,2,4,0,&!6
       2,1,4,2,0,&
       4,2,1,4,0,&
       2,4,1,2,0,&
       2,2,4,1,0,&
       2,4,2,1,0,&
       2,0,1,2,4,&!7
       2,0,1,4,2,&
       4,0,2,1,4,&
       4,0,4,1,2,&
       2,0,2,4,1,&
       2,0,4,2,1,&
       9,1,2,3,4,&!8
       5,1,2,4,3,&
       5,1,3,2,4,&
       3,1,4,2,3,&
       3,1,3,4,2,&
       5,1,4,3,2,&
       5,2,1,3,4,&
       5,2,1,4,3,&
       3,3,1,2,4,&
       3,4,1,2,3,&
       3,3,1,4,2,&
       3,4,1,3,2,&
       3,2,3,1,4,&
       3,2,4,1,3,&
       5,3,2,1,4,&
       3,4,2,1,3,&
       5,3,4,1,2,&
       3,4,3,1,2,&
       3,2,3,4,1,&
       3,2,4,3,1,&
       3,3,2,4,1,&
       5,4,2,3,1,&
       3,3,4,2,1,&
       5,4,3,2,1],&
       [5,52])

  abstract interface
     function trafo_in(in) 
       use kinds!NODEP!
       real(kind=double),dimension(3)::trafo_in
       real(kind=double),dimension(3),intent(in)::in
     end function trafo_in
  end interface
  abstract interface
     pure function coord_scalar_in(hyp)
       use kinds!NODEP!
       real(kind=double)::coord_scalar_in
       real(kind=double),dimension(3),intent(in)::hyp
     end function coord_scalar_in
  end interface
  abstract interface
     subroutine coord_hcd_in(hyp,cart,denom)
       use kinds!NODEP!
       real(kind=double),dimension(3),intent(in)::hyp
       real(kind=double),dimension(3),intent(out)::cart
       real(kind=double),intent(out)::denom
     end subroutine coord_hcd_in
  end interface
  interface
     pure function alphaspdf(Q)
       use kinds!NODEP!
       real(kind=double)::alphaspdf
       real(kind=double),intent(in)::Q
     end function alphaspdf
  end interface
  interface
     pure subroutine evolvepdf(x,q,f)
       use kinds!NODEP!
       real(kind=double),intent(in)::x,q
       real(kind=double),intent(out),dimension(-6:6)::f
     end subroutine evolvepdf
  end interface
!  procedure(alpha_s_interface)::alphasPDF
!  procedure(evolvepdf_interface)::evolvepdf
  real(kind=double)::pts2_scale
!  real(kind=double),private :: gev2_q_min,gev2_q_max,x_min,x_max

contains

  pure function muli_get_state_transformations(inout_kind,lha_flavors) result(transformations)
    integer,intent(in)::inout_kind
    integer,dimension(4),intent(in)::lha_flavors
    integer,dimension(4)::signature
    logical,dimension(3)::transformations
    where(lha_flavors>0)
       signature=1
    elsewhere(lha_flavors<0)
       signature=-1
    elsewhere
       signature=0
    end where
    !print *,"inout_kind=",inout_kind
    !print *,"lha_flavors=",lha_flavors
    !print *,"signature",signature
    if(&
         (sum(inout_signatures(1:2,inout_kind))==sum(signature(1:2))).and.&
         (sum(inout_signatures(3:4,inout_kind))==sum(signature(3:4)))&
         )then
       transformations(1)=.false.
    else
       transformations(1)=.true.
       signature=-signature
    end if
    if(all(inout_signatures(1:2,inout_kind)==signature(1:2)))then
       transformations(2)=.false.
    else
       transformations(2)=.true.
    end if
    if(all(inout_signatures(3:4,inout_kind)==signature(3:4)))then
       transformations(3)=.false.
    else
       transformations(3)=.true.
    end if
    !print *,"signature",signature
    !print *,"transformations=",transformations
  end function muli_get_state_transformations
        
  pure function id(a)
    real(kind=double),dimension(:),intent(in)::a
    real(kind=double),dimension(size(a))::id
    id=a
  end function id

  pure function h_to_c_ort(hyp)
    real(kind=double),dimension(3)::h_to_c_ort
    real(kind=double),dimension(3),intent(in)::hyp
    h_to_c_ort=&
         &[sqrt(sqrt(((hyp(1)*(1D0-hyp(3)))+hyp(3))**2+(hyp(2)-(5D-1))**2)-(hyp(2)-(5D-1)))&
         &,sqrt(sqrt(((hyp(1)*(1D0-hyp(3)))+hyp(3))**2+(hyp(2)-(5D-1))**2)+(hyp(2)-(5D-1)))&
         &,hyp(3)]
  end function h_to_c_ort
  
  pure function c_to_h_ort(cart)
    real(kind=double),dimension(3)::c_to_h_ort
    real(kind=double),dimension(3),intent(in)::cart
    c_to_h_ort=[(cart(3)-(cart(1)*cart(2)))/(cart(3)-1D0),(1D0 - cart(1)**2 + cart(2)**2)/2D0,cart(3)]
  end function c_to_h_ort

  pure function h_to_c_noparam(hyp)
    real(kind=double),dimension(2)::h_to_c_noparam
    real(kind=double),dimension(2),intent(in)::hyp
    h_to_c_noparam=&
         &[sqrt(sqrt(hyp(1)**8+(((hyp(2)-(5D-1))**3)*4)**2)-((hyp(2)-(5D-1))**3)*4)&
         &,sqrt(sqrt(hyp(1)**8+(((hyp(2)-(5D-1))**3)*4)**2)+((hyp(2)-(5D-1))**3)*4)]
  end function h_to_c_noparam

  pure function c_to_h_noparam(cart)
    real(kind=double),dimension(2)::c_to_h_noparam
    real(kind=double),dimension(2),intent(in)::cart
    c_to_h_noparam=&
         &[sqrt(sqrt(cart(1)*cart(2)))&
         &,(1D0+sign(abs((cart(2)**2) - (cart(1)**2))**(1/3D0),cart(2)-cart(1)))/2D0]
  end function c_to_h_noparam

  pure function h_to_c_param(hyp)
    real(kind=double),dimension(3)::h_to_c_param
    real(kind=double),dimension(3),intent(in)::hyp
    h_to_c_param=&
         &[sqrt(sqrt((((hyp(1)**4)*(1D0-hyp(3)))+hyp(3))**2+(((hyp(2)-(5D-1))**3)*4)**2)-((hyp(2)-(5D-1))**3)*4)&
         &,sqrt(sqrt((((hyp(1)**4)*(1D0-hyp(3)))+hyp(3))**2+(((hyp(2)-(5D-1))**3)*4)**2)+((hyp(2)-(5D-1))**3)*4)&
         &,hyp(3)]
  end function h_to_c_param
  
  pure function c_to_h_param(cart)
    real(kind=double),dimension(3)::c_to_h_param
    real(kind=double),dimension(3),intent(in)::cart
    c_to_h_param=&
         &[(((cart(1)*cart(2)) - cart(3))/(1D0 - cart(3)))**(1/4D0)&
         &,(1D0+sign(abs((cart(2)**2) - (cart(1)**2))**(1/3D0),cart(2)-cart(1)))/2D0&
         &,cart(3)]
  end function c_to_h_param

  pure function h_to_c_smooth(hyp)
    real(kind=double),dimension(3)::h_to_c_smooth
    real(kind=double),dimension(3),intent(in)::hyp
    real(kind=double)::h2
    h2=(((hyp(2)-5D-1)**3)*4D0+hyp(2)-5D-1)/2D0
    h_to_c_smooth=&
         &[sqrt(sqrt((((hyp(1)**4)*(1D0-hyp(3)))+hyp(3))**2+h2**2)-h2)&
         &,sqrt(sqrt((((hyp(1)**4)*(1D0-hyp(3)))+hyp(3))**2+h2**2)+h2)&
         &,hyp(3)]
  end function h_to_c_smooth
  
  pure function c_to_h_smooth(cart)
    real(kind=double),dimension(3)::c_to_h_smooth
    real(kind=double),dimension(3),intent(in)::cart
    c_to_h_smooth=&
         [((product(cart(1:2))-cart(3))/(1D0-cart(3)))**(1/4D0),&
         (3D0-3D0**(2D0/3)/&
         (-9D0*cart(1)**2 + 9D0*cart(2)**2 + sqrt(3D0+81D0*(cart(1)**2-cart(2)**2)**2))**(1D0/3)&
         + 3**(1D0/3)*(-9D0*cart(1)**2 + 9D0*cart(2)**2 + sqrt(3D0 + 81D0*(cart(1)**2&
         - cart(2)**2)**2))**(1D0/3))/6D0,cart(3)]
  end function c_to_h_smooth

  pure function h_to_c_ort_def(hyp)
    real(kind=double),dimension(3)::h_to_c_ort_def
    real(kind=double),dimension(3),intent(in)::hyp
    h_to_c_ort_def=h_to_c_ort([hyp(1),hyp(2),pts2_scale])
  end function h_to_c_ort_def
  
  pure function c_to_h_ort_def(cart)
    real(kind=double),dimension(3)::c_to_h_ort_def
    real(kind=double),dimension(3),intent(in)::cart
    c_to_h_ort_def=c_to_h_ort([cart(1),cart(2),pts2_scale])
  end function c_to_h_ort_def
  
  pure function h_to_c_param_def(hyp)
    real(kind=double),dimension(3)::h_to_c_param_def
    real(kind=double),dimension(3),intent(in)::hyp
    h_to_c_param_def=h_to_c_param([hyp(1),hyp(2),pts2_scale])
  end function h_to_c_param_def
  
  pure function c_to_h_param_def(cart)
    real(kind=double),dimension(3)::c_to_h_param_def
    real(kind=double),dimension(3),intent(in)::cart
    if(product(cart(1:2))>=pts2_scale)then
       c_to_h_param_def=c_to_h_param([cart(1),cart(2),pts2_scale])
    else
       c_to_h_param_def=[-1D0,-1D0,-1D0]
    end if
  end function c_to_h_param_def

  pure function h_to_c_smooth_def(hyp)
    real(kind=double),dimension(3)::h_to_c_smooth_def
    real(kind=double),dimension(3),intent(in)::hyp
    h_to_c_smooth_def=h_to_c_smooth([hyp(1),hyp(2),pts2_scale])
  end function h_to_c_smooth_def
  
  pure function c_to_h_smooth_def(cart)
    real(kind=double),dimension(3)::c_to_h_smooth_def
    real(kind=double),dimension(3),intent(in)::cart
    if(product(cart(1:2))>=pts2_scale)then
       c_to_h_smooth_def=c_to_h_smooth([cart(1),cart(2),pts2_scale])
    else
       c_to_h_smooth_def=[-1D0,-1D0,-1D0]
    end if
  end function c_to_h_smooth_def

  pure function voxel_h_to_c_ort(hyp)
    real(kind=double)::voxel_h_to_c_ort
    real(kind=double),dimension(3),intent(in)::hyp
    real(kind=double)::T,TH1
    T=1D0-hyp(3)
    TH1=T*(1D0-hyp(1))
    voxel_h_to_c_ort=Sqrt(T**2/(5D0-4D0*(1D0-hyp(2))*hyp(2)-4D0*(2D0-TH1)*TH1))
  end function voxel_h_to_c_ort

  pure function voxel_c_to_h_ort(cart)
    real(kind=double)::voxel_c_to_h_ort
    real(kind=double),dimension(3),intent(in)::cart
    real(kind=double)::P
    P=product(cart(1:2))
    if(P>cart(3))then
       voxel_c_to_h_ort=(cart(1)**2 + cart(2)**2)/(1D0-cart(3))
    else
       voxel_c_to_h_ort=0D0
    end if
  end function voxel_c_to_h_ort

  pure function voxel_h_to_c_noparam(hyp)
    real(kind=double)::voxel_h_to_c_noparam
    real(kind=double),dimension(3),intent(in)::hyp
    voxel_h_to_c_noparam=12D0*Sqrt((hyp(1)**6*(1D0-2D0*hyp(2))**4)/(4*hyp(1)**8+(1D0-2D0*hyp(2))**6))
  end function voxel_h_to_c_noparam

  pure function voxel_c_to_h_noparam(cart)
    real(kind=double)::voxel_c_to_h_noparam
    real(kind=double),dimension(3),intent(in)::cart
    real(kind=double)::P
    voxel_c_to_h_noparam=(cart(1)**2+cart(2)**2)/(12D0*(cart(1)*cart(2))**(3D0/4D0)&
         *(cart(2)**2+cart(1)**2)**(2D0/3D0))
  end function voxel_c_to_h_noparam

  pure function voxel_h_to_c_param(hyp)
    real(kind=double)::voxel_h_to_c_param
    real(kind=double),dimension(3),intent(in)::hyp
    voxel_h_to_c_param=12*Sqrt((hyp(1)**6*(1D0-2D0*hyp(2))**4*(hyp(3)-1D0)**2)&
         /((1D0-2D0*hyp(2))**6+4D0*(hyp(3)-(hyp(1)**4*(hyp(3)-1D0)))**2))
  end function voxel_h_to_c_param

  pure function voxel_c_to_h_param(cart)
    real(kind=double)::voxel_c_to_h_param
    real(kind=double),dimension(3),intent(in)::cart
    real(kind=double)::P,T,CP,CM
    P=product(cart(1:2))
    if(P>cart(3))then
       P=P-cart(3)
       CP=cart(1)**2+cart(2)**2
       CM=abs(cart(2)**2-cart(1)**2)
       T=1-cart(3)
       voxel_c_to_h_param=(Cp*sqrt(sqrt(P/T)))/(12*Cm**(2D0/3D0)*P)
    else
       voxel_c_to_h_param=0D0
    end if
  end function voxel_c_to_h_param

  pure function voxel_h_to_c_smooth(hyp)
    real(kind=double)::voxel_h_to_c_smooth
    real(kind=double),dimension(3),intent(in)::hyp
    real(kind=double)::T
    T=1D0-hyp(3)
    voxel_h_to_c_smooth=&
         &8D0*(hyp(1)**3*(1D0+3D0*(hyp(2)-1D0)*hyp(2))*T)&
         &/sqrt((1D0-2D0*hyp(2)*(2D0+hyp(2)*(2D0*hyp(2)-3D0)))**2+4D0*(1D0+(hyp(1)**4-1D0)*T)**2)
  end function voxel_h_to_c_smooth

  pure function voxel_c_to_h_smooth(cart)
    real(kind=double)::voxel_c_to_h_smooth
    real(kind=double),dimension(3),intent(in)::cart
    real(kind=double)::P,S,T,CM,CP
    P=product(cart(1:2))
    if(P>cart(3))then
       P=P-cart(3)
       CP=cart(1)**2+cart(2)**2
       CM=cart(2)**2-cart(1)**2
       T=1-cart(3)
       S=sqrt(3D0+81D0*cm**2)
       voxel_c_to_h_smooth=(3D0**(1D0/3D0)*Cp*(3D0**(1D0/3D0)+(9D0*Cm+S)**(2D0/3D0))&
            *sqrt(sqrt(P/T)))/(4D0*P*S*(9D0*Cm+S)**(1D0/3D0))
    else
       voxel_c_to_h_smooth=0D0
    end if
end function voxel_c_to_h_smooth

!

  pure function voxel_h_to_c_ort_def(hyp)
    real(kind=double)::voxel_h_to_c_ort_def
    real(kind=double),dimension(3),intent(in)::hyp
    voxel_h_to_c_ort_def=voxel_h_to_c_ort(hyp)
  end function voxel_h_to_c_ort_def

  pure function voxel_c_to_h_ort_def(cart)
    real(kind=double)::voxel_c_to_h_ort_def
    real(kind=double),dimension(3),intent(in)::cart
    voxel_c_to_h_ort_def=voxel_c_to_h_ort(cart)
  end function voxel_c_to_h_ort_def

  pure function voxel_h_to_c_param_def(hyp)
    real(kind=double)::voxel_h_to_c_param_def
    real(kind=double),dimension(3),intent(in)::hyp
    voxel_h_to_c_param_def=voxel_h_to_c_param(hyp)
  end function voxel_h_to_c_param_def

  pure function voxel_c_to_h_param_def(cart)
    real(kind=double)::voxel_c_to_h_param_def
    real(kind=double),dimension(3),intent(in)::cart
    voxel_c_to_h_param_def=voxel_c_to_h_param(cart)
  end function voxel_c_to_h_param_def

  pure function voxel_h_to_c_smooth_def(hyp)
    real(kind=double)::voxel_h_to_c_smooth_def
    real(kind=double),dimension(3),intent(in)::hyp
    voxel_h_to_c_smooth_def=voxel_h_to_c_smooth(hyp)
  end function voxel_h_to_c_smooth_def

  pure function voxel_c_to_h_smooth_def(cart)
    real(kind=double)::voxel_c_to_h_smooth_def
    real(kind=double),dimension(3),intent(in)::cart
    voxel_c_to_h_smooth_def=voxel_c_to_h_smooth(cart)
  end function voxel_c_to_h_smooth_def

  pure function denom_cart(cart)
    real(kind=double)::denom_cart
    real(kind=double),dimension(3),intent(in)::cart
    denom_cart=1D0/(864D0*Sqrt(cart(3)**3*(1D0-cart(3)/product(cart(1:2)))))
  end function denom_cart

  pure function denom_ort(hyp)
    real(kind=double)::denom_ort
    real(kind=double),dimension(3),intent(in)::hyp
    real(kind=double)::Y,P
    Y=(1D0-2D0*hyp(2))**2
    P=1D0-hyp(3)
    if(hyp(1)>0D0.and.hyp(3)>0D0)then
       denom_ort=sqrt((P + (-1 + Hyp(1))*P**2)/(746496*hyp(1)*hyp(3)**3*(4*(1 + (-1 + hyp(1))*P)**2 + Y)))

    else
       denom_ort=0D0
    end if
  end function denom_ort

  pure function denom_param(hyp)
    real(kind=double)::denom_param
    real(kind=double),dimension(3),intent(in)::hyp
    real(kind=double)::X,Y,P
    X=hyp(1)**4
    Y=1D0-2D0*hyp(2)
    P=1D0-hyp(3)
    if(hyp(3)>0D0)then
       denom_param=sqrt((P*(1+P*(X-1))*Sqrt(X)*Y**4)/(5184*(4*(1+P*(X-1))**2+Y**6)*hyp(3)**3))
    else
       denom_param=0D0
    end if
  end function denom_param

  pure function denom_param_reg(hyp)
    real(kind=double)::denom_param_reg
    real(kind=double),dimension(3),intent(in)::hyp
    real(kind=double)::X,Y,P
    X=hyp(1)**4
    Y=1D0-2D0*hyp(2)
    P=1D0-hyp(3)
    if(hyp(3)>0D0)then
       denom_param_reg=sqrt((P*(1+P*(X-1))*Sqrt(X)*Y**4)/(5184*(4*(1+P*(X-1))**2+Y**6)*(hyp(3)+norm2_p_t_0)**3))
    else
       denom_param_reg=0D0
    end if
  end function denom_param_reg

  pure function denom_smooth(hyp)
    real(kind=double)::denom_smooth
    real(kind=double),dimension(3),intent(in)::hyp
    real(kind=double)::X,Y,P
    X=hyp(1)**2
    Y=(1D0-2D0*hyp(2))**2
    P=1D0-hyp(3)
    if(hyp(3)>0D0)then
       denom_smooth=sqrt((P*X*(1 + P*(-1 + X**2))*(1 + 3*Y)**2)/(46656*hyp(3)**3&
            *(16*(1 + P*(-1 + X**2))**2 + Y + 2*Y**2 + Y**3)))
    else
       denom_smooth=0D0
    end if
  end function denom_smooth

  pure function denom_smooth_reg(hyp)
    real(kind=double)::denom_smooth_reg
    real(kind=double),dimension(3),intent(in)::hyp
    real(kind=double)::X,Y,P
    X=hyp(1)**2
    Y=(1D0-2D0*hyp(2))**2
    P=1D0-hyp(3)
    if(hyp(3)>0D0)then
       denom_smooth_reg=sqrt((P*X*(1 + P*(-1 + X**2))*(1 + 3*Y)**2)/(46656*(hyp(3)+norm2_p_t_0)**3&
            *(16*(1 + P*(-1 + X**2))**2 + Y + 2*Y**2 + Y**3)))
    else
       denom_smooth_reg=0D0
    end if
  end function denom_smooth_reg
  
   pure function denom_cart_save(cart)
    real(kind=double)::denom_cart_save
    real(kind=double),dimension(3),intent(in)::cart
    if(product(cart(1:2))>cart(3))then
       denom_cart_save=denom_cart(cart)
    else
       denom_cart_save=0D0
    end if
  end function denom_cart_save

  pure function denom_ort_save(hyp)
    real(kind=double)::denom_ort_save
    real(kind=double),dimension(3),intent(in)::hyp
    real(kind=double)::Y,Z,W
    real(kind=double),dimension(3)::cart
    cart=h_to_c_ort(hyp)
    if(cart(1)>1D0.or.cart(2)>1D0)then
       denom_ort_save=0D0
    else
       denom_ort_save=denom_ort(hyp)
    end if
  end function denom_ort_save

  pure function denom_param_save(hyp)
    real(kind=double)::denom_param_save
    real(kind=double),dimension(3),intent(in)::hyp
    real(kind=double)::Y,Z,W
    real(kind=double),dimension(3)::cart
    cart=h_to_c_param(hyp)
    if(cart(1)>1D0.or.cart(2)>1D0)then
       denom_param_save=0D0
    else
       denom_param_save=denom_param(hyp)
    end if
  end function denom_param_save

!  pure 
  function denom_smooth_save(hyp)
    real(kind=double)::denom_smooth_save
    real(kind=double),dimension(3),intent(in)::hyp
    real(kind=double)::Y,Z,W
    real(kind=double),dimension(3)::cart
    cart=h_to_c_smooth(hyp)
    if(cart(1)>1D0.or.cart(2)>1D0)then
       denom_smooth_save=0D0
    else
       denom_smooth_save=denom_smooth(hyp)
    end if
  end function denom_smooth_save

  subroutine denom_cart_cuba_int(d_cart,cart,d_denom,denom,pt2s)
    real(kind=double),dimension(3),intent(in)::cart
    real(kind=double),dimension(1),intent(out)::denom
    real(kind=double),intent(in) :: pt2s
    integer,intent(in)::d_cart,d_denom
    denom(1)=denom_cart_save([cart(1),cart(2),pt2s])
  end subroutine denom_cart_cuba_int

  subroutine denom_ort_cuba_int(d_hyp,hyp,d_denom,denom,pt2s)
    real(kind=double),dimension(3),intent(in)::hyp
    real(kind=double),dimension(1),intent(out)::denom
    real(kind=double),intent(in) :: pt2s
    integer,intent(in)::d_hyp,d_denom
    denom(1)=denom_ort_save([hyp(1),hyp(2),pt2s])
  end subroutine denom_ort_cuba_int

  subroutine denom_param_cuba_int(d_hyp,hyp,d_denom,denom,pt2s)
    real(kind=double),dimension(3),intent(in)::hyp
    real(kind=double),dimension(1),intent(out)::denom
    real(kind=double),intent(in) :: pt2s
    integer,intent(in)::d_hyp,d_denom
    denom(1)=denom_param_save([hyp(1),hyp(2),pt2s])
  end subroutine denom_param_cuba_int

  subroutine denom_smooth_cuba_int(d_hyp,hyp,d_denom,denom,pt2s)
    real(kind=double),dimension(3),intent(in)::hyp
    real(kind=double),dimension(1),intent(out)::denom
    real(kind=double),intent(in) :: pt2s
    integer,intent(in)::d_hyp,d_denom
    denom(1)=denom_smooth_save([hyp(1),hyp(2),pt2s])
  end subroutine denom_smooth_cuba_int

  subroutine coordinates_hcd_cart(hyp,cart,denom)
    real(kind=double),dimension(3),intent(in)::hyp
    real(kind=double),dimension(3),intent(out)::cart
    real(kind=double),intent(out)::denom
    cart=hyp
    denom=denom_cart_save(cart)
  end subroutine coordinates_hcd_cart

  subroutine coordinates_hcd_ort(hyp,cart,denom)
    real(kind=double),dimension(3),intent(in)::hyp
    real(kind=double),dimension(3),intent(out)::cart
    real(kind=double),intent(out)::denom
    cart=h_to_c_ort(hyp)
    denom=denom_ort(hyp)
  end subroutine coordinates_hcd_ort

  subroutine coordinates_hcd_param(hyp,cart,denom)
    real(kind=double),dimension(3),intent(in)::hyp
    real(kind=double),dimension(3),intent(out)::cart
    real(kind=double),intent(out)::denom
    cart=h_to_c_param(hyp)
    denom=denom_param(hyp)
  end subroutine coordinates_hcd_param

  subroutine coordinates_hcd_param_reg(hyp,cart,denom)
    real(kind=double),dimension(3),intent(in)::hyp
    real(kind=double),dimension(3),intent(out)::cart
    real(kind=double),intent(out)::denom
    cart=h_to_c_param(hyp)
    denom=denom_param_reg(hyp)
  end subroutine coordinates_hcd_param_reg

  subroutine coordinates_hcd_smooth(hyp,cart,denom)
    real(kind=double),dimension(3),intent(in)::hyp
    real(kind=double),dimension(3),intent(out)::cart
    real(kind=double),intent(out)::denom
    cart=h_to_c_smooth(hyp)
    denom=denom_smooth(hyp)
  end subroutine coordinates_hcd_smooth
  
  subroutine coordinates_hcd_smooth_reg(hyp,cart,denom)
    real(kind=double),dimension(3),intent(in)::hyp
    real(kind=double),dimension(3),intent(out)::cart
    real(kind=double),intent(out)::denom
    cart=h_to_c_smooth(hyp)
    denom=denom_smooth_reg(hyp)
  end subroutine coordinates_hcd_smooth_reg
  
  pure function pdf_in_in_kind(process_id,double_pdf_id,c1,c2,gev_pt)
    real(kind=double)::pdf_in_in_kind
    real(kind=double),intent(in)::c1,c2,gev_pt
    integer,intent(in)::process_id,double_pdf_id
    real(kind=double)::pdf1,pdf2
    call single_pdf(valid_processes(1,process_id),double_pdf_kinds(1,double_pdf_id),c1,gev_pt,pdf1)
    call single_pdf(valid_processes(2,process_id),double_pdf_kinds(2,double_pdf_id),c2,gev_pt,pdf2)
    pdf_in_in_kind=pdf1*pdf2
  contains
    pure subroutine single_pdf(flavor,pdf_kind,c,gev_pt,pdf)
      integer,intent(in)::flavor,pdf_kind
      real(kind=double),intent(in)::c,gev_pt
      real(kind=double),intent(out)::pdf
      real(kind=double),dimension(-6:6)::lha_pdf
      call evolvePDF(c,gev_pt,lha_pdf)
      select case(pdf_kind)
      case(1)
         pdf=lha_pdf(0)
      case(2)
         if(flavor==1.or.flavor==2)then
            pdf=lha_pdf(-flavor)
         else
            pdf=lha_pdf(flavor)
         end if
      case(3)
         pdf=lha_pdf(1)-lha_pdf(-1)
      case(4)
         pdf=lha_pdf(2)-lha_pdf(-2)
      end select
    end subroutine single_pdf
  end function pdf_in_in_kind

  elemental function ps_io_pol(process_io_id,pt2shat)
    real(kind=double)::ps_io_pol
    integer,intent(in)::process_io_id
    real(kind=double),intent(in)::pt2shat
    ps_io_pol=dot_product(&
         [1D0,pt2shat,pt2shat**2,pt2shat**3]&
         ,phase_space_coefficients_inout(1:4,valid_processes(6,process_io_id)))
  end function ps_io_pol

  pure subroutine interactions_dddsigma(process_id,double_pdf_id,hyp,cart,dddsigma)
    real(kind=double),intent(out)::dddsigma
    integer,intent(in)::process_id,double_pdf_id
    real(kind=double),dimension(3),intent(in)::hyp
    real(kind=double),dimension(3),intent(out)::cart
    real(kind=double)::a,pt2shat,gev_pt
    cart=h_to_c_param(hyp)
    a=product(cart(1:2))
    if(cart(1)<=1D0.and.cart(2)<=1D0)then
       pt2shat=hyp(3)/a
       gev_pt=sqrt(hyp(3))*gev_pt_max
!       print *,process_id,pt2shat
       dddsigma=&
            &const_pref&
            &*alphasPDF(gev_pt)**2&
            &*ps_io_pol(process_id,pt2shat)&
            &*pdf_in_in_kind(process_id,double_pdf_id,cart(1),cart(2),gev_pt)&
            &*denom_param(hyp)&
            &/a
    else
       dddsigma=0D0
    end if
  end subroutine interactions_dddsigma

  pure subroutine interactions_dddsigma_reg(process_id,double_pdf_id,hyp,cart,dddsigma)
    real(kind=double),intent(out)::dddsigma
    integer,intent(in)::process_id,double_pdf_id
    real(kind=double),dimension(3),intent(in)::hyp
    real(kind=double),dimension(3),intent(out)::cart
    real(kind=double)::a,pt2shat,gev_pt,gev2_pt
    cart=h_to_c_param(hyp)
    a=product(cart(1:2))
    if(cart(1)<=1D0.and.cart(2)<=1D0)then
       pt2shat=hyp(3)/a
       gev_pt=sqrt(hyp(3))*gev_pt_max
       gev2_pt=hyp(3)*gev2_pt_max
!       print *,process_id,pt2shat
       dddsigma=&
            &const_pref&
            &*alphasPDF(sqrt(gev2_pt+gev2_p_t_0))**2&
            &*ps_io_pol(process_id,pt2shat)&
            &*pdf_in_in_kind(process_id,double_pdf_id,cart(1),cart(2),gev_pt)&
            &*denom_param_reg(hyp)&
            &/a
    else
       dddsigma=0D0
    end if
  end subroutine interactions_dddsigma_reg

  subroutine interactions_dddsigma_print(process_id,double_pdf_id,hyp,cart,dddsigma)
    real(kind=double),intent(out)::dddsigma
    integer,intent(in)::process_id,double_pdf_id
    real(kind=double),dimension(3),intent(in)::hyp
    real(kind=double),dimension(3),intent(out)::cart
    real(kind=double)::a,pt2shat,gev_pt
    cart=h_to_c_param(hyp)
    a=product(cart(1:2))
    if(cart(1)<=1D0.and.cart(2)<=1D0)then
       pt2shat=hyp(3)/a
       gev_pt=sqrt(hyp(3))*gev_pt_max
!       print *,process_id,pt2shat
       dddsigma=&
            &const_pref&
!            &*alphasPDF(gev_pt)**2&
            &*ps_io_pol(process_id,pt2shat)&
            &*pdf_in_in_kind(process_id,double_pdf_id,cart(1),cart(2),gev_pt)&
            &*denom_param(hyp)&
            &/a
    else
       dddsigma=0D0
    end if
    write(11,fmt=*)dddsigma,pt2shat,&
         pdf_in_in_kind(process_id,double_pdf_id,cart(1),cart(2),&
         gev_pt),ps_io_pol(process_id,pt2shat),const_pref,denom_param(hyp),a
    flush(11)
  end subroutine interactions_dddsigma_print

  pure subroutine interactions_dddsigma_cart(process_id,double_pdf_id,cart,dddsigma)
    real(kind=double),intent(out)::dddsigma
    integer,intent(in)::process_id,double_pdf_id
    real(kind=double),dimension(3),intent(in)::cart
    real(kind=double)::a,pt2shat,gev_pt
    a=product(cart(1:2))
    if(cart(1)<=1D0.and.cart(2)<=1D0)then
       pt2shat=cart(3)/a
       gev_pt=sqrt(cart(3))*gev_pt_max
!       print *,process_id,pt2shat
       dddsigma=&
            &const_pref&
            &*alphasPDF(gev_pt)**2&
            &*ps_io_pol(process_id,pt2shat)&
            &*pdf_in_in_kind(process_id,double_pdf_id,cart(1),cart(2),gev_pt)&
            &*denom_cart(cart)&
            &/a
    else
       dddsigma=0D0
    end if
  end subroutine interactions_dddsigma_cart

  subroutine cuba_gg_me_smooth(d_hyp,hyp,d_me,me,pt2s)
    integer,intent(in)::d_hyp,d_me
    real(kind=double),dimension(d_hyp),intent(in)::hyp
    real(kind=double),dimension(1),intent(out)::me
    real(kind=double),dimension(3)::cart
    real(kind=double),intent(in)::pt2s
    real(kind=double)::p,p2
    if(d_hyp==3)then
       p=hyp(3)
       p2=hyp(3)**2
    else
       if(d_hyp==2)then
          p=sqrt(pt2s)
          p2=pt2s
       end if
    end if
    cart=h_to_c_smooth([hyp(1),hyp(2),p2])
    if(p>pts_min.and.product(cart(1:2))>p2)then
       me(1)=&
            &const_pref&
            &*alphasPDF(p*gev_pt_max)**2&
            &*ps_io_pol(109,p2)&
            &*pdf_in_in_kind(109,11,cart(1),cart(2),p2)&
            &*denom_smooth([hyp(1),hyp(2),p2])&
            &/product(cart(1:2))
    else
       me(1)=0D0
    end if
  end subroutine cuba_gg_me_smooth

  subroutine cuba_gg_me_param(d_hyp,hyp,d_me,me,pt2s)
    integer,intent(in)::d_hyp,d_me
    real(kind=double),dimension(d_hyp),intent(in)::hyp
    real(kind=double),dimension(1),intent(out)::me
    real(kind=double),dimension(3)::cart
    real(kind=double),intent(in)::pt2s
    real(kind=double)::p,p2
    if(d_hyp==3)then
       p=hyp(3)
       p2=hyp(3)**2
    else
       if(d_hyp==2)then
          p=sqrt(pt2s)
          p2=pt2s
       end if
    end if
    cart=h_to_c_param([hyp(1),hyp(2),p2])
    if(p>pts_min.and.product(cart(1:2))>p2)then
       me(1)=&
            &const_pref&
            &*alphasPDF(p*gev_pt_max)**2&
            &*ps_io_pol(109,p2)&
            &*pdf_in_in_kind(109,11,cart(1),cart(2),p2)&
            &*denom_param([hyp(1),hyp(2),p2])&
            &/product(cart(1:2))
    else
       me(1)=0D0
    end if
  end subroutine cuba_gg_me_param

  subroutine cuba_gg_me_ort(d_hyp,hyp,d_me,me,pt2s)
    integer,intent(in)::d_hyp,d_me
    real(kind=double),dimension(d_hyp),intent(in)::hyp
    real(kind=double),dimension(1),intent(out)::me
    real(kind=double),dimension(3)::cart
    real(kind=double),intent(in)::pt2s
    real(kind=double)::p,p2
    if(d_hyp==3)then
       p=hyp(3)
       p2=hyp(3)**2
    else
       if(d_hyp==2)then
          p=sqrt(pt2s)
          p2=pt2s
       end if
    end if
    cart=h_to_c_ort([hyp(1),cart(2),p2])
    if(p>pts_min.and.product(cart(1:2))>p2)then
       me(1)=&
            &const_pref&
            &*alphasPDF(p*gev_pt_max)**2&
            &*ps_io_pol(109,p2)&
            &*pdf_in_in_kind(109,11,cart(1),cart(2),p2)&
            &*denom_ort([hyp(1),hyp(2),p2])&
            &/product(cart(1:2))
    else
       me(1)=0D0
    end if
  end subroutine cuba_gg_me_ort

  subroutine cuba_gg_me_cart(d_cart,cart,d_me,me,pt2s)
    integer,intent(in)::d_cart,d_me
    real(kind=double),dimension(d_cart),intent(in)::cart
    real(kind=double),dimension(1),intent(out)::me
    real(kind=double),intent(in)::pt2s
    real(kind=double)::a,p,p2
    if(d_cart==3)then
       p=cart(3)
       p2=cart(3)**2
    else
       if(d_cart==2)then
          p=sqrt(pt2s)
          p2=pt2s
       end if
    end if
    a=product(cart(1:2))
    if(p>pts_min.and.a>p2)then
       me(1)=&
            &const_pref&
            &*alphasPDF(p*gev_pt_max)**2&
            &*ps_io_pol(109,p2)&
            &*pdf_in_in_kind(109,11,cart(1),cart(2),p2)&
            &*denom_cart([cart(1),cart(2),p2])&
            &/a
    else
       me(1)=0D0
    end if
  end subroutine cuba_gg_me_cart

  subroutine interactions_proton_proton_integrand_generic_17_reg(hyp_2,trafo,f,pt)
    real(kind=double),dimension(2),intent(in)::hyp_2
    procedure(coord_hcd_in)::trafo
    real(kind=double),dimension(17),intent(out)::f
    class(transversal_momentum_type), intent(in) :: pt
    real(kind=double),dimension(3)::cart,hyp_3
    real(kind=double),dimension(5)::psin
    real(kind=double),dimension(-6:6)::c,d
    real(kind=double)::gev_pt,gev2_pt,pts,pt2s,pt2shat,a,&
         pdf_seaquark_seaquark,pdf_seaquark_gluon,pdf_gluon_gluon,&
         pdf_up_seaquark,pdf_up_gluon,pdf_down_seaquark,pdf_down_gluon,&
         v1u,v1d,v2u,v2d,denom
        
    pts=pt%get_unit_scale()
    pt2s=pt%get_unit2_scale()
    gev_pt=pt%get_gev_scale()
    gev2_pt=pt%get_gev2_scale()

    hyp_3(1:2)=hyp_2
    hyp_3(3)=pt2s
    call trafo(hyp_3,cart,denom)
    a=product(cart(1:2))
    if(cart(1)<=1D0.and.cart(2)<=1D0.and.a>pt2s)then
       pt2shat=pt2s/a

       ! phase space polynom
       psin=matmul([1D0,pt2shat,pt2shat**2,pt2shat**3],phase_space_coefficients_in)
       ! pdf
       call evolvepdf(cart(1),gev_pt,c)
       call evolvepdf(cart(2),gev_pt,d)
       !c=[1,1,1,1,1,1,1,1,1,1,1,1,1]*1D0
       !d=c
       v1d=c(1)-c(-1)
       v1u=c(2)-c(-2)
       v2d=d(1)-d(-1)
       v2u=d(2)-d(-2)
       c(1)=c(-1)
       c(2)=c(-2)
       d(1)=d(-1)
       d(2)=d(-2)
       f(1)=0D0
       !gluon_gluon
       f( 2)=(&
            !type5
            &c(0)*d(0)&
            &)*psin(5)
       !gluon_seaquark
       f( 3)=(&
            !type4
            &c(0)*d(-4)+c(0)*d(-3)+c(0)*d(-2)+c(0)*d(-1)+c(0)*d(1)+c(0)*d(2)+c(0)*d(3)+c(0)*d(4)&
            &)*psin(4)
       !gluon_down
       f( 4)=(&
            !type4
            &c( 0)*v2d&
            &)*psin(4)
       !gluon_up
       f( 5)=(&
            !type4
            &c(0)*v2u&
            &)*psin(4)
       !seaquark_gluon
       f( 6)=(&
            !type4
            &c(-4)*d(0)+c(-3)*d(0)+c(-2)*d(0)+c(-1)*d(0)+c(1)*d(0)+c(2)*d(0)+c(3)*d(0)+c(4)*d(0)&
            &)*psin(4)
       !seaquark_seaquark
       f( 7)=&
            !type1
            (c(-4)*d(-3)+c(-4)*d(-2)+c(-4)*d(-1)+c(-4)*d( 1)+c(-4)*d( 2)+c(-4)*d( 3)+&
             c(-3)*d(-4)+c(-3)*d(-2)+c(-3)*d(-1)+c(-3)*d( 1)+c(-3)*d( 2)+c(-3)*d( 4)+&
             c(-2)*d(-4)+c(-2)*d(-3)+c(-2)*d(-1)+c(-2)*d( 1)+c(-2)*d( 3)+c(-2)*d( 4)+&
             c(-1)*d(-4)+c(-1)*d(-3)+c(-1)*d(-2)+c(-1)*d( 2)+c(-1)*d( 3)+c(-1)*d( 4)+&
             c( 1)*d(-4)+c( 1)*d(-3)+c( 1)*d(-2)+c( 1)*d( 2)+c( 1)*d( 3)+c( 1)*d( 4)+&
             c( 2)*d(-4)+c( 2)*d(-3)+c( 2)*d(-1)+c( 2)*d( 1)+c( 2)*d( 3)+c( 2)*d( 4)+&
             c( 3)*d(-4)+c( 3)*d(-2)+c( 3)*d(-1)+c( 3)*d( 1)+c( 3)*d( 2)+c( 3)*d( 4)+&
             c( 4)*d(-3)+c( 4)*d(-2)+c( 4)*d(-1)+c( 4)*d( 1)+c( 4)*d( 2)+c( 4)*d( 3))&
             *psin(1)&
            !type2
             +(c(-4)*d(-4)+c(-3)*d(-3)+c(-2)*d(-2)+c(-1)*d(-1)+c( 4)*d( 4)+c( 3)*d( 3)+c(2)*d( 2)+c(1)*d( 1))&
             *psin(2)&
             !type3
             +(c(-4)*d( 4)+c(-3)*d( 3)+c(-2)*d( 2)+c(-1)*d( 1)+c( 4)*d(-4)+c( 3)*d(-3)+c(2)*d(-2)+c(1)*d(-1))&
             *psin(3)
       !seaquark_down
       f( 8)=&
            !type1
            (c(-4)*v2d+c(-3)*v2d+c(-2)*v2d+c( 2)*v2d+c( 3)*v2d+c( 4)*v2d)&
            *psin(1)&
            !type2
            +c( 1)*v2d&
            *psin(2)&
            !type3
            +c(-1)*v2d&
            *psin(3)
       !seaquark_up
       f( 9)=&
            !type1
            (c(-4)*v2u+c(-3)*v2u+c(-1)*v2u+c( 1)*v2u+c( 3)*v2u+c( 4)*v2u)&
            *psin(1)&
            !type2
            +c(2)*v2u&
            *psin(2)&
            !type3
            +c(-2)*v2u&
            *psin(3)
       !down_gluon
       f(10)=(&
            !type4
            v1d*d( 0)&
            )*psin(4)
       !down_seaquark
       f(11)=&
            !type1
            (v1d*d(-4)+v1d*d(-3)+v1d*d(-2)+v1d*d( 2)+v1d*d( 3)+v1d*d( 4))&
            *psin(1)&
            !type2
            +v1d*d( 1)&
            *psin(2)&
            !type3
            +v1d*d(-1)&
            *psin(3)
       !down_down
       f(12)=v1d*v2d*psin(2)
       !down_up
       f(13)=v1d*v2u*psin(1)
       !up_gluon
       f(14)=(&
            !type4
            &v1u*d(0)&
            &)*psin(4)
       !up_seaquark
       f(15)=&
            !type1
            (v1u*d(-4)+v1u*d(-3)+v1u*d(-1)+v1u*d( 1)+v1u*d( 3)+v1u*d( 4))&
            *psin(1)&
            !type2
            +v1u*d(2)&
            *psin(2)&
            !type3
            +v1u*d(-2)&
            *psin(3)
       !up_down
       f(16)=v1u*v2d*psin(1)
       !up_up
       f(17)=v1u*v2u*psin(2)
       f=f&
       *const_pref&
            *alphasPDF(sqrt(gev2_pt+gev2_p_t_0))**2&
            *denom&
            /a
       !       print *,const_pref,alphasPDF(gev_pt)**2,denom_smooth(hyp),a
    else
       f=[0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0]
    end if
!    print *,pt2shat,c(0)*d(0),psin(5),const_pref,alphasPDF(gev_pt)**2,denom,a
  end subroutine interactions_proton_proton_integrand_generic_17_reg

!!$  subroutine coordinates_proton_proton_integrand_cart_11(d_hyp,hyp_2,d_f,f)
!!$    integer,intent(in)::d_hyp,d_f
!!$    real(kind=double),dimension(2),intent(in)::hyp_2
!!$    real(kind=double),dimension(11),intent(out)::f
!!$    call coordinates_proton_proton_integrand_generic_11(hyp_2,coordinates_hcd_cart,f)
!!$!    write (51,*)hyp_2,momentum_get_pts_scale(),f
!!$  end subroutine coordinates_proton_proton_integrand_cart_11
!!$
!!$  subroutine coordinates_proton_proton_integrand_ort_11(d_hyp,hyp_2,d_f,f)
!!$    integer,intent(in)::d_hyp,d_f
!!$    real(kind=double),dimension(2),intent(in)::hyp_2
!!$    real(kind=double),dimension(11),intent(out)::f
!!$    call coordinates_proton_proton_integrand_generic_11(hyp_2,coordinates_hcd_ort,f)
!!$!    write (52,*)hyp_2,momentum_get_pts_scale(),f
!!$  end subroutine coordinates_proton_proton_integrand_ort_11
!!$
!!$  subroutine coordinates_proton_proton_integrand_param_11(d_hyp,hyp_2,d_f,f)
!!$    integer,intent(in)::d_hyp,d_f
!!$    real(kind=double),dimension(2),intent(in)::hyp_2
!!$    real(kind=double),dimension(11),intent(out)::f
!!$    call coordinates_proton_proton_integrand_generic_11(hyp_2,coordinates_hcd_param,f)
!!$!    write (53,*)hyp_2,momentum_get_pts_scale(),f
!!$  end subroutine coordinates_proton_proton_integrand_param_11
!!$
!!$  subroutine coordinates_proton_proton_integrand_smooth_11(d_hyp,hyp_2,d_f,f)
!!$    integer,intent(in)::d_hyp,d_f
!!$    real(kind=double),dimension(2),intent(in)::hyp_2
!!$    real(kind=double),dimension(11),intent(out)::f
!!$    call coordinates_proton_proton_integrand_generic_11(hyp_2,coordinates_hcd_smooth,f)
!!$!    write (54,*)hyp_2,momentum_get_pts_scale(),f
!!$  end subroutine coordinates_proton_proton_integrand_smooth_11

  subroutine interactions_proton_proton_integrand_param_17_reg(d_hyp,hyp_2,d_f,f,pt)
    integer,intent(in)::d_hyp,d_f
    real(kind=double),dimension(2),intent(in)::hyp_2
    real(kind=double),dimension(17),intent(out)::f
    class(transversal_momentum_type), intent(in) :: pt
    call interactions_proton_proton_integrand_generic_17_reg(hyp_2,coordinates_hcd_param_reg,f,pt)
    !    write (53,*)hyp_2,momentum_get_pts_scale(),f
  end subroutine interactions_proton_proton_integrand_param_17_reg
  
  subroutine interactions_proton_proton_integrand_smooth_17_reg(d_hyp,hyp_2,d_f,f,pt)
    integer,intent(in)::d_hyp,d_f
    real(kind=double),dimension(2),intent(in)::hyp_2
    real(kind=double),dimension(17),intent(out)::f
    class(transversal_momentum_type), intent(in) :: pt
    call interactions_proton_proton_integrand_generic_17_reg(hyp_2,coordinates_hcd_smooth_reg,f,pt)
    !    write (53,*)hyp_2,momentum_get_pts_scale(),f
  end subroutine interactions_proton_proton_integrand_smooth_17_reg
  

end module muli_interactions
