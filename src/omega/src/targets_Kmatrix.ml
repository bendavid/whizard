(* $Id: targets_Kmatrix.ml 2975 2011-01-25 14:04:43Z jr_reuter $

   Copyright (C) 1999-2011 by

       Wolfgang Kilian <kilian@hep.physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>
       Christian Speckner <christian.speckner@physik.uni-freiburg.de>

   WHIZARD is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   WHIZARD is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *)

let rcs_file = RCS.parse "Targets_Kmatrix" ["K-Matrix Support routines"]
    { RCS.revision = "$Revision: 2975 $";
      RCS.date = "$Date: 2011-01-25 15:04:43 +0100 (Tue, 25 Jan 2011) $";
      RCS.author = "$Author: jr_reuter $";
      RCS.source
        = "$URL: svn+ssh://jr_reuter@login.hepforge.org/hepforge/svn/whizard/trunk/src/omega/src/targets_Kmatrix.ml $" }

module Fortran =
  struct

    open Format

    let nl = print_newline

(* Special functions for the K matrix approach. This might be generalized
   to other functions that have to have access to the parameters and
   coupling constants. At the moment, this is hardcoded. *)

    let print pure_functions = 
      let pure =
        if pure_functions then
          "pure "
        else 
          "" in
      printf "  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"; nl ();
      printf "  !!! Special K matrix functions"; nl (); 
      printf "  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"; nl (); 
      nl();
      printf "  %sfunction width_res (z,res,w_wkm,m,g) result (w)" pure; nl ();
      printf "      real(kind=default), intent(in) :: z, w_wkm, m, g"; nl ();
      printf "      integer, intent(in) :: res"; nl ();
      printf "      real(kind=default) :: w"; nl ();
      printf "      if (z.eq.0) then"; nl ();
      printf "        w = 0"; nl ();
      printf "      else"; nl ();
      printf "        if (w_wkm.eq.0) then"; nl ();
      printf "          select case (res)"; nl ();
      printf "            case (1) !!! Scalar isosinglet"; nl ();
      printf "              w = 3.*g**2/32./PI * m**3/vev**2"; nl ();
      printf "            case (2) !!! Scalar isoquintet"; nl ();
      printf "              w = g**2/64./PI * m**3/vev**2"; nl ();
      printf "            case (3) !!! Vector isotriplet"; nl ();
      printf "              w = g**2/48./PI * m"; nl ();
      printf "            case (4) !!! Tensor isosinglet"; nl ();
      printf "              w = g**2/320./PI * m**3/vev**2"; nl ();
      printf "            case (5) !!! Tensor isoquintet"; nl ();
      printf "              w = 3.*g**2/1920./PI * m**3/vev**2"; nl ();
      printf "            case default"; nl ();
      printf "              w = 0"; nl ();
      printf "          end select"; nl ();
      printf "        else"; nl ();
      printf "          w = w_wkm"; nl ();
      printf "        end if"; nl ();
      printf "      end if"; nl ();
      printf "  end function width_res"; nl ();          
      nl ();
      printf "  %sfunction s0stu (s, m) result (s0)" pure; nl ();
      printf "      real(kind=default), intent(in) :: s, m"; nl ();
      printf "      real(kind=default) :: s0"; nl ();
      printf "      if (m.ge.1.0e08) then"; nl ();
      printf "        s0 = 0"; nl ();  
      printf "      else"; nl ();
      printf "        s0 = m**2 - s/2 + m**4/s * log(m**2/(s+m**2))"; nl ();
      printf "      end if"; nl ();
      printf "  end function s0stu"; nl(); 
      nl ();
      printf "  %sfunction s1stu (s, m) result (s1)" pure; nl ();
      printf "      real(kind=default), intent(in) :: s, m"; nl ();
      printf "      real(kind=default) :: s1"; nl ();
      printf "      if (m.ge.1.0e08) then"; nl ();
      printf "        s1 = 0"; nl ();  
      printf "      else"; nl ();
      printf "        s1 = 2*m**4/s + s/6 + m**4/s**2*(2*m**2+s) &"; nl();
      printf "             * log(m**2/(s+m**2))"; nl ();
      printf "      end if"; nl ();
      printf "  end function s1stu"; nl(); 
      nl ();
      printf "  %sfunction s2stu (s, m) result (s2)" pure; nl ();
      printf "      real(kind=default), intent(in) :: s, m"; nl ();
      printf "      real(kind=default) :: s2"; nl ();
      printf "      if (m.ge.1.0e08) then"; nl ();
      printf "        s2 = 0"; nl ();  
      printf "      else"; nl ();
      printf "        s2 = m**4/s**2 * (6*m**2 + 3*s) + &"; nl();
      printf "             m**4/s**3 * (6*m**4 + 6*m**2*s + s**2) &"; nl();
      printf "             * log(m**2/(s+m**2))"; nl ();
      printf "      end if"; nl ();
      printf "  end function s2stu"; nl(); 
      nl ();
      printf "  %sfunction p0stu (s, m) result (p0)" pure; nl ();
      printf "      real(kind=default), intent(in) :: s, m"; nl ();
      printf "      real(kind=default) :: p0"; nl ();
      printf "      if (m.ge.1.0e08) then"; nl ();
      printf "        p0 = 0"; nl ();  
      printf "      else"; nl ();
      printf "        p0 = 1 + (2*s+m**2)*log(m**2/(s+m**2))/s"; nl ();
      printf "      end if"; nl (); 
      printf "  end function p0stu"; nl(); 
      nl ();
      printf "  %sfunction p1stu (s, m) result (p1)" pure; nl ();
      printf "      real(kind=default), intent(in) :: s, m"; nl ();
      printf "      real(kind=default) :: p1"; nl ();
      printf "      if (m.ge.1.0e08) then"; nl ();
      printf "        p1 = 0"; nl ();  
      printf "      else"; nl ();
      printf "        p1 = (m**2 + 2*s)/s**2 * (2*s+(2*m**2+s) &"; nl(); 
      printf "                * log(m**2/(s+m**2)))"; nl ();
      printf "      end if"; nl ();
      printf "  end function p1stu"; nl(); 
      nl ();
      printf "  %sfunction d0stu (s, m) result (d0)" pure; nl ();
      printf "      real(kind=default), intent(in) :: s, m"; nl ();
      printf "      real(kind=default) :: d0"; nl ();
      printf "      if (m.ge.1.0e08) then"; nl ();
      printf "        d0 = 0"; nl ();  
      printf "      else"; nl ();
      printf "        d0 = (2*m**2+11*s)/2 + (m**4+6*m**2*s+6*s**2) &"; nl();
      printf "              /s * log(m**2/(s+m**2))"; nl ();
      printf "      end if"; nl ();
      printf "  end function d0stu"; nl(); 
      nl ();
      printf "  %sfunction d1stu (s, m) result (d1)" pure; nl ();
      printf "      real(kind=default), intent(in) :: s, m"; nl ();
      printf "      real(kind=default) :: d1"; nl ();
      printf "      if (m.ge.1.0e08) then"; nl ();
      printf "        d1 = 0"; nl ();  
      printf "      else"; nl ();
      printf "        d1 = (s*(12*m**4 + 72*m**2*s + 73*s**2) &"; nl();
      printf "            + 6*(2*m**2 + s)*(m**4 + 6*m**2*s + 6*s**2) &"; nl();
      printf "            * log(m**2/(s+m**2)))/6/s**2"; nl ();
      printf "      end if"; nl ();
      printf "  end function d1stu"; nl(); 
      nl ();
      printf "  %sfunction da00 (cc, s, m) result (amp_00)" pure; nl ();
      printf "      real(kind=default), intent(in) :: s"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: m, cc"; nl ();
      printf "      real(kind=default) :: a00_0, a00_1"; nl ();
      printf "      complex(kind=default), dimension(1:6) :: a00"; nl ();
      printf "      complex(kind=default) :: ii, jj, amp_00"; nl ();
      printf "      ii = cmplx(0.0,1.0/32.0/Pi,default)"; nl ();
      printf "      jj = s**2/vev**4*ii"; nl ();
      printf "      !!! Scalar isosinglet"; nl ();
      printf "      if (cc(1).ne.0) then"; nl ();
      printf "        if (fudge_km.ne.0) then"; nl ();
      printf "          a00(1) = vev**4/s**2 * fudge_km * &"; nl ();
      printf "              cmplx(0.0,32.0*Pi,default)*(1.0 + &"; nl ();
      printf "              (s-m(1)**2)/(ii*cc(1)**2/vev**2*(3.0*s**2 + &"; nl ();
      printf "              (s-m(1)**2)*2.0*s0stu(s,m(1))) - (s-m(1)**2)))"; nl ();
      printf "        else"; nl ();
      printf "          a00(1) = vev**2/s**2 * cc(1)**2 * &"; nl ();
      printf "              (3.0 * s**2/cmplx(s-m(1)**2,m(1)*width_res(w_res,1,&"; nl ();
      printf "              wkm(1),m(1),cc(1)),default) + 2.0 * s0stu(s,m(1)))"; nl ();
      printf "        end if"; nl ();          
      printf "      else"; nl ();
      printf "         a00(1) = 0"; nl ();
      printf "      end if"; nl ();
      printf "      !!! Scalar isoquintet"; nl ();
      printf "      a00(2) = 5.0*cc(2)**2/vev**2 * s0stu(s,m(2)) / 3.0"; nl ();
      printf "      a00(2) = vev**4/s**2*a00(2) /&"; nl();
      printf "                  (1.0_default - fudge_km*ii*a00(2))"; nl ();
      printf "      !!! Vector isotriplet"; nl ();
      printf "      a00(3) = cc(3)**2*(4.0*p0stu(s,m(3)) + 3.0*s/m(3)**2)"; nl ();
      printf "      a00(3) = vev**4/s**2*a00(3)/&"; nl ();
      printf "                  (1.0_default - fudge_km*ii*a00(3))"; nl ();
      printf "      !!! Tensor isosinglet"; nl ();
      printf "      a00(4) = cc(4)**2/vev**2 * (d0stu(s,m(4)) &"; nl ();
      printf "              /3.0 + 11.0*s**2/m(4)**2/36.0)"; nl ();
      printf "      a00(4) = vev**4/s**2*a00(4)/&"; nl ();
      printf "                 (1.0_default - fudge_km*ii*a00(4))"; nl ();
      printf "      !!! Tensor isoquintet"; nl ();
      printf "      a00(5) = 5.0*cc(5)**2/vev**2*(d0stu(s,m(5))&"; nl ();
      printf "              /3.0 + s**2/m(5)**2/18.0)/6.0"; nl ();
      printf "      a00(5) = vev**4/s**2*a00(5)/&"; nl ();
      printf "                 (1.0_default - fudge_km*ii*a00(5))"; nl ();
      printf "      !!! Low energy theory alphas"; nl ();
      printf "      a00_0 = 2*fudge_higgs*vev**2/s + 8*(7*a4 + 11*a5)/3"; nl ();
      printf "      a00_1 = 25*log(lam_reg**2/s)/9 + 11./54.0_default"; nl ();
      printf "      a00(6) = a00_0 !!! + a00_1/16/Pi**2"; nl ();
      printf "      a00(6) = fudge_km*jj*a00(6)**2 / (1.0_default - jj*a00(6))"; nl ();
      printf "      amp_00 = sum(a00)"; nl ();
      printf "  end function da00"; nl();
      nl ();
      printf "  %sfunction da02 (cc, s, m) result (amp_02)" pure; nl ();
      printf "      real(kind=default), intent(in) :: s"; nl ();
      printf "      real(kind=default), dimension(5), intent(in) :: m, cc"; nl ();
      printf "      real(kind=default) :: a02_0, a02_1"; nl ();
      printf "      complex(kind=default), dimension(1:6) :: a02"; nl ();
      printf "      complex(kind=default) :: ii, jj, amp_02"; nl ();
      printf "      ii = cmplx(0.0,1.0/32.0/Pi,default)"; nl ();
      printf "      jj = s**2/vev**4*ii"; nl ();
      printf "      !!! Scalar  isosinglet"; nl ();
      printf "      a02(1) = 2.0*cc(1)**2/vev**2 * s2stu(s,m(1))"; nl ();
      printf "      a02(1) = vev**4/s**2*a02(1)/&"; nl (); 
      printf "                 (1.0_default - fudge_km*ii*a02(1))"; nl ();
      printf "      !!! Scalar isoquintet"; nl ();
      printf "      a02(2) = 5.0*cc(2)**2/vev**2 * s2stu(s,m(2)) / 3.0"; nl ();
      printf "      a02(2) = vev**4/s**2*a02(2)/&"; nl (); 
      printf "                 (1.0_default - fudge_km*ii*a02(2))"; nl ();
      printf "      !!! Vector isotriplet"; nl (); 
      printf "      a02(3) = 4.0*cc(3)**2*(2*s+m(3)**2)*s2stu(s,m(3))/m(3)**4"; nl ();
      printf "      a02(3) = vev**4/s**2*a02(3)/&"; nl ();
      printf "                 (1.0_default - fudge_km*ii*a02(3))"; nl ();
      printf "      !!! Tensor isosinglet"; nl (); 
      printf "      if (cc(4).ne.0) then"; nl ();
      printf "        if (fudge_km.ne.0) then"; nl ();
      printf "         a02(4) = vev**4/s**2 * fudge_km * &"; nl ();
      printf "              cmplx(0.0,32.0*Pi,default)*(1.0 + &"; nl ();
      printf "              (s-m(4)**2)/(ii*cc(4)**2/vev**2*(s**2/10.0 + &"; nl ();
      printf "              (s-m(4)**2)*((1.0+6.0*s/m(4)**2+6.0* &"; nl ();
      printf "              s**2/m(4)**4)* s2stu(s,m(4))/3.0 &"; nl ();
      printf "              + s**2/m(4)**2/180.0)) - (s-m(4)**2)))"; nl ();
      printf "        else"; nl ();
      printf "          a02(4) = vev**2/s**2 * cc(4)**2 * ( s**2/ &"; nl (); 
      printf "               cmplx(s-m(4)**2,m(4)*width_res(w_res,4,wkm(4),&"; nl ();
      printf "               m(4),cc(4)),default)/10.0 + &"; nl ();
      printf "               (1.+6.*s/m(4)**2+6.*s**2/m(4)**4)*s2stu(s,m(4))/ &"; nl ();
      printf "               3. + s**2/m(4)**2/180.)"; nl ();
      printf "        end if"; nl ();          
      printf "      else"; nl ();
      printf "         a02(4) = 0"; nl ();
      printf "      end if"; nl ();
      printf "      !!! Tensor isoquintet"; nl ();
      printf "      a02(5) = cc(5)**2/vev**2*(5.0*(1.0+6.0* &"; nl ();
      printf "               s/m(5)**2+6.0*s**2/m(5)**4)*s2stu(s,m(5))/3.0 &"; nl ();  
      printf "               + s**2/m(5)**2/216.0)/6.0"; nl ();
      printf "      a02(5) = vev**4/s**2*a02(5)/&"; nl (); 
      printf "                 (1.0_default - fudge_km*ii*a02(5))"; nl ();
      printf "      !!! Low energy theory alphas"; nl ();
      printf "      a02_0 = 8*(2*a4 + a5)/15"; nl ();
      printf "      a02_1 = log(lam_reg**2/s)/9 - 7./135.0_default"; nl ();
      printf "      a02(6) = a02_0 !!! + a02_1/16/Pi**2"; nl ();
      printf "      a02(6) = fudge_km*jj*a02(6)**2 / (1.0_default - jj*a02(6))"; nl ();
      printf "      amp_02 = sum(a02)"; nl ();
      printf "  end function da02"; nl(); 
      nl ();
      printf "  %sfunction da11 (cc, s, m) result (amp_11)" pure; nl ();
      printf "      real(kind=default), intent(in) :: s"; nl ();
      printf "      real(kind=default), dimension(5), intent(in) :: m, cc"; nl ();
      printf "      real(kind=default) :: a11_0, a11_1"; nl ();
      printf "      complex(kind=default), dimension(1:6) :: a11"; nl ();
      printf "      complex(kind=default) :: ii, jj, amp_11"; nl ();
      printf "      ii = cmplx(0.0,1.0/32.0/Pi,default)"; nl ();
      printf "      jj = s**2/vev**4*ii"; nl ();
      printf "      !!! Scalar isosinglet"; nl ();
      printf "      a11(1) = 2.0*cc(1)**2/vev**2 * s1stu(s,m(1))"; nl ();
      printf "      a11(1) = vev**4/s**2*a11(1)/&"; nl ();
      printf "                 (1.0_default - fudge_km*ii*a11(1))"; nl ();
      printf "      !!! Scalar isoquintet"; nl ();
      printf "      a11(2) = - 5.0*cc(2)**2/vev**2 * s1stu(s,m(2)) / 6.0"; nl ();
      printf "      a11(2) = vev**4/s**2*a11(2)/&"; nl (); 
      printf "                 (1.0_default - fudge_km*ii*a11(2))"; nl ();
      printf "      !!! Vector isotriplet"; nl ();
      printf "      if (cc(3).ne.0) then"; nl ();
      printf "        if (fudge_km.ne.0) then"; nl ();
      printf "          a11(3) = vev**4/s**2 * fudge_km * &"; nl ();
      printf "              cmplx(0.0,32.0*Pi,default)*(1.0 + (s-m(3)**2) &"; nl ();
      printf "              /(ii*cc(3)**2*(2.0*s/3.0 + (s-m(3)**2)&"; nl ();
      printf "              *(s/m(3)**2+2.0*p1stu(s,m(3)))) - (s-m(3)**2)))"; nl ();
      printf "        else"; nl ();
      printf "          a11(3) = vev**4/s**2 * cc(3)**2 * ( 2.*s / &"; nl (); 
      printf "              cmplx(s-m(3)**2,m(3)*width_res(w_res,3,wkm(3),m(3),&"; nl ();
      printf "              cc(3)),default)/3. + s/m(3)**2 + 2.*p1stu(s,m(3)))"; nl ();
      printf "        end if"; nl ();          
      printf "      else"; nl ();
      printf "         a11(3) = 0"; nl ();
      printf "      end if"; nl ();
      printf "      !!! Tensor isosinglet"; nl ();
      printf "      a11(4) = cc(4)**2/vev**2*(d1stu(s,m(4)) &"; nl ();
      printf "               /3.0 - s**2/m(4)**2/36.0)"; nl ();
      printf "      a11(4) = vev**4/s**2*a11(4)/&"; nl (); 
      printf "                 (1.0_default - fudge_km*ii*a11(4))"; nl ();
      printf "      !!! Tensor isoquintet"; nl ();
      printf "      a11(5) = 5.0*cc(5)**2/vev**2*(-d1stu(s,m(5)) &"; nl ();
      printf "               + s**2/m(5)**2/12.0)/36.0"; nl ();
      printf "      a11(5) = vev**4/s**2*a11(5)/&"; nl (); 
      printf "                 (1.0_default - fudge_km*ii*a11(5))"; nl ();
      printf "      !!! Low energy theory alphas"; nl ();
      printf "      a11_0 = fudge_higgs*vev**2/3/s + 4*(a4 - 2*a5)/3"; nl ();
      printf "      a11_1 = - 1.0/54.0_default"; nl ();
      printf "      a11(6) = a11_0 !!! + a11_1/16/Pi**2"; nl ();
      printf "      a11(6) = fudge_km*jj*a11(6)**2 / (1.0_default - jj*a11(6))"; nl ();
      printf "      amp_11 = sum(a11)"; nl ();
      printf "  end function da11"; nl(); 
      nl ();
      printf "  %sfunction da20 (cc, s, m) result (amp_20)" pure; nl ();
      printf "      real(kind=default), intent(in) :: s"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: m, cc"; nl ();
      printf "      real(kind=default) :: a20_0, a20_1"; nl ();
      printf "      complex(kind=default), dimension(1:6) :: a20"; nl ();
      printf "      complex(kind=default) :: ii, jj, amp_20"; nl ();
      printf "      ii = cmplx(0.0,1.0/32.0/Pi,default)"; nl ();
      printf "      jj = s**2/vev**4*ii"; nl ();
      printf "      !!! Scalar isosinglet"; nl ();
      printf "      a20(1) = 2.0*cc(1)**2/vev**2 * s0stu(s,m(1))"; nl ();
      printf "      a20(1) = vev**4/s**2*a20(1)/&"; nl ();
      printf "                 (1.0_default - fudge_km*ii*a20(1))"; nl ();
      printf "      !!! Scalar isoquintet"; nl ();
      printf "      if (cc(2).ne.0) then"; nl ();
      printf "        if (fudge_km.ne.0) then"; nl ();
      printf "          a20(2) = vev**4/s**2 * fudge_km * &"; nl (); 
      printf "              cmplx(0.0,32.0*Pi,default)*(1.0 + &"; nl ();
      printf "              (s-m(2)**2)/(ii*cc(2)**2/vev**2*(s**2/2.0 + &"; nl ();
      printf "              (s-m(2)**2)*s0stu(s,m(2))/6.0) - (s-m(2)**2)))"; nl ();
      printf "        else"; nl ();
      printf "          a20(2) = vev**2/s**2 * cc(2)**2 * ( s**2 / &"; nl (); 
      printf "              cmplx(s-m(2)**2,m(2)*width_res(w_res,2,wkm(2),&"; nl ();
      printf "              m(2),cc(2)),default)/2. + s0stu(s,m(2))/6.)"; nl ();
      printf "        end if"; nl ();          
      printf "      else"; nl ();
      printf "         a20(2) = 0"; nl ();
      printf "      end if"; nl ();
      printf "      !!! Vector isotriplet"; nl ();
      printf "      a20(3) = - cc(3)**2*(2.0*p0stu(s,m(3)) + 3.0*s/m(3)**2)"; nl ();
      printf "      a20(3) = vev**4/s**2*a20(3)/&"; nl (); 
      printf "                 (1.0_default - fudge_km*ii*a20(3))"; nl ();
      printf "      !!! Tensor isosinglet"; nl ();
      printf "      a20(4) = cc(4)**2/vev**2*(d1stu(s,m(4)) &"; nl ();
      printf "               /3.0 + s**2/m(4)**2/18.0)"; nl ();
      printf "      a20(4) = vev**4/s**2*a20(4)/&"; nl (); 
      printf "                 (1.0_default - fudge_km*ii*a20(4))"; nl ();
      printf "      !!! Tensor isoquintet"; nl ();
      printf "      a20(5) = cc(5)**2/vev**2*(d0stu(s,m(5)) &"; nl ();
      printf "                + 5.0*s**2/m(4)**2/3.0)/36.0"; nl ();
      printf "      a20(5) = vev**4/s**2*a20(5)/&"; nl (); 
      printf "                 (1.0_default - fudge_km*ii*a20(5))"; nl ();
      printf "      !!! Low energy theory alphas"; nl ();
      printf "      a20_0 = -fudge_higgs*vev**2/s + 16*(2*a4 + a5)/3"; nl ();
      printf "      a20_1 = 10*log(lam_reg**2/s)/9 + 25/108.0_default"; nl ();
      printf "      a20(6) = a20_0 !!! + a20_1/16/Pi**2"; nl ();
      printf "      a20(6) = fudge_km*jj*a20(6)**2 / (1.0_default - jj*a20(6))"; nl ();
      printf "      amp_20 = sum(a20)"; nl ();
      printf "  end function da20"; nl(); 
      nl ();
      printf "  %sfunction da22 (cc, s, m) result (amp_22)" pure; nl ();
      printf "      real(kind=default), intent(in) :: s"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: m, cc"; nl ();
      printf "      real(kind=default) :: a22_0, a22_1"; nl ();
      printf "      complex(kind=default), dimension(1:6) :: a22"; nl ();
      printf "      complex(kind=default) :: ii, jj, amp_22"; nl ();
      printf "      ii = cmplx(0.0,1.0/32.0/Pi,default)"; nl ();
      printf "      jj = s**2/vev**4*ii"; nl ();
      printf "      !!! Scalar isosinglet"; nl ();
      printf "      a22(1) = 2.0*cc(1)**2/vev**2 * s2stu(s,m(1))"; nl ();
      printf "      a22(1) = vev**4/s**2*a22(1)/&"; nl (); 
      printf "                 (1.0_default - fudge_km*ii*a22(1))"; nl ();
      printf "      !!! Scalar isoquintet"; nl ();
      printf "      a22(2) = cc(2)**2/vev**2 * s2stu(s,m(2)) / 6.0"; nl ();
      printf "      a22(2) = vev**4/s**2*a22(2)/&"; nl (); 
      printf "                 (1.0_default - fudge_km*ii*a22(2))"; nl ();
      printf "      !!! Vector triplet"; nl ();
      printf "      a22(3) = - 2.0*cc(3)**2*(2*s+m(3)**2)*s2stu(s,m(3))/m(3)**4"; nl ();
      printf "      a22(3) = vev**4/s**2*a22(3)/&"; nl ();
      printf "                 (1.0_default - fudge_km*ii*a22(3))"; nl ();
      printf "      !!! Tensor isosinglet"; nl ();
      printf "      a22(4) = cc(4)**2/vev**2*((1.0 + 6.0*s/m(4)**2+6.0* &"; nl ();
      printf "            s**2/m(4)**4)*s2stu(s,m(4))/3.0 + s**2/m(4)**2/180.0)"; nl ();
      printf "      a22(4) = vev**4/s**2*a22(4)/&"; nl ();
      printf "                 (1.0_default - fudge_km*ii*a22(4))"; nl ();
      printf "      !!! Tensor isoquintet"; nl ();
      printf "      if (cc(5).ne.0) then"; nl ();
      printf "        if (fudge_km.ne.0) then"; nl ();
      printf "          a22(5) = vev**4 / s**2 * & "; nl ();
      printf "              cmplx(0.0,32.0*Pi,default)*(1.0 + &"; nl ();
      printf "              (s-m(5)**2)/(ii*cc(5)**2/vev**2*(s**2/60.0 + &"; nl ();
      printf "              (s-m(5)**2)*((1.0+6.0*s/m(5)**2+6.0* &"; nl ();
      printf "              s**2/m(5)**4)*s2stu(s,m(5))/36.0 &"; nl (); 
      printf "              + s**2/m(5)**2/2160.0)) - (s-m(5)**2)))"; nl ();
      printf "        else"; nl ();
      printf "          a22(5) = vev**2/s**2 * cc(5)**2 * ( s**2 / &"; nl (); 
      printf "              cmplx(s-m(5)**2,m(5)*width_res(w_res,5,wkm(5),&"; nl ();
      printf "              m(5),cc(5)),default)/80. + (1.0+6.0* &"; nl ();
      printf "              s/m(5)**2+6.0*s**2/m(5)**4)*s2stu(s,m(5))/36.0 + &"; nl ();
      printf "              s**2/m(5)**2/2160.0)"; nl ();
      printf "        end if"; nl ();                    
      printf "      else"; nl ();
      printf "         a22(5) = 0"; nl ();
      printf "      end if"; nl ();
      printf "      !!! Low energy theory alphas"; nl ();
      printf "      a22_0 = 4*(a4 + 2*a5)/15"; nl ();
      printf "      a22_1 = 2*log(lam_reg**2/s)/45 - 247/5400.0_default"; nl ();
      printf "      a22(6) = a22_0 !!! + a22_1/16/Pi**2"; nl ();
      printf "      a22(6) = fudge_km*jj*a22(6)**2 / (1.0_default - jj*a22(6))"; nl ();
      printf "      amp_22 = sum(a22)"; nl ();
      printf "  end function da22"; nl();
      nl ();
      printf "  %sfunction dalzz0_s (cc,m,k) result (alzz0_s)" pure; nl ();
      printf "      type(momentum), intent(in) :: k"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: cc, m"; nl ();
      printf "      complex(kind=default) :: alzz0_s"; nl ();
      printf "      real(kind=default) :: s"; nl ();
      printf "      s = k*k"; nl ();
      printf "      alzz0_s = 2*g**4/costhw**2*((da00(cc,s,m) &"; nl ();
      printf "                - da20(cc,s,m))/24 &"; nl ();
      printf "                - 5*(da02(cc,s,m) - da22(cc,s,m))/12)"; nl ();
      printf "  end function dalzz0_s"; nl ();
      nl ();
      printf "  %sfunction dalzz0_t (cc,m,k) result (alzz0_t)" pure; nl ();
      printf "      type(momentum), intent(in) :: k"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: cc, m"; nl ();
      printf "      complex(kind=default) :: alzz0_t"; nl ();
      printf "      real(kind=default) :: s"; nl ();
      printf "      s = k*k"; nl ();
      printf "      alzz0_t = 5*g**4/costhw**2*(da02(cc,s,m) - &"; nl ();
      printf "                da22(cc,s,m))/4"; nl (); 
      printf "  end function dalzz0_t"; nl ();
      nl ();
      printf "  %sfunction dalzz1_s (cc,m,k) result (alzz1_s)" pure; nl ();
      printf "      type(momentum), intent(in) :: k"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: cc, m"; nl ();
      printf "      complex(kind=default) :: alzz1_s"; nl ();
      printf "      real(kind=default) :: s"; nl ();
      printf "      s = k*k"; nl ();
      printf "      alzz1_s = g**4/costhw**2*(da20(cc,s,m)/8 &"; nl ();
      printf "                - 5*da22(cc,s,m)/4)"; nl (); 
      printf "  end function dalzz1_s"; nl ();
      nl ();  
      printf "  %sfunction dalzz1_t (cc,m,k) result (alzz1_t)" pure; nl ();
      printf "      type(momentum), intent(in) :: k"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: cc, m"; nl ();
      printf "      complex(kind=default) :: alzz1_t"; nl ();
      printf "      real(kind=default) :: s"; nl ();
      printf "      s = k*k"; nl ();
      printf "      alzz1_t = g**4/costhw**2*(- 3*da11(cc,s,m)/8 &"; nl ();
      printf "                + 15*da22(cc,s,m)/8)"; nl (); 
      printf "  end function dalzz1_t"; nl ();
      nl ();  
      printf "  %sfunction dalzz1_u (cc,m,k) result (alzz1_u)" pure; nl ();
      printf "      type(momentum), intent(in) :: k"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: cc, m"; nl ();
      printf "      complex(kind=default) :: alzz1_u"; nl ();
      printf "      real(kind=default) :: s"; nl ();
      printf "      s = k*k"; nl ();
      printf "      alzz1_u = g**4/costhw**2*(3*da11(cc,s,m)/8 &"; nl ();
      printf "                + 15*da22(cc,s,m)/8)"; nl (); 
      printf "  end function dalzz1_u"; nl ();
      nl ();  
      printf "  %sfunction dalww0_s (cc,m,k) result (alww0_s)" pure; nl ();
      printf "      type(momentum), intent(in) :: k"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: cc, m"; nl ();
      printf "      complex(kind=default) :: alww0_s"; nl ();
      printf "      real(kind=default) :: s"; nl ();
      printf "      s = k*k"; nl ();
      printf "      alww0_s = g**4*((2*da00(cc,s,m) + da20(cc,s,m))/24 &"; nl ();
      printf "                  - 5*(2*da02(cc,s,m) + da22(cc,s,m))/12)"; nl (); 
      printf "  end function dalww0_s"; nl ();
      nl ();  
      printf "  %sfunction dalww0_t (cc,m,k) result (alww0_t)" pure; nl ();
      printf "      type(momentum), intent(in) :: k"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: cc, m"; nl ();
      printf "      complex(kind=default) :: alww0_t"; nl ();
      printf "      real(kind=default) :: s"; nl ();
      printf "      s = k*k"; nl ();
      printf "      alww0_t = g**4*(10*da02(cc,s,m) - 3*da11(cc,s,m) &"; nl ();
      printf "                + 5*da22(cc,s,m))/8"; nl (); 
      printf "  end function dalww0_t"; nl ();
      nl ();  
      printf "  %sfunction dalww0_u (cc,m,k) result (alww0_u)" pure; nl ();
      printf "      type(momentum), intent(in) :: k"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: cc, m"; nl ();
      printf "      complex(kind=default) :: alww0_u"; nl ();
      printf "      real(kind=default) :: s"; nl ();
      printf "      s = k*k"; nl ();
      printf "      alww0_u = g**4*(10*da02(cc,s,m) + 3*da11(cc,s,m) &"; nl (); 
      printf "                + 5*da22(cc,s,m))/8"; nl (); 
      printf "  end function dalww0_u"; nl ();
      nl ();
      printf "  %sfunction dalww2_s (cc,m,k) result (alww2_s)" pure; nl ();
      printf "      type(momentum), intent(in) :: k"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: cc, m"; nl ();
      printf "      complex(kind=default) :: alww2_s"; nl ();
      printf "      real(kind=default) :: s"; nl ();
      printf "      s = k*k"; nl ();
      printf "      alww2_s = g**4*(da20(cc,s,m) - 10*da22(cc,s,m))/4 "; nl (); 
      printf "  end function dalww2_s"; nl ();
      nl ();
      printf "  %sfunction dalww2_t (cc,m,k) result (alww2_t)" pure; nl ();
      printf "      type(momentum), intent(in) :: k"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: cc, m"; nl ();
      printf "      complex(kind=default) :: alww2_t"; nl ();
      printf "      real(kind=default) :: s"; nl ();
      printf "      s = k*k"; nl ();
      printf "      alww2_t = 15*g**4*da22(cc,s,m)/4"; nl (); 
      printf "  end function dalww2_t"; nl ();
      nl ();
      printf "  %sfunction dalz4_s (cc,m,k) result (alz4_s)" pure; nl ();
      printf "      type(momentum), intent(in) :: k"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: cc, m"; nl ();
      printf "      complex(kind=default) :: alz4_s"; nl ();
      printf "      real(kind=default) :: s"; nl ();
      printf "      s = k*k"; nl ();
      printf "      alz4_s = g**4/costhw**4*((da00(cc,s,m) &"; nl ();
      printf "               + 2*da20(cc,s,m))/12 &"; nl (); 
      printf "               - 5*(da02(cc,s,m)+2*da22(cc,s,m))/6)"; nl (); 
      printf "  end function dalz4_s"; nl ();
      nl ();
      printf "  @[<5>"; 
      printf "  %sfunction dalz4_t (cc,m,k) result (alz4_t)" pure; nl ();
      printf "      type(momentum), intent(in) :: k"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: cc, m"; nl ();
      printf "      complex(kind=default) :: alz4_t"; nl ();
      printf "      real(kind=default) :: s"; nl ();
      printf "      s = k*k"; nl ();
      printf "      alz4_t = g**4/costhw**4*5*(da02(cc,s,m) &"; nl ();
      printf "               + 2*da22(cc,s,m))/4"; nl (); 
      printf "  end function dalz4_t"; nl ();
      nl ();
  end

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
