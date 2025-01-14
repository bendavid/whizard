% omega_logo.mp --
%
%  Copyright (C) 1999-2025 by 
%
%      Wolfgang Kilian <kilian@physik.uni-siegen.de>
%      Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
%      Juergen Reuter <juergen.reuter@desy.de>
%      Christian Speckner <cnspeckn@googlemail.com>
%
%  WHIZARD is free software; you can redistribute it and/or modify it
%  under the terms of the GNU General Public License as published by 
%  the Free Software Foundation; either version 2, or (at your option)
%  any later version.
%
%  WHIZARD is distributed in the hope that it will be useful, but
%  WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program; if not, write to the Free Software
%  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  These are the capital Omegas in the AMS Euler fonts,
%  adapted to Metapost.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% These all in bitpad numbers (.001"):
     programem =      3700;     
     baseline  =     0;
     capheight =     2560;

ptsize = 700;
leftside = rightside = 0h;

if unknown xscale_factor: xscale_factor := 1; fi
h = ptsize * xscale_factor / programem;
v = ptsize / programem;

save_leftside:=leftside; save_rightside:=rightside;
def more_side(expr s_sharp) =
 leftside:=save_leftside+s_sharp; rightside:=save_rightside+s_sharp;
enddef;

% ----- Fontbegin, Charbegin -----------------------------------
% --------------------------------------------------------------

transform rot;

def charbegin(expr c,w_sharp,h_sharp,d_sharp) =
  begingroup
    beginfig(c);
      W := w_sharp*pt;
      chardx:=round(W+leftside+rightside);
      charwd:=w_sharp+leftside+rightside;    charht:=h_sharp;  chardp:=d_sharp;
      charic:=0; clearxy; clearit; clearpen;
      rot := identity;
      pair tiept[];
 enddef;

def endchar =
      % setbounds currentpicture to
      %   (0,-chardp)--(charwd,-chardp)--(charwd,charht)--(0,charht)--cycle;
    endfig;
  endgroup
enddef;

def mathcorr(expr subwidth_sharp) = % DEK
 charic:=subwidth_sharp; charwd:=charwd-charic;
enddef;

% Adjusting stems
% revised by DEK to allow highres adjustments, 11 Aug 87

vardef set_stem_round(expr slo,s,shi,clo,c,chi) =
 stem_lo:=slo*h; stem_hi:=shi*h; stem_norm:=s*h;
 curve_lo:=clo*h; curve_hi:=chi*h; curve_norm:=c*h;
  save a,b;
  a-b = round (stem_norm - curve_norm);
  a = round(.5(stem_norm + curve_norm + a - b));
  stem_norm_corr := a-stem_norm; % a is normal stem width in pixels
  curve_norm_corr := b-curve_norm; % b is normal curve width in pixels
enddef;

def no_stem_round = set_stem_round(-1,-1,-1,-1,-1,-1) enddef;
no_stem_round; % default is to do ordinary rounding

% The |stem_round| macro rounds its argument, forcing numbers that look like
% stem widths to round near to |stem_norm|, and similarly forcing vertical curve
% weights to round near to |curve_norm|.

def stem_round primary w = if w<0: -stem_rnd(-w) else: stem_rnd(w) fi enddef;

def stem_rnd(expr w) =
    round(w
    if (stem_lo<=w) and (w<=stem_hi): +stem_norm_corr
    elseif (curve_lo<=w) and (w<=curve_hi): +curve_norm_corr
    fi)
enddef;

% Filling cyclic paths with step width adjustment and rounding

% Before calling the |adj_fill| macro, the user should set up an
% array |t[]| and a nonnegative integer |n| so that |t[1]| through |t[n]|
% are time values on some cyclic path |p|.  It should be true that |t[i]<t[j]|
% whenever |i<j|.  Also |t[n]-t[1]| should be less than the length of |p|.
% The |adj_fill| macro takes four lists of time values given as indices into
% the |t| array.  The avoids the necessity of writing \MF\ macros to sort
% the time values.
% Groups of paths are allowed to have points ``tied together.''  This is
% implemented by saving coordinates in a special array of type |pair|
% called |tiept|.  If a path contains a point that is tied to a point in
% an already computed path, then the adjusted coordinates of that point will
% be saved in the |tiept| array.  This array should be made unknown before
% starting a new group of paths; e.g., in |beginchar|.


% Make |y'a| and |y'b| rounded versions of |y.a| and |y.b|, so that
% |y'a-y'b| is as close as possible to |y.a-y.b|.
% If a time value is given as both fixed and vertical or horizontal then
% |y'a| or |y'b| or both may already be known.  Then we just round what
% we can.

vardef rnd_pr_y(suffix a, b) =
  if known y'a: if unknown y'b: y'b-y'a=round(y.b-y.a); fi
  elseif known y'b: y'b-y'a=round(y.b-y.a);
  else:
    y'a-y'b = round(y.a-y.b);
    y'a = round(.5(y.a + y.b + y'a - y'b));
  fi
enddef;

% Rounding |x| coordinates is similar except we use the special |stem_round|
% routine.

vardef rnd_pr_x(suffix a, b) =
% use the next line if you want to see what channel settings are reasonable
% (also set tracingtitles:=1 in such a case)
% message decimal t.a&","&decimal t.b&":"&decimal((x.b-x.a)/h);
  if known x'a: if unknown x'b: x'b-x'a=stem_round(x.b-x.a); fi
  elseif known x'b: x'b-x'a=stem_round(x.b-x.a);
  else:
    x'a-x'b = stem_round(x.a-x.b);
    x'a = round(.5(x.a + x.b + x'a - x'b));
  fi
enddef;



% Set up a transform |curtx=tx.a| that takes |x.a| into |x'a| and |x.b|
% into |x'b| without slanting or changing $y$-components.

vardef set_tx(suffix a,b) =
  save u,v;
  xypart tx.a = yxpart tx.a = 0;
  (x.a,0) transformed tx.a = (x'a,0);
  (u,v) = (x.b,1) transformed tx.a - (x'b,1);
  if known u: xxpart tx.a = yypart tx.a = 1;
        else: (u,v)=origin;
  fi
  curtx := tx.a
enddef;


% Set up a transform |curty=ty.a| that takes |y.a| into |y'a| and |y.b|
% into |y'b| without slanting or changing $x$-components.

vardef set_ty(suffix a,b) =
  save u,v;
  xypart ty.a = yxpart ty.a = 0;
  (0,y.a) transformed ty.a = (0,y'a);
  (u,v) = (1,y.b) transformed ty.a - (1,y'b);
  if known v: xxpart ty.a = yypart ty.a = 1;
        else: (u,v)=origin;
  fi
  curty := ty.a
enddef;


% The following macros ensure that |x'i| or |y'i| agree with the current
% transform.  It is important that this be done for all relevant |i| each
% time |set_tx| or |set_ty| is called.  Since some points may be tied to
% others, this can affect which |x'j| and |y'j| are known.  Future calls to
% |set_tx| and |set_ty| should be based on the most up to date possible
% information.

vardef yset@# = (0,y'@#) = (0,y@#) transformed curty; enddef;
vardef xset@# = (x'@#,0) = (x@#,0) transformed curtx; enddef;


% Apply |set_txy| to each pair indices |a,b| such that |xy'[a]| and |xy'[b]|
% are known, but |xy'[c] is unknown for all |c| between |a| and |b|.
% This leaves the appropriate initial transformation in |curtx| or |curty|.
% The |xyset| parameter is either |xset| or |yset| as explained above.

vardef set_trans(suffix xy, set_txy, xyset) =
  save previ, firsti;
  for i=1 upto n: if known xy'[i]:
      if known firsti:
     set_txy([previ], [i]);
     for j=previ+1 upto i-1: xyset[j]; endfor
      else: firsti = i;
      fi
      previ := i;
  fi endfor     
  if known firsti:
    for i=1 upto firsti: if known xy'[i]:
      set_txy([previ], [i]);
      if previ>=firsti:
     for j=previ+1 upto n: xyset[j]; endfor
     for j=1 upto i-1: xyset[j]; endfor
      else:
     for j=previ+1 upto i-1: xyset[j]; endfor
      fi
      previ:=i;
    fi endfor
  else:
    for i=1 upto n: xyset[i]; endfor
  fi
enddef;



% Return the transformed $i$th segement of |p_path| as defined by the time
% values in |t[]|, updating |curtx| and |curty| if appropriate.

vardef new_seg(expr i) =
  save p; path p;
  if known tx[i]: curtx:=tx[i]; fi
  if known ty[i]: curty:=ty[i]; fi
  p = subpath (t[i],t[i+1]) of p_path transformed (curtx transformed curty);
  p
enddef;



% The following macros are used only when |t| entries are readjusted:


% Find the first time on the path |p| where the direction is |dir| or |-dir|.

def extremetime expr dir of p =
  begingroup save a,b;
  a = directiontime dir of p; if a<0: a:=infinity; fi
  b = directiontime -dir of p; if b<0: b:=infinity; fi
  if a<b: a else: b fi
  endgroup
enddef;


% Adjust the time value |tt| to the nearest time when the direction of |p_path|
% is |dir| or |-dir|.

vardef adj_t(suffix tt)(expr dir) =
  save p, a, b; path p;
  p = subpath (tt,tt+nn) of p_path & cycle;
  a = extremetime dir of p;
  a := if a<1: a[tt,floor tt+1] else: a+floor tt fi;
  b = extremetime dir of reverse p;
  b := if b<1: b[tt,ceiling tt-1] else: ceiling tt - b fi;
  tt := if b+a>2tt: b else: a fi;
enddef;


% Issue an error message when |t[i]>t[i+1]| after the above adjustment process.

vardef bad_order(expr i) =
  initerim showstopping:=0;
  show t[i], t[i+1];
  errmessage "Adjusted t entries "&decimal i&" and "&decimal(i+1)
          &" are out of order. (See above)";
enddef;


% The |adj_fill| macro performs the entire adjustment and filling based on
% the following parameters: a list |tfx| of |t| indices for points whose
% $x$-coordinates should not be moved during the adjustment process, a similar
% list |tfy| for $y$-coordinates, a list of pairs $(i,j)$ where $i$ is a |t|
% index and |tiept[j]| is the corresponding tie point, lists |tv| and |th| of
% pairs of |t| indices that correspond to opposite sides of vertical and
% horizontal strokes, and finally a cyclic path |p|.  (Note the scaling by |h|
% and |v|.)

vardef adj_fill@#(text tfx, tfy, tie, tv, th)(expr p) =
% message str@#; % that's for use with the stem-round message above
  save p_path, nn, x, y, tx, ty, curtx, curty;
  path p_path, p_path';
  transform tx[], ty[], curtx, curty;
  p_path = p transformed (identity xscaled h yscaled v transformed rot);
  nn = length p_path;
    forsuffixes i=tfx: x.fix.i=1; endfor          % Prepare for |adj_t| calls.
    forsuffixes i=tfy: y.fix.i=1; endfor
    for w=1 tv: if pair w: (x.fix[xpart w],x.fix[ypart w]) = (1,1); fi endfor
    for w=1 th: if pair w: (y.fix[xpart w],y.fix[ypart w]) = (1,1); fi endfor
    for i=1 upto n:
      if t[i]>floor t[i]:
     if unknown x.fix[i]: adj_t(t[i],right); fi
     if unknown y.fix[i]: adj_t(t[i],up); fi
      fi
    endfor
    t[n+1] := t1+nn;
    for i=1 upto n: if t[i]>t[i+1]: bad_order(i); fi endfor
  for i=1 upto n: z[i] = point t[i] of p_path; endfor
  forsuffixes i=tfx: x'i =x.i; endfor
  forsuffixes i=tfy: y'i =y.i; endfor
  for w=1 tie: if pair w: z'[xpart w] = tiept[ypart w]; fi endfor
  for w=1 tv: if pair w: rnd_pr_x([xpart w], [ypart w]); fi endfor
  for w=1 th: if pair w: rnd_pr_y([xpart w], [ypart w]); fi endfor
  curtx=curty=identity;
  set_trans(x, set_tx, xset);
  set_trans(y, set_ty, yset);
  p_path' = if n=0: p_path else:
              for i=1 upto n: new_seg(i)-- endfor cycle
         fi;
  begingroup save currenttransform;
  transform currenttransform; currenttransform:=identity;
  if known fillwhite:
     draw p_path' withpen pencircle scaled 4;     % was scaled 2
  else:
    begingroup save pic;               % Now fill
    picture pic;
    pic=currentpicture;
    currentpicture:=nullpicture;
    fill p_path';
    % cull currentpicture dropping origin;
    addto currentpicture also pic;
    endgroup;
  fi
  endgroup;
enddef;

% UPPERCASE GREEK CHARACTERS
set_stem_round(270,290,320,321,335,367); % DEK
more_side(100h);
%upper case Omega     

charbegin( 1, 3042h, capheight*v, baseline );
n := 13;
t1 := 2;
t2 := 5;
t3 := 8;
t4 := 10;
t5 := 13;
t6 := 14;
t7 := 16.36;
t8 := 18;
t9 := 20;
t10 := 23;
t11 := 25;
t12 := 29;
t13 := 29.48;

adj_fill.A(1, 5, 8, 12)                    % fixed x points
     ()                         % fixed y points
     ()                         % tied points
     ((4,9), (2,11))                    % verticals
     ((6,7), (3,10), (1,13))               % horizontals
     ((3021,188){-30,35}...{-30,35}          % 0
      (2991,223){-1,0}...{-738,-41}          % 1
      (2018,184){1,33}...{1,33}          % 2
      (2019,217){564,169}...               % 3
      (2412,479){1,1}...               % 4
      (2746,1425){0,1}...               % 5
      (2446,2275){-1,1}...               % 6
      (2098,2478){-833,274}...          % *7
      (1613,2549){-1,0}...               % 8
      (617,2203){-1,-1}...               % 9
      (263,1251){0,-1}...               % 10
      (330,776){256,-808}...               % *11
      (519,443){1,-1}...{627,-285}          % 12
      (861,216){-12,-23}...{-12,-23}          % 13
      (849,193){-248,29}...{-555,24}          % 14
      (54,242)--                    % 15
      (-6,14){701,52}...{356,-33}          % 16
      (1208,-9){80,205}...{80,205}          % 17
      (1288,196){-787,148}...          % 18
      (880,393){-1,1}...               % 19
      (590,1217){0,1}...               % 20
      (674,1784){322,975}...               % *21
      (912,2192){1,1}...               % 22
      (1543,2420){1,0}...               % 23
      (2110,2208){1,-1}...               % 24
      (2411,1362){0,-1}...               % 25
      (2341,830){-268,-893}...          % *26
      (2143,469){-1,-1}...{-469,-109}     % 27
      (1685,211){-34,-216}...{-34,-216}     % 28
      (1651,-5){676,50}...{317,-26}          % 29
      (2929,-9)--cycle);               % 30

endchar;

charbegin( 2, 3026h, capheight*v, baseline );
n := 12;
t1 := 1;
t2 := 4;
t3 := 7;
t4 := 10;
t5 := 12;
t6 := 16;
t7 := 18;
t8 := 20;
t9 := 22;
t10 := 24;
t11 := 28;
t12 := 29;

adj_fill.A(1, 5, 7, 11)                    % fixed x points
     ()                         % fixed y points
     ()                         % tied points
     ((4,8), (2,10))                    % verticals
     ((5,6), (3,9), (1,12))               % horizontals
     ((3022,390){-1,0}...{-748,-44}          % 0
      (1980,344){5,35}...{5,35}          % 1
      (1985,379){398,51}...               % 2
      (2497,669){1,1}...               % 3
      (2781,1461){0,1}...               % 4
      (2503,2284){-1,1}...               % 5
      (2111,2510){-944,305}...          % *6
      (1559,2589){-1,0}...               % 7
      (972,2506){-1000,-319}...          % *8
      (559,2270){-1,-1}...               % 9
      (227,1425){0,-1}...{609,-202}          % 10
      (971,375){5,-21}...{5,-21}          % 11
      (976,354){-1,0}...{-605,55}          % 12
      (82,405)--                    % 13
      (-12,53){21,-42}...{21,-42}          % 14
      (9,11){680,37}...               % 15
      (604,30){1,0}...{315,-24}          % 16
      (1213,11){109,342}...{109,342}          % 17
      (1322,353){-12,4}...               % 18
      (880,728){-611,1027}...          % *19
      (711,1380){0,1}...               % 20
      (1007,2264){1,1}...               % 21
      (1525,2439){1,0}...               % 22
      (2007,2269){1,-1}...               % 23
      (2309,1401){0,-1}...               % 24
      (2247,906){-240,-825}...          % *25
      (2069,576){-1,-1}...{-501,-131}     % 26
      (1682,349){-88,-352}...{-88,-352}     % 27
      (1594,-3){648,52}...               % 28
      (2286,20){1,0}...{274,-19}          % 29
      (2900,-17)--cycle);               % 30

  endchar;

% UPPER CASE DUBBAYA
more_side(200h);
charbegin( 3, 3658h, capheight*v, baseline );

n := 13;
t1 := 0;
t2 := 3;
t3 := 4;
t4 := 6;
t5 := 9;
t6 := 11;
t7 := 12;
t8 := 13;
t9 := 18;
t10 := 21;
t11 := 24;
t12 := 26;
t13 := 27;

adj_fill.A(3, 12)                    % fixed x points
     (1, 13)                         % fixed y points
     ((1,1), (13,1))                    % tied points
     ((6,7))                         % verticals
     ((7,8), (5,9), (4,10), (2,11))          % horizontals
     ((3822,2548){-298,-50}...{-298,-50}     % 0
      (3524,2498){-247,-428}...          % 1
      (2574,539){-269,-585}...{-30,-28}     % 2
      (2544,511){-41,49}...               % 3
      (2503,560){-110,499}...{-117,780}     % 4
      (2144,2529){-26,24}...{-26,24}          % 5
      (2118,2553){-283,-108}...{-283,-108}     % 6
      (1835,2445){-185,-535}...          % 7
      (1025,597){-144,-306}...{-30,-15}     % 8
      (995,582){-23,22}...               % 9
      (972,604){-52,237}...{-146,781}     % 10
      (555,2549){-606,-11}...{-606,-11}     % 11
      (-51,2538)--                    % 12
      (-44,2423){414,-111}...          % 13
      (122,2375){264,-74}...               % 14
      (222,2273){125,-318}...          % 15
      (480,1389){548,-2296}...{76,-418}     % *16
      (770,-23){39,-17}...{39,-17}          % 17
      (809,-40){237,106}...{237,106}          % 18
      (1046,66){247,751}...               % 19
      (1825,1985){159,324}...                 % 20
      (1859,2029)...                    % 21
      (1884,1984){121,-550}...{79,-458}     % 22
      (2285,-12){46,-27}...{46,-27}          % 23
      (2331,-39){223,98}...{223,98}          % 24
      (2554,59){277,679}...{322,523}          % 25
      (3822,2471){0,77}...{0,77}          % 26
      (3822,2548)--cycle);               % 27

  endchar;

end.
