/* circe2_c.c -- beam spectra for linear colliders and photon colliders */
/* $Id: circe2.nw,v 1.56 2002/10/14 10:12:06 ohl Exp $
   Copyright (C) 2002-2011 by 
        Wolfgang Kilian <kilian@physik.uni-siegen.de>
        Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
        Juergen Reuter <juergen.reuter@desy.de>
        Christian Speckner <cnspeckn@googlemail.com>
  
   Circe2 is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.
  
   Circe2 is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
  
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */

/**********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#undef min
#define min(a,b) ((a) < (b) ? (a) : (b))
#undef max
#define max(a,b) ((a) > (b) ? (a) : (b))

/**********************************************************************/

#define POINTER_PANIC(fct,ptr,name) \
  if (ptr == NULL) { \
    fprintf (stderr, "%s: PANIC: %s not initialized!\n", #fct, name); \
    exit (-1); \
  }

/**********************************************************************/

#define POLAVG 1
#define POLHEL 2
#define POLGEN 3
#define CIRCE2_EOK     0
#define CIRCE2_EFILE  -1
#define CIRCE2_EMATCH -2
#define CIRCE2_EFORMT -3
#define CIRCE2_ESIZE  -4

/**********************************************************************/

typedef struct
{
  int n;
  double *x;
  int pid;
  int pol;
  int *map;
  double *y;
  double *alpha;
  double *xi, *eta;
  double *a, *b;
} circe2_division;
typedef struct
{
  circe2_division *d1, *d2;
  double *weight;
  double **value;
  int triangle;
  double lumi;
  double channel_weight;
} circe2_channel;
typedef struct
{
  int n;
  circe2_channel **ch;
  int polarization_support;
} circe2_channels;

/**********************************************************************/

void *
xmalloc (size_t size)
{
  void *ptr = malloc (size);
  if (ptr == NULL) {
    fprintf (stderr, "can't get %d bytes ... exiting!\n", size);
    exit (-1);
  }
  return ptr;
}
static circe2_division *
circe2_new_division (int nbins)
{
  circe2_division *d;
  d = xmalloc (sizeof (circe2_division));
  d->n = nbins;
  d->x = xmalloc ((d->n + 1) * sizeof(double));
  d->map = xmalloc (d->n * sizeof(int));
  d->y = xmalloc ((d->n + 1) * sizeof(double));
  d->alpha = xmalloc (d->n * sizeof(double));
  d->xi = xmalloc (d->n * sizeof(double));
  d->eta = xmalloc (d->n * sizeof(double));
  d->a = xmalloc (d->n * sizeof(double));
  d->b = xmalloc (d->n * sizeof(double));
  return d;
}
static circe2_channel *
circe2_new_channel (int nbins1, int nbins2)
{
  circe2_channel *p;
  int i;
  p = xmalloc (sizeof (circe2_channel));
  p->d1 = circe2_new_division (nbins1);
  p->d2 = circe2_new_division (nbins2);
  p->weight = xmalloc ((p->d1->n * p->d2->n + 1) * sizeof(double));
  p->value = xmalloc (p->d1->n * sizeof(double *));
  for (i = 0; i < p->d1->n; i++)
    p->value[i] = xmalloc (p->d2->n * sizeof(double));
  return p;
}
static circe2_channels *
circe2_new_channels (int nchannels, int nbins1, int nbins2)
{
  int i;
  circe2_channels *p;
  p = xmalloc (sizeof (circe2_channels));
  p->n = nchannels;
  p->ch = xmalloc (p->n * sizeof (circe2_channel *));
  for (i = 0; i < p->n; i++)
    p->ch[i] = circe2_new_channel (nbins1, nbins2);
  return p;
}
static inline void
split_index (circe2_channel *ch, int *i1, int *i2, int i) {
  *i2 = 1 + (i - 1) / ch->d1->n;
  *i1 = i - (*i2 - 1) * ch->d1->n;
}
static circe2_channel *
circe2_find_channel (circe2_channels *channels,
                     int p1, int h1, int p2, int h2)
{
  int i;
  if (((channels->polarization_support == POLAVG)
       || (channels->polarization_support == POLGEN))
      && ((h1 != 0) || (h2 != 0))) {
    fprintf (stderr,
             "circe2: current beam description "
             "supports only polarization averages\n");
    return NULL;
  }
  if ((channels->polarization_support == POLHEL)
           && ((h1 == 0) || (h2 == 0))) {
    fprintf (stderr,
             "circe2: polarization averages not "
             "supported by current beam description\n");
    return NULL;
  }
  for (i = 0; i < channels->n; i++) {
    circe2_channel *ch = channels->ch[i];
    if ((p1 == ch->d1->pid) && (h1 == ch->d1->pol)
        && (p2 == ch->d2->pid) && (h2 == ch->d2->pol))
      return ch;
  }
  return NULL;
}
static int
binary_search (double *x, int bot, int top, double u)
{
  int low = bot;
  int high = top;
  while (1) {
    if (high <= (low + 1)) {
      return (low + 1);
      break;
    } else {
      int i = (low + high) / 2;
      if (u < x[i])
        high = i;
      else
        low = i;
    }
  }
}

/**********************************************************************/

void
circe2_generate (circe2_channels *channels,
                 int p1, int h1, int p2, int h2,
                 double *y1, double *y2,
                 void (*rng)(double *))
{
  circe2_channel *ch = NULL;
  int i, i1, i2;
  double u, x1, x2;
  ch = circe2_find_channel (channels, p1, h1, p2, h2);
  if (ch == NULL) {
    fprintf (stderr,
             "circe2: no channel for particles (%d, %d) "
             "and polarizations (%d, %d)\n", p1, p2, h1, h2);
    *y1 = -3.4e+38;
    *y2 = -3.4e+38;
    return;
  }
  rng (&u);
  i = binary_search (ch->weight, 0, ch->d1->n * ch->d2->n, u);
  split_index (ch, &i1, &i2, i);
        rng (&u);
        x1 = ch->d1->x[i1]*u + ch->d1->x[i1-1]*(1-u);
        rng (&u);
        x2 = ch->d2->x[i2]*u + ch->d2->x[i2-1]*(1-u);
  switch (ch->d1->map[i1]) {
  case 0:
    *y1 = x1;
  case 1:
    *y1 = pow (ch->d1->a[i1] * (x1 - ch->d1->xi[i1]), ch->d1->alpha[i1])
           / ch->d1->b[i1]
           + ch->d1->eta[i1];
  case 2:
    *y1 = ch->d1->a[i1] * tan (ch->d1->a[i1] * (x1 - ch->d1->xi[i1])
                                / (ch->d1->b[i1] * ch->d1->b[i1]))
           + ch->d1->eta[i1];
  default:
    fprintf (stderr, "circe2: internal error: invalid map: %d\n", ch->d1->map[i1]);
  }
  switch (ch->d2->map[i2]) {
  case 0:
    *y2 = x2;
  case 1:
    *y2 = pow (ch->d2->a[i2] * (x2 - ch->d2->xi[i2]), ch->d2->alpha[i2])
           / ch->d2->b[i2]
           + ch->d2->eta[i2];
  case 2:
    *y2 = ch->d2->a[i2] * tan (ch->d2->a[i2] * (x2 - ch->d2->xi[i2])
                                / (ch->d2->b[i2] * ch->d2->b[i2]))
           + ch->d2->eta[i2];
  default:
    fprintf (stderr, "circe2: internal error: invalid map: %d\n", ch->d2->map[i2]);
  }
  if (ch->triangle) {
    *y2 = *y1 * *y2;
    rng (&u);
    if (2*u >= 1) {
      double tmp;
      tmp = *y1;
      *y1 = *y2;
      *y2 = tmp;
    }
  }
}
void
circe2_random_channel (circe2_channels *channels,
                       int *p1, int *h1, int *p2, int *h2,
                       void (*rng) (double *))
{
  circe2_channel *ch;
  int ic, ibot, itop;
  double u;
  POINTER_PANIC(circe2_random_channel, channels, "channels");
  rng (&u);
  ibot = 0;
  itop = channels->n;
  while (ibot + 1 < itop) {
    ic = (ibot + itop) / 2;
    if (u < channels->ch[ic]->channel_weight)
      itop = ic;
    else
      ibot = ic;
  }
  ch = channels->ch[ibot + 1];
  POINTER_PANIC(circe2_random_channel, ch, "selected channel");
  *p1 = ch->d1->pid;
  *h1 = ch->d1->pol;
  *p2 = ch->d2->pid;
  *h2 = ch->d2->pol;
}
double
circe2_lumi (circe2_channels *channels, int p1, int h1, int p2, int h2)
{
  int i;
  double lumi;
  POINTER_PANIC(circe2_random_channel, channels, "channels");
  lumi = 0;
  for (i = 0; i < channels->n; i++) {
    circe2_channel *c = channels->ch[i];
    if (((p1 == c->d1->pid) || (p1 == 0))
        && ((h1 == c->d1->pol) || (h1 == 0))
        && ((p2 == c->d2->pid) || (p2 == 0))
        && ((h2 == c->d2->pol) || (h2 == 0)))
      lumi += c->lumi;
  }
  return lumi;
}
double
circe2_distribution (circe2_channels *channels,
                     int p1, int h1, int p2, int h2,
                     double yy1, double yy2)
{
  circe2_channel *ch;
  int i, i1, i2;
  double y1, y2, d;
  POINTER_PANIC(circe2_random_channel, channels, "channels");
  ch = circe2_find_channel (channels, p1, h1, p2, h2);
  if (ch == NULL)
    return 0.0;
  if (ch->triangle) {
    y1 = max (yy1, yy2);
    y2 = min (yy1, yy2) / y1;
  } else {
    y1 = yy1;
    y2 = yy2;
  }
  if ((y1 < ch->d1->y[0]) || (y1 > ch->d1->y[ch->d1->n])
      || (y2 < ch->d2->y[0]) || (y2 > ch->d2->y[ch->d2->n]))
    return 0.0;
  i1 = binary_search (ch->d1->y, 0, ch->d1->n, y1);
  i2 = binary_search (ch->d2->y, 0, ch->d2->n, y2);
  d = ch->value[i1][i2];
  switch (ch->d1->map[i1]) {
  case 0:
    /* identity */
  case 1:
    d = d * ch->d1->b[i1] / (ch->d1->a[i1] * ch->d1->alpha[i1])
          * pow (ch->d1->b[i1] * (y1 - ch->d1->eta[i1]), 1/ch->d1->alpha[i1] - 1);
  case 2:
    d = d * ch->d1->b[i1] * ch->d1->b[i1]
          / ((y1 - ch->d1->eta[i1]) * (y1 - ch->d1->eta[i1])
               + ch->d1->a[i1] * ch->d1->a[i1]);
  default:
    fprintf (stderr, "circe2: internal error: invalid map: %d\n", ch->d1->map[i1]);
    exit (-1);
  }
  switch (ch->d2->map[i2]) {
  case 0:
    /* identity */
  case 1:
    d = d * ch->d2->b[i2] / (ch->d2->a[i2] * ch->d2->alpha[i2])
          * pow (ch->d2->b[i2] * (y2 - ch->d2->eta[i2]), 1/ch->d2->alpha[i2] - 1);
  case 2:
    d = d * ch->d2->b[i2] * ch->d2->b[i2]
          / ((y2 - ch->d2->eta[i2]) * (y2 - ch->d2->eta[i2])
               + ch->d2->a[i2] * ch->d2->a[i2]);
  default:
    fprintf (stderr, "circe2: internal error: invalid map: %d\n", ch->d2->map[i2]);
    exit (-1);
  }
  if (ch->triangle)
    d = d / y1;
  return d;
}
void
circe2_load (const char *file, const char *design, double roots, int *error)
{
  char buffer[73];
  char file_design[73];
  char file_polarization_support[73];
  double file_roots;
  int loaded;
  FILE *f;
  if (*error > 0)
    printf ("circe2: %s\n",
            "$Id: circe2.nw,v 1.56 2002/10/14 10:12:06 ohl Exp $");
  loaded = 0;
  f = fopen (file, "r");
  if (f == NULL) {
    fprintf (stderr, "circe2_load: can't open %s: %s\n", file, sys_errlist[errno]);
    *error = CIRCE2_EFILE;
    return;
  }
  while (loaded == 0) {
    while (1) {
      fgets (buffer, 72, f);
      if (strncmp ("ECRIC2", buffer, 6) == 0) {
        fclose (f);
        if (loaded)
          *error = CIRCE2_EOK;
        else {
          fprintf (stderr, "circe2_load: no match in %s\n", file);
          *error = CIRCE2_EMATCH;
        }
        return;
      } else if ((buffer[0] == '!') && (*error > 0))
        printf ("circe2: %s\n", buffer);
    }
    fprintf (stderr, "circe2_load: invalid format %s\n", file);
    *error = CIRCE2_EFORMT;
    return;
    if (strncmp ("FORMAT#1", buffer, 8) == 0) {
    }
    fgets (buffer, 72, f);
    if (strncmp ("ECRIC2", buffer, 6) != 0) {
      fprintf (stderr, "circe2_load: invalid format %s\n", file);
      *error = CIRCE2_EFORMT;
      return;
    }
  }
}
/*
      integer prefix
      logical match
      prefix = index (design, '*') - 1
 100  continue
          20   continue
                  read (lun, '(A)', end = 29) buffer
                  if (buffer(1:6) .eq. 'CIRCE2') then
                     goto 21
                  else if (buffer(1:1) .eq. '!') then
                     if (ierror .gt. 0) then
                        write (*, '(A)') buffer
                     end if
                     goto 20
                  end if
                  write (*, '(A)') 'cir2ld: invalid file'
                  ierror = EFORMT
                  return
          29   continue
               if (loaded .gt. 0) then           
                  close (unit = lun)
                  ierror = EOK
               else
                  ierror = EMATCH
               end if
               return
          21   continue
         if (buffer(8:15) .eq. 'FORMAT#1') then
            read (lun, *)
            read (lun, *) fdesgn, froots
                match = .false.
                if (fdesgn .eq. design) then
                   match = .true.
                else if (prefix .eq. 0) then
                   match = .true.
                else if (prefix .gt. 0) then
                   if (fdesgn(1:min(prefix,len(fdesgn))) &
                        .eq. design(1:min(prefix,len(design)))) then
                      match = .true.
                   end if
                end if
            if (match .and. (abs (froots - roots) .le. 1d0)) then
                   read (lun, *) 
                   read (lun, *) c2p%nc, fpolsp
                   if (c2p%nc .gt. NCMAX) then
                      write (*, '(A)') 'cir2ld: too many channels'
                      ierror = ESIZE
                      return
                   end if
                       if (      (fpolsp(1:1).eq.'a') &
                            .or. (fpolsp(1:1).eq.'A')) then
                          c2p%polspt = POLAVG
                       else if (      (fpolsp(1:1).eq.'h') &
                                 .or. (fpolsp(1:1).eq.'H')) then
                          c2p%polspt = POLHEL
                       else if (      (fpolsp(1:1).eq.'d') &
                                 .or. (fpolsp(1:1).eq.'D')) then
                          c2p%polspt = POLGEN
                       else
                          write (*, '(A,I5)') &
                               'cir2ld: invalid polarization support: ', fpolsp
                          ierror = EFORMT
                          return
                       end if
                   c2p%cwgt(0) = 0
                   do ic = 1, c2p%nc
                          read (lun, *)
                          read (lun, *) c2p%pid1(ic), c2p%pol1(ic), &
                                c2p%pid2(ic), c2p%pol2(ic), c2p%lumi(ic)
                          c2p%cwgt(ic) = c2p%cwgt(ic-1) + c2p%lumi(ic)
                              if (c2p%polspt .eq. POLAVG &
                                   .and. (      (c2p%pol1(ic) .ne. 0)  &
                                           .or. (c2p%pol2(ic) .ne. 0))) then
                                 write (*, '(A)') 'cir2ld: expecting averaged polarization'
                                 ierror = EFORMT
                                 return
                              else if (c2p%polspt .eq. POLHEL &
                                   .and. (      (c2p%pol1(ic) .eq. 0)       &
                                           .or. (c2p%pol2(ic) .eq. 0))) then
                                 write (*, '(A)') 'cir2ld: expecting helicities'
                                 ierror = EFORMT
                                 return
                              else if (c2p%polspt .eq. POLGEN) then
                                 write (*, '(A)') 'cir2ld: general polarizations not supported yet'
                                 ierror = EFORMT
                                 return
                              else if (c2p%polspt .eq. POLGEN &
                                   .and. (      (c2p%pol1(ic) .ne. 0)       &
                                           .or. (c2p%pol2(ic) .ne. 0))) then
                                 write (*, '(A)') 'cir2ld: expecting pol = 0'
                                 ierror = EFORMT
                                 return
                              end if
                          read (lun, *)
                          read (lun, *) c2p%nb1(ic), c2p%nb2(ic), c2p%triang(ic)
                          if ((c2p%nb1(ic) .gt. NBMAX) .or. (c2p%nb2(ic) .gt. NBMAX)) then
                             write (*, '(A)') 'cir2ld: too many bins'
                             ierror = ESIZE
                             return
                          end if
                          read (lun, *)
                          read (lun, *) c2p%xb1(0,ic)
                          do i1 = 1, c2p%nb1(ic)
                             read (lun, *) c2p%xb1(i1,ic), c2p%map1(i1,ic), c2p%alpha1(i1,ic), &
                                 c2p%xi1(i1,ic), c2p%eta1(i1,ic), c2p%a1(i1,ic), c2p%b1(i1,ic)
                          end do
                          read (lun, *)
                          read (lun, *) c2p%xb2(0,ic)
                          do i2 = 1, c2p%nb2(ic)
                             read (lun, *) c2p%xb2(i2,ic), c2p%map2(i2,ic), c2p%alpha2(i2,ic), &
                                 c2p%xi2(i2,ic), c2p%eta2(i2,ic), c2p%a2(i2,ic), c2p%b2(i2,ic)
                          end do
                          do i = 0, c2p%nb1(ic)
                             i1 = max (i, 1)
                             if (c2p%map1(i1,ic) .eq. 0) then
                                c2p%yb1(i,ic) = c2p%xb1(i,ic)
                             else if (c2p%map1(i1,ic) .eq. 1) then
                                c2p%yb1(i,ic) = &
                                     (c2p%a1(i1,ic) &
                                       * (c2p%xb1(i,ic)-c2p%xi1(i1,ic)))**c2p%alpha1(i1,ic) &
                                          / c2p%b1(i1,ic) + c2p%eta1(i1,ic)
                             else if (c2p%map1(i1,ic) .eq. 2) then
                                c2p%yb1(i,ic) = c2p%a1(i1,ic) &
                                     * tan(c2p%a1(i1,ic)/c2p%b1(i1,ic)**2 &
                                            * (c2p%xb1(i,ic)-c2p%xi1(i1,ic))) &
                                         + c2p%eta1(i1,ic)
                             else
                                write (*, '(A,I3)') 'cir2ld: invalid map: ', c2p%map1(i1,ic)
                                ierror = EFORMT
                                return
                             end if
                          end do
                          do i = 0, c2p%nb2(ic)
                             i2 = max (i, 1)
                             if (c2p%map2(i2,ic) .eq. 0) then
                                c2p%yb2(i,ic) = c2p%xb2(i,ic)
                             else if (c2p%map2(i2,ic) .eq. 1) then
                                c2p%yb2(i,ic) &
                                     = (c2p%a2(i2,ic) &
                                         * (c2p%xb2(i,ic)-c2p%xi2(i2,ic)))**c2p%alpha2(i2,ic) &
                                            / c2p%b2(i2,ic) + c2p%eta2(i2,ic)
                             else if (c2p%map2(i2,ic) .eq. 2) then
                                c2p%yb2(i,ic) = c2p%a2(i2,ic) &
                                     * tan(c2p%a2(i2,ic)/c2p%b2(i2,ic)**2 &
                                            * (c2p%xb2(i,ic)-c2p%xi2(i2,ic))) &
                                         + c2p%eta2(i2,ic)
                             else
                                write (*, '(A,I3)') 'cir2ld: invalid map: ', c2p%map2(i2,ic)
                                ierror = EFORMT
                                return
                             end if
                          end do
                          read (lun, *)
                          c2p%wgt(0,ic) = 0
                          do i = 1, c2p%nb1(ic)*c2p%nb2(ic)
                             read (lun, *) w
                             c2p%wgt(i,ic) = c2p%wgt(i-1,ic) + w
                                   i2 = 1 + (i - 1) / c2p%nb1(ic)
                                   i1 = i - (i2 - 1) * c2p%nb1(ic)
                             c2p%val(i1,i2,ic) = w & 
                                  / (  (c2p%xb1(i1,ic) - c2p%xb1(i1-1,ic)) &
                                     * (c2p%xb2(i2,ic) - c2p%xb2(i2-1,ic)))
                          end do
                          c2p%wgt(c2p%nb1(ic)*c2p%nb2(ic),ic) = 1
                   end do   
                   do ic = 1, c2p%nc
                      c2p%cwgt(ic) = c2p%cwgt(ic) / c2p%cwgt(c2p%nc)
                   end do
               loaded = loaded + 1
            else
                101  continue
                     read (lun, *) buffer
                     if (buffer(1:6) .ne. 'ECRIC2') then
                        goto 101
                     end if
               goto 100
            end if
         else
            write (*, '(2A)') 'cir2ld: invalid format: ', buffer(8:72)
            ierror = EFORMT
            return
         end if
               read (lun, '(A)') buffer
               if (buffer(1:6) .ne. 'ECRIC2') then
                  write (*, '(A)') 'cir2ld: invalid file'
                  ierror = EFORMT
                  return
               end if
         goto 100
      end
*/
