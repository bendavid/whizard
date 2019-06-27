C*********************************************************************
C*********************************************************************
C*                                                                  **
C*                                                      June 2010   **
C*                                                                  **
C*                       The Lund Monte Carlo                       **
C*                                                                  **
C*                        PYTHIA version 6.4                        **
C*                                                                  **
C*                        Torbjorn Sjostrand                        **
C*                 Department of Theoretical Physics                **
C*                         Lund University                          **
C*               Solvegatan 14A, S-223 62 Lund, Sweden              **
C*                    E-mail torbjorn@thep.lu.se                    **
C*                                                                  **
C*                  SUSY and Technicolor parts by                   **
C*                         Stephen Mrenna                           **
C*                       Computing Division                         ** 
C*            Generators and Detector Simulation Group              **
C*              Fermi National Accelerator Laboratory               **
C*                 MS 234, Batavia, IL  60510, USA                  **
C*                   phone + 1 - 630 - 840 - 2556                   **
C*                      E-mail mrenna@fnal.gov                      **
C*                                                                  **
C*         New multiple interactions and more SUSY parts by         **
C*                          Peter Skands                            **
C*               CERN/PH, CH-1211 Geneva, Switzerland               **
C*                    phone +41 - 22 - 767 2447                     **
C*                   E-mail peter.skands@cern.ch                    **
C*                                                                  **
C*         Several parts are written by Hans-Uno Bengtsson          **
C*          PYSHOW is written together with Mats Bengtsson          **
C*               PYMAEL is written by Emanuel Norrbin               **
C*     advanced popcorn baryon production written by Patrik Eden    **
C*    code for virtual photons mainly written by Christer Friberg   **
C*    code for low-mass strings mainly written by Emanuel Norrbin   **
C*        Bose-Einstein code mainly written by Leif Lonnblad        **
C*      CTEQ  parton distributions are by the CTEQ collaboration    **
C*      GRV 94 parton distributions are by Glueck, Reya and Vogt    **
C*   SaS photon parton distributions together with Gerhard Schuler  **
C*     g + g and q + qbar -> t + tbar + H code by Zoltan Kunszt     **
C*         MSSM Higgs mass calculation code by M. Carena,           **
C*           J.R. Espinosa, M. Quiros and C.E.M. Wagner             **
C*  UED implementation by M. Elkacimi, D. Goujdami, H. Przysiezniak **
C*         PYGAUS adapted from CERN library (K.S. Kolbig)           **
C*        NRQCD/colour octet production of onium by S. Wolf         **
C*                                                                  **
C*   The latest program version and documentation is found on WWW   **
C*            http://www.thep.lu.se/~torbjorn/Pythia.html           **
C*                                                                  **
C*              Copyright Torbjorn Sjostrand, Lund 2010             **
C*                                                                  **
C*********************************************************************
C*********************************************************************
      SUBROUTINE UPINIT
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
 
C...PYTHIA commonblock: only used to provide read unit MSTP(161).
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      SAVE /PYPARS/
 
C...User process initialization commonblock.
      INTEGER MAXPUP
      PARAMETER (MAXPUP=100)
      INTEGER IDBMUP,PDFGUP,PDFSUP,IDWTUP,NPRUP,LPRUP
      DOUBLE PRECISION EBMUP,XSECUP,XERRUP,XMAXUP
      COMMON/HEPRUP/IDBMUP(2),EBMUP(2),PDFGUP(2),PDFSUP(2),
     &IDWTUP,NPRUP,XSECUP(MAXPUP),XERRUP(MAXPUP),XMAXUP(MAXPUP),
     &LPRUP(MAXPUP)
      SAVE /HEPRUP/

C...Lines to read in assumed never longer than 200 characters. 
      PARAMETER (MAXLEN=200)
      CHARACTER*(MAXLEN) STRING

C...Format for reading lines.
      CHARACTER*6 STRFMT
      STRFMT='(A000)'
      WRITE(STRFMT(3:5),'(I3)') MAXLEN

C...Loop until finds line beginning with "<init>" or "<init ". 
  100 READ(MSTP(161),STRFMT,END=130,ERR=130) STRING
      IBEG=0
  110 IBEG=IBEG+1
C...Allow indentation.
      IF(STRING(IBEG:IBEG).EQ.' '.AND.IBEG.LT.MAXLEN-5) GOTO 110 
      IF(STRING(IBEG:IBEG+5).NE.'<init>'.AND.
     &STRING(IBEG:IBEG+5).NE.'<init ') GOTO 100

C...Read first line of initialization info.
      READ(MSTP(161),*,END=130,ERR=130) IDBMUP(1),IDBMUP(2),EBMUP(1),
     &EBMUP(2),PDFGUP(1),PDFGUP(2),PDFSUP(1),PDFSUP(2),IDWTUP,NPRUP

C...Read NPRUP subsequent lines with information on each process.
      DO 120 IPR=1,NPRUP
        READ(MSTP(161),*,END=130,ERR=130) XSECUP(IPR),XERRUP(IPR),
     &  XMAXUP(IPR),LPRUP(IPR)
  120 CONTINUE
      RETURN

C...Error exit: give up if initalization does not work.
  130 WRITE(*,*) ' Failed to read LHEF initialization information.'
      WRITE(*,*) ' Event generation will be stopped.'
      CALL PYSTOP(12)
 
      RETURN
      END

C...Old example: handles a simple Pythia 6.4 initialization file.
 
c      SUBROUTINE UPINIT
 
C...Double precision and integer declarations.
c      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
c      IMPLICIT INTEGER(I-N)
 
C...Commonblocks.
c      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
c      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
c      SAVE /PYDAT1/,/PYPARS/
 
C...User process initialization commonblock.
c      INTEGER MAXPUP
c      PARAMETER (MAXPUP=100)
c      INTEGER IDBMUP,PDFGUP,PDFSUP,IDWTUP,NPRUP,LPRUP
c      DOUBLE PRECISION EBMUP,XSECUP,XERRUP,XMAXUP
c      COMMON/HEPRUP/IDBMUP(2),EBMUP(2),PDFGUP(2),PDFSUP(2),
c     &IDWTUP,NPRUP,XSECUP(MAXPUP),XERRUP(MAXPUP),XMAXUP(MAXPUP),
c     &LPRUP(MAXPUP)
c      SAVE /HEPRUP/
 
C...Read info from file.
c      IF(MSTP(161).GT.0) THEN
c        READ(MSTP(161),*,END=110,ERR=110) IDBMUP(1),IDBMUP(2),EBMUP(1),
c     &  EBMUP(2),PDFGUP(1),PDFGUP(2),PDFSUP(1),PDFSUP(2),IDWTUP,NPRUP
c        DO 100 IPR=1,NPRUP
c          READ(MSTP(161),*,END=110,ERR=110) XSECUP(IPR),XERRUP(IPR),
c     &    XMAXUP(IPR),LPRUP(IPR)
c  100   CONTINUE
c        RETURN
C...Error or prematurely reached end of file.
c  110   WRITE(MSTU(11),5000)
c        STOP
 
C...Else not implemented.
c      ELSE
c        WRITE(MSTU(11),5100)
c        STOP
c      ENDIF
 
C...Format for error printout.
c 5000 FORMAT(1X,'Error: UPINIT routine failed to read information'/
c     &1X,'Execution stopped!')
c 5100 FORMAT(1X,'Error: You have not implemented UPINIT routine'/
c     &1X,'Dummy routine in PYTHIA file called instead.'/
c     &1X,'Execution stopped!')
 
c      RETURN
c      END
 
C*********************************************************************
 
C...UPEVNT
C...Dummy routine, to be replaced by a user implementing external
C...processes. Depending on cross section model chosen, it either has
C...to generate a process of the type IDPRUP requested, or pick a type
C...itself and generate this event. The event is to be stored in the
C...HEPEUP commonblock, including (often) an event weight.

C...New example: handles a standard Les Houches Events File.

      SUBROUTINE UPEVNT
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
 
C...PYTHIA commonblock: only used to provide read unit MSTP(162).
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      SAVE /PYPARS/
 
C...User process event common block.
      INTEGER MAXNUP
      PARAMETER (MAXNUP=500)
      INTEGER NUP,IDPRUP,IDUP,ISTUP,MOTHUP,ICOLUP
      DOUBLE PRECISION XWGTUP,SCALUP,AQEDUP,AQCDUP,PUP,VTIMUP,SPINUP
      COMMON/HEPEUP/NUP,IDPRUP,XWGTUP,SCALUP,AQEDUP,AQCDUP,IDUP(MAXNUP),
     &ISTUP(MAXNUP),MOTHUP(2,MAXNUP),ICOLUP(2,MAXNUP),PUP(5,MAXNUP),
     &VTIMUP(MAXNUP),SPINUP(MAXNUP)
      SAVE /HEPEUP/

C...Lines to read in assumed never longer than 200 characters. 
      PARAMETER (MAXLEN=200)
      CHARACTER*(MAXLEN) STRING

C...Format for reading lines.
      CHARACTER*6 STRFMT
      STRFMT='(A000)'
      WRITE(STRFMT(3:5),'(I3)') MAXLEN

C...Loop until finds line beginning with "<event>" or "<event ". 
  100 READ(MSTP(162),STRFMT,END=130,ERR=130) STRING
      IBEG=0
  110 IBEG=IBEG+1
C...Allow indentation.
      IF(STRING(IBEG:IBEG).EQ.' '.AND.IBEG.LT.MAXLEN-6) GOTO 110 
      IF(STRING(IBEG:IBEG+6).NE.'<event>'.AND.
     &STRING(IBEG:IBEG+6).NE.'<event ') GOTO 100

C...Read first line of event info.
      READ(MSTP(162),*,END=130,ERR=130) NUP,IDPRUP,XWGTUP,SCALUP,
     &AQEDUP,AQCDUP

C...Read NUP subsequent lines with information on each particle.
      DO 120 I=1,NUP
        READ(MSTP(162),*,END=130,ERR=130) IDUP(I),ISTUP(I),
     &  MOTHUP(1,I),MOTHUP(2,I),ICOLUP(1,I),ICOLUP(2,I),
     &  (PUP(J,I),J=1,5),VTIMUP(I),SPINUP(I)
  120 CONTINUE
      RETURN

C...Error exit, typically when no more events.
  130 WRITE(*,*) ' Failed to read LHEF event information.'
      WRITE(*,*) ' Will assume end of file has been reached.'
      NUP=0
      MSTI(51)=1
 
      RETURN
      END

C...Old example: handles a simple Pythia 6.4 event file.
 
c      SUBROUTINE UPEVNT
 
C...Double precision and integer declarations.
c      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
c      IMPLICIT INTEGER(I-N)
 
C...Commonblocks.
c      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
c      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
c      SAVE /PYDAT1/,/PYPARS/
 
C...User process event common block.
c      INTEGER MAXNUP
c      PARAMETER (MAXNUP=500)
c      INTEGER NUP,IDPRUP,IDUP,ISTUP,MOTHUP,ICOLUP
c      DOUBLE PRECISION XWGTUP,SCALUP,AQEDUP,AQCDUP,PUP,VTIMUP,SPINUP
c      COMMON/HEPEUP/NUP,IDPRUP,XWGTUP,SCALUP,AQEDUP,AQCDUP,IDUP(MAXNUP),
c     &ISTUP(MAXNUP),MOTHUP(2,MAXNUP),ICOLUP(2,MAXNUP),PUP(5,MAXNUP),
c     &VTIMUP(MAXNUP),SPINUP(MAXNUP)
c      SAVE /HEPEUP/
 
C...Read info from file.
c      IF(MSTP(162).GT.0) THEN
c        READ(MSTP(162),*,END=110,ERR=110) NUP,IDPRUP,XWGTUP,SCALUP,
c     &  AQEDUP,AQCDUP
c        DO 100 I=1,NUP
c          READ(MSTP(162),*,END=110,ERR=110) IDUP(I),ISTUP(I),
c     &    MOTHUP(1,I),MOTHUP(2,I),ICOLUP(1,I),ICOLUP(2,I),
c     &    (PUP(J,I),J=1,5),VTIMUP(I),SPINUP(I)
c  100   CONTINUE
c        RETURN
C...Special when reached end of file or other error.
c  110   NUP=0
 
C...Else not implemented.
c      ELSE
c        WRITE(MSTU(11),5000)
c        STOP
c      ENDIF

C...Format for error printout.
c 5000 FORMAT(1X,'Error: You have not implemented UPEVNT routine'/
c     &1X,'Dummy routine in PYTHIA file called instead.'/
c     &1X,'Execution stopped!')
 
c      RETURN
c      END
 
C*********************************************************************
 
C...UPVETO
C...Dummy routine, to be replaced by user, to veto event generation
C...on the parton level, after parton showers but before multiple
C...interactions, beam remnants and hadronization is added.
C...If resonances like W, Z, top, Higgs and SUSY particles are handed
C...undecayed from UPEVNT, or are generated by PYTHIA, they will also
C...be undecayed at this stage; if decayed their decay products will
C...have been allowed to shower.
 
C...All partons at the end of the shower phase are stored in the
C...HEPEVT commonblock. The interesting information is
C...NHEP = the number of such partons, in entries 1 <= i <= NHEP,
C...IDHEP(I) = the particle ID code according to PDG conventions,
C...PHEP(J,I) = the (p_x, p_y, p_z, E, m) of the particle.
C...All ISTHEP entries are 1, while the rest is zeroed.
 
C...The user decision is to be conveyed by the IVETO value.
C...IVETO = 0 : retain current event and generate in full;
C...      = 1 : abort generation of current event and move to next.

      SUBROUTINE UPVETO(IVETO)
 
C...HEPEVT commonblock.
      PARAMETER (NMXHEP=4000)
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      DOUBLE PRECISION PHEP,VHEP
      SAVE /HEPEVT/
 
C...Next few lines allow you to see what info PYVETO extracted from
C...the full event record for the first two events.
C...Delete if you don't want it.
      DATA NLIST/0/
      SAVE NLIST
      IF(NLIST.LE.2) THEN
        WRITE(*,*) ' Full event record at time of UPVETO call:'
        CALL PYLIST(1)
        WRITE(*,*) ' Part of event record made available to UPVETO:'
        CALL PYLIST(5)
        NLIST=NLIST+1
      ENDIF
 
C...Make decision here.
      IVETO = 0
 
      RETURN
      END
 
