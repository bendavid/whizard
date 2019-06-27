CCCCCCCCCCCCCCCCCCD
C
C
C     dw - matching WHIZARD output to pythia parton shower
C
C
CCCCCCCCCCCCCCCCCCD


      PROGRAM FIRST

      IMPLICIT NONE

C...Pythia parameters. 
      INTEGER MSTP,MSTI, NMUL
      DOUBLE PRECISION PARP,PARI, PT, W
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
C...Make sure PYDATA is linked
      EXTERNAL PYDATA

C...The event record.
      INTEGER N,NPAD,K
      DOUBLE PRECISION P,V
      COMMON/PYJETS/N,NPAD,K(4000,5),P(4000,5),V(4000,5)

C...Pythia common blocks
      INTEGER PYCOMP,KCHG,MINT,NPART,NPARTD,IPART,MAXNUR
      DOUBLE PRECISION PMAS,PARF,VCKM,VINT,PTPART
C...Particle properties + some flavour parameters.
      COMMON/PYDAT2/KCHG(500,4),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON/PYINT1/MINT(400),VINT(400)
      PARAMETER (MAXNUR=1000)
      COMMON/PYPART/NPART,NPARTD,IPART(MAXNUR),PTPART(MAXNUR)

C...User process initialization commonblock.
      INTEGER MAXPUP
      PARAMETER (MAXPUP=100)
      INTEGER IDBMUP,PDFGUP,PDFSUP,IDWTUP,NPRUP,LPRUP
      DOUBLE PRECISION EBMUP,XSECUP,XERRUP,XMAXUP
      COMMON/HEPRUP/IDBMUP(2),EBMUP(2),PDFGUP(2),PDFSUP(2),
     &   IDWTUP,NPRUP,XSECUP(MAXPUP),XERRUP(MAXPUP),XMAXUP(MAXPUP),
     &   LPRUP(MAXPUP)

C...User process event common block.
      INTEGER MAXNUP
      PARAMETER (MAXNUP=500)
      INTEGER NUP,IDPRUP,IDUP,ISTUP,MOTHUP,ICOLUP
      DOUBLE PRECISION XWGTUP,SCALUP,AQEDUP,AQCDUP,PUP,VTIMUP,SPINUP
      COMMON/HEPEUP/NUP,IDPRUP,XWGTUP,SCALUP,AQEDUP,AQCDUP,IDUP(MAXNUP),
     &   ISTUP(MAXNUP),MOTHUP(2,MAXNUP),ICOLUP(2,MAXNUP),PUP(5,MAXNUP),
     &   VTIMUP(MAXNUP),SPINUP(MAXNUP)

C...Run info common block
      INTEGER LNHIN,LNHOUT,MSCAL,IEVNT,CNT
      COMMON/UPRUNI/LNHIN,LNHOUT,MSCAL,IEVNT,CNT

C...Event information common block
      INTEGER nljets,maxj,minj
      COMMON/EVTOEV/nljets,maxj,minj

C...Matching parameters common block
      double precision etcjet,rclmax,etaclmax,qcut,clfact
      integer ktsche,ktmode
      logical lhefout
      common/MATPAR/etcjet,rclmax,etaclmax,qcut,clfact,ktsche,ktmode,
     &     lhefout

C...Local variables      
      INTEGER NEV,IEV,NEX,i, NITER, istrstd
      CHARACTER*3 CHLNHOUT
      CHARACTER*5 CGIVE
      CHARACTER*30 CGIVE0
      logical lok
C...Maximum number of events to generate.
      NEV = -1             
C   initialize WHIZARDs LHEF output
      OPEN (LNHIN, FILE='mlm_sample.lhef', ERR=90 )
c     set Pythia parameters for matching
      LNHOUT = 88
      CALL PYGIVE('MSTP(143)=1;MSTP(81)=0;MSTJ(1)=1')
      CALL PYGIVE('MSTU(21)=1;MSTU(22)=10;MSTP(111)=0')
      OPEN (LNHOUT, FILE='mlm_sample_matched.lhef', ERR=90 )
      WRITE(CHLNHOUT,'(I3)') LNHOUT
      CALL PYGIVE('MSTP(163)='//CHLNHOUT)
C...Initialize with external process.
      CALL PYINIT('USER',' ',' ',0D0)
      CALL PYLHIN(LNHIN)

      IEVNT=0
C...Event loop
      DO 100 WHILE(IEVNT.LT.NEV.OR.NEV.LT.0)
         IEVNT=IEVNT+1
C...Get event
        CALL PYEVNT
        CALL PYLHEO

C...If event generation failed, quit loop
        IF(MSTI(51).EQ.1) THEN
           cnt = ievnt
          GOTO 110 
        ENDIF
 100  CONTINUE

 110  CALL PYSTAT(1)

      CLOSE (LNHOUT)
      CLOSE (LNHIN)

 90   WRITE(*,*) 'Error: Could not open LHEF event file'
      WRITE(*,*) 'Quitting...'
      END


