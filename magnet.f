Cbogi
Cbogi  Alle Zeilen die mit Cbogi beginnen, sind von
Cbogi  Michael Boegl, EPV, Uni Bayreuth
Cbogi  Das Programm ist zum Spulenberechnen mit/ohne supraleitenden
Cbogi  Schild und ist aus Helsinki.
Cbogi  Ich habe das Programm von Jashushi Kondo erhalten und
Cbogi  portierte es auf die HP Apollo 710.
Cbogi
Cbogi  Dabei stehen folgende Wuensche fuer das Programm auf der Wunschliste::
Cbogi  1. Gnuplot Output
Cbogi  5. Einlesen des Ausgabefiles, so dass man sich die interaktive
Cbogi     Tortur durch das Frage- und Antwortspiel erspart.
Cbogi  6. WRSHI(...) sollte so umgeschrieben werden, dass nicht mehr auf
Cbogi     das Terminal als File No. 6 zugegriffen werden muss.
Cbogi
Cbogi  Michael Boegl, EPV, Uni-Bayreuth, Oktober '93
Cbogi
Cbogi  What's already done:
Cbogi  1. Es laeuft
Cbogi  2. field calculation on lattice
Cbogi  3. WRITE(6,xxxx) -> PRINT xxxx
Cbogi  4. Output in verschiedene Files
Cbogi  5. Output ist Gnuplot kompatibel (Es gibt noch Probleme mit splot)!
C
Cjl    Hi Bogi,
Cjl    ich versuche den TRAFLU f"ur jedes Schild einzeln eingebbar
Cjl    zu machen. Ich ersetze die Veriable TRAFLU durch das Array
Cjl    TRAFLX(ISHI) ...
Cjl    Eingaberoutine: TRAFLX wird jetzt mit dem Schild eingegeben
C
C
      SUBROUTINE ZERO(MATRIX, DIM1, DIM2)
C
C           (* Makes every component of MATRIX(DIM1,DIM2) zero.   *)
C
      INTEGER DIM1, DIM2
      DOUBLE PRECISION MATRIX(DIM1, DIM2)
C
      INTEGER I, J
C                  ...............................
C
      DO 1020 I = 1, DIM1
         DO 1010 J = 1, DIM2
            MATRIX(I,J) = 0D0
 1010    CONTINUE
 1020 CONTINUE
C
      RETURN
      END
C
C
C     ****************************************************************
C
C
      DOUBLE PRECISION FUNCTION ELLIP3(N, K)
C
C           (* Calculates complete elliptic integral of the third *)
C           (* kind   using   IMSL-functions   DELRF   and  DELRJ *)
C           (* (incomplete  elliptic  integrals  of the first and *)
C           (* third  kind, respectively). The input parameters N *)
C           (* and  K are the parameters of the complete elliptic *)
C           (* integral  of third kind when its normal definition *)
C           (* is used.                                           *)
C
      DOUBLE PRECISION N, K
C
      DOUBLE PRECISION DELRF, DELRJ
C                  ...............................
C
      ELLIP3 = DELRF(0D0, 1D0-K, 1D0) +
     1         DELRJ(0D0, 1D0-K, 1D0, 1D0-N)*N/3D0
C
      RETURN
      END
C
C
C     ****************************************************************
C
C
      DOUBLE PRECISION FUNCTION FDSKBZ(X)
C
C           (* When integrated over X from 0 to PI and multiplied *)
C           (* by  the  current  density, this function gives the *)
C           (* z-component  of the magnetic field produced by the *)
C           (* current disk defined in FUNCTION DSKBZ.            *)
C
      DOUBLE PRECISION X, KSIF, RF, RSOUF, WF
      COMMON /AREA/ KSIF, RF, RSOUF, WF
C           (* X : theta-component of the source point            *)
C           (* KSIF : z-component  of  the vector from the center *)
C           (*        of the source ring to the field point       *)
C           (* RF : r-coordinate of the field point               *)
C           (* RSOUF : the "average radius" of the source ring    *)
C           (* WF : the width of the source ring                  *)
C
      DOUBLE PRECISION C, RAD1, RAD2, K1, K2, T1, T2, S1, S2
C           (* C : cos(X)                                         *)
C           (* RAD1, RAD2 : inner and outer radius of the current *)
C           (*              disk, respectively                    *)
C           (* K1, K2, T1, T2, S1, S2 : defined in formulae below *)
C                  ...............................
C
      C = DCOS(X)
C
      RAD1 = RSOUF - WF/2D0
      RAD2 = RSOUF + WF/2D0
C
      K1 = DSQRT(RAD1*RAD1 + RF*RF + KSIF*KSIF - 2D0*RAD1*RF*C)
      K2 = DSQRT(RAD2*RAD2 + RF*RF + KSIF*KSIF - 2D0*RAD2*RF*C)
C
      T1 = KSIF*KSIF + RAD1*RAD1 + 2D0*RF*RF - 3D0*RAD1*RF*C
      T2 = KSIF*KSIF + RAD2*RAD2 + 2D0*RF*RF - 3D0*RAD2*RF*C
C
      S1 = RF*C*C*(((RAD1*RAD1+RF*RF)*C-RAD1*RF*(1D0+C*C))/K1 - C*K1) /
     1                                     (KSIF*KSIF + RF*RF*(1D0-C*C))
      S2 = RF*C*C*(((RAD2*RAD2+RF*RF)*C-RAD2*RF*(1D0+C*C))/K2 - C*K2) /
     1                                     (KSIF*KSIF + RF*RF*(1D0-C*C))
C
      FDSKBZ = 2D-7 * ((T2/K2-T1/K1)*C/RF + S2 - S1
     1                 + 2D0*C*C*DLOG((K2+RAD2-RF*C)/(K1+RAD1-RF*C)))
C
      RETURN
      END
C
C
C     ****************************************************************
C
C
      DOUBLE PRECISION FUNCTION FDSKBR(X)
C
C           (* When integrated over X from 0 to PI and multiplied *)
C           (* by  the  current  density, this function gives the *)
C           (* r-component  of the magnetic field produced by the *)
C           (* current disk defined in FUNCTION DSKBR.            *)
C
      DOUBLE PRECISION X, KSIF, RF, RSOUF, WF
      COMMON /AREA/ KSIF, RF, RSOUF, WF
C           (* X : theta-component of the source point            *)
C           (* KSIF : z-component  of  the vector from the center *)
C           (*        of the source ring to the field point       *)
C           (* RF : r-coordinate of the field point               *)
C           (* RSOUF : the "average radius" of the source ring    *)
C           (* WF : the width of the source ring                  *)
C
      DOUBLE PRECISION C, RAD1, RAD2, K1, K2
C           (* C : cos(X)                                         *)
C           (* RAD1, RAD2 : inner and outer radius of the current *)
C           (*              disk, respectively                    *)
C           (* K1, K2 : defined in formulae below                 *)
C                  ...............................
C
      C = DCOS(X)
C
      RAD1 = RSOUF - WF/2D0
      RAD2 = RSOUF + WF/2D0
C
      K1 = DSQRT(RAD1*RAD1 + RF*RF + KSIF*KSIF - 2D0*RAD1*RF*C)
      K2 = DSQRT(RAD2*RAD2 + RF*RF + KSIF*KSIF - 2D0*RAD2*RF*C)
C
      FDSKBR = 2D-7*KSIF*(C/(KSIF*KSIF+RF*RF*(1-C*C))) *
     1       ((-KSIF*KSIF-RF*RF+RAD2*RF*C)/K2 -
     2        (-KSIF*KSIF-RF*RF+RAD1*RF*C)/K1)
C
      RETURN
      END
C
C
C     ****************************************************************
C
C
      DOUBLE PRECISION FUNCTION FDSPOT(X)
C
C           (* When integrated over X from 0 to PI, multiplied by *)
C           (* the  current  density  and  divided  by  WF,  this *)
C           (* function  gives  the  vector potential produced by *)
C           (* the current disk defined in FUNCTION DSKPOT.       *)
C
      DOUBLE PRECISION X, KSIF, RF, RSOUF, WF
      COMMON /AREA/ KSIF, RF, RSOUF, WF
C           (* X : theta-component of the source point            *)
C           (* KSIF : z-component  of  the vector from the center *)
C           (*        of the source ring to the field point       *)
C           (* RF : r-coordinate of the field point               *)
C           (* RSOUF : the "average radius" of the source ring    *)
C           (* WF : the width of the source ring                  *)
C
      DOUBLE PRECISION RAD1, RAD2, C, K1, K2, S1, S2
C           (* RAD1, RAD2 : inner and outer radius of the current *)
C           (*              disk, respectively                    *)
C           (* C : cos(X)                                         *)
C           (* K1, K2, S1, S2 : defined in formulae below         *)
C
      C = DCOS(X)
C
      RAD1 = RSOUF - WF/2D0
      RAD2 = RSOUF + WF/2D0
C
      K1 = DSQRT(RAD1*RAD1 + RF*RF + KSIF*KSIF - 2*RAD1*RF*C)
      K2 = DSQRT(RAD2*RAD2 + RF*RF + KSIF*KSIF - 2*RAD2*RF*C)
C
      S1 = RAD1 - RF*C + K1
      S2 = RAD2 - RF*C + K2
C
      IF (((S1 .LE. 0) .AND. (S2 .GT. 0)) .OR.
     1    ((S2 .LE. 0) .AND. (S1 .GT. 0))) THEN
         FDSPOT = 0
      ELSE
         FDSPOT = (2D-7) * C * (K2 - K1 + RF*C*DLOG(S2/S1))
      ENDIF
C

      RETURN
      END
C
C
C     ****************************************************************
C
C
      DOUBLE PRECISION FUNCTION CYLBZ(ZZ, R, RAD, CURDEN, LENGTH)
C
C           (* Gives   the  z-component  of  the  magnetic  field *)
C           (* produced  by  a  cylindrical  current sheet, whose *)
C           (* radius  is  RAD, current density is CURDEN, length *)
C           (* is  LENGTH,  and  whose  center  is  at origo. The *)
C           (* central axis of the cylinder is the z-axis.        *)
C           (* The  field  point  in this coordinate system is at *)
C           (* (z=ZZ, r=R).                                       *)
C
      DOUBLE PRECISION ZZ, R, RAD, CURDEN, LENGTH
C
      DOUBLE PRECISION KSI1, KSI2, R1, R2, K1, K2, C, DELK, ELLIP3
C           (* KSI1, KSI2 : integration   starting  and  stopping *)
C           (*              points                                *)
C           (* R1, R2, K1, K2, C : defined in formulae below      *)
C           (* DELK : complete  elliptic  integral  of  1st  kind *)
C           (*        (IMSL)                                      *)
C           (* ELLIP3 : complete elliptic integral of 3rd kind    *)
C                  ...............................
C
      KSI1 = ZZ - LENGTH/2D0
      KSI2 = ZZ + LENGTH/2D0
      R1 = (RAD+R)*(RAD+R) + KSI1*KSI1
      R2 = (RAD+R)*(RAD+R) + KSI2*KSI2
      K1 = 4D0*RAD*R/R1
      K2 = 4D0*RAD*R/R2
      C = 4D0*RAD*R/((RAD+R)*(RAD+R))
C
      CYLBZ = ((ELLIP3(C,K2)*(RAD-R) + DELK(K2)*(RAD+R))*KSI2*DSQRT(K2)
     1       - (ELLIP3(C,K1)*(RAD-R) + DELK(K1)*(RAD+R))*KSI1*DSQRT(K1))
     2      * (1D-7)*CURDEN*DSQRT(C)/(2*R*RAD)
C
      RETURN
      END
C
C
C     ****************************************************************
C
C
      DOUBLE PRECISION FUNCTION CYLBR(ZZ, R, RAD, CURDEN, LENGTH)
C
C           (* As  FUNCTION CYLBZ, but this gives the r-component *)
C           (* of the field.                                      *)
C
      DOUBLE PRECISION ZZ, R, RAD, CURDEN, LENGTH
C
      DOUBLE PRECISION KSI1, KSI2, R1, R2, K1, K2, DELK, DELE
C           (* KSI1, KSI2 : integration   starting  and  stopping *)
C           (*              points                                *)
C           (* R1, R2, K1, K2 : defined in formulae below         *)
C           (* DELK : complete  elliptic  integral  of  1st  kind *)
C           (*        (IMSL)                                      *)
C           (* DELE : complete  elliptic  integral  of  2nd  kind *)
C           (*        (IMSL)                                      *)
C                  ...............................
C
      KSI1 = ZZ - LENGTH/2D0
      KSI2 = ZZ + LENGTH/2D0
      R1 = (RAD+R)*(RAD+R) + KSI1*KSI1
      R2 = (RAD+R)*(RAD+R) + KSI2*KSI2
      K1 = 4D0*RAD*R/R1
      K2 = 4D0*RAD*R/R2
C
      CYLBR = ((2D0*DELE(K2)-(2D0-K2)*DELK(K2))*DSQRT(R2) -
     1         (2D0*DELE(K1)-(2D0-K1)*DELK(K1))*DSQRT(R1)) *
     2        (1D-7)*CURDEN/R
C
      RETURN
      END
C
C
C     ****************************************************************
C
C
      DOUBLE PRECISION FUNCTION CYLBAX(ZZ, RAD, CURDEN, LENGTH)
C
C           (* Gives  the  z-component  of  the magnetic field on *)
C           (* the  z-axis,  produced  by  a  cylindrical current *)
C           (* sheet,  whose  radius  is  RAD, current density is *)
C           (* CURDEN,  and length is LENGTH. The central axis of *)
C           (* the  cylinder is the z-axis, and it is centered at *)
C           (* z=0.  The field point in this coordinate system is *)
C           (* at (z=ZZ, r=0).                                    *)
C
      DOUBLE PRECISION ZZ, RAD, CURDEN, LENGTH
C
      DOUBLE PRECISION DCONST, PI
C           (* DCONST : an IMSL-function for scientific constants *)
C           (* PI : 3.14159....                                   *)
C                  ...............................
C
      PI = DCONST('PI')
      CYLBAX = 2D0*PI*(1D-7)*CURDEN *
     1         ((ZZ+LENGTH/2D0) /
     2               DSQRT((ZZ+LENGTH/2D0)*(ZZ+LENGTH/2D0) + RAD**2) -
     3          (ZZ-LENGTH/2D0) /
     4               DSQRT((ZZ-LENGTH/2D0)*(ZZ-LENGTH/2D0) + RAD**2))
C
      RETURN
      END
C
C
C     ****************************************************************
C
C
      DOUBLE PRECISION FUNCTION DSKBZ(ZZ, RFIELD, RSOUR, CURDEN, WIDTH)
C
C           (* Calculates  the  z-component of the magnetic field *)
C           (* produced  by  a current disk located perpendicular *)
C           (* to  the  z-axis  and  centered to origo. The outer *)
C           (* radius  of the disk is RSOUR+WIDTH/2, and it has a *)
C           (* hole of radius RSOUR-WIDTH/2 in the center.        *)
C           (* The  field  point  in this coordinate system is at *)
C           (* (z=ZZ, r=RFIELD).                                  *)
C
      DOUBLE PRECISION WIDTH, RSOUR, RFIELD, ZZ, CURDEN
C
      DOUBLE PRECISION KSIF, RF, RSOUF, WF, ERRABS, ERREL, ERREST,
     1                 PI, RAD1, RAD2, RESULT, DCONST,FDSKBZ
      COMMON /AREA/ KSIF, RF, RSOUF, WF
      EXTERNAL FDSKBZ
C           (* KSIF, RF, RSOUF, WF : new  names  for  ZZ, RFIELD, *)
C           (*                       RSOUR  and WIDTH ; needed in *)
C           (*                       FUNCTION FDSKBZ              *)
C           (* ERRABS, ERREL, ERREST : required   and   estimated *)
C           (*                         integration accuracies     *)
C           (* PI : 3.14159....                                   *)
C           (* RAD1, RAD2 : the  inner  and  outer  radius of the *)
C           (*              current disk, respectively            *)
C           (* RESULT : the  result  of  integration  of FUNCTION *)
C           (*          FDSKBZ(X) FROM 0 TO PI                    *)
C           (* DCONST : an IMSL-function for scientific constants *)
C           (* FDSKBZ : the function which is integrated here     *)
C                  ...............................
C
      PI = DCONST('PI')
C
      IF (RFIELD .EQ. 0) THEN
C           (* if the field point is on central axis *)
         RAD1 = RSOUR - WIDTH/2D0
         RAD2 = RSOUR + WIDTH/2D0
         DSKBZ = (RAD1/DSQRT(RAD1*RAD1+ZZ*ZZ) -
     1            RAD2/DSQRT(RAD2*RAD2+ZZ*ZZ) +
     2            DLOG((DSQRT(RAD2*RAD2+ZZ*ZZ)+RAD2)/
     3                 (DSQRT(RAD1*RAD1+ZZ*ZZ)+RAD1))) *
     4           PI*2D-7*CURDEN
C
      ELSE
C           (* if the field point not on central axis *)
         ERRABS = 0D0
         ERREL = 1.0D-8
         KSIF = ZZ
         RF = RFIELD
         RSOUF = RSOUR
         WF = WIDTH
         CALL = DQDAGS(FDSKBZ, 0D0, PI, ERRABS, ERREL, RESULT, ERREST)
C              (* integrate FUNCTION FDSKBZ(X) from 0 to PI *)
         DSKBZ = RESULT*CURDEN
      ENDIF
C
      RETURN
      END
C
C
C     ****************************************************************
C
C
      DOUBLE PRECISION FUNCTION DSKBR(ZZ, RFIELD, RSOUR, CURDEN, WIDTH)
C
C           (* As  FUNCTION DSKBZ, but this gives the r-component *)
C           (* of the field.                                      *)
C
      DOUBLE PRECISION WIDTH, RSOUR, RFIELD, ZZ, CURDEN
C
      DOUBLE PRECISION KSIF, RF, RSOUF, WF, ERRABS, ERREL, ERREST,
     1                 PI, RESULT, DCONST, FDSKBR
      COMMON /AREA/ KSIF, RF, RSOUF, WF
      EXTERNAL FDSKBR
C           (* KSIF, RF, RSOUF, WF : new  names  for  ZZ, RFIELD, *)
C           (*                       RSOUR  and WIDTH ; needed in *)
C           (*                       FUNCTION FDSKBR              *)
C           (* ERRABS, ERREL, ERREST : required   and   estimated *)
C           (*                         integration accuracies     *)
C           (* PI : 3.14159....                                   *)
C           (* RESULT : the  result  of  integration  of FUNCTION *)
C           (*          FDSKBR(X) FROM 0 TO PI                    *)
C           (* DCONST : an IMSL-function for scientific constants *)
C           (* FDSKBR : the function which is integrated here     *)
C                  ...............................
C
      PI = DCONST('PI')
C
      IF (RFIELD .EQ. 0) THEN
C           (* if the field point is on central axis *)
         DSKBR = 0
C
      ELSE
C           (* if the field point not on central axis *)
         ERRABS = 0D0
         ERREL = 1.0D-8
         KSIF = ZZ
         RF = RFIELD
         RSOUF = RSOUR
         WF = WIDTH
         CALL = DQDAGS(FDSKBR, 0D0, PI, ERRABS, ERREL, RESULT, ERREST)
C              (* integrate FUNCTION FDSKBR(X) from 0 to PI *)
         DSKBR = RESULT*CURDEN
      ENDIF
C
      RETURN
      END
C
C
C     ****************************************************************
C
C
      DOUBLE PRECISION FUNCTION CYLSOU(KSI, R, RAD)
C
C           (* (CYLSOU(KSI2, ...)-CYLSOU(KSI1, ...))*CURDEN gives *)
C           (* the  vector  potential  produced  by a cylindrical *)
C           (* current   sheet,  whose  radius  is  RAD,  current *)
C           (* density  is  CURDEN, and whose center is at origo. *)
C           (* The central axis of the cylinder is the z-axis.    *)
C           (* The field point is as defined in FUNCTION CYLPOT.  *)
C
      DOUBLE PRECISION KSI, R, RAD
C
      DOUBLE PRECISION R1, K, C, DELK, DELE, ELLIP3, DMACH
C           (* R1, K, C : defined in formulae below               *)
C           (* DELK : complete elliptic integral of 1st kind      *)
C           (*        (IMSL)                                      *)
C           (* DELE : complete elliptic integral of 2nd kind      *)
C           (*        (IMSL)                                      *)
C           (* ELLIP3 : complete elliptic integral of 3rd kind    *)
C           (* DMACH(1) : the  smallest  positive  number  of the *)
C           (*            computer (IMSL)                         *)
C                  ...............................
C
      R1 = (RAD+R)*(RAD+R) + KSI*KSI
      K = 4D0*RAD*R/R1
      C = 4D0*RAD*R/((RAD+R)*(RAD+R))
C
      IF ((1D0-C) .GE. (5*DMACH(1))**(1D0/3D0)) THEN
C           (* if C<>1 *)
         CYLSOU = (DELK(K)*(R1+(RAD-R)*(RAD-R)) -
     1              ELLIP3(C,K)*(RAD-R)*(RAD-R) - DELE(K)*R1)
     2            * (1D-7)*KSI/(R*DSQRT(R1))
C
      ELSE
         IF ((1D0-K) .GE. (10*DMACH(1))) THEN
C              (* if C=1 and K<>1 *)
            CYLSOU = (DELK(K)-DELE(K)) * (1D-7)*KSI*DSQRT(R1)/R
C
         ELSE
C              (* if C=K=1 *)
            CYLSOU = 0D0
         ENDIF
      ENDIF
C
      RETURN
      END
C
C
C     ****************************************************************
C
C
      DOUBLE PRECISION FUNCTION CYLPOT(HEIGHT, RSOUR, RFIELD, ZZ)
C
C           (* Calculates  the  vector potential per unit current *)
C           (* density,  produced  by a cylindrical current sheet *)
C           (* whose  radius  is  RSOUR and length is HEIGHT, and *)
C           (* which  is  centered  at origo. The central axis of *)
C           (* the cylider is the z-axis.                         *)
C           (* The  field  point  in this coordinate system is at *)
C           (* (z=ZZ, r=RFIELD).                                  *)
C
      DOUBLE PRECISION HEIGHT, RSOUR, RFIELD, ZZ
C
      DOUBLE PRECISION KSI1, KSI2, CYLSOU
C           (* KSI1, KSI2, CYLSOU : CYLPOT = CYLSOU(KSI2, ....) - *)
C           (*                               CYLSOU(KSI1, ....)   *)
C                  ...............................
C
      KSI1 = ZZ - HEIGHT/2D0
      KSI2 = ZZ + HEIGHT/2D0
C
      CYLPOT = CYLSOU(KSI2, RFIELD, RSOUR) - CYLSOU(KSI1, RFIELD, RSOUR)
C
      RETURN
      END
C
C
C     ****************************************************************
C
C
      DOUBLE PRECISION FUNCTION DSKPOT(WIDTH, RSOUR, RFIELD, ZZ)
C
C           (* Calculates  the  vector potential per unit current *)
C           (* density,   produced  by  a  current  disk  located *)
C           (* perpendicular   to  the  z-axis  and  centered  to *)
C           (* origo.  The  outer  radius  of  the disk is RSOUR+ *)
C           (* WIDTH/2, and it has a hole of radius RSOUR-WIDTH/2 *)
C           (* in the center.                                     *)
C           (* The  field  point  in this coordinate system is at *)
C           (* (z=ZZ, r=RFIELD).                                  *)
C
      DOUBLE PRECISION WIDTH, RSOUR, RFIELD, ZZ
C
      DOUBLE PRECISION KSIF, RF, RSOUF, WF, ERRABS, ERREL, ERREST,
     1                 PI, RESULT, DCONST, FDSPOT
      COMMON /AREA/ KSIF, RF, RSOUF, WF
      EXTERNAL FDSPOT
C           (* KSIF, RF, RSOUF, WF : new  names  for  ZZ, RFIELD, *)
C           (*                       RSOUR  and WIDTH ; needed in *)
C           (*                       FUNCTION FDSPOT              *)
C           (* ERRABS, ERREL, ERREST : required   and   estimated *)
C           (*                         integration accuracies     *)
C           (* PI : 3.14159....                                   *)
C           (* RESULT : the  result  of  integration  of FUNCTION *)
C           (*          FDSPOT(X) from 0 to PI                    *)
C           (* DCONST : an IMSL-function for scientific constants *)
C           (* FDSPOT : the function which is integrated here     *)
C                  ...............................
C
      PI = DCONST('PI')
C
      ERRABS = 0D0
      ERREL = 1.0D-8
      KSIF = ZZ
      RF = RFIELD
      RSOUF = RSOUR
      WF = WIDTH
      CALL = DQDAGS(FDSPOT, 0D0, PI, ERRABS, ERREL, RESULT, ERREST)
C           (* integrate FUNCTION FDSPOT(X) from 0 to PI *)
      DSKPOT = RESULT
C
      RETURN
      END
C
C
C     ****************************************************************
C
C
      SUBROUTINE SOLCYL(LENSHI, RADSHI, CENSHI, LENSOL, RADSOL, CENSOL,
     1                 LAYERS, LOOPS, CUR, WIRDIA, FOILTH, N, ASTOR, NN)
C
C           (* Calculates  the vector potential distribution on a *)
C           (* cylinder  part of the superconducting shield, pro- *)
C           (* duced  by a solenoid coil. The result is stored in *)
C           (* the vector ASTOR(NN).                              *)
C           (*                                                    *)
C           (* The meanings of the input parameters are:          *)
C           (*     LENSOL,  RADSOL,  LAYERS,  LOOPS, CUR, WIRDIA, *)
C           (*        FOILTH : as in SUBROUTINE BSOLEN            *)
C           (*     LENSHI : the length of the shield part         *)
C           (*     RADSHI : the radius of the shield part         *)
C           (*     CENSHI : the z-coordinate of the center of the *)
C           (*              shield part                           *)
C           (*     CENSOL : the z-coordinate of the center of the *)
C           (*              solenoid coil                         *)
C           (*     N : the  amount  of  rings  the shield part is *)
C           (*         divided to                                 *)
C           (*     ASTOR : the  vector  in  which  the  result is *)
C           (*             stored                                 *)
C           (*     NN : dimension of ASTOR                        *)
C
      INTEGER LAYERS, N, NN
      DOUBLE PRECISION LENSHI, RADSHI, LENSOL, RADSOL, CENSOL, LOOPS,
     1                 CUR, WIRDIA, FOILTH, ASTOR(NN),CENSHI
Cbogi CENSHI added to parameter list to prevent from compiler warnings
C
      INTEGER ILAY, J
      DOUBLE PRECISION CURDEN, RADLAY, Z, ZZ, CYLPOT
C           (* ILAY : the layer number                            *)
C           (* J : the  index  of  the current ring of the shield *)
C           (*     part                                           *)
C           (* CURDEN : the current density in the solenoid       *)
C           (* RADLAY : the  radius  of  the  ILAYth layer in the *)
C           (*          solenoid coil                             *)
C           (* Z : the  z-coordinate  of  the Jth current ring of *)
C           (*     the shield cylinder*)
C           (* ZZ : z-component  of the vector from the center of *)
C           (*      the  solenoid  coil  to the center of the Jth *)
C           (*      current ring of the cylinder*)
C           (* CYLPOT : gives  the  vector  potential produced by *)
C           (*          one layer of the solenoid                 *)
C                  ...............................
C
      CURDEN = CUR*LOOPS/LENSOL
      RADLAY = RADSOL + WIRDIA/2D0
C
      DO 2320 ILAY = 1, LAYERS
         DO 2310 J = 1, N
            Z = CENSHI + LENSHI/2D0 - (J-0.5)*LENSHI/N
            ZZ = Z - CENSOL
            ASTOR(J) = ASTOR(J) +
     1                 CYLPOT(LENSOL, RADLAY, RADSHI, ZZ)*CURDEN
 2310    CONTINUE
         RADLAY = RADLAY + WIRDIA + FOILTH
 2320 CONTINUE
C
      RETURN
      END
C
C
C     ****************************************************************
C
C
      SUBROUTINE SOLDSK(ZDSK, RADSHI, RHOLE, LENSOL, RADSOL, CENSOL,
     1                 LAYERS, LOOPS, CUR, WIRDIA, FOILTH, N, ASTOR, NN)
C
C           (* Calculates  the vector potential distribution on a *)
C           (* disk  part of the superconducting shield, produced *)
C           (* by  a  solenoid  coil. The result is stored in the *)
C           (* vector ASTOR(NN).                                  *)
C           (*                                                    *)
C           (* The meanings of the input parameters are:          *)
C           (*     LENSOL,  RADSOL,  LAYERS,  LOOPS, CUR, WIRDIA, *)
C           (*        FOILTH : as in SUBROUTINE BSOLEN            *)
C           (*     ZDSK : the z-coordinate of the shield disk     *)
C           (*     RADSHI : the outer radius of the shield disk   *)
C           (*     RHOLE : the  radius  of the hole in the center *)
C           (*             of the shield disk                     *)
C           (*     CENSOL : the z-coordinate of the center of the *)
C           (*              solenoid coil                         *)
C           (*     N : the  amount  of  rings  the shield disk is *)
C           (*         divided to                                 *)
C           (*     ASTOR : the  vector  in  which  the  result is *)
C           (*             stored                                 *)
C           (*     NN : dimension of ASTOR                        *)
C
      INTEGER LAYERS, N, NN
      DOUBLE PRECISION ZDSK, RADSHI, RHOLE, LENSOL, RADSOL, CENSOL,
     1          LOOPS, CUR, WIRDIA, FOILTH, ASTOR(NN)
C
      INTEGER ILAY, J
      DOUBLE PRECISION CURDEN, RADLAY, ZZ, RFIELD, CYLPOT
C           (* ILAY : the layer number                            *)
C           (* J : the index of the current ring of the disk      *)
C           (* CURDEN : the current density in the solenoid       *)
C           (* RADLAY : the  radius  of  the  ILAYth layer in the *)
C           (*          solenoid coil                             *)
C           (* ZZ : z-component  of the vector from the center of *)
C           (*      the solenoid coil to the shield disk          *)
C           (* RFIELD : r-component of the field point            *)
C           (* CYLPOT : gives  the  vector  potential produced by *)
C           (*          one layer of the solenoid                 *)
C                  ...............................
C
      CURDEN = CUR*LOOPS/LENSOL
      RADLAY = RADSOL + WIRDIA/2D0
C
      DO 2420 ILAY = 1, LAYERS
         DO 2410 J = 1, N
            RFIELD = RHOLE + (J-0.5)*(RADSHI-RHOLE)/N
            ZZ = ZDSK - CENSOL
            ASTOR(J) = ASTOR(J) +
     1                 CYLPOT(LENSOL, RADLAY, RFIELD, ZZ)*CURDEN
 2410    CONTINUE
         RADLAY = RADLAY + WIRDIA + FOILTH
 2420 CONTINUE
C
      RETURN
      END
C
C
C     ****************************************************************
C
C
      SUBROUTINE BSOLEN(ZZ, R, LENSOL, RADSOL, WIRDIA, LOOPS, LAYERS,
     1                  CUR, FOILTH, BZSOL, BRSOL)
C
C           (* Gives  the  magnetic field produced by a solenoid, *)
C           (* whose  central  axis  is  the  z-axis and which is *)
C           (* centered  to  z=0. The field point in this coordi- *)
C           (* nate system is at (z=ZZ, r=R).                     *)
C           (* The meanings of the input parameters are:          *)
C           (*     ZZ : z-coordinate of the field point           *)
C           (*     R : r-coordinate of the field point            *)
C           (*     LENSOL : the length of the solenoid            *)
C           (*     RADSOL : the inner radius of the solenoid      *)
C           (*     WIRDIA : the diameter of the current lead      *)
C           (*     LOOPS : the number of turns in one layer       *)
C           (*     LAYERS : the number of layers in the solenoid  *)
C           (*     CUR : the current in the lead                  *)
C           (*     FOILTH : the  thickness of the insulating foil *)
C           (*              between the layers                    *)
C           (*     BZSOL : the z-component of the magnetic field  *)
C           (*     BRSOL : the r-component of the magnetic field  *)
C
      INTEGER LAYERS
      DOUBLE PRECISION ZZ, R, LENSOL, RADSOL, WIRDIA, LOOPS, CUR,
     1                 FOILTH, BZSOL, BRSOL, CYLBAX
C
      INTEGER I
      DOUBLE PRECISION RADLAY, CURDEN, CYLBZ, CYLBR
C           (* I : the layer number                               *)
C           (* RADLAY : the radius of the Ith layer               *)
C           (* CURDEN : the current density in the solenoid       *)
C           (* CYLBZ, CYLBR : give the z- and r-components of the *)
C           (*                magnetic   field  produced  by  one *)
C           (*                layer of the solenoid               *)
C           (* CYLBAX : gives the magnetic field on central axis  *)
C                  ...............................
C
      RADLAY = RADSOL + WIRDIA/2D0
      CURDEN = LOOPS*CUR/LENSOL
C
      DO 2510 I = 1, LAYERS
C
         IF (R .EQ. 0) THEN
C              (* if the field point is on central axis *)
            BZSOL = BZSOL + CYLBAX(ZZ, RADLAY, CURDEN, LENSOL)
C
         ELSE
C              (* if the field point not on central axis *)
            BZSOL = BZSOL + CYLBZ(ZZ, R, RADLAY, CURDEN, LENSOL)
            BRSOL = BRSOL + CYLBR(ZZ, R, RADLAY, CURDEN, LENSOL)
         ENDIF
C
         RADLAY = RADLAY + WIRDIA + FOILTH
 2510 CONTINUE
C
      RETURN
      END
C
C
C     ****************************************************************
C
C
      SUBROUTINE WRSHI(FILE, MAX, NSHI, LENSHI, RADSHI, RHOLE, CENSHI,
     1                 TRAFLX)
C
C           (* Writes  the dimensions of the shield parts to file *)
C           (* number FILE ; FILE=6 corresponds to the terminal.  *)
C
      INTEGER FILE, MAX, NSHI
      DOUBLE PRECISION LENSHI(MAX), RADSHI(MAX), RHOLE(MAX),
     1                 CENSHI(MAX), TRAFLX(MAX)
C
      INTEGER ISHI
C           (* ISHI : an index for the shield parts               *)
C                  ...............................
C
      IF (NSHI .EQ. 0) THEN
C           (* if no shield *)
         WRITE(FILE,2610)
 2610    FORMAT('Now you have no shield at all.')
C
      ELSE
C           (* if there is a shield *)
         DO 2680 ISHI = 1, NSHI
            IF (LENSHI(ISHI) .EQ. 0) WRITE(FILE,2620) ISHI
 2620       FORMAT('Shield part number ',I2,' is a disk, whose',
     1             ' dimensions are: ')
            IF (LENSHI(ISHI) .NE. 0) WRITE(FILE,2630) ISHI
 2630       FORMAT('Shield part number ',I2,' is a cylinder,',
     1              'whose dimensions are: ')
C
            IF (LENSHI(ISHI) .NE. 0) WRITE(FILE,2640) ISHI,
     1                                               LENSHI(ISHI)*1000
C                 (* length of cylindrical shield part *)
 2640       FORMAT(' LENSHI(',I2,') = ',F10.3,' mm')
C
            WRITE(FILE,2650) ISHI, RADSHI(ISHI)*1000
C                 (* radius of cylindrical and outer radius of *)
C                 (* planar shield part                        *)
 2650       FORMAT(' RADSHI(',I2,') = ',F10.3,' mm')
C
            IF (LENSHI(ISHI) .EQ. 0) WRITE(FILE,2660) ISHI,
     1                                               RHOLE(ISHI)*1000
C                 (* radius of the hole in the planar shield part *)
 2660       FORMAT(' RHOLE(',I2,') = ',F10.3,' mm')
C
            WRITE(FILE,2670) ISHI, CENSHI(ISHI)*1000
C                 (* z-coordinate of the center of the shield part *)
 2670       FORMAT(' CENSHI(',I2,') = ',F10.3,' mm')
C
            IF (LENSHI(ISHI) .NE. 0) THEN
               WRITE(FILE,2675) ISHI, TRAFLX(ISHI)*1D8
 2675          FORMAT(' TRAFLX(',I2,') = ',G9.4,' Gauss*cm*cm')
            ENDIF

 2680    CONTINUE
      ENDIF
C
      RETURN
      END
C
C
C     ****************************************************************
C
C
      SUBROUTINE WROTH(FILE, MAX, LENSOL, RADSOL, CENSOL, LAYERS, LOOPS,
     1                 SYMSOL, CUR, WIRDIA, FOILTH, BEXT, TRAFLX, BCNTR)
C
C           (* Writes  the  dimensions  of the solenoid coils and *)
C           (* the  other  parameters  of  the system, except the *)
C           (* dimensions  of  the  shield  parts, to file number *)
C           (* FILE ; FILE=6 corresponds to the terminal.         *)
C
Cjl   Why writing no shield-parts ?
Cjl   Aber lass und erst mal den eingeschlossenen Fluss jeden Schildes
Cjl   ausgeben
C
      INTEGER FILE, MAX, LAYERS(MAX)
      DOUBLE PRECISION LENSOL(MAX), RADSOL(MAX), CENSOL(MAX),
     1                LOOPS(MAX), CUR(MAX), WIRDIA, FOILTH, BEXT,TRAFLX,
     2                BCNTR
      LOGICAL SYMSOL(MAX)
C
      INTEGER ISOL
C           (* ISOL : an index for the solenoid parts             *)
C                  ...............................
C
      WRITE(FILE,2709)
 2709 FORMAT ('Field value in the Origin: ',G16.10,'Gauss' /)
      IF (LENSOL(1) .NE. 0) THEN
C           (* if there are solenoids *)
         ISOL = 1
C
 2710    CONTINUE
            WRITE(FILE,2720) ISOL, LENSOL(ISOL)*1000, ISOL,
     1                 RADSOL(ISOL)*1000, ISOL, CENSOL(ISOL)*1000, ISOL,
     2                 LAYERS(ISOL), ISOL, LOOPS(ISOL), ISOL,
     3                 SYMSOL(ISOL), ISOL, CUR(ISOL)
C                 (* length,  inner  radius, z-coordinate of the *)
C                 (* center, number of layers, number of current *)
C                 (* loops in one layer, is there a similar part *)
C                 (* located symmetrically, current in the wire  *)
 2720       FORMAT(/,
     1             ' LENSOL(',I2,') = ',F10.3,' mm' /,
     2             ' RADSOL(',I2,') = ',F10.3,' mm' /,
     3             ' CENSOL(',I2,') = ',F10.3,' mm' /,
     4             ' LAYERS(',I2,') = ',I10 /,
     5             ' LOOPS (',I2,') = ',F10.3 /,
     6             ' SYMSOL(',I2,') = ',L10 /,
     7             ' CUR   (',I2,') = ',F10.3,' A')
            ISOL = ISOL + 1
            IF (LENSOL(ISOL) .NE. 0) GOTO 2710
C                 (* if there are more solenoids *)
         CONTINUE
C
         WRITE(FILE,2730) WIRDIA*1000
C              (* the diameter of the wire *)
 2730    FORMAT(/' WIRDIA = ',F14.4,' mm')
C
         WRITE(FILE,2740) FOILTH*1000
C              (* the thickness of insulating foil between layers *)
 2740    FORMAT(' FOILTH = ',F14.4,' mm')
C
      ELSE
C           (* if no solenoids *)
         WRITE(FILE,2750)
 2750    FORMAT(''/'Now you have no solenoids. ')
      ENDIF
C
      IF (BEXT .NE. 0) THEN
C           (* if there is external field *)
         WRITE(FILE,2760) BEXT*10000
C              (* external field in Gausses *)
 2760    FORMAT(''/' BEXT = ', G9.4, ' Gauss ')
      ELSE
C           (* if no external field *)
         WRITE(FILE,2770)
 2770    FORMAT(''/' BEXT = 0.0 Gauss ')
      ENDIF
C
Cjl  !!! Just remove the Cjl's
Cjl
Cjl      IF (TRAFLU .NE. 0) THEN
C           (* if there is trapped flux in the shield *)
Cjl         WRITE(FILE,2780) TRAFLU*1D8
C              (* trapped flux in the shield in Gauss*cm*cm *)
Cjl 2780    FORMAT(' FLUX = ', G9.4,' Gauss*cm*cm')
Cjl      ELSE
C           (* if no trapped flux *)
Cjl         WRITE(FILE,2790)
Cjl 2790    FORMAT(' FLUX = 0.0 Gauss*cm*cm')
Cjl      ENDIF
C
      RETURN
      END
C
C
C     ****************************************************************
C
C
      SUBROUTINE DIMENS(NSHI, DIFSHI, LENSHI, RADSHI, RHOLE, CENSHI,
     1             SYMSHI, LENSOL, RADSOL, CENSOL, LAYERS, LOOPS,
     2             SYMSOL, CUR, WIRDIA, FOILTH, BEXT, TRAFLX, MAX)
C
C           (* Asks the dimensions, currents etc. of the system.  *)
C
      INTEGER MAX, NSHI, LAYERS(MAX)
      DOUBLE PRECISION LENSHI(MAX), RADSHI(MAX), RHOLE(MAX),
     1               TRAFLX(MAX),
     2               CENSHI(MAX), LENSOL(MAX), RADSOL(MAX), CENSOL(MAX),
     3               LOOPS(MAX), CUR(MAX), WIRDIA, FOILTH, BEXT
      LOGICAL DIFSHI, SYMSHI(MAX), SYMSOL(MAX)
C
      INTEGER ISHI, ISOL
      LOGICAL HOLE
      CHARACTER*1 ANSW
C           (* ISHI : index of the shield part                    *)
C           (* ISOL : index of the solenoid                       *)
C           (* HOLE : .TRUE. if there is a hole through the whole *)
C           (*        shield,  so  that  there  can  be a trapped *)
C           (*        flux ; .FALSE. otherwise                    *)
C                  ...............................
C
      PRINT 2801
 2801 FORMAT('This  program  calculates  magnetic  fields  produced',
     1                                            ' by  a system of'/,
     2        'cocentric coils in a superconducting shield. The',
     3                                        ' shield must consist'/,
     4        'of cocentric cylinders and circular disks. There can ',
     5                                            'be a hole in the'/,
     6        ' center of each disk.'/,
     7        'All dimensions are given in mm:s|')
C
 2802 CONTINUE
C
      PRINT *, ('  ')
      CALL WRSHI(6, MAX, NSHI, LENSHI, RADSHI, RHOLE, CENSHI, TRAFLX)
C
      PRINT 2803
 2803 FORMAT('Do you want to change the shield (Y = yes, N = no)?')
C
 2804 CONTINUE
         READ(*, '(A1)', ERR=2804, END=2804) ANSW
         IF ((ANSW .NE. 'N') .AND. (ANSW .NE. 'Y') .AND.
     1       (ANSW .NE. 'n') .AND. (ANSW .NE. 'y')) PRINT 2805
 2805    FORMAT(' Give either "Y" or "N"')
         IF ((ANSW .NE. 'N') .AND. (ANSW .NE. 'Y') .AND.
     1       (ANSW .NE. 'n') .AND. (ANSW .NE. 'y')) GOTO 2804
C              (* repeat asking until the answer is acceptable *)
      CONTINUE
C
      IF ((ANSW .EQ. 'Y') .OR. (ANSW .EQ. 'y')) THEN
C           (* new shield *)
         DIFSHI = .TRUE.
C
         ISHI = 1
C
         PRINT 2806
 2806    FORMAT('Have you any superconducting shield ',
     1          '(Y = yes, N = no)?')
C
 2807    CONTINUE
            READ(*, '(A1)', ERR=2807, END=2807) ANSW
            IF ( (ANSW .NE. 'N') .AND. (ANSW .NE. 'n') .AND.
     1           (ANSW .NE. 'Y') .AND. (ANSW .NE. 'y')) PRINT 2808

 2808       FORMAT(' Give either "Y" or "N"')
            IF ((ANSW .NE. 'N') .AND. (ANSW .NE. 'n') .AND.
     1          (ANSW .NE. 'Y') .AND. (ANSW .NE. 'y')) GOTO 2807
         CONTINUE
C
         IF ((ANSW .EQ. 'N') .OR. (ANSW .EQ. 'n')) GOTO 2827
C              (* if no shield at all *)
C
 2809    CONTINUE
 2810       CONTINUE
            PRINT 2811, ISHI, ISHI, LENSHI(ISHI)*1000
 2811       FORMAT('Give the length of the shield part number ',I2,
     1             ' (LENSHI(',I2,') = ',F6.2,' mm). ' / ' If the',
     2             ' part is a disk, give 0. ')
            READ(*, *, ERR=2810, END=2812) LENSHI(ISHI)
C                 (* REWIND 5 & END=2812  =>  if only Enter is given, *)
C                 (* the value of LENSHI will not be changed          *)
            LENSHI(ISHI) = LENSHI(ISHI)/1000D0
C                 (* change the dimension from mm:s to m:s *)
C
 2812       CONTINUE
C
            PRINT 2813, ISHI, ISHI, RADSHI(ISHI)*1000
 2813       FORMAT('Give the radius of the shield part number ',I2,
     1             ' (RADSHI(',I2,') = ',F6.2,' mm).' / ' If the',
     2             ' part is a disk, give its outer radius. ')
            READ(*, *, ERR=2812, END=2814) RADSHI(ISHI)
            RADSHI(ISHI) = RADSHI(ISHI)/1000D0
C
 2814       CONTINUE
C
            IF (LENSHI(ISHI) .EQ. 0) THEN
C                 (* if the shield part is a disk *)
 2815          CONTINUE
               PRINT 2816, ISHI, RHOLE(ISHI)*1000
 2816          FORMAT('Give the radius of the hole in the disk',
     1                ' (RHOLE(',I2,') = ', F6.2, ' mm). ')
               READ(*, *, ERR=2815, END=2817) RHOLE(ISHI)
               RHOLE(ISHI) = RHOLE(ISHI)/1000D0
 2817          CONTINUE
            ENDIF
C
 2818       CONTINUE
C
            PRINT 2819, ISHI, ISHI, CENSHI(ISHI)*1000
 2819       FORMAT('Give the z-coordinate of the center of the ',
     1             'shield part number ',I2 / ' (CENSHI(',I2,') = ',
     2             F6.2,' mm). ')
            READ(*, *, ERR=2818, END=2820) CENSHI(ISHI)
            CENSHI(ISHI) = CENSHI(ISHI)/1000D0
C
 2820       CONTINUE
C
Cjl
Cjl         (* Here the trapped flux is given for a cylindrical shield *)
Cjl

C           (* check whether there is a hole through the shield *)
            HOLE = ( ((LENSHI(ISHI) .NE. 0) .OR. (RHOLE(ISHI) .NE. 0)))
Cjl
            IF (HOLE) THEN
Cjl            (* if there is a hole through the shield *)
 6695          CONTINUE
Cjl
               PRINT 6669, ISHI
 6669          FORMAT('Give the flux trapped in the center of the ',
     1                'shield part number ',I2 /
     2                'in Gausses per square centimeter' )
Cjl
               IF (TRAFLX(ISHI) .EQ. 0) THEN
                 PRINT 2858, ISHI
 2858            FORMAT (' (TRAFLX(',I2,') = 0.000 Gauss*cm*cm).')
               ELSE
                 PRINT 6696, ISHI, TRAFLX(ISHI)*1D8
 6696            FORMAT(' (TRAFLX(',I2,') = ',G9.4,' Gauss*cm*cm) ')
               ENDIF
               READ(*, *, ERR=6695, END=6697) TRAFLX(ISHI)
               TRAFLX(ISHI) = TRAFLX(ISHI)*1D-8
 6697          CONTINUE
Cjl
            ELSE
Cjl           (* if there is no hole through the shield *)
              TRAFLX(ISHI) = 0D0
            ENDIF
Cjl
Cjl
C
            IF (((ABS(CENSHI(ISHI)) - LENSHI(ISHI)/2) .GE. 0) .AND.
     1          (CENSHI(ISHI) .NE. 0)) THEN
C                 (* if there is space for a symmetrical shield part *)
 2821          CONTINUE
                  PRINT 2822, -CENSHI(ISHI)*1000, ISHI, SYMSHI(ISHI)
 2822             FORMAT('Is there a similar part centered to z = ',
     1                   F6.2, ' mm (Y/N)?' / ' If there is one, ',
     2                   'don''t give its dimensions any more|' /
     3                   ' Now SYMSHI(', I2, ') = ', L2, '. ')

                  READ(*, '(A1)', ERR=2821, END=2823) ANSW
                  IF ((ANSW .NE. 'Y') .AND. (ANSW .NE. 'y') .AND.
     1                (ANSW .NE. 'N') .AND. (ANSW .NE. 'n')) GOTO 2821
               CONTINUE
               SYMSHI(ISHI) = ((ANSW .EQ. 'Y')  .OR. (ANSW .EQ. 'y'))
 2823          CONTINUE
            ELSE
C                 (* if no space for a symmetrical part *)
               SYMSHI(ISHI) = .FALSE.
            ENDIF
C
            IF (SYMSHI(ISHI)) THEN
C                 (* the index for symmetrical part will be ISHI+1 *)
               LENSHI(ISHI+1) = LENSHI(ISHI)
               RADSHI(ISHI+1) = RADSHI(ISHI)
               RHOLE(ISHI+1) = RHOLE(ISHI)
               CENSHI(ISHI+1) = -CENSHI(ISHI)
            ENDIF
C
            IF (.NOT. SYMSHI(ISHI)) THEN
               ISHI = ISHI + 1
            ELSE
               ISHI = ISHI + 2
            ENDIF
C
            PRINT 2824
 2824       FORMAT('Are there any more shield parts ',
     1             '(Y = yes, N = no)?')
C
 2825       CONTINUE
               READ(*, '(A1)', ERR=2825, END=2825) ANSW
               IF ((ANSW .NE. 'N') .AND. (ANSW .NE. 'n') .AND.
     1             (ANSW .NE. 'Y') .AND. (ANSW .NE. 'y')) PRINT 2826
 2826          FORMAT(' Give either "Y" or "N"')
C
               IF ((ANSW .NE. 'N') .AND. (ANSW .NE. 'n') .AND.
     1             (ANSW .NE. 'Y') .AND. (ANSW .NE. 'y')) GOTO 2825
            CONTINUE
            IF ((ANSW .EQ. 'Y') .OR. (ANSW .EQ. 'y')) GOTO 2809
C              (* if there are more shield parts *)
         CONTINUE
C
 2827    CONTINUE
C
         NSHI = ISHI - 1
C              (* now the value of NSHI will be the actual number *)
C              (* of shield parts                                 *)
C
      ELSE
C           (* if the shield was not changed *)
         DIFSHI = .FALSE.
      ENDIF
C
      ISOL = 1
C
 2828 CONTINUE
         PRINT 2829, ISOL, ISOL, LENSOL(ISOL)*1000
 2829    FORMAT('Give the length of the solenoid number ', I2,
     1          ' (LENSOL(', I2, ') = ', F6.2, ' mm). ' / ' If no ',
     2          'more solenoids, give 0. ')
         READ(*, *, ERR=2828, END=2830) LENSOL(ISOL)
C              (* REWIND 5 & END=2830  =>  if only Enter is given, *)
C              (* the value of LENSOL will not be changed          *)
         LENSOL(ISOL) = LENSOL(ISOL)/1000D0
C                 (* change the dimension from mm:s to m:s *)
 2830    CONTINUE
C
         IF (LENSOL(ISOL) .EQ. 0) GOTO 2845
C              (* if no more solenoids *)
C
         PRINT 2831, ISOL, ISOL, RADSOL(ISOL)*1000
 2831    FORMAT('Give the inner radius of the solenoid number ',I2,
     1          ' (RADSOL(',I2,') = ', F6.2, ' mm). ')
         READ(*, *, ERR=2830, END=2832) RADSOL(ISOL)
         RADSOL(ISOL) = RADSOL(ISOL)/1000D0
C
 2832    CONTINUE
C
         PRINT 2833, ISOL, ISOL, CENSOL(ISOL)*1000
 2833    FORMAT('Give the center of the solenoid number ',I2,
     1          ' (CENSOL(',I2,') = ', F6.2, ' mm). ')
         READ(*, *, ERR=2832, END=2834) CENSOL(ISOL)
         CENSOL(ISOL) = CENSOL(ISOL)/1000D0
C
 2834    CONTINUE
C
         PRINT 2835, ISOL, ISOL, LAYERS(ISOL)
 2835    FORMAT('Give the number of layers in the solenoid ',
     1          'number ',I2,' (LAYERS(',I2,') = ', I2, '). ')
         READ(*, *, ERR=2834, END=2836) LAYERS(ISOL)
C
 2836    CONTINUE
C
         PRINT 2837, ISOL, ISOL, LOOPS(ISOL)
 2837    FORMAT('Give the number of turns per layer in the solenoid',
     1          ' number ', I2 / ' (LOOPS(', I2, ') = ', F6.2, '). ')
         READ(*, *, ERR=2836, END=2838) LOOPS(ISOL)
C
 2838    CONTINUE
C
 2839    CONTINUE
         PRINT 2840, ISOL, ISOL, CUR(ISOL)
 2840    FORMAT('Give the current in the solenoid number ', I2,
     1          ' in Amps (CUR(', I2, ') = ', F6.3, '). ')
         READ(*, *, ERR=2839, END=2841) CUR(ISOL)
C
 2841    CONTINUE
C
         IF ((ABS(CENSOL(ISOL)) - LENSOL(ISOL)/2) .GE. 0) THEN
C              (* if there is space for a symmetrical solenoid *)
 2842       CONTINUE
               PRINT 2843, -CENSOL(ISOL)*1000, ISOL, SYMSOL(ISOL)
 2843          FORMAT('Is there a similar solenoid with the same ',
     1              'current centered to z = ', F6.2, ' mm' / ' (Y/N)?',
     2               ' If there is one, don''t give its dimensions ',
     3               'any more|' / ' Now SYMSOL(', I2, ') = ', L2, '. ')
               READ(*, '(A1)', ERR=2842, END=2844) ANSW
               IF ((ANSW .NE. 'Y') .AND. (ANSW .NE. 'N') .AND.
     1             (ANSW .NE. 'y') .AND. (ANSW .NE. 'n')) GOTO 2842
            CONTINUE
            SYMSOL(ISOL) = ((ANSW .EQ. 'Y')  .OR. (ANSW .EQ. 'y'))
 2844       CONTINUE
C
         ELSE
C              (* if no space for a symmetrical solenoid *)
            SYMSOL(ISOL) = .FALSE.
         ENDIF
C
         IF (SYMSOL(ISOL)) THEN
C              (* the index for symmetrical coil will be ISOL+1 *)
            LENSOL(ISOL+1) = LENSOL(ISOL)
            RADSOL(ISOL+1) = RADSOL(ISOL)
            CENSOL(ISOL+1) = -CENSOL(ISOL)
            LAYERS(ISOL+1) = LAYERS(ISOL)
            LOOPS(ISOL+1) = LOOPS(ISOL)
            SYMSOL(ISOL+1) = SYMSOL(ISOL)
            CUR(ISOL+1) = CUR(ISOL)
         ENDIF
C
         IF (.NOT. SYMSOL(ISOL)) THEN
            ISOL = ISOL + 1
         ELSE
            ISOL = ISOL + 2
         ENDIF
C
         GOTO 2828
C
 2845 CONTINUE
C
      IF (LENSOL(1) .NE. 0) THEN
C           (* if there are solenoids *)
 2846    CONTINUE
C
         PRINT 2847, WIRDIA*1000
 2847    FORMAT('Give the diameter of the wire (WIRDIA = ', F7.4,
     1          ' mm). ')
         READ(*, *, ERR=2846, END=2848) WIRDIA
         WIRDIA = WIRDIA/1000D0
C
 2848    CONTINUE
C
 2849    CONTINUE
         PRINT 2850, FOILTH*1000
 2850    FORMAT('Give the thickness of the foil between the layers ',
     1          '(FOILTH) = ', F7.4, ' mm). ' / ' If there is no foil ',
     2          'and you wind layers directly on top of each other,' /
     3          ' putting the wire onto the notch between adjacent ',
     4          'turns in the previous' / ' layer, give -0.134 times ',
     5          'the thickness of the wire.')
         READ(*, *, ERR=2849, END=2851) FOILTH
         FOILTH = FOILTH/1000D0
 2851    CONTINUE
C
      ENDIF
C
 2852 CONTINUE
      IF (BEXT .EQ. 0) THEN
         PRINT 2853
 2853    FORMAT ('Give the external field in Gausses (BEXT = 0.000 G).'
     1            / ' The field must be parallel to the axis of ',
     2           'the shield. ')
      ELSE
         PRINT 2854, BEXT
 2854    FORMAT ('Give the external field in Gausses (BEXT = ', G6.4,
     1           ' G. ' / ' The field must be parallel to the axis of ',
     2           'the shield. ')
      ENDIF
      READ(*, *, ERR=2852, END=2855) BEXT
      BEXT = BEXT/10000D0
C
 2855 CONTINUE
C
Cjl       (* Input f"ur trapped-flux zur schildeingabe verschoben *)
Cjl
      RETURN
      END
C
C
C     ****************************************************************
C
C
      SUBROUTINE SHIPOT(NN, MAX, NSHI, LENSHI, RADSHI, RHOLE, CENSHI,
     1                  SYMSHI, M)
C
C           (* Calculates   the  interaction   between  different *)
C           (* current  rings  in the shield. The theta component *)
C           (* of the vector potential in the Ith ring , produced *)
C           (* by  unit  current  density  in  the  IIth ring, is *)
C           (* stored as the component M(I, II) of the matrix M.  *)
C           (*                                                    *)
C           (* The meanings of the input parameters are:          *)
C           (*     NN, MAX, NSHI, LENSHI, RADSHI, RHOLE, CENSHI : *)
C           (*        as in SUBROUTINE RESULT                     *)
C           (*     SYMSHI(K) : .TRUE.  if there is a similar part *)
C           (*                 centered  to z=-CENSHI(K); .FALSE. *)
C           (*                 otherwise                          *)
C           (*     M : the  NNxNN  matrix  containing  the result *)
C           (*         (output)                                   *)
C
      INTEGER NN, MAX, NSHI
      DOUBLE PRECISION LENSHI(MAX), RADSHI(MAX), RHOLE(MAX),
     1                 CENSHI(MAX), M(NN,NN)
      LOGICAL SYMSHI(MAX)
C
      INTEGER N, IND1, IND2, J, JJ, III, JJJ
      DOUBLE PRECISION ZFIEL1, ZFIEL2, ZSOUR1, ZSOUR2, DISTZ1, DISTZ2,
     1                 RFIELD, RSOURC, DSKPOT, CYLPOT, RES1, RES2, RES3
C           (* N : the  number  of  current  rings in each of the *)
C           (*     shield parts                                   *)
C           (* IND1, IND2 : indices  for shield parts ; IND1 used *)
C           (*              normally  for  field  part,  IND2 for *)
C           (*              source part                           *)
C           (* J, JJ : used in current ring indices               *)
C           (* III, JJJ : used  to  index current rings belonging *)
C           (*            to the same cylindrical shield part     *)
C           (* ZFIEL1 : z-coordinate  of  the  field  ring  if it *)
C           (*          belongs to a cylindrical shield part      *)
C           (* ZFIEL2 : z-coordinate of the corresponding ring in *)
C           (*          the  symmetrically located similar shield *)
C           (*          part, if there is one                     *)
C           (* ZSOUR1, ZSOUR2 : as  ZFIEL1  and  ZFIEL2,  but for *)
C           (*                  source current rings              *)
C           (* DISTZ1 : distance between IND1th and IND2th planar *)
C           (*          shield parts                              *)
C           (* DISTZ2 : distance  from  IND1th planar shield part *)
C           (*          to  the  symmetrically located partner of *)
C           (*          IND2th planar shield part, or vice versa  *)
C           (* RFIELD : the  radius  of  the  field  ring  if  it *)
C           (*          belongs to a planar shield part           *)
C           (* RSOURC : the  radius  of  the  source  ring  if it *)
C           (*          belongs to a planar shield part           *)
C           (* DSKPOT : function  subprogram  for calculating the *)
C           (*          vector  potential  produced  by  a planar *)
C           (*          current ring                              *)
C           (* CYLPOT : function  subprogram  for calculating the *)
C           (*          vector  potential  producedt by a current *)
C           (*          sheet of cylindrical shape                *)
C           (* RES1, RES2, RES3 : used for storing the results of *)
C           (*                    calculations   of  interactions *)
C           (*                    between   similar   cylindrical *)
C           (*                    current rings                   *)
C                  ...............................
C
      N = INT(NN/NSHI)
C
      IND1 = 1
 2905 CONTINUE
C
         IND2 = 1
 2910    CONTINUE
C
            IF (LENSHI(IND1) .EQ. 0) THEN
C                 (* if the field part is a disk *)
C
               IF (LENSHI(IND2) .EQ. 0) THEN
C                    (* if the source is a disk *)
C
                  DISTZ1 = CENSHI(IND1) - CENSHI(IND2)
                  IF ((SYMSHI(IND1)) .OR. (SYMSHI(IND2)))
     1               DISTZ2 = CENSHI(IND1) + CENSHI(IND2)
C
                  DO 2920 J = 1, N
C                       (* each ring of the field part *)
                     RFIELD = RHOLE(IND1) +
     1                        (J-0.5)*(RADSHI(IND1)-RHOLE(IND1))/N
C
                     DO 2915 JJ = 1, N
C                          (* each ring of the source part *)
                        RSOURC = RHOLE(IND2) +
     1                          (JJ-0.5)*(RADSHI(IND2)-RHOLE(IND2))/N
                        M((IND1-1)*N+J, (IND2-1)*N+JJ) =
     1                     DSKPOT((RADSHI(IND2)-RHOLE(IND2))/N,
     2                            RSOURC, RFIELD, DISTZ1)

                        IF (SYMSHI(IND1)) THEN
C                             (* if the field part has a similar *)
C                             (* partner located symmetrically   *)
                           M(IND1*N+J, (IND2-1)*N+JJ) =
     1                        DSKPOT((RADSHI(IND2)-RHOLE(IND2))/N,
     2                               RSOURC, RFIELD, -DISTZ2)
C                                (* the index of the symmetrically *)
C                                (* located field part is IND1+1   *)
C
                           IF (SYMSHI(IND2)) THEN
C                                (* if both shield parts have similar *)
C                                (* partners located symmetrically    *)
                              M(IND1*N+J, IND2*N+JJ) =
     1                           M((IND1-1)*N+J, (IND2-1)*N+JJ)
                              M((IND1-1)*N+J, IND2*N+JJ) =
     1                           M(IND1*N+J, (IND2-1)*N+JJ)
C                                   (* the index of the symmetrically *)
C                                   (* located source part is IND2+1  *)
                           ENDIF
C
                        ELSE IF (SYMSHI(IND2)) THEN
C                             (* if only the source part has a simi- *)
C                             (* lar partner located symmetrically   *)
                           M((IND1-1)*N+J, IND2*N+JJ) =
     1                        DSKPOT((RADSHI(IND2)-RHOLE(IND2))/N,
     2                               RSOURC, RFIELD, DISTZ2)
                        ENDIF
 2915                CONTINUE
 2920             CONTINUE
C
               ELSE
C                    (* if the field part is a disk but the source *)
C                    (* part a cylinder                            *)
C
                  DO 2930 J = 1, N
C                       (* each ring of the field part *)
                     RFIELD = RHOLE(IND1) +
     1                        (J-0.5)*(RADSHI(IND1)-RHOLE(IND1))/N
C
                     DO 2925 JJ = 1, N
C                          (* each ring of the source part *)
                        ZSOUR1 = CENSHI(IND2) + LENSHI(IND2)/2D0 -
     1                           (JJ-0.5)*LENSHI(IND2)/N
                        IF (SYMSHI(IND2))
     1                     ZSOUR2 = -CENSHI(IND2) + LENSHI(IND2)/2D0 -
     2                              (JJ-0.5)*LENSHI(IND2)/N
                        M((IND1-1)*N+J, (IND2-1)*N+JJ) =
     1                     CYLPOT(LENSHI(IND2)/N, RADSHI(IND2), RFIELD,
     2                            CENSHI(IND1)-ZSOUR1)
C
                        IF (SYMSHI(IND1)) THEN
C                             (* if the field part has a similar *)
C                             (* partner located symmetrically   *)
                           M(IND1*N+J, (IND2-1)*N+JJ) =
     1                        CYLPOT(LENSHI(IND2)/N, RADSHI(IND2),
     2                           RFIELD, -CENSHI(IND1)-ZSOUR1)
C
                           IF (SYMSHI(IND2)) THEN
C                                (* if both shield parts have similar *)
C                                (* partners located symmetrically    *)
                              M(IND1*N+J, (IND2+1)*N-JJ+1) =
     1                           M((IND1-1)*N+J, (IND2-1)*N+JJ)
                              M((IND1-1)*N+J, (IND2+1)*N-JJ+1) =
     1                           M(IND1*N+J, (IND2-1)*N+JJ)
                           ENDIF
C
                        ELSE IF (SYMSHI(IND2)) THEN
C                             (* if only the source part has a simi- *)
C                             (* lar partner located symmetrically   *)
                           M((IND1-1)*N+J, IND2*N+JJ) =
     1                        CYLPOT(LENSHI(IND2)/N, RADSHI(IND2),
     2                           RFIELD, CENSHI(IND1)-ZSOUR2)
                        ENDIF
 2925                CONTINUE
 2930             CONTINUE
               ENDIF
C
            ELSE
C                 (* if the field part is a cylinder *)
C
               IF (LENSHI(IND2) .EQ. 0) THEN
C                    (* if the source is a disk *)
C
                  DO 2940 J = 1, N
C                       (* each ring of the field part *)
                     ZFIEL1 = CENSHI(IND1) + LENSHI(IND1)/2D0 -
     1                        (J-0.5)*LENSHI(IND1)/N
                     IF (SYMSHI(IND1))
     1                  ZFIEL2 = -CENSHI(IND1) + LENSHI(IND1)/2D0 -
     2                           (J-0.5)*LENSHI(IND1)/N
C
                     DO 2935 JJ = 1, N
C                          (* each ring of the source part *)
                        RSOURC = RHOLE(IND2) +
     1                          (JJ-0.5)*(RADSHI(IND2)-RHOLE(IND2))/N
                        M((IND1-1)*N+J, (IND2-1)*N+JJ) =
     1                     DSKPOT((RADSHI(IND2)-RHOLE(IND2))/N, RSOURC,
     2                            RADSHI(IND1), ZFIEL1-CENSHI(IND2))
C
                        IF (SYMSHI(IND1)) THEN
C                             (* if the field part has a similar *)
C                             (* partner located symmetrically   *)
                           M(IND1*N+J, (IND2-1)*N+JJ) =
     1                        DSKPOT((RADSHI(IND2)-RHOLE(IND2))/N,
     2                               RSOURC, RADSHI(IND1),
     3                               ZFIEL2-CENSHI(IND2))
C
                           IF (SYMSHI(IND2)) THEN
C                                (* if both shield parts have similar *)
C                                (* partners located symmetrically    *)
                              M((IND1+1)*N-J+1, IND2*N+JJ) =
     1                           M((IND1-1)*N+J, (IND2-1)*N+JJ)
                              M(IND1*N-J+1, IND2*N+JJ) =
     1                           M(IND1*N+J, (IND2-1)*N+JJ)
                           ENDIF
C
                        ELSE IF (SYMSHI(IND2)) THEN
C                             (* if only the source part has a simi- *)
C                             (* lar partner located symmetrically   *)
                           M((IND1-1)*N+J, IND2*N+JJ) =
     1                        DSKPOT((RADSHI(IND2)-RHOLE(IND2))/N,
     2                               RSOURC, RADSHI(IND1),
     3                               ZFIEL1+CENSHI(IND2))
                        ENDIF
 2935                CONTINUE
 2940             CONTINUE
C
               ELSE
C                    (* if both the source and the field part are *)
C                    (* cylinders                                 *)
C
                  IF (IND1 .EQ. IND2) THEN
C                       (* if  the  source and field cylinders are *)
C                       (* the  same, or if they are symmetrically *)
C                       (* located similar partners of each other, *)
C                       (* the  symmetry of the system can be used *)
C                       (* to make the amount of calculations much *)
C                       (* fewer:  the interaction between current *)
C                       (* rings  depends  only  on their distance *)
C                       (* from each other                         *)
                     DO 2950 J = 1, N
                        III = 1
                        JJJ = J
                        RES1 = CYLPOT(LENSHI(IND1)/N, RADSHI(IND1),
     1                              RADSHI(IND1), (J-1)*LENSHI(IND1)/N)
                        IF (SYMSHI(IND1)) THEN
                           RES2 = CYLPOT(LENSHI(IND1)/N, RADSHI(IND1),
     1                                  RADSHI(IND1), 2*CENSHI(IND1)+
     2                                  (J-1)*LENSHI(IND1)/N)
                           RES3 = CYLPOT(LENSHI(IND1)/N, RADSHI(IND1),
     1                                  RADSHI(IND1), 2*CENSHI(IND1)-
     2                                  (J-1)*LENSHI(IND1)/N)
                        ENDIF
C                             (* RES1: interactions within one part; *)
C                             (* RES2 and RES3: interactions between *)
C                             (* a  cylinder  and  its symmetrically *)
C                             (* located similar partner             *)
C
 2945                   CONTINUE
                           M((IND1-1)*N+III, (IND1-1)*N+JJJ) = RES1
                           M((IND1-1)*N+JJJ, (IND1-1)*N+III) = RES1
                           IF (SYMSHI(IND1)) THEN
                              M(IND1*N+III, IND1*N+JJJ) = RES1
                              M(IND1*N+JJJ, IND1*N+III) = RES1
                              M((IND1-1)*N+III, IND1*N+JJJ) = RES2
                              M(IND1*N+JJJ, (IND1-1)*N+III) = RES2
                              M((IND1-1)*N+JJJ, IND1*N+III) = RES3
                              M(IND1*N+III, (IND1-1)*N+JJJ) = RES3
                           ENDIF
C
                           III = III + 1
                           JJJ = JJJ + 1
                           IF (JJJ .LE. N) GOTO 2945
C                                 (* if  all possible distances *)
C                                 (* between current rings have *)
C                                 (* not yet been gone through  *)
                        CONTINUE
 2950                CONTINUE
                  ELSE
C                       (* if the source and field parts are *)
C                       (* different solenoids               *)
C
                     DO 2960 J = 1, N
C                          (* each ring of the field part *)
                        ZFIEL1 = CENSHI(IND1) + LENSHI(IND1)/2D0 -
     1                           (J-0.5)*LENSHI(IND1)/N
                        IF (SYMSHI(IND1))
     1                     ZFIEL2 = -CENSHI(IND1) + LENSHI(IND1)/2D0 -
     2                              (J-0.5)*LENSHI(IND1)/N
C
                        DO 2955 JJ = 1, N
C                             (* each ring of the source part *)
                           ZSOUR1 = CENSHI(IND2) + LENSHI(IND2)/2D0 -
     1                              (JJ-0.5)*LENSHI(IND2)/N
                           IF (SYMSHI(IND2))
     1                        ZSOUR2 = -CENSHI(IND2) + LENSHI(IND2)/2D0
     2                                 - (JJ-0.5)*LENSHI(IND2)/N
                           M((IND1-1)*N+J, (IND2-1)*N+JJ) =
     1                        CYLPOT(LENSHI(IND2)/N, RADSHI(IND2),
     2                           RADSHI(IND1), ZFIEL1-ZSOUR1)
C
                           IF (SYMSHI(IND1)) THEN
C                                (* if the field part has a similar *)
C                                (* partner located symmetrically   *)
                              M(IND1*N+J, (IND2-1)*N+JJ) =
     1                           CYLPOT(LENSHI(IND2)/N, RADSHI(IND2),
     2                                  RADSHI(IND1), ZFIEL2-ZSOUR1)
                              IF (SYMSHI(IND2)) THEN
C                                   (* if both shield parts have *)
C                                   (* similar  partners located *)
C                                   (* symmetrically             *)
                                 M((IND1+1)*N-J+1, (IND2+1)*N-JJ+1) =
     1                              M((IND1-1)*N+J, (IND2-1)*N+JJ)
                                 M(IND1*N-J+1, (IND2+1)*N-JJ+1) =
     1                              M(IND1*N+J, (IND2-1)*N+JJ)
                              ENDIF
C
                           ELSE IF (SYMSHI(IND2)) THEN
C                                (* if only the source part has *)
C                                (* a  similar  partner located *)
C                                (* symmetrically               *)
                              M((IND1-1)*N+J, IND2*N+JJ) =
     1                           CYLPOT(LENSHI(IND2)/N, RADSHI(IND2),
     2                                  RADSHI(IND1), ZFIEL1-ZSOUR2)
                           ENDIF
 2955                   CONTINUE
 2960                CONTINUE
                  ENDIF
               ENDIF
            ENDIF
C
            IF (SYMSHI(IND2)) THEN
               IND2 = IND2 + 2
C                 (* the effect of the symmetrically located similar *)
C                 (* source part, whose index is IND2+1, has already *)
C                 (* been calculated*)
            ELSE
               IND2 = IND2 + 1
            ENDIF
C
            IF (IND2 .LE. NSHI) GOTO 2910
C                 (* if there are more source parts *)
         CONTINUE
C
         IF (SYMSHI(IND1)) THEN
            IND1 = IND1 + 2
         ELSE
            IND1 = IND1 + 1
         ENDIF
C
         IF (IND1 .LE. NSHI) GOTO 2905
C              (* if there are more field parts *)
      CONTINUE
C
      RETURN
      END
C
C
C     ****************************************************************
C
C
      SUBROUTINE SOLPOT(NN, MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE,
     1                  SYMSHI, LENSOL, RADSOL, CENSOL, LAYERS, LOOPS,
     2                  SYMSOL, CUR, WIRDIA, FOILTH, ASTOR, A)
C
C           (* Calculates  the  theta  component  of  the  vector *)
C           (* potential  produced  by  the  solenoid  coils. The *)
C           (* potential  in  the  Jth ring of the shield will be *)
C           (* stored as the Jth component of the vector A.       *)
C           (*                                                    *)
C           (* The meanings of the input parameters are:          *)
C           (*     NN,  MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE, *)
C           (*        LENSOL,   RADSOL,  CENSOL,  LAYERS,  LOOPS, *)
C           (*        SYMSOL, CUR, WIRDIA, FOILTH :               *)
C           (*           as in SUBROUTINE RESULT                  *)
C           (*     SYMSHI(I) : .TRUE.  if there is a similar part *)
C           (*                 centered  to z=-CENSHI(I); .FALSE. *)
C           (*                 otherwise                          *)
C           (*     ASTOR(NN) : a  "storage  vector"  used for the *)
C           (*                 results  of subroutines SOLDSK and *)
C           (*                 SOLCYL                             *)
C           (*     A(NN) : the result of this subroutine (output) *)
C
      INTEGER NN, MAX, NSHI, LAYERS(MAX)
      DOUBLE PRECISION LENSHI(MAX), RADSHI(MAX), CENSHI(MAX),
     1                RHOLE(MAX), LENSOL(MAX), RADSOL(MAX), CENSOL(MAX),
     2                LOOPS(MAX), CUR(MAX), WIRDIA, FOILTH,
     3                ASTOR(NN), A(NN)
      LOGICAL SYMSHI(MAX), SYMSOL(MAX)
C
      INTEGER ISOL, ISHI, N, J
C           (* ISOL : the index of the solenoid                   *)
C           (* ISHI : the index of the shield part                *)
C           (* N : the  number  of current rings used to approxi- *)
C           (*     mate each of the shield parts                  *)
C           (* J : used to index the current rings in the shield  *)
C                  ...............................
C
      N = INT(NN/NSHI)
      ISOL = 1
C
 3010 CONTINUE
C           (* calculate the vector potentials produced by each coil *)
         ISHI = 1

 3020    CONTINUE
C              (* calculate the vector potential produced by ISOLth *)
C              (* solenoid on the current rings forming each of the *)
C              (* shield parts                                      *)
            CALL ZERO(ASTOR, 1, NN)
C                 (* makes every component of array ASTOR(NN) zero *)
C
            IF (LENSHI(ISHI) .EQ. 0) THEN
C                 (* if the shield part in the field region is a disk *)
               CALL SOLDSK(CENSHI(ISHI), RADSHI(ISHI), RHOLE(ISHI),
     1                     LENSOL(ISOL), RADSOL(ISOL), CENSOL(ISOL),
     2                     LAYERS(ISOL), LOOPS(ISOL), CUR(ISOL), WIRDIA,
     3                     FOILTH, N, ASTOR, NN)
C                                (* the vector potential distribution *)
C                                (* on  the  shield part is stored as *)
C                                (* the  first  N components of array *)
C                                (* ASTOR(NN)                         *)
               IF (SYMSOL(ISOL))
     1            CALL SOLDSK(CENSHI(ISHI), RADSHI(ISHI), RHOLE(ISHI),
     2                     LENSOL(ISOL), RADSOL(ISOL), -CENSOL(ISOL),
     3                     LAYERS(ISOL), LOOPS(ISOL), CUR(ISOL), WIRDIA,
     4                     FOILTH, N, ASTOR, NN)
C                     (* adds the effect of symmetrically located *)
C                     (* similar solenoid                         *)
C
               DO 3030 J = 1, N
C                    (* each ring of the shield disk *)
                  A((ISHI-1)*N+J) = A((ISHI-1)*N+J) + ASTOR(J)
                  IF (SYMSOL(ISOL) .AND. SYMSHI(ISHI))
     2                A(ISHI*N+J) = A(ISHI*N+J) + ASTOR(J)
 3030          CONTINUE
C
               IF ((.NOT. SYMSOL(ISOL)) .AND. SYMSHI(ISHI)) THEN
C                    (* if only the shield part has a similar partner *)
C                    (* located symmetrically, at -CENSHI(ISHI)       *)
                  CALL ZERO(ASTOR, 1, NN)
                  CALL SOLDSK(-CENSHI(ISHI), RADSHI(ISHI), RHOLE(ISHI),
     1                     LENSOL(ISOL), RADSOL(ISOL), CENSOL(ISOL),
     2                     LAYERS(ISOL), LOOPS(ISOL), CUR(ISOL), WIRDIA,
     3                     FOILTH, N, ASTOR, NN)
                  DO 3040 J = 1, N
                     A(ISHI*N+J) = A(ISHI*N+J) + ASTOR(J)
 3040              CONTINUE
               ENDIF
C
            ELSE
C                 (* if the shield part in the field region is *)
C                 (* a cylinder                                *)
               CALL SOLCYL(LENSHI(ISHI), RADSHI(ISHI), CENSHI(ISHI),
     1                     LENSOL(ISOL), RADSOL(ISOL), CENSOL(ISOL),
     2                     LAYERS(ISOL), LOOPS(ISOL), CUR(ISOL), WIRDIA,
     3                     FOILTH, N, ASTOR, NN)
               IF (SYMSOL(ISOL) .AND. (CENSHI(ISHI) .NE. 0))
C                    (* if there is a similar solenoid located *)
C                    (* symmetrically,  and  the center of the *)
C                    (* shield part is not in origo            *)
     1            CALL SOLCYL(LENSHI(ISHI), RADSHI(ISHI), CENSHI(ISHI),
     2                     LENSOL(ISOL), RADSOL(ISOL), -CENSOL(ISOL),
     3                     LAYERS(ISOL), LOOPS(ISOL), CUR(ISOL), WIRDIA,
     4                     FOILTH, N, ASTOR, NN)
C
               DO 3050 J = 1, N
C                    (* each ring of the shield cylinder *)
                  A((ISHI-1)*N+J) = A((ISHI-1)*N+J) + ASTOR(J)
                  IF (SYMSOL(ISOL) .AND. (CENSHI(ISHI) .EQ. 0))
     1               A(ISHI*N-J+1) = A(ISHI*N-J+1) + ASTOR(J)
                  IF (SYMSOL(ISOL) .AND. SYMSHI(ISHI))
     1               A((ISHI+1)*N-J+1) = A((ISHI+1)*N-J+1) + ASTOR(J)
 3050          CONTINUE
C
               IF ((.NOT. SYMSOL(ISOL)) .AND. SYMSHI(ISHI)) THEN
C                    (* if only the shield part has a similar partner *)
C                    (* located symmetrically, at -CENSHI(ISHI)       *)
                  CALL ZERO(ASTOR, 1, NN)
                  CALL SOLCYL(LENSHI(ISHI), RADSHI(ISHI), -CENSHI(ISHI),
     1                     LENSOL(ISOL), RADSOL(ISOL), CENSOL(ISOL),
     2                     LAYERS(ISOL), LOOPS(ISOL), CUR(ISOL), WIRDIA,
     3                     FOILTH, N, ASTOR, NN)
                  DO 3060 J = 1, N
                     A(ISHI*N+J) = A(ISHI*N+J) + ASTOR(J)
 3060             CONTINUE
               ENDIF
            ENDIF
C
            IF (SYMSHI(ISHI)) THEN
               ISHI = ISHI + 2
C                 (* the index of the symmetric shield part is ISHI+1 *)
            ELSE
               ISHI = ISHI + 1
            ENDIF
C
            IF (ISHI .LE. NSHI) GOTO 3020
C                 (* if there are more shield parts *)
C
         CONTINUE
C
         IF (.NOT. SYMSOL(ISOL)) THEN
            ISOL = ISOL + 1
         ELSE
            ISOL = ISOL + 2
C                 (* the index of the symmetric solenoid is ISOL+1 *)
         ENDIF
C
         IF (LENSOL(ISOL) .GT. 0) GOTO 3010
C              (* if there are more solenoids *)
      CONTINUE
C
      RETURN
      END
C
C
C     ****************************************************************
C
C
      SUBROUTINE CURREN(NN, MAX, NSHI, DIFSHI, LENSHI, RADSHI, CENSHI,
     1                 RHOLE, SYMSHI, LENSOL, RADSOL, CENSOL, LAYERS,
     2                 LOOPS, SYMSOL, CUR, WIRDIA, FOILTH, BEXT, TRAFLX,
     3                 M, A, X)
C
C           (* This subroutine calculates the current distribution *)
C           (* in  the  shield,  dividing each of the shield parts *)
C           (* into  N  rings and using the fact that the magnetic *)
C           (* flux  through  any  of these rings must be the same *)
C           (* constant,  the flux trapped by the shield. This can *)
C           (* also   be   expressed  in  cylindrical  coordinates *)
C           (* demanding  the magnetic vector potential in each of *)
C           (* the  rings  to  be  the trapped flux divided by the *)
C           (* length  of  the  periphery of the ring. Because the *)
C           (* vector  potential  is  a  function  not only of the *)
C           (* currents  in  the  coils  but  also  of the current *)
C           (* distribution  in the shield, itself, we arrive at a *)
C           (* matrix   equation  MX + A = C,  where  the  unknown *)
C           (* vector X contains the currents in the shield rings, *)
C           (* vector  A  contains  the effect of the coils on the *)
C           (* vector  potential  in the shield, matrix M contains *)
C           (* the  effect  of  the  current  distribution  in the *)
C           (* shield  itself, and vector C contains the effect of *)
C           (* the trapped flux.                                   *)
C           (*                                                     *)
C           (* The meanings of the input parameters are:          *)
C           (*     NN,  MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE, *)
C           (*        LENSOL,   RADSOL,  CENSOL,  LAYERS,  LOOPS, *)
C           (*        SYMSOL, CUR, WIRDIA, FOILTH, BEXT, TRAFLX : *)
C           (*           as in SUBROUTINE RESULT                  *)
C           (*     SYMSHI(I) : .TRUE.  if there is a similar part *)
C           (*                 centered  to z=-CENSHI(I); .FALSE. *)
C           (*                 otherwise                          *)
C           (*     DIFSHI : .TRUE.  if the shield dimensions have *)
C           (*              been changed ; .FALSE. otherwise      *)
C           (*     M, A, X : see  above  ;  the  matrix  equation *)
C           (*               MX + A = C  is  solved  here  in the *)
C           (*               form MX = A', where A' = C - A       *)
C
      INTEGER NN, MAX, NSHI, LAYERS(MAX)
      DOUBLE PRECISION LENSHI(MAX), RADSHI(MAX), CENSHI(MAX),
     1          TRAFLX(MAX),
     2          RHOLE(MAX), LENSOL(MAX), RADSOL(MAX), CENSOL(MAX),
     3          LOOPS(MAX), CUR(MAX), WIRDIA, FOILTH, BEXT,
     4          X(NN), M(NN,NN), A(NN)
      LOGICAL DIFSHI, SYMSHI(MAX), SYMSOL(MAX)
C
      INTEGER N, ISHI, I, N1
      DOUBLE PRECISION DCONST, PI, RADIUS
C           (* N : the  number  of current rings used to approxi- *)
C           (*     mate each of the shield parts                  *)
C           (* ISHI : the index of the shield part                *)
C           (* I : used to index the current rings in the shield  *)
C           (* N1 : the  total  number  of  current rings used to *)
C           (*      approximate the superconducting shield        *)
C           (* DCONST : an IMSL-subroutine for values of scienti- *)
C           (*          fic constants                             *)
C           (* PI : 3.1415...                                     *)
C           (* RADIUS : the  "average radius" of the current ring *)
C           (*          in the planar part of the superconducting *)
C           (*          shield                                    *)
C                  ...............................
C
      PI = DCONST('PI')
      N = INT(NN/NSHI)
      CALL ZERO(A, 1, NN)
      CALL ZERO(X, 1, NN)
C
      IF (DIFSHI) THEN
         CALL ZERO(M, NN, NN)
         CALL SHIPOT(NN, MAX, NSHI, LENSHI, RADSHI, RHOLE, CENSHI,
     1               SYMSHI, M)
C           (* if the shield has been changed, calculate the matrix M *)
      ENDIF
C
      IF (LENSOL(1) .NE. 0)
     1   CALL SOLPOT(NN, MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE,
     2               SYMSHI, LENSOL, RADSOL, CENSOL, LAYERS, LOOPS,
     3               SYMSOL, CUR, WIRDIA, FOILTH, X, A)
C              (* if there are solenoids, calculate the vector *)
C              (* potential  produced  by  them  and store the *)
C              (* result to the vector A ; vector X is used in *)
C              (* SUBROUTINE SOLPOT only as work space         *)
      CONTINUE
C
      DO 3130 ISHI = 1, NSHI
         IF (LENSHI(ISHI) .EQ. 0) THEN
C              (* if planar shield part *)
            DO 3110 I = 1, N
               RADIUS = RHOLE(ISHI) +
     1                  (I-0.5)*(RADSHI(ISHI)-RHOLE(ISHI))/N
C                    (* each current ring has different radius *)
               A(N*(ISHI-1)+I) = TRAFLX(ISHI)/(2*PI*RADIUS) -
     1                           A(N*(ISHI-1)+I) - BEXT*RADIUS/2D0
C                    (* A' = C - A *)
 3110       CONTINUE
C
         ELSE
C              (* if cylindrical shield part *)
Cjl            DO 3120 I = 1, N
Cjl               A(N*(ISHI-1)+I) = TRAFLU/(2*PI*RADSHI(ISHI)) -
Cjl     1                           A(N*(ISHI-1)+I) - BEXT*RADSHI(ISHI)/2D0
CjlC                    (* A' = C - A *)
            DO 3120 I = 1, N
               A(N*(ISHI-1)+I) = TRAFLX(ISHI)/(2*PI*RADSHI(ISHI)) -
     1                           A(N*(ISHI-1)+I) - BEXT*RADSHI(ISHI)/2D0
C                    (* A' = C - A *)
 3120       CONTINUE
         ENDIF
 3130 CONTINUE
C
      CALL ZERO (X, 1, NN)
      N1 = NSHI*N
C           (* N1 is the total number of current rings *)
C
      CALL DLSARG(N1, M, NN, A, 1, X)
C           (* solves the matrix equation MX = A ; an IMSL-subroutine *)
C
      RETURN
      END
C
C
C     ****************************************************************
C
C
      SUBROUTINE FLUX(Z, R, NN, MAX, NSHI, LENSHI, RADSHI, CENSHI,
     1            RHOLE, CURSHI, CENSOL, LENSOL, RADSOL, LOOPS, LAYERS,
     2            WIRDIA, FOILTH, CUR, BEXT, MAGFLU)
C
C           (* Calculates  the  vector  potential  in  the  point *)
C           (* (R, Z).                                            *)
C           (*                                                    *)
C           (* The meanings of the input parameters are:          *)
C           (*     Z, R : the coordinates of the field point      *)
C           (*     NN,  MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE, *)
C           (*        CURSHI,   CENSOL,  LENSOL,  RADSOL,  LOOPS, *)
C           (*        LAYERS, WIRDIA, FOILTH, CUR, BEXT : *)
C           (*           as in SUBROUTINE RESULT                  *)
C           (*     MAGFLU : the magnetic flux through the ring of *)
C           (*              radius R located at z = Z ; output    *)
C
      INTEGER NN, MAX, NSHI, LAYERS(MAX)
      DOUBLE PRECISION Z, R, LENSHI(MAX), RADSHI(MAX), CENSHI(MAX),
     1                RHOLE(MAX), CURSHI(NN), CENSOL(MAX), LENSOL(MAX),
     2                RADSOL(MAX), LOOPS(MAX), WIRDIA, FOILTH,
     3                CUR(MAX), BEXT, MAGFLU
C
      INTEGER N, ISHI, INDSOL, ILAY, I
      DOUBLE PRECISION DISTZ, RADIUS, RADLAY, CURDEN, DSKPOT, CYLPOT,
     1                 DCONST, PI
C           (* N : the  number  of current rings used to approxi- *)
C           (*     mate each of the shield parts                  *)
C           (* ISHI : the index of the shield part                *)
C           (* INDSOL : the index of the solenoid                 *)
C           (* ILAY : the index of the wire layer in the solenoid *)
C           (* I : used to index the current rings in the shield  *)
C           (* DISTZ : the  z-component  of  the  vector from the *)
C           (*         field  point  to  the center of the source *)
C           (*         current  ring  in  the  shield  or  to the *)
C           (*         center of the solenoid                     *)
C           (* RADIUS : the   "average  radius"   of  the  source *)
C           (*          current  ring  in  the planar part of the *)
C           (*          superconducting shield                    *)
C           (* RADLAY : the "average radius" of the wire layer in *)
C           (*          the solenoid                              *)
C           (* CURDEN : the  current  density in the shield or in *)
C           (*          the solenoid (A/m)                        *)
C           (* DSKPOT : function   which  calculates  the  vector *)
C           (*          potential of a planar current sheet       *)
C           (* CYLPOT : function   which  calculates  the  vector *)
C           (*          potential of a cylindrical current sheet  *)
C           (* DCONST : an IMSL-subroutine for values of scienti- *)
C           (*          fic constants                             *)
C           (* PI : 3.1415...                                     *)
C                  ...............................
C
      PI = DCONST('PI')
C
      MAGFLU = BEXT*PI*R*R
C           (* the magnetic flux due to the external field *)
C
      IF (NSHI .NE. 0) N = INT(NN/NSHI)
C
      IF (NSHI .NE. 0) THEN
C           (* if there is a superconducting shield *)
         DO 3230 ISHI = 1, NSHI
C              (* calculate the flux produced by the shield *)
            IF (LENSHI(ISHI) .GT. 0) THEN
C                 (* if the shield part is cylindrical *)
               DO 3210 I = 1, N
                  CURDEN = CURSHI(N*(ISHI-1)+I)
                  DISTZ = Z - CENSHI(ISHI) -
     1                    ((N+1)/2D0-I)*LENSHI(ISHI)/N
                  MAGFLU = MAGFLU + CYLPOT(LENSHI(ISHI)/N, RADSHI(ISHI),
     1                                     R, DISTZ) * CURDEN*2*PI*R
C                       (* the effect of one cylindrical current ring *)
 3210          CONTINUE
C
            ELSE
C                 (* if the shield part is planar *)
               DO 3220 I = 1, N
                  CURDEN = CURSHI(N*(ISHI-1)+I)
                  RADIUS = RHOLE(ISHI) +
     1                     (I-0.5)*(RADSHI(ISHI)-RHOLE(ISHI))/N
                  MAGFLU = MAGFLU + DSKPOT((RADSHI(ISHI)-RHOLE(ISHI))/N,
     1                        RADIUS, R, Z-CENSHI(ISHI)) * CURDEN*2*PI*R
C                       (* the effect of one planar current ring *)
 3220          CONTINUE
            ENDIF
 3230    CONTINUE
      ENDIF
C
C
      INDSOL = 1
C
 3240 CONTINUE
C           (* calculate the magnetic flux due to the solenoids *)
         IF (LENSOL(INDSOL) .EQ. 0) GOTO 3260
C              (* if no more solenoids *)
         RADLAY = RADSOL(INDSOL) + 0.5*WIRDIA
         CURDEN = LOOPS(INDSOL)*CUR(INDSOL)/LENSOL(INDSOL)
C
         DO 3250 ILAY = 1, LAYERS(INDSOL)
            DISTZ = Z - CENSOL(INDSOL)
            MAGFLU = MAGFLU + CYLPOT(LENSOL(INDSOL), RADLAY, R, DISTZ)
     1                        * CURDEN*2*PI*R
C
            RADLAY = RADLAY + WIRDIA + FOILTH
 3250    CONTINUE
C
         INDSOL = INDSOL+1
         GOTO 3240
 3260 CONTINUE
C
      RETURN
      END
C
C
C     ****************************************************************
C
C
      SUBROUTINE FIELD(Z, R, NN, MAX, NSHI, LENSHI, RADSHI, CENSHI,
     1            RHOLE, CURSHI, CENSOL, LENSOL, RADSOL, LOOPS, LAYERS,
     2            WIRDIA, FOILTH, CUR, BEXT, BZ, BR)
C
C           (* Calculates the magnetic field in the point (R, Z). *)
C           (*                                                    *)
C           (* The meanings of the input parameters are:          *)
C           (*     Z, R : the coordinates of the field point      *)
C           (*     NN,  MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE, *)
C           (*        CURSHI,   CENSOL,  LENSOL,  RADSOL,  LOOPS, *)
C           (*        LAYERS, WIRDIA, FOILTH, CUR, BEXT :         *)
C           (*           as in SUBROUTINE RESULT                  *)
C           (*     BZ, BR : z-  and  r-components of the magnetic *)
C           (*              field in the point (R, Z) ; output    *)
C
      INTEGER NN, MAX, NSHI, LAYERS(MAX)
      DOUBLE PRECISION Z, R, LENSHI(MAX), RADSHI(MAX), CENSHI(MAX),
     1                RHOLE(MAX), CURSHI(NN), CENSOL(MAX), LENSOL(MAX),
     2                RADSOL(MAX), LOOPS(MAX), WIRDIA, FOILTH,
     3                CUR(MAX), BEXT, BZ, BR, CYLBAX, CYLBZ, CYLBR,
     4                DSKBZ, DSKBR
Cbogi   added CYLBAX, CYLBZ, CYLBR, DSKBZ, DSKBR in order to
Cbogi   compile without warnings (-> fuehrt zum richtigen Ergebnis!!)

      LOGICAL ONDISK
C
      INTEGER N, ISHI, I
      DOUBLE PRECISION BZSOL, BRSOL, BZSHI, BRSHI, DISTZ, RADIUS,
     1                 WIDTH, CURDEN
Cbogi CURDEN added in order to comopile without warnings
C           (* ONDISK : .TRUE.  if  the  field point is on any of *)
C           (*          the superconducting disks                 *)
C           (* N : the  number  of current rings used to approxi- *)
C           (*     mate each of the shield parts                  *)
C           (* ISHI : the index of the shield part                *)
C           (* ISOL : the index of the solenoid                   *)
Cbogi       (* ISOL is not used in this function and therefore skiped out *)
C           (* I : used to index the current rings in the shield  *)
C           (* BZSOL, BRSOL : the  z-  and  r-components  of  the *)
C           (*                field in the point (R, Z), produced *)
C           (*                by the solenoids                    *)
C           (* BZSHI, BRSHI : the  z-  and  r-components  of  the *)
C           (*                field in the point (R, Z), produced *)
C           (*                by the shield                       *)
C           (* DISTZ : the  z-component  of  the  vector from the *)
C           (*         field  point  to  the center of the source *)
C           (*         current ring in the shield                 *)
C           (* RADIUS : the   "average  radius"   of  the  source *)
C           (*          current  ring  in  the planar part of the *)
C           (*          superconducting shield                    *)
C           (* SHIAXI, SHIBZ, SHIBR : names  of functions used in *)
C           (*                        this subroutine             *)
Cbogi       (* SHIBR, SHIAXI and SHIBZ are not used and therefore skiped out *)
C                  ...............................
C
      BZ = 0D0
      BR = 0D0
      BZSOL = 0D0
      BRSOL = 0D0
      BZSHI = 0D0
      BRSHI = 0D0
      ONDISK = .FALSE.
C
      IF (NSHI .NE. 0) N = INT(NN/NSHI)
C
      IF (NSHI .NE. 0) THEN
C           (* if there is a superconducting shield *)
         DO 3330 ISHI = 1, NSHI
C              (* calculate the field produced by the shield *)
            IF (LENSHI(ISHI) .GT. 0) THEN
C                 (* if the shield part is cylindrical *)
               DO 3310 I = 1, N
                  DISTZ = Z - CENSHI(ISHI) -
     1                    ((N+1)/2D0-I)*LENSHI(ISHI)/N
                  CURDEN = CURSHI(N*(ISHI-1)+I)
                  IF (R .LT. 1.0D-6) THEN
C                       (* if the field point is on central axis *)
                     BZSHI = BZSHI + CYLBAX(DISTZ, RADSHI(ISHI), CURDEN,
     1                                      LENSHI(ISHI)/N)
                  ELSE
C                       (* if the field point is not on central axis *)
                     BZSHI = BZSHI + CYLBZ(DISTZ, R, RADSHI(ISHI),
     1                                     CURDEN, LENSHI(ISHI)/N)
                     BRSHI = BRSHI + CYLBR(DISTZ, R, RADSHI(ISHI),
     1                                     CURDEN, LENSHI(ISHI)/N)
                  ENDIF
C                       (* the effect of one cylindrical current ring *)
 3310          CONTINUE
C
            ELSE
C                 (* if the shield part is planar *)
               IF ((DABS(Z-CENSHI(ISHI)) .LE. 1D-10) .AND.
     1            (R .GE. RHOLE(ISHI)) .AND. (R .LE. RADSHI(ISHI))) THEN
C                    (* FUNCTION DSKBZ cannot calculate the field on *)
C                    (* a source disk                                *)
                  ONDISK = .TRUE.
                  BRSHI=0
C
               ELSE
C                    (* if the field point not on the shield disk *)
                  DO 3320 I = 1, N
                     WIDTH = (RADSHI(ISHI)-RHOLE(ISHI))/N
                     RADIUS = RHOLE(ISHI) + (I-0.5)*WIDTH
                     DISTZ = Z - CENSHI(ISHI)
                     CURDEN = CURSHI(N*(ISHI-1)+I)
C
                     BZSHI = BZSHI + DSKBZ(DISTZ, R, RADIUS, CURDEN,
     1                                     WIDTH)
                     BRSHI = BRSHI + DSKBR(DISTZ, R, RADIUS, CURDEN,
     1                                     WIDTH)
 3320             CONTINUE
C                       (* the effect of one planar current ring *)
               ENDIF
            ENDIF
 3330    CONTINUE
      ENDIF
C
C
      INDSOL = 1
C
 3340 CONTINUE
C           (* calculate the field produced by the solenoids *)
         IF (LENSOL(INDSOL) .EQ. 0) GOTO 3350
C              (* if no more solenoids *)
         CALL BSOLEN(Z-CENSOL(INDSOL), R, LENSOL(INDSOL),
     1               RADSOL(INDSOL), WIRDIA, LOOPS(INDSOL),
     2               LAYERS(INDSOL), CUR(INDSOL), FOILTH, BZSOL, BRSOL)
C
         INDSOL = INDSOL+1
         GOTO 3340
 3350 CONTINUE
C
      IF (ONDISK) THEN
         BZ = 0
      ELSE
         BZ = BZSHI + BZSOL + BEXT
      ENDIF
C
      BR = BRSHI + BRSOL
C
      RETURN
      END
C
C
C     ****************************************************************
C
C
      SUBROUTINE DISTRI(NN, MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE,
     1                  CURSHI, FILE)
C
C           (* Writes  the  current distribution in the shield to *)
C           (* the display and the file number FILE.              *)
C           (*                                                    *)
C           (* The meanings of the input parameters are:          *)
C           (*     NN,  MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE, *)
C           (*        CURSHI : as in SUBROUTINE RESULT            *)
C           (*     FILE : the   number  of  the  file  where  the *)
C           (*            results are stored                      *)
C
      INTEGER NN, MAX, NSHI, FILE
      DOUBLE PRECISION LENSHI(MAX), RADSHI(MAX), CENSHI(MAX),
     1                 RHOLE(MAX), CURSHI(NN)
C
      INTEGER N, ISHI, I
      DOUBLE PRECISION Z, R
C           (* N : the  number  of current rings used to approxi- *)
C           (*     mate each of the shield parts                  *)
C           (* ISHI : the index of the shield part                *)
C           (* I : used to index the current rings in the shield  *)
C           (* Z, R : the coordinates of the current ring         *)
C                  ...............................
C
      IF (NSHI .NE. 0) THEN
         PRINT *, (' ')
C
         PRINT 3410
         WRITE(FILE,3410)
 3410    FORMAT('#'/'#',5X, ' z (mm)', 4X, 'r (mm)', 8X,
     1          'Current (A/mm)'/'#')
C
         N = INT(NN/NSHI)
C
         DO 3460 ISHI = 1, NSHI
C
            IF (LENSHI(ISHI) .GT. 0) THEN
C                 (* if cylindrical shield part *)
               DO 3430 I = 1, N
                  Z = CENSHI(ISHI) + ((N+1)/2D0-I)*LENSHI(ISHI)/N
                  R = RADSHI(ISHI)
                  PRINT 3420, Z*1000, R*1000,
     1                       CURSHI(N*(ISHI-1)+I)/1000
                  WRITE(FILE,3420) Z*1000, R*1000,
     1                       CURSHI(N*(ISHI-1)+I)/1000
 3420             FORMAT(2X, F8.2 , 2X, F8.2, 4X, F14.5)
 3430          CONTINUE
C
            ELSE
C                 (* if the shield part is a disk *)
               DO 3450 I = 1, N
                  Z = CENSHI(ISHI)
                  R = RHOLE(ISHI) + (I-0.5)*(RADSHI(ISHI)-RHOLE(ISHI))/N
                  PRINT 3440, Z*1000, R*1000,
     1                          CURSHI(N*(ISHI-1)+I)/1000
                  WRITE(FILE,3440) Z*1000, R*1000,
     1                             CURSHI(N*(ISHI-1)+I)/1000
 3440             FORMAT(2X, F8.2 , 2X, F8.2, 4X, F14.5)
 3450          CONTINUE
            ENDIF
 3460    CONTINUE
      ENDIF
C
      RETURN
      END
C
C
C     ****************************************************************
C
C
C
      SUBROUTINE ASK(PROTYP, RBEG, DELTAR, REND, ZBEG, DELTAZ, ZEND)
C
C           (* Asks where the magnetic flux or field profile must *)
C           (* be calculated.                                     *)
Cbogi       (* modified for calculating the magnetic field profile   *)
Cbogi       (* on a lattice                                          *)
C           (*                                                    *)
C           (* The meanings of the input parameters are:          *)
C           (*     PROTYP : the type of the profile ;             *)
C           (*                    1 = field profile,              *)
C           (*                    2 = flux profile                *)
Cbogi       (*     RBEG : the minimal Radius on the profile          *)
Cbogi       (*     DELTAR: the distance for two different radii if   *)
Cbogi       (*              calculating on a lattice                 *)
Cbogi       (*     REND:  the maximum radius for lattice             *)
C           (*     ZBEG : the z-coordinate of the first point     *)
C           (*     DELTAZ : the   distance   between   successive *)
C           (*              points                                *)
C           (*     ZEND : the z-coordinate of the last point      *)
C
      INTEGER PROTYP
      DOUBLE PRECISION RBEG, DELTAR, REND, ZBEG, DELTAZ, ZEND
C                  ...............................
C
      IF (PROTYP .EQ. 1) THEN
C           (* if field profile *)
         PRINT 3510
 3510    FORMAT ('Give the minimal, the difference and the maximum',
     1                  ' radius,'/'where the field profile should',
     2                  '  be calculated [mm]:')
         READ(*,*) RBEG, DELTAR, REND
C
      ELSE
C           (* if flux profile *)
         PRINT 3520
 3520    FORMAT ('Give the radius of the rings through which the',
     1           ' flux must be calculated.')
         READ(*,*) RBEG
         DELTAR = 0.0
         REND   = 0.0
      ENDIF
C
      PRINT  3530
 3530 FORMAT ('Give ZBEG, DELTAZ and ZEND in mm:')
      READ(*,*) ZBEG, DELTAZ, ZEND
C
      RBEG = RBEG/1000D0
      DELTAR = DELTAR/1000D0
      REND = REND/1000D0
      ZBEG = ZBEG/1000D0
      DELTAZ = DELTAZ/1000D0
      ZEND = ZEND/1000D0
C
      RETURN
      END
C
C
C     ****************************************************************
C
C
      SUBROUTINE RESULT(NN, MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE,
     1                  CURSHI, CENSOL, LENSOL, RADSOL, LOOPS, LAYERS,
     2                  SYMSOL, WIRDIA, FOILTH, CUR, BEXT, TRAFLX)
C
C           (* This  SUBROUTINE  organizes the calculation of the *)
C           (* magnetic  field  profile  and/or the flux profile, *)
C           (* and the output of the results.                     *)
C           (*                                                    *)
C           (* The meanings of the input parameters are:          *)
C           (*     NN : dimension of the vector CURSHI            *)
C           (*     MAX : the  maximum  number  of  solenoids  and *)
C           (*           shield parts                             *)
C           (*     NSHI : the number of shield parts              *)
C           (*     LENSHI(I) : the length of the Ith shield part  *)
C           (*     RADSHI(I) : the radius of the Ith shield part; *)
C           (*                 if  the  part is a disk, RADSHI(I) *)
C           (*                 is its outer radius                *)
C           (*     CENSHI(I) : the  z-coordinate of the center of *)
C           (*                 the Ith shield part                *)
C           (*     RHOLE(I) : the  radius  of  the  hole  in  the *)
C           (*                center of the Ith part, if the part *)
C           (*                happens to be a disk                *)
C           (*     TRAFLX(I) : magnetic flux trapped into the     *)
C           (*                 shield                             *)
C           (*     CURSHI : NN-dimensional  vector containing the *)
C           (*              current distribution in the shield    *)
C           (*     CENSOL(K) : the  z-coordinate of the center of *)
C           (*                 the Kth solenoid                   *)
C           (*     LENSOL(K) : the length of the Kth solenoid     *)
C           (*     RADSOL(K) : the   inner   radius  of  the  Kth *)
C           (*                 solenoid                           *)
C           (*     LOOPS(K) : the  number of turns of wire in one *)
C           (*                layer of the Kth solenoid           *)
C           (*     LAYERS(K) : the  number  of  layers of wire in *)
C           (*                 the Kth solenoid                   *)
C           (*     SYMSOL(K) : .TRUE. if there is a similar sole- *)
C           (*                 noid  centered  to z = -CENSOL(K); *)
C           (*                 .FALSE. otherwise                  *)
C           (*     WIRDIA : the diameter of the wire used to wind *)
C           (*              the solenoids                         *)
C           (*     FOILTH : the  thickness of the insulating foil *)
C           (*              between the layers                    *)
C           (*     CUR(K) : the  current  in  the wire of the Kth *)
C           (*              solenoid                              *)
C           (*     BEXT : the external magnetic field             *)
C
      INTEGER NN, MAX, NSHI, LAYERS(MAX)
      DOUBLE PRECISION LENSHI(MAX), RADSHI(MAX), CENSHI(MAX),
     1                 TRAFLX(MAX),
     2                 RHOLE(MAX), CURSHI(NN),CENSOL(MAX), LENSOL(MAX),
     3                 RADSOL(MAX), LOOPS(MAX),WIRDIA, FOILTH,
     4                 CUR(MAX), BEXT
      LOGICAL SYMSOL(MAX)
C
      DOUBLE PRECISION R, RBEG, DELTAR, REND, Z, ZBEG, DELTAZ, ZEND,
     1                 BZ0, BR0, BZ, BR, HOMOG, MAGFLU, TOTAL
      CHARACTER*1 ANSW
      CHARACTER*128 FLDNAM, FLXNAM, CURNAM, DSCNAM
      INTEGER FILFLD,FILFLX,FILCUR,FILDSC,NAMLEN
      LOGICAL FLXLOG,FLDLOG,CURLOG
Cbogi       (* FILENA: will contain the base name of the output     *)
Cbogi       (*           file                                       *)
Cbogi       (* FILxxx are the Filenumbers of the *.flx, *.cur,..    *)
Cbogi       (*        files.                                        *)
Cbogi       (* NAMLEN: will contain the length of the FILENA        *)
C           (* R, Z : the r- and z-coordinates of the field point *)
C           (* ZBEG, DELTAZ, ZEND : the  field  profile is calcu- *)
C           (*        lated  in points with given r-coordinate R, *)
C           (*        and  z-coordinate running from ZBEG to ZEND *)
C           (*        with step DELTAZ                            *)
C           (* BZ0, BR0 : the z- and r-components of the magnetic *)
C           (*            field in origo                          *)
C           (* BZ, BR : the  z-  and r-components of the magnetic *)
C           (*          fields in the point (R, Z)                *)
C           (* HOMOG : the homogenity of BZ in ppm:s ;            *)
C           (*         HOMOG = 1000000*(BZ-BZ0)/BZ0               *)
C           (* TOTAL : the  magnitude  of total magnetic field in *)
C           (*         the point (R,Z)                            *)
C           (* POTEN : the vector potential in the point (R, Z)   *)
C           (* DCONST : an IMSL-subroutine for values of scienti- *)
C           (*          fic constants                             *)
C           (* PI : 3.1415...                                     *)
C                  ...............................
C
C
      CALL FIELD(0D0, 0D0, NN, MAX, NSHI, LENSHI, RADSHI, CENSHI,
     1           RHOLE, CURSHI, CENSOL, LENSOL, RADSOL, LOOPS, LAYERS,
     2           WIRDIA, FOILTH, CUR, BEXT, BZ0, BR0)
C           (* calculate the magnetic field in origo *)
C
      PRINT 3609,BZ0*(1.0D4)
 3609 FORMAT('Field value in the origin is: ',G16.10,'Gauss')
      PRINT 3610
 3610 FORMAT('Give the basic filename for the results to be',
     1                                             ' stored.'/,
     2       ' There are outputfiles ~.fld, ~.flx, ~.cur',
     3                                          ' and ~.dsc'/,
     4       ' for the field , the flux, ',
     5       'the current distribution and the '/,
     6       ' coil/shield data.')
C
Cbogi Setup for the different filenames:
      READ (*,'(30A)') FLXNAM
      FLDNAM = FLXNAM
      CURNAM = FLXNAM
      DSCNAM = FLXNAM
      NAMLEN = INDEX (FLXNAM,' ')
      FLXNAM(NAMLEN:NAMLEN+4) = '.flx'
      FLDNAM(NAMLEN:NAMLEN+4) = '.fld'
      CURNAM(NAMLEN:NAMLEN+4) = '.cur'
      DSCNAM(NAMLEN:NAMLEN+4) = '.dsc'
      FILFLX=10
      FILFLD=11
      FILCUR=12
      FILDSC=13
      FLDLOG = .FALSE.
      FLXLOG = .FALSE.
      CURLOG = .FALSE.
C
C
C
 3615 CONTINUE
 3620    CONTINUE
            PRINT 3625
 3625       FORMAT('Do you want to know a field profile ("F"), ',
     1             'a magnetic flux' / ' profile ("X"), or the ',
     2             'current distribution in the shield ("C")?')
            READ(*, '(A1)', ERR=3620, END=3620) ANSW
            IF ((ANSW .NE. 'X') .AND. (ANSW .NE. 'x') .AND.
     1          (ANSW .NE. 'F') .AND. (ANSW .NE. 'f') .AND.
     2          (ANSW .NE. 'C') .AND. (ANSW .NE. 'c')) GOTO 3620
         CONTINUE
C
         IF ((ANSW .EQ. 'F') .OR. (ANSW .EQ. 'f')) THEN
C              (* field profile *)
            CALL ASK(1, RBEG, DELTAR, REND, ZBEG, DELTAZ, ZEND)
C                 (* ask the profile coordinates *)
C
            IF ( .NOT. FLDLOG ) THEN
               FLDLOG = .TRUE.
               OPEN(FILFLD,FILE=FLDNAM(:NAMLEN+4),STATUS='NEW')
               WRITE (FILFLD,3628)
               WRITE (FILFLD,3630) BZ0*(1.0D4)
               WRITE (FILFLD,3635)
            ENDIF
            PRINT *, ('  ')
 3628       FORMAT ('#')
            PRINT 3630, BZ0*(1.0D4)
 3630       FORMAT ('# Field value in the center of the shield is ',
     1              G16.10, ' Gauss')
C
            PRINT *, ('  ')
            PRINT 3635
 3635       FORMAT('#'/'#',5X, 'z (mm)', 6X, 'r (mm)', 7X, 'Bz (G)', 9X,
     1             'Br (G)', 7X, 'Btot(G)', 5X, 'Homog (ppm)'/'#')
C
C           (* loop for radius and a prevention from negative radii (=ERROR) *)
            RBEG = DABS(RBEG)
            REND = DABS(REND)
            R = DMIN1(RBEG,REND)
            DELTAR = DABS(DELTAR)
            REND = DMAX1(RBEG,REND) + DELTAR/10
            IF (DELTAR .EQ. 0.0) THEN
               R = REND
               DELTAR = 1.0
            ENDIF
C           (* bogi make sure that the calculation is done up to ZEND *)
C           (*                                 and in low-high order: *)
            Z = DMIN1(ZBEG,ZEND)
            DELTAZ = DABS(DELTAZ)
            ZEND = DMAX1(ZBEG,ZEND) + DELTAZ/10.0
            ZBEG = Z
            IF (DELTAZ .EQ. 0.0) THEN
               Z = ZEND
               DELTAZ = 1.0
            ENDIF
 3639       CONTINUE
Cbogi          continue field calculation until R=REND
               Z = ZBEG
 3640          CONTINUE
C                 (* repeat field calculation until Z = ZEND *)
                  CALL FIELD(Z,R,NN,MAX,NSHI,LENSHI,RADSHI,CENSHI,
     1                    RHOLE,CURSHI,CENSOL,LENSOL,RADSOL,LOOPS,
     2                    LAYERS,WIRDIA,FOILTH,CUR,BEXT,BZ,BR)
                  IF (BZ0.NE.0) HOMOG = (1.0D6)*(BZ-BZ0)/BZ0
                  TOTAL = DSQRT(BR*BR+BZ*BZ)
                  WRITE(FILFLD,3645) Z*1000, R*1000, BZ*1D4, BR*1D4,
     1                          TOTAL*1D4, HOMOG
                  PRINT 3645, Z*1000, R*1000, BZ*1D4, BR*1D4,
     1                          TOTAL*1D4, HOMOG
 3645             FORMAT(2X,F10.4,2X,F10.4,4X,G14.7,2X,G14.7,2X,G14.7,
     1                          4X, G14.7)
C
                  Z = Z + DELTAZ
                  IF (Z .LE. ZEND) GOTO 3640
               CONTINUE
            WRITE(FILFLD,3646)
 3646       FORMAT('')
            R = R + DELTAR
            IF (R .LE. REND) GOTO 3639
            CONTINUE
C
         ELSE IF ((ANSW .EQ. 'X') .OR. (ANSW .EQ. 'x'))THEN
C              (* flux profile *)
            CALL ASK(2, R, DELTAR, REND, ZBEG, DELTAZ, ZEND)
C                 (* ask the profile coordinates *)
C
            IF ( .NOT. FLXLOG) THEN
               FLXLOG = .TRUE.
               OPEN(FILFLX,FILE=FLXNAM(:NAMLEN+4),STATUS='NEW')
               WRITE(FILFLX,3650)
            ENDIF
            PRINT *, ('  ')
            PRINT 3650
Cbogi '#' fuer GNUplot--Kompatibilitaet:
 3650       FORMAT('#'/'#',4X, 'z (mm)', 4X, 'r (mm)', 3X,
     1                'Flux (Gauss*cm*cm)'/'#')
C
Cbogi make sure that the calculation is done up to ZEND and in low-high order:
            Z = DMIN1(ZBEG,ZEND)
            DELTAZ = DABS(DELTAZ)
            ZEND = DMAX1(ZBEG,ZEND) + DELTAZ/10.0
            IF (DELTAZ .EQ. 0.0) Z = ZEND
C
 3655       CONTINUE
C                 (* repeat flux calculation until Z = ZEND *)
               IF (R .NE. 0)
     1            CALL FLUX(Z, R, NN, MAX, NSHI, LENSHI, RADSHI,
     2                      CENSHI, RHOLE, CURSHI, CENSOL, LENSOL,
     3                      RADSOL, LOOPS, LAYERS, WIRDIA, FOILTH, CUR,
     4                      BEXT, MAGFLU)
               IF (R .EQ. 0) POTEN = 0
               WRITE(FILFLX,3660) Z*1000, R*1000, MAGFLU*1D8
               PRINT 3660, Z*1000, R*1000, MAGFLU*1D8
 3660          FORMAT(2X, F8.2 , 2X, F8.2, F14.5)
C
               Z = Z + DELTAZ
               IF (Z. LE. ZEND) GOTO 3655
            CONTINUE
            WRITE(FILFLX,3661)
 3661       FORMAT('')
C
         ELSE
C              (* current distribution in the shield *)
            IF ( (.NOT. CURLOG) .AND. (NSHI .NE. 0) ) THEN
               CURLOG = .TRUE.
               OPEN(FILCUR,FILE=CURNAM(:NAMLEN+4),STATUS='NEW')
               CALL DISTRI(NN, MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE,
     1                  CURSHI, FILCUR)
            ENDIF
C
         ENDIF
C
 3665    CONTINUE
            PRINT 3670
 3670       FORMAT('Do you want to continue with the same ',
     1             'parameters (Y/N)?')
            READ(*,'(A1)') ANSW
            IF ((ANSW .NE. 'Y') .AND. (ANSW .NE. 'y') .AND.
     1          (ANSW .NE. 'N') .AND. (ANSW .NE. 'n')) GOTO 3665
         CONTINUE
         IF ((ANSW .EQ. 'Y') .OR. (ANSW .EQ. 'y')) GOTO 3615
      CONTINUE
C
      OPEN(FILDSC,FILE=DSCNAM(:NAMLEN+4),STATUS='NEW')
      CALL WRSHI(FILDSC, MAX, NSHI, LENSHI, RADSHI, RHOLE, CENSHI,
     1           TRAFLX)
      CALL WROTH(FILDSC, MAX, LENSOL, RADSOL, CENSOL, LAYERS,
     1           LOOPS, SYMSOL, CUR, WIRDIA, FOILTH, BEXT, TRAFLX,
     2           BZ0*(1.0D4))
C           (* write the input parameters to the result file *)
C
      IF ( FLXLOG ) CLOSE(FILFLX)
      IF ( FLDLOG ) CLOSE(FILFLD)
      IF ( CURLOG ) CLOSE(FILCUR)
      CLOSE(FILDSC)
      RETURN
      END
C
C
C     ****************************************************************
C
C
C     MAIN PROGRAM
C
C           (* This  program  calculates  the magnetic field of a *)
C           (* collection of coaxial solenoids and cylindrical or *)
C           (* planar  superconducting  shield parts. Each of the *)
C           (* shield  parts  is  approximated by an equal number *)
C           (* of  current  rings, whose total number is NN (or a *)
C           (* little less).                                      *)
C
      INTEGER NN, MAX
C           (* NN : the  maximum number of current rings used for *)
C           (*      approximating the current distribution in the *)
C           (*      superconducting shield                        *)
C           (* MAX : the  maximum  number of solenoids and shield *)
C           (*       parts                                        *)
C
      PARAMETER(NN=100, MAX=30)
C
      INTEGER NSHI, LAYERS(MAX)
      DOUBLE PRECISION LENSHI(MAX), RADSHI(MAX), RHOLE(MAX),
     1              TRAFLX(MAX),
     2              CENSHI(MAX), LENSOL(MAX),RADSOL(MAX), CENSOL(MAX),
     3              LOOPS(MAX), CUR(MAX), WIRDIA, FOILTH, BEXT,
     4              CURSHI(NN), M(NN,NN), A(NN), RWKSP(2*NN*NN+4*NN)
      CHARACTER*1 ANSW
      LOGICAL DIFSHI, SYMSHI(MAX), SYMSOL(MAX)
C
      COMMON /WORKSP/ RWKSP
C                  ...............................
C
      IF (NN .GE. 35) CALL IWKIN(2*NN*NN+4*NN)
C           (* total amount of automatically allocated space is *)
C           (* 2500  double  precision units; when workspace is *)
C           (* needed more, it must be allocated using IWKIN    *)
C
 9910 CONTINUE
         CALL DIMENS(NSHI, DIFSHI, LENSHI, RADSHI, RHOLE, CENSHI,
     1             SYMSHI, LENSOL, RADSOL, CENSOL, LAYERS, LOOPS,
     2             SYMSOL, CUR, WIRDIA, FOILTH, BEXT, TRAFLX, MAX)
C              (* ask the input parameters *)
C
         IF (NSHI .NE. 0)
C              (* if there is a superconducting shield *)
     1      CALL CURREN(NN, MAX, NSHI, DIFSHI, LENSHI, RADSHI, CENSHI,
     2                  RHOLE, SYMSHI, LENSOL, RADSOL, CENSOL, LAYERS,
     3                  LOOPS, SYMSOL, CUR, WIRDIA, FOILTH, BEXT,
     4                  TRAFLX, M, A, CURSHI)
C                 (* calculate the current distribution in the shield *)
C
         CALL RESULT(NN, MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE,
     1               CURSHI, CENSOL, LENSOL, RADSOL, LOOPS, LAYERS,
     2               SYMSOL, WIRDIA, FOILTH, CUR, BEXT, TRAFLX)
C              (* calculate and print the magnetic field profile etc. *)
C
 9920    CONTINUE
            PRINT 9930
 9930       FORMAT ('Do you want to continue (Y/N)?')
            READ(*,'(A1)') ANSW
            IF ((ANSW .NE. 'Y') .AND. (ANSW .NE. 'y') .AND.
     1          (ANSW .NE. 'N') .AND. (ANSW .NE. 'n')) GOTO 9920
         CONTINUE
         IF ((ANSW .EQ. 'Y') .OR. (ANSW .EQ. 'y')) GOTO 9910
      CONTINUE
C
      STOP
      END
