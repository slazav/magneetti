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
