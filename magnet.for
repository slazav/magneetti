C                                                                       COI00010                                                                       COI00020
      SUBROUTINE ZERO(MATRIX, DIM1, DIM2)                               COI00030
C                                                                       COI00040
C           (* Makes every component of MATRIX(DIM1,DIM2) zero.   *)    COI00050
C                                                                       COI00060
      INTEGER DIM1, DIM2                                                COI00070
      DOUBLE PRECISION MATRIX(DIM1, DIM2)                               COI00080
C                                                                       COI00090
      INTEGER I, J                                                      COI00100
C                  ...............................                      COI00110
C                                                                       COI00120
      DO 1020 I = 1, DIM1                                               COI00130
         DO 1010 J = 1, DIM2                                            COI00140
            MATRIX(I,J) = 0D0                                           COI00150
 1010    CONTINUE                                                       COI00160
 1020 CONTINUE                                                          COI00170
C                                                                       COI00180
      RETURN                                                            COI00190
      END                                                               COI00200
C                                                                       COI00210
C                                                                       COI00220
C     ****************************************************************  COI00230
C                                                                       COI00240
C                                                                       COI00250
      DOUBLE PRECISION FUNCTION ELLIP3(N, K)                            COI00260
C                                                                       COI00270
C           (* Calculates complete elliptic integral of the third *)    COI00280
C           (* kind   using   IMSL-functions   DELRF   and  DELRJ *)    COI00290
C           (* (incomplete  elliptic  integrals  of the first and *)    COI00300
C           (* third  kind, respectively). The input parameters N *)    COI00310
C           (* and  K are the parameters of the complete elliptic *)    COI00320
C           (* integral  of third kind when its normal definition *)    COI00330
C           (* is used.                                           *)    COI00340
C                                                                       COI00350
      DOUBLE PRECISION N, K                                             COI00360
C                                                                       COI00370
      DOUBLE PRECISION DELRF, DELRJ                                     COI00380
C                  ...............................                      COI00390
C                                                                       COI00400
      ELLIP3 = DELRF(0D0, 1D0-K, 1D0) +                                 COI00410
     1         DELRJ(0D0, 1D0-K, 1D0, 1D0-N)*N/3D0                      COI00420
C                                                                       COI00430
      RETURN                                                            COI00440
      END                                                               COI00450
C                                                                       COI00460
C                                                                       COI00470
C     ****************************************************************  COI00480
C                                                                       COI00490
C                                                                       COI00500
      DOUBLE PRECISION FUNCTION FDSKBZ(X)                               COI00510
C                                                                       COI00520
C           (* When integrated over X from 0 to PI and multiplied *)    COI00530
C           (* by  the  current  density, this function gives the *)    COI00540
C           (* z-component  of the magnetic field produced by the *)    COI00550
C           (* current disk defined in FUNCTION DSKBZ.            *)    COI00560
C                                                                       COI00570
      DOUBLE PRECISION X, KSIF, RF, RSOUF, WF                           COI00580
      COMMON /AREA/ KSIF, RF, RSOUF, WF                                 COI00590
C           (* X : theta-component of the source point            *)    COI00600
C           (* KSIF : z-component  of  the vector from the center *)    COI00610
C           (*        of the source ring to the field point       *)    COI00620
C           (* RF : r-coordinate of the field point               *)    COI00630
C           (* RSOUF : the "average radius" of the source ring    *)    COI00640
C           (* WF : the width of the source ring                  *)    COI00650
C                                                                       COI00660
      DOUBLE PRECISION C, RAD1, RAD2, K1, K2, T1, T2, S1, S2            COI00670
C           (* C : cos(X)                                         *)    COI00680
C           (* RAD1, RAD2 : inner and outer radius of the current *)    COI00690
C           (*              disk, respectively                    *)    COI00700
C           (* K1, K2, T1, T2, S1, S2 : defined in formulae below *)    COI00710
C                  ...............................                      COI00720
C                                                                       COI00730
      C = DCOS(X)                                                       COI00740
C                                                                       COI00750
      RAD1 = RSOUF - WF/2D0                                             COI00760
      RAD2 = RSOUF + WF/2D0                                             COI00770
C                                                                       COI00780
      K1 = DSQRT(RAD1*RAD1 + RF*RF + KSIF*KSIF - 2D0*RAD1*RF*C)         COI00790
      K2 = DSQRT(RAD2*RAD2 + RF*RF + KSIF*KSIF - 2D0*RAD2*RF*C)         COI00800
C                                                                       COI00810
      T1 = KSIF*KSIF + RAD1*RAD1 + 2D0*RF*RF - 3D0*RAD1*RF*C            COI00820
      T2 = KSIF*KSIF + RAD2*RAD2 + 2D0*RF*RF - 3D0*RAD2*RF*C            COI00830
C                                                                       COI00840
      S1 = RF*C*C*(((RAD1*RAD1+RF*RF)*C-RAD1*RF*(1D0+C*C))/K1 - C*K1) / COI00850
     1                                     (KSIF*KSIF + RF*RF*(1D0-C*C))COI00860
      S2 = RF*C*C*(((RAD2*RAD2+RF*RF)*C-RAD2*RF*(1D0+C*C))/K2 - C*K2) / COI00870
     1                                     (KSIF*KSIF + RF*RF*(1D0-C*C))COI00880
C                                                                       COI00890
      FDSKBZ = 2D-7 * ((T2/K2-T1/K1)*C/RF + S2 - S1                     COI00900
     1                 + 2D0*C*C*DLOG((K2+RAD2-RF*C)/(K1+RAD1-RF*C)))   COI00910
C                                                                       COI00920
      RETURN                                                            COI00930
      END                                                               COI00940
C                                                                       COI00950
C                                                                       COI00960
C     ****************************************************************  COI00970
C                                                                       COI00980
C                                                                       COI00990
      DOUBLE PRECISION FUNCTION FDSKBR(X)                               COI01000
C                                                                       COI01010
C           (* When integrated over X from 0 to PI and multiplied *)    COI01020
C           (* by  the  current  density, this function gives the *)    COI01030
C           (* r-component  of the magnetic field produced by the *)    COI01040
C           (* current disk defined in FUNCTION DSKBR.            *)    COI01050
C                                                                       COI01060
      DOUBLE PRECISION X, KSIF, RF, RSOUF, WF                           COI01070
      COMMON /AREA/ KSIF, RF, RSOUF, WF                                 COI01080
C           (* X : theta-component of the source point            *)    COI01090
C           (* KSIF : z-component  of  the vector from the center *)    COI01100
C           (*        of the source ring to the field point       *)    COI01110
C           (* RF : r-coordinate of the field point               *)    COI01120
C           (* RSOUF : the "average radius" of the source ring    *)    COI01130
C           (* WF : the width of the source ring                  *)    COI01140
C                                                                       COI01150
      DOUBLE PRECISION C, RAD1, RAD2, K1, K2                            COI01160
C           (* C : cos(X)                                         *)    COI01170
C           (* RAD1, RAD2 : inner and outer radius of the current *)    COI01180
C           (*              disk, respectively                    *)    COI01190
C           (* K1, K2 : defined in formulae below                 *)    COI01200
C                  ...............................                      COI01210
C                                                                       COI01220
      C = DCOS(X)                                                       COI01230
C                                                                       COI01240
      RAD1 = RSOUF - WF/2D0                                             COI01250
      RAD2 = RSOUF + WF/2D0                                             COI01260
C                                                                       COI01270
      K1 = DSQRT(RAD1*RAD1 + RF*RF + KSIF*KSIF - 2D0*RAD1*RF*C)         COI01280
      K2 = DSQRT(RAD2*RAD2 + RF*RF + KSIF*KSIF - 2D0*RAD2*RF*C)         COI01290
C                                                                       COI01300
      FDSKBR = 2D-7*KSIF*(C/(KSIF*KSIF+RF*RF*(1-C*C))) *                COI01310
     1       ((-KSIF*KSIF-RF*RF+RAD2*RF*C)/K2 -                         COI01320
     2        (-KSIF*KSIF-RF*RF+RAD1*RF*C)/K1)                          COI01330
C                                                                       COI01340
      RETURN                                                            COI01350
      END                                                               COI01360
C                                                                       COI01370
C                                                                       COI01380
C     ****************************************************************  COI01390
C                                                                       COI01400
C                                                                       COI01410
      DOUBLE PRECISION FUNCTION FDSPOT(X)                               COI01420
C                                                                       COI01430
C           (* When integrated over X from 0 to PI, multiplied by *)    COI01440
C           (* the  current  density  and  divided  by  WF,  this *)    COI01450
C           (* function  gives  the  vector potential produced by *)    COI01460
C           (* the current disk defined in FUNCTION DSKPOT.       *)    COI01470
C                                                                       COI01480
      DOUBLE PRECISION X, KSIF, RF, RSOUF, WF                           COI01490
      COMMON /AREA/ KSIF, RF, RSOUF, WF                                 COI01500
C           (* X : theta-component of the source point            *)    COI01510
C           (* KSIF : z-component  of  the vector from the center *)    COI01520
C           (*        of the source ring to the field point       *)    COI01530
C           (* RF : r-coordinate of the field point               *)    COI01540
C           (* RSOUF : the "average radius" of the source ring    *)    COI01550
C           (* WF : the width of the source ring                  *)    COI01560
C                                                                       COI01570
      DOUBLE PRECISION RAD1, RAD2, C, K1, K2, S1, S2                    COI01580
C           (* RAD1, RAD2 : inner and outer radius of the current *)    COI01590
C           (*              disk, respectively                    *)    COI01600
C           (* C : cos(X)                                         *)    COI01610
C           (* K1, K2, S1, S2 : defined in formulae below         *)    COI01620
C                                                                       COI01630
      C = DCOS(X)                                                       COI01640
C                                                                       COI01650
      RAD1 = RSOUF - WF/2D0                                             COI01660
      RAD2 = RSOUF + WF/2D0                                             COI01670
C                                                                       COI01680
      K1 = DSQRT(RAD1*RAD1 + RF*RF + KSIF*KSIF - 2*RAD1*RF*C)           COI01690
      K2 = DSQRT(RAD2*RAD2 + RF*RF + KSIF*KSIF - 2*RAD2*RF*C)           COI01700
C                                                                       COI01710
      S1 = RAD1 - RF*C + K1                                             COI01720
      S2 = RAD2 - RF*C + K2                                             COI01730
C                                                                       COI01740
      IF (((S1 .LE. 0) .AND. (S2 .GT. 0)) .OR.                          COI01750
     1    ((S2 .LE. 0) .AND. (S1 .GT. 0))) THEN                         COI01760
         FDSPOT = 0                                                     COI01770
      ELSE                                                              COI01780
         FDSPOT = (2D-7) * C * (K2 - K1 + RF*C*DLOG(S2/S1))             COI01790
      ENDIF                                                             COI01800
C                                                                       COI01810
                                                                        COI01820
      RETURN                                                            COI01830
      END                                                               COI01840
C                                                                       COI01850
C                                                                       COI01860
C     ****************************************************************  COI01870
C                                                                       COI01880
C                                                                       COI01890
      DOUBLE PRECISION FUNCTION CYLBZ(ZZ, R, RAD, CURDEN, LENGTH)       COI01900
C                                                                       COI01910
C           (* Gives   the  z-component  of  the  magnetic  field *)    COI01920
C           (* produced  by  a  cylindrical  current sheet, whose *)    COI01930
C           (* radius  is  RAD, current density is CURDEN, length *)    COI01940
C           (* is  LENGTH,  and  whose  center  is  at origo. The *)    COI01950
C           (* central axis of the cylinder is the z-axis.        *)    COI01960
C           (* The  field  point  in this coordinate system is at *)    COI01970
C           (* (z=ZZ, r=R).                                       *)    COI01980
C                                                                       COI01990
      DOUBLE PRECISION ZZ, R, RAD, CURDEN, LENGTH                       COI02000
C                                                                       COI02010
      DOUBLE PRECISION KSI1, KSI2, R1, R2, K1, K2, C, DELK, ELLIP3      COI02020
C           (* KSI1, KSI2 : integration   starting  and  stopping *)    COI02030
C           (*              points                                *)    COI02040
C           (* R1, R2, K1, K2, C : defined in formulae below      *)    COI02050
C           (* DELK : complete  elliptic  integral  of  1st  kind *)    COI02060
C           (*        (IMSL)                                      *)    COI02070
C           (* ELLIP3 : complete elliptic integral of 3rd kind    *)    COI02080
C                  ...............................                      COI02090
C                                                                       COI02100
      KSI1 = ZZ - LENGTH/2D0                                            COI02110
      KSI2 = ZZ + LENGTH/2D0                                            COI02120
      R1 = (RAD+R)*(RAD+R) + KSI1*KSI1                                  COI02130
      R2 = (RAD+R)*(RAD+R) + KSI2*KSI2                                  COI02140
      K1 = 4D0*RAD*R/R1                                                 COI02150
      K2 = 4D0*RAD*R/R2                                                 COI02160
      C = 4D0*RAD*R/((RAD+R)*(RAD+R))                                   COI02170
C                                                                       COI02180
      CYLBZ = ((ELLIP3(C,K2)*(RAD-R) + DELK(K2)*(RAD+R))*KSI2*DSQRT(K2) COI02190
     1       - (ELLIP3(C,K1)*(RAD-R) + DELK(K1)*(RAD+R))*KSI1*DSQRT(K1))COI02200
     2      * (1D-7)*CURDEN*DSQRT(C)/(2*R*RAD)                          COI02210
C                                                                       COI02220
      RETURN                                                            COI02230
      END                                                               COI02240
C                                                                       COI02250
C                                                                       COI02260
C     ****************************************************************  COI02270
C                                                                       COI02280
C                                                                       COI02290
      DOUBLE PRECISION FUNCTION CYLBR(ZZ, R, RAD, CURDEN, LENGTH)       COI02300
C                                                                       COI02310
C           (* As  FUNCTION CYLBZ, but this gives the r-component *)    COI02320
C           (* of the field.                                      *)    COI02330
C                                                                       COI02340
      DOUBLE PRECISION ZZ, R, RAD, CURDEN, LENGTH                       COI02350
C                                                                       COI02360
      DOUBLE PRECISION KSI1, KSI2, R1, R2, K1, K2, DELK, DELE           COI02370
C           (* KSI1, KSI2 : integration   starting  and  stopping *)    COI02380
C           (*              points                                *)    COI02390
C           (* R1, R2, K1, K2 : defined in formulae below         *)    COI02400
C           (* DELK : complete  elliptic  integral  of  1st  kind *)    COI02410
C           (*        (IMSL)                                      *)    COI02420
C           (* DELE : complete  elliptic  integral  of  2nd  kind *)    COI02430
C           (*        (IMSL)                                      *)    COI02440
C                  ...............................                      COI02450
C                                                                       COI02460
      KSI1 = ZZ - LENGTH/2D0                                            COI02470
      KSI2 = ZZ + LENGTH/2D0                                            COI02480
      R1 = (RAD+R)*(RAD+R) + KSI1*KSI1                                  COI02490
      R2 = (RAD+R)*(RAD+R) + KSI2*KSI2                                  COI02500
      K1 = 4D0*RAD*R/R1                                                 COI02510
      K2 = 4D0*RAD*R/R2                                                 COI02520
C                                                                       COI02530
      CYLBR = ((2D0*DELE(K2)-(2D0-K2)*DELK(K2))*DSQRT(R2) -             COI02540
     1         (2D0*DELE(K1)-(2D0-K1)*DELK(K1))*DSQRT(R1)) *            COI02550
     2        (1D-7)*CURDEN/R                                           COI02560
C                                                                       COI02570
      RETURN                                                            COI02580
      END                                                               COI02590
C                                                                       COI02600
C                                                                       COI02610
C     ****************************************************************  COI02620
C                                                                       COI02630
C                                                                       COI02640
      DOUBLE PRECISION FUNCTION CYLBAX(ZZ, RAD, CURDEN, LENGTH)         COI02650
C                                                                       COI02660
C           (* Gives  the  z-component  of  the magnetic field on *)    COI02670
C           (* the  z-axis,  produced  by  a  cylindrical current *)    COI02680
C           (* sheet,  whose  radius  is  RAD, current density is *)    COI02690
C           (* CURDEN,  and length is LENGTH. The central axis of *)    COI02700
C           (* the  cylinder is the z-axis, and it is centered at *)    COI02710
C           (* z=0.  The field point in this coordinate system is *)    COI02720
C           (* at (z=ZZ, r=0).                                    *)    COI02730
C                                                                       COI02740
      DOUBLE PRECISION ZZ, RAD, CURDEN, LENGTH                          COI02750
C                                                                       COI02760
      DOUBLE PRECISION DCONST, PI                                       COI02770
C           (* DCONST : an IMSL-function for scientific constants *)    COI02780
C           (* PI : 3.14159....                                   *)    COI02790
C                  ...............................                      COI02800
C                                                                       COI02810
      PI = DCONST('PI')                                                 COI02820
      CYLBAX = 2D0*PI*(1D-7)*CURDEN *                                   COI02830
     1         ((ZZ+LENGTH/2D0) /                                       COI02840
     2               DSQRT((ZZ+LENGTH/2D0)*(ZZ+LENGTH/2D0) + RAD**2) -  COI02850
     3          (ZZ-LENGTH/2D0) /                                       COI02860
     4               DSQRT((ZZ-LENGTH/2D0)*(ZZ-LENGTH/2D0) + RAD**2))   COI02870
C                                                                       COI02880
      RETURN                                                            COI02890
      END                                                               COI02900
C                                                                       COI02910
C                                                                       COI02920
C     ****************************************************************  COI02930
C                                                                       COI02940
C                                                                       COI02950
      DOUBLE PRECISION FUNCTION DSKBZ(ZZ, RFIELD, RSOUR, CURDEN, WIDTH) COI02960
C                                                                       COI02970
C           (* Calculates  the  z-component of the magnetic field *)    COI02980
C           (* produced  by  a current disk located perpendicular *)    COI02990
C           (* to  the  z-axis  and  centered to origo. The outer *)    COI03000
C           (* radius  of the disk is RSOUR+WIDTH/2, and it has a *)    COI03010
C           (* hole of radius RSOUR-WIDTH/2 in the center.        *)    COI03020
C           (* The  field  point  in this coordinate system is at *)    COI03030
C           (* (z=ZZ, r=RFIELD).                                  *)    COI03040
C                                                                       COI03050
      DOUBLE PRECISION WIDTH, RSOUR, RFIELD, ZZ, CURDEN                 COI03060
C                                                                       COI03070
      DOUBLE PRECISION KSIF, RF, RSOUF, WF, ERRABS, ERREL, ERREST,      COI03080
     1                 PI, RAD1, RAD2, RESULT, DCONST,FDSKBZ            COI03090
      COMMON /AREA/ KSIF, RF, RSOUF, WF                                 COI03100
      EXTERNAL FDSKBZ                                                   COI03110
C           (* KSIF, RF, RSOUF, WF : new  names  for  ZZ, RFIELD, *)    COI03120
C           (*                       RSOUR  and WIDTH ; needed in *)    COI03130
C           (*                       FUNCTION FDSKBZ              *)    COI03140
C           (* ERRABS, ERREL, ERREST : required   and   estimated *)    COI03150
C           (*                         integration accuracies     *)    COI03160
C           (* PI : 3.14159....                                   *)    COI03170
C           (* RAD1, RAD2 : the  inner  and  outer  radius of the *)    COI03180
C           (*              current disk, respectively            *)    COI03190
C           (* RESULT : the  result  of  integration  of FUNCTION *)    COI03200
C           (*          FDSKBZ(X) FROM 0 TO PI                    *)    COI03210
C           (* DCONST : an IMSL-function for scientific constants *)    COI03220
C           (* FDSKBZ : the function which is integrated here     *)    COI03230
C                  ...............................                      COI03240
C                                                                       COI03250
      PI = DCONST('PI')                                                 COI03260
C                                                                       COI03270
      IF (RFIELD .EQ. 0) THEN                                           COI03280
C           (* if the field point is on central axis *)                 COI03290
         RAD1 = RSOUR - WIDTH/2D0                                       COI03300
         RAD2 = RSOUR + WIDTH/2D0                                       COI03310
         DSKBZ = (RAD1/DSQRT(RAD1*RAD1+ZZ*ZZ) -                         COI03320
     1            RAD2/DSQRT(RAD2*RAD2+ZZ*ZZ) +                         COI03330
     2            DLOG((DSQRT(RAD2*RAD2+ZZ*ZZ)+RAD2)/                   COI03340
     3                 (DSQRT(RAD1*RAD1+ZZ*ZZ)+RAD1))) *                COI03350
     4           PI*2D-7*CURDEN                                         COI03360
C                                                                       COI03370
      ELSE                                                              COI03380
C           (* if the field point not on central axis *)                COI03390
         ERRABS = 0D0                                                   COI03400
         ERREL = 1.0D-8                                                 COI03410
         KSIF = ZZ                                                      COI03420
         RF = RFIELD                                                    COI03430
         RSOUF = RSOUR                                                  COI03440
         WF = WIDTH                                                     COI03450
         CALL = DQDAGS(FDSKBZ, 0D0, PI, ERRABS, ERREL, RESULT, ERREST)  COI03460
C              (* integrate FUNCTION FDSKBZ(X) from 0 to PI *)          COI03470
         DSKBZ = RESULT*CURDEN                                          COI03480
      ENDIF                                                             COI03490
C                                                                       COI03500
      RETURN                                                            COI03510
      END                                                               COI03520
C                                                                       COI03530
C                                                                       COI03540
C     ****************************************************************  COI03550
C                                                                       COI03560
C                                                                       COI03570
      DOUBLE PRECISION FUNCTION DSKBR(ZZ, RFIELD, RSOUR, CURDEN, WIDTH) COI03580
C                                                                       COI03590
C           (* As  FUNCTION DSKBZ, but this gives the r-component *)    COI03600
C           (* of the field.                                      *)    COI03610
C                                                                       COI03620
      DOUBLE PRECISION WIDTH, RSOUR, RFIELD, ZZ, CURDEN                 COI03630
C                                                                       COI03640
      DOUBLE PRECISION KSIF, RF, RSOUF, WF, ERRABS, ERREL, ERREST,      COI03650
     1                 PI, RESULT, DCONST, FDSKBR                       COI03660
      COMMON /AREA/ KSIF, RF, RSOUF, WF                                 COI03670
      EXTERNAL FDSKBR                                                   COI03680
C           (* KSIF, RF, RSOUF, WF : new  names  for  ZZ, RFIELD, *)    COI03690
C           (*                       RSOUR  and WIDTH ; needed in *)    COI03700
C           (*                       FUNCTION FDSKBR              *)    COI03710
C           (* ERRABS, ERREL, ERREST : required   and   estimated *)    COI03720
C           (*                         integration accuracies     *)    COI03730
C           (* PI : 3.14159....                                   *)    COI03740
C           (* RESULT : the  result  of  integration  of FUNCTION *)    COI03750
C           (*          FDSKBR(X) FROM 0 TO PI                    *)    COI03760
C           (* DCONST : an IMSL-function for scientific constants *)    COI03770
C           (* FDSKBR : the function which is integrated here     *)    COI03780
C                  ...............................                      COI03790
C                                                                       COI03800
      PI = DCONST('PI')                                                 COI03810
C                                                                       COI03820
      IF (RFIELD .EQ. 0) THEN                                           COI03830
C           (* if the field point is on central axis *)                 COI03840
         DSKBR = 0                                                      COI03850
C                                                                       COI03860
      ELSE                                                              COI03870
C           (* if the field point not on central axis *)                COI03880
         ERRABS = 0D0                                                   COI03890
         ERREL = 1.0D-8                                                 COI03900
         KSIF = ZZ                                                      COI03910
         RF = RFIELD                                                    COI03920
         RSOUF = RSOUR                                                  COI03930
         WF = WIDTH                                                     COI03940
         CALL = DQDAGS(FDSKBR, 0D0, PI, ERRABS, ERREL, RESULT, ERREST)  COI03950
C              (* integrate FUNCTION FDSKBR(X) from 0 to PI *)          COI03960
         DSKBR = RESULT*CURDEN                                          COI03970
      ENDIF                                                             COI03980
C                                                                       COI03990
      RETURN                                                            COI04000
      END                                                               COI04010
C                                                                       COI04020
C                                                                       COI04030
C     ****************************************************************  COI04040
C                                                                       COI04050
C                                                                       COI04060
      DOUBLE PRECISION FUNCTION CYLSOU(KSI, R, RAD)                     COI04070
C                                                                       COI04080
C           (* (CYLSOU(KSI2, ...)-CYLSOU(KSI1, ...))*CURDEN gives *)    COI04090
C           (* the  vector  potential  produced  by a cylindrical *)    COI04100
C           (* current   sheet,  whose  radius  is  RAD,  current *)    COI04110
C           (* density  is  CURDEN, and whose center is at origo. *)    COI04120
C           (* The central axis of the cylinder is the z-axis.    *)    COI04130
C           (* The field point is as defined in FUNCTION CYLPOT.  *)    COI04140
C                                                                       COI04150
      DOUBLE PRECISION KSI, R, RAD                                      COI04160
C                                                                       COI04170
      DOUBLE PRECISION R1, K, C, DELK, DELE, ELLIP3, DMACH              COI04180
C           (* R1, K, C : defined in formulae below               *)    COI04190
C           (* DELK : complete elliptic integral of 1st kind      *)    COI04200
C           (*        (IMSL)                                      *)    COI04210
C           (* DELE : complete elliptic integral of 2nd kind      *)    COI04220
C           (*        (IMSL)                                      *)    COI04230
C           (* ELLIP3 : complete elliptic integral of 3rd kind    *)    COI04240
C           (* DMACH(1) : the  smallest  positive  number  of the *)    COI04250
C           (*            computer (IMSL)                         *)    COI04260
C                  ...............................                      COI04270
C                                                                       COI04280
      R1 = (RAD+R)*(RAD+R) + KSI*KSI                                    COI04290
      K = 4D0*RAD*R/R1                                                  COI04300
      C = 4D0*RAD*R/((RAD+R)*(RAD+R))                                   COI04310
C                                                                       COI04320
      IF ((1D0-C) .GE. (5*DMACH(1))**(1D0/3D0)) THEN                    COI04330
C           (* if C<>1 *)                                               COI04340
         CYLSOU = (DELK(K)*(R1+(RAD-R)*(RAD-R)) -                       COI04350
     1              ELLIP3(C,K)*(RAD-R)*(RAD-R) - DELE(K)*R1)           COI04360
     2            * (1D-7)*KSI/(R*DSQRT(R1))                            COI04370
C                                                                       COI04380
      ELSE                                                              COI04390
         IF ((1D0-K) .GE. (10*DMACH(1))) THEN                           COI04400
C              (* if C=1 and K<>1 *)                                    COI04410
            CYLSOU = (DELK(K)-DELE(K)) * (1D-7)*KSI*DSQRT(R1)/R         COI04420
C                                                                       COI04430
         ELSE                                                           COI04440
C              (* if C=K=1 *)                                           COI04450
            CYLSOU = 0D0                                                COI04460
         ENDIF                                                          COI04470
      ENDIF                                                             COI04480
C                                                                       COI04490
      RETURN                                                            COI04500
      END                                                               COI04510
C                                                                       COI04520
C                                                                       COI04530
C     ****************************************************************  COI04540
C                                                                       COI04550
C                                                                       COI04560
      DOUBLE PRECISION FUNCTION CYLPOT(HEIGHT, RSOUR, RFIELD, ZZ)       COI04570
C                                                                       COI04580
C           (* Calculates  the  vector potential per unit current *)    COI04590
C           (* density,  produced  by a cylindrical current sheet *)    COI04600
C           (* whose  radius  is  RSOUR and length is HEIGHT, and *)    COI04610
C           (* which  is  centered  at origo. The central axis of *)    COI04620
C           (* the cylider is the z-axis.                         *)    COI04630
C           (* The  field  point  in this coordinate system is at *)    COI04640
C           (* (z=ZZ, r=RFIELD).                                  *)    COI04650
C                                                                       COI04660
      DOUBLE PRECISION HEIGHT, RSOUR, RFIELD, ZZ                        COI04670
C                                                                       COI04680
      DOUBLE PRECISION KSI1, KSI2, CYLSOU                               COI04690
C           (* KSI1, KSI2, CYLSOU : CYLPOT = CYLSOU(KSI2, ....) - *)    COI04700
C           (*                               CYLSOU(KSI1, ....)   *)    COI04710
C                  ...............................                      COI04720
C                                                                       COI04730
      KSI1 = ZZ - HEIGHT/2D0                                            COI04740
      KSI2 = ZZ + HEIGHT/2D0                                            COI04750
C                                                                       COI04760
      CYLPOT = CYLSOU(KSI2, RFIELD, RSOUR) - CYLSOU(KSI1, RFIELD, RSOUR)COI04770
C                                                                       COI04780
      RETURN                                                            COI04790
      END                                                               COI04800
C                                                                       COI04810
C                                                                       COI04820
C     ****************************************************************  COI04830
C                                                                       COI04840
C                                                                       COI04850
      DOUBLE PRECISION FUNCTION DSKPOT(WIDTH, RSOUR, RFIELD, ZZ)        COI04860
C                                                                       COI04870
C           (* Calculates  the  vector potential per unit current *)    COI04880
C           (* density,   produced  by  a  current  disk  located *)    COI04890
C           (* perpendicular   to  the  z-axis  and  centered  to *)    COI04900
C           (* origo.  The  outer  radius  of  the disk is RSOUR+ *)    COI04910
C           (* WIDTH/2, and it has a hole of radius RSOUR-WIDTH/2 *)    COI04920
C           (* in the center.                                     *)    COI04930
C           (* The  field  point  in this coordinate system is at *)    COI04940
C           (* (z=ZZ, r=RFIELD).                                  *)    COI04950
C                                                                       COI04960
      DOUBLE PRECISION WIDTH, RSOUR, RFIELD, ZZ                         COI04970
C                                                                       COI04980
      DOUBLE PRECISION KSIF, RF, RSOUF, WF, ERRABS, ERREL, ERREST,      COI04990
     1                 PI, RESULT, DCONST, FDSPOT                       COI05000
      COMMON /AREA/ KSIF, RF, RSOUF, WF                                 COI05010
      EXTERNAL FDSPOT                                                   COI05020
C           (* KSIF, RF, RSOUF, WF : new  names  for  ZZ, RFIELD, *)    COI05030
C           (*                       RSOUR  and WIDTH ; needed in *)    COI05040
C           (*                       FUNCTION FDSPOT              *)    COI05050
C           (* ERRABS, ERREL, ERREST : required   and   estimated *)    COI05060
C           (*                         integration accuracies     *)    COI05070
C           (* PI : 3.14159....                                   *)    COI05080
C           (* RESULT : the  result  of  integration  of FUNCTION *)    COI05090
C           (*          FDSPOT(X) from 0 to PI                    *)    COI05100
C           (* DCONST : an IMSL-function for scientific constants *)    COI05110
C           (* FDSPOT : the function which is integrated here     *)    COI05120
C                  ...............................                      COI05130
C                                                                       COI05140
      PI = DCONST('PI')                                                 COI05150
C                                                                       COI05160
      ERRABS = 0D0                                                      COI05170
      ERREL = 1.0D-8                                                    COI05180
      KSIF = ZZ                                                         COI05190
      RF = RFIELD                                                       COI05200
      RSOUF = RSOUR                                                     COI05210
      WF = WIDTH                                                        COI05220
      CALL = DQDAGS(FDSPOT, 0D0, PI, ERRABS, ERREL, RESULT, ERREST)     COI05230
C           (* integrate FUNCTION FDSPOT(X) from 0 to PI *)             COI05240
      DSKPOT = RESULT                                                   COI05250
C                                                                       COI05260
      RETURN                                                            COI05270
      END                                                               COI05280
C                                                                       COI05290
C                                                                       COI05300
C     ****************************************************************  COI05310
C                                                                       COI05320
C                                                                       COI05330
      SUBROUTINE SOLCYL(LENSHI, RADSHI, CENSHI, LENSOL, RADSOL, CENSOL, COI05340
     1                 LAYERS, LOOPS, CUR, WIRDIA, FOILTH, N, ASTOR, NN)COI05350
C                                                                       COI05360
C           (* Calculates  the vector potential distribution on a *)    COI05370
C           (* cylinder  part of the superconducting shield, pro- *)    COI05380
C           (* duced  by a solenoid coil. The result is stored in *)    COI05390
C           (* the vector ASTOR(NN).                              *)    COI05400
C           (*                                                    *)    COI05410
C           (* The meanings of the input parameters are:          *)    COI05420
C           (*     LENSOL,  RADSOL,  LAYERS,  LOOPS, CUR, WIRDIA, *)    COI05430
C           (*        FOILTH : as in SUBROUTINE BSOLEN            *)    COI05440
C           (*     LENSHI : the length of the shield part         *)    COI05450
C           (*     RADSHI : the radius of the shield part         *)    COI05460
C           (*     CENSHI : the z-coordinate of the center of the *)    COI05470
C           (*              shield part                           *)    COI05480
C           (*     CENSOL : the z-coordinate of the center of the *)    COI05490
C           (*              solenoid coil                         *)    COI05500
C           (*     N : the  amount  of  rings  the shield part is *)    COI05510
C           (*         divided to                                 *)    COI05520
C           (*     ASTOR : the  vector  in  which  the  result is *)    COI05530
C           (*             stored                                 *)    COI05540
C           (*     NN : dimension of ASTOR                        *)    COI05550
C                                                                       COI05560
      INTEGER LAYERS, N, NN                                             COI05570
      DOUBLE PRECISION LENSHI, RADSHI, LENSOL, RADSOL, CENSOL, LOOPS,   COI05580
     1                 CUR, WIRDIA, FOILTH, ASTOR(NN)                   COI05590
C                                                                       COI05600
      INTEGER ILAY, J                                                   COI05610
      DOUBLE PRECISION CURDEN, RADLAY, Z, ZZ, CYLPOT                    COI05620
C           (* ILAY : the layer number                            *)    COI05630
C           (* J : the  index  of  the current ring of the shield *)    COI05640
C           (*     part                                           *)    COI05650
C           (* CURDEN : the current density in the solenoid       *)    COI05660
C           (* RADLAY : the  radius  of  the  ILAYth layer in the *)    COI05670
C           (*          solenoid coil                             *)    COI05680
C           (* Z : the  z-coordinate  of  the Jth current ring of *)    COI05690
C           (*     the shield cylinder*)                                COI05700
C           (* ZZ : z-component  of the vector from the center of *)    COI05710
C           (*      the  solenoid  coil  to the center of the Jth *)    COI05720
C           (*      current ring of the cylinder*)                      COI05730
C           (* CYLPOT : gives  the  vector  potential produced by *)    COI05740
C           (*          one layer of the solenoid                 *)    COI05750
C                  ...............................                      COI05760
C                                                                       COI05770
      CURDEN = CUR*LOOPS/LENSOL                                         COI05780
      RADLAY = RADSOL + WIRDIA/2D0                                      COI05790
C                                                                       COI05800
      DO 2320 ILAY = 1, LAYERS                                          COI05810
         DO 2310 J = 1, N                                               COI05820
            Z = CENSHI + LENSHI/2D0 - (J-0.5)*LENSHI/N                  COI05830
            ZZ = Z - CENSOL                                             COI05840
            ASTOR(J) = ASTOR(J) +                                       COI05850
     1                 CYLPOT(LENSOL, RADLAY, RADSHI, ZZ)*CURDEN        COI05860
 2310    CONTINUE                                                       COI05870
         RADLAY = RADLAY + WIRDIA + FOILTH                              COI05880
 2320 CONTINUE                                                          COI05890
C                                                                       COI05900
      RETURN                                                            COI05910
      END                                                               COI05920
C                                                                       COI05930
C                                                                       COI05940
C     ****************************************************************  COI05950
C                                                                       COI05960
C                                                                       COI05970
      SUBROUTINE SOLDSK(ZDSK, RADSHI, RHOLE, LENSOL, RADSOL, CENSOL,    COI05980
     1                 LAYERS, LOOPS, CUR, WIRDIA, FOILTH, N, ASTOR, NN)COI05990
C                                                                       COI06000
C           (* Calculates  the vector potential distribution on a *)    COI06010
C           (* disk  part of the superconducting shield, produced *)    COI06020
C           (* by  a  solenoid  coil. The result is stored in the *)    COI06030
C           (* vector ASTOR(NN).                                  *)    COI06040
C           (*                                                    *)    COI06050
C           (* The meanings of the input parameters are:          *)    COI06060
C           (*     LENSOL,  RADSOL,  LAYERS,  LOOPS, CUR, WIRDIA, *)    COI06070
C           (*        FOILTH : as in SUBROUTINE BSOLEN            *)    COI06080
C           (*     ZDSK : the z-coordinate of the shield disk     *)    COI06090
C           (*     RADSHI : the outer radius of the shield disk   *)    COI06100
C           (*     RHOLE : the  radius  of the hole in the center *)    COI06110
C           (*             of the shield disk                     *)    COI06120
C           (*     CENSOL : the z-coordinate of the center of the *)    COI06130
C           (*              solenoid coil                         *)    COI06140
C           (*     N : the  amount  of  rings  the shield disk is *)    COI06150
C           (*         divided to                                 *)    COI06160
C           (*     ASTOR : the  vector  in  which  the  result is *)    COI06170
C           (*             stored                                 *)    COI06180
C           (*     NN : dimension of ASTOR                        *)    COI06190
C                                                                       COI06200
      INTEGER LAYERS, N, NN                                             COI06210
      DOUBLE PRECISION ZDSK, RADSHI, RHOLE, LENSOL, RADSOL, CENSOL,     COI06220
     1          LOOPS, CUR, WIRDIA, FOILTH, ASTOR(NN)                   COI06230
C                                                                       COI06240
      INTEGER ILAY, J                                                   COI06250
      DOUBLE PRECISION CURDEN, RADLAY, ZZ, RFIELD, CYLPOT               COI06260
C           (* ILAY : the layer number                            *)    COI06270
C           (* J : the index of the current ring of the disk      *)    COI06280
C           (* CURDEN : the current density in the solenoid       *)    COI06290
C           (* RADLAY : the  radius  of  the  ILAYth layer in the *)    COI06300
C           (*          solenoid coil                             *)    COI06310
C           (* ZZ : z-component  of the vector from the center of *)    COI06320
C           (*      the solenoid coil to the shield disk          *)    COI06330
C           (* RFIELD : r-component of the field point            *)    COI06340
C           (* CYLPOT : gives  the  vector  potential produced by *)    COI06350
C           (*          one layer of the solenoid                 *)    COI06360
C                  ...............................                      COI06370
C                                                                       COI06380
      CURDEN = CUR*LOOPS/LENSOL                                         COI06390
      RADLAY = RADSOL + WIRDIA/2D0                                      COI06400
C                                                                       COI06410
      DO 2420 ILAY = 1, LAYERS                                          COI06420
         DO 2410 J = 1, N                                               COI06430
            RFIELD = RHOLE + (J-0.5)*(RADSHI-RHOLE)/N                   COI06440
            ZZ = ZDSK - CENSOL                                          COI06450
            ASTOR(J) = ASTOR(J) +                                       COI06460
     1                 CYLPOT(LENSOL, RADLAY, RFIELD, ZZ)*CURDEN        COI06470
 2410    CONTINUE                                                       COI06480
         RADLAY = RADLAY + WIRDIA + FOILTH                              COI06490
 2420 CONTINUE                                                          COI06500
C                                                                       COI06510
      RETURN                                                            COI06520
      END                                                               COI06530
C                                                                       COI06540
C                                                                       COI06550
C     ****************************************************************  COI06560
C                                                                       COI06570
C                                                                       COI06580
      SUBROUTINE BSOLEN(ZZ, R, LENSOL, RADSOL, WIRDIA, LOOPS, LAYERS,   COI06590
     1                  CUR, FOILTH, BZSOL, BRSOL)                      COI06600
C                                                                       COI06610
C           (* Gives  the  magnetic field produced by a solenoid, *)    COI06620
C           (* whose  central  axis  is  the  z-axis and which is *)    COI06630
C           (* centered  to  z=0. The field point in this coordi- *)    COI06640
C           (* nate system is at (z=ZZ, r=R).                     *)    COI06650
C           (* The meanings of the input parameters are:          *)    COI06660
C           (*     ZZ : z-coordinate of the field point           *)    COI06670
C           (*     R : r-coordinate of the field point            *)    COI06680
C           (*     LENSOL : the length of the solenoid            *)    COI06690
C           (*     RADSOL : the inner radius of the solenoid      *)    COI06700
C           (*     WIRDIA : the diameter of the current lead      *)    COI06710
C           (*     LOOPS : the number of turns in one layer       *)    COI06720
C           (*     LAYERS : the number of layers in the solenoid  *)    COI06730
C           (*     CUR : the current in the lead                  *)    COI06740
C           (*     FOILTH : the  thickness of the insulating foil *)    COI06750
C           (*              between the layers                    *)    COI06760
C           (*     BZSOL : the z-component of the magnetic field  *)    COI06770
C           (*     BRSOL : the r-component of the magnetic field  *)    COI06780
C                                                                       COI06790
      INTEGER LAYERS                                                    COI06800
      DOUBLE PRECISION ZZ, R, LENSOL, RADSOL, WIRDIA, LOOPS, CUR,       COI06810
     1                 FOILTH, BZSOL, BRSOL, CYLBAX                     COI06820
C                                                                       COI06830
      INTEGER I                                                         COI06840
      DOUBLE PRECISION RADLAY, CURDEN, CYLBZ, CYLBR                     COI06850
C           (* I : the layer number                               *)    COI06860
C           (* RADLAY : the radius of the Ith layer               *)    COI06870
C           (* CURDEN : the current density in the solenoid       *)    COI06880
C           (* CYLBZ, CYLBR : give the z- and r-components of the *)    COI06890
C           (*                magnetic   field  produced  by  one *)    COI06900
C           (*                layer of the solenoid               *)    COI06910
C           (* CYLBAX : gives the magnetic field on central axis  *)    COI06920
C                  ...............................                      COI06930
C                                                                       COI06940
      RADLAY = RADSOL + WIRDIA/2D0                                      COI06950
      CURDEN = LOOPS*CUR/LENSOL                                         COI06960
C                                                                       COI06970
      DO 2510 I = 1, LAYERS                                             COI06980
C                                                                       COI06990
         IF (R .EQ. 0) THEN                                             COI07000
C              (* if the field point is on central axis *)              COI07010
            BZSOL = BZSOL + CYLBAX(ZZ, RADLAY, CURDEN, LENSOL)          COI07020
C                                                                       COI07030
         ELSE                                                           COI07040
C              (* if the field point not on central axis *)             COI07050
            BZSOL = BZSOL + CYLBZ(ZZ, R, RADLAY, CURDEN, LENSOL)        COI07060
            BRSOL = BRSOL + CYLBR(ZZ, R, RADLAY, CURDEN, LENSOL)        COI07070
         ENDIF                                                          COI07080
C                                                                       COI07090
         RADLAY = RADLAY + WIRDIA + FOILTH                              COI07100
 2510 CONTINUE                                                          COI07110
C                                                                       COI07120
      RETURN                                                            COI07130
      END                                                               COI07140
C                                                                       COI07150
C                                                                       COI07160
C     ****************************************************************  COI07170
C                                                                       COI07180
C                                                                       COI07190
      SUBROUTINE WRSHI(FILE, MAX, NSHI, LENSHI, RADSHI, RHOLE, CENSHI)  COI07200
C                                                                       COI07210
C           (* Writes  the dimensions of the shield parts to file *)    COI07220
C           (* number FILE ; FILE=6 corresponds to the terminal.  *)    COI07230
C                                                                       COI07240
      INTEGER FILE, MAX, NSHI                                           COI07250
      DOUBLE PRECISION LENSHI(MAX), RADSHI(MAX), RHOLE(MAX), CENSHI(MAX)COI07260
C                                                                       COI07270
      INTEGER ISHI                                                      COI07280
C           (* ISHI : an index for the shield parts               *)    COI07290
C                  ...............................                      COI07300
C                                                                       COI07310
      IF (NSHI .EQ. 0) THEN                                             COI07320
C           (* if no shield *)                                          COI07330
         WRITE(FILE,2610)                                               COI07340
 2610    FORMAT(' Now you have no shield at all.')                      COI07350
C                                                                       COI07360
      ELSE                                                              COI07370
C           (* if there is a shield *)                                  COI07380
         DO 2680 ISHI = 1, NSHI                                         COI07390
            WRITE (FILE,*) ' '                                          COI07400
            IF (LENSHI(ISHI) .EQ. 0) WRITE(FILE,2620) ISHI              COI07410
 2620       FORMAT(1X,'Shield part number ',I2,' is a disk, whose',     COI07420
     1             ' dimensions are: ')                                 COI07430
            IF (LENSHI(ISHI) .NE. 0) WRITE(FILE,2630) ISHI              COI07440
 2630       FORMAT(1X,'Shield part number ',I2,' is a cylinder, whose', COI07450
     1             ' dimensions are: ')                                 COI07460
C                                                                       COI07470
            IF (LENSHI(ISHI) .NE. 0) WRITE(FILE,2640) ISHI,             COI07480
     1                                               LENSHI(ISHI)*1000  COI07490
C                 (* length of cylindrical shield part *)               COI07500
 2640       FORMAT(5X,'LENSHI(',I2,') = ',F7.3,' mm')                   COI07510
C                                                                       COI07520
            WRITE(FILE,2650) ISHI, RADSHI(ISHI)*1000                    COI07530
C                 (* radius of cylindrical and outer radius of *)       COI07540
C                 (* planar shield part                        *)       COI07550
 2650       FORMAT(5X,'RADSHI(',I2,') = ',F7.3,' mm')                   COI07560
C                                                                       COI07570
            IF (LENSHI(ISHI) .EQ. 0) WRITE(FILE,2660) ISHI,             COI07580
     1                                               RHOLE(ISHI)*1000   COI07590
C                 (* radius of the hole in the planar shield part *)    COI07600
 2660       FORMAT(5X,'RHOLE(',I2,') = ',F7.3,' mm')                    COI07610
C                                                                       COI07620
            WRITE(FILE,2670) ISHI, CENSHI(ISHI)*1000                    COI07630
C                 (* z-coordinate of the center of the shield part *)   COI07640
 2670       FORMAT(5X,'CENSHI(',I2,') = ',F7.3,' mm')                   COI07650
C                                                                       COI07660
 2680    CONTINUE                                                       COI07670
      ENDIF                                                             COI07680
C                                                                       COI07690
      RETURN                                                            COI07700
      END                                                               COI07710
C                                                                       COI07720
C                                                                       COI07730
C     ****************************************************************  COI07740
C                                                                       COI07750
C                                                                       COI07760
      SUBROUTINE WROTH(FILE, MAX, LENSOL, RADSOL, CENSOL, LAYERS, LOOPS,COI07770
     1                 SYMSOL, CUR, WIRDIA, FOILTH, BEXT, TRAFLU)       COI07780
C                                                                       COI07790
C           (* Writes  the  dimensions  of the solenoid coils and *)    COI07800
C           (* the  other  parameters  of  the system, except the *)    COI07810
C           (* dimensions  of  the  shield  parts, to file number *)    COI07820
C           (* FILE ; FILE=6 corresponds to the terminal.         *)    COI07830
C                                                                       COI07840
      INTEGER FILE, MAX, LAYERS(MAX)                                    COI07850
      DOUBLE PRECISION LENSOL(MAX), RADSOL(MAX), CENSOL(MAX),           COI07860
     1                LOOPS(MAX), CUR(MAX), WIRDIA, FOILTH, BEXT, TRAFLUCOI07870
      LOGICAL SYMSOL(MAX)                                               COI07880
C                                                                       COI07890
      INTEGER ISOL                                                      COI07900
C           (* ISOL : an index for the solenoid parts             *)    COI07910
C                  ...............................                      COI07920
C                                                                       COI07930
      IF (LENSOL(1) .NE. 0) THEN                                        COI07940
C           (* if there are solenoids *)                                COI07950
         ISOL = 1                                                       COI07960
C                                                                       COI07970
 2710    CONTINUE                                                       COI07980
            WRITE(FILE,*) ('  ')                                        COI07990
            WRITE(FILE,2720) ISOL, LENSOL(ISOL)*1000, ISOL,             COI08000
     1                 RADSOL(ISOL)*1000, ISOL, CENSOL(ISOL)*1000, ISOL,COI08010
     2                 LAYERS(ISOL), ISOL, LOOPS(ISOL), ISOL,           COI08020
     3                 SYMSOL(ISOL), ISOL, CUR(ISOL)                    COI08030
C                 (* length,  inner  radius, z-coordinate of the *)     COI08040
C                 (* center, number of layers, number of current *)     COI08050
C                 (* loops in one layer, is there a similar part *)     COI08060
C                 (* located symmetrically, current in the wire  *)     COI08070
 2720       FORMAT(1X,'LENSOL(',I2,') = ',F7.3,' mm' / 1X,'RADSOL(',I2, COI08080
     1          ') = ',F7.3,' mm' / 1X,'CENSOL(',I2,') = ',F7.3,' mm' / COI08090
     2          1X,'LAYERS(',I2,') =',I4 / 1X,'LOOPS(',I2,') = ',       COI08100
     3          F7.3, / 1X,'SYMSOL(',I2,') = ',L2 / 1X,'CUR(',I2,       COI08110
     4          ') = ',F7.3,' A')                                       COI08120
            ISOL = ISOL + 1                                             COI08130
            IF (LENSOL(ISOL) .NE. 0) GOTO 2710                          COI08140
C                 (* if there are more solenoids *)                     COI08150
         CONTINUE                                                       COI08160
C                                                                       COI08170
         WRITE(FILE,*) ('  ')                                           COI08180
         WRITE(FILE,2730) WIRDIA*1000                                   COI08190
C              (* the diameter of the wire *)                           COI08200
 2730    FORMAT(1X,'WIRDIA = ',F7.4,' mm')                              COI08210
C                                                                       COI08220
         WRITE(FILE,2740) FOILTH*1000                                   COI08230
C              (* the thickness of insulating foil between layers *)    COI08240
 2740    FORMAT(1X,'FOILTH = ',F7.4,' mm')                              COI08250
C                                                                       COI08260
      ELSE                                                              COI08270
C           (* if no solenoids *)                                       COI08280
         WRITE(FILE,*) ('  ')                                           COI08290
         WRITE(FILE,2750)                                               COI08300
 2750    FORMAT(' Now you have no solenoids. ')                         COI08310
      ENDIF                                                             COI08320
C                                                                       COI08330
      WRITE(FILE,*) ('  ')                                              COI08340
      IF (BEXT .NE. 0) THEN                                             COI08350
C           (* if there is external field *)                            COI08360
         WRITE(FILE,2760) BEXT*10000                                    COI08370
C              (* external field in Gausses *)                          COI08380
 2760    FORMAT(' BEXT = ', G9.4, ' Gauss ')                            COI08390
      ELSE                                                              COI08400
C           (* if no external field *)                                  COI08410
         WRITE(FILE,2770)                                               COI08420
 2770    FORMAT(' BEXT = 0.0 Gauss ')                                   COI08430
      ENDIF                                                             COI08440
C                                                                       COI08450
      IF (TRAFLU .NE. 0) THEN                                           COI08460
C           (* if there is trapped flux in the shield *)                COI08470
         WRITE(FILE,2780) TRAFLU*1D8                                    COI08480
C              (* trapped flux in the shield in Gauss*cm*cm *)          COI08490
 2780    FORMAT(' FLUX = ', G9.4,' Gauss*cm*cm  ')                      COI08500
      ELSE                                                              COI08510
C           (* if no trapped flux *)                                    COI08520
         WRITE(FILE,2790)                                               COI08530
 2790    FORMAT(' FLUX = 0.0 Gauss*cm*cm  ')                            COI08540
      ENDIF                                                             COI08550
C                                                                       COI08560
      RETURN                                                            COI08570
      END                                                               COI08580
C                                                                       COI08590
C                                                                       COI08600
C     ****************************************************************  COI08610
C                                                                       COI08620
C                                                                       COI08630
      SUBROUTINE DIMENS(NSHI, DIFSHI, LENSHI, RADSHI, RHOLE, CENSHI,    COI08640
     1             SYMSHI, LENSOL, RADSOL, CENSOL, LAYERS, LOOPS,       COI08650
     2             SYMSOL, CUR, WIRDIA, FOILTH, BEXT, TRAFLU, MAX)      COI08660
C                                                                       COI08670
C           (* Asks the dimensions, currents etc. of the system.  *)    COI08680
C                                                                       COI08690
      INTEGER MAX, NSHI, LAYERS(MAX)                                    COI08700
      DOUBLE PRECISION LENSHI(MAX), RADSHI(MAX), RHOLE(MAX),            COI08710
     1               CENSHI(MAX), LENSOL(MAX), RADSOL(MAX), CENSOL(MAX),COI08720
     2               LOOPS(MAX), CUR(MAX), WIRDIA, FOILTH, BEXT, TRAFLU COI08730
      LOGICAL DIFSHI, SYMSHI(MAX), SYMSOL(MAX)                          COI08740
C                                                                       COI08750
      INTEGER ISHI, ISOL                                                COI08760
      LOGICAL HOLE                                                      COI08770
      CHARACTER*1 ANSW                                                  COI08780
C           (* ISHI : index of the shield part                    *)    COI08790
C           (* ISOL : index of the solenoid                       *)    COI08800
C           (* HOLE : .TRUE. if there is a hole through the whole *)    COI08810
C           (*        shield,  so  that  there  can  be a trapped *)    COI08820
C           (*        flux ; .FALSE. otherwise                    *)    COI08830
C                  ...............................                      COI08840
C                                                                       COI08850
      WRITE(6,2801)                                                     COI08860
 2801 FORMAT('0This  program  calculates  magnetic  fields  produced ', COI08870
     1'by  a system of' / ' cocentric  coils  in a superconducting ',   COI08880
     2'shield. The shield must consist' / ' of  cocentric cylinders ',  COI08890
     3'and circular disks. There can be a hole in the' /                COI08900
     4' center of each disk.' / '0All dimensions are given in mm:s|')   COI08910
C                                                                       COI08920
 2802 CONTINUE                                                          COI08930
C                                                                       COI08940
      WRITE(6,*) ('  ')                                                 COI08950
      CALL WRSHI(6, MAX, NSHI, LENSHI, RADSHI, RHOLE, CENSHI)           COI08960
C                                                                       COI08970
      WRITE(6,2803)                                                     COI08980
 2803 FORMAT('0Do you want to change the shield (Y = yes, N = no)?')    COI08990
C                                                                       COI09000
 2804 CONTINUE                                                          COI09010
         REWIND 5                                                       COI09020
         READ(5, '(A1)', ERR=2804, END=2804) ANSW                       COI09030
         IF ((ANSW .NE. 'N') .AND. (ANSW .NE. 'Y')) WRITE(6,2805)       COI09040
 2805    FORMAT(' Give either "Y" or "N" |')                            COI09050
         IF ((ANSW .NE. 'N') .AND. (ANSW .NE. 'Y')) GOTO 2804           COI09060
C              (* repeat asking until the answer is acceptable *)       COI09070
      CONTINUE                                                          COI09080
C                                                                       COI09090
      IF (ANSW .EQ. 'Y') THEN                                           COI09100
C           (* new shield *)                                            COI09110
         DIFSHI = .TRUE.                                                COI09120
C                                                                       COI09130
         ISHI = 1                                                       COI09140
C                                                                       COI09150
         WRITE(6,2806)                                                  COI09160
 2806    FORMAT('0Have you any superconducting shield ',                COI09170
     1          '(Y = yes, N = no)?')                                   COI09180
C                                                                       COI09190
 2807    CONTINUE                                                       COI09200
            REWIND 5                                                    COI09210
            READ(5, '(A1)', ERR=2807, END=2807) ANSW                    COI09220
            IF ((ANSW .NE. 'N') .AND. (ANSW .NE. 'Y')) WRITE(6,2808)    COI09230
 2808       FORMAT(' Give either "Y" or "N" |')                         COI09240
            IF ((ANSW .NE. 'N') .AND. (ANSW .NE. 'Y')) GOTO 2807        COI09250
         CONTINUE                                                       COI09260
C                                                                       COI09270
         IF (ANSW .EQ. 'N') GOTO 2827                                   COI09280
C              (* if no shield at all *)                                COI09290
C                                                                       COI09300
 2809    CONTINUE                                                       COI09310
 2810       CONTINUE                                                    COI09320
            WRITE(6,2811) ISHI, ISHI, LENSHI(ISHI)*1000                 COI09330
 2811       FORMAT('0Give the length of the shield part number ',I2,    COI09340
     1             ' (LENSHI(',I2,') = ',F6.2,' mm). ' / ' If the',     COI09350
     2             ' part is a disk, give 0. ')                         COI09360
            REWIND 5                                                    COI09370
            READ(5, *, ERR=2810, END=2812) LENSHI(ISHI)                 COI09380
C                 (* REWIND 5 & END=2812  =>  if only Enter is given, *)COI09390
C                 (* the value of LENSHI will not be changed          *)COI09400
            LENSHI(ISHI) = LENSHI(ISHI)/1000D0                          COI09410
C                 (* change the dimension from mm:s to m:s *)           COI09420
C                                                                       COI09430
 2812       CONTINUE                                                    COI09440
C                                                                       COI09450
            WRITE(6,2813) ISHI, ISHI, RADSHI(ISHI)*1000                 COI09460
 2813       FORMAT('0Give the radius of the shield part number ',I2,    COI09470
     1             ' (RADSHI(',I2,') = ',F6.2,' mm).' / ' If the',      COI09480
     2             ' part is a disk, give its outer radius. ')          COI09490
            REWIND 5                                                    COI09500
            READ(5, *, ERR=2812, END=2814) RADSHI(ISHI)                 COI09510
            RADSHI(ISHI) = RADSHI(ISHI)/1000D0                          COI09520
C                                                                       COI09530
 2814       CONTINUE                                                    COI09540
C                                                                       COI09550
            IF (LENSHI(ISHI) .EQ. 0) THEN                               COI09560
C                 (* if the shield part is a disk *)                    COI09570
 2815          CONTINUE                                                 COI09580
               WRITE(6,2816) ISHI, RHOLE(ISHI)*1000                     COI09590
 2816          FORMAT('0Give the radius of the hole in the disk',       COI09600
     1                ' (RHOLE(',I2,') = ', F6.2, ' mm). ')             COI09610
               REWIND 5                                                 COI09620
               READ(5, *, ERR=2815, END=2817) RHOLE(ISHI)               COI09630
               RHOLE(ISHI) = RHOLE(ISHI)/1000D0                         COI09640
 2817          CONTINUE                                                 COI09650
            ENDIF                                                       COI09660
C                                                                       COI09670
 2818       CONTINUE                                                    COI09680
C                                                                       COI09690
            WRITE(6,2819) ISHI, ISHI, CENSHI(ISHI)*1000                 COI09700
 2819       FORMAT('0Give the z-coordinate of the center of the ',      COI09710
     1             'shield part number ',I2 / ' (CENSHI(',I2,') = ',    COI09720
     2             F6.2,' mm). ')                                       COI09730
            REWIND 5                                                    COI09740
            READ(5, *, ERR=2818, END=2820) CENSHI(ISHI)                 COI09750
            CENSHI(ISHI) = CENSHI(ISHI)/1000D0                          COI09760
C                                                                       COI09770
 2820       CONTINUE                                                    COI09780
C                                                                       COI09790
            IF (((ABS(CENSHI(ISHI)) - LENSHI(ISHI)/2) .GE. 0) .AND.     COI09800
     1          (CENSHI(ISHI) .NE. 0)) THEN                             COI09810
C                 (* if there is space for a symmetrical shield part *) COI09820
 2821          CONTINUE                                                 COI09830
                  WRITE(6,2822) -CENSHI(ISHI)*1000, ISHI, SYMSHI(ISHI)  COI09840
 2822             FORMAT('0Is there a similar part centered to z = ',   COI09850
     1                   F6.2, ' mm (Y/N)?' / ' If there is one, ',     COI09860
     2                   'don''t give its dimensions any more|' /       COI09870
     3                   ' Now SYMSHI(', I2, ') = ', L2, '. ')          COI09880
                  REWIND 5                                              COI09890
                  READ(5, '(A1)', ERR=2821, END=2823) ANSW              COI09900
                  IF ((ANSW .NE. 'Y') .AND. (ANSW .NE. 'N')) GOTO 2821  COI09910
               CONTINUE                                                 COI09920
               SYMSHI(ISHI) = (ANSW .EQ. 'Y')                           COI09930
 2823          CONTINUE                                                 COI09940
            ELSE                                                        COI09950
C                 (* if no space for a symmetrical part *)              COI09960
               SYMSHI(ISHI) = .FALSE.                                   COI09970
            ENDIF                                                       COI09980
C                                                                       COI09990
            IF (SYMSHI(ISHI)) THEN                                      COI10000
C                 (* the index for symmetrical part will be ISHI+1 *)   COI10010
               LENSHI(ISHI+1) = LENSHI(ISHI)                            COI10020
               RADSHI(ISHI+1) = RADSHI(ISHI)                            COI10030
               RHOLE(ISHI+1) = RHOLE(ISHI)                              COI10040
               CENSHI(ISHI+1) = -CENSHI(ISHI)                           COI10050
            ENDIF                                                       COI10060
C                                                                       COI10070
            IF (.NOT. SYMSHI(ISHI)) THEN                                COI10080
               ISHI = ISHI + 1                                          COI10090
            ELSE                                                        COI10100
               ISHI = ISHI + 2                                          COI10110
            ENDIF                                                       COI10120
C                                                                       COI10130
            WRITE(6,2824)                                               COI10140
 2824       FORMAT('0Are there any more shield parts ',                 COI10150
     1             '(Y = yes, N = no)?')                                COI10160
C                                                                       COI10170
 2825       CONTINUE                                                    COI10180
               REWIND 5                                                 COI10190
               READ(5, '(A1)', ERR=2825, END=2825) ANSW                 COI10200
               IF ((ANSW .NE. 'N') .AND. (ANSW .NE. 'Y')) WRITE(6,2826) COI10210
 2826          FORMAT(' Give either "Y" or "N" |')                      COI10220
               IF ((ANSW .NE. 'N') .AND. (ANSW .NE. 'Y')) GOTO 2825     COI10230
            CONTINUE                                                    COI10240
C                                                                       COI10250
            IF (ANSW .EQ. 'Y') GOTO 2809                                COI10260
C              (* if there are more shield parts *)                     COI10270
         CONTINUE                                                       COI10280
C                                                                       COI10290
 2827    CONTINUE                                                       COI10300
C                                                                       COI10310
         NSHI = ISHI - 1                                                COI10320
C              (* now the value of NSHI will be the actual number *)    COI10330
C              (* of shield parts                                 *)    COI10340
C                                                                       COI10350
      ELSE                                                              COI10360
C           (* if the shield was not changed *)                         COI10370
         DIFSHI = .FALSE.                                               COI10380
      ENDIF                                                             COI10390
C                                                                       COI10400
      ISOL = 1                                                          COI10410
C                                                                       COI10420
 2828 CONTINUE                                                          COI10430
         WRITE(6,2829) ISOL, ISOL, LENSOL(ISOL)*1000                    COI10440
 2829    FORMAT('0Give the length of the solenoid number ', I2,         COI10450
     1          ' (LENSOL(', I2, ') = ', F6.2, ' mm). ' / ' If no ',    COI10460
     2          'more solenoids, give 0. ')                             COI10470
         REWIND 5                                                       COI10480
         READ(5, *, ERR=2828, END=2830) LENSOL(ISOL)                    COI10490
C              (* REWIND 5 & END=2830  =>  if only Enter is given, *)   COI10500
C              (* the value of LENSOL will not be changed          *)   COI10510
         LENSOL(ISOL) = LENSOL(ISOL)/1000D0                             COI10520
C                 (* change the dimension from mm:s to m:s *)           COI10530
 2830    CONTINUE                                                       COI10540
C                                                                       COI10550
         IF (LENSOL(ISOL) .EQ. 0) GOTO 2845                             COI10560
C              (* if no more solenoids *)                               COI10570
C                                                                       COI10580
         WRITE(6,2831) ISOL, ISOL, RADSOL(ISOL)*1000                    COI10590
 2831    FORMAT('0Give the inner radius of the solenoid number ',I2,    COI10600
     1          ' (RADSOL(',I2,') = ', F6.2, ' mm). ')                  COI10610
         REWIND 5                                                       COI10620
         READ(5, *, ERR=2830, END=2832) RADSOL(ISOL)                    COI10630
         RADSOL(ISOL) = RADSOL(ISOL)/1000D0                             COI10640
C                                                                       COI10650
 2832    CONTINUE                                                       COI10660
C                                                                       COI10670
         WRITE(6,2833) ISOL, ISOL, CENSOL(ISOL)*1000                    COI10680
 2833    FORMAT('0Give the center of the solenoid number ',I2,          COI10690
     1          ' (CENSOL(',I2,') = ', F6.2, ' mm). ')                  COI10700
         REWIND 5                                                       COI10710
         READ(5, *, ERR=2832, END=2834) CENSOL(ISOL)                    COI10720
         CENSOL(ISOL) = CENSOL(ISOL)/1000D0                             COI10730
C                                                                       COI10740
 2834    CONTINUE                                                       COI10750
C                                                                       COI10760
         WRITE(6,2835) ISOL, ISOL, LAYERS(ISOL)                         COI10770
 2835    FORMAT('0Give the number of layers in the solenoid ',          COI10780
     1          'number ',I2,' (LAYERS(',I2,') = ', I2, '). ')          COI10790
         REWIND 5                                                       COI10800
         READ(5, *, ERR=2834, END=2836) LAYERS(ISOL)                    COI10810
C                                                                       COI10820
 2836    CONTINUE                                                       COI10830
C                                                                       COI10840
         WRITE(6,2837) ISOL, ISOL, LOOPS(ISOL)                          COI10850
 2837    FORMAT('0Give the number of turns per layer in the solenoid',  COI10860
     1          ' number ', I2 / ' (LOOPS(', I2, ') = ', F6.2, '). ')   COI10870
         REWIND 5                                                       COI10880
         READ(5, *, ERR=2836, END=2838) LOOPS(ISOL)                     COI10890
C                                                                       COI10900
 2838    CONTINUE                                                       COI10910
C                                                                       COI10920
 2839    CONTINUE                                                       COI10930
         WRITE(6,2840) ISOL, ISOL, CUR(ISOL)                            COI10940
 2840    FORMAT('0Give the current in the solenoid number ', I2,        COI10950
     1          ' in Amps (CUR(', I2, ') = ', F6.3, '). ')              COI10960
         REWIND 5                                                       COI10970
         READ(5, *, ERR=2839, END=2841) CUR(ISOL)                       COI10980
C                                                                       COI10990
 2841    CONTINUE                                                       COI11000
C                                                                       COI11010
         IF ((ABS(CENSOL(ISOL)) - LENSOL(ISOL)/2) .GE. 0) THEN          COI11020
C              (* if there is space for a symmetrical solenoid *)       COI11030
 2842       CONTINUE                                                    COI11040
               WRITE(6,2843) -CENSOL(ISOL)*1000, ISOL, SYMSOL(ISOL)     COI11050
 2843          FORMAT('0Is there a similar solenoid with the same ',    COI11060
     1              'current centered to z = ', F6.2, ' mm' / ' (Y/N)?',COI11070
     2               ' If there is one, don''t give its dimensions ',   COI11080
     3               'any more|' / ' Now SYMSOL(', I2, ') = ', L2, '. ')COI11090
               REWIND 5                                                 COI11100
               READ(5, '(A1)', ERR=2842, END=2844) ANSW                 COI11110
               IF ((ANSW .NE. 'Y') .AND. (ANSW .NE. 'N')) GOTO 2842     COI11120
            CONTINUE                                                    COI11130
            SYMSOL(ISOL) = (ANSW .EQ. 'Y')                              COI11140
 2844       CONTINUE                                                    COI11150
C                                                                       COI11160
         ELSE                                                           COI11170
C              (* if no space for a symmetrical solenoid *)             COI11180
            SYMSOL(ISOL) = .FALSE.                                      COI11190
         ENDIF                                                          COI11200
C                                                                       COI11210
         IF (SYMSOL(ISOL)) THEN                                         COI11220
C              (* the index for symmetrical coil will be ISOL+1 *)      COI11230
            LENSOL(ISOL+1) = LENSOL(ISOL)                               COI11240
            RADSOL(ISOL+1) = RADSOL(ISOL)                               COI11250
            CENSOL(ISOL+1) = -CENSOL(ISOL)                              COI11260
            LAYERS(ISOL+1) = LAYERS(ISOL)                               COI11270
            LOOPS(ISOL+1) = LOOPS(ISOL)                                 COI11280
            SYMSOL(ISOL+1) = SYMSOL(ISOL)                               COI11290
            CUR(ISOL+1) = CUR(ISOL)                                     COI11300
         ENDIF                                                          COI11310
C                                                                       COI11320
         IF (.NOT. SYMSOL(ISOL)) THEN                                   COI11330
            ISOL = ISOL + 1                                             COI11340
         ELSE                                                           COI11350
            ISOL = ISOL + 2                                             COI11360
         ENDIF                                                          COI11370
C                                                                       COI11380
         GOTO 2828                                                      COI11390
C                                                                       COI11400
 2845 CONTINUE                                                          COI11410
C                                                                       COI11420
      IF (LENSOL(1) .NE. 0) THEN                                        COI11430
C           (* if there are solenoids *)                                COI11440
 2846    CONTINUE                                                       COI11450
C                                                                       COI11460
         WRITE(6,2847) WIRDIA*1000                                      COI11470
 2847    FORMAT('0Give the diameter of the wire (WIRDIA = ', F7.4,      COI11480
     1          ' mm). ')                                               COI11490
         REWIND 5                                                       COI11500
         READ(5, *, ERR=2846, END=2848) WIRDIA                          COI11510
         WIRDIA = WIRDIA/1000D0                                         COI11520
C                                                                       COI11530
 2848    CONTINUE                                                       COI11540
C                                                                       COI11550
 2849    CONTINUE                                                       COI11560
         WRITE(6,2850) FOILTH*1000                                      COI11570
 2850    FORMAT('0Give the thickness of the foil between the layers ',  COI11580
     1          '(FOILTH) = ', F7.4, ' mm). ' / ' If there is no foil ',COI11590
     2          'and you wind layers directly on top of each other,' /  COI11600
     3          ' putting the wire onto the notch between adjacent ',   COI11610
     4          'turns in the previous' / ' layer, give -0.134 times ', COI11620
     5          'the thickness of the wire.')                           COI11630
         REWIND 5                                                       COI11640
         READ(5, *, ERR=2849, END=2851) FOILTH                          COI11650
         FOILTH = FOILTH/1000D0                                         COI11660
 2851    CONTINUE                                                       COI11670
C                                                                       COI11680
      ENDIF                                                             COI11690
C                                                                       COI11700
 2852 CONTINUE                                                          COI11710
      IF (BEXT .EQ. 0) THEN                                             COI11720
         WRITE(6,2853)                                                  COI11730
 2853    FORMAT ('0Give the external field in Gausses (BEXT = 0.000 G).'COI11740
     1            / ' The field must be parallel to the axis of ',      COI11750
     2           'the shield. ')                                        COI11760
      ELSE                                                              COI11770
         WRITE(6,2854) BEXT                                             COI11780
 2854    FORMAT ('0Give the external field in Gausses (BEXT = ', G6.4,  COI11790
     1           ' G. ' / ' The field must be parallel to the axis of ',COI11800
     2           'the shield. ')                                        COI11810
      ENDIF                                                             COI11820
      REWIND 5                                                          COI11830
      READ(5, *, ERR=2852, END=2855) BEXT                               COI11840
      BEXT = BEXT/10000D0                                               COI11850
C                                                                       COI11860
 2855 CONTINUE                                                          COI11870
C                                                                       COI11880
      ISHI = 1                                                          COI11890
      HOLE = .TRUE.                                                     COI11900
 2856 CONTINUE                                                          COI11910
C           (* check whether there is a hole through the shield *)      COI11920
         HOLE = (HOLE .AND. ((LENSHI(ISHI) .NE. 0) .OR.                 COI11930
     1                      (RHOLE(ISHI) .NE. 0)))                      COI11940
         ISHI = ISHI + 1                                                COI11950
         IF ((HOLE) .AND. (ISHI .LE. NSHI)) GOTO 2856                   COI11960
      CONTINUE                                                          COI11970
C                                                                       COI11980
 2857 CONTINUE                                                          COI11990
      IF (HOLE) THEN                                                    COI12000
C           (* if there is a hole through the shield *)                 COI12010
         IF (TRAFLU .EQ. 0) THEN                                        COI12020
            WRITE(6,2858)                                               COI12030
 2858       FORMAT ('0Give the trapped flux in the shield in Gausses ', COI12040
     1             'times square centimeter.' /                         COI12050
     2             ' (FLUX = 0.000 Gauss*cm*cm).')                      COI12060
         ELSE                                                           COI12070
            WRITE(6,2859) TRAFLU*1D8                                    COI12080
 2859       FORMAT ('0Give the trapped flux in the shield in Gausses ', COI12090
     1             'times square centimeter.' /                         COI12100
     2             ' (FLUX = ', G8.4, ' Gauss*cm*cm).')                 COI12110
         ENDIF                                                          COI12120
         REWIND 5                                                       COI12130
         READ(5, *, ERR=2857, END=2860) TRAFLU                          COI12140
         TRAFLU = TRAFLU*1D-8                                           COI12150
 2860    CONTINUE                                                       COI12160
C                                                                       COI12170
      ELSE                                                              COI12180
C           (* if there is no hole through the shield *)                COI12190
         TRAFLU = 0D0                                                   COI12200
      ENDIF                                                             COI12210
C                                                                       COI12220
      CONTINUE                                                          COI12230
C                                                                       COI12240
      RETURN                                                            COI12250
      END                                                               COI12260
C                                                                       COI12270
C                                                                       COI12280
C     ****************************************************************  COI12290
C                                                                       COI12300
C                                                                       COI12310
      SUBROUTINE SHIPOT(NN, MAX, NSHI, LENSHI, RADSHI, RHOLE, CENSHI,   COI12320
     1                  SYMSHI, M)                                      COI12330
C                                                                       COI12340
C           (* Calculates   the  interaction   between  different *)    COI12350
C           (* current  rings  in the shield. The theta component *)    COI12360
C           (* of the vector potential in the Ith ring , produced *)    COI12370
C           (* by  unit  current  density  in  the  IIth ring, is *)    COI12380
C           (* stored as the component M(I, II) of the matrix M.  *)    COI12390
C           (*                                                    *)    COI12400
C           (* The meanings of the input parameters are:          *)    COI12410
C           (*     NN, MAX, NSHI, LENSHI, RADSHI, RHOLE, CENSHI : *)    COI12420
C           (*        as in SUBROUTINE RESULT                     *)    COI12430
C           (*     SYMSHI(K) : .TRUE.  if there is a similar part *)    COI12440
C           (*                 centered  to z=-CENSHI(K); .FALSE. *)    COI12450
C           (*                 otherwise                          *)    COI12460
C           (*     M : the  NNxNN  matrix  containing  the result *)    COI12470
C           (*         (output)                                   *)    COI12480
C                                                                       COI12490
      INTEGER NN, MAX, NSHI                                             COI12500
      DOUBLE PRECISION LENSHI(MAX), RADSHI(MAX), RHOLE(MAX),            COI12510
     1                 CENSHI(MAX), M(NN,NN)                            COI12520
      LOGICAL SYMSHI(MAX)                                               COI12530
C                                                                       COI12540
      INTEGER N, IND1, IND2, J, JJ, III, JJJ                            COI12550
      DOUBLE PRECISION ZFIEL1, ZFIEL2, ZSOUR1, ZSOUR2, DISTZ1, DISTZ2,  COI12560
     1                 RFIELD, RSOURC, DSKPOT, CYLPOT, RES1, RES2, RES3 COI12570
C           (* N : the  number  of  current  rings in each of the *)    COI12580
C           (*     shield parts                                   *)    COI12590
C           (* IND1, IND2 : indices  for shield parts ; IND1 used *)    COI12600
C           (*              normally  for  field  part,  IND2 for *)    COI12610
C           (*              source part                           *)    COI12620
C           (* J, JJ : used in current ring indices               *)    COI12630
C           (* III, JJJ : used  to  index current rings belonging *)    COI12640
C           (*            to the same cylindrical shield part     *)    COI12650
C           (* ZFIEL1 : z-coordinate  of  the  field  ring  if it *)    COI12660
C           (*          belongs to a cylindrical shield part      *)    COI12670
C           (* ZFIEL2 : z-coordinate of the corresponding ring in *)    COI12680
C           (*          the  symmetrically located similar shield *)    COI12690
C           (*          part, if there is one                     *)    COI12700
C           (* ZSOUR1, ZSOUR2 : as  ZFIEL1  and  ZFIEL2,  but for *)    COI12710
C           (*                  source current rings              *)    COI12720
C           (* DISTZ1 : distance between IND1th and IND2th planar *)    COI12730
C           (*          shield parts                              *)    COI12740
C           (* DISTZ2 : distance  from  IND1th planar shield part *)    COI12750
C           (*          to  the  symmetrically located partner of *)    COI12760
C           (*          IND2th planar shield part, or vice versa  *)    COI12770
C           (* RFIELD : the  radius  of  the  field  ring  if  it *)    COI12780
C           (*          belongs to a planar shield part           *)    COI12790
C           (* RSOURC : the  radius  of  the  source  ring  if it *)    COI12800
C           (*          belongs to a planar shield part           *)    COI12810
C           (* DSKPOT : function  subprogram  for calculating the *)    COI12820
C           (*          vector  potential  produced  by  a planar *)    COI12830
C           (*          current ring                              *)    COI12840
C           (* CYLPOT : function  subprogram  for calculating the *)    COI12850
C           (*          vector  potential  producedt by a current *)    COI12860
C           (*          sheet of cylindrical shape                *)    COI12870
C           (* RES1, RES2, RES3 : used for storing the results of *)    COI12880
C           (*                    calculations   of  interactions *)    COI12890
C           (*                    between   similar   cylindrical *)    COI12900
C           (*                    current rings                   *)    COI12910
C                  ...............................                      COI12920
C                                                                       COI12930
      N = INT(NN/NSHI)                                                  COI12940
C                                                                       COI12950
      IND1 = 1                                                          COI12960
 2905 CONTINUE                                                          COI12970
C                                                                       COI12980
         IND2 = 1                                                       COI12990
 2910    CONTINUE                                                       COI13000
C                                                                       COI13010
            IF (LENSHI(IND1) .EQ. 0) THEN                               COI13020
C                 (* if the field part is a disk *)                     COI13030
C                                                                       COI13040
               IF (LENSHI(IND2) .EQ. 0) THEN                            COI13050
C                    (* if the source is a disk *)                      COI13060
C                                                                       COI13070
                  DISTZ1 = CENSHI(IND1) - CENSHI(IND2)                  COI13080
                  IF ((SYMSHI(IND1)) .OR. (SYMSHI(IND2)))               COI13090
     1               DISTZ2 = CENSHI(IND1) + CENSHI(IND2)               COI13100
C                                                                       COI13110
                  DO 2920 J = 1, N                                      COI13120
C                       (* each ring of the field part *)               COI13130
                     RFIELD = RHOLE(IND1) +                             COI13140
     1                        (J-0.5)*(RADSHI(IND1)-RHOLE(IND1))/N      COI13150
C                                                                       COI13160
                     DO 2915 JJ = 1, N                                  COI13170
C                          (* each ring of the source part *)           COI13180
                        RSOURC = RHOLE(IND2) +                          COI13190
     1                          (JJ-0.5)*(RADSHI(IND2)-RHOLE(IND2))/N   COI13200
                        M((IND1-1)*N+J, (IND2-1)*N+JJ) =                COI13210
     1                     DSKPOT((RADSHI(IND2)-RHOLE(IND2))/N,         COI13220
     2                            RSOURC, RFIELD, DISTZ1)               COI13230
                                                                        COI13240
                        IF (SYMSHI(IND1)) THEN                          COI13250
C                             (* if the field part has a similar *)     COI13260
C                             (* partner located symmetrically   *)     COI13270
                           M(IND1*N+J, (IND2-1)*N+JJ) =                 COI13280
     1                        DSKPOT((RADSHI(IND2)-RHOLE(IND2))/N,      COI13290
     2                               RSOURC, RFIELD, -DISTZ2)           COI13300
C                                (* the index of the symmetrically *)   COI13310
C                                (* located field part is IND1+1   *)   COI13320
C                                                                       COI13330
                           IF (SYMSHI(IND2)) THEN                       COI13340
C                                (* if both shield parts have similar *)COI13350
C                                (* partners located symmetrically    *)COI13360
                              M(IND1*N+J, IND2*N+JJ) =                  COI13370
     1                           M((IND1-1)*N+J, (IND2-1)*N+JJ)         COI13380
                              M((IND1-1)*N+J, IND2*N+JJ) =              COI13390
     1                           M(IND1*N+J, (IND2-1)*N+JJ)             COI13400
C                                   (* the index of the symmetrically *)COI13410
C                                   (* located source part is IND2+1  *)COI13420
                           ENDIF                                        COI13430
C                                                                       COI13440
                        ELSE IF (SYMSHI(IND2)) THEN                     COI13450
C                             (* if only the source part has a simi- *) COI13460
C                             (* lar partner located symmetrically   *) COI13470
                           M((IND1-1)*N+J, IND2*N+JJ) =                 COI13480
     1                        DSKPOT((RADSHI(IND2)-RHOLE(IND2))/N,      COI13490
     2                               RSOURC, RFIELD, DISTZ2)            COI13500
                        ENDIF                                           COI13510
 2915                CONTINUE                                           COI13520
 2920             CONTINUE                                              COI13530
C                                                                       COI13540
               ELSE                                                     COI13550
C                    (* if the field part is a disk but the source *)   COI13560
C                    (* part a cylinder                            *)   COI13570
C                                                                       COI13580
                  DO 2930 J = 1, N                                      COI13590
C                       (* each ring of the field part *)               COI13600
                     RFIELD = RHOLE(IND1) +                             COI13610
     1                        (J-0.5)*(RADSHI(IND1)-RHOLE(IND1))/N      COI13620
C                                                                       COI13630
                     DO 2925 JJ = 1, N                                  COI13640
C                          (* each ring of the source part *)           COI13650
                        ZSOUR1 = CENSHI(IND2) + LENSHI(IND2)/2D0 -      COI13660
     1                           (JJ-0.5)*LENSHI(IND2)/N                COI13670
                        IF (SYMSHI(IND2))                               COI13680
     1                     ZSOUR2 = -CENSHI(IND2) + LENSHI(IND2)/2D0 -  COI13690
     2                              (JJ-0.5)*LENSHI(IND2)/N             COI13700
                        M((IND1-1)*N+J, (IND2-1)*N+JJ) =                COI13710
     1                     CYLPOT(LENSHI(IND2)/N, RADSHI(IND2), RFIELD, COI13720
     2                            CENSHI(IND1)-ZSOUR1)                  COI13730
C                                                                       COI13740
                        IF (SYMSHI(IND1)) THEN                          COI13750
C                             (* if the field part has a similar *)     COI13760
C                             (* partner located symmetrically   *)     COI13770
                           M(IND1*N+J, (IND2-1)*N+JJ) =                 COI13780
     1                        CYLPOT(LENSHI(IND2)/N, RADSHI(IND2),      COI13790
     2                           RFIELD, -CENSHI(IND1)-ZSOUR1)          COI13800
C                                                                       COI13810
                           IF (SYMSHI(IND2)) THEN                       COI13820
C                                (* if both shield parts have similar *)COI13830
C                                (* partners located symmetrically    *)COI13840
                              M(IND1*N+J, (IND2+1)*N-JJ+1) =            COI13850
     1                           M((IND1-1)*N+J, (IND2-1)*N+JJ)         COI13860
                              M((IND1-1)*N+J, (IND2+1)*N-JJ+1) =        COI13870
     1                           M(IND1*N+J, (IND2-1)*N+JJ)             COI13880
                           ENDIF                                        COI13890
C                                                                       COI13900
                        ELSE IF (SYMSHI(IND2)) THEN                     COI13910
C                             (* if only the source part has a simi- *) COI13920
C                             (* lar partner located symmetrically   *) COI13930
                           M((IND1-1)*N+J, IND2*N+JJ) =                 COI13940
     1                        CYLPOT(LENSHI(IND2)/N, RADSHI(IND2),      COI13950
     2                           RFIELD, CENSHI(IND1)-ZSOUR2)           COI13960
                        ENDIF                                           COI13970
 2925                CONTINUE                                           COI13980
 2930             CONTINUE                                              COI13990
               ENDIF                                                    COI14000
C                                                                       COI14010
            ELSE                                                        COI14020
C                 (* if the field part is a cylinder *)                 COI14030
C                                                                       COI14040
               IF (LENSHI(IND2) .EQ. 0) THEN                            COI14050
C                    (* if the source is a disk *)                      COI14060
C                                                                       COI14070
                  DO 2940 J = 1, N                                      COI14080
C                       (* each ring of the field part *)               COI14090
                     ZFIEL1 = CENSHI(IND1) + LENSHI(IND1)/2D0 -         COI14100
     1                        (J-0.5)*LENSHI(IND1)/N                    COI14110
                     IF (SYMSHI(IND1))                                  COI14120
     1                  ZFIEL2 = -CENSHI(IND1) + LENSHI(IND1)/2D0 -     COI14130
     2                           (J-0.5)*LENSHI(IND1)/N                 COI14140
C                                                                       COI14150
                     DO 2935 JJ = 1, N                                  COI14160
C                          (* each ring of the source part *)           COI14170
                        RSOURC = RHOLE(IND2) +                          COI14180
     1                          (JJ-0.5)*(RADSHI(IND2)-RHOLE(IND2))/N   COI14190
                        M((IND1-1)*N+J, (IND2-1)*N+JJ) =                COI14200
     1                     DSKPOT((RADSHI(IND2)-RHOLE(IND2))/N, RSOURC, COI14210
     2                            RADSHI(IND1), ZFIEL1-CENSHI(IND2))    COI14220
C                                                                       COI14230
                        IF (SYMSHI(IND1)) THEN                          COI14240
C                             (* if the field part has a similar *)     COI14250
C                             (* partner located symmetrically   *)     COI14260
                           M(IND1*N+J, (IND2-1)*N+JJ) =                 COI14270
     1                        DSKPOT((RADSHI(IND2)-RHOLE(IND2))/N,      COI14280
     2                               RSOURC, RADSHI(IND1),              COI14290
     3                               ZFIEL2-CENSHI(IND2))               COI14300
C                                                                       COI14310
                           IF (SYMSHI(IND2)) THEN                       COI14320
C                                (* if both shield parts have similar *)COI14330
C                                (* partners located symmetrically    *)COI14340
                              M((IND1+1)*N-J+1, IND2*N+JJ) =            COI14350
     1                           M((IND1-1)*N+J, (IND2-1)*N+JJ)         COI14360
                              M(IND1*N-J+1, IND2*N+JJ) =                COI14370
     1                           M(IND1*N+J, (IND2-1)*N+JJ)             COI14380
                           ENDIF                                        COI14390
C                                                                       COI14400
                        ELSE IF (SYMSHI(IND2)) THEN                     COI14410
C                             (* if only the source part has a simi- *) COI14420
C                             (* lar partner located symmetrically   *) COI14430
                           M((IND1-1)*N+J, IND2*N+JJ) =                 COI14440
     1                        DSKPOT((RADSHI(IND2)-RHOLE(IND2))/N,      COI14450
     2                               RSOURC, RADSHI(IND1),              COI14460
     3                               ZFIEL1+CENSHI(IND2))               COI14470
                        ENDIF                                           COI14480
 2935                CONTINUE                                           COI14490
 2940             CONTINUE                                              COI14500
C                                                                       COI14510
               ELSE                                                     COI14520
C                    (* if both the source and the field part are *)    COI14530
C                    (* cylinders                                 *)    COI14540
C                                                                       COI14550
                  IF (IND1 .EQ. IND2) THEN                              COI14560
C                       (* if  the  source and field cylinders are *)   COI14570
C                       (* the  same, or if they are symmetrically *)   COI14580
C                       (* located similar partners of each other, *)   COI14590
C                       (* the  symmetry of the system can be used *)   COI14600
C                       (* to make the amount of calculations much *)   COI14610
C                       (* fewer:  the interaction between current *)   COI14620
C                       (* rings  depends  only  on their distance *)   COI14630
C                       (* from each other                         *)   COI14640
                     DO 2950 J = 1, N                                   COI14650
                        III = 1                                         COI14660
                        JJJ = J                                         COI14670
                        RES1 = CYLPOT(LENSHI(IND1)/N, RADSHI(IND1),     COI14680
     1                              RADSHI(IND1), (J-1)*LENSHI(IND1)/N) COI14690
                        IF (SYMSHI(IND1)) THEN                          COI14700
                           RES2 = CYLPOT(LENSHI(IND1)/N, RADSHI(IND1),  COI14710
     1                                  RADSHI(IND1), 2*CENSHI(IND1)+   COI14720
     2                                  (J-1)*LENSHI(IND1)/N)           COI14730
                           RES3 = CYLPOT(LENSHI(IND1)/N, RADSHI(IND1),  COI14740
     1                                  RADSHI(IND1), 2*CENSHI(IND1)-   COI14750
     2                                  (J-1)*LENSHI(IND1)/N)           COI14760
                        ENDIF                                           COI14770
C                             (* RES1: interactions within one part; *) COI14780
C                             (* RES2 and RES3: interactions between *) COI14790
C                             (* a  cylinder  and  its symmetrically *) COI14800
C                             (* located similar partner             *) COI14810
C                                                                       COI14820
 2945                   CONTINUE                                        COI14830
                           M((IND1-1)*N+III, (IND1-1)*N+JJJ) = RES1     COI14840
                           M((IND1-1)*N+JJJ, (IND1-1)*N+III) = RES1     COI14850
                           IF (SYMSHI(IND1)) THEN                       COI14860
                              M(IND1*N+III, IND1*N+JJJ) = RES1          COI14870
                              M(IND1*N+JJJ, IND1*N+III) = RES1          COI14880
                              M((IND1-1)*N+III, IND1*N+JJJ) = RES2      COI14890
                              M(IND1*N+JJJ, (IND1-1)*N+III) = RES2      COI14900
                              M((IND1-1)*N+JJJ, IND1*N+III) = RES3      COI14910
                              M(IND1*N+III, (IND1-1)*N+JJJ) = RES3      COI14920
                           ENDIF                                        COI14930
C                                                                       COI14940
                           III = III + 1                                COI14950
                           JJJ = JJJ + 1                                COI14960
                           IF (JJJ .LE. N) GOTO 2945                    COI14970
C                                 (* if  all possible distances *)      COI14980
C                                 (* between current rings have *)      COI14990
C                                 (* not yet been gone through  *)      COI15000
                        CONTINUE                                        COI15010
 2950                CONTINUE                                           COI15020
                  ELSE                                                  COI15030
C                       (* if the source and field parts are *)         COI15040
C                       (* different solenoids               *)         COI15050
C                                                                       COI15060
                     DO 2960 J = 1, N                                   COI15070
C                          (* each ring of the field part *)            COI15080
                        ZFIEL1 = CENSHI(IND1) + LENSHI(IND1)/2D0 -      COI15090
     1                           (J-0.5)*LENSHI(IND1)/N                 COI15100
                        IF (SYMSHI(IND1))                               COI15110
     1                     ZFIEL2 = -CENSHI(IND1) + LENSHI(IND1)/2D0 -  COI15120
     2                              (J-0.5)*LENSHI(IND1)/N              COI15130
C                                                                       COI15140
                        DO 2955 JJ = 1, N                               COI15150
C                             (* each ring of the source part *)        COI15160
                           ZSOUR1 = CENSHI(IND2) + LENSHI(IND2)/2D0 -   COI15170
     1                              (JJ-0.5)*LENSHI(IND2)/N             COI15180
                           IF (SYMSHI(IND2))                            COI15190
     1                        ZSOUR2 = -CENSHI(IND2) + LENSHI(IND2)/2D0 COI15200
     2                                 - (JJ-0.5)*LENSHI(IND2)/N        COI15210
                           M((IND1-1)*N+J, (IND2-1)*N+JJ) =             COI15220
     1                        CYLPOT(LENSHI(IND2)/N, RADSHI(IND2),      COI15230
     2                           RADSHI(IND1), ZFIEL1-ZSOUR1)           COI15240
C                                                                       COI15250
                           IF (SYMSHI(IND1)) THEN                       COI15260
C                                (* if the field part has a similar *)  COI15270
C                                (* partner located symmetrically   *)  COI15280
                              M(IND1*N+J, (IND2-1)*N+JJ) =              COI15290
     1                           CYLPOT(LENSHI(IND2)/N, RADSHI(IND2),   COI15300
     2                                  RADSHI(IND1), ZFIEL2-ZSOUR1)    COI15310
                              IF (SYMSHI(IND2)) THEN                    COI15320
C                                   (* if both shield parts have *)     COI15330
C                                   (* similar  partners located *)     COI15340
C                                   (* symmetrically             *)     COI15350
                                 M((IND1+1)*N-J+1, (IND2+1)*N-JJ+1) =   COI15360
     1                              M((IND1-1)*N+J, (IND2-1)*N+JJ)      COI15370
                                 M(IND1*N-J+1, (IND2+1)*N-JJ+1) =       COI15380
     1                              M(IND1*N+J, (IND2-1)*N+JJ)          COI15390
                              ENDIF                                     COI15400
C                                                                       COI15410
                           ELSE IF (SYMSHI(IND2)) THEN                  COI15420
C                                (* if only the source part has *)      COI15430
C                                (* a  similar  partner located *)      COI15440
C                                (* symmetrically               *)      COI15450
                              M((IND1-1)*N+J, IND2*N+JJ) =              COI15460
     1                           CYLPOT(LENSHI(IND2)/N, RADSHI(IND2),   COI15470
     2                                  RADSHI(IND1), ZFIEL1-ZSOUR2)    COI15480
                           ENDIF                                        COI15490
 2955                   CONTINUE                                        COI15500
 2960                CONTINUE                                           COI15510
                  ENDIF                                                 COI15520
               ENDIF                                                    COI15530
            ENDIF                                                       COI15540
C                                                                       COI15550
            IF (SYMSHI(IND2)) THEN                                      COI15560
               IND2 = IND2 + 2                                          COI15570
C                 (* the effect of the symmetrically located similar *) COI15580
C                 (* source part, whose index is IND2+1, has already *) COI15590
C                 (* been calculated*)                                  COI15600
            ELSE                                                        COI15610
               IND2 = IND2 + 1                                          COI15620
            ENDIF                                                       COI15630
C                                                                       COI15640
            IF (IND2 .LE. NSHI) GOTO 2910                               COI15650
C                 (* if there are more source parts *)                  COI15660
         CONTINUE                                                       COI15670
C                                                                       COI15680
         IF (SYMSHI(IND1)) THEN                                         COI15690
            IND1 = IND1 + 2                                             COI15700
         ELSE                                                           COI15710
            IND1 = IND1 + 1                                             COI15720
         ENDIF                                                          COI15730
C                                                                       COI15740
         IF (IND1 .LE. NSHI) GOTO 2905                                  COI15750
C              (* if there are more field parts *)                      COI15760
      CONTINUE                                                          COI15770
C                                                                       COI15780
      RETURN                                                            COI15790
      END                                                               COI15800
C                                                                       COI15810
C                                                                       COI15820
C     ****************************************************************  COI15830
C                                                                       COI15840
C                                                                       COI15850
      SUBROUTINE SOLPOT(NN, MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE,   COI15860
     1                  SYMSHI, LENSOL, RADSOL, CENSOL, LAYERS, LOOPS,  COI15870
     2                  SYMSOL, CUR, WIRDIA, FOILTH, ASTOR, A)          COI15880
C                                                                       COI15890
C           (* Calculates  the  theta  component  of  the  vector *)    COI15900
C           (* potential  produced  by  the  solenoid  coils. The *)    COI15910
C           (* potential  in  the  Jth ring of the shield will be *)    COI15920
C           (* stored as the Jth component of the vector A.       *)    COI15930
C           (*                                                    *)    COI15940
C           (* The meanings of the input parameters are:          *)    COI15950
C           (*     NN,  MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE, *)    COI15960
C           (*        LENSOL,   RADSOL,  CENSOL,  LAYERS,  LOOPS, *)    COI15970
C           (*        SYMSOL, CUR, WIRDIA, FOILTH :               *)    COI15980
C           (*           as in SUBROUTINE RESULT                  *)    COI15990
C           (*     SYMSHI(I) : .TRUE.  if there is a similar part *)    COI16000
C           (*                 centered  to z=-CENSHI(I); .FALSE. *)    COI16010
C           (*                 otherwise                          *)    COI16020
C           (*     ASTOR(NN) : a  "storage  vector"  used for the *)    COI16030
C           (*                 results  of subroutines SOLDSK and *)    COI16040
C           (*                 SOLCYL                             *)    COI16050
C           (*     A(NN) : the result of this subroutine (output) *)    COI16060
C                                                                       COI16070
      INTEGER NN, MAX, NSHI, LAYERS(MAX)                                COI16080
      DOUBLE PRECISION LENSHI(MAX), RADSHI(MAX), CENSHI(MAX),           COI16090
     1                RHOLE(MAX), LENSOL(MAX), RADSOL(MAX), CENSOL(MAX),COI16100
     2                LOOPS(MAX), CUR(MAX), WIRDIA, FOILTH,             COI16110
     3                ASTOR(NN), A(NN)                                  COI16120
      LOGICAL SYMSHI(MAX), SYMSOL(MAX)                                  COI16130
C                                                                       COI16140
      INTEGER ISOL, ISHI, N, J                                          COI16150
C           (* ISOL : the index of the solenoid                   *)    COI16160
C           (* ISHI : the index of the shield part                *)    COI16170
C           (* N : the  number  of current rings used to approxi- *)    COI16180
C           (*     mate each of the shield parts                  *)    COI16190
C           (* J : used to index the current rings in the shield  *)    COI16200
C                  ...............................                      COI16210
C                                                                       COI16220
      N = INT(NN/NSHI)                                                  COI16230
      ISOL = 1                                                          COI16240
C                                                                       COI16250
 3010 CONTINUE                                                          COI16260
C           (* calculate the vector potentials produced by each coil *) COI16270
         ISHI = 1                                                       COI16280
                                                                        COI16290
 3020    CONTINUE                                                       COI16300
C              (* calculate the vector potential produced by ISOLth *)  COI16310
C              (* solenoid on the current rings forming each of the *)  COI16320
C              (* shield parts                                      *)  COI16330
            CALL ZERO(ASTOR, 1, NN)                                     COI16340
C                 (* makes every component of array ASTOR(NN) zero *)   COI16350
C                                                                       COI16360
            IF (LENSHI(ISHI) .EQ. 0) THEN                               COI16370
C                 (* if the shield part in the field region is a disk *)COI16380
               CALL SOLDSK(CENSHI(ISHI), RADSHI(ISHI), RHOLE(ISHI),     COI16390
     1                     LENSOL(ISOL), RADSOL(ISOL), CENSOL(ISOL),    COI16400
     2                     LAYERS(ISOL), LOOPS(ISOL), CUR(ISOL), WIRDIA,COI16410
     3                     FOILTH, N, ASTOR, NN)                        COI16420
C                                (* the vector potential distribution *)COI16430
C                                (* on  the  shield part is stored as *)COI16440
C                                (* the  first  N components of array *)COI16450
C                                (* ASTOR(NN)                         *)COI16460
               IF (SYMSOL(ISOL))                                        COI16470
     1            CALL SOLDSK(CENSHI(ISHI), RADSHI(ISHI), RHOLE(ISHI),  COI16480
     2                     LENSOL(ISOL), RADSOL(ISOL), -CENSOL(ISOL),   COI16490
     3                     LAYERS(ISOL), LOOPS(ISOL), CUR(ISOL), WIRDIA,COI16500
     4                     FOILTH, N, ASTOR, NN)                        COI16510
C                     (* adds the effect of symmetrically located *)    COI16520
C                     (* similar solenoid                         *)    COI16530
C                                                                       COI16540
               DO 3030 J = 1, N                                         COI16550
C                    (* each ring of the shield disk *)                 COI16560
                  A((ISHI-1)*N+J) = A((ISHI-1)*N+J) + ASTOR(J)          COI16570
                  IF (SYMSOL(ISOL) .AND. SYMSHI(ISHI))                  COI16580
     2                A(ISHI*N+J) = A(ISHI*N+J) + ASTOR(J)              COI16590
 3030          CONTINUE                                                 COI16600
C                                                                       COI16610
               IF ((.NOT. SYMSOL(ISOL)) .AND. SYMSHI(ISHI)) THEN        COI16620
C                    (* if only the shield part has a similar partner *)COI16630
C                    (* located symmetrically, at -CENSHI(ISHI)       *)COI16640
                  CALL ZERO(ASTOR, 1, NN)                               COI16650
                  CALL SOLDSK(-CENSHI(ISHI), RADSHI(ISHI), RHOLE(ISHI), COI16660
     1                     LENSOL(ISOL), RADSOL(ISOL), CENSOL(ISOL),    COI16670
     2                     LAYERS(ISOL), LOOPS(ISOL), CUR(ISOL), WIRDIA,COI16680
     3                     FOILTH, N, ASTOR, NN)                        COI16690
                  DO 3040 J = 1, N                                      COI16700
                     A(ISHI*N+J) = A(ISHI*N+J) + ASTOR(J)               COI16710
 3040              CONTINUE                                             COI16720
               ENDIF                                                    COI16730
C                                                                       COI16740
            ELSE                                                        COI16750
C                 (* if the shield part in the field region is *)       COI16760
C                 (* a cylinder                                *)       COI16770
               CALL SOLCYL(LENSHI(ISHI), RADSHI(ISHI), CENSHI(ISHI),    COI16780
     1                     LENSOL(ISOL), RADSOL(ISOL), CENSOL(ISOL),    COI16790
     2                     LAYERS(ISOL), LOOPS(ISOL), CUR(ISOL), WIRDIA,COI16800
     3                     FOILTH, N, ASTOR, NN)                        COI16810
               IF (SYMSOL(ISOL) .AND. (CENSHI(ISHI) .NE. 0))            COI16820
C                    (* if there is a similar solenoid located *)       COI16830
C                    (* symmetrically,  and  the center of the *)       COI16840
C                    (* shield part is not in origo            *)       COI16850
     1            CALL SOLCYL(LENSHI(ISHI), RADSHI(ISHI), CENSHI(ISHI), COI16860
     2                     LENSOL(ISOL), RADSOL(ISOL), -CENSOL(ISOL),   COI16870
     3                     LAYERS(ISOL), LOOPS(ISOL), CUR(ISOL), WIRDIA,COI16880
     4                     FOILTH, N, ASTOR, NN)                        COI16890
C                                                                       COI16900
               DO 3050 J = 1, N                                         COI16910
C                    (* each ring of the shield cylinder *)             COI16920
                  A((ISHI-1)*N+J) = A((ISHI-1)*N+J) + ASTOR(J)          COI16930
                  IF (SYMSOL(ISOL) .AND. (CENSHI(ISHI) .EQ. 0))         COI16940
     1               A(ISHI*N-J+1) = A(ISHI*N-J+1) + ASTOR(J)           COI16950
                  IF (SYMSOL(ISOL) .AND. SYMSHI(ISHI))                  COI16960
     1               A((ISHI+1)*N-J+1) = A((ISHI+1)*N-J+1) + ASTOR(J)   COI16970
 3050          CONTINUE                                                 COI16980
C                                                                       COI16990
               IF ((.NOT. SYMSOL(ISOL)) .AND. SYMSHI(ISHI)) THEN        COI17000
C                    (* if only the shield part has a similar partner *)COI17010
C                    (* located symmetrically, at -CENSHI(ISHI)       *)COI17020
                  CALL ZERO(ASTOR, 1, NN)                               COI17030
                  CALL SOLCYL(LENSHI(ISHI), RADSHI(ISHI), -CENSHI(ISHI),COI17040
     1                     LENSOL(ISOL), RADSOL(ISOL), CENSOL(ISOL),    COI17050
     2                     LAYERS(ISOL), LOOPS(ISOL), CUR(ISOL), WIRDIA,COI17060
     3                     FOILTH, N, ASTOR, NN)                        COI17070
                  DO 3060 J = 1, N                                      COI17080
                     A(ISHI*N+J) = A(ISHI*N+J) + ASTOR(J)               COI17090
 3060             CONTINUE                                              COI17100
               ENDIF                                                    COI17110
            ENDIF                                                       COI17120
C                                                                       COI17130
            IF (SYMSHI(ISHI)) THEN                                      COI17140
               ISHI = ISHI + 2                                          COI17150
C                 (* the index of the symmetric shield part is ISHI+1 *)COI17160
            ELSE                                                        COI17170
               ISHI = ISHI + 1                                          COI17180
            ENDIF                                                       COI17190
C                                                                       COI17200
            IF (ISHI .LE. NSHI) GOTO 3020                               COI17210
C                 (* if there are more shield parts *)                  COI17220
C                                                                       COI17230
         CONTINUE                                                       COI17240
C                                                                       COI17250
         IF (.NOT. SYMSOL(ISOL)) THEN                                   COI17260
            ISOL = ISOL + 1                                             COI17270
         ELSE                                                           COI17280
            ISOL = ISOL + 2                                             COI17290
C                 (* the index of the symmetric solenoid is ISOL+1 *)   COI17300
         ENDIF                                                          COI17310
C                                                                       COI17320
         IF (LENSOL(ISOL) .GT. 0) GOTO 3010                             COI17330
C              (* if there are more solenoids *)                        COI17340
      CONTINUE                                                          COI17350
C                                                                       COI17360
      RETURN                                                            COI17370
      END                                                               COI17380
C                                                                       COI17390
C                                                                       COI17400
C     ****************************************************************  COI17410
C                                                                       COI17420
C                                                                       COI17430
      SUBROUTINE CURREN(NN, MAX, NSHI, DIFSHI, LENSHI, RADSHI, CENSHI,  COI17440
     1                 RHOLE, SYMSHI, LENSOL, RADSOL, CENSOL, LAYERS,   COI17450
     2                 LOOPS, SYMSOL, CUR, WIRDIA, FOILTH, BEXT, TRAFLU,COI17460
     3                 M, A, X)                                         COI17470
C                                                                       COI17480
C           (* This subroutine calculates the current distribution *)   COI17490
C           (* in  the  shield,  dividing each of the shield parts *)   COI17500
C           (* into  N  rings and using the fact that the magnetic *)   COI17510
C           (* flux  through  any  of these rings must be the same *)   COI17520
C           (* constant,  the flux trapped by the shield. This can *)   COI17530
C           (* also   be   expressed  in  cylindrical  coordinates *)   COI17540
C           (* demanding  the magnetic vector potential in each of *)   COI17550
C           (* the  rings  to  be  the trapped flux divided by the *)   COI17560
C           (* length  of  the  periphery of the ring. Because the *)   COI17570
C           (* vector  potential  is  a  function  not only of the *)   COI17580
C           (* currents  in  the  coils  but  also  of the current *)   COI17590
C           (* distribution  in the shield, itself, we arrive at a *)   COI17600
C           (* matrix   equation  MX + A = C,  where  the  unknown *)   COI17610
C           (* vector X contains the currents in the shield rings, *)   COI17620
C           (* vector  A  contains  the effect of the coils on the *)   COI17630
C           (* vector  potential  in the shield, matrix M contains *)   COI17640
C           (* the  effect  of  the  current  distribution  in the *)   COI17650
C           (* shield  itself, and vector C contains the effect of *)   COI17660
C           (* the trapped flux.                                   *)   COI17670
C           (*                                                     *)   COI17680
C           (* The meanings of the input parameters are:          *)    COI17690
C           (*     NN,  MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE, *)    COI17700
C           (*        LENSOL,   RADSOL,  CENSOL,  LAYERS,  LOOPS, *)    COI17710
C           (*        SYMSOL, CUR, WIRDIA, FOILTH, BEXT, TRAFLU : *)    COI17720
C           (*           as in SUBROUTINE RESULT                  *)    COI17730
C           (*     SYMSHI(I) : .TRUE.  if there is a similar part *)    COI17740
C           (*                 centered  to z=-CENSHI(I); .FALSE. *)    COI17750
C           (*                 otherwise                          *)    COI17760
C           (*     DIFSHI : .TRUE.  if the shield dimensions have *)    COI17770
C           (*              been changed ; .FALSE. otherwise      *)    COI17780
C           (*     M, A, X : see  above  ;  the  matrix  equation *)    COI17790
C           (*               MX + A = C  is  solved  here  in the *)    COI17800
C           (*               form MX = A', where A' = C - A       *)    COI17810
C                                                                       COI17820
      INTEGER NN, MAX, NSHI, LAYERS(MAX)                                COI17830
      DOUBLE PRECISION LENSHI(MAX), RADSHI(MAX), CENSHI(MAX),           COI17840
     1          RHOLE(MAX), LENSOL(MAX), RADSOL(MAX), CENSOL(MAX),      COI17850
     2          LOOPS(MAX), CUR(MAX), WIRDIA, FOILTH, BEXT, TRAFLU,     COI17860
     3          X(NN), M(NN,NN), A(NN)                                  COI17870
      LOGICAL DIFSHI, SYMSHI(MAX), SYMSOL(MAX)                          COI17880
C                                                                       COI17890
      INTEGER N, ISHI, I, N1                                            COI17900
      DOUBLE PRECISION DCONST, PI, RADIUS                               COI17910
C           (* N : the  number  of current rings used to approxi- *)    COI17920
C           (*     mate each of the shield parts                  *)    COI17930
C           (* ISHI : the index of the shield part                *)    COI17940
C           (* I : used to index the current rings in the shield  *)    COI17950
C           (* N1 : the  total  number  of  current rings used to *)    COI17960
C           (*      approximate the superconducting shield        *)    COI17970
C           (* DCONST : an IMSL-subroutine for values of scienti- *)    COI17980
C           (*          fic constants                             *)    COI17990
C           (* PI : 3.1415...                                     *)    COI18000
C           (* RADIUS : the  "average radius" of the current ring *)    COI18010
C           (*          in the planar part of the superconducting *)    COI18020
C           (*          shield                                    *)    COI18030
C                  ...............................                      COI18040
C                                                                       COI18050
      PI = DCONST('PI')                                                 COI18060
      N = INT(NN/NSHI)                                                  COI18070
      CALL ZERO(A, 1, NN)                                               COI18080
      CALL ZERO(X, 1, NN)                                               COI18090
C                                                                       COI18100
      IF (DIFSHI) THEN                                                  COI18110
         CALL ZERO(M, NN, NN)                                           COI18120
         CALL SHIPOT(NN, MAX, NSHI, LENSHI, RADSHI, RHOLE, CENSHI,      COI18130
     1               SYMSHI, M)                                         COI18140
C           (* if the shield has been changed, calculate the matrix M *)COI18150
      ENDIF                                                             COI18160
C                                                                       COI18170
      IF (LENSOL(1) .NE. 0)                                             COI18180
     1   CALL SOLPOT(NN, MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE,      COI18190
     2               SYMSHI, LENSOL, RADSOL, CENSOL, LAYERS, LOOPS,     COI18200
     3               SYMSOL, CUR, WIRDIA, FOILTH, X, A)                 COI18210
C              (* if there are solenoids, calculate the vector *)       COI18220
C              (* potential  produced  by  them  and store the *)       COI18230
C              (* result to the vector A ; vector X is used in *)       COI18240
C              (* SUBROUTINE SOLPOT only as work space         *)       COI18250
      CONTINUE                                                          COI18260
C                                                                       COI18270
      DO 3130 ISHI = 1, NSHI                                            COI18280
         IF (LENSHI(ISHI) .EQ. 0) THEN                                  COI18290
C              (* if planar shield part *)                              COI18300
            DO 3110 I = 1, N                                            COI18310
               RADIUS = RHOLE(ISHI) +                                   COI18320
     1                  (I-0.5)*(RADSHI(ISHI)-RHOLE(ISHI))/N            COI18330
C                    (* each current ring has different radius *)       COI18340
               A(N*(ISHI-1)+I) = TRAFLU/(2*PI*RADIUS) -                 COI18350
     1                           A(N*(ISHI-1)+I) - BEXT*RADIUS/2D0      COI18360
C                    (* A' = C - A *)                                   COI18370
 3110       CONTINUE                                                    COI18380
C                                                                       COI18390
         ELSE                                                           COI18400
C              (* if cylindrical shield part *)                         COI18410
            DO 3120 I = 1, N                                            COI18420
               A(N*(ISHI-1)+I) = TRAFLU/(2*PI*RADSHI(ISHI)) -           COI18430
     1                           A(N*(ISHI-1)+I) - BEXT*RADSHI(ISHI)/2D0COI18440
C                    (* A' = C - A *)                                   COI18450
 3120       CONTINUE                                                    COI18460
         ENDIF                                                          COI18470
 3130 CONTINUE                                                          COI18480
C                                                                       COI18490
      CALL ZERO (X, 1, NN)                                              COI18500
      N1 = NSHI*N                                                       COI18510
C           (* N1 is the total number of current rings *)               COI18520
C                                                                       COI18530
      CALL DLSARG(N1, M, NN, A, 1, X)                                   COI18540
C           (* solves the matrix equation MX = A ; an IMSL-subroutine *)COI18550
C                                                                       COI18560
      RETURN                                                            COI18570
      END                                                               COI18580
C                                                                       COI18590
C                                                                       COI18600
C     ****************************************************************  COI18610
C                                                                       COI18620
C                                                                       COI18630
      SUBROUTINE FLUX(Z, R, NN, MAX, NSHI, LENSHI, RADSHI, CENSHI,      COI18640
     1            RHOLE, CURSHI, CENSOL, LENSOL, RADSOL, LOOPS, LAYERS, COI18650
     2            WIRDIA, FOILTH, CUR, BEXT, MAGFLU)                    COI18660
C                                                                       COI18670
C           (* Calculates  the  vector  potential  in  the  point *)    COI18680
C           (* (R, Z).                                            *)    COI18690
C           (*                                                    *)    COI18700
C           (* The meanings of the input parameters are:          *)    COI18710
C           (*     Z, R : the coordinates of the field point      *)    COI18720
C           (*     NN,  MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE, *)    COI18730
C           (*        CURSHI,   CENSOL,  LENSOL,  RADSOL,  LOOPS, *)    COI18740
C           (*        LAYERS, WIRDIA, FOILTH, CUR, BEXT : *)            COI18750
C           (*           as in SUBROUTINE RESULT                  *)    COI18760
C           (*     MAGFLU : the magnetic flux through the ring of *)    COI18770
C           (*              radius R located at z = Z ; output    *)    COI18780
C                                                                       COI18790
      INTEGER NN, MAX, NSHI, LAYERS(MAX)                                COI18800
      DOUBLE PRECISION Z, R, LENSHI(MAX), RADSHI(MAX), CENSHI(MAX),     COI18810
     1                RHOLE(MAX), CURSHI(NN), CENSOL(MAX), LENSOL(MAX), COI18820
     2                RADSOL(MAX), LOOPS(MAX), WIRDIA, FOILTH,          COI18830
     3                CUR(MAX), BEXT, MAGFLU                            COI18840
C                                                                       COI18850
      INTEGER N, ISHI, INDSOL, ILAY, I                                  COI18860
      DOUBLE PRECISION DISTZ, RADIUS, RADLAY, CURDEN, DSKPOT, CYLPOT,   COI18870
     1                 DCONST, PI                                       COI18880
C           (* N : the  number  of current rings used to approxi- *)    COI18890
C           (*     mate each of the shield parts                  *)    COI18900
C           (* ISHI : the index of the shield part                *)    COI18910
C           (* INDSOL : the index of the solenoid                 *)    COI18920
C           (* ILAY : the index of the wire layer in the solenoid *)    COI18930
C           (* I : used to index the current rings in the shield  *)    COI18940
C           (* DISTZ : the  z-component  of  the  vector from the *)    COI18950
C           (*         field  point  to  the center of the source *)    COI18960
C           (*         current  ring  in  the  shield  or  to the *)    COI18970
C           (*         center of the solenoid                     *)    COI18980
C           (* RADIUS : the   "average  radius"   of  the  source *)    COI18990
C           (*          current  ring  in  the planar part of the *)    COI19000
C           (*          superconducting shield                    *)    COI19010
C           (* RADLAY : the "average radius" of the wire layer in *)    COI19020
C           (*          the solenoid                              *)    COI19030
C           (* CURDEN : the  current  density in the shield or in *)    COI19040
C           (*          the solenoid (A/m)                        *)    COI19050
C           (* DSKPOT : function   which  calculates  the  vector *)    COI19060
C           (*          potential of a planar current sheet       *)    COI19070
C           (* CYLPOT : function   which  calculates  the  vector *)    COI19080
C           (*          potential of a cylindrical current sheet  *)    COI19090
C           (* DCONST : an IMSL-subroutine for values of scienti- *)    COI19100
C           (*          fic constants                             *)    COI19110
C           (* PI : 3.1415...                                     *)    COI19120
C                  ...............................                      COI19130
C                                                                       COI19140
      PI = DCONST('PI')                                                 COI19150
C                                                                       COI19160
      MAGFLU = BEXT*PI*R*R                                              COI19170
C           (* the magnetic flux due to the external field *)           COI19180
C                                                                       COI19190
      IF (NSHI .NE. 0) N = INT(NN/NSHI)                                 COI19200
C                                                                       COI19210
      IF (NSHI .NE. 0) THEN                                             COI19220
C           (* if there is a superconducting shield *)                  COI19230
         DO 3230 ISHI = 1, NSHI                                         COI19240
C              (* calculate the flux produced by the shield *)          COI19250
            IF (LENSHI(ISHI) .GT. 0) THEN                               COI19260
C                 (* if the shield part is cylindrical *)               COI19270
               DO 3210 I = 1, N                                         COI19280
                  CURDEN = CURSHI(N*(ISHI-1)+I)                         COI19290
                  DISTZ = Z - CENSHI(ISHI) -                            COI19300
     1                    ((N+1)/2D0-I)*LENSHI(ISHI)/N                  COI19310
                  MAGFLU = MAGFLU + CYLPOT(LENSHI(ISHI)/N, RADSHI(ISHI),COI19320
     1                                     R, DISTZ) * CURDEN*2*PI*R    COI19330
C                       (* the effect of one cylindrical current ring *)COI19340
 3210          CONTINUE                                                 COI19350
C                                                                       COI19360
            ELSE                                                        COI19370
C                 (* if the shield part is planar *)                    COI19380
               DO 3220 I = 1, N                                         COI19390
                  CURDEN = CURSHI(N*(ISHI-1)+I)                         COI19400
                  RADIUS = RHOLE(ISHI) +                                COI19410
     1                     (I-0.5)*(RADSHI(ISHI)-RHOLE(ISHI))/N         COI19420
                  MAGFLU = MAGFLU + DSKPOT((RADSHI(ISHI)-RHOLE(ISHI))/N,COI19430
     1                        RADIUS, R, Z-CENSHI(ISHI)) * CURDEN*2*PI*RCOI19440
C                       (* the effect of one planar current ring *)     COI19450
 3220          CONTINUE                                                 COI19460
            ENDIF                                                       COI19470
 3230    CONTINUE                                                       COI19480
      ENDIF                                                             COI19490
C                                                                       COI19500
C                                                                       COI19510
      INDSOL = 1                                                        COI19520
C                                                                       COI19530
 3240 CONTINUE                                                          COI19540
C           (* calculate the magnetic flux due to the solenoids *)      COI19550
         IF (LENSOL(INDSOL) .EQ. 0) GOTO 3260                           COI19560
C              (* if no more solenoids *)                               COI19570
         RADLAY = RADSOL(INDSOL) + 0.5*WIRDIA                           COI19580
         CURDEN = LOOPS(INDSOL)*CUR(INDSOL)/LENSOL(INDSOL)              COI19590
C                                                                       COI19600
         DO 3250 ILAY = 1, LAYERS(INDSOL)                               COI19610
            DISTZ = Z - CENSOL(INDSOL)                                  COI19620
            MAGFLU = MAGFLU + CYLPOT(LENSOL(INDSOL), RADLAY, R, DISTZ)  COI19630
     1                        * CURDEN*2*PI*R                           COI19640
C                                                                       COI19650
            RADLAY = RADLAY + WIRDIA + FOILTH                           COI19660
 3250    CONTINUE                                                       COI19670
C                                                                       COI19680
         INDSOL = INDSOL+1                                              COI19690
         GOTO 3240                                                      COI19700
 3260 CONTINUE                                                          COI19710
C                                                                       COI19720
      RETURN                                                            COI19730
      END                                                               COI19740
C                                                                       COI19750
C                                                                       COI19760
C     ****************************************************************  COI19770
C                                                                       COI19780
C                                                                       COI19790
      SUBROUTINE FIELD(Z, R, NN, MAX, NSHI, LENSHI, RADSHI, CENSHI,     COI19800
     1            RHOLE, CURSHI, CENSOL, LENSOL, RADSOL, LOOPS, LAYERS, COI19810
     2            WIRDIA, FOILTH, CUR, BEXT, BZ, BR)                    COI19820
C                                                                       COI19830
C           (* Calculates the magnetic field in the point (R, Z). *)    COI19840
C           (*                                                    *)    COI19850
C           (* The meanings of the input parameters are:          *)    COI19860
C           (*     Z, R : the coordinates of the field point      *)    COI19870
C           (*     NN,  MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE, *)    COI19880
C           (*        CURSHI,   CENSOL,  LENSOL,  RADSOL,  LOOPS, *)    COI19890
C           (*        LAYERS, WIRDIA, FOILTH, CUR, BEXT :         *)    COI19900
C           (*           as in SUBROUTINE RESULT                  *)    COI19910
C           (*     BZ, BR : z-  and  r-components of the magnetic *)    COI19920
C           (*              field in the point (R, Z) ; output    *)    COI19930
C                                                                       COI19940
      INTEGER NN, MAX, NSHI, LAYERS(MAX)                                COI19950
      DOUBLE PRECISION Z, R, LENSHI(MAX), RADSHI(MAX), CENSHI(MAX),     COI19960
     1                RHOLE(MAX), CURSHI(NN), CENSOL(MAX), LENSOL(MAX), COI19970
     2                RADSOL(MAX), LOOPS(MAX), WIRDIA, FOILTH,          COI19980
     3                CUR(MAX), BEXT, BZ, BR                            COI19990
                                                                        COI20000
      LOGICAL ONDISK                                                    COI20010
C                                                                       COI20020
      INTEGER N, ISHI, ISOL, I                                          COI20030
      DOUBLE PRECISION BZSOL, BRSOL, BZSHI, BRSHI, DISTZ, RADIUS,       COI20040
     1                 WIDTH, SHIAXI, SHIBZ, SHIBR                      COI20050
C           (* ONDISK : .TRUE.  if  the  field point is on any of *)    COI20060
C           (*          the superconducting disks                 *)    COI20070
C           (* N : the  number  of current rings used to approxi- *)    COI20080
C           (*     mate each of the shield parts                  *)    COI20090
C           (* ISHI : the index of the shield part                *)    COI20100
C           (* ISOL : the index of the solenoid                   *)    COI20110
C           (* I : used to index the current rings in the shield  *)    COI20120
C           (* BZSOL, BRSOL : the  z-  and  r-components  of  the *)    COI20130
C           (*                field in the point (R, Z), produced *)    COI20140
C           (*                by the solenoids                    *)    COI20150
C           (* BZSHI, BRSHI : the  z-  and  r-components  of  the *)    COI20160
C           (*                field in the point (R, Z), produced *)    COI20170
C           (*                by the shield                       *)    COI20180
C           (* DISTZ : the  z-component  of  the  vector from the *)    COI20190
C           (*         field  point  to  the center of the source *)    COI20200
C           (*         current ring in the shield                 *)    COI20210
C           (* RADIUS : the   "average  radius"   of  the  source *)    COI20220
C           (*          current  ring  in  the planar part of the *)    COI20230
C           (*          superconducting shield                    *)    COI20240
C           (* SHIAXI, SHIBZ, SHIBR : names  of functions used in *)    COI20250
C           (*                        this subroutine             *)    COI20260
C                  ...............................                      COI20270
C                                                                       COI20280
      BZ = 0D0                                                          COI20290
      BR = 0D0                                                          COI20300
      BZSOL = 0D0                                                       COI20310
      BRSOL = 0D0                                                       COI20320
      BZSHI = 0D0                                                       COI20330
      BRSHI = 0D0                                                       COI20340
      ONDISK = .FALSE.                                                  COI20350
C                                                                       COI20360
      IF (NSHI .NE. 0) N = INT(NN/NSHI)                                 COI20370
C                                                                       COI20380
      IF (NSHI .NE. 0) THEN                                             COI20390
C           (* if there is a superconducting shield *)                  COI20400
         DO 3330 ISHI = 1, NSHI                                         COI20410
C              (* calculate the field produced by the shield *)         COI20420
            IF (LENSHI(ISHI) .GT. 0) THEN                               COI20430
C                 (* if the shield part is cylindrical *)               COI20440
               DO 3310 I = 1, N                                         COI20450
                  DISTZ = Z - CENSHI(ISHI) -                            COI20460
     1                    ((N+1)/2D0-I)*LENSHI(ISHI)/N                  COI20470
                  CURDEN = CURSHI(N*(ISHI-1)+I)                         COI20480
                  IF (R .LT. 1.0D-6) THEN                               COI20490
C                       (* if the field point is on central axis *)     COI20500
                     BZSHI = BZSHI + CYLBAX(DISTZ, RADSHI(ISHI), CURDEN,COI20510
     1                                      LENSHI(ISHI)/N)             COI20520
                  ELSE                                                  COI20530
C                       (* if the field point is not on central axis *) COI20540
                     BZSHI = BZSHI + CYLBZ(DISTZ, R, RADSHI(ISHI),      COI20550
     1                                     CURDEN, LENSHI(ISHI)/N)      COI20560
                     BRSHI = BRSHI + CYLBR(DISTZ, R, RADSHI(ISHI),      COI20570
     1                                     CURDEN, LENSHI(ISHI)/N)      COI20580
                  ENDIF                                                 COI20590
C                       (* the effect of one cylindrical current ring *)COI20600
 3310          CONTINUE                                                 COI20610
C                                                                       COI20620
            ELSE                                                        COI20630
C                 (* if the shield part is planar *)                    COI20640
               IF ((DABS(Z-CENSHI(ISHI)) .LE. 1D-10) .AND.              COI20650
     1            (R .GE. RHOLE(ISHI)) .AND. (R .LE. RADSHI(ISHI))) THENCOI20660
C                    (* FUNCTION DSKBZ cannot calculate the field on *) COI20670
C                    (* a source disk                                *) COI20680
                  ONDISK = .TRUE.                                       COI20690
                  BRSHI=0                                               COI20700
C                                                                       COI20710
               ELSE                                                     COI20720
C                    (* if the field point not on the shield disk *)    COI20730
                  DO 3320 I = 1, N                                      COI20740
                     WIDTH = (RADSHI(ISHI)-RHOLE(ISHI))/N               COI20750
                     RADIUS = RHOLE(ISHI) + (I-0.5)*WIDTH               COI20760
                     DISTZ = Z - CENSHI(ISHI)                           COI20770
                     CURDEN = CURSHI(N*(ISHI-1)+I)                      COI20780
C                                                                       COI20790
                     BZSHI = BZSHI + DSKBZ(DISTZ, R, RADIUS, CURDEN,    COI20800
     1                                     WIDTH)                       COI20810
                     BRSHI = BRSHI + DSKBR(DISTZ, R, RADIUS, CURDEN,    COI20820
     1                                     WIDTH)                       COI20830
 3320             CONTINUE                                              COI20840
C                       (* the effect of one planar current ring *)     COI20850
               ENDIF                                                    COI20860
            ENDIF                                                       COI20870
 3330    CONTINUE                                                       COI20880
      ENDIF                                                             COI20890
C                                                                       COI20900
C                                                                       COI20910
      INDSOL = 1                                                        COI20920
C                                                                       COI20930
 3340 CONTINUE                                                          COI20940
C           (* calculate the field produced by the solenoids *)         COI20950
         IF (LENSOL(INDSOL) .EQ. 0) GOTO 3350                           COI20960
C              (* if no more solenoids *)                               COI20970
         CALL BSOLEN(Z-CENSOL(INDSOL), R, LENSOL(INDSOL),               COI20980
     1               RADSOL(INDSOL), WIRDIA, LOOPS(INDSOL),             COI20990
     2               LAYERS(INDSOL), CUR(INDSOL), FOILTH, BZSOL, BRSOL) COI21000
C                                                                       COI21010
         INDSOL = INDSOL+1                                              COI21020
         GOTO 3340                                                      COI21030
 3350 CONTINUE                                                          COI21040
C                                                                       COI21050
      IF (ONDISK) THEN                                                  COI21060
         BZ = 0                                                         COI21070
      ELSE                                                              COI21080
         BZ = BZSHI + BZSOL + BEXT                                      COI21090
      ENDIF                                                             COI21100
C                                                                       COI21110
      BR = BRSHI + BRSOL                                                COI21120
C                                                                       COI21130
      RETURN                                                            COI21140
      END                                                               COI21150
C                                                                       COI21160
C                                                                       COI21170
C     ****************************************************************  COI21180
C                                                                       COI21190
C                                                                       COI21200
      SUBROUTINE DISTRI(NN, MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE,   COI21210
     1                  CURSHI, FILE)                                   COI21220
C                                                                       COI21230
C           (* Writes  the  current distribution in the shield to *)    COI21240
C           (* the display and the file number FILE.              *)    COI21250
C           (*                                                    *)    COI21260
C           (* The meanings of the input parameters are:          *)    COI21270
C           (*     NN,  MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE, *)    COI21280
C           (*        CURSHI : as in SUBROUTINE RESULT            *)    COI21290
C           (*     FILE : the   number  of  the  file  where  the *)    COI21300
C           (*            results are stored                      *)    COI21310
C                                                                       COI21320
      INTEGER NN, MAX, NSHI, FILE                                       COI21330
      DOUBLE PRECISION LENSHI(MAX), RADSHI(MAX), CENSHI(MAX),           COI21340
     1                 RHOLE(MAX), CURSHI(NN)                           COI21350
C                                                                       COI21360
      INTEGER N, ISHI, I, J                                             COI21370
      DOUBLE PRECISION Z, R                                             COI21380
C           (* N : the  number  of current rings used to approxi- *)    COI21390
C           (*     mate each of the shield parts                  *)    COI21400
C           (* ISHI : the index of the shield part                *)    COI21410
C           (* I : used to index the current rings in the shield  *)    COI21420
C           (* Z, R : the coordinates of the current ring         *)    COI21430
C                  ...............................                      COI21440
C                                                                       COI21450
      IF (NSHI .NE. 0) THEN                                             COI21460
         WRITE(6,*) ('  ')                                              COI21470
         WRITE(FILE,*) ('  ')                                           COI21480
C                                                                       COI21490
         WRITE(6,3410)                                                  COI21500
         WRITE(FILE,3410)                                               COI21510
 3410    FORMAT(5X, 'z (mm)', 4X, 'r (mm)', 8X, 'Current (A/mm)')       COI21520
C                                                                       COI21530
         WRITE(6,*) ('  ')                                              COI21540
         WRITE(FILE,*) ('  ')                                           COI21550
C                                                                       COI21560
         N = INT(NN/NSHI)                                               COI21570
C                                                                       COI21580
         DO 3460 ISHI = 1, NSHI                                         COI21590
C                                                                       COI21600
            IF (LENSHI(ISHI) .GT. 0) THEN                               COI21610
C                 (* if cylindrical shield part *)                      COI21620
               DO 3430 I = 1, N                                         COI21630
                  Z = CENSHI(ISHI) + ((N+1)/2D0-I)*LENSHI(ISHI)/N       COI21640
                  R = RADSHI(ISHI)                                      COI21650
                  WRITE(6,3420) Z*1000, R*1000,                         COI21660
     1                       CURSHI(N*(ISHI-1)+I)/1000                  COI21670
                  WRITE(FILE,3420) Z*1000, R*1000,                      COI21680
     1                       CURSHI(N*(ISHI-1)+I)/1000                  COI21690
 3420             FORMAT(2X, F8.2 , 2X, F8.2, 4X, F14.5)                COI21700
 3430          CONTINUE                                                 COI21710
C                                                                       COI21720
            ELSE                                                        COI21730
C                 (* if the shield part is a disk *)                    COI21740
               DO 3450 I = 1, N                                         COI21750
                  Z = CENSHI(ISHI)                                      COI21760
                  R = RHOLE(ISHI) + (I-0.5)*(RADSHI(ISHI)-RHOLE(ISHI))/NCOI21770
                  WRITE(6,3440) Z*1000, R*1000,                         COI21780
     1                          CURSHI(N*(ISHI-1)+I)/1000               COI21790
                  WRITE(FILE,3440) Z*1000, R*1000,                      COI21800
     1                             CURSHI(N*(ISHI-1)+I)/1000            COI21810
 3440             FORMAT(2X, F8.2 , 2X, F8.2, 4X, F14.5)                COI21820
 3450          CONTINUE                                                 COI21830
            ENDIF                                                       COI21840
 3460    CONTINUE                                                       COI21850
      ENDIF                                                             COI21860
C                                                                       COI21870
      RETURN                                                            COI21880
      END                                                               COI21890
C                                                                       COI21900
C                                                                       COI21910
C     ****************************************************************  COI21920
C                                                                       COI21930
C                                                                       COI21940
C                                                                       COI21950
      SUBROUTINE ASK(PROTYP, R, ZBEG, DELTAZ, ZEND)                     COI21960
C                                                                       COI21970
C           (* Asks where the magnetic flux or field profile must *)    COI21980
C           (* be calculated.                                     *)    COI21990
C           (*                                                    *)    COI22000
C           (* The meanings of the input parameters are:          *)    COI22010
C           (*     PROTYP : the type of the profile ;             *)    COI22020
C           (*                    1 = field profile,              *)    COI22030
C           (*                    2 = flux profile                *)    COI22040
C           (*     R : the radius of the profile                  *)    COI22050
C           (*     ZBEG : the z-coordinate of the first point     *)    COI22060
C           (*     DELTAZ : the   distance   between   successive *)    COI22070
C           (*              points                                *)    COI22080
C           (*     ZEND : the z-coordinate of the last point      *)    COI22090
C                                                                       COI22100
      INTEGER PROTYP                                                    COI22110
      DOUBLE PRECISION R, ZBEG, DELTAZ, ZEND                            COI22120
C                  ...............................                      COI22130
C                                                                       COI22140
      IF (PROTYP .EQ. 1) THEN                                           COI22150
C           (* if field profile *)                                      COI22160
         WRITE(6,3510)                                                  COI22170
 3510    FORMAT ('0Give the radius of the profile.')                    COI22180
C                                                                       COI22190
      ELSE                                                              COI22200
C           (* if flux profile *)                                       COI22210
         WRITE(6,3520)                                                  COI22220
 3520    FORMAT ('0Give the radius of the rings through which the',     COI22230
     1           ' flux must be calculated.')                           COI22240
      ENDIF                                                             COI22250
C                                                                       COI22260
      READ(5,*) R                                                       COI22270
      R = R/1000D0                                                      COI22280
C                                                                       COI22290
      WRITE(6, 3530)                                                    COI22300
 3530 FORMAT ('0Give ZBEG, DELTAZ and ZEND in mm:s.')                   COI22310
      READ(5,*) ZBEG, DELTAZ, ZEND                                      COI22320
      ZBEG = ZBEG/1000D0                                                COI22330
      DELTAZ = DELTAZ/1000D0                                            COI22340
      ZEND = ZEND/1000D0                                                COI22350
C                                                                       COI22360
      RETURN                                                            COI22370
      END                                                               COI22380
C                                                                       COI22390
C                                                                       COI22400
C     ****************************************************************  COI22410
C                                                                       COI22420
C                                                                       COI22430
      SUBROUTINE RESULT(NN, MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE,   COI22440
     1                  CURSHI, CENSOL, LENSOL, RADSOL, LOOPS, LAYERS,  COI22450
     2                  SYMSOL, WIRDIA, FOILTH, CUR, BEXT, TRAFLU)      COI22460
C                                                                       COI22470
C           (* This  SUBROUTINE  organizes the calculation of the *)    COI22480
C           (* magnetic  field  profile  and/or the flux profile, *)    COI22490
C           (* and the output of the results.                     *)    COI22500
C           (*                                                    *)    COI22510
C           (* The meanings of the input parameters are:          *)    COI22520
C           (*     NN : dimension of the vector CURSHI            *)    COI22530
C           (*     MAX : the  maximum  number  of  solenoids  and *)    COI22540
C           (*           shield parts                             *)    COI22550
C           (*     NSHI : the number of shield parts              *)    COI22560
C           (*     LENSHI(I) : the length of the Ith shield part  *)    COI22570
C           (*     RADSHI(I) : the radius of the Ith shield part; *)    COI22580
C           (*                 if  the  part is a disk, RADSHI(I) *)    COI22590
C           (*                 is its outer radius                *)    COI22600
C           (*     CENSHI(I) : the  z-coordinate of the center of *)    COI22610
C           (*                 the Ith shield part                *)    COI22620
C           (*     RHOLE(I) : the  radius  of  the  hole  in  the *)    COI22630
C           (*                center of the Ith part, if the part *)    COI22640
C           (*                happens to be a disk                *)    COI22650
C           (*     CURSHI : NN-dimensional  vector containing the *)    COI22660
C           (*              current distribution in the shield    *)    COI22670
C           (*     CENSOL(K) : the  z-coordinate of the center of *)    COI22680
C           (*                 the Kth solenoid                   *)    COI22690
C           (*     LENSOL(K) : the length of the Kth solenoid     *)    COI22700
C           (*     RADSOL(K) : the   inner   radius  of  the  Kth *)    COI22710
C           (*                 solenoid                           *)    COI22720
C           (*     LOOPS(K) : the  number of turns of wire in one *)    COI22730
C           (*                layer of the Kth solenoid           *)    COI22740
C           (*     LAYERS(K) : the  number  of  layers of wire in *)    COI22750
C           (*                 the Kth solenoid                   *)    COI22760
C           (*     SYMSOL(K) : .TRUE. if there is a similar sole- *)    COI22770
C           (*                 noid  centered  to z = -CENSOL(K); *)    COI22780
C           (*                 .FALSE. otherwise                  *)    COI22790
C           (*     WIRDIA : the diameter of the wire used to wind *)    COI22800
C           (*              the solenoids                         *)    COI22810
C           (*     FOILTH : the  thickness of the insulating foil *)    COI22820
C           (*              between the layers                    *)    COI22830
C           (*     CUR(K) : the  current  in  the wire of the Kth *)    COI22840
C           (*              solenoid                              *)    COI22850
C           (*     BEXT : the external magnetic field             *)    COI22860
C           (*     TRAFLU : magnetic flux trapped into the shield *)    COI22870
C                                                                       COI22880
      INTEGER NN, MAX, NSHI, LAYERS(MAX)                                COI22890
      DOUBLE PRECISION LENSHI(MAX), RADSHI(MAX), CENSHI(MAX),           COI22900
     1                 RHOLE(MAX), CURSHI(NN),CENSOL(MAX), LENSOL(MAX), COI22910
     2                 RADSOL(MAX), LOOPS(MAX),WIRDIA, FOILTH,          COI22920
     3                 CUR(MAX), BEXT, TRAFLU                           COI22930
      LOGICAL SYMSOL(MAX)                                               COI22940
C                                                                       COI22950
      INTEGER FILE                                                      COI22960
      DOUBLE PRECISION R, Z, ZBEG, DELTAZ, ZEND, BZ0, BR0, BZ, BR,      COI22970
     1                 HOMOG, MAGFLU, TOTAL                             COI22980
      CHARACTER*1 ANSW                                                  COI22990
C           (* FILE : the  number  of  the file where the results *)    COI23000
C           (*        are stored                                  *)    COI23010
C           (* R, Z : the r- and z-coordinates of the field point *)    COI23020
C           (* ZBEG, DELTAZ, ZEND : the  field  profile is calcu- *)    COI23030
C           (*        lated  in points with given r-coordinate R, *)    COI23040
C           (*        and  z-coordinate running from ZBEG to ZEND *)    COI23050
C           (*        with step DELTAZ                            *)    COI23060
C           (* BZ0, BR0 : the z- and r-components of the magnetic *)    COI23070
C           (*            field in origo                          *)    COI23080
C           (* BZ, BR : the  z-  and r-components of the magnetic *)    COI23090
C           (*          fields in the point (R, Z)                *)    COI23100
C           (* HOMOG : the homogenity of BZ in ppm:s ;            *)    COI23110
C           (*         HOMOG = 1000000*(BZ-BZ0)/BZ0               *)    COI23120
C           (* TOTAL : the  magnitude  of total magnetic field in *)    COI23130
C           (*         the point (R,Z)                            *)    COI23140
C           (* POTEN : the vector potential in the point (R, Z)   *)    COI23150
C           (* DCONST : an IMSL-subroutine for values of scienti- *)    COI23160
C           (*          fic constants                             *)    COI23170
C           (* PI : 3.1415...                                     *)    COI23180
C                  ...............................                      COI23190
C                                                                       COI23200
      PI = DCONST('PI')                                                 COI23210
C                                                                       COI23220
      CALL FIELD(0D0, 0D0, NN, MAX, NSHI, LENSHI, RADSHI, CENSHI,       COI23230
     1           RHOLE, CURSHI, CENSOL, LENSOL, RADSOL, LOOPS, LAYERS,  COI23240
     2           WIRDIA, FOILTH, CUR, BEXT, BZ0, BR0)                   COI23250
C           (* calculate the magnetic field in origo *)                 COI23260
C                                                                       COI23270
 3605 CONTINUE                                                          COI23280
         WRITE(6,3610)                                                  COI23290
 3610    FORMAT('0Give the number of the file in which the ',           COI23300
     1          'results will be stored (10-99)')                       COI23310
         REWIND 5                                                       COI23320
         READ(5,*) FILE                                                 COI23330
         IF ((FILE .LT. 10) .OR. (FILE .GT. 99)) GOTO 3605              COI23340
      CONTINUE                                                          COI23350
C                                                                       COI23360
 3615 CONTINUE                                                          COI23370
 3620    CONTINUE                                                       COI23380
            WRITE(6,3625)                                               COI23390
 3625       FORMAT('0Do you want to know a field profile ("F"), ',      COI23400
     1             'a magnetic flux' / ' profile ("X"), or the ',       COI23410
     2             'current distribution in the shield ("C")?')         COI23420
            REWIND 5                                                    COI23430
            READ(5, '(A1)', ERR=3620, END=3620) ANSW                    COI23440
            IF ((ANSW .NE. 'X') .AND. (ANSW .NE. 'F') .AND.             COI23450
     1          (ANSW .NE. 'C')) GOTO 3620                              COI23460
         CONTINUE                                                       COI23470
C                                                                       COI23480
         IF (ANSW .EQ. 'F') THEN                                        COI23490
C              (* field profile *)                                      COI23500
            CALL ASK(1, R, ZBEG, DELTAZ, ZEND)                          COI23510
C                 (* ask the profile coordinates *)                     COI23520
C                                                                       COI23530
            WRITE(6,*) ('  ')                                           COI23540
            WRITE(FILE,*) ('  ')                                        COI23550
            WRITE(6,3630) BZ0*(1.0D4)                                   COI23560
            WRITE(FILE,3630) BZ0*(1.0D4)                                COI23570
 3630       FORMAT (' Field value in the center of the shield is ',     COI23580
     1              G16.10, ' Gauss')                                   COI23590
C                                                                       COI23600
            WRITE(6,*) ('  ')                                           COI23610
            WRITE(FILE,*) ('  ')                                        COI23620
            WRITE(6,3635)                                               COI23630
            WRITE(FILE,3635)                                            COI23640
 3635       FORMAT(5X, 'z (mm)', 4X, 'r (mm)', 6X, 'Bz (G)', 9X,        COI23650
     1             'Br (G)', 7X, 'Btot(G)', 5X, 'Homog (ppm)')          COI23660
C                                                                       COI23670
            WRITE(FILE,*) ' '                                           COI23680
C                                                                       COI23690
            Z = ZBEG                                                    COI23700
C                                                                       COI23710
 3640        CONTINUE                                                   COI23720
C                 (* repeat field calculation until Z = ZEND *)         COI23730
               CALL FIELD(Z, R, NN, MAX, NSHI, LENSHI, RADSHI, CENSHI,  COI23740
     1                RHOLE, CURSHI, CENSOL, LENSOL, RADSOL, LOOPS,     COI23750
     2                LAYERS, WIRDIA, FOILTH, CUR, BEXT, BZ, BR)        COI23760
               IF (BZ0.NE.0) HOMOG = (1.0D6)*(BZ-BZ0)/BZ0               COI23770
               TOTAL = DSQRT(BR*BR+BZ*BZ)                               COI23780
               WRITE(FILE,3645) Z*1000, R*1000, BZ*1D4, BR*1D4,         COI23790
     1                          TOTAL*1D4, HOMOG                        COI23800
               WRITE(6,3645) Z*1000, R*1000, BZ*1D4, BR*1D4,            COI23810
     1                       TOTAL*1D4, HOMOG                           COI23820
 3645          FORMAT(2X, F8.2 , 2X, F8.2, F14.5, F14.5, F14.5, 5X,     COI23830
     1                G12.6)                                            COI23840
C                                                                       COI23850
               Z = Z + DELTAZ                                           COI23860
               IF (Z .LE. ZEND) GOTO 3640                               COI23870
            CONTINUE                                                    COI23880
C                                                                       COI23890
         ELSE IF (ANSW .EQ. 'X') THEN                                   COI23900
C              (* flux profile *)                                       COI23910
            CALL ASK(2, R, ZBEG, DELTAZ, ZEND)                          COI23920
C                 (* ask the profile coordinates *)                     COI23930
C                                                                       COI23940
            WRITE(6,*) ('  ')                                           COI23950
            WRITE(FILE,*) ('  ')                                        COI23960
            WRITE(6,3650)                                               COI23970
            WRITE(FILE,3650)                                            COI23980
 3650       FORMAT(5X, 'z (mm)', 4X, 'r (mm)', 3X, 'Flux (Gauss*cm*cm)')COI23990
C                                                                       COI24000
            WRITE(FILE,*) ' '                                           COI24010
C                                                                       COI24020
            Z = ZBEG                                                    COI24030
C                                                                       COI24040
 3655       CONTINUE                                                    COI24050
C                 (* repeat flux calculation until Z = ZEND *)          COI24060
               IF (R .NE. 0)                                            COI24070
     1            CALL FLUX(Z, R, NN, MAX, NSHI, LENSHI, RADSHI,        COI24080
     2                      CENSHI, RHOLE, CURSHI, CENSOL, LENSOL,      COI24090
     3                      RADSOL, LOOPS, LAYERS, WIRDIA, FOILTH, CUR, COI24100
     4                      BEXT, MAGFLU)                               COI24110
               IF (R .EQ. 0) POTEN = 0                                  COI24120
               WRITE(FILE,3660) Z*1000, R*1000, MAGFLU*1D8              COI24130
               WRITE(6,3660) Z*1000, R*1000, MAGFLU*1D8                 COI24140
 3660          FORMAT(2X, F8.2 , 2X, F8.2, F14.5)                       COI24150
C                                                                       COI24160
               Z = Z + DELTAZ                                           COI24170
               IF (Z. LE. ZEND) GOTO 3655                               COI24180
            CONTINUE                                                    COI24190
C                                                                       COI24200
         ELSE                                                           COI24210
C              (* current distribution in the shield *)                 COI24220
            CALL DISTRI(NN, MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE,   COI24230
     1                  CURSHI, FILE)                                   COI24240
C                                                                       COI24250
         ENDIF                                                          COI24260
C                                                                       COI24270
         WRITE(FILE,*) ' '                                              COI24280
C                                                                       COI24290
 3665    CONTINUE                                                       COI24300
            WRITE(6,3670)                                               COI24310
 3670       FORMAT('0Do you want to continue with the same ',           COI24320
     1             'parameters (Y/N)?')                                 COI24330
            READ(5,'(A1)') ANSW                                         COI24340
            IF ((ANSW .NE. 'Y') .AND. (ANSW .NE. 'N')) GOTO 3665        COI24350
         CONTINUE                                                       COI24360
         IF (ANSW .EQ. 'Y') GOTO 3615                                   COI24370
      CONTINUE                                                          COI24380
C                                                                       COI24390
      WRITE(FILE,*) ('  ')                                              COI24400
      WRITE(FILE,*) ('  ')                                              COI24410
      CALL WRSHI(FILE, MAX, NSHI, LENSHI, RADSHI, RHOLE, CENSHI)        COI24420
      CALL WROTH(FILE, MAX, LENSOL, RADSOL, CENSOL, LAYERS,             COI24430
     1           LOOPS, SYMSOL, CUR, WIRDIA, FOILTH, BEXT, TRAFLU)      COI24440
      WRITE(FILE,*) ' '                                                 COI24450
      WRITE(FILE,*) '...............................................',  COI24460
     1              '........................'                          COI24470
      WRITE(FILE,*) ' '                                                 COI24480
C           (* write the input parameters to the result file *)         COI24490
C                                                                       COI24500
      RETURN                                                            COI24510
      END                                                               COI24520
C                                                                       COI24530
C                                                                       COI24540
C     ****************************************************************  COI24550
C                                                                       COI24560
C                                                                       COI24570
C     MAIN PROGRAM                                                      COI24580
C                                                                       COI24590
C           (* This  program  calculates  the magnetic field of a *)    COI24600
C           (* collection of coaxial solenoids and cylindrical or *)    COI24610
C           (* planar  superconducting  shield parts. Each of the *)    COI24620
C           (* shield  parts  is  approximated by an equal number *)    COI24630
C           (* of  current  rings, whose total number is NN (or a *)    COI24640
C           (* little less).                                      *)    COI24650
C                                                                       COI24660
      INTEGER NN, MAX                                                   COI24670
C           (* NN : the  maximum number of current rings used for *)    COI24680
C           (*      approximating the current distribution in the *)    COI24690
C           (*      superconducting shield                        *)    COI24700
C           (* MAX : the  maximum  number of solenoids and shield *)    COI24710
C           (*       parts                                        *)    COI24720
C                                                                       COI24730
      PARAMETER(NN=100, MAX=30)                                         COI24740
C                                                                       COI24750
      INTEGER NSHI, LEVOLD, LAYERS(MAX)                                 COI24760
      DOUBLE PRECISION LENSHI(MAX), RADSHI(MAX), RHOLE(MAX),            COI24770
     1              CENSHI(MAX), LENSOL(MAX),RADSOL(MAX), CENSOL(MAX),  COI24780
     2              LOOPS(MAX), CUR(MAX), WIRDIA, FOILTH, BEXT, TRAFLU, COI24790
     3              CURSHI(NN), M(NN,NN), A(NN), RWKSP(2*NN*NN+4*NN)    COI24800
      CHARACTER*1 ANSW                                                  COI24810
      LOGICAL DIFSHI, SYMSHI(MAX), SYMSOL(MAX)                          COI24820
C                                                                       COI24830
      COMMON /WORKSP/ RWKSP                                             COI24840
C                  ...............................                      COI24850
C                                                                       COI24860
      IF (NN .GE. 35) CALL IWKIN(2*NN*NN+4*NN)                          COI24870
C           (* total amount of automatically allocated space is *)      COI24880
C           (* 2500  double  precision units; when workspace is *)      COI24890
C           (* needed more, it must be allocated using IWKIN    *)      COI24900
C                                                                       COI24910
 9910 CONTINUE                                                          COI24920
         CALL DIMENS(NSHI, DIFSHI, LENSHI, RADSHI, RHOLE, CENSHI,       COI24930
     1             SYMSHI, LENSOL, RADSOL, CENSOL, LAYERS, LOOPS,       COI24940
     2             SYMSOL, CUR, WIRDIA, FOILTH, BEXT, TRAFLU, MAX)      COI24950
C              (* ask the input parameters *)                           COI24960
C                                                                       COI24970
         IF (NSHI .NE. 0)                                               COI24980
C              (* if there is a superconducting shield *)               COI24990
     1      CALL CURREN(NN, MAX, NSHI, DIFSHI, LENSHI, RADSHI, CENSHI,  COI25000
     2                  RHOLE, SYMSHI, LENSOL, RADSOL, CENSOL, LAYERS,  COI25010
     3                  LOOPS, SYMSOL, CUR, WIRDIA, FOILTH, BEXT,       COI25020
     4                  TRAFLU, M, A, CURSHI)                           COI25030
C                 (* calculate the current distribution in the shield *)COI25040
C                                                                       COI25050
         CALL RESULT(NN, MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE,      COI25060
     1               CURSHI, CENSOL, LENSOL, RADSOL, LOOPS, LAYERS,     COI25070
     2               SYMSOL, WIRDIA, FOILTH, CUR, BEXT, TRAFLU)         COI25080
C              (* calculate and print the magnetic field profile etc. *)COI25090
C                                                                       COI25100
 9920    CONTINUE                                                       COI25110
            WRITE(6,9930)                                               COI25120
 9930       FORMAT ('0Do you want to continue (Y/N)?')                  COI25130
            READ(5,'(A1)') ANSW                                         COI25140
            IF ((ANSW .NE. 'Y') .AND. (ANSW .NE. 'N')) GOTO 9920        COI25150
         CONTINUE                                                       COI25160
         IF (ANSW .EQ. 'Y') GOTO 9910                                   COI25170
      CONTINUE                                                          COI25180
C                                                                       COI25190
      STOP                                                              COI25200
      END                                                               COI25210

From Giorgi.Tvalashvili@uni-bayreuth.de Fri Jan 12 14:39 MET 1996
Received: from btr0x1.hrz.uni-bayreuth.de by btp9x4.phy.uni-bayreuth.de with SMTP
	(1.37.109.4/16.2) id AA12173; Fri, 12 Jan 96 14:39:23 +0100
Return-Path: <Giorgi.Tvalashvili@uni-bayreuth.de>
Received: from btp9x5.phy.uni-bayreuth.de by btr0x1.hrz.uni-bayreuth.de (4.1/btr0x1 (UBTGW/btr0x1-2.4.7))
	id AA15066; Fri, 12 Jan 96 14:48:51 +0100
Received: from btr0x1.hrz.uni-bayreuth.de by btp9x5.phy.uni-bayreuth.de with SMTP
	(1.38.193.4/16.2) id AA11668; Fri, 12 Jan 1996 14:43:13 +0100
Received: from btrzv5.hrz.uni-bayreuth.de (btrzv5.fddi.uni-bayreuth.de) by btr0x1.hrz.uni-bayreuth.de (4.1/btr0x1 (UBTGW/btr0x1-2.4.7))
	id AA15063; Fri, 12 Jan 96 14:48:47 +0100
Received: by uni-bayreuth.de (MX V4.2 AXP) id 12; Fri, 12 Jan 1996 14:48:45
          +0100
Date: Fri, 12 Jan 1996 14:48:43 +0100
From: Giorgi.Tvalashvili@uni-bayreuth.de
To: GIORGI@btp9x5.phy.uni-bayreuth.de
Message-Id: <0099C485.8892D57D.12@uni-bayreuth.de>
Subject: for
Status: RO

C                                                                       COI00010                                                                       COI00020
      SUBROUTINE ZERO(MATRIX, DIM1, DIM2)                               COI00030
C                                                                       COI00040
C           (* Makes every component of MATRIX(DIM1,DIM2) zero.   *)    COI00050
C                                                                       COI00060
      INTEGER DIM1, DIM2                                                COI00070
      DOUBLE PRECISION MATRIX(DIM1, DIM2)                               COI00080
C                                                                       COI00090
      INTEGER I, J                                                      COI00100
C                  ...............................                      COI00110
C                                                                       COI00120
      DO 1020 I = 1, DIM1                                               COI00130
         DO 1010 J = 1, DIM2                                            COI00140
            MATRIX(I,J) = 0D0                                           COI00150
 1010    CONTINUE                                                       COI00160
 1020 CONTINUE                                                          COI00170
C                                                                       COI00180
      RETURN                                                            COI00190
      END                                                               COI00200
C                                                                       COI00210
C                                                                       COI00220
C     ****************************************************************  COI00230
C                                                                       COI00240
C                                                                       COI00250
      DOUBLE PRECISION FUNCTION ELLIP3(N, K)                            COI00260
C                                                                       COI00270
C           (* Calculates complete elliptic integral of the third *)    COI00280
C           (* kind   using   IMSL-functions   DELRF   and  DELRJ *)    COI00290
C           (* (incomplete  elliptic  integrals  of the first and *)    COI00300
C           (* third  kind, respectively). The input parameters N *)    COI00310
C           (* and  K are the parameters of the complete elliptic *)    COI00320
C           (* integral  of third kind when its normal definition *)    COI00330
C           (* is used.                                           *)    COI00340
C                                                                       COI00350
      DOUBLE PRECISION N, K                                             COI00360
C                                                                       COI00370
      DOUBLE PRECISION DELRF, DELRJ                                     COI00380
C                  ...............................                      COI00390
C                                                                       COI00400
      ELLIP3 = DELRF(0D0, 1D0-K, 1D0) +                                 COI00410
     1         DELRJ(0D0, 1D0-K, 1D0, 1D0-N)*N/3D0                      COI00420
C                                                                       COI00430
      RETURN                                                            COI00440
      END                                                               COI00450
C                                                                       COI00460
C                                                                       COI00470
C     ****************************************************************  COI00480
C                                                                       COI00490
C                                                                       COI00500
      DOUBLE PRECISION FUNCTION FDSKBZ(X)                               COI00510
C                                                                       COI00520
C           (* When integrated over X from 0 to PI and multiplied *)    COI00530
C           (* by  the  current  density, this function gives the *)    COI00540
C           (* z-component  of the magnetic field produced by the *)    COI00550
C           (* current disk defined in FUNCTION DSKBZ.            *)    COI00560
C                                                                       COI00570
      DOUBLE PRECISION X, KSIF, RF, RSOUF, WF                           COI00580
      COMMON /AREA/ KSIF, RF, RSOUF, WF                                 COI00590
C           (* X : theta-component of the source point            *)    COI00600
C           (* KSIF : z-component  of  the vector from the center *)    COI00610
C           (*        of the source ring to the field point       *)    COI00620
C           (* RF : r-coordinate of the field point               *)    COI00630
C           (* RSOUF : the "average radius" of the source ring    *)    COI00640
C           (* WF : the width of the source ring                  *)    COI00650
C                                                                       COI00660
      DOUBLE PRECISION C, RAD1, RAD2, K1, K2, T1, T2, S1, S2            COI00670
C           (* C : cos(X)                                         *)    COI00680
C           (* RAD1, RAD2 : inner and outer radius of the current *)    COI00690
C           (*              disk, respectively                    *)    COI00700
C           (* K1, K2, T1, T2, S1, S2 : defined in formulae below *)    COI00710
C                  ...............................                      COI00720
C                                                                       COI00730
      C = DCOS(X)                                                       COI00740
C                                                                       COI00750
      RAD1 = RSOUF - WF/2D0                                             COI00760
      RAD2 = RSOUF + WF/2D0                                             COI00770
C                                                                       COI00780
      K1 = DSQRT(RAD1*RAD1 + RF*RF + KSIF*KSIF - 2D0*RAD1*RF*C)         COI00790
      K2 = DSQRT(RAD2*RAD2 + RF*RF + KSIF*KSIF - 2D0*RAD2*RF*C)         COI00800
C                                                                       COI00810
      T1 = KSIF*KSIF + RAD1*RAD1 + 2D0*RF*RF - 3D0*RAD1*RF*C            COI00820
      T2 = KSIF*KSIF + RAD2*RAD2 + 2D0*RF*RF - 3D0*RAD2*RF*C            COI00830
C                                                                       COI00840
      S1 = RF*C*C*(((RAD1*RAD1+RF*RF)*C-RAD1*RF*(1D0+C*C))/K1 - C*K1) / COI00850
     1                                     (KSIF*KSIF + RF*RF*(1D0-C*C))COI00860
      S2 = RF*C*C*(((RAD2*RAD2+RF*RF)*C-RAD2*RF*(1D0+C*C))/K2 - C*K2) / COI00870
     1                                     (KSIF*KSIF + RF*RF*(1D0-C*C))COI00880
C                                                                       COI00890
      FDSKBZ = 2D-7 * ((T2/K2-T1/K1)*C/RF + S2 - S1                     COI00900
     1                 + 2D0*C*C*DLOG((K2+RAD2-RF*C)/(K1+RAD1-RF*C)))   COI00910
C                                                                       COI00920
      RETURN                                                            COI00930
      END                                                               COI00940
C                                                                       COI00950
C                                                                       COI00960
C     ****************************************************************  COI00970
C                                                                       COI00980
C                                                                       COI00990
      DOUBLE PRECISION FUNCTION FDSKBR(X)                               COI01000
C                                                                       COI01010
C           (* When integrated over X from 0 to PI and multiplied *)    COI01020
C           (* by  the  current  density, this function gives the *)    COI01030
C           (* r-component  of the magnetic field produced by the *)    COI01040
C           (* current disk defined in FUNCTION DSKBR.            *)    COI01050
C                                                                       COI01060
      DOUBLE PRECISION X, KSIF, RF, RSOUF, WF                           COI01070
      COMMON /AREA/ KSIF, RF, RSOUF, WF                                 COI01080
C           (* X : theta-component of the source point            *)    COI01090
C           (* KSIF : z-component  of  the vector from the center *)    COI01100
C           (*        of the source ring to the field point       *)    COI01110
C           (* RF : r-coordinate of the field point               *)    COI01120
C           (* RSOUF : the "average radius" of the source ring    *)    COI01130
C           (* WF : the width of the source ring                  *)    COI01140
C                                                                       COI01150
      DOUBLE PRECISION C, RAD1, RAD2, K1, K2                            COI01160
C           (* C : cos(X)                                         *)    COI01170
C           (* RAD1, RAD2 : inner and outer radius of the current *)    COI01180
C           (*              disk, respectively                    *)    COI01190
C           (* K1, K2 : defined in formulae below                 *)    COI01200
C                  ...............................                      COI01210
C                                                                       COI01220
      C = DCOS(X)                                                       COI01230
C                                                                       COI01240
      RAD1 = RSOUF - WF/2D0                                             COI01250
      RAD2 = RSOUF + WF/2D0                                             COI01260
C                                                                       COI01270
      K1 = DSQRT(RAD1*RAD1 + RF*RF + KSIF*KSIF - 2D0*RAD1*RF*C)         COI01280
      K2 = DSQRT(RAD2*RAD2 + RF*RF + KSIF*KSIF - 2D0*RAD2*RF*C)         COI01290
C                                                                       COI01300
      FDSKBR = 2D-7*KSIF*(C/(KSIF*KSIF+RF*RF*(1-C*C))) *                COI01310
     1       ((-KSIF*KSIF-RF*RF+RAD2*RF*C)/K2 -                         COI01320
     2        (-KSIF*KSIF-RF*RF+RAD1*RF*C)/K1)                          COI01330
C                                                                       COI01340
      RETURN                                                            COI01350
      END                                                               COI01360
C                                                                       COI01370
C                                                                       COI01380
C     ****************************************************************  COI01390
C                                                                       COI01400
C                                                                       COI01410
      DOUBLE PRECISION FUNCTION FDSPOT(X)                               COI01420
C                                                                       COI01430
C           (* When integrated over X from 0 to PI, multiplied by *)    COI01440
C           (* the  current  density  and  divided  by  WF,  this *)    COI01450
C           (* function  gives  the  vector potential produced by *)    COI01460
C           (* the current disk defined in FUNCTION DSKPOT.       *)    COI01470
C                                                                       COI01480
      DOUBLE PRECISION X, KSIF, RF, RSOUF, WF                           COI01490
      COMMON /AREA/ KSIF, RF, RSOUF, WF                                 COI01500
C           (* X : theta-component of the source point            *)    COI01510
C           (* KSIF : z-component  of  the vector from the center *)    COI01520
C           (*        of the source ring to the field point       *)    COI01530
C           (* RF : r-coordinate of the field point               *)    COI01540
C           (* RSOUF : the "average radius" of the source ring    *)    COI01550
C           (* WF : the width of the source ring                  *)    COI01560
C                                                                       COI01570
      DOUBLE PRECISION RAD1, RAD2, C, K1, K2, S1, S2                    COI01580
C           (* RAD1, RAD2 : inner and outer radius of the current *)    COI01590
C           (*              disk, respectively                    *)    COI01600
C           (* C : cos(X)                                         *)    COI01610
C           (* K1, K2, S1, S2 : defined in formulae below         *)    COI01620
C                                                                       COI01630
      C = DCOS(X)                                                       COI01640
C                                                                       COI01650
      RAD1 = RSOUF - WF/2D0                                             COI01660
      RAD2 = RSOUF + WF/2D0                                             COI01670
C                                                                       COI01680
      K1 = DSQRT(RAD1*RAD1 + RF*RF + KSIF*KSIF - 2*RAD1*RF*C)           COI01690
      K2 = DSQRT(RAD2*RAD2 + RF*RF + KSIF*KSIF - 2*RAD2*RF*C)           COI01700
C                                                                       COI01710
      S1 = RAD1 - RF*C + K1                                             COI01720
      S2 = RAD2 - RF*C + K2                                             COI01730
C                                                                       COI01740
      IF (((S1 .LE. 0) .AND. (S2 .GT. 0)) .OR.                          COI01750
     1    ((S2 .LE. 0) .AND. (S1 .GT. 0))) THEN                         COI01760
         FDSPOT = 0                                                     COI01770
      ELSE                                                              COI01780
         FDSPOT = (2D-7) * C * (K2 - K1 + RF*C*DLOG(S2/S1))             COI01790
      ENDIF                                                             COI01800
C                                                                       COI01810
                                                                        COI01820
      RETURN                                                            COI01830
      END                                                               COI01840
C                                                                       COI01850
C                                                                       COI01860
C     ****************************************************************  COI01870
C                                                                       COI01880
C                                                                       COI01890
      DOUBLE PRECISION FUNCTION CYLBZ(ZZ, R, RAD, CURDEN, LENGTH)       COI01900
C                                                                       COI01910
C           (* Gives   the  z-component  of  the  magnetic  field *)    COI01920
C           (* produced  by  a  cylindrical  current sheet, whose *)    COI01930
C           (* radius  is  RAD, current density is CURDEN, length *)    COI01940
C           (* is  LENGTH,  and  whose  center  is  at origo. The *)    COI01950
C           (* central axis of the cylinder is the z-axis.        *)    COI01960
C           (* The  field  point  in this coordinate system is at *)    COI01970
C           (* (z=ZZ, r=R).                                       *)    COI01980
C                                                                       COI01990
      DOUBLE PRECISION ZZ, R, RAD, CURDEN, LENGTH                       COI02000
C                                                                       COI02010
      DOUBLE PRECISION KSI1, KSI2, R1, R2, K1, K2, C, DELK, ELLIP3      COI02020
C           (* KSI1, KSI2 : integration   starting  and  stopping *)    COI02030
C           (*              points                                *)    COI02040
C           (* R1, R2, K1, K2, C : defined in formulae below      *)    COI02050
C           (* DELK : complete  elliptic  integral  of  1st  kind *)    COI02060
C           (*        (IMSL)                                      *)    COI02070
C           (* ELLIP3 : complete elliptic integral of 3rd kind    *)    COI02080
C                  ...............................                      COI02090
C                                                                       COI02100
      KSI1 = ZZ - LENGTH/2D0                                            COI02110
      KSI2 = ZZ + LENGTH/2D0                                            COI02120
      R1 = (RAD+R)*(RAD+R) + KSI1*KSI1                                  COI02130
      R2 = (RAD+R)*(RAD+R) + KSI2*KSI2                                  COI02140
      K1 = 4D0*RAD*R/R1                                                 COI02150
      K2 = 4D0*RAD*R/R2                                                 COI02160
      C = 4D0*RAD*R/((RAD+R)*(RAD+R))                                   COI02170
C                                                                       COI02180
      CYLBZ = ((ELLIP3(C,K2)*(RAD-R) + DELK(K2)*(RAD+R))*KSI2*DSQRT(K2) COI02190
     1       - (ELLIP3(C,K1)*(RAD-R) + DELK(K1)*(RAD+R))*KSI1*DSQRT(K1))COI02200
     2      * (1D-7)*CURDEN*DSQRT(C)/(2*R*RAD)                          COI02210
C                                                                       COI02220
      RETURN                                                            COI02230
      END                                                               COI02240
C                                                                       COI02250
C                                                                       COI02260
C     ****************************************************************  COI02270
C                                                                       COI02280
C                                                                       COI02290
      DOUBLE PRECISION FUNCTION CYLBR(ZZ, R, RAD, CURDEN, LENGTH)       COI02300
C                                                                       COI02310
C           (* As  FUNCTION CYLBZ, but this gives the r-component *)    COI02320
C           (* of the field.                                      *)    COI02330
C                                                                       COI02340
      DOUBLE PRECISION ZZ, R, RAD, CURDEN, LENGTH                       COI02350
C                                                                       COI02360
      DOUBLE PRECISION KSI1, KSI2, R1, R2, K1, K2, DELK, DELE           COI02370
C           (* KSI1, KSI2 : integration   starting  and  stopping *)    COI02380
C           (*              points                                *)    COI02390
C           (* R1, R2, K1, K2 : defined in formulae below         *)    COI02400
C           (* DELK : complete  elliptic  integral  of  1st  kind *)    COI02410
C           (*        (IMSL)                                      *)    COI02420
C           (* DELE : complete  elliptic  integral  of  2nd  kind *)    COI02430
C           (*        (IMSL)                                      *)    COI02440
C                  ...............................                      COI02450
C                                                                       COI02460
      KSI1 = ZZ - LENGTH/2D0                                            COI02470
      KSI2 = ZZ + LENGTH/2D0                                            COI02480
      R1 = (RAD+R)*(RAD+R) + KSI1*KSI1                                  COI02490
      R2 = (RAD+R)*(RAD+R) + KSI2*KSI2                                  COI02500
      K1 = 4D0*RAD*R/R1                                                 COI02510
      K2 = 4D0*RAD*R/R2                                                 COI02520
C                                                                       COI02530
      CYLBR = ((2D0*DELE(K2)-(2D0-K2)*DELK(K2))*DSQRT(R2) -             COI02540
     1         (2D0*DELE(K1)-(2D0-K1)*DELK(K1))*DSQRT(R1)) *            COI02550
     2        (1D-7)*CURDEN/R                                           COI02560
C                                                                       COI02570
      RETURN                                                            COI02580
      END                                                               COI02590
C                                                                       COI02600
C                                                                       COI02610
C     ****************************************************************  COI02620
C                                                                       COI02630
C                                                                       COI02640
      DOUBLE PRECISION FUNCTION CYLBAX(ZZ, RAD, CURDEN, LENGTH)         COI02650
C                                                                       COI02660
C           (* Gives  the  z-component  of  the magnetic field on *)    COI02670
C           (* the  z-axis,  produced  by  a  cylindrical current *)    COI02680
C           (* sheet,  whose  radius  is  RAD, current density is *)    COI02690
C           (* CURDEN,  and length is LENGTH. The central axis of *)    COI02700
C           (* the  cylinder is the z-axis, and it is centered at *)    COI02710
C           (* z=0.  The field point in this coordinate system is *)    COI02720
C           (* at (z=ZZ, r=0).                                    *)    COI02730
C                                                                       COI02740
      DOUBLE PRECISION ZZ, RAD, CURDEN, LENGTH                          COI02750
C                                                                       COI02760
      DOUBLE PRECISION DCONST, PI                                       COI02770
C           (* DCONST : an IMSL-function for scientific constants *)    COI02780
C           (* PI : 3.14159....                                   *)    COI02790
C                  ...............................                      COI02800
C                                                                       COI02810
      PI = DCONST('PI')                                                 COI02820
      CYLBAX = 2D0*PI*(1D-7)*CURDEN *                                   COI02830
     1         ((ZZ+LENGTH/2D0) /                                       COI02840
     2               DSQRT((ZZ+LENGTH/2D0)*(ZZ+LENGTH/2D0) + RAD**2) -  COI02850
     3          (ZZ-LENGTH/2D0) /                                       COI02860
     4               DSQRT((ZZ-LENGTH/2D0)*(ZZ-LENGTH/2D0) + RAD**2))   COI02870
C                                                                       COI02880
      RETURN                                                            COI02890
      END                                                               COI02900
C                                                                       COI02910
C                                                                       COI02920
C     ****************************************************************  COI02930
C                                                                       COI02940
C                                                                       COI02950
      DOUBLE PRECISION FUNCTION DSKBZ(ZZ, RFIELD, RSOUR, CURDEN, WIDTH) COI02960
C                                                                       COI02970
C           (* Calculates  the  z-component of the magnetic field *)    COI02980
C           (* produced  by  a current disk located perpendicular *)    COI02990
C           (* to  the  z-axis  and  centered to origo. The outer *)    COI03000
C           (* radius  of the disk is RSOUR+WIDTH/2, and it has a *)    COI03010
C           (* hole of radius RSOUR-WIDTH/2 in the center.        *)    COI03020
C           (* The  field  point  in this coordinate system is at *)    COI03030
C           (* (z=ZZ, r=RFIELD).                                  *)    COI03040
C                                                                       COI03050
      DOUBLE PRECISION WIDTH, RSOUR, RFIELD, ZZ, CURDEN                 COI03060
C                                                                       COI03070
      DOUBLE PRECISION KSIF, RF, RSOUF, WF, ERRABS, ERREL, ERREST,      COI03080
     1                 PI, RAD1, RAD2, RESULT, DCONST,FDSKBZ            COI03090
      COMMON /AREA/ KSIF, RF, RSOUF, WF                                 COI03100
      EXTERNAL FDSKBZ                                                   COI03110
C           (* KSIF, RF, RSOUF, WF : new  names  for  ZZ, RFIELD, *)    COI03120
C           (*                       RSOUR  and WIDTH ; needed in *)    COI03130
C           (*                       FUNCTION FDSKBZ              *)    COI03140
C           (* ERRABS, ERREL, ERREST : required   and   estimated *)    COI03150
C           (*                         integration accuracies     *)    COI03160
C           (* PI : 3.14159....                                   *)    COI03170
C           (* RAD1, RAD2 : the  inner  and  outer  radius of the *)    COI03180
C           (*              current disk, respectively            *)    COI03190
C           (* RESULT : the  result  of  integration  of FUNCTION *)    COI03200
C           (*          FDSKBZ(X) FROM 0 TO PI                    *)    COI03210
C           (* DCONST : an IMSL-function for scientific constants *)    COI03220
C           (* FDSKBZ : the function which is integrated here     *)    COI03230
C                  ...............................                      COI03240
C                                                                       COI03250
      PI = DCONST('PI')                                                 COI03260
C                                                                       COI03270
      IF (RFIELD .EQ. 0) THEN                                           COI03280
C           (* if the field point is on central axis *)                 COI03290
         RAD1 = RSOUR - WIDTH/2D0                                       COI03300
         RAD2 = RSOUR + WIDTH/2D0                                       COI03310
         DSKBZ = (RAD1/DSQRT(RAD1*RAD1+ZZ*ZZ) -                         COI03320
     1            RAD2/DSQRT(RAD2*RAD2+ZZ*ZZ) +                         COI03330
     2            DLOG((DSQRT(RAD2*RAD2+ZZ*ZZ)+RAD2)/                   COI03340
     3                 (DSQRT(RAD1*RAD1+ZZ*ZZ)+RAD1))) *                COI03350
     4           PI*2D-7*CURDEN                                         COI03360
C                                                                       COI03370
      ELSE                                                              COI03380
C           (* if the field point not on central axis *)                COI03390
         ERRABS = 0D0                                                   COI03400
         ERREL = 1.0D-8                                                 COI03410
         KSIF = ZZ                                                      COI03420
         RF = RFIELD                                                    COI03430
         RSOUF = RSOUR                                                  COI03440
         WF = WIDTH                                                     COI03450
         CALL = DQDAGS(FDSKBZ, 0D0, PI, ERRABS, ERREL, RESULT, ERREST)  COI03460
C              (* integrate FUNCTION FDSKBZ(X) from 0 to PI *)          COI03470
         DSKBZ = RESULT*CURDEN                                          COI03480
      ENDIF                                                             COI03490
C                                                                       COI03500
      RETURN                                                            COI03510
      END                                                               COI03520
C                                                                       COI03530
C                                                                       COI03540
C     ****************************************************************  COI03550
C                                                                       COI03560
C                                                                       COI03570
      DOUBLE PRECISION FUNCTION DSKBR(ZZ, RFIELD, RSOUR, CURDEN, WIDTH) COI03580
C                                                                       COI03590
C           (* As  FUNCTION DSKBZ, but this gives the r-component *)    COI03600
C           (* of the field.                                      *)    COI03610
C                                                                       COI03620
      DOUBLE PRECISION WIDTH, RSOUR, RFIELD, ZZ, CURDEN                 COI03630
C                                                                       COI03640
      DOUBLE PRECISION KSIF, RF, RSOUF, WF, ERRABS, ERREL, ERREST,      COI03650
     1                 PI, RESULT, DCONST, FDSKBR                       COI03660
      COMMON /AREA/ KSIF, RF, RSOUF, WF                                 COI03670
      EXTERNAL FDSKBR                                                   COI03680
C           (* KSIF, RF, RSOUF, WF : new  names  for  ZZ, RFIELD, *)    COI03690
C           (*                       RSOUR  and WIDTH ; needed in *)    COI03700
C           (*                       FUNCTION FDSKBR              *)    COI03710
C           (* ERRABS, ERREL, ERREST : required   and   estimated *)    COI03720
C           (*                         integration accuracies     *)    COI03730
C           (* PI : 3.14159....                                   *)    COI03740
C           (* RESULT : the  result  of  integration  of FUNCTION *)    COI03750
C           (*          FDSKBR(X) FROM 0 TO PI                    *)    COI03760
C           (* DCONST : an IMSL-function for scientific constants *)    COI03770
C           (* FDSKBR : the function which is integrated here     *)    COI03780
C                  ...............................                      COI03790
C                                                                       COI03800
      PI = DCONST('PI')                                                 COI03810
C                                                                       COI03820
      IF (RFIELD .EQ. 0) THEN                                           COI03830
C           (* if the field point is on central axis *)                 COI03840
         DSKBR = 0                                                      COI03850
C                                                                       COI03860
      ELSE                                                              COI03870
C           (* if the field point not on central axis *)                COI03880
         ERRABS = 0D0                                                   COI03890
         ERREL = 1.0D-8                                                 COI03900
         KSIF = ZZ                                                      COI03910
         RF = RFIELD                                                    COI03920
         RSOUF = RSOUR                                                  COI03930
         WF = WIDTH                                                     COI03940
         CALL = DQDAGS(FDSKBR, 0D0, PI, ERRABS, ERREL, RESULT, ERREST)  COI03950
C              (* integrate FUNCTION FDSKBR(X) from 0 to PI *)          COI03960
         DSKBR = RESULT*CURDEN                                          COI03970
      ENDIF                                                             COI03980
C                                                                       COI03990
      RETURN                                                            COI04000
      END                                                               COI04010
C                                                                       COI04020
C                                                                       COI04030
C     ****************************************************************  COI04040
C                                                                       COI04050
C                                                                       COI04060
      DOUBLE PRECISION FUNCTION CYLSOU(KSI, R, RAD)                     COI04070
C                                                                       COI04080
C           (* (CYLSOU(KSI2, ...)-CYLSOU(KSI1, ...))*CURDEN gives *)    COI04090
C           (* the  vector  potential  produced  by a cylindrical *)    COI04100
C           (* current   sheet,  whose  radius  is  RAD,  current *)    COI04110
C           (* density  is  CURDEN, and whose center is at origo. *)    COI04120
C           (* The central axis of the cylinder is the z-axis.    *)    COI04130
C           (* The field point is as defined in FUNCTION CYLPOT.  *)    COI04140
C                                                                       COI04150
      DOUBLE PRECISION KSI, R, RAD                                      COI04160
C                                                                       COI04170
      DOUBLE PRECISION R1, K, C, DELK, DELE, ELLIP3, DMACH              COI04180
C           (* R1, K, C : defined in formulae below               *)    COI04190
C           (* DELK : complete elliptic integral of 1st kind      *)    COI04200
C           (*        (IMSL)                                      *)    COI04210
C           (* DELE : complete elliptic integral of 2nd kind      *)    COI04220
C           (*        (IMSL)                                      *)    COI04230
C           (* ELLIP3 : complete elliptic integral of 3rd kind    *)    COI04240
C           (* DMACH(1) : the  smallest  positive  number  of the *)    COI04250
C           (*            computer (IMSL)                         *)    COI04260
C                  ...............................                      COI04270
C                                                                       COI04280
      R1 = (RAD+R)*(RAD+R) + KSI*KSI                                    COI04290
      K = 4D0*RAD*R/R1                                                  COI04300
      C = 4D0*RAD*R/((RAD+R)*(RAD+R))                                   COI04310
C                                                                       COI04320
      IF ((1D0-C) .GE. (5*DMACH(1))**(1D0/3D0)) THEN                    COI04330
C           (* if C<>1 *)                                               COI04340
         CYLSOU = (DELK(K)*(R1+(RAD-R)*(RAD-R)) -                       COI04350
     1              ELLIP3(C,K)*(RAD-R)*(RAD-R) - DELE(K)*R1)           COI04360
     2            * (1D-7)*KSI/(R*DSQRT(R1))                            COI04370
C                                                                       COI04380
      ELSE                                                              COI04390
         IF ((1D0-K) .GE. (10*DMACH(1))) THEN                           COI04400
C              (* if C=1 and K<>1 *)                                    COI04410
            CYLSOU = (DELK(K)-DELE(K)) * (1D-7)*KSI*DSQRT(R1)/R         COI04420
C                                                                       COI04430
         ELSE                                                           COI04440
C              (* if C=K=1 *)                                           COI04450
            CYLSOU = 0D0                                                COI04460
         ENDIF                                                          COI04470
      ENDIF                                                             COI04480
C                                                                       COI04490
      RETURN                                                            COI04500
      END                                                               COI04510
C                                                                       COI04520
C                                                                       COI04530
C     ****************************************************************  COI04540
C                                                                       COI04550
C                                                                       COI04560
      DOUBLE PRECISION FUNCTION CYLPOT(HEIGHT, RSOUR, RFIELD, ZZ)       COI04570
C                                                                       COI04580
C           (* Calculates  the  vector potential per unit current *)    COI04590
C           (* density,  produced  by a cylindrical current sheet *)    COI04600
C           (* whose  radius  is  RSOUR and length is HEIGHT, and *)    COI04610
C           (* which  is  centered  at origo. The central axis of *)    COI04620
C           (* the cylider is the z-axis.                         *)    COI04630
C           (* The  field  point  in this coordinate system is at *)    COI04640
C           (* (z=ZZ, r=RFIELD).                                  *)    COI04650
C                                                                       COI04660
      DOUBLE PRECISION HEIGHT, RSOUR, RFIELD, ZZ                        COI04670
C                                                                       COI04680
      DOUBLE PRECISION KSI1, KSI2, CYLSOU                               COI04690
C           (* KSI1, KSI2, CYLSOU : CYLPOT = CYLSOU(KSI2, ....) - *)    COI04700
C           (*                               CYLSOU(KSI1, ....)   *)    COI04710
C                  ...............................                      COI04720
C                                                                       COI04730
      KSI1 = ZZ - HEIGHT/2D0                                            COI04740
      KSI2 = ZZ + HEIGHT/2D0                                            COI04750
C                                                                       COI04760
      CYLPOT = CYLSOU(KSI2, RFIELD, RSOUR) - CYLSOU(KSI1, RFIELD, RSOUR)COI04770
C                                                                       COI04780
      RETURN                                                            COI04790
      END                                                               COI04800
C                                                                       COI04810
C                                                                       COI04820
C     ****************************************************************  COI04830
C                                                                       COI04840
C                                                                       COI04850
      DOUBLE PRECISION FUNCTION DSKPOT(WIDTH, RSOUR, RFIELD, ZZ)        COI04860
C                                                                       COI04870
C           (* Calculates  the  vector potential per unit current *)    COI04880
C           (* density,   produced  by  a  current  disk  located *)    COI04890
C           (* perpendicular   to  the  z-axis  and  centered  to *)    COI04900
C           (* origo.  The  outer  radius  of  the disk is RSOUR+ *)    COI04910
C           (* WIDTH/2, and it has a hole of radius RSOUR-WIDTH/2 *)    COI04920
C           (* in the center.                                     *)    COI04930
C           (* The  field  point  in this coordinate system is at *)    COI04940
C           (* (z=ZZ, r=RFIELD).                                  *)    COI04950
C                                                                       COI04960
      DOUBLE PRECISION WIDTH, RSOUR, RFIELD, ZZ                         COI04970
C                                                                       COI04980
      DOUBLE PRECISION KSIF, RF, RSOUF, WF, ERRABS, ERREL, ERREST,      COI04990
     1                 PI, RESULT, DCONST, FDSPOT                       COI05000
      COMMON /AREA/ KSIF, RF, RSOUF, WF                                 COI05010
      EXTERNAL FDSPOT                                                   COI05020
C           (* KSIF, RF, RSOUF, WF : new  names  for  ZZ, RFIELD, *)    COI05030
C           (*                       RSOUR  and WIDTH ; needed in *)    COI05040
C           (*                       FUNCTION FDSPOT              *)    COI05050
C           (* ERRABS, ERREL, ERREST : required   and   estimated *)    COI05060
C           (*                         integration accuracies     *)    COI05070
C           (* PI : 3.14159....                                   *)    COI05080
C           (* RESULT : the  result  of  integration  of FUNCTION *)    COI05090
C           (*          FDSPOT(X) from 0 to PI                    *)    COI05100
C           (* DCONST : an IMSL-function for scientific constants *)    COI05110
C           (* FDSPOT : the function which is integrated here     *)    COI05120
C                  ...............................                      COI05130
C                                                                       COI05140
      PI = DCONST('PI')                                                 COI05150
C                                                                       COI05160
      ERRABS = 0D0                                                      COI05170
      ERREL = 1.0D-8                                                    COI05180
      KSIF = ZZ                                                         COI05190
      RF = RFIELD                                                       COI05200
      RSOUF = RSOUR                                                     COI05210
      WF = WIDTH                                                        COI05220
      CALL = DQDAGS(FDSPOT, 0D0, PI, ERRABS, ERREL, RESULT, ERREST)     COI05230
C           (* integrate FUNCTION FDSPOT(X) from 0 to PI *)             COI05240
      DSKPOT = RESULT                                                   COI05250
C                                                                       COI05260
      RETURN                                                            COI05270
      END                                                               COI05280
C                                                                       COI05290
C                                                                       COI05300
C     ****************************************************************  COI05310
C                                                                       COI05320
C                                                                       COI05330
      SUBROUTINE SOLCYL(LENSHI, RADSHI, CENSHI, LENSOL, RADSOL, CENSOL, COI05340
     1                 LAYERS, LOOPS, CUR, WIRDIA, FOILTH, N, ASTOR, NN)COI05350
C                                                                       COI05360
C           (* Calculates  the vector potential distribution on a *)    COI05370
C           (* cylinder  part of the superconducting shield, pro- *)    COI05380
C           (* duced  by a solenoid coil. The result is stored in *)    COI05390
C           (* the vector ASTOR(NN).                              *)    COI05400
C           (*                                                    *)    COI05410
C           (* The meanings of the input parameters are:          *)    COI05420
C           (*     LENSOL,  RADSOL,  LAYERS,  LOOPS, CUR, WIRDIA, *)    COI05430
C           (*        FOILTH : as in SUBROUTINE BSOLEN            *)    COI05440
C           (*     LENSHI : the length of the shield part         *)    COI05450
C           (*     RADSHI : the radius of the shield part         *)    COI05460
C           (*     CENSHI : the z-coordinate of the center of the *)    COI05470
C           (*              shield part                           *)    COI05480
C           (*     CENSOL : the z-coordinate of the center of the *)    COI05490
C           (*              solenoid coil                         *)    COI05500
C           (*     N : the  amount  of  rings  the shield part is *)    COI05510
C           (*         divided to                                 *)    COI05520
C           (*     ASTOR : the  vector  in  which  the  result is *)    COI05530
C           (*             stored                                 *)    COI05540
C           (*     NN : dimension of ASTOR                        *)    COI05550
C                                                                       COI05560
      INTEGER LAYERS, N, NN                                             COI05570
      DOUBLE PRECISION LENSHI, RADSHI, LENSOL, RADSOL, CENSOL, LOOPS,   COI05580
     1                 CUR, WIRDIA, FOILTH, ASTOR(NN)                   COI05590
C                                                                       COI05600
      INTEGER ILAY, J                                                   COI05610
      DOUBLE PRECISION CURDEN, RADLAY, Z, ZZ, CYLPOT                    COI05620
C           (* ILAY : the layer number                            *)    COI05630
C           (* J : the  index  of  the current ring of the shield *)    COI05640
C           (*     part                                           *)    COI05650
C           (* CURDEN : the current density in the solenoid       *)    COI05660
C           (* RADLAY : the  radius  of  the  ILAYth layer in the *)    COI05670
C           (*          solenoid coil                             *)    COI05680
C           (* Z : the  z-coordinate  of  the Jth current ring of *)    COI05690
C           (*     the shield cylinder*)                                COI05700
C           (* ZZ : z-component  of the vector from the center of *)    COI05710
C           (*      the  solenoid  coil  to the center of the Jth *)    COI05720
C           (*      current ring of the cylinder*)                      COI05730
C           (* CYLPOT : gives  the  vector  potential produced by *)    COI05740
C           (*          one layer of the solenoid                 *)    COI05750
C                  ...............................                      COI05760
C                                                                       COI05770
      CURDEN = CUR*LOOPS/LENSOL                                         COI05780
      RADLAY = RADSOL + WIRDIA/2D0                                      COI05790
C                                                                       COI05800
      DO 2320 ILAY = 1, LAYERS                                          COI05810
         DO 2310 J = 1, N                                               COI05820
            Z = CENSHI + LENSHI/2D0 - (J-0.5)*LENSHI/N                  COI05830
            ZZ = Z - CENSOL                                             COI05840
            ASTOR(J) = ASTOR(J) +                                       COI05850
     1                 CYLPOT(LENSOL, RADLAY, RADSHI, ZZ)*CURDEN        COI05860
 2310    CONTINUE                                                       COI05870
         RADLAY = RADLAY + WIRDIA + FOILTH                              COI05880
 2320 CONTINUE                                                          COI05890
C                                                                       COI05900
      RETURN                                                            COI05910
      END                                                               COI05920
C                                                                       COI05930
C                                                                       COI05940
C     ****************************************************************  COI05950
C                                                                       COI05960
C                                                                       COI05970
      SUBROUTINE SOLDSK(ZDSK, RADSHI, RHOLE, LENSOL, RADSOL, CENSOL,    COI05980
     1                 LAYERS, LOOPS, CUR, WIRDIA, FOILTH, N, ASTOR, NN)COI05990
C                                                                       COI06000
C           (* Calculates  the vector potential distribution on a *)    COI06010
C           (* disk  part of the superconducting shield, produced *)    COI06020
C           (* by  a  solenoid  coil. The result is stored in the *)    COI06030
C           (* vector ASTOR(NN).                                  *)    COI06040
C           (*                                                    *)    COI06050
C           (* The meanings of the input parameters are:          *)    COI06060
C           (*     LENSOL,  RADSOL,  LAYERS,  LOOPS, CUR, WIRDIA, *)    COI06070
C           (*        FOILTH : as in SUBROUTINE BSOLEN            *)    COI06080
C           (*     ZDSK : the z-coordinate of the shield disk     *)    COI06090
C           (*     RADSHI : the outer radius of the shield disk   *)    COI06100
C           (*     RHOLE : the  radius  of the hole in the center *)    COI06110
C           (*             of the shield disk                     *)    COI06120
C           (*     CENSOL : the z-coordinate of the center of the *)    COI06130
C           (*              solenoid coil                         *)    COI06140
C           (*     N : the  amount  of  rings  the shield disk is *)    COI06150
C           (*         divided to                                 *)    COI06160
C           (*     ASTOR : the  vector  in  which  the  result is *)    COI06170
C           (*             stored                                 *)    COI06180
C           (*     NN : dimension of ASTOR                        *)    COI06190
C                                                                       COI06200
      INTEGER LAYERS, N, NN                                             COI06210
      DOUBLE PRECISION ZDSK, RADSHI, RHOLE, LENSOL, RADSOL, CENSOL,     COI06220
     1          LOOPS, CUR, WIRDIA, FOILTH, ASTOR(NN)                   COI06230
C                                                                       COI06240
      INTEGER ILAY, J                                                   COI06250
      DOUBLE PRECISION CURDEN, RADLAY, ZZ, RFIELD, CYLPOT               COI06260
C           (* ILAY : the layer number                            *)    COI06270
C           (* J : the index of the current ring of the disk      *)    COI06280
C           (* CURDEN : the current density in the solenoid       *)    COI06290
C           (* RADLAY : the  radius  of  the  ILAYth layer in the *)    COI06300
C           (*          solenoid coil                             *)    COI06310
C           (* ZZ : z-component  of the vector from the center of *)    COI06320
C           (*      the solenoid coil to the shield disk          *)    COI06330
C           (* RFIELD : r-component of the field point            *)    COI06340
C           (* CYLPOT : gives  the  vector  potential produced by *)    COI06350
C           (*          one layer of the solenoid                 *)    COI06360
C                  ...............................                      COI06370
C                                                                       COI06380
      CURDEN = CUR*LOOPS/LENSOL                                         COI06390
      RADLAY = RADSOL + WIRDIA/2D0                                      COI06400
C                                                                       COI06410
      DO 2420 ILAY = 1, LAYERS                                          COI06420
         DO 2410 J = 1, N                                               COI06430
            RFIELD = RHOLE + (J-0.5)*(RADSHI-RHOLE)/N                   COI06440
            ZZ = ZDSK - CENSOL                                          COI06450
            ASTOR(J) = ASTOR(J) +                                       COI06460
     1                 CYLPOT(LENSOL, RADLAY, RFIELD, ZZ)*CURDEN        COI06470
 2410    CONTINUE                                                       COI06480
         RADLAY = RADLAY + WIRDIA + FOILTH                              COI06490
 2420 CONTINUE                                                          COI06500
C                                                                       COI06510
      RETURN                                                            COI06520
      END                                                               COI06530
C                                                                       COI06540
C                                                                       COI06550
C     ****************************************************************  COI06560
C                                                                       COI06570
C                                                                       COI06580
      SUBROUTINE BSOLEN(ZZ, R, LENSOL, RADSOL, WIRDIA, LOOPS, LAYERS,   COI06590
     1                  CUR, FOILTH, BZSOL, BRSOL)                      COI06600
C                                                                       COI06610
C           (* Gives  the  magnetic field produced by a solenoid, *)    COI06620
C           (* whose  central  axis  is  the  z-axis and which is *)    COI06630
C           (* centered  to  z=0. The field point in this coordi- *)    COI06640
C           (* nate system is at (z=ZZ, r=R).                     *)    COI06650
C           (* The meanings of the input parameters are:          *)    COI06660
C           (*     ZZ : z-coordinate of the field point           *)    COI06670
C           (*     R : r-coordinate of the field point            *)    COI06680
C           (*     LENSOL : the length of the solenoid            *)    COI06690
C           (*     RADSOL : the inner radius of the solenoid      *)    COI06700
C           (*     WIRDIA : the diameter of the current lead      *)    COI06710
C           (*     LOOPS : the number of turns in one layer       *)    COI06720
C           (*     LAYERS : the number of layers in the solenoid  *)    COI06730
C           (*     CUR : the current in the lead                  *)    COI06740
C           (*     FOILTH : the  thickness of the insulating foil *)    COI06750
C           (*              between the layers                    *)    COI06760
C           (*     BZSOL : the z-component of the magnetic field  *)    COI06770
C           (*     BRSOL : the r-component of the magnetic field  *)    COI06780
C                                                                       COI06790
      INTEGER LAYERS                                                    COI06800
      DOUBLE PRECISION ZZ, R, LENSOL, RADSOL, WIRDIA, LOOPS, CUR,       COI06810
     1                 FOILTH, BZSOL, BRSOL, CYLBAX                     COI06820
C                                                                       COI06830
      INTEGER I                                                         COI06840
      DOUBLE PRECISION RADLAY, CURDEN, CYLBZ, CYLBR                     COI06850
C           (* I : the layer number                               *)    COI06860
C           (* RADLAY : the radius of the Ith layer               *)    COI06870
C           (* CURDEN : the current density in the solenoid       *)    COI06880
C           (* CYLBZ, CYLBR : give the z- and r-components of the *)    COI06890
C           (*                magnetic   field  produced  by  one *)    COI06900
C           (*                layer of the solenoid               *)    COI06910
C           (* CYLBAX : gives the magnetic field on central axis  *)    COI06920
C                  ...............................                      COI06930
C                                                                       COI06940
      RADLAY = RADSOL + WIRDIA/2D0                                      COI06950
      CURDEN = LOOPS*CUR/LENSOL                                         COI06960
C                                                                       COI06970
      DO 2510 I = 1, LAYERS                                             COI06980
C                                                                       COI06990
         IF (R .EQ. 0) THEN                                             COI07000
C              (* if the field point is on central axis *)              COI07010
            BZSOL = BZSOL + CYLBAX(ZZ, RADLAY, CURDEN, LENSOL)          COI07020
C                                                                       COI07030
         ELSE                                                           COI07040
C              (* if the field point not on central axis *)             COI07050
            BZSOL = BZSOL + CYLBZ(ZZ, R, RADLAY, CURDEN, LENSOL)        COI07060
            BRSOL = BRSOL + CYLBR(ZZ, R, RADLAY, CURDEN, LENSOL)        COI07070
         ENDIF                                                          COI07080
C                                                                       COI07090
         RADLAY = RADLAY + WIRDIA + FOILTH                              COI07100
 2510 CONTINUE                                                          COI07110
C                                                                       COI07120
      RETURN                                                            COI07130
      END                                                               COI07140
C                                                                       COI07150
C                                                                       COI07160
C     ****************************************************************  COI07170
C                                                                       COI07180
C                                                                       COI07190
      SUBROUTINE WRSHI(FILE, MAX, NSHI, LENSHI, RADSHI, RHOLE, CENSHI)  COI07200
C                                                                       COI07210
C           (* Writes  the dimensions of the shield parts to file *)    COI07220
C           (* number FILE ; FILE=6 corresponds to the terminal.  *)    COI07230
C                                                                       COI07240
      INTEGER FILE, MAX, NSHI                                           COI07250
      DOUBLE PRECISION LENSHI(MAX), RADSHI(MAX), RHOLE(MAX), CENSHI(MAX)COI07260
C                                                                       COI07270
      INTEGER ISHI                                                      COI07280
C           (* ISHI : an index for the shield parts               *)    COI07290
C                  ...............................                      COI07300
C                                                                       COI07310
      IF (NSHI .EQ. 0) THEN                                             COI07320
C           (* if no shield *)                                          COI07330
         WRITE(FILE,2610)                                               COI07340
 2610    FORMAT(' Now you have no shield at all.')                      COI07350
C                                                                       COI07360
      ELSE                                                              COI07370
C           (* if there is a shield *)                                  COI07380
         DO 2680 ISHI = 1, NSHI                                         COI07390
            WRITE (FILE,*) ' '                                          COI07400
            IF (LENSHI(ISHI) .EQ. 0) WRITE(FILE,2620) ISHI              COI07410
 2620       FORMAT(1X,'Shield part number ',I2,' is a disk, whose',     COI07420
     1             ' dimensions are: ')                                 COI07430
            IF (LENSHI(ISHI) .NE. 0) WRITE(FILE,2630) ISHI              COI07440
 2630       FORMAT(1X,'Shield part number ',I2,' is a cylinder, whose', COI07450
     1             ' dimensions are: ')                                 COI07460
C                                                                       COI07470
            IF (LENSHI(ISHI) .NE. 0) WRITE(FILE,2640) ISHI,             COI07480
     1                                               LENSHI(ISHI)*1000  COI07490
C                 (* length of cylindrical shield part *)               COI07500
 2640       FORMAT(5X,'LENSHI(',I2,') = ',F7.3,' mm')                   COI07510
C                                                                       COI07520
            WRITE(FILE,2650) ISHI, RADSHI(ISHI)*1000                    COI07530
C                 (* radius of cylindrical and outer radius of *)       COI07540
C                 (* planar shield part                        *)       COI07550
 2650       FORMAT(5X,'RADSHI(',I2,') = ',F7.3,' mm')                   COI07560
C                                                                       COI07570
            IF (LENSHI(ISHI) .EQ. 0) WRITE(FILE,2660) ISHI,             COI07580
     1                                               RHOLE(ISHI)*1000   COI07590
C                 (* radius of the hole in the planar shield part *)    COI07600
 2660       FORMAT(5X,'RHOLE(',I2,') = ',F7.3,' mm')                    COI07610
C                                                                       COI07620
            WRITE(FILE,2670) ISHI, CENSHI(ISHI)*1000                    COI07630
C                 (* z-coordinate of the center of the shield part *)   COI07640
 2670       FORMAT(5X,'CENSHI(',I2,') = ',F7.3,' mm')                   COI07650
C                                                                       COI07660
 2680    CONTINUE                                                       COI07670
      ENDIF                                                             COI07680
C                                                                       COI07690
      RETURN                                                            COI07700
      END                                                               COI07710
C                                                                       COI07720
C                                                                       COI07730
C     ****************************************************************  COI07740
C                                                                       COI07750
C                                                                       COI07760
      SUBROUTINE WROTH(FILE, MAX, LENSOL, RADSOL, CENSOL, LAYERS, LOOPS,COI07770
     1                 SYMSOL, CUR, WIRDIA, FOILTH, BEXT, TRAFLU)       COI07780
C                                                                       COI07790
C           (* Writes  the  dimensions  of the solenoid coils and *)    COI07800
C           (* the  other  parameters  of  the system, except the *)    COI07810
C           (* dimensions  of  the  shield  parts, to file number *)    COI07820
C           (* FILE ; FILE=6 corresponds to the terminal.         *)    COI07830
C                                                                       COI07840
      INTEGER FILE, MAX, LAYERS(MAX)                                    COI07850
      DOUBLE PRECISION LENSOL(MAX), RADSOL(MAX), CENSOL(MAX),           COI07860
     1                LOOPS(MAX), CUR(MAX), WIRDIA, FOILTH, BEXT, TRAFLUCOI07870
      LOGICAL SYMSOL(MAX)                                               COI07880
C                                                                       COI07890
      INTEGER ISOL                                                      COI07900
C           (* ISOL : an index for the solenoid parts             *)    COI07910
C                  ...............................                      COI07920
C                                                                       COI07930
      IF (LENSOL(1) .NE. 0) THEN                                        COI07940
C           (* if there are solenoids *)                                COI07950
         ISOL = 1                                                       COI07960
C                                                                       COI07970
 2710    CONTINUE                                                       COI07980
            WRITE(FILE,*) ('  ')                                        COI07990
            WRITE(FILE,2720) ISOL, LENSOL(ISOL)*1000, ISOL,             COI08000
     1                 RADSOL(ISOL)*1000, ISOL, CENSOL(ISOL)*1000, ISOL,COI08010
     2                 LAYERS(ISOL), ISOL, LOOPS(ISOL), ISOL,           COI08020
     3                 SYMSOL(ISOL), ISOL, CUR(ISOL)                    COI08030
C                 (* length,  inner  radius, z-coordinate of the *)     COI08040
C                 (* center, number of layers, number of current *)     COI08050
C                 (* loops in one layer, is there a similar part *)     COI08060
C                 (* located symmetrically, current in the wire  *)     COI08070
 2720       FORMAT(1X,'LENSOL(',I2,') = ',F7.3,' mm' / 1X,'RADSOL(',I2, COI08080
     1          ') = ',F7.3,' mm' / 1X,'CENSOL(',I2,') = ',F7.3,' mm' / COI08090
     2          1X,'LAYERS(',I2,') =',I4 / 1X,'LOOPS(',I2,') = ',       COI08100
     3          F7.3, / 1X,'SYMSOL(',I2,') = ',L2 / 1X,'CUR(',I2,       COI08110
     4          ') = ',F7.3,' A')                                       COI08120
            ISOL = ISOL + 1                                             COI08130
            IF (LENSOL(ISOL) .NE. 0) GOTO 2710                          COI08140
C                 (* if there are more solenoids *)                     COI08150
         CONTINUE                                                       COI08160
C                                                                       COI08170
         WRITE(FILE,*) ('  ')                                           COI08180
         WRITE(FILE,2730) WIRDIA*1000                                   COI08190
C              (* the diameter of the wire *)                           COI08200
 2730    FORMAT(1X,'WIRDIA = ',F7.4,' mm')                              COI08210
C                                                                       COI08220
         WRITE(FILE,2740) FOILTH*1000                                   COI08230
C              (* the thickness of insulating foil between layers *)    COI08240
 2740    FORMAT(1X,'FOILTH = ',F7.4,' mm')                              COI08250
C                                                                       COI08260
      ELSE                                                              COI08270
C           (* if no solenoids *)                                       COI08280
         WRITE(FILE,*) ('  ')                                           COI08290
         WRITE(FILE,2750)                                               COI08300
 2750    FORMAT(' Now you have no solenoids. ')                         COI08310
      ENDIF                                                             COI08320
C                                                                       COI08330
      WRITE(FILE,*) ('  ')                                              COI08340
      IF (BEXT .NE. 0) THEN                                             COI08350
C           (* if there is external field *)                            COI08360
         WRITE(FILE,2760) BEXT*10000                                    COI08370
C              (* external field in Gausses *)                          COI08380
 2760    FORMAT(' BEXT = ', G9.4, ' Gauss ')                            COI08390
      ELSE                                                              COI08400
C           (* if no external field *)                                  COI08410
         WRITE(FILE,2770)                                               COI08420
 2770    FORMAT(' BEXT = 0.0 Gauss ')                                   COI08430
      ENDIF                                                             COI08440
C                                                                       COI08450
      IF (TRAFLU .NE. 0) THEN                                           COI08460
C           (* if there is trapped flux in the shield *)                COI08470
         WRITE(FILE,2780) TRAFLU*1D8                                    COI08480
C              (* trapped flux in the shield in Gauss*cm*cm *)          COI08490
 2780    FORMAT(' FLUX = ', G9.4,' Gauss*cm*cm  ')                      COI08500
      ELSE                                                              COI08510
C           (* if no trapped flux *)                                    COI08520
         WRITE(FILE,2790)                                               COI08530
 2790    FORMAT(' FLUX = 0.0 Gauss*cm*cm  ')                            COI08540
      ENDIF                                                             COI08550
C                                                                       COI08560
      RETURN                                                            COI08570
      END                                                               COI08580
C                                                                       COI08590
C                                                                       COI08600
C     ****************************************************************  COI08610
C                                                                       COI08620
C                                                                       COI08630
      SUBROUTINE DIMENS(NSHI, DIFSHI, LENSHI, RADSHI, RHOLE, CENSHI,    COI08640
     1             SYMSHI, LENSOL, RADSOL, CENSOL, LAYERS, LOOPS,       COI08650
     2             SYMSOL, CUR, WIRDIA, FOILTH, BEXT, TRAFLU, MAX)      COI08660
C                                                                       COI08670
C           (* Asks the dimensions, currents etc. of the system.  *)    COI08680
C                                                                       COI08690
      INTEGER MAX, NSHI, LAYERS(MAX)                                    COI08700
      DOUBLE PRECISION LENSHI(MAX), RADSHI(MAX), RHOLE(MAX),            COI08710
     1               CENSHI(MAX), LENSOL(MAX), RADSOL(MAX), CENSOL(MAX),COI08720
     2               LOOPS(MAX), CUR(MAX), WIRDIA, FOILTH, BEXT, TRAFLU COI08730
      LOGICAL DIFSHI, SYMSHI(MAX), SYMSOL(MAX)                          COI08740
C                                                                       COI08750
      INTEGER ISHI, ISOL                                                COI08760
      LOGICAL HOLE                                                      COI08770
      CHARACTER*1 ANSW                                                  COI08780
C           (* ISHI : index of the shield part                    *)    COI08790
C           (* ISOL : index of the solenoid                       *)    COI08800
C           (* HOLE : .TRUE. if there is a hole through the whole *)    COI08810
C           (*        shield,  so  that  there  can  be a trapped *)    COI08820
C           (*        flux ; .FALSE. otherwise                    *)    COI08830
C                  ...............................                      COI08840
C                                                                       COI08850
      WRITE(6,2801)                                                     COI08860
 2801 FORMAT('0This  program  calculates  magnetic  fields  produced ', COI08870
     1'by  a system of' / ' cocentric  coils  in a superconducting ',   COI08880
     2'shield. The shield must consist' / ' of  cocentric cylinders ',  COI08890
     3'and circular disks. There can be a hole in the' /                COI08900
     4' center of each disk.' / '0All dimensions are given in mm:s|')   COI08910
C                                                                       COI08920
 2802 CONTINUE                                                          COI08930
C                                                                       COI08940
      WRITE(6,*) ('  ')                                                 COI08950
      CALL WRSHI(6, MAX, NSHI, LENSHI, RADSHI, RHOLE, CENSHI)           COI08960
C                                                                       COI08970
      WRITE(6,2803)                                                     COI08980
 2803 FORMAT('0Do you want to change the shield (Y = yes, N = no)?')    COI08990
C                                                                       COI09000
 2804 CONTINUE                                                          COI09010
         REWIND 5                                                       COI09020
         READ(5, '(A1)', ERR=2804, END=2804) ANSW                       COI09030
         IF ((ANSW .NE. 'N') .AND. (ANSW .NE. 'Y')) WRITE(6,2805)       COI09040
 2805    FORMAT(' Give either "Y" or "N" |')                            COI09050
         IF ((ANSW .NE. 'N') .AND. (ANSW .NE. 'Y')) GOTO 2804           COI09060
C              (* repeat asking until the answer is acceptable *)       COI09070
      CONTINUE                                                          COI09080
C                                                                       COI09090
      IF (ANSW .EQ. 'Y') THEN                                           COI09100
C           (* new shield *)                                            COI09110
         DIFSHI = .TRUE.                                                COI09120
C                                                                       COI09130
         ISHI = 1                                                       COI09140
C                                                                       COI09150
         WRITE(6,2806)                                                  COI09160
 2806    FORMAT('0Have you any superconducting shield ',                COI09170
     1          '(Y = yes, N = no)?')                                   COI09180
C                                                                       COI09190
 2807    CONTINUE                                                       COI09200
            REWIND 5                                                    COI09210
            READ(5, '(A1)', ERR=2807, END=2807) ANSW                    COI09220
            IF ((ANSW .NE. 'N') .AND. (ANSW .NE. 'Y')) WRITE(6,2808)    COI09230
 2808       FORMAT(' Give either "Y" or "N" |')                         COI09240
            IF ((ANSW .NE. 'N') .AND. (ANSW .NE. 'Y')) GOTO 2807        COI09250
         CONTINUE                                                       COI09260
C                                                                       COI09270
         IF (ANSW .EQ. 'N') GOTO 2827                                   COI09280
C              (* if no shield at all *)                                COI09290
C                                                                       COI09300
 2809    CONTINUE                                                       COI09310
 2810       CONTINUE                                                    COI09320
            WRITE(6,2811) ISHI, ISHI, LENSHI(ISHI)*1000                 COI09330
 2811       FORMAT('0Give the length of the shield part number ',I2,    COI09340
     1             ' (LENSHI(',I2,') = ',F6.2,' mm). ' / ' If the',     COI09350
     2             ' part is a disk, give 0. ')                         COI09360
            REWIND 5                                                    COI09370
            READ(5, *, ERR=2810, END=2812) LENSHI(ISHI)                 COI09380
C                 (* REWIND 5 & END=2812  =>  if only Enter is given, *)COI09390
C                 (* the value of LENSHI will not be changed          *)COI09400
            LENSHI(ISHI) = LENSHI(ISHI)/1000D0                          COI09410
C                 (* change the dimension from mm:s to m:s *)           COI09420
C                                                                       COI09430
 2812       CONTINUE                                                    COI09440
C                                                                       COI09450
            WRITE(6,2813) ISHI, ISHI, RADSHI(ISHI)*1000                 COI09460
 2813       FORMAT('0Give the radius of the shield part number ',I2,    COI09470
     1             ' (RADSHI(',I2,') = ',F6.2,' mm).' / ' If the',      COI09480
     2             ' part is a disk, give its outer radius. ')          COI09490
            REWIND 5                                                    COI09500
            READ(5, *, ERR=2812, END=2814) RADSHI(ISHI)                 COI09510
            RADSHI(ISHI) = RADSHI(ISHI)/1000D0                          COI09520
C                                                                       COI09530
 2814       CONTINUE                                                    COI09540
C                                                                       COI09550
            IF (LENSHI(ISHI) .EQ. 0) THEN                               COI09560
C                 (* if the shield part is a disk *)                    COI09570
 2815          CONTINUE                                                 COI09580
               WRITE(6,2816) ISHI, RHOLE(ISHI)*1000                     COI09590
 2816          FORMAT('0Give the radius of the hole in the disk',       COI09600
     1                ' (RHOLE(',I2,') = ', F6.2, ' mm). ')             COI09610
               REWIND 5                                                 COI09620
               READ(5, *, ERR=2815, END=2817) RHOLE(ISHI)               COI09630
               RHOLE(ISHI) = RHOLE(ISHI)/1000D0                         COI09640
 2817          CONTINUE                                                 COI09650
            ENDIF                                                       COI09660
C                                                                       COI09670
 2818       CONTINUE                                                    COI09680
C                                                                       COI09690
            WRITE(6,2819) ISHI, ISHI, CENSHI(ISHI)*1000                 COI09700
 2819       FORMAT('0Give the z-coordinate of the center of the ',      COI09710
     1             'shield part number ',I2 / ' (CENSHI(',I2,') = ',    COI09720
     2             F6.2,' mm). ')                                       COI09730
            REWIND 5                                                    COI09740
            READ(5, *, ERR=2818, END=2820) CENSHI(ISHI)                 COI09750
            CENSHI(ISHI) = CENSHI(ISHI)/1000D0                          COI09760
C                                                                       COI09770
 2820       CONTINUE                                                    COI09780
C                                                                       COI09790
            IF (((ABS(CENSHI(ISHI)) - LENSHI(ISHI)/2) .GE. 0) .AND.     COI09800
     1          (CENSHI(ISHI) .NE. 0)) THEN                             COI09810
C                 (* if there is space for a symmetrical shield part *) COI09820
 2821          CONTINUE                                                 COI09830
                  WRITE(6,2822) -CENSHI(ISHI)*1000, ISHI, SYMSHI(ISHI)  COI09840
 2822             FORMAT('0Is there a similar part centered to z = ',   COI09850
     1                   F6.2, ' mm (Y/N)?' / ' If there is one, ',     COI09860
     2                   'don''t give its dimensions any more|' /       COI09870
     3                   ' Now SYMSHI(', I2, ') = ', L2, '. ')          COI09880
                  REWIND 5                                              COI09890
                  READ(5, '(A1)', ERR=2821, END=2823) ANSW              COI09900
                  IF ((ANSW .NE. 'Y') .AND. (ANSW .NE. 'N')) GOTO 2821  COI09910
               CONTINUE                                                 COI09920
               SYMSHI(ISHI) = (ANSW .EQ. 'Y')                           COI09930
 2823          CONTINUE                                                 COI09940
            ELSE                                                        COI09950
C                 (* if no space for a symmetrical part *)              COI09960
               SYMSHI(ISHI) = .FALSE.                                   COI09970
            ENDIF                                                       COI09980
C                                                                       COI09990
            IF (SYMSHI(ISHI)) THEN                                      COI10000
C                 (* the index for symmetrical part will be ISHI+1 *)   COI10010
               LENSHI(ISHI+1) = LENSHI(ISHI)                            COI10020
               RADSHI(ISHI+1) = RADSHI(ISHI)                            COI10030
               RHOLE(ISHI+1) = RHOLE(ISHI)                              COI10040
               CENSHI(ISHI+1) = -CENSHI(ISHI)                           COI10050
            ENDIF                                                       COI10060
C                                                                       COI10070
            IF (.NOT. SYMSHI(ISHI)) THEN                                COI10080
               ISHI = ISHI + 1                                          COI10090
            ELSE                                                        COI10100
               ISHI = ISHI + 2                                          COI10110
            ENDIF                                                       COI10120
C                                                                       COI10130
            WRITE(6,2824)                                               COI10140
 2824       FORMAT('0Are there any more shield parts ',                 COI10150
     1             '(Y = yes, N = no)?')                                COI10160
C                                                                       COI10170
 2825       CONTINUE                                                    COI10180
               REWIND 5                                                 COI10190
               READ(5, '(A1)', ERR=2825, END=2825) ANSW                 COI10200
               IF ((ANSW .NE. 'N') .AND. (ANSW .NE. 'Y')) WRITE(6,2826) COI10210
 2826          FORMAT(' Give either "Y" or "N" |')                      COI10220
               IF ((ANSW .NE. 'N') .AND. (ANSW .NE. 'Y')) GOTO 2825     COI10230
            CONTINUE                                                    COI10240
C                                                                       COI10250
            IF (ANSW .EQ. 'Y') GOTO 2809                                COI10260
C              (* if there are more shield parts *)                     COI10270
         CONTINUE                                                       COI10280
C                                                                       COI10290
 2827    CONTINUE                                                       COI10300
C                                                                       COI10310
         NSHI = ISHI - 1                                                COI10320
C              (* now the value of NSHI will be the actual number *)    COI10330
C              (* of shield parts                                 *)    COI10340
C                                                                       COI10350
      ELSE                                                              COI10360
C           (* if the shield was not changed *)                         COI10370
         DIFSHI = .FALSE.                                               COI10380
      ENDIF                                                             COI10390
C                                                                       COI10400
      ISOL = 1                                                          COI10410
C                                                                       COI10420
 2828 CONTINUE                                                          COI10430
         WRITE(6,2829) ISOL, ISOL, LENSOL(ISOL)*1000                    COI10440
 2829    FORMAT('0Give the length of the solenoid number ', I2,         COI10450
     1          ' (LENSOL(', I2, ') = ', F6.2, ' mm). ' / ' If no ',    COI10460
     2          'more solenoids, give 0. ')                             COI10470
         REWIND 5                                                       COI10480
         READ(5, *, ERR=2828, END=2830) LENSOL(ISOL)                    COI10490
C              (* REWIND 5 & END=2830  =>  if only Enter is given, *)   COI10500
C              (* the value of LENSOL will not be changed          *)   COI10510
         LENSOL(ISOL) = LENSOL(ISOL)/1000D0                             COI10520
C                 (* change the dimension from mm:s to m:s *)           COI10530
 2830    CONTINUE                                                       COI10540
C                                                                       COI10550
         IF (LENSOL(ISOL) .EQ. 0) GOTO 2845                             COI10560
C              (* if no more solenoids *)                               COI10570
C                                                                       COI10580
         WRITE(6,2831) ISOL, ISOL, RADSOL(ISOL)*1000                    COI10590
 2831    FORMAT('0Give the inner radius of the solenoid number ',I2,    COI10600
     1          ' (RADSOL(',I2,') = ', F6.2, ' mm). ')                  COI10610
         REWIND 5                                                       COI10620
         READ(5, *, ERR=2830, END=2832) RADSOL(ISOL)                    COI10630
         RADSOL(ISOL) = RADSOL(ISOL)/1000D0                             COI10640
C                                                                       COI10650
 2832    CONTINUE                                                       COI10660
C                                                                       COI10670
         WRITE(6,2833) ISOL, ISOL, CENSOL(ISOL)*1000                    COI10680
 2833    FORMAT('0Give the center of the solenoid number ',I2,          COI10690
     1          ' (CENSOL(',I2,') = ', F6.2, ' mm). ')                  COI10700
         REWIND 5                                                       COI10710
         READ(5, *, ERR=2832, END=2834) CENSOL(ISOL)                    COI10720
         CENSOL(ISOL) = CENSOL(ISOL)/1000D0                             COI10730
C                                                                       COI10740
 2834    CONTINUE                                                       COI10750
C                                                                       COI10760
         WRITE(6,2835) ISOL, ISOL, LAYERS(ISOL)                         COI10770
 2835    FORMAT('0Give the number of layers in the solenoid ',          COI10780
     1          'number ',I2,' (LAYERS(',I2,') = ', I2, '). ')          COI10790
         REWIND 5                                                       COI10800
         READ(5, *, ERR=2834, END=2836) LAYERS(ISOL)                    COI10810
C                                                                       COI10820
 2836    CONTINUE                                                       COI10830
C                                                                       COI10840
         WRITE(6,2837) ISOL, ISOL, LOOPS(ISOL)                          COI10850
 2837    FORMAT('0Give the number of turns per layer in the solenoid',  COI10860
     1          ' number ', I2 / ' (LOOPS(', I2, ') = ', F6.2, '). ')   COI10870
         REWIND 5                                                       COI10880
         READ(5, *, ERR=2836, END=2838) LOOPS(ISOL)                     COI10890
C                                                                       COI10900
 2838    CONTINUE                                                       COI10910
C                                                                       COI10920
 2839    CONTINUE                                                       COI10930
         WRITE(6,2840) ISOL, ISOL, CUR(ISOL)                            COI10940
 2840    FORMAT('0Give the current in the solenoid number ', I2,        COI10950
     1          ' in Amps (CUR(', I2, ') = ', F6.3, '). ')              COI10960
         REWIND 5                                                       COI10970
         READ(5, *, ERR=2839, END=2841) CUR(ISOL)                       COI10980
C                                                                       COI10990
 2841    CONTINUE                                                       COI11000
C                                                                       COI11010
         IF ((ABS(CENSOL(ISOL)) - LENSOL(ISOL)/2) .GE. 0) THEN          COI11020
C              (* if there is space for a symmetrical solenoid *)       COI11030
 2842       CONTINUE                                                    COI11040
               WRITE(6,2843) -CENSOL(ISOL)*1000, ISOL, SYMSOL(ISOL)     COI11050
 2843          FORMAT('0Is there a similar solenoid with the same ',    COI11060
     1              'current centered to z = ', F6.2, ' mm' / ' (Y/N)?',COI11070
     2               ' If there is one, don''t give its dimensions ',   COI11080
     3               'any more|' / ' Now SYMSOL(', I2, ') = ', L2, '. ')COI11090
               REWIND 5                                                 COI11100
               READ(5, '(A1)', ERR=2842, END=2844) ANSW                 COI11110
               IF ((ANSW .NE. 'Y') .AND. (ANSW .NE. 'N')) GOTO 2842     COI11120
            CONTINUE                                                    COI11130
            SYMSOL(ISOL) = (ANSW .EQ. 'Y')                              COI11140
 2844       CONTINUE                                                    COI11150
C                                                                       COI11160
         ELSE                                                           COI11170
C              (* if no space for a symmetrical solenoid *)             COI11180
            SYMSOL(ISOL) = .FALSE.                                      COI11190
         ENDIF                                                          COI11200
C                                                                       COI11210
         IF (SYMSOL(ISOL)) THEN                                         COI11220
C              (* the index for symmetrical coil will be ISOL+1 *)      COI11230
            LENSOL(ISOL+1) = LENSOL(ISOL)                               COI11240
            RADSOL(ISOL+1) = RADSOL(ISOL)                               COI11250
            CENSOL(ISOL+1) = -CENSOL(ISOL)                              COI11260
            LAYERS(ISOL+1) = LAYERS(ISOL)                               COI11270
            LOOPS(ISOL+1) = LOOPS(ISOL)                                 COI11280
            SYMSOL(ISOL+1) = SYMSOL(ISOL)                               COI11290
            CUR(ISOL+1) = CUR(ISOL)                                     COI11300
         ENDIF                                                          COI11310
C                                                                       COI11320
         IF (.NOT. SYMSOL(ISOL)) THEN                                   COI11330
            ISOL = ISOL + 1                                             COI11340
         ELSE                                                           COI11350
            ISOL = ISOL + 2                                             COI11360
         ENDIF                                                          COI11370
C                                                                       COI11380
         GOTO 2828                                                      COI11390
C                                                                       COI11400
 2845 CONTINUE                                                          COI11410
C                                                                       COI11420
      IF (LENSOL(1) .NE. 0) THEN                                        COI11430
C           (* if there are solenoids *)                                COI11440
 2846    CONTINUE                                                       COI11450
C                                                                       COI11460
         WRITE(6,2847) WIRDIA*1000                                      COI11470
 2847    FORMAT('0Give the diameter of the wire (WIRDIA = ', F7.4,      COI11480
     1          ' mm). ')                                               COI11490
         REWIND 5                                                       COI11500
         READ(5, *, ERR=2846, END=2848) WIRDIA                          COI11510
         WIRDIA = WIRDIA/1000D0                                         COI11520
C                                                                       COI11530
 2848    CONTINUE                                                       COI11540
C                                                                       COI11550
 2849    CONTINUE                                                       COI11560
         WRITE(6,2850) FOILTH*1000                                      COI11570
 2850    FORMAT('0Give the thickness of the foil between the layers ',  COI11580
     1          '(FOILTH) = ', F7.4, ' mm). ' / ' If there is no foil ',COI11590
     2          'and you wind layers directly on top of each other,' /  COI11600
     3          ' putting the wire onto the notch between adjacent ',   COI11610
     4          'turns in the previous' / ' layer, give -0.134 times ', COI11620
     5          'the thickness of the wire.')                           COI11630
         REWIND 5                                                       COI11640
         READ(5, *, ERR=2849, END=2851) FOILTH                          COI11650
         FOILTH = FOILTH/1000D0                                         COI11660
 2851    CONTINUE                                                       COI11670
C                                                                       COI11680
      ENDIF                                                             COI11690
C                                                                       COI11700
 2852 CONTINUE                                                          COI11710
      IF (BEXT .EQ. 0) THEN                                             COI11720
         WRITE(6,2853)                                                  COI11730
 2853    FORMAT ('0Give the external field in Gausses (BEXT = 0.000 G).'COI11740
     1            / ' The field must be parallel to the axis of ',      COI11750
     2           'the shield. ')                                        COI11760
      ELSE                                                              COI11770
         WRITE(6,2854) BEXT                                             COI11780
 2854    FORMAT ('0Give the external field in Gausses (BEXT = ', G6.4,  COI11790
     1           ' G. ' / ' The field must be parallel to the axis of ',COI11800
     2           'the shield. ')                                        COI11810
      ENDIF                                                             COI11820
      REWIND 5                                                          COI11830
      READ(5, *, ERR=2852, END=2855) BEXT                               COI11840
      BEXT = BEXT/10000D0                                               COI11850
C                                                                       COI11860
 2855 CONTINUE                                                          COI11870
C                                                                       COI11880
      ISHI = 1                                                          COI11890
      HOLE = .TRUE.                                                     COI11900
 2856 CONTINUE                                                          COI11910
C           (* check whether there is a hole through the shield *)      COI11920
         HOLE = (HOLE .AND. ((LENSHI(ISHI) .NE. 0) .OR.                 COI11930
     1                      (RHOLE(ISHI) .NE. 0)))                      COI11940
         ISHI = ISHI + 1                                                COI11950
         IF ((HOLE) .AND. (ISHI .LE. NSHI)) GOTO 2856                   COI11960
      CONTINUE                                                          COI11970
C                                                                       COI11980
 2857 CONTINUE                                                          COI11990
      IF (HOLE) THEN                                                    COI12000
C           (* if there is a hole through the shield *)                 COI12010
         IF (TRAFLU .EQ. 0) THEN                                        COI12020
            WRITE(6,2858)                                               COI12030
 2858       FORMAT ('0Give the trapped flux in the shield in Gausses ', COI12040
     1             'times square centimeter.' /                         COI12050
     2             ' (FLUX = 0.000 Gauss*cm*cm).')                      COI12060
         ELSE                                                           COI12070
            WRITE(6,2859) TRAFLU*1D8                                    COI12080
 2859       FORMAT ('0Give the trapped flux in the shield in Gausses ', COI12090
     1             'times square centimeter.' /                         COI12100
     2             ' (FLUX = ', G8.4, ' Gauss*cm*cm).')                 COI12110
         ENDIF                                                          COI12120
         REWIND 5                                                       COI12130
         READ(5, *, ERR=2857, END=2860) TRAFLU                          COI12140
         TRAFLU = TRAFLU*1D-8                                           COI12150
 2860    CONTINUE                                                       COI12160
C                                                                       COI12170
      ELSE                                                              COI12180
C           (* if there is no hole through the shield *)                COI12190
         TRAFLU = 0D0                                                   COI12200
      ENDIF                                                             COI12210
C                                                                       COI12220
      CONTINUE                                                          COI12230
C                                                                       COI12240
      RETURN                                                            COI12250
      END                                                               COI12260
C                                                                       COI12270
C                                                                       COI12280
C     ****************************************************************  COI12290
C                                                                       COI12300
C                                                                       COI12310
      SUBROUTINE SHIPOT(NN, MAX, NSHI, LENSHI, RADSHI, RHOLE, CENSHI,   COI12320
     1                  SYMSHI, M)                                      COI12330
C                                                                       COI12340
C           (* Calculates   the  interaction   between  different *)    COI12350
C           (* current  rings  in the shield. The theta component *)    COI12360
C           (* of the vector potential in the Ith ring , produced *)    COI12370
C           (* by  unit  current  density  in  the  IIth ring, is *)    COI12380
C           (* stored as the component M(I, II) of the matrix M.  *)    COI12390
C           (*                                                    *)    COI12400
C           (* The meanings of the input parameters are:          *)    COI12410
C           (*     NN, MAX, NSHI, LENSHI, RADSHI, RHOLE, CENSHI : *)    COI12420
C           (*        as in SUBROUTINE RESULT                     *)    COI12430
C           (*     SYMSHI(K) : .TRUE.  if there is a similar part *)    COI12440
C           (*                 centered  to z=-CENSHI(K); .FALSE. *)    COI12450
C           (*                 otherwise                          *)    COI12460
C           (*     M : the  NNxNN  matrix  containing  the result *)    COI12470
C           (*         (output)                                   *)    COI12480
C                                                                       COI12490
      INTEGER NN, MAX, NSHI                                             COI12500
      DOUBLE PRECISION LENSHI(MAX), RADSHI(MAX), RHOLE(MAX),            COI12510
     1                 CENSHI(MAX), M(NN,NN)                            COI12520
      LOGICAL SYMSHI(MAX)                                               COI12530
C                                                                       COI12540
      INTEGER N, IND1, IND2, J, JJ, III, JJJ                            COI12550
      DOUBLE PRECISION ZFIEL1, ZFIEL2, ZSOUR1, ZSOUR2, DISTZ1, DISTZ2,  COI12560
     1                 RFIELD, RSOURC, DSKPOT, CYLPOT, RES1, RES2, RES3 COI12570
C           (* N : the  number  of  current  rings in each of the *)    COI12580
C           (*     shield parts                                   *)    COI12590
C           (* IND1, IND2 : indices  for shield parts ; IND1 used *)    COI12600
C           (*              normally  for  field  part,  IND2 for *)    COI12610
C           (*              source part                           *)    COI12620
C           (* J, JJ : used in current ring indices               *)    COI12630
C           (* III, JJJ : used  to  index current rings belonging *)    COI12640
C           (*            to the same cylindrical shield part     *)    COI12650
C           (* ZFIEL1 : z-coordinate  of  the  field  ring  if it *)    COI12660
C           (*          belongs to a cylindrical shield part      *)    COI12670
C           (* ZFIEL2 : z-coordinate of the corresponding ring in *)    COI12680
C           (*          the  symmetrically located similar shield *)    COI12690
C           (*          part, if there is one                     *)    COI12700
C           (* ZSOUR1, ZSOUR2 : as  ZFIEL1  and  ZFIEL2,  but for *)    COI12710
C           (*                  source current rings              *)    COI12720
C           (* DISTZ1 : distance between IND1th and IND2th planar *)    COI12730
C           (*          shield parts                              *)    COI12740
C           (* DISTZ2 : distance  from  IND1th planar shield part *)    COI12750
C           (*          to  the  symmetrically located partner of *)    COI12760
C           (*          IND2th planar shield part, or vice versa  *)    COI12770
C           (* RFIELD : the  radius  of  the  field  ring  if  it *)    COI12780
C           (*          belongs to a planar shield part           *)    COI12790
C           (* RSOURC : the  radius  of  the  source  ring  if it *)    COI12800
C           (*          belongs to a planar shield part           *)    COI12810
C           (* DSKPOT : function  subprogram  for calculating the *)    COI12820
C           (*          vector  potential  produced  by  a planar *)    COI12830
C           (*          current ring                              *)    COI12840
C           (* CYLPOT : function  subprogram  for calculating the *)    COI12850
C           (*          vector  potential  producedt by a current *)    COI12860
C           (*          sheet of cylindrical shape                *)    COI12870
C           (* RES1, RES2, RES3 : used for storing the results of *)    COI12880
C           (*                    calculations   of  interactions *)    COI12890
C           (*                    between   similar   cylindrical *)    COI12900
C           (*                    current rings                   *)    COI12910
C                  ...............................                      COI12920
C                                                                       COI12930
      N = INT(NN/NSHI)                                                  COI12940
C                                                                       COI12950
      IND1 = 1                                                          COI12960
 2905 CONTINUE                                                          COI12970
C                                                                       COI12980
         IND2 = 1                                                       COI12990
 2910    CONTINUE                                                       COI13000
C                                                                       COI13010
            IF (LENSHI(IND1) .EQ. 0) THEN                               COI13020
C                 (* if the field part is a disk *)                     COI13030
C                                                                       COI13040
               IF (LENSHI(IND2) .EQ. 0) THEN                            COI13050
C                    (* if the source is a disk *)                      COI13060
C                                                                       COI13070
                  DISTZ1 = CENSHI(IND1) - CENSHI(IND2)                  COI13080
                  IF ((SYMSHI(IND1)) .OR. (SYMSHI(IND2)))               COI13090
     1               DISTZ2 = CENSHI(IND1) + CENSHI(IND2)               COI13100
C                                                                       COI13110
                  DO 2920 J = 1, N                                      COI13120
C                       (* each ring of the field part *)               COI13130
                     RFIELD = RHOLE(IND1) +                             COI13140
     1                        (J-0.5)*(RADSHI(IND1)-RHOLE(IND1))/N      COI13150
C                                                                       COI13160
                     DO 2915 JJ = 1, N                                  COI13170
C                          (* each ring of the source part *)           COI13180
                        RSOURC = RHOLE(IND2) +                          COI13190
     1                          (JJ-0.5)*(RADSHI(IND2)-RHOLE(IND2))/N   COI13200
                        M((IND1-1)*N+J, (IND2-1)*N+JJ) =                COI13210
     1                     DSKPOT((RADSHI(IND2)-RHOLE(IND2))/N,         COI13220
     2                            RSOURC, RFIELD, DISTZ1)               COI13230
                                                                        COI13240
                        IF (SYMSHI(IND1)) THEN                          COI13250
C                             (* if the field part has a similar *)     COI13260
C                             (* partner located symmetrically   *)     COI13270
                           M(IND1*N+J, (IND2-1)*N+JJ) =                 COI13280
     1                        DSKPOT((RADSHI(IND2)-RHOLE(IND2))/N,      COI13290
     2                               RSOURC, RFIELD, -DISTZ2)           COI13300
C                                (* the index of the symmetrically *)   COI13310
C                                (* located field part is IND1+1   *)   COI13320
C                                                                       COI13330
                           IF (SYMSHI(IND2)) THEN                       COI13340
C                                (* if both shield parts have similar *)COI13350
C                                (* partners located symmetrically    *)COI13360
                              M(IND1*N+J, IND2*N+JJ) =                  COI13370
     1                           M((IND1-1)*N+J, (IND2-1)*N+JJ)         COI13380
                              M((IND1-1)*N+J, IND2*N+JJ) =              COI13390
     1                           M(IND1*N+J, (IND2-1)*N+JJ)             COI13400
C                                   (* the index of the symmetrically *)COI13410
C                                   (* located source part is IND2+1  *)COI13420
                           ENDIF                                        COI13430
C                                                                       COI13440
                        ELSE IF (SYMSHI(IND2)) THEN                     COI13450
C                             (* if only the source part has a simi- *) COI13460
C                             (* lar partner located symmetrically   *) COI13470
                           M((IND1-1)*N+J, IND2*N+JJ) =                 COI13480
     1                        DSKPOT((RADSHI(IND2)-RHOLE(IND2))/N,      COI13490
     2                               RSOURC, RFIELD, DISTZ2)            COI13500
                        ENDIF                                           COI13510
 2915                CONTINUE                                           COI13520
 2920             CONTINUE                                              COI13530
C                                                                       COI13540
               ELSE                                                     COI13550
C                    (* if the field part is a disk but the source *)   COI13560
C                    (* part a cylinder                            *)   COI13570
C                                                                       COI13580
                  DO 2930 J = 1, N                                      COI13590
C                       (* each ring of the field part *)               COI13600
                     RFIELD = RHOLE(IND1) +                             COI13610
     1                        (J-0.5)*(RADSHI(IND1)-RHOLE(IND1))/N      COI13620
C                                                                       COI13630
                     DO 2925 JJ = 1, N                                  COI13640
C                          (* each ring of the source part *)           COI13650
                        ZSOUR1 = CENSHI(IND2) + LENSHI(IND2)/2D0 -      COI13660
     1                           (JJ-0.5)*LENSHI(IND2)/N                COI13670
                        IF (SYMSHI(IND2))                               COI13680
     1                     ZSOUR2 = -CENSHI(IND2) + LENSHI(IND2)/2D0 -  COI13690
     2                              (JJ-0.5)*LENSHI(IND2)/N             COI13700
                        M((IND1-1)*N+J, (IND2-1)*N+JJ) =                COI13710
     1                     CYLPOT(LENSHI(IND2)/N, RADSHI(IND2), RFIELD, COI13720
     2                            CENSHI(IND1)-ZSOUR1)                  COI13730
C                                                                       COI13740
                        IF (SYMSHI(IND1)) THEN                          COI13750
C                             (* if the field part has a similar *)     COI13760
C                             (* partner located symmetrically   *)     COI13770
                           M(IND1*N+J, (IND2-1)*N+JJ) =                 COI13780
     1                        CYLPOT(LENSHI(IND2)/N, RADSHI(IND2),      COI13790
     2                           RFIELD, -CENSHI(IND1)-ZSOUR1)          COI13800
C                                                                       COI13810
                           IF (SYMSHI(IND2)) THEN                       COI13820
C                                (* if both shield parts have similar *)COI13830
C                                (* partners located symmetrically    *)COI13840
                              M(IND1*N+J, (IND2+1)*N-JJ+1) =            COI13850
     1                           M((IND1-1)*N+J, (IND2-1)*N+JJ)         COI13860
                              M((IND1-1)*N+J, (IND2+1)*N-JJ+1) =        COI13870
     1                           M(IND1*N+J, (IND2-1)*N+JJ)             COI13880
                           ENDIF                                        COI13890
C                                                                       COI13900
                        ELSE IF (SYMSHI(IND2)) THEN                     COI13910
C                             (* if only the source part has a simi- *) COI13920
C                             (* lar partner located symmetrically   *) COI13930
                           M((IND1-1)*N+J, IND2*N+JJ) =                 COI13940
     1                        CYLPOT(LENSHI(IND2)/N, RADSHI(IND2),      COI13950
     2                           RFIELD, CENSHI(IND1)-ZSOUR2)           COI13960
                        ENDIF                                           COI13970
 2925                CONTINUE                                           COI13980
 2930             CONTINUE                                              COI13990
               ENDIF                                                    COI14000
C                                                                       COI14010
            ELSE                                                        COI14020
C                 (* if the field part is a cylinder *)                 COI14030
C                                                                       COI14040
               IF (LENSHI(IND2) .EQ. 0) THEN                            COI14050
C                    (* if the source is a disk *)                      COI14060
C                                                                       COI14070
                  DO 2940 J = 1, N                                      COI14080
C                       (* each ring of the field part *)               COI14090
                     ZFIEL1 = CENSHI(IND1) + LENSHI(IND1)/2D0 -         COI14100
     1                        (J-0.5)*LENSHI(IND1)/N                    COI14110
                     IF (SYMSHI(IND1))                                  COI14120
     1                  ZFIEL2 = -CENSHI(IND1) + LENSHI(IND1)/2D0 -     COI14130
     2                           (J-0.5)*LENSHI(IND1)/N                 COI14140
C                                                                       COI14150
                     DO 2935 JJ = 1, N                                  COI14160
C                          (* each ring of the source part *)           COI14170
                        RSOURC = RHOLE(IND2) +                          COI14180
     1                          (JJ-0.5)*(RADSHI(IND2)-RHOLE(IND2))/N   COI14190
                        M((IND1-1)*N+J, (IND2-1)*N+JJ) =                COI14200
     1                     DSKPOT((RADSHI(IND2)-RHOLE(IND2))/N, RSOURC, COI14210
     2                            RADSHI(IND1), ZFIEL1-CENSHI(IND2))    COI14220
C                                                                       COI14230
                        IF (SYMSHI(IND1)) THEN                          COI14240
C                             (* if the field part has a similar *)     COI14250
C                             (* partner located symmetrically   *)     COI14260
                           M(IND1*N+J, (IND2-1)*N+JJ) =                 COI14270
     1                        DSKPOT((RADSHI(IND2)-RHOLE(IND2))/N,      COI14280
     2                               RSOURC, RADSHI(IND1),              COI14290
     3                               ZFIEL2-CENSHI(IND2))               COI14300
C                                                                       COI14310
                           IF (SYMSHI(IND2)) THEN                       COI14320
C                                (* if both shield parts have similar *)COI14330
C                                (* partners located symmetrically    *)COI14340
                              M((IND1+1)*N-J+1, IND2*N+JJ) =            COI14350
     1                           M((IND1-1)*N+J, (IND2-1)*N+JJ)         COI14360
                              M(IND1*N-J+1, IND2*N+JJ) =                COI14370
     1                           M(IND1*N+J, (IND2-1)*N+JJ)             COI14380
                           ENDIF                                        COI14390
C                                                                       COI14400
                        ELSE IF (SYMSHI(IND2)) THEN                     COI14410
C                             (* if only the source part has a simi- *) COI14420
C                             (* lar partner located symmetrically   *) COI14430
                           M((IND1-1)*N+J, IND2*N+JJ) =                 COI14440
     1                        DSKPOT((RADSHI(IND2)-RHOLE(IND2))/N,      COI14450
     2                               RSOURC, RADSHI(IND1),              COI14460
     3                               ZFIEL1+CENSHI(IND2))               COI14470
                        ENDIF                                           COI14480
 2935                CONTINUE                                           COI14490
 2940             CONTINUE                                              COI14500
C                                                                       COI14510
               ELSE                                                     COI14520
C                    (* if both the source and the field part are *)    COI14530
C                    (* cylinders                                 *)    COI14540
C                                                                       COI14550
                  IF (IND1 .EQ. IND2) THEN                              COI14560
C                       (* if  the  source and field cylinders are *)   COI14570
C                       (* the  same, or if they are symmetrically *)   COI14580
C                       (* located similar partners of each other, *)   COI14590
C                       (* the  symmetry of the system can be used *)   COI14600
C                       (* to make the amount of calculations much *)   COI14610
C                       (* fewer:  the interaction between current *)   COI14620
C                       (* rings  depends  only  on their distance *)   COI14630
C                       (* from each other                         *)   COI14640
                     DO 2950 J = 1, N                                   COI14650
                        III = 1                                         COI14660
                        JJJ = J                                         COI14670
                        RES1 = CYLPOT(LENSHI(IND1)/N, RADSHI(IND1),     COI14680
     1                              RADSHI(IND1), (J-1)*LENSHI(IND1)/N) COI14690
                        IF (SYMSHI(IND1)) THEN                          COI14700
                           RES2 = CYLPOT(LENSHI(IND1)/N, RADSHI(IND1),  COI14710
     1                                  RADSHI(IND1), 2*CENSHI(IND1)+   COI14720
     2                                  (J-1)*LENSHI(IND1)/N)           COI14730
                           RES3 = CYLPOT(LENSHI(IND1)/N, RADSHI(IND1),  COI14740
     1                                  RADSHI(IND1), 2*CENSHI(IND1)-   COI14750
     2                                  (J-1)*LENSHI(IND1)/N)           COI14760
                        ENDIF                                           COI14770
C                             (* RES1: interactions within one part; *) COI14780
C                             (* RES2 and RES3: interactions between *) COI14790
C                             (* a  cylinder  and  its symmetrically *) COI14800
C                             (* located similar partner             *) COI14810
C                                                                       COI14820
 2945                   CONTINUE                                        COI14830
                           M((IND1-1)*N+III, (IND1-1)*N+JJJ) = RES1     COI14840
                           M((IND1-1)*N+JJJ, (IND1-1)*N+III) = RES1     COI14850
                           IF (SYMSHI(IND1)) THEN                       COI14860
                              M(IND1*N+III, IND1*N+JJJ) = RES1          COI14870
                              M(IND1*N+JJJ, IND1*N+III) = RES1          COI14880
                              M((IND1-1)*N+III, IND1*N+JJJ) = RES2      COI14890
                              M(IND1*N+JJJ, (IND1-1)*N+III) = RES2      COI14900
                              M((IND1-1)*N+JJJ, IND1*N+III) = RES3      COI14910
                              M(IND1*N+III, (IND1-1)*N+JJJ) = RES3      COI14920
                           ENDIF                                        COI14930
C                                                                       COI14940
                           III = III + 1                                COI14950
                           JJJ = JJJ + 1                                COI14960
                           IF (JJJ .LE. N) GOTO 2945                    COI14970
C                                 (* if  all possible distances *)      COI14980
C                                 (* between current rings have *)      COI14990
C                                 (* not yet been gone through  *)      COI15000
                        CONTINUE                                        COI15010
 2950                CONTINUE                                           COI15020
                  ELSE                                                  COI15030
C                       (* if the source and field parts are *)         COI15040
C                       (* different solenoids               *)         COI15050
C                                                                       COI15060
                     DO 2960 J = 1, N                                   COI15070
C                          (* each ring of the field part *)            COI15080
                        ZFIEL1 = CENSHI(IND1) + LENSHI(IND1)/2D0 -      COI15090
     1                           (J-0.5)*LENSHI(IND1)/N                 COI15100
                        IF (SYMSHI(IND1))                               COI15110
     1                     ZFIEL2 = -CENSHI(IND1) + LENSHI(IND1)/2D0 -  COI15120
     2                              (J-0.5)*LENSHI(IND1)/N              COI15130
C                                                                       COI15140
                        DO 2955 JJ = 1, N                               COI15150
C                             (* each ring of the source part *)        COI15160
                           ZSOUR1 = CENSHI(IND2) + LENSHI(IND2)/2D0 -   COI15170
     1                              (JJ-0.5)*LENSHI(IND2)/N             COI15180
                           IF (SYMSHI(IND2))                            COI15190
     1                        ZSOUR2 = -CENSHI(IND2) + LENSHI(IND2)/2D0 COI15200
     2                                 - (JJ-0.5)*LENSHI(IND2)/N        COI15210
                           M((IND1-1)*N+J, (IND2-1)*N+JJ) =             COI15220
     1                        CYLPOT(LENSHI(IND2)/N, RADSHI(IND2),      COI15230
     2                           RADSHI(IND1), ZFIEL1-ZSOUR1)           COI15240
C                                                                       COI15250
                           IF (SYMSHI(IND1)) THEN                       COI15260
C                                (* if the field part has a similar *)  COI15270
C                                (* partner located symmetrically   *)  COI15280
                              M(IND1*N+J, (IND2-1)*N+JJ) =              COI15290
     1                           CYLPOT(LENSHI(IND2)/N, RADSHI(IND2),   COI15300
     2                                  RADSHI(IND1), ZFIEL2-ZSOUR1)    COI15310
                              IF (SYMSHI(IND2)) THEN                    COI15320
C                                   (* if both shield parts have *)     COI15330
C                                   (* similar  partners located *)     COI15340
C                                   (* symmetrically             *)     COI15350
                                 M((IND1+1)*N-J+1, (IND2+1)*N-JJ+1) =   COI15360
     1                              M((IND1-1)*N+J, (IND2-1)*N+JJ)      COI15370
                                 M(IND1*N-J+1, (IND2+1)*N-JJ+1) =       COI15380
     1                              M(IND1*N+J, (IND2-1)*N+JJ)          COI15390
                              ENDIF                                     COI15400
C                                                                       COI15410
                           ELSE IF (SYMSHI(IND2)) THEN                  COI15420
C                                (* if only the source part has *)      COI15430
C                                (* a  similar  partner located *)      COI15440
C                                (* symmetrically               *)      COI15450
                              M((IND1-1)*N+J, IND2*N+JJ) =              COI15460
     1                           CYLPOT(LENSHI(IND2)/N, RADSHI(IND2),   COI15470
     2                                  RADSHI(IND1), ZFIEL1-ZSOUR2)    COI15480
                           ENDIF                                        COI15490
 2955                   CONTINUE                                        COI15500
 2960                CONTINUE                                           COI15510
                  ENDIF                                                 COI15520
               ENDIF                                                    COI15530
            ENDIF                                                       COI15540
C                                                                       COI15550
            IF (SYMSHI(IND2)) THEN                                      COI15560
               IND2 = IND2 + 2                                          COI15570
C                 (* the effect of the symmetrically located similar *) COI15580
C                 (* source part, whose index is IND2+1, has already *) COI15590
C                 (* been calculated*)                                  COI15600
            ELSE                                                        COI15610
               IND2 = IND2 + 1                                          COI15620
            ENDIF                                                       COI15630
C                                                                       COI15640
            IF (IND2 .LE. NSHI) GOTO 2910                               COI15650
C                 (* if there are more source parts *)                  COI15660
         CONTINUE                                                       COI15670
C                                                                       COI15680
         IF (SYMSHI(IND1)) THEN                                         COI15690
            IND1 = IND1 + 2                                             COI15700
         ELSE                                                           COI15710
            IND1 = IND1 + 1                                             COI15720
         ENDIF                                                          COI15730
C                                                                       COI15740
         IF (IND1 .LE. NSHI) GOTO 2905                                  COI15750
C              (* if there are more field parts *)                      COI15760
      CONTINUE                                                          COI15770
C                                                                       COI15780
      RETURN                                                            COI15790
      END                                                               COI15800
C                                                                       COI15810
C                                                                       COI15820
C     ****************************************************************  COI15830
C                                                                       COI15840
C                                                                       COI15850
      SUBROUTINE SOLPOT(NN, MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE,   COI15860
     1                  SYMSHI, LENSOL, RADSOL, CENSOL, LAYERS, LOOPS,  COI15870
     2                  SYMSOL, CUR, WIRDIA, FOILTH, ASTOR, A)          COI15880
C                                                                       COI15890
C           (* Calculates  the  theta  component  of  the  vector *)    COI15900
C           (* potential  produced  by  the  solenoid  coils. The *)    COI15910
C           (* potential  in  the  Jth ring of the shield will be *)    COI15920
C           (* stored as the Jth component of the vector A.       *)    COI15930
C           (*                                                    *)    COI15940
C           (* The meanings of the input parameters are:          *)    COI15950
C           (*     NN,  MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE, *)    COI15960
C           (*        LENSOL,   RADSOL,  CENSOL,  LAYERS,  LOOPS, *)    COI15970
C           (*        SYMSOL, CUR, WIRDIA, FOILTH :               *)    COI15980
C           (*           as in SUBROUTINE RESULT                  *)    COI15990
C           (*     SYMSHI(I) : .TRUE.  if there is a similar part *)    COI16000
C           (*                 centered  to z=-CENSHI(I); .FALSE. *)    COI16010
C           (*                 otherwise                          *)    COI16020
C           (*     ASTOR(NN) : a  "storage  vector"  used for the *)    COI16030
C           (*                 results  of subroutines SOLDSK and *)    COI16040
C           (*                 SOLCYL                             *)    COI16050
C           (*     A(NN) : the result of this subroutine (output) *)    COI16060
C                                                                       COI16070
      INTEGER NN, MAX, NSHI, LAYERS(MAX)                                COI16080
      DOUBLE PRECISION LENSHI(MAX), RADSHI(MAX), CENSHI(MAX),           COI16090
     1                RHOLE(MAX), LENSOL(MAX), RADSOL(MAX), CENSOL(MAX),COI16100
     2                LOOPS(MAX), CUR(MAX), WIRDIA, FOILTH,             COI16110
     3                ASTOR(NN), A(NN)                                  COI16120
      LOGICAL SYMSHI(MAX), SYMSOL(MAX)                                  COI16130
C                                                                       COI16140
      INTEGER ISOL, ISHI, N, J                                          COI16150
C           (* ISOL : the index of the solenoid                   *)    COI16160
C           (* ISHI : the index of the shield part                *)    COI16170
C           (* N : the  number  of current rings used to approxi- *)    COI16180
C           (*     mate each of the shield parts                  *)    COI16190
C           (* J : used to index the current rings in the shield  *)    COI16200
C                  ...............................                      COI16210
C                                                                       COI16220
      N = INT(NN/NSHI)                                                  COI16230
      ISOL = 1                                                          COI16240
C                                                                       COI16250
 3010 CONTINUE                                                          COI16260
C           (* calculate the vector potentials produced by each coil *) COI16270
         ISHI = 1                                                       COI16280
                                                                        COI16290
 3020    CONTINUE                                                       COI16300
C              (* calculate the vector potential produced by ISOLth *)  COI16310
C              (* solenoid on the current rings forming each of the *)  COI16320
C              (* shield parts                                      *)  COI16330
            CALL ZERO(ASTOR, 1, NN)                                     COI16340
C                 (* makes every component of array ASTOR(NN) zero *)   COI16350
C                                                                       COI16360
            IF (LENSHI(ISHI) .EQ. 0) THEN                               COI16370
C                 (* if the shield part in the field region is a disk *)COI16380
               CALL SOLDSK(CENSHI(ISHI), RADSHI(ISHI), RHOLE(ISHI),     COI16390
     1                     LENSOL(ISOL), RADSOL(ISOL), CENSOL(ISOL),    COI16400
     2                     LAYERS(ISOL), LOOPS(ISOL), CUR(ISOL), WIRDIA,COI16410
     3                     FOILTH, N, ASTOR, NN)                        COI16420
C                                (* the vector potential distribution *)COI16430
C                                (* on  the  shield part is stored as *)COI16440
C                                (* the  first  N components of array *)COI16450
C                                (* ASTOR(NN)                         *)COI16460
               IF (SYMSOL(ISOL))                                        COI16470
     1            CALL SOLDSK(CENSHI(ISHI), RADSHI(ISHI), RHOLE(ISHI),  COI16480
     2                     LENSOL(ISOL), RADSOL(ISOL), -CENSOL(ISOL),   COI16490
     3                     LAYERS(ISOL), LOOPS(ISOL), CUR(ISOL), WIRDIA,COI16500
     4                     FOILTH, N, ASTOR, NN)                        COI16510
C                     (* adds the effect of symmetrically located *)    COI16520
C                     (* similar solenoid                         *)    COI16530
C                                                                       COI16540
               DO 3030 J = 1, N                                         COI16550
C                    (* each ring of the shield disk *)                 COI16560
                  A((ISHI-1)*N+J) = A((ISHI-1)*N+J) + ASTOR(J)          COI16570
                  IF (SYMSOL(ISOL) .AND. SYMSHI(ISHI))                  COI16580
     2                A(ISHI*N+J) = A(ISHI*N+J) + ASTOR(J)              COI16590
 3030          CONTINUE                                                 COI16600
C                                                                       COI16610
               IF ((.NOT. SYMSOL(ISOL)) .AND. SYMSHI(ISHI)) THEN        COI16620
C                    (* if only the shield part has a similar partner *)COI16630
C                    (* located symmetrically, at -CENSHI(ISHI)       *)COI16640
                  CALL ZERO(ASTOR, 1, NN)                               COI16650
                  CALL SOLDSK(-CENSHI(ISHI), RADSHI(ISHI), RHOLE(ISHI), COI16660
     1                     LENSOL(ISOL), RADSOL(ISOL), CENSOL(ISOL),    COI16670
     2                     LAYERS(ISOL), LOOPS(ISOL), CUR(ISOL), WIRDIA,COI16680
     3                     FOILTH, N, ASTOR, NN)                        COI16690
                  DO 3040 J = 1, N                                      COI16700
                     A(ISHI*N+J) = A(ISHI*N+J) + ASTOR(J)               COI16710
 3040              CONTINUE                                             COI16720
               ENDIF                                                    COI16730
C                                                                       COI16740
            ELSE                                                        COI16750
C                 (* if the shield part in the field region is *)       COI16760
C                 (* a cylinder                                *)       COI16770
               CALL SOLCYL(LENSHI(ISHI), RADSHI(ISHI), CENSHI(ISHI),    COI16780
     1                     LENSOL(ISOL), RADSOL(ISOL), CENSOL(ISOL),    COI16790
     2                     LAYERS(ISOL), LOOPS(ISOL), CUR(ISOL), WIRDIA,COI16800
     3                     FOILTH, N, ASTOR, NN)                        COI16810
               IF (SYMSOL(ISOL) .AND. (CENSHI(ISHI) .NE. 0))            COI16820
C                    (* if there is a similar solenoid located *)       COI16830
C                    (* symmetrically,  and  the center of the *)       COI16840
C                    (* shield part is not in origo            *)       COI16850
     1            CALL SOLCYL(LENSHI(ISHI), RADSHI(ISHI), CENSHI(ISHI), COI16860
     2                     LENSOL(ISOL), RADSOL(ISOL), -CENSOL(ISOL),   COI16870
     3                     LAYERS(ISOL), LOOPS(ISOL), CUR(ISOL), WIRDIA,COI16880
     4                     FOILTH, N, ASTOR, NN)                        COI16890
C                                                                       COI16900
               DO 3050 J = 1, N                                         COI16910
C                    (* each ring of the shield cylinder *)             COI16920
                  A((ISHI-1)*N+J) = A((ISHI-1)*N+J) + ASTOR(J)          COI16930
                  IF (SYMSOL(ISOL) .AND. (CENSHI(ISHI) .EQ. 0))         COI16940
     1               A(ISHI*N-J+1) = A(ISHI*N-J+1) + ASTOR(J)           COI16950
                  IF (SYMSOL(ISOL) .AND. SYMSHI(ISHI))                  COI16960
     1               A((ISHI+1)*N-J+1) = A((ISHI+1)*N-J+1) + ASTOR(J)   COI16970
 3050          CONTINUE                                                 COI16980
C                                                                       COI16990
               IF ((.NOT. SYMSOL(ISOL)) .AND. SYMSHI(ISHI)) THEN        COI17000
C                    (* if only the shield part has a similar partner *)COI17010
C                    (* located symmetrically, at -CENSHI(ISHI)       *)COI17020
                  CALL ZERO(ASTOR, 1, NN)                               COI17030
                  CALL SOLCYL(LENSHI(ISHI), RADSHI(ISHI), -CENSHI(ISHI),COI17040
     1                     LENSOL(ISOL), RADSOL(ISOL), CENSOL(ISOL),    COI17050
     2                     LAYERS(ISOL), LOOPS(ISOL), CUR(ISOL), WIRDIA,COI17060
     3                     FOILTH, N, ASTOR, NN)                        COI17070
                  DO 3060 J = 1, N                                      COI17080
                     A(ISHI*N+J) = A(ISHI*N+J) + ASTOR(J)               COI17090
 3060             CONTINUE                                              COI17100
               ENDIF                                                    COI17110
            ENDIF                                                       COI17120
C                                                                       COI17130
            IF (SYMSHI(ISHI)) THEN                                      COI17140
               ISHI = ISHI + 2                                          COI17150
C                 (* the index of the symmetric shield part is ISHI+1 *)COI17160
            ELSE                                                        COI17170
               ISHI = ISHI + 1                                          COI17180
            ENDIF                                                       COI17190
C                                                                       COI17200
            IF (ISHI .LE. NSHI) GOTO 3020                               COI17210
C                 (* if there are more shield parts *)                  COI17220
C                                                                       COI17230
         CONTINUE                                                       COI17240
C                                                                       COI17250
         IF (.NOT. SYMSOL(ISOL)) THEN                                   COI17260
            ISOL = ISOL + 1                                             COI17270
         ELSE                                                           COI17280
            ISOL = ISOL + 2                                             COI17290
C                 (* the index of the symmetric solenoid is ISOL+1 *)   COI17300
         ENDIF                                                          COI17310
C                                                                       COI17320
         IF (LENSOL(ISOL) .GT. 0) GOTO 3010                             COI17330
C              (* if there are more solenoids *)                        COI17340
      CONTINUE                                                          COI17350
C                                                                       COI17360
      RETURN                                                            COI17370
      END                                                               COI17380
C                                                                       COI17390
C                                                                       COI17400
C     ****************************************************************  COI17410
C                                                                       COI17420
C                                                                       COI17430
      SUBROUTINE CURREN(NN, MAX, NSHI, DIFSHI, LENSHI, RADSHI, CENSHI,  COI17440
     1                 RHOLE, SYMSHI, LENSOL, RADSOL, CENSOL, LAYERS,   COI17450
     2                 LOOPS, SYMSOL, CUR, WIRDIA, FOILTH, BEXT, TRAFLU,COI17460
     3                 M, A, X)                                         COI17470
C                                                                       COI17480
C           (* This subroutine calculates the current distribution *)   COI17490
C           (* in  the  shield,  dividing each of the shield parts *)   COI17500
C           (* into  N  rings and using the fact that the magnetic *)   COI17510
C           (* flux  through  any  of these rings must be the same *)   COI17520
C           (* constant,  the flux trapped by the shield. This can *)   COI17530
C           (* also   be   expressed  in  cylindrical  coordinates *)   COI17540
C           (* demanding  the magnetic vector potential in each of *)   COI17550
C           (* the  rings  to  be  the trapped flux divided by the *)   COI17560
C           (* length  of  the  periphery of the ring. Because the *)   COI17570
C           (* vector  potential  is  a  function  not only of the *)   COI17580
C           (* currents  in  the  coils  but  also  of the current *)   COI17590
C           (* distribution  in the shield, itself, we arrive at a *)   COI17600
C           (* matrix   equation  MX + A = C,  where  the  unknown *)   COI17610
C           (* vector X contains the currents in the shield rings, *)   COI17620
C           (* vector  A  contains  the effect of the coils on the *)   COI17630
C           (* vector  potential  in the shield, matrix M contains *)   COI17640
C           (* the  effect  of  the  current  distribution  in the *)   COI17650
C           (* shield  itself, and vector C contains the effect of *)   COI17660
C           (* the trapped flux.                                   *)   COI17670
C           (*                                                     *)   COI17680
C           (* The meanings of the input parameters are:          *)    COI17690
C           (*     NN,  MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE, *)    COI17700
C           (*        LENSOL,   RADSOL,  CENSOL,  LAYERS,  LOOPS, *)    COI17710
C           (*        SYMSOL, CUR, WIRDIA, FOILTH, BEXT, TRAFLU : *)    COI17720
C           (*           as in SUBROUTINE RESULT                  *)    COI17730
C           (*     SYMSHI(I) : .TRUE.  if there is a similar part *)    COI17740
C           (*                 centered  to z=-CENSHI(I); .FALSE. *)    COI17750
C           (*                 otherwise                          *)    COI17760
C           (*     DIFSHI : .TRUE.  if the shield dimensions have *)    COI17770
C           (*              been changed ; .FALSE. otherwise      *)    COI17780
C           (*     M, A, X : see  above  ;  the  matrix  equation *)    COI17790
C           (*               MX + A = C  is  solved  here  in the *)    COI17800
C           (*               form MX = A', where A' = C - A       *)    COI17810
C                                                                       COI17820
      INTEGER NN, MAX, NSHI, LAYERS(MAX)                                COI17830
      DOUBLE PRECISION LENSHI(MAX), RADSHI(MAX), CENSHI(MAX),           COI17840
     1          RHOLE(MAX), LENSOL(MAX), RADSOL(MAX), CENSOL(MAX),      COI17850
     2          LOOPS(MAX), CUR(MAX), WIRDIA, FOILTH, BEXT, TRAFLU,     COI17860
     3          X(NN), M(NN,NN), A(NN)                                  COI17870
      LOGICAL DIFSHI, SYMSHI(MAX), SYMSOL(MAX)                          COI17880
C                                                                       COI17890
      INTEGER N, ISHI, I, N1                                            COI17900
      DOUBLE PRECISION DCONST, PI, RADIUS                               COI17910
C           (* N : the  number  of current rings used to approxi- *)    COI17920
C           (*     mate each of the shield parts                  *)    COI17930
C           (* ISHI : the index of the shield part                *)    COI17940
C           (* I : used to index the current rings in the shield  *)    COI17950
C           (* N1 : the  total  number  of  current rings used to *)    COI17960
C           (*      approximate the superconducting shield        *)    COI17970
C           (* DCONST : an IMSL-subroutine for values of scienti- *)    COI17980
C           (*          fic constants                             *)    COI17990
C           (* PI : 3.1415...                                     *)    COI18000
C           (* RADIUS : the  "average radius" of the current ring *)    COI18010
C           (*          in the planar part of the superconducting *)    COI18020
C           (*          shield                                    *)    COI18030
C                  ...............................                      COI18040
C                                                                       COI18050
      PI = DCONST('PI')                                                 COI18060
      N = INT(NN/NSHI)                                                  COI18070
      CALL ZERO(A, 1, NN)                                               COI18080
      CALL ZERO(X, 1, NN)                                               COI18090
C                                                                       COI18100
      IF (DIFSHI) THEN                                                  COI18110
         CALL ZERO(M, NN, NN)                                           COI18120
         CALL SHIPOT(NN, MAX, NSHI, LENSHI, RADSHI, RHOLE, CENSHI,      COI18130
     1               SYMSHI, M)                                         COI18140
C           (* if the shield has been changed, calculate the matrix M *)COI18150
      ENDIF                                                             COI18160
C                                                                       COI18170
      IF (LENSOL(1) .NE. 0)                                             COI18180
     1   CALL SOLPOT(NN, MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE,      COI18190
     2               SYMSHI, LENSOL, RADSOL, CENSOL, LAYERS, LOOPS,     COI18200
     3               SYMSOL, CUR, WIRDIA, FOILTH, X, A)                 COI18210
C              (* if there are solenoids, calculate the vector *)       COI18220
C              (* potential  produced  by  them  and store the *)       COI18230
C              (* result to the vector A ; vector X is used in *)       COI18240
C              (* SUBROUTINE SOLPOT only as work space         *)       COI18250
      CONTINUE                                                          COI18260
C                                                                       COI18270
      DO 3130 ISHI = 1, NSHI                                            COI18280
         IF (LENSHI(ISHI) .EQ. 0) THEN                                  COI18290
C              (* if planar shield part *)                              COI18300
            DO 3110 I = 1, N                                            COI18310
               RADIUS = RHOLE(ISHI) +                                   COI18320
     1                  (I-0.5)*(RADSHI(ISHI)-RHOLE(ISHI))/N            COI18330
C                    (* each current ring has different radius *)       COI18340
               A(N*(ISHI-1)+I) = TRAFLU/(2*PI*RADIUS) -                 COI18350
     1                           A(N*(ISHI-1)+I) - BEXT*RADIUS/2D0      COI18360
C                    (* A' = C - A *)                                   COI18370
 3110       CONTINUE                                                    COI18380
C                                                                       COI18390
         ELSE                                                           COI18400
C              (* if cylindrical shield part *)                         COI18410
            DO 3120 I = 1, N                                            COI18420
               A(N*(ISHI-1)+I) = TRAFLU/(2*PI*RADSHI(ISHI)) -           COI18430
     1                           A(N*(ISHI-1)+I) - BEXT*RADSHI(ISHI)/2D0COI18440
C                    (* A' = C - A *)                                   COI18450
 3120       CONTINUE                                                    COI18460
         ENDIF                                                          COI18470
 3130 CONTINUE                                                          COI18480
C                                                                       COI18490
      CALL ZERO (X, 1, NN)                                              COI18500
      N1 = NSHI*N                                                       COI18510
C           (* N1 is the total number of current rings *)               COI18520
C                                                                       COI18530
      CALL DLSARG(N1, M, NN, A, 1, X)                                   COI18540
C           (* solves the matrix equation MX = A ; an IMSL-subroutine *)COI18550
C                                                                       COI18560
      RETURN                                                            COI18570
      END                                                               COI18580
C                                                                       COI18590
C                                                                       COI18600
C     ****************************************************************  COI18610
C                                                                       COI18620
C                                                                       COI18630
      SUBROUTINE FLUX(Z, R, NN, MAX, NSHI, LENSHI, RADSHI, CENSHI,      COI18640
     1            RHOLE, CURSHI, CENSOL, LENSOL, RADSOL, LOOPS, LAYERS, COI18650
     2            WIRDIA, FOILTH, CUR, BEXT, MAGFLU)                    COI18660
C                                                                       COI18670
C           (* Calculates  the  vector  potential  in  the  point *)    COI18680
C           (* (R, Z).                                            *)    COI18690
C           (*                                                    *)    COI18700
C           (* The meanings of the input parameters are:          *)    COI18710
C           (*     Z, R : the coordinates of the field point      *)    COI18720
C           (*     NN,  MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE, *)    COI18730
C           (*        CURSHI,   CENSOL,  LENSOL,  RADSOL,  LOOPS, *)    COI18740
C           (*        LAYERS, WIRDIA, FOILTH, CUR, BEXT : *)            COI18750
C           (*           as in SUBROUTINE RESULT                  *)    COI18760
C           (*     MAGFLU : the magnetic flux through the ring of *)    COI18770
C           (*              radius R located at z = Z ; output    *)    COI18780
C                                                                       COI18790
      INTEGER NN, MAX, NSHI, LAYERS(MAX)                                COI18800
      DOUBLE PRECISION Z, R, LENSHI(MAX), RADSHI(MAX), CENSHI(MAX),     COI18810
     1                RHOLE(MAX), CURSHI(NN), CENSOL(MAX), LENSOL(MAX), COI18820
     2                RADSOL(MAX), LOOPS(MAX), WIRDIA, FOILTH,          COI18830
     3                CUR(MAX), BEXT, MAGFLU                            COI18840
C                                                                       COI18850
      INTEGER N, ISHI, INDSOL, ILAY, I                                  COI18860
      DOUBLE PRECISION DISTZ, RADIUS, RADLAY, CURDEN, DSKPOT, CYLPOT,   COI18870
     1                 DCONST, PI                                       COI18880
C           (* N : the  number  of current rings used to approxi- *)    COI18890
C           (*     mate each of the shield parts                  *)    COI18900
C           (* ISHI : the index of the shield part                *)    COI18910
C           (* INDSOL : the index of the solenoid                 *)    COI18920
C           (* ILAY : the index of the wire layer in the solenoid *)    COI18930
C           (* I : used to index the current rings in the shield  *)    COI18940
C           (* DISTZ : the  z-component  of  the  vector from the *)    COI18950
C           (*         field  point  to  the center of the source *)    COI18960
C           (*         current  ring  in  the  shield  or  to the *)    COI18970
C           (*         center of the solenoid                     *)    COI18980
C           (* RADIUS : the   "average  radius"   of  the  source *)    COI18990
C           (*          current  ring  in  the planar part of the *)    COI19000
C           (*          superconducting shield                    *)    COI19010
C           (* RADLAY : the "average radius" of the wire layer in *)    COI19020
C           (*          the solenoid                              *)    COI19030
C           (* CURDEN : the  current  density in the shield or in *)    COI19040
C           (*          the solenoid (A/m)                        *)    COI19050
C           (* DSKPOT : function   which  calculates  the  vector *)    COI19060
C           (*          potential of a planar current sheet       *)    COI19070
C           (* CYLPOT : function   which  calculates  the  vector *)    COI19080
C           (*          potential of a cylindrical current sheet  *)    COI19090
C           (* DCONST : an IMSL-subroutine for values of scienti- *)    COI19100
C           (*          fic constants                             *)    COI19110
C           (* PI : 3.1415...                                     *)    COI19120
C                  ...............................                      COI19130
C                                                                       COI19140
      PI = DCONST('PI')                                                 COI19150
C                                                                       COI19160
      MAGFLU = BEXT*PI*R*R                                              COI19170
C           (* the magnetic flux due to the external field *)           COI19180
C                                                                       COI19190
      IF (NSHI .NE. 0) N = INT(NN/NSHI)                                 COI19200
C                                                                       COI19210
      IF (NSHI .NE. 0) THEN                                             COI19220
C           (* if there is a superconducting shield *)                  COI19230
         DO 3230 ISHI = 1, NSHI                                         COI19240
C              (* calculate the flux produced by the shield *)          COI19250
            IF (LENSHI(ISHI) .GT. 0) THEN                               COI19260
C                 (* if the shield part is cylindrical *)               COI19270
               DO 3210 I = 1, N                                         COI19280
                  CURDEN = CURSHI(N*(ISHI-1)+I)                         COI19290
                  DISTZ = Z - CENSHI(ISHI) -                            COI19300
     1                    ((N+1)/2D0-I)*LENSHI(ISHI)/N                  COI19310
                  MAGFLU = MAGFLU + CYLPOT(LENSHI(ISHI)/N, RADSHI(ISHI),COI19320
     1                                     R, DISTZ) * CURDEN*2*PI*R    COI19330
C                       (* the effect of one cylindrical current ring *)COI19340
 3210          CONTINUE                                                 COI19350
C                                                                       COI19360
            ELSE                                                        COI19370
C                 (* if the shield part is planar *)                    COI19380
               DO 3220 I = 1, N                                         COI19390
                  CURDEN = CURSHI(N*(ISHI-1)+I)                         COI19400
                  RADIUS = RHOLE(ISHI) +                                COI19410
     1                     (I-0.5)*(RADSHI(ISHI)-RHOLE(ISHI))/N         COI19420
                  MAGFLU = MAGFLU + DSKPOT((RADSHI(ISHI)-RHOLE(ISHI))/N,COI19430
     1                        RADIUS, R, Z-CENSHI(ISHI)) * CURDEN*2*PI*RCOI19440
C                       (* the effect of one planar current ring *)     COI19450
 3220          CONTINUE                                                 COI19460
            ENDIF                                                       COI19470
 3230    CONTINUE                                                       COI19480
      ENDIF                                                             COI19490
C                                                                       COI19500
C                                                                       COI19510
      INDSOL = 1                                                        COI19520
C                                                                       COI19530
 3240 CONTINUE                                                          COI19540
C           (* calculate the magnetic flux due to the solenoids *)      COI19550
         IF (LENSOL(INDSOL) .EQ. 0) GOTO 3260                           COI19560
C              (* if no more solenoids *)                               COI19570
         RADLAY = RADSOL(INDSOL) + 0.5*WIRDIA                           COI19580
         CURDEN = LOOPS(INDSOL)*CUR(INDSOL)/LENSOL(INDSOL)              COI19590
C                                                                       COI19600
         DO 3250 ILAY = 1, LAYERS(INDSOL)                               COI19610
            DISTZ = Z - CENSOL(INDSOL)                                  COI19620
            MAGFLU = MAGFLU + CYLPOT(LENSOL(INDSOL), RADLAY, R, DISTZ)  COI19630
     1                        * CURDEN*2*PI*R                           COI19640
C                                                                       COI19650
            RADLAY = RADLAY + WIRDIA + FOILTH                           COI19660
 3250    CONTINUE                                                       COI19670
C                                                                       COI19680
         INDSOL = INDSOL+1                                              COI19690
         GOTO 3240                                                      COI19700
 3260 CONTINUE                                                          COI19710
C                                                                       COI19720
      RETURN                                                            COI19730
      END                                                               COI19740
C                                                                       COI19750
C                                                                       COI19760
C     ****************************************************************  COI19770
C                                                                       COI19780
C                                                                       COI19790
      SUBROUTINE FIELD(Z, R, NN, MAX, NSHI, LENSHI, RADSHI, CENSHI,     COI19800
     1            RHOLE, CURSHI, CENSOL, LENSOL, RADSOL, LOOPS, LAYERS, COI19810
     2            WIRDIA, FOILTH, CUR, BEXT, BZ, BR)                    COI19820
C                                                                       COI19830
C           (* Calculates the magnetic field in the point (R, Z). *)    COI19840
C           (*                                                    *)    COI19850
C           (* The meanings of the input parameters are:          *)    COI19860
C           (*     Z, R : the coordinates of the field point      *)    COI19870
C           (*     NN,  MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE, *)    COI19880
C           (*        CURSHI,   CENSOL,  LENSOL,  RADSOL,  LOOPS, *)    COI19890
C           (*        LAYERS, WIRDIA, FOILTH, CUR, BEXT :         *)    COI19900
C           (*           as in SUBROUTINE RESULT                  *)    COI19910
C           (*     BZ, BR : z-  and  r-components of the magnetic *)    COI19920
C           (*              field in the point (R, Z) ; output    *)    COI19930
C                                                                       COI19940
      INTEGER NN, MAX, NSHI, LAYERS(MAX)                                COI19950
      DOUBLE PRECISION Z, R, LENSHI(MAX), RADSHI(MAX), CENSHI(MAX),     COI19960
     1                RHOLE(MAX), CURSHI(NN), CENSOL(MAX), LENSOL(MAX), COI19970
     2                RADSOL(MAX), LOOPS(MAX), WIRDIA, FOILTH,          COI19980
     3                CUR(MAX), BEXT, BZ, BR                            COI19990
                                                                        COI20000
      LOGICAL ONDISK                                                    COI20010
C                                                                       COI20020
      INTEGER N, ISHI, ISOL, I                                          COI20030
      DOUBLE PRECISION BZSOL, BRSOL, BZSHI, BRSHI, DISTZ, RADIUS,       COI20040
     1                 WIDTH, SHIAXI, SHIBZ, SHIBR                      COI20050
C           (* ONDISK : .TRUE.  if  the  field point is on any of *)    COI20060
C           (*          the superconducting disks                 *)    COI20070
C           (* N : the  number  of current rings used to approxi- *)    COI20080
C           (*     mate each of the shield parts                  *)    COI20090
C           (* ISHI : the index of the shield part                *)    COI20100
C           (* ISOL : the index of the solenoid                   *)    COI20110
C           (* I : used to index the current rings in the shield  *)    COI20120
C           (* BZSOL, BRSOL : the  z-  and  r-components  of  the *)    COI20130
C           (*                field in the point (R, Z), produced *)    COI20140
C           (*                by the solenoids                    *)    COI20150
C           (* BZSHI, BRSHI : the  z-  and  r-components  of  the *)    COI20160
C           (*                field in the point (R, Z), produced *)    COI20170
C           (*                by the shield                       *)    COI20180
C           (* DISTZ : the  z-component  of  the  vector from the *)    COI20190
C           (*         field  point  to  the center of the source *)    COI20200
C           (*         current ring in the shield                 *)    COI20210
C           (* RADIUS : the   "average  radius"   of  the  source *)    COI20220
C           (*          current  ring  in  the planar part of the *)    COI20230
C           (*          superconducting shield                    *)    COI20240
C           (* SHIAXI, SHIBZ, SHIBR : names  of functions used in *)    COI20250
C           (*                        this subroutine             *)    COI20260
C                  ...............................                      COI20270
C                                                                       COI20280
      BZ = 0D0                                                          COI20290
      BR = 0D0                                                          COI20300
      BZSOL = 0D0                                                       COI20310
      BRSOL = 0D0                                                       COI20320
      BZSHI = 0D0                                                       COI20330
      BRSHI = 0D0                                                       COI20340
      ONDISK = .FALSE.                                                  COI20350
C                                                                       COI20360
      IF (NSHI .NE. 0) N = INT(NN/NSHI)                                 COI20370
C                                                                       COI20380
      IF (NSHI .NE. 0) THEN                                             COI20390
C           (* if there is a superconducting shield *)                  COI20400
         DO 3330 ISHI = 1, NSHI                                         COI20410
C              (* calculate the field produced by the shield *)         COI20420
            IF (LENSHI(ISHI) .GT. 0) THEN                               COI20430
C                 (* if the shield part is cylindrical *)               COI20440
               DO 3310 I = 1, N                                         COI20450
                  DISTZ = Z - CENSHI(ISHI) -                            COI20460
     1                    ((N+1)/2D0-I)*LENSHI(ISHI)/N                  COI20470
                  CURDEN = CURSHI(N*(ISHI-1)+I)                         COI20480
                  IF (R .LT. 1.0D-6) THEN                               COI20490
C                       (* if the field point is on central axis *)     COI20500
                     BZSHI = BZSHI + CYLBAX(DISTZ, RADSHI(ISHI), CURDEN,COI20510
     1                                      LENSHI(ISHI)/N)             COI20520
                  ELSE                                                  COI20530
C                       (* if the field point is not on central axis *) COI20540
                     BZSHI = BZSHI + CYLBZ(DISTZ, R, RADSHI(ISHI),      COI20550
     1                                     CURDEN, LENSHI(ISHI)/N)      COI20560
                     BRSHI = BRSHI + CYLBR(DISTZ, R, RADSHI(ISHI),      COI20570
     1                                     CURDEN, LENSHI(ISHI)/N)      COI20580
                  ENDIF                                                 COI20590
C                       (* the effect of one cylindrical current ring *)COI20600
 3310          CONTINUE                                                 COI20610
C                                                                       COI20620
            ELSE                                                        COI20630
C                 (* if the shield part is planar *)                    COI20640
               IF ((DABS(Z-CENSHI(ISHI)) .LE. 1D-10) .AND.              COI20650
     1            (R .GE. RHOLE(ISHI)) .AND. (R .LE. RADSHI(ISHI))) THENCOI20660
C                    (* FUNCTION DSKBZ cannot calculate the field on *) COI20670
C                    (* a source disk                                *) COI20680
                  ONDISK = .TRUE.                                       COI20690
                  BRSHI=0                                               COI20700
C                                                                       COI20710
               ELSE                                                     COI20720
C                    (* if the field point not on the shield disk *)    COI20730
                  DO 3320 I = 1, N                                      COI20740
                     WIDTH = (RADSHI(ISHI)-RHOLE(ISHI))/N               COI20750
                     RADIUS = RHOLE(ISHI) + (I-0.5)*WIDTH               COI20760
                     DISTZ = Z - CENSHI(ISHI)                           COI20770
                     CURDEN = CURSHI(N*(ISHI-1)+I)                      COI20780
C                                                                       COI20790
                     BZSHI = BZSHI + DSKBZ(DISTZ, R, RADIUS, CURDEN,    COI20800
     1                                     WIDTH)                       COI20810
                     BRSHI = BRSHI + DSKBR(DISTZ, R, RADIUS, CURDEN,    COI20820
     1                                     WIDTH)                       COI20830
 3320             CONTINUE                                              COI20840
C                       (* the effect of one planar current ring *)     COI20850
               ENDIF                                                    COI20860
            ENDIF                                                       COI20870
 3330    CONTINUE                                                       COI20880
      ENDIF                                                             COI20890
C                                                                       COI20900
C                                                                       COI20910
      INDSOL = 1                                                        COI20920
C                                                                       COI20930
 3340 CONTINUE                                                          COI20940
C           (* calculate the field produced by the solenoids *)         COI20950
         IF (LENSOL(INDSOL) .EQ. 0) GOTO 3350                           COI20960
C              (* if no more solenoids *)                               COI20970
         CALL BSOLEN(Z-CENSOL(INDSOL), R, LENSOL(INDSOL),               COI20980
     1               RADSOL(INDSOL), WIRDIA, LOOPS(INDSOL),             COI20990
     2               LAYERS(INDSOL), CUR(INDSOL), FOILTH, BZSOL, BRSOL) COI21000
C                                                                       COI21010
         INDSOL = INDSOL+1                                              COI21020
         GOTO 3340                                                      COI21030
 3350 CONTINUE                                                          COI21040
C                                                                       COI21050
      IF (ONDISK) THEN                                                  COI21060
         BZ = 0                                                         COI21070
      ELSE                                                              COI21080
         BZ = BZSHI + BZSOL + BEXT                                      COI21090
      ENDIF                                                             COI21100
C                                                                       COI21110
      BR = BRSHI + BRSOL                                                COI21120
C                                                                       COI21130
      RETURN                                                            COI21140
      END                                                               COI21150
C                                                                       COI21160
C                                                                       COI21170
C     ****************************************************************  COI21180
C                                                                       COI21190
C                                                                       COI21200
      SUBROUTINE DISTRI(NN, MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE,   COI21210
     1                  CURSHI, FILE)                                   COI21220
C                                                                       COI21230
C           (* Writes  the  current distribution in the shield to *)    COI21240
C           (* the display and the file number FILE.              *)    COI21250
C           (*                                                    *)    COI21260
C           (* The meanings of the input parameters are:          *)    COI21270
C           (*     NN,  MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE, *)    COI21280
C           (*        CURSHI : as in SUBROUTINE RESULT            *)    COI21290
C           (*     FILE : the   number  of  the  file  where  the *)    COI21300
C           (*            results are stored                      *)    COI21310
C                                                                       COI21320
      INTEGER NN, MAX, NSHI, FILE                                       COI21330
      DOUBLE PRECISION LENSHI(MAX), RADSHI(MAX), CENSHI(MAX),           COI21340
     1                 RHOLE(MAX), CURSHI(NN)                           COI21350
C                                                                       COI21360
      INTEGER N, ISHI, I, J                                             COI21370
      DOUBLE PRECISION Z, R                                             COI21380
C           (* N : the  number  of current rings used to approxi- *)    COI21390
C           (*     mate each of the shield parts                  *)    COI21400
C           (* ISHI : the index of the shield part                *)    COI21410
C           (* I : used to index the current rings in the shield  *)    COI21420
C           (* Z, R : the coordinates of the current ring         *)    COI21430
C                  ...............................                      COI21440
C                                                                       COI21450
      IF (NSHI .NE. 0) THEN                                             COI21460
         WRITE(6,*) ('  ')                                              COI21470
         WRITE(FILE,*) ('  ')                                           COI21480
C                                                                       COI21490
         WRITE(6,3410)                                                  COI21500
         WRITE(FILE,3410)                                               COI21510
 3410    FORMAT(5X, 'z (mm)', 4X, 'r (mm)', 8X, 'Current (A/mm)')       COI21520
C                                                                       COI21530
         WRITE(6,*) ('  ')                                              COI21540
         WRITE(FILE,*) ('  ')                                           COI21550
C                                                                       COI21560
         N = INT(NN/NSHI)                                               COI21570
C                                                                       COI21580
         DO 3460 ISHI = 1, NSHI                                         COI21590
C                                                                       COI21600
            IF (LENSHI(ISHI) .GT. 0) THEN                               COI21610
C                 (* if cylindrical shield part *)                      COI21620
               DO 3430 I = 1, N                                         COI21630
                  Z = CENSHI(ISHI) + ((N+1)/2D0-I)*LENSHI(ISHI)/N       COI21640
                  R = RADSHI(ISHI)                                      COI21650
                  WRITE(6,3420) Z*1000, R*1000,                         COI21660
     1                       CURSHI(N*(ISHI-1)+I)/1000                  COI21670
                  WRITE(FILE,3420) Z*1000, R*1000,                      COI21680
     1                       CURSHI(N*(ISHI-1)+I)/1000                  COI21690
 3420             FORMAT(2X, F8.2 , 2X, F8.2, 4X, F14.5)                COI21700
 3430          CONTINUE                                                 COI21710
C                                                                       COI21720
            ELSE                                                        COI21730
C                 (* if the shield part is a disk *)                    COI21740
               DO 3450 I = 1, N                                         COI21750
                  Z = CENSHI(ISHI)                                      COI21760
                  R = RHOLE(ISHI) + (I-0.5)*(RADSHI(ISHI)-RHOLE(ISHI))/NCOI21770
                  WRITE(6,3440) Z*1000, R*1000,                         COI21780
     1                          CURSHI(N*(ISHI-1)+I)/1000               COI21790
                  WRITE(FILE,3440) Z*1000, R*1000,                      COI21800
     1                             CURSHI(N*(ISHI-1)+I)/1000            COI21810
 3440             FORMAT(2X, F8.2 , 2X, F8.2, 4X, F14.5)                COI21820
 3450          CONTINUE                                                 COI21830
            ENDIF                                                       COI21840
 3460    CONTINUE                                                       COI21850
      ENDIF                                                             COI21860
C                                                                       COI21870
      RETURN                                                            COI21880
      END                                                               COI21890
C                                                                       COI21900
C                                                                       COI21910
C     ****************************************************************  COI21920
C                                                                       COI21930
C                                                                       COI21940
C                                                                       COI21950
      SUBROUTINE ASK(PROTYP, R, ZBEG, DELTAZ, ZEND)                     COI21960
C                                                                       COI21970
C           (* Asks where the magnetic flux or field profile must *)    COI21980
C           (* be calculated.                                     *)    COI21990
C           (*                                                    *)    COI22000
C           (* The meanings of the input parameters are:          *)    COI22010
C           (*     PROTYP : the type of the profile ;             *)    COI22020
C           (*                    1 = field profile,              *)    COI22030
C           (*                    2 = flux profile                *)    COI22040
C           (*     R : the radius of the profile                  *)    COI22050
C           (*     ZBEG : the z-coordinate of the first point     *)    COI22060
C           (*     DELTAZ : the   distance   between   successive *)    COI22070
C           (*              points                                *)    COI22080
C           (*     ZEND : the z-coordinate of the last point      *)    COI22090
C                                                                       COI22100
      INTEGER PROTYP                                                    COI22110
      DOUBLE PRECISION R, ZBEG, DELTAZ, ZEND                            COI22120
C                  ...............................                      COI22130
C                                                                       COI22140
      IF (PROTYP .EQ. 1) THEN                                           COI22150
C           (* if field profile *)                                      COI22160
         WRITE(6,3510)                                                  COI22170
 3510    FORMAT ('0Give the radius of the profile.')                    COI22180
C                                                                       COI22190
      ELSE                                                              COI22200
C           (* if flux profile *)                                       COI22210
         WRITE(6,3520)                                                  COI22220
 3520    FORMAT ('0Give the radius of the rings through which the',     COI22230
     1           ' flux must be calculated.')                           COI22240
      ENDIF                                                             COI22250
C                                                                       COI22260
      READ(5,*) R                                                       COI22270
      R = R/1000D0                                                      COI22280
C                                                                       COI22290
      WRITE(6, 3530)                                                    COI22300
 3530 FORMAT ('0Give ZBEG, DELTAZ and ZEND in mm:s.')                   COI22310
      READ(5,*) ZBEG, DELTAZ, ZEND                                      COI22320
      ZBEG = ZBEG/1000D0                                                COI22330
      DELTAZ = DELTAZ/1000D0                                            COI22340
      ZEND = ZEND/1000D0                                                COI22350
C                                                                       COI22360
      RETURN                                                            COI22370
      END                                                               COI22380
C                                                                       COI22390
C                                                                       COI22400
C     ****************************************************************  COI22410
C                                                                       COI22420
C                                                                       COI22430
      SUBROUTINE RESULT(NN, MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE,   COI22440
     1                  CURSHI, CENSOL, LENSOL, RADSOL, LOOPS, LAYERS,  COI22450
     2                  SYMSOL, WIRDIA, FOILTH, CUR, BEXT, TRAFLU)      COI22460
C                                                                       COI22470
C           (* This  SUBROUTINE  organizes the calculation of the *)    COI22480
C           (* magnetic  field  profile  and/or the flux profile, *)    COI22490
C           (* and the output of the results.                     *)    COI22500
C           (*                                                    *)    COI22510
C           (* The meanings of the input parameters are:          *)    COI22520
C           (*     NN : dimension of the vector CURSHI            *)    COI22530
C           (*     MAX : the  maximum  number  of  solenoids  and *)    COI22540
C           (*           shield parts                             *)    COI22550
C           (*     NSHI : the number of shield parts              *)    COI22560
C           (*     LENSHI(I) : the length of the Ith shield part  *)    COI22570
C           (*     RADSHI(I) : the radius of the Ith shield part; *)    COI22580
C           (*                 if  the  part is a disk, RADSHI(I) *)    COI22590
C           (*                 is its outer radius                *)    COI22600
C           (*     CENSHI(I) : the  z-coordinate of the center of *)    COI22610
C           (*                 the Ith shield part                *)    COI22620
C           (*     RHOLE(I) : the  radius  of  the  hole  in  the *)    COI22630
C           (*                center of the Ith part, if the part *)    COI22640
C           (*                happens to be a disk                *)    COI22650
C           (*     CURSHI : NN-dimensional  vector containing the *)    COI22660
C           (*              current distribution in the shield    *)    COI22670
C           (*     CENSOL(K) : the  z-coordinate of the center of *)    COI22680
C           (*                 the Kth solenoid                   *)    COI22690
C           (*     LENSOL(K) : the length of the Kth solenoid     *)    COI22700
C           (*     RADSOL(K) : the   inner   radius  of  the  Kth *)    COI22710
C           (*                 solenoid                           *)    COI22720
C           (*     LOOPS(K) : the  number of turns of wire in one *)    COI22730
C           (*                layer of the Kth solenoid           *)    COI22740
C           (*     LAYERS(K) : the  number  of  layers of wire in *)    COI22750
C           (*                 the Kth solenoid                   *)    COI22760
C           (*     SYMSOL(K) : .TRUE. if there is a similar sole- *)    COI22770
C           (*                 noid  centered  to z = -CENSOL(K); *)    COI22780
C           (*                 .FALSE. otherwise                  *)    COI22790
C           (*     WIRDIA : the diameter of the wire used to wind *)    COI22800
C           (*              the solenoids                         *)    COI22810
C           (*     FOILTH : the  thickness of the insulating foil *)    COI22820
C           (*              between the layers                    *)    COI22830
C           (*     CUR(K) : the  current  in  the wire of the Kth *)    COI22840
C           (*              solenoid                              *)    COI22850
C           (*     BEXT : the external magnetic field             *)    COI22860
C           (*     TRAFLU : magnetic flux trapped into the shield *)    COI22870
C                                                                       COI22880
      INTEGER NN, MAX, NSHI, LAYERS(MAX)                                COI22890
      DOUBLE PRECISION LENSHI(MAX), RADSHI(MAX), CENSHI(MAX),           COI22900
     1                 RHOLE(MAX), CURSHI(NN),CENSOL(MAX), LENSOL(MAX), COI22910
     2                 RADSOL(MAX), LOOPS(MAX),WIRDIA, FOILTH,          COI22920
     3                 CUR(MAX), BEXT, TRAFLU                           COI22930
      LOGICAL SYMSOL(MAX)                                               COI22940
C                                                                       COI22950
      INTEGER FILE                                                      COI22960
      DOUBLE PRECISION R, Z, ZBEG, DELTAZ, ZEND, BZ0, BR0, BZ, BR,      COI22970
     1                 HOMOG, MAGFLU, TOTAL                             COI22980
      CHARACTER*1 ANSW                                                  COI22990
C           (* FILE : the  number  of  the file where the results *)    COI23000
C           (*        are stored                                  *)    COI23010
C           (* R, Z : the r- and z-coordinates of the field point *)    COI23020
C           (* ZBEG, DELTAZ, ZEND : the  field  profile is calcu- *)    COI23030
C           (*        lated  in points with given r-coordinate R, *)    COI23040
C           (*        and  z-coordinate running from ZBEG to ZEND *)    COI23050
C           (*        with step DELTAZ                            *)    COI23060
C           (* BZ0, BR0 : the z- and r-components of the magnetic *)    COI23070
C           (*            field in origo                          *)    COI23080
C           (* BZ, BR : the  z-  and r-components of the magnetic *)    COI23090
C           (*          fields in the point (R, Z)                *)    COI23100
C           (* HOMOG : the homogenity of BZ in ppm:s ;            *)    COI23110
C           (*         HOMOG = 1000000*(BZ-BZ0)/BZ0               *)    COI23120
C           (* TOTAL : the  magnitude  of total magnetic field in *)    COI23130
C           (*         the point (R,Z)                            *)    COI23140
C           (* POTEN : the vector potential in the point (R, Z)   *)    COI23150
C           (* DCONST : an IMSL-subroutine for values of scienti- *)    COI23160
C           (*          fic constants                             *)    COI23170
C           (* PI : 3.1415...                                     *)    COI23180
C                  ...............................                      COI23190
C                                                                       COI23200
      PI = DCONST('PI')                                                 COI23210
C                                                                       COI23220
      CALL FIELD(0D0, 0D0, NN, MAX, NSHI, LENSHI, RADSHI, CENSHI,       COI23230
     1           RHOLE, CURSHI, CENSOL, LENSOL, RADSOL, LOOPS, LAYERS,  COI23240
     2           WIRDIA, FOILTH, CUR, BEXT, BZ0, BR0)                   COI23250
C           (* calculate the magnetic field in origo *)                 COI23260
C                                                                       COI23270
 3605 CONTINUE                                                          COI23280
         WRITE(6,3610)                                                  COI23290
 3610    FORMAT('0Give the number of the file in which the ',           COI23300
     1          'results will be stored (10-99)')                       COI23310
         REWIND 5                                                       COI23320
         READ(5,*) FILE                                                 COI23330
         IF ((FILE .LT. 10) .OR. (FILE .GT. 99)) GOTO 3605              COI23340
      CONTINUE                                                          COI23350
C                                                                       COI23360
 3615 CONTINUE                                                          COI23370
 3620    CONTINUE                                                       COI23380
            WRITE(6,3625)                                               COI23390
 3625       FORMAT('0Do you want to know a field profile ("F"), ',      COI23400
     1             'a magnetic flux' / ' profile ("X"), or the ',       COI23410
     2             'current distribution in the shield ("C")?')         COI23420
            REWIND 5                                                    COI23430
            READ(5, '(A1)', ERR=3620, END=3620) ANSW                    COI23440
            IF ((ANSW .NE. 'X') .AND. (ANSW .NE. 'F') .AND.             COI23450
     1          (ANSW .NE. 'C')) GOTO 3620                              COI23460
         CONTINUE                                                       COI23470
C                                                                       COI23480
         IF (ANSW .EQ. 'F') THEN                                        COI23490
C              (* field profile *)                                      COI23500
            CALL ASK(1, R, ZBEG, DELTAZ, ZEND)                          COI23510
C                 (* ask the profile coordinates *)                     COI23520
C                                                                       COI23530
            WRITE(6,*) ('  ')                                           COI23540
            WRITE(FILE,*) ('  ')                                        COI23550
            WRITE(6,3630) BZ0*(1.0D4)                                   COI23560
            WRITE(FILE,3630) BZ0*(1.0D4)                                COI23570
 3630       FORMAT (' Field value in the center of the shield is ',     COI23580
     1              G16.10, ' Gauss')                                   COI23590
C                                                                       COI23600
            WRITE(6,*) ('  ')                                           COI23610
            WRITE(FILE,*) ('  ')                                        COI23620
            WRITE(6,3635)                                               COI23630
            WRITE(FILE,3635)                                            COI23640
 3635       FORMAT(5X, 'z (mm)', 4X, 'r (mm)', 6X, 'Bz (G)', 9X,        COI23650
     1             'Br (G)', 7X, 'Btot(G)', 5X, 'Homog (ppm)')          COI23660
C                                                                       COI23670
            WRITE(FILE,*) ' '                                           COI23680
C                                                                       COI23690
            Z = ZBEG                                                    COI23700
C                                                                       COI23710
 3640        CONTINUE                                                   COI23720
C                 (* repeat field calculation until Z = ZEND *)         COI23730
               CALL FIELD(Z, R, NN, MAX, NSHI, LENSHI, RADSHI, CENSHI,  COI23740
     1                RHOLE, CURSHI, CENSOL, LENSOL, RADSOL, LOOPS,     COI23750
     2                LAYERS, WIRDIA, FOILTH, CUR, BEXT, BZ, BR)        COI23760
               IF (BZ0.NE.0) HOMOG = (1.0D6)*(BZ-BZ0)/BZ0               COI23770
               TOTAL = DSQRT(BR*BR+BZ*BZ)                               COI23780
               WRITE(FILE,3645) Z*1000, R*1000, BZ*1D4, BR*1D4,         COI23790
     1                          TOTAL*1D4, HOMOG                        COI23800
               WRITE(6,3645) Z*1000, R*1000, BZ*1D4, BR*1D4,            COI23810
     1                       TOTAL*1D4, HOMOG                           COI23820
 3645          FORMAT(2X, F8.2 , 2X, F8.2, F14.5, F14.5, F14.5, 5X,     COI23830
     1                G12.6)                                            COI23840
C                                                                       COI23850
               Z = Z + DELTAZ                                           COI23860
               IF (Z .LE. ZEND) GOTO 3640                               COI23870
            CONTINUE                                                    COI23880
C                                                                       COI23890
         ELSE IF (ANSW .EQ. 'X') THEN                                   COI23900
C              (* flux profile *)                                       COI23910
            CALL ASK(2, R, ZBEG, DELTAZ, ZEND)                          COI23920
C                 (* ask the profile coordinates *)                     COI23930
C                                                                       COI23940
            WRITE(6,*) ('  ')                                           COI23950
            WRITE(FILE,*) ('  ')                                        COI23960
            WRITE(6,3650)                                               COI23970
            WRITE(FILE,3650)                                            COI23980
 3650       FORMAT(5X, 'z (mm)', 4X, 'r (mm)', 3X, 'Flux (Gauss*cm*cm)')COI23990
C                                                                       COI24000
            WRITE(FILE,*) ' '                                           COI24010
C                                                                       COI24020
            Z = ZBEG                                                    COI24030
C                                                                       COI24040
 3655       CONTINUE                                                    COI24050
C                 (* repeat flux calculation until Z = ZEND *)          COI24060
               IF (R .NE. 0)                                            COI24070
     1            CALL FLUX(Z, R, NN, MAX, NSHI, LENSHI, RADSHI,        COI24080
     2                      CENSHI, RHOLE, CURSHI, CENSOL, LENSOL,      COI24090
     3                      RADSOL, LOOPS, LAYERS, WIRDIA, FOILTH, CUR, COI24100
     4                      BEXT, MAGFLU)                               COI24110
               IF (R .EQ. 0) POTEN = 0                                  COI24120
               WRITE(FILE,3660) Z*1000, R*1000, MAGFLU*1D8              COI24130
               WRITE(6,3660) Z*1000, R*1000, MAGFLU*1D8                 COI24140
 3660          FORMAT(2X, F8.2 , 2X, F8.2, F14.5)                       COI24150
C                                                                       COI24160
               Z = Z + DELTAZ                                           COI24170
               IF (Z. LE. ZEND) GOTO 3655                               COI24180
            CONTINUE                                                    COI24190
C                                                                       COI24200
         ELSE                                                           COI24210
C              (* current distribution in the shield *)                 COI24220
            CALL DISTRI(NN, MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE,   COI24230
     1                  CURSHI, FILE)                                   COI24240
C                                                                       COI24250
         ENDIF                                                          COI24260
C                                                                       COI24270
         WRITE(FILE,*) ' '                                              COI24280
C                                                                       COI24290
 3665    CONTINUE                                                       COI24300
            WRITE(6,3670)                                               COI24310
 3670       FORMAT('0Do you want to continue with the same ',           COI24320
     1             'parameters (Y/N)?')                                 COI24330
            READ(5,'(A1)') ANSW                                         COI24340
            IF ((ANSW .NE. 'Y') .AND. (ANSW .NE. 'N')) GOTO 3665        COI24350
         CONTINUE                                                       COI24360
         IF (ANSW .EQ. 'Y') GOTO 3615                                   COI24370
      CONTINUE                                                          COI24380
C                                                                       COI24390
      WRITE(FILE,*) ('  ')                                              COI24400
      WRITE(FILE,*) ('  ')                                              COI24410
      CALL WRSHI(FILE, MAX, NSHI, LENSHI, RADSHI, RHOLE, CENSHI)        COI24420
      CALL WROTH(FILE, MAX, LENSOL, RADSOL, CENSOL, LAYERS,             COI24430
     1           LOOPS, SYMSOL, CUR, WIRDIA, FOILTH, BEXT, TRAFLU)      COI24440
      WRITE(FILE,*) ' '                                                 COI24450
      WRITE(FILE,*) '...............................................',  COI24460
     1              '........................'                          COI24470
      WRITE(FILE,*) ' '                                                 COI24480
C           (* write the input parameters to the result file *)         COI24490
C                                                                       COI24500
      RETURN                                                            COI24510
      END                                                               COI24520
C                                                                       COI24530
C                                                                       COI24540
C     ****************************************************************  COI24550
C                                                                       COI24560
C                                                                       COI24570
C     MAIN PROGRAM                                                      COI24580
C                                                                       COI24590
C           (* This  program  calculates  the magnetic field of a *)    COI24600
C           (* collection of coaxial solenoids and cylindrical or *)    COI24610
C           (* planar  superconducting  shield parts. Each of the *)    COI24620
C           (* shield  parts  is  approximated by an equal number *)    COI24630
C           (* of  current  rings, whose total number is NN (or a *)    COI24640
C           (* little less).                                      *)    COI24650
C                                                                       COI24660
      INTEGER NN, MAX                                                   COI24670
C           (* NN : the  maximum number of current rings used for *)    COI24680
C           (*      approximating the current distribution in the *)    COI24690
C           (*      superconducting shield                        *)    COI24700
C           (* MAX : the  maximum  number of solenoids and shield *)    COI24710
C           (*       parts                                        *)    COI24720
C                                                                       COI24730
      PARAMETER(NN=100, MAX=30)                                         COI24740
C                                                                       COI24750
      INTEGER NSHI, LEVOLD, LAYERS(MAX)                                 COI24760
      DOUBLE PRECISION LENSHI(MAX), RADSHI(MAX), RHOLE(MAX),            COI24770
     1              CENSHI(MAX), LENSOL(MAX),RADSOL(MAX), CENSOL(MAX),  COI24780
     2              LOOPS(MAX), CUR(MAX), WIRDIA, FOILTH, BEXT, TRAFLU, COI24790
     3              CURSHI(NN), M(NN,NN), A(NN), RWKSP(2*NN*NN+4*NN)    COI24800
      CHARACTER*1 ANSW                                                  COI24810
      LOGICAL DIFSHI, SYMSHI(MAX), SYMSOL(MAX)                          COI24820
C                                                                       COI24830
      COMMON /WORKSP/ RWKSP                                             COI24840
C                  ...............................                      COI24850
C                                                                       COI24860
      IF (NN .GE. 35) CALL IWKIN(2*NN*NN+4*NN)                          COI24870
C           (* total amount of automatically allocated space is *)      COI24880
C           (* 2500  double  precision units; when workspace is *)      COI24890
C           (* needed more, it must be allocated using IWKIN    *)      COI24900
C                                                                       COI24910
 9910 CONTINUE                                                          COI24920
         CALL DIMENS(NSHI, DIFSHI, LENSHI, RADSHI, RHOLE, CENSHI,       COI24930
     1             SYMSHI, LENSOL, RADSOL, CENSOL, LAYERS, LOOPS,       COI24940
     2             SYMSOL, CUR, WIRDIA, FOILTH, BEXT, TRAFLU, MAX)      COI24950
C              (* ask the input parameters *)                           COI24960
C                                                                       COI24970
         IF (NSHI .NE. 0)                                               COI24980
C              (* if there is a superconducting shield *)               COI24990
     1      CALL CURREN(NN, MAX, NSHI, DIFSHI, LENSHI, RADSHI, CENSHI,  COI25000
     2                  RHOLE, SYMSHI, LENSOL, RADSOL, CENSOL, LAYERS,  COI25010
     3                  LOOPS, SYMSOL, CUR, WIRDIA, FOILTH, BEXT,       COI25020
     4                  TRAFLU, M, A, CURSHI)                           COI25030
C                 (* calculate the current distribution in the shield *)COI25040
C                                                                       COI25050
         CALL RESULT(NN, MAX, NSHI, LENSHI, RADSHI, CENSHI, RHOLE,      COI25060
     1               CURSHI, CENSOL, LENSOL, RADSOL, LOOPS, LAYERS,     COI25070
     2               SYMSOL, WIRDIA, FOILTH, CUR, BEXT, TRAFLU)         COI25080
C              (* calculate and print the magnetic field profile etc. *)COI25090
C                                                                       COI25100
 9920    CONTINUE                                                       COI25110
            WRITE(6,9930)                                               COI25120
 9930       FORMAT ('0Do you want to continue (Y/N)?')                  COI25130
            READ(5,'(A1)') ANSW                                         COI25140
            IF ((ANSW .NE. 'Y') .AND. (ANSW .NE. 'N')) GOTO 9920        COI25150
         CONTINUE                                                       COI25160
         IF (ANSW .EQ. 'Y') GOTO 9910                                   COI25170
      CONTINUE                                                          COI25180
C                                                                       COI25190
      STOP                                                              COI25200
      END                                                               COI25210


