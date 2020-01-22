C-----------------------------------------------------------------------
C  IMSL Name:  ACHAR (Single precision version)
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    September 9, 1985
C
C  Purpose:    Return a character given its ASCII value.
C
C  Usage:      ACHAR(I)
C
C  Arguments:
C     I      - Integer ASCII value of the character desired.  (Input)
C              I must be greater than or equal to zero and less than or
C              equal to 127.
C     ACHAR  - CHARACTER*1 string containing the character in the I-th
C              position of the ASCII collating sequence.  (Output)
C
C  Keywords:   Utilities; Character string manipulation; character
C              conversion
C
C  GAMS:       N3
C
C  Chapter:    MATH/LIBRARY Utilities
C              STAT/LIBRARY Utilities
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      CHARACTER *1 FUNCTION ACHAR (I)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    I
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  CHAR
      INTRINSIC  CHAR
      CHARACTER  CHAR
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1STI, E1PSH, E1POP
C
      IF (I .LT. 0) THEN
         ACHAR = CHAR(0)
         CALL E1PSH ('ACHAR ')
         CALL E1STI (1, I)
         CALL E1MES (5, 1, 'The ASCII value must be non-negative '//
     &               'while I = %(I1) is given.')
         CALL E1POP ('ACHAR ')
      ELSE
         ACHAR = CHAR(I)
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  C1TCI
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    August 13, 1984
C
C  Purpose:    Convert character string into corresponding integer
C              form.
C
C  Usage:      CALL C1TCI (CHRSTR, SLEN, NUM, IER)
C
C  Arguments:
C   CHRSTR  - Character array that contains the number description.
C             (Input)
C   SLEN    - Length of the character array.  (Input)
C   NUM     - The answer.  (Output)
C   IER     - Completion code.  (Output)  Where
C                IER =-2  indicates that the number is too large to
C                         be converted;
C                IER =-1  indicates that SLEN <= 0;
C                IER = 0  indicates normal completion;
C                IER > 0  indicates that the input string contains a
C                         nonnumeric character.  IER is the index of
C                         the first nonnumeric character in CHRSTR.
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE C1TCI (CHRSTR, SLEN, NUM, IER)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    SLEN, NUM, IER
      CHARACTER  CHRSTR(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    COUNT, I, IMACH5, J, N, S, SIGN
      CHARACTER  ZERO
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      CHARACTER  BLANK, DIGIT*10, MINUS, PLUS
      SAVE       BLANK, DIGIT, MINUS, PLUS
C                                  SPECIFICATIONS FOR EQUIVALENCE
      EQUIVALENCE (DIGIT, ZERO)
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  INDEX
      INTRINSIC  INDEX
      INTEGER    INDEX
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   IMACH
      INTEGER    IMACH
C
      DATA DIGIT/'0123456789'/
      DATA BLANK/' '/, MINUS/'-'/, PLUS/'+'/
C
C                                  CHECK SLEN
      NUM = 0
      IF (SLEN .LE. 0) THEN
         IER = -1
         GO TO 50
      END IF
C                                  HANDLE LEADING BLANKS
      SIGN = 1
      I = 1
   10 IF (I .LE. SLEN) THEN
         IF (CHRSTR(I) .EQ. BLANK) THEN
            I = I + 1
            GO TO 10
         END IF
      ELSE
         IER = 1
         GO TO 50
      END IF
C                                  CHECK FOR SIGN, IF ANY
      S = I
      IF (CHRSTR(I) .EQ. MINUS) THEN
         SIGN = -1
         I = I + 1
      ELSE IF (CHRSTR(I) .EQ. PLUS) THEN
         I = I + 1
      END IF
   20 IF (I .LE. SLEN) THEN
         IF (CHRSTR(I) .EQ. BLANK) THEN
            I = I + 1
            GO TO 20
         END IF
      ELSE
         IER = S
         GO TO 50
      END IF
C                                  SKIP LEADING ZERO
      J = I
   30 IF (I .LE. SLEN) THEN
         IF (CHRSTR(I) .EQ. ZERO) THEN
            I = I + 1
            GO TO 30
         END IF
      ELSE
         IER = 0
         GO TO 50
      END IF
C                                  CHECK FIRST NONBLANK CHARACTER
      COUNT = 0
C                                  CHECK NUMERIC CHARACTERS
      IMACH5 = IMACH(5)
   40 N = INDEX(DIGIT,CHRSTR(I))
      IF (N .NE. 0) THEN
         COUNT = COUNT + 1
         IF (NUM .GT. ((IMACH5-N)+1)/10) THEN
            IER = -2
            GO TO 50
         ELSE
            NUM = NUM*10 - 1 + N
            I = I + 1
            IF (I .LE. SLEN) GO TO 40
         END IF
      END IF
C
      IF (COUNT .EQ. 0) THEN
         IF (I .GT. J) THEN
            IER = I
         ELSE
            IER = S
         END IF
      ELSE IF (I .GT. SLEN) THEN
         NUM = SIGN*NUM
         IER = 0
      ELSE
         NUM = SIGN*NUM
         IER = I
      END IF
C
   50 CONTINUE
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  C1TIC
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    March 9, 1984
C
C  Purpose:    Convert an integer to its corresponding character form.
C              (Right justified)
C
C  Usage:      CALL C1TIC(NUM, CHRSTR, SLEN, IER)
C
C  Arguments:
C     NUM    - Integer number.  (Input)
C     CHRSTR - Character array that receives the result.  (Output)
C     SLEN   - Length of the character array.  (Input)
C     IER    - Completion code.  (Output) Where
C                 IER < 0  indicates that SLEN <= 0,
C                 IER = 0  indicates normal completion,
C                 IER > 0  indicates that the character array is too
C                       small to hold the complete number.  IER
C                       indicates how many significant digits are
C                       being truncated.
C
C  Remarks:
C  1. The character array is filled in a right justified manner.
C  2. Leading zeros are replaced by blanks.
C  3. Sign is inserted only for negative number.
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE C1TIC (NUM, CHRSTR, SLEN, IER)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    NUM, SLEN, IER
      CHARACTER  CHRSTR(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, J, K, L
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      CHARACTER  BLANK(1), DIGIT(10), MINUS(1)
      SAVE       BLANK, DIGIT, MINUS
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  IABS
      INTRINSIC  IABS
      INTEGER    IABS
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   M1VE
C
      DATA DIGIT/'0', '1', '2', '3', '4', '5', '6', '7', '8',
     &     '9'/
      DATA BLANK/' '/, MINUS/'-'/
C                                  CHECK SLEN
      IF (SLEN .LE. 0) THEN
         IER = -1
         RETURN
      END IF
C                                  THE NUMBER IS ZERO
      IF (NUM .EQ. 0) THEN
         CALL M1VE (BLANK, 1, 1, 1, CHRSTR, 1, SLEN-1, SLEN, I)
         CHRSTR(SLEN) = DIGIT(1)
         IER = 0
         RETURN
      END IF
C                                  CONVERT NUMBER DIGIT BY DIGIT TO
C                                  CHARACTER FORM
      J = SLEN
      K = IABS(NUM)
   10 IF (K.GT.0 .AND. J.GE.1) THEN
         L = K
         K = K/10
         L = L - K*10
         CHRSTR(J) = DIGIT(L+1)
         J = J - 1
         GO TO 10
      END IF
C
   20 IF (K .EQ. 0) THEN
         IF (NUM .LT. 0) THEN
            CALL M1VE (MINUS, 1, 1, 1, CHRSTR, J, J, SLEN, I)
            IF (I .NE. 0) THEN
               IER = 1
               RETURN
            END IF
            J = J - 1
         END IF
         IER = 0
         CALL M1VE (BLANK, 1, 1, 1, CHRSTR, 1, J, SLEN, I)
         RETURN
      END IF
C                                  DETERMINE THE NUMBER OF SIGNIFICANT
C                                  DIGITS BEING TRUNCATED
      I = 0
   30 IF (K .GT. 0) THEN
         K = K/10
         I = I + 1
         GO TO 30
      END IF
C
      IF (NUM .LT. 0) I = I + 1
      IER = I
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  DASUM (Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    August 9, 1986
C
C  Purpose:    Sum the absolute values of the components of a
C              double precision vector.
C
C  Usage:      DASUM(N, DX, INCX)
C
C  Arguments:
C     N      - Length of vectors X.  (Input)
C     DX     - Double precision vector of length N*INCX.  (Input)
C     INCX   - Displacement between elements of DX.  (Input)
C              X(I) is defined to be DX(1+(I-1)*INCX).  INCX must be
C              greater than 0.
C     DASUM  - Double precision sum from I=1 to N of DABS(X(I)).
C              (Output)
C              X(I) refers to a specific element of DX.
C
C  GAMS:       D1a
C
C  Chapters:   MATH/LIBRARY Basic Matrix/Vector Operations
C              STAT/LIBRARY Mathematical Support
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION DASUM (N, DX, INCX)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, INCX
      DOUBLE PRECISION DX(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, M, MP1, NINCX
C                                  SPECIFICATIONS FOR SPECIAL CASES
C     INTRINSIC  MOD
      INTRINSIC  MOD
      INTEGER    MOD
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  DABS
      INTRINSIC  DABS
      DOUBLE PRECISION DABS
C
      DASUM = 0.0D0
      IF (N .GT. 0) THEN
         IF (INCX .NE. 1) THEN
C                                  CODE FOR INCREMENT NOT EQUAL TO 1
            NINCX = N*INCX
            DO 10  I=1, NINCX, INCX
               DASUM = DASUM + DABS(DX(I))
   10       CONTINUE
         ELSE
C                                  CODE FOR INCREMENT EQUAL TO 1
            M = MOD(N,6)
C                                  CLEAN-UP LOOP
            DO 30  I=1, M
               DASUM = DASUM + DABS(DX(I))
   30       CONTINUE
            MP1 = M + 1
            DO 40  I=MP1, N, 6
               DASUM = DASUM + DABS(DX(I)) + DABS(DX(I+1)) +
     &                 DABS(DX(I+2)) + DABS(DX(I+3)) + DABS(DX(I+4)) +
     &                 DABS(DX(I+5))
   40       CONTINUE
         END IF
      END IF
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  DAXPY (Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    August 9, 1986
C
C  Purpose:    Compute the scalar times a vector plus a vector,
C              y = ax + y, all double precision.
C
C  Usage:      CALL DAXPY (N, DA, DX, INCX, DY, INCY)
C
C  Arguments:
C     N      - Length of vectors X and Y.  (Input)
C     DA     - Double precision scalar.  (Input)
C     DX     - Double precision vector of length MAX(N*IABS(INCX),1).
C                 (Input)
C     INCX   - Displacement between elements of DX.  (Input)
C              X(I) is defined to be
C                 DX(1+(I-1)*INCX) if INCX.GE.0  or
C                 DX(1+(I-N)*INCX) if INCX.LT.0.
C     DY     - Double precision vector of length MAX(N*IABS(INCY),1).
C                 (Input/Output)
C              DAXPY replaces Y(I) with DA*X(I) + Y(I) for I=1,...,N.
C              X(I) and Y(I) refer to specific elements of DX and DY.
C     INCY   - Displacement between elements of DY.  (Input)
C              Y(I) is defined to be
C                 DY(1+(I-1)*INCY) if INCY.GE.0  or
C                 DY(1+(I-N)*INCY) if INCY.LT.0.
C
C  GAMS:       D1a7
C
C  Chapters:   MATH/LIBRARY Basic Matrix/Vector Operations
C              STAT/LIBRARY Mathematical Support
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DAXPY (N, DA, DX, INCX, DY, INCY)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, INCX, INCY
      DOUBLE PRECISION DA, DX(*), DY(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, IX, IY, M, MP1
C                                  SPECIFICATIONS FOR SPECIAL CASES
C     INTRINSIC  MOD
      INTRINSIC  MOD
      INTEGER    MOD
C
      IF (N .GT. 0) THEN
         IF (DA .NE. 0.0D0) THEN
            IF (INCX.NE.1 .OR. INCY.NE.1) THEN
C                                  CODE FOR UNEQUAL INCREMENTS OR EQUAL
C                                  INCREMENTS NOT EQUAL TO 1
               IX = 1
               IY = 1
               IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
               IF (INCY .LT. 0) IY = (-N+1)*INCY + 1
               DO 10  I=1, N
                  DY(IY) = DY(IY) + DA*DX(IX)
                  IX = IX + INCX
                  IY = IY + INCY
   10          CONTINUE
            ELSE
C                                  CODE FOR BOTH INCREMENTS EQUAL TO 1
               M = MOD(N,4)
C                                  CLEAN-UP LOOP
               DO 30  I=1, M
                  DY(I) = DY(I) + DA*DX(I)
   30          CONTINUE
               MP1 = M + 1
               DO 40  I=MP1, N, 4
                  DY(I) = DY(I) + DA*DX(I)
                  DY(I+1) = DY(I+1) + DA*DX(I+1)
                  DY(I+2) = DY(I+2) + DA*DX(I+2)
                  DY(I+3) = DY(I+3) + DA*DX(I+3)
   40          CONTINUE
            END IF
         END IF
      END IF
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  CONST/DCONST (Single/Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    June 11, 1986
C
C  Purpose:    Return the value of various mathematical and physical
C              constants.
C
C  Usage:      CONST(NAME)
C
C  Arguments:
C     NAME   - Character string containing the name of the desired
C              constant.  (Input)
C              See remark 3 for a list of valid constants.
C     CONST  - Value of the constant.  (Output)
C
C  Remarks:
C  1. The case of the character string in NAME does not matter.  The
C     names 'PI', 'Pi', 'pI' and 'pi' are equivalent.
C
C  2. The units of the physical constants are in SI units, (meter-
C     kilogram-second).
C
C  3. The names allowed are as follows:
C       Name             Description            Value          Reference
C     AMU              Atomic mass unit       1.6605655E-27 kg       (1)
C     ATM              Standard atm pressure  1.01325E+5 N/m**2     E(2)
C     AU               Astronomical unit      1.496E+11 m            ( )
C     Avogadro         Avogadro's number      6.022045E+23 1/mole    (1)
C     Boltzman         Boltzman's constant    1.380662E-23 J/K       (1)
C     C                Speed of light         2.997924580E+8 m/s     (1)
C     Catalan          Catalan's constant     0.915965...           E(3)
C     E                Base of natural logs   2.718...              E(3)
C     ElectronCharge   Electron change        1.6021892E-19 C        (1)
C     ElectronMass     Electron mass          9.109534E-31 kg        (1)
C     ElectronVolt     Electron volt          1.6021892E-19 J        (1)
C     Euler            Euler's constant gamma 0.577...              E(3)
C     Faraday          Faraday constant       9.648456E+4 C/mole     (1)
C     FineStructure    Fine structure         7.2973506E-3           (1)
C     Gamma            Euler's constant       0.577...              E(3)
C     Gas              Gas constant           8.31441 J/mole/K       (1)
C     Gravity          Gravitational constant 6.6720E-11 N*m**2/kg**2(1)
C     Hbar             Planck constant / 2 pi 1.0545887E-34 J*s      (1)
C     PerfectGasVolume Std vol ideal gas      2.241383E-2 m**3/mole  (1)
C     Pi               Pi                     3.141...              E(3)
C     Planck           Planck's constant h    6.626176E-34 J*s       (1)
C     ProtonMass       Proton mass            1.6726485E-27 kg       (1)
C     Rydberg          Rydberg's constant     1.097373177E+7 /m      (1)
C     SpeedLight       Speed of light         2.997924580E+8 m/s     (1)
C     StandardGravity  Standard g             9.80665 m/s**2        E(2)
C     StandardPressure Standard atm pressure  1.01325E+5 N/m**2     E(2)
C     StefanBoltzmann  Stefan-Boltzman        5.67032E-8 W/K**4/m**2 (1)
C     WaterTriple      Triple point of water  2.7316E+2 K           E(2)
C
C  Keywords:   Mathematical constants; Physical constants;
C              Avogadro's number; Atomic mass unit; Astronomical unit;
C              Boltzman's constant; Speed of light; Catalan's constant;
C              Electron charge; Electron mass; Electron volt; Gamma;
C              Euler's constant; Faraday's constant; Fine structure
C              constant; Gas constant; Gravitational constant;
C              Planck's constant; Protron mass; Stefan-Boltzman's
C              constant; Pi; Triple point of water
C
C  GAMS:       C19
C
C  Chapter:    MATH/LIBRARY Utilities
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION DCONST (NAME)
C                                  SPECIFICATIONS FOR ARGUMENTS
      CHARACTER  NAME*(*)
C                                  SPECIFICATIONS FOR PARAMETERS
      INTEGER    MAXLEN, MAXTAB
      PARAMETER  (MAXLEN=16, MAXTAB=27)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, KEY
      CHARACTER  CAPNAM*(MAXLEN)
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      DOUBLE PRECISION TABLE(MAXTAB)
      CHARACTER  CTABLE(MAXTAB)*(MAXLEN)
      SAVE       CTABLE, TABLE
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  LEN,MIN0
      INTRINSIC  LEN, MIN0
      INTEGER    LEN, MIN0
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1PSH, E1STL, SSRCH
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   ACHAR, DMACH, ICASE
      INTEGER    ICASE
      DOUBLE PRECISION DMACH
      CHARACTER  ACHAR
C                                  Must be in alphabetical order
      DATA (CTABLE(I),TABLE(I),I=1,10)/'AMU', 1.6605655D-27,
     &     'ATM', 1.01325D+5, 'AU', 1.496D+11, 'AVOGADRO',
     &     6.022045D+23, 'BOLTZMAN', 1.380662D-23, 'C', 2.99792458D+8,
     &     'CATALAN', 0.915965594177219015054603514932D0, 'E',
     &     2.71828182845904523536028747135D0, 'ELECTRONCHARGE',
     &     1.6021892D-19, 'ELECTRONMASS', 9.109534D-31/
      DATA (CTABLE(I),TABLE(I),I=11,20)/'ELECTRONVOLT', 1.6021892D-19,
     &     'EULER', 0.577215664901532860606512090082D0, 'FARADAY',
     &     9.648456D+4, 'FINESTRUCTURE', 7.2973506D-3, 'GAMMA',
     &     0.577215664901532860606512090082D0, 'GAS', 8.31441D0,
     &     'GRAVITY', 6.672D-11, 'HBAR', 1.0545887D-34,
     &     'PERFECTGASVOLUME', 2.241383D-2, 'PI',
     &     3.14159265358979323846264338328D0/
      DATA (CTABLE(I),TABLE(I),I=21,MAXTAB)/'PLANCK', 6.626176D-34,
     &     'PROTONMASS', 1.6726485D-27, 'RYDBERG', 1.097373177D+7,
     &     'SPEEDLIGHT', 2.99792458D+8, 'STANDARDGRAVITY', 9.80665D0,
     &     'STEFANBOLTZMANN', 5.67032D-8, 'WATERTRIPLE', 2.7316D+2/
C
      CALL E1PSH ('DCONST ')
C                                  Convert NAME to upper case CAPNAM
      CAPNAM = ' '
      DO 10  I=1, MIN0(LEN(NAME),MAXLEN)
         CAPNAM(I:I) = ACHAR(ICASE(NAME(I:I)))
   10 CONTINUE
C                                  Binary search of CTABLE for CAPNAM
      CALL SSRCH (MAXTAB, CAPNAM, CTABLE, 1, KEY)
      IF (KEY .GT. 0) THEN
         DCONST = TABLE(KEY)
      ELSE
         CALL E1STL (1, NAME)
         CALL E1MES (5, 1, 'The argument NAME = %(L1) is '//
     &               'illegal.')
C                                  Return not-a-number on error
         DCONST = DMACH(6)
      END IF
C
 9000 CALL E1POP ('DCONST ')
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  DDOT (Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    August 9, 1986
C
C  Purpose:    Compute the double-precision dot product x*y.
C
C  Usage:      DDOT(N, DX, INCX, DY, INCY)
C
C  Arguments:
C     N      - Length of vectors X and Y.  (Input)
C     DX     - Double precision vector of length MAX(N*IABS(INCX),1).
C              (Input)
C     INCX   - Displacement between elements of DX.  (Input)
C              X(I) is defined to be.. DX(1+(I-1)*INCX) if INCX .GE. 0
C              or DX(1+(i-n)*INCX) if INCX .LT. 0.
C     DY     - Double precision vector of length MAX(N*IABS(INCY),1).
C              (Input)
C     INCY   - Displacement between elements of DY.  (Input)
C              Y(I) is defined to be.. DY(1+(I-1)*INCY) if INCY .GE. 0
C              or DY(1+(I-N)*INCY) if INCY .LT. 0.
C     DDOT   - Double precision sum from I=1 to N of X(I)*Y(I).
C              (Output)
C              X(I) and Y(I) refer to specific elements of DX and DY,
C              respectively. See INCX and INCY argument descriptions.
C
C  Keywords:   Level 1 BLAS; DDOT; Inner product; Scalar product
C
C  GAMS:       D1a4
C
C  Chapters:   MATH/LIBRARY Basic Matrix/Vector Operations
C              STAT/LIBRARY Mathematical Support
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION DDOT (N, DX, INCX, DY, INCY)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, INCX, INCY
      DOUBLE PRECISION DX(*), DY(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, IX, IY, M, MP1
C                                  SPECIFICATIONS FOR SPECIAL CASES
C     INTRINSIC  MOD
      INTRINSIC  MOD
      INTEGER    MOD
C
      DDOT = 0.0D0
      IF (N .GT. 0) THEN
         IF (INCX.NE.1 .OR. INCY.NE.1) THEN
C                                  CODE FOR UNEQUAL INCREMENTS.
            IX = 1
            IY = 1
            IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
            IF (INCY .LT. 0) IY = (-N+1)*INCY + 1
            DO 10  I=1, N
               DDOT = DDOT + DX(IX)*DY(IY)
               IX = IX + INCX
               IY = IY + INCY
   10       CONTINUE
         ELSE
C                                  CODE FOR BOTH INCREMENTS EQUAL TO 1
C                                    CLEAN-UP LOOP SO REMAINING VECTOR
C                                    LENGTH IS A MULTIPLE OF 5.
            M = MOD(N,5)
            DO 30  I=1, M
               DDOT = DDOT + DX(I)*DY(I)
   30       CONTINUE
            MP1 = M + 1
            DO 40  I=MP1, N, 5
               DDOT = DDOT + DX(I)*DY(I) + DX(I+1)*DY(I+1) +
     &                DX(I+2)*DY(I+2) + DX(I+3)*DY(I+3) +
     &                DX(I+4)*DY(I+4)
   40       CONTINUE
         END IF
      END IF
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  ELE/DELE (Single/Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    February 20, 1986
C
C  Purpose:    Evaluate the complete elliptic integral of the second
C              kind E(x).
C
C  Usage:      ELE(X)
C
C  Arguments:
C     X      - Argument for which the function value is desired.
C              (Input)
C              X must be greater than or equal to 0 and less than or
C              equal to 1.
C     ELE    - Function value.  (Output)
C
C  Remark:
C     See the references:
C     Carlson, B. C. and Notis, E., M., (1981), Algorithms for
C       incomplete elliptic integrals, ACM Transactions on Mathematical
C       Software, 7, pages 398-403.
C     Carlson, B. C., (1979), Computing elliptic integrals by
C       duplication, Numeriche Mathematic, 33, pages 1-16.
C
C  GAMS:       C14
C
C  Chapter:    SFUN/LIBRARY Elliptic Integrals
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION DELE (X)
C                                  SPECIFICATIONS FOR ARGUMENTS
      DOUBLE PRECISION X
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I
      DOUBLE PRECISION CAYSQ, EPS, ETA, SUMA, SUMB
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      DOUBLE PRECISION A(10), B(10)
      SAVE       A, B
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  DLOG
      INTRINSIC  DLOG
      DOUBLE PRECISION DLOG
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1PSH, E1STD
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   DMACH, N1RTY
      INTEGER    N1RTY
      DOUBLE PRECISION DMACH
C
      DATA A(1)/0.1494662175718132D-03/
      DATA A(2)/0.2468503330460722D-02/
      DATA A(3)/0.8638442173604073D-02/
      DATA A(4)/0.1077063503986645D-01/
      DATA A(5)/0.7820404060959553D-02/
      DATA A(6)/0.7595093422559432D-02/
      DATA A(7)/0.115695957452954D-01/
      DATA A(8)/0.2183181167613048D-01/
      DATA A(9)/0.5680519456755915D-01/
      DATA A(10)/0.4431471805608895D00/
      DATA B(1)/0.3185919565550157D-04/
      DATA B(2)/0.9898332846225384D-03/
      DATA B(3)/0.6432146586438302D-02/
      DATA B(4)/0.1680402334636338D-01/
      DATA B(5)/0.2614501470031388D-01/
      DATA B(6)/0.3347894366576162D-01/
      DATA B(7)/0.4271789054738309D-01/
      DATA B(8)/0.5859366125553149D-01/
      DATA B(9)/0.9374999972120313D-01/
      DATA B(10)/0.2499999999999017D00/
C
      CALL E1PSH ('DELE ')
      DELE = DMACH(2)
C
      IF (X.LT.0.0D0 .OR. X.GT.1.0D0) THEN
         CALL E1STD (1, X)
         CALL E1MES (5, 2, 'The argument, X = %(D1), must be greater '//
     &               'than or equal to 0.0 and less than or equal '//
     &               'to 1.0.')
      END IF
C
      IF (N1RTY(0) .GT. 0) GO TO 9000
C
      EPS = DMACH(4)
      CAYSQ = X
C
      ETA = 1.0D0 - CAYSQ
C
      IF (ETA .GE. EPS) THEN
         SUMA = 0.0D0
         SUMB = 0.0D0
         DO 10  I=1, 10
            SUMA = (SUMA+A(I))*ETA
            SUMB = (SUMB+B(I))*ETA
   10    CONTINUE
         DELE = SUMA - DLOG(ETA)*SUMB
         DELE = DELE + 1.0D0 + EPS
      ELSE
         DELE = 1.0D0
      END IF
C
 9000 CALL E1POP ('DELE ')
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  ELK/DELK (Single/Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    February 20, 1986
C
C  Purpose:    Evaluate the complete elliptic integral of the kind K(x).
C
C  Usage:      ELK(X)
C
C  Arguments:
C     X      - Argument for which the function value is desired.
C              (Input)
C              X must be greater than or equal to 0 and less than 1.
C     ELK    - Function value.  (Output)
C
C  Remark:
C     See the references:
C     Carlson, B. C. and Notis, E., M., (1981), Algorithms for
C       incomplete elliptic integrals, ACM Transactions on Mathematical
C       Software, 7, pages 398-403.
C     Carlson, B. C., (1979), Computing elliptic integrals by
C       duplication, Numeriche Mathematic, 33, pages 1-16.
C
C  GAMS:       C14
C
C  Chapter:    SFUN/LIBRARY Elliptic Integrals
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION DELK (X)
C                                  SPECIFICATIONS FOR ARGUMENTS
      DOUBLE PRECISION X
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I
      DOUBLE PRECISION CAYSQ, EPS, ETA, SUMA, SUMB
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      DOUBLE PRECISION A(11), B(11)
      SAVE       A, B
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  DLOG
      INTRINSIC  DLOG
      DOUBLE PRECISION DLOG
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1PSH, E1STI, E1STD
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   DMACH, N1RTY
      INTEGER    N1RTY
      DOUBLE PRECISION DMACH
C
      DATA A(1)/0.1393087857006646D-03/
      DATA A(2)/0.2296634898396958D-02/
      DATA A(3)/0.8003003980649985D-02/
      DATA A(4)/0.9848929322176892D-02/
      DATA A(5)/0.6847909282624505D-02/
      DATA A(6)/0.6179627446053317D-02/
      DATA A(7)/0.8789801874555064D-02/
      DATA A(8)/0.1493801353268716D-01/
      DATA A(9)/0.3088514627130518D-01/
      DATA A(10)/0.9657359028085625D-01/
      DATA A(11)/0.138629436111989D01/
      DATA B(1)/0.2970028096655561D-04/
      DATA B(2)/0.9215546349632497D-03/
      DATA B(3)/0.5973904299155429D-02/
      DATA B(4)/0.155309416319772D-01/
      DATA B(5)/0.2393191332311079D-01/
      DATA B(6)/0.3012484901289892D-01/
      DATA B(7)/0.373777397586236D-01/
      DATA B(8)/0.4882804190686239D-01/
      DATA B(9)/0.7031249973903835D-01/
      DATA B(10)/0.124999999999908D00/
      DATA B(11)/0.5D00/
C
      CALL E1PSH ('DELK ')
      DELK = DMACH(2)
C
      IF (X.LT.0.0D0 .OR. X.GE.1.0D0) THEN
         CALL E1STD (1, X)
         CALL E1MES (5, 2, 'The argument, X = %(D1), must be '//
     &               'greater than or equal to 0.0 and less than '//
     &               '1.0.')
      END IF
C
      IF (N1RTY(0) .GT. 0) GO TO 9000
C
      EPS = DMACH(4)
      CAYSQ = X
C
      ETA = 1.0D0 - CAYSQ
C
      IF (ETA .GE. EPS) THEN
         SUMA = A(1)
         SUMB = B(1)
         DO 10  I=2, 11
            SUMA = SUMA*ETA + A(I)
            SUMB = SUMB*ETA + B(I)
   10    CONTINUE
         DELK = SUMA - DLOG(ETA)*SUMB
      ELSE
C                                  RETURN FOR SMALL ARGUMENT
         DELK = A(11) - DLOG(ETA)*B(11)
      END IF
C
 9000 CALL E1POP ('DELK ')
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  ELRC/DELRC (Single/Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    February 20, 1986
C
C  Purpose:    Evaluate an elementary integral from which inverse
C              circular functions, logarithms and inverse hyperbolic
C              functions can be computed.
C
C  Usage:      ELRC(X, Y)
C
C  Arguments:
C     X      - First variable of the incomplete elliptic integral.
C              (Input)
C              It must be nonnegative and satisfy the conditions given
C              in Remark 1.
C     Y      - Second variable of the incomplete elliptic integral.
C              (Input)
C              It must be positive and satisfy the conditions given
C              in Remark 1.
C     ELRC   - Function value.  (Output)
C
C  Remarks:
C  1. The sum X+Y must be greater than or equal to ARGMIN and both X
C     and Y must be less than or equal to ARGMAX, where
C       ARGMIN = mmin*5
C       ARGMAX = mmax/5
C     with mmin the machine minimum and mmax the machine maximum.
C
C  2. See the references:
C     Carlson, B. C. and Notis, E., M., (1981), Algorithms for
C       incomplete elliptic integrals, ACM Transactions on Mathematical
C       Software, 7, pages 398-403.
C     Carlson, B. C., (1979), Computing elliptic integrals by
C       duplication, Numeriche Mathematic, 33, pages 1-16.
C
C  GAMS:       C14
C
C  Chapter:    SFUN/LIBRARY Elliptic Integrals
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION DELRC (X, Y)
C                                  SPECIFICATIONS FOR ARGUMENTS
      DOUBLE PRECISION X, Y
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      DOUBLE PRECISION ALAMDA, AMU, ARGMAX, ARGMIN, C1, C2, ERRTOL, S,
     &           SN, XN, YN
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  DABS,DMAX1,DSQRT
      INTRINSIC  DABS, DMAX1, DSQRT
      DOUBLE PRECISION DABS, DMAX1, DSQRT
      SAVE       ARGMIN, ARGMAX
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1PSH, E1STD
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   DMACH, N1RTY
      INTEGER    N1RTY
      DOUBLE PRECISION DMACH
C
      CALL E1PSH ('DELRC ')
      DELRC = DMACH(2)
C                                  INITIALIZE UPPER AND LOWER LIMITS
      ARGMIN = 5.0D0*DMACH(1)
      ARGMAX = DMACH(2)/5.0D0
C
      IF (X .LT. 0.0D0) THEN
         CALL E1STD (1, X)
         CALL E1MES (5, 1, 'The value of the first variable of the '//
     &               'integral, X = %(D1), must be at least 0.')
      END IF
C
      IF (Y .LE. 0.0D0) THEN
         CALL E1STD (1, Y)
         CALL E1MES (5, 2, 'The value of the second variable of the '//
     &               'integral, Y = %(D1), must be positive.')
      END IF
      IF (N1RTY(0) .GT. 0) GO TO 9000
C
      IF (X+Y .LT. ARGMIN) THEN
         CALL E1STD (1, X+Y)
         CALL E1STD (2, ARGMIN)
         CALL E1MES (5, 3, 'The sum, X+Y = %(D1), must be greater '//
     &               'than or equal to ARGMIN = %(D2).')
         GO TO 9000
      END IF
C
      IF (DMAX1(X,Y) .GT. ARGMAX) THEN
         CALL E1STD (1, X)
         CALL E1STD (2, Y)
         CALL E1STD (3, ARGMAX)
         CALL E1MES (5, 4, 'The arguments, X = %(D1), Y = %(D2), '//
     &               'must be less than or equal to ARGMAX = %(D3).')
         GO TO 9000
      END IF
C
      ERRTOL = 1.0D-3
      XN = X
      YN = Y
C                                  BEGIN LOOP
   10 AMU = (XN+YN+YN)/3.0D0
      SN = (YN+AMU)/AMU - 2.0D0
      IF (DABS(SN) .GE. ERRTOL) THEN
         ALAMDA = 2.0D0*DSQRT(XN)*DSQRT(YN) + YN
         XN = (XN+ALAMDA)*0.25D0
         YN = (YN+ALAMDA)*0.25D0
         GO TO 10
      END IF
C                                  FINAL CALCULATION
      C1 = 1.0D0/7.0D0
      C2 = 9.0D0/22.0D0
      S = SN*SN*(0.3D0+SN*(C1+SN*(0.375D0+SN*C2)))
      DELRC = (1.0D0+S)/DSQRT(AMU)
C
 9000 CALL E1POP ('DELRC ')
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  ELRF/DELRF (Single/Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    February 20, 1986
C
C  Purpose:    Evaluate Carlson's incomplete elliptic integral of the
C              first kind RF(X,Y,Z).
C
C  Usage:      ELRF(X, Y, Z)
C
C  Arguments:
C     X      - First variable of the incomplete elliptic integral.
C              (Input)
C              It must be nonnegative and satisfy the conditions given
C              in Remark 1.
C     Y      - Second variable of the incomplete elliptic integral.
C              (Input)
C              It must be nonnegative and satisfy the conditions given
C              in Remark 1.
C     Z      - Third variable of the incomplete elliptic integral.
C              (Input)
C              It must be nonnegative and satisfy the conditions given
C              in Remark 1.
C     ELRF   - Function value.  (Output)
C
C  Remarks:
C  1. The sums X+Y, X+Z and Y+Z must be greater than or equal to five
C     times the smallest positive machine number.  In addition, each
C     argument must be less than or equal to one-fifth the largest
C     machine number.
C
C  2. See the references:
C     Carlson, B. C. and Notis, E., M., (1981), Algorithms for
C       incomplete elliptic integrals, ACM Transactions on Mathematical
C       Software, 7, pages 398-403.
C     Carlson, B. C., (1979), Computing elliptic integrals by
C       duplication, Numeriche Mathematic, 33, pages 1-16.
C
C  GAMS:       C14
C
C  Chapter:    SFUN/LIBRARY Elliptic Integrals
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION DELRF (X, Y, Z)
C                                  SPECIFICATIONS FOR ARGUMENTS
      DOUBLE PRECISION X, Y, Z
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      DOUBLE PRECISION ALAMDA, AMU, ARGMAX, ARGMIN, C1, C2, C3, E2,
     &           E3, EPSLON, ERRTOL, S, XN, XNDEV, XNROOT, YN, YNDEV,
     &           YNROOT, ZN, ZNDEV, ZNROOT
      SAVE       ARGMIN, ARGMAX
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  DABS,DMAX1,DMIN1,DSQRT
      INTRINSIC  DABS, DMAX1, DMIN1, DSQRT
      DOUBLE PRECISION DABS, DMAX1, DMIN1, DSQRT
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1PSH, E1STD
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   DMACH, N1RTY
      INTEGER    N1RTY
      DOUBLE PRECISION DMACH
C
      CALL E1PSH ('DELRF ')
      DELRF = DMACH(2)
C                                  INITIALIZE UPPER AND LOWER LIMITS
      ARGMIN = 5.0D0*DMACH(1)
      ARGMAX = DMACH(2)/5.0D0
C
      IF (DMIN1(X,Y,Z) .LT. 0.0D0) THEN
         CALL E1STD (1, X)
         CALL E1STD (2, Y)
         CALL E1STD (3, Z)
         CALL E1MES (5, 1, 'At least one of the input arguments, '//
     &               'X = %(D1), Y = %(D2), Z = %(D3), is negative.  '//
     &               'They must all be nonnegative.')
         GO TO 9000
      END IF
C
      IF (DMIN1(X+Y,X+Z,Y+Z) .LT. ARGMIN) THEN
         CALL E1STD (1, X+Y)
         CALL E1STD (2, X+Z)
         CALL E1STD (3, Y+Z)
         CALL E1STD (4, ARGMIN)
         CALL E1MES (5, 2, 'At least one of the sums, X+Y = %(D1), '//
     &               'X+Z = %(D2), Y+Z = %(D3), is less than '//
     &               'ARGMIN = %(D4).')
      END IF
C
      IF (DMAX1(X,Y,Z) .GT. ARGMAX) THEN
         CALL E1STD (1, X)
         CALL E1STD (2, Y)
         CALL E1STD (3, Z)
         CALL E1STD (4, ARGMAX)
         CALL E1MES (5, 3, 'At least one of the arguments, '//
     &               'X = %(D1), Y = %(D2), Z = %(D3), is '//
     &               'greater than ARGMAX = %(D4).')
      END IF
      IF (N1RTY(0) .GT. 0) GO TO 9000
C
      ERRTOL = 1.0D-3
      XN = X
      YN = Y
      ZN = Z
C                                  BEGIN LOOP
   10 AMU = (XN+YN+ZN)/3.0D0
      XNDEV = 2.0D0 - (AMU+XN)/AMU
      YNDEV = 2.0D0 - (AMU+YN)/AMU
      ZNDEV = 2.0D0 - (AMU+ZN)/AMU
      EPSLON = DMAX1(DABS(XNDEV),DABS(YNDEV),DABS(ZNDEV))
      IF (EPSLON .LT. ERRTOL) GO TO 20
      XNROOT = DSQRT(XN)
      YNROOT = DSQRT(YN)
      ZNROOT = DSQRT(ZN)
      ALAMDA = XNROOT*(YNROOT+ZNROOT) + YNROOT*ZNROOT
      XN = (XN+ALAMDA)*0.25D0
      YN = (YN+ALAMDA)*0.25D0
      ZN = (ZN+ALAMDA)*0.25D0
      GO TO 10
C                                  FINAL CALCULATION
   20 C1 = 1.0D0/24.0D0
      C2 = 3.0D0/44.0D0
      C3 = 1.0D0/14.0D0
      E2 = XNDEV*YNDEV - ZNDEV*ZNDEV
      E3 = XNDEV*YNDEV*ZNDEV
      S = 1.0D0 + (C1*E2-0.1D0-C2*E3)*E2 + C3*E3
      DELRF = S/DSQRT(AMU)
C
 9000 CALL E1POP ('DELRF ')
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  ELRJ/DELRJ (Single/Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    February 20, 1986
C
C  Purpose:    Evaluate Carlson's incomplete elliptic integral of the
C              third kind RJ(X,Y,Z,RHO)
C
C  Usage:      ELRJ(X, Y, Z, RHO)
C
C  Arguments:
C     X      - First variable of the incomplete elliptic integral.
C              (Input)
C              It must be nonnegative and satisfy the conditions given
C              in Remark 1.
C     Y      - Second variable of the incomplete elliptic integral.
C              (Input)
C              It must be nonnegative and satisfy the conditions given
C              in Remark 1.
C     Z      - Third variable of the incomplete elliptic integral.
C              (Input)
C              It must be nonnegative and satisfy the conditions given
C              in Remark 1.
C     RHO    - Fourth variable of the incomplete elliptic integral.
C              (Input)
C              It must be positive and satisfy the conditions given
C              in Remark 1.
C     ELRJ   - Function value.  (Output)
C
C  Remarks:
C  1. The sums X+Y, X+Z, Y+Z and RHO must be greater than or equal to
C     ARGMIN and less than or equal to ARGMAX, where
C       ARGMIN = (5*mmin)**(1/3)
C       ARGMAX = 0.3*(mmax/5)**(1/3)
C     with mmin the machine minimum and mmax the machine maximum.
C
C  2. See the references:
C     Carlson, B. C. and Notis, E., M., (1981), Algorithms for
C       incomplete elliptic integrals, ACM Transactions on Mathematical
C       Software, 7, pages 398-403.
C     Carlson, B. C., (1979), Computing elliptic integrals by
C       duplication, Numeriche Mathematic, 33, pages 1-16.
C
C  GAMS:       C14
C
C  Chapter:    SFUN/LIBRARY Elliptic Integrals
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION DELRJ (X, Y, Z, RHO)
C                                  SPECIFICATIONS FOR ARGUMENTS
      DOUBLE PRECISION X, Y, Z, RHO
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      DOUBLE PRECISION ALAMDA, ALFA, AMU, ARGMAX, ARGMIN, BETA, C1,
     &           C2, C3, C4, E2, E3, EA, EB, EC, EPSLON, ERRTOL, PN,
     &           PNDEV, POWER4, RC, S1, S2, S3, SIGMA, XN, XNDEV,
     &           XNROOT, YN, YNDEV, YNROOT, ZN, ZNDEV, ZNROOT
      SAVE       ARGMIN, ARGMAX
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  DABS,DMAX1,DMIN1,DSQRT
      INTRINSIC  DABS, DMAX1, DMIN1, DSQRT
      DOUBLE PRECISION DABS, DMAX1, DMIN1, DSQRT
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1PSH, E1STD
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   DMACH, N1RTY, DELRC
      INTEGER    N1RTY
      DOUBLE PRECISION DMACH, DELRC
C                                  SPECIFICATIONS FOR FUNCTIONS
C
      CALL E1PSH ('DELRJ ')
      DELRJ = DMACH(2)
C                                  INITIALIZE UPPER AND LOWER LIMITS
      ARGMIN = (5.0D0*DMACH(1))**(1.0D0/3.0D0)
      ARGMAX = ((DMACH(2)/5.0D0)**(1.0D0/3.0D0))*0.3D0
C
      IF (DMIN1(X,Y,Z) .LT. 0.0D0) THEN
         CALL E1STD (1, X)
         CALL E1STD (2, Y)
         CALL E1STD (3, Z)
         CALL E1MES (5, 1, 'At least one of the input arguments, '//
     &               'X = %(D1), Y = %(D2), Z = %(D3), is negative.  '//
     &               'They must all be nonnegative.')
      END IF
C
      IF (RHO .LE. 0.0D0) THEN
         CALL E1STD (1, RHO)
         CALL E1MES (5, 2, 'The argument of the incomplete '//
     &               'elliptic integral, RHO = %(D1), must be '//
     &               'positive.')
      END IF
      IF (N1RTY(0) .GT. 0) GO TO 9000
C
      IF (DMIN1(X+Y,X+Z,Y+Z,RHO) .LT. ARGMIN) THEN
         CALL E1STD (1, X+Y)
         CALL E1STD (2, X+Z)
         CALL E1STD (3, Y+Z)
         CALL E1STD (4, RHO)
         CALL E1STD (5, ARGMIN)
         CALL E1MES (5, 3, 'At least one of the sums, X+Y = %(D1), '//
     &               'X+Z = %(D2), Y+Z = %(D3), or the fourth '//
     &               'argument, RHO = %(D4), is less than ARGMIN = '//
     &               '%(D5).')
      END IF
C
      IF (DMAX1(X,Y,Z,RHO) .GT. ARGMAX) THEN
         CALL E1STD (1, X)
         CALL E1STD (2, Y)
         CALL E1STD (3, Z)
         CALL E1STD (4, RHO)
         CALL E1STD (5, ARGMAX)
         CALL E1MES (5, 4, 'At least one of the arguments, '//
     &               'X = %(D1), Y = %(D2), Z = %(D3), RHO = %(D4) '//
     &               'is greater than ARGMAX = %(D5).')
      END IF
      IF (N1RTY(0) .GT. 0) GO TO 9000
C
      ERRTOL = 1.0D-3
      XN = X
      YN = Y
      ZN = Z
C                                  BEGIN LOOP
C
      PN = RHO
      SIGMA = 0.0D0
      POWER4 = 1.0D0
C
   10 AMU = (XN+YN+ZN+PN+PN)*0.2D0
      XNDEV = (AMU-XN)/AMU
      YNDEV = (AMU-YN)/AMU
      ZNDEV = (AMU-ZN)/AMU
      PNDEV = (AMU-PN)/AMU
      EPSLON = DMAX1(DABS(XNDEV),DABS(YNDEV),DABS(ZNDEV),DABS(PNDEV))
      IF (EPSLON .GE. ERRTOL) THEN
         XNROOT = DSQRT(XN)
         YNROOT = DSQRT(YN)
         ZNROOT = DSQRT(ZN)
         ALAMDA = XNROOT*(YNROOT+ZNROOT) + YNROOT*ZNROOT
         ALFA = PN*(XNROOT+YNROOT+ZNROOT) + XNROOT*YNROOT*ZNROOT
         ALFA = ALFA*ALFA
         BETA = PN*(PN+ALAMDA)*(PN+ALAMDA)
         RC = DELRC(ALFA,BETA)
         SIGMA = SIGMA + POWER4*RC
         POWER4 = POWER4*0.25D0
         XN = (XN+ALAMDA)*0.25D0
         YN = (YN+ALAMDA)*0.25D0
         ZN = (ZN+ALAMDA)*0.25D0
         PN = (PN+ALAMDA)*0.25D0
         GO TO 10
      END IF
C
      C1 = 3.0D0/14.0D0
      C2 = 1.0D0/3.0D0
      C3 = 3.0D0/22.0D0
      C4 = 3.0D0/26.0D0
      EA = XNDEV*(YNDEV+ZNDEV) + YNDEV*ZNDEV
      EB = XNDEV*YNDEV*ZNDEV
      EC = PNDEV*PNDEV
      E2 = EA - 3.0D0*EC
      E3 = EB + 2.0D0*PNDEV*(EA-EC)
      S1 = 1.0D0 + E2*(-C1+0.75D0*C3*E2-1.5D0*C4*E3)
      S2 = EB*(0.5D0*C2+PNDEV*(-C3-C3+PNDEV*C4))
      S3 = PNDEV*EA*(C2-PNDEV*C3) - C2*PNDEV*EC
      DELRJ = 3.0D0*SIGMA + POWER4*(S1+S2+S3)/(AMU*DSQRT(AMU))
C
 9000 CALL E1POP ('DELRJ ')
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  L2ARG/DL2ARG (Single/Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    February 27, 1985
C
C  Purpose:    Solve a real general system of linear equations with
C              iterative refinement.
C
C  Usage:      CALL L2ARG (N, A, LDA, B, IPATH, X, FAC, IPVT, WK)
C
C  Arguments:  See LSARG/DLSARG.
C
C  Remarks:    See LSARG/DLSARG.
C
C  Chapter:    MATH/LIBRARY Linear Systems
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DL2ARG (N, A, LDA, B, IPATH, X, FAC, IPVT, WK)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, LDA, IPATH, IPVT(*)
      DOUBLE PRECISION A(LDA,*), B(*), X(*), FAC(*), WK(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      DOUBLE PRECISION RCOND
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1PSH, E1STI, E1STD, DL2CRG, DLFIRG
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   DMACH, N1RCD, N1RTY
      INTEGER    N1RCD, N1RTY
      DOUBLE PRECISION DMACH
C
      CALL E1PSH ('DL2ARG ')
C
      IF (N .LE. 0) THEN
         CALL E1STI (1, N)
         CALL E1MES (5, 1, 'The number of equations must be '//
     &               'positive while N = %(I1) is given.')
         GO TO 9000
      END IF
C
      IF (N .GT. LDA) THEN
         CALL E1STI (1, N)
         CALL E1STI (2, LDA)
         CALL E1MES (5, 2, 'The order of the matrix must be '//
     &               'less than or equal to its leading dimension '//
     &               'while N = %(I1) and LDA = %(I2) are given.')
         GO TO 9000
      END IF
C
      IF (IPATH.NE.1 .AND. IPATH.NE.2) THEN
         CALL E1STI (1, IPATH)
         CALL E1MES (5, 3, 'IPATH must be either 1 or 2 while '//
     &               'a value of %(I1) is given.')
         GO TO 9000
      END IF
C                                  FACTOR AND ESTIMATE RECIPROCAL OF
C                                  CONDITION NUMBER OF A
      CALL DL2CRG (N, A, LDA, FAC, N, IPVT, RCOND, WK)
      IF (N1RTY(1) .EQ. 4) GO TO 9000
C                                  ITERATIVE REFINEMENT STEP
      CALL DLFIRG (N, A, LDA, FAC, N, IPVT, B, IPATH, X, WK)
      IF (N1RCD(1).NE.0 .OR. RCOND.LE.DMACH(4)) THEN
         CALL E1STD (1, RCOND)
         CALL E1MES (3, 1, 'The matrix is too ill-conditioned. '//
     &               'An estimate of the reciprocal of its L1 '//
     &               'condition number is RCOND = %(D1).  '//
     &               'The solution might not be accurate.')
      END IF
C
 9000 CALL E1POP ('DL2ARG ')
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  L2CRG/DL2CRG (Single/Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    February 27, 1985
C
C  Purpose:    Compute the LU factorization of a real general matrix and
C              estimate its L1 condition number.
C
C  Usage:      CALL L2CRG (N, A, LDA, FAC, LDFAC, IPVT, RCOND, Z)
C
C  Arguments:  See LFCRG/DLFCRG.
C
C  Remarks:    See LFCRG/DLFCRG.
C
C  Chapter:    MATH/LIBRARY Linear Systems
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DL2CRG (N, A, LDA, FAC, LDFAC, IPVT, RCOND, Z)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, LDA, LDFAC, IPVT(*)
      DOUBLE PRECISION RCOND, A(LDA,*), FAC(LDFAC,*), Z(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    J, K, KP1, L
      DOUBLE PRECISION ANORM, EK, S, SM, T, WK, WKM, YNORM
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  DABS,DSIGN
      INTRINSIC  DABS, DSIGN
      DOUBLE PRECISION DABS, DSIGN
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1PSH, E1STI, E1STD, DAXPY, DSCAL,
     &           DSET, DL2TRG, DNR1RR
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   DMACH, N1RTY, DASUM, DDOT
      INTEGER    N1RTY
      DOUBLE PRECISION DMACH, DASUM, DDOT
C
      CALL E1PSH ('DL2CRG ')
C
      IF (N .LE. 0) THEN
         CALL E1STI (1, N)
         CALL E1MES (5, 1, 'The order of the matrix must be '//
     &               'positive while N = %(I1) is given.')
         GO TO 9000
      END IF
C
      IF (N .GT. LDA) THEN
         CALL E1STI (1, N)
         CALL E1STI (2, LDA)
         CALL E1MES (5, 2, 'The order of the matrix must be '//
     &               'less than or equal to its leading dimension '//
     &               'while N = %(I1) and LDA = %(I2) are given.')
         GO TO 9000
      END IF
C
      IF (N .GT. LDFAC) THEN
         CALL E1STI (1, N)
         CALL E1STI (2, LDFAC)
         CALL E1MES (5, 3, 'The order of the matrix must be '//
     &               'less than or equal to its leading dimension '//
     &               'while N = %(I1) and LDFAC = %(I2) are given.')
         GO TO 9000
      END IF
C
      RCOND = 0.0D0
C                                  COMPUTE 1-NORM OF A
      CALL DNR1RR (N, N, A, LDA, ANORM)
C                                  FACTORIZATION STEP
C
      CALL DL2TRG (N, A, LDA, FAC, LDFAC, IPVT, Z)
      IF (N1RTY(1) .EQ. 4) GO TO 9000
C                                  RCOND = 1/(NORM(A)*(ESTIMATE OF
C                                  NORM(INVERSE(A)))). ESTIMATE =
C                                  NORM(Z)/NORM(Y) WHERE A*Z = Y AND
C                                  TRANS(A)*Y = E . TRANS(A) IS THE
C                                  TRANSPOSE OF A. THE COMPONENTS OF
C                                  E ARE CHOSEN TO CAUSE MAXIMUM LO-
C                                  CAL GROWTH IN THE ELEMENTS OF W
C                                  WHERE TRANS(U)*W = E. THE VECTORS
C                                  ARE FREQUENTLY RESCALED TO AVOID
C                                  OVERFLOW. SOLVE TRANS(U)*W = E
      EK = 1.0D0
      CALL DSET (N, 0.0D0, Z, 1)
      DO 20  K=1, N
         IF (Z(K) .NE. 0.0D0) EK = DSIGN(EK,-Z(K))
         IF (DABS(EK-Z(K)) .GT. DABS(FAC(K,K))) THEN
            S = DABS(FAC(K,K))/DABS(EK-Z(K))
            CALL DSCAL (N, S, Z, 1)
            EK = S*EK
         END IF
         WK = EK - Z(K)
         WKM = -EK - Z(K)
         S = DABS(WK)
         SM = DABS(WKM)
         IF (FAC(K,K) .NE. 0.0D0) THEN
            WK = WK/FAC(K,K)
            WKM = WKM/FAC(K,K)
         ELSE
            WK = 1.0D0
            WKM = 1.0D0
         END IF
         KP1 = K + 1
         IF (KP1 .LE. N) THEN
            DO 10  J=KP1, N
               SM = SM + DABS(Z(J)+WKM*FAC(K,J))
               Z(J) = Z(J) + WK*FAC(K,J)
               S = S + DABS(Z(J))
   10       CONTINUE
            IF (S .LT. SM) THEN
               T = WKM - WK
               WK = WKM
               CALL DAXPY (N-K, T, FAC(K,KP1), LDFAC, Z(KP1), 1)
            END IF
         END IF
         Z(K) = WK
   20 CONTINUE
      S = 1.0D0/DASUM(N,Z,1)
      CALL DSCAL (N, S, Z, 1)
C                                  SOLVE TRANS(L)*Y = W
      DO 30  K=N, 1, -1
         IF (K .LT. N) Z(K) = Z(K) + DDOT(N-K,FAC(K+1,K),1,Z(K+1),1)
         IF (DABS(Z(K)) .GT. 1.0D0) THEN
            S = 1.0D0/DABS(Z(K))
            CALL DSCAL (N, S, Z, 1)
         END IF
         L = IPVT(K)
         T = Z(L)
         Z(L) = Z(K)
         Z(K) = T
   30 CONTINUE
      S = 1.0D0/DASUM(N,Z,1)
      CALL DSCAL (N, S, Z, 1)
C
      YNORM = 1.0D0
C                                  SOLVE L*V = Y
      DO 40  K=1, N
         L = IPVT(K)
         T = Z(L)
         Z(L) = Z(K)
         Z(K) = T
         IF (K .LT. N) CALL DAXPY (N-K, T, FAC(K+1,K), 1, Z(K+1), 1)
         IF (DABS(Z(K)) .GT. 1.0D0) THEN
            S = 1.0D0/DABS(Z(K))
            CALL DSCAL (N, S, Z, 1)
            YNORM = S*YNORM
         END IF
   40 CONTINUE
      S = 1.0D0/DASUM(N,Z,1)
      CALL DSCAL (N, S, Z, 1)
      YNORM = S*YNORM
C                                  SOLVE U*Z = V
      DO 50  K=N, 1, -1
         IF (DABS(Z(K)) .GT. DABS(FAC(K,K))) THEN
            S = DABS(FAC(K,K))/DABS(Z(K))
            CALL DSCAL (N, S, Z, 1)
            YNORM = S*YNORM
         END IF
         IF (FAC(K,K) .NE. 0.0D0) THEN
            Z(K) = Z(K)/FAC(K,K)
         ELSE
            Z(K) = 1.0D0
         END IF
         T = -Z(K)
         CALL DAXPY (K-1, T, FAC(1,K), 1, Z(1), 1)
   50 CONTINUE
C                                  MAKE ZNORM = 1.0
      S = 1.0D0/DASUM(N,Z,1)
      CALL DSCAL (N, S, Z, 1)
      YNORM = S*YNORM
      IF (ANORM .NE. 0.0D0) RCOND = YNORM/ANORM
C
      IF (RCOND .LE. DMACH(4)) THEN
         CALL E1STD (1, RCOND)
         CALL E1MES (3, 1, 'The matrix is algorithmically '//
     &               'singular.  An estimate of the reciprocal '//
     &               'of its L1 condition number is RCOND = %(D1).')
      END IF
C
 9000 CALL E1POP ('DL2CRG ')
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  L2TRG/DL2TRG (Single/Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    April 28, 1989
C
C  Purpose:    Compute the LU factorization of a real general matrix.
C
C  Usage:      CALL L2TRG (N, A, LDA, FAC, LDFAC, IPVT, SCALE)
C
C  Arguments:  See LFTRG/DLFTRG.
C
C  Remarks:    See LFTRG/DLFTRG.
C
C  Chapter:    MATH/LIBRARY Linear Systems
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DL2TRG (N, A, LDA, FAC, LDFAC, IPVT, SCALE)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, LDA, LDFAC, IPVT(*)
      DOUBLE PRECISION A(LDA,*), FAC(LDFAC,*), SCALE(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, INDJ, INFO, K, L
      DOUBLE PRECISION BIG, CURMAX, SMALL, T, VALUE
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  DABS
      INTRINSIC  DABS
      DOUBLE PRECISION DABS
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1PSH, E1STI, DGER, DSCAL, DSWAP, DCRGRG
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   DMACH, IDAMAX
      INTEGER    IDAMAX
      DOUBLE PRECISION DMACH
C
      CALL E1PSH ('DL2TRG ')
C
      IF (N .LE. 0) THEN
         CALL E1STI (1, N)
         CALL E1MES (5, 1, 'The order of the matrix must be '//
     &               'positive while N = %(I1) is given.')
         GO TO 9000
      END IF
C
      IF (N .GT. LDA) THEN
         CALL E1STI (1, N)
         CALL E1STI (2, LDA)
         CALL E1MES (5, 2, 'The order of the matrix must be '//
     &               'less than or equal to its leading dimension '//
     &               'while N = %(I1) and LDA = %(I2) are given.')
         GO TO 9000
      END IF
C
      IF (N .GT. LDFAC) THEN
         CALL E1STI (1, N)
         CALL E1STI (2, LDFAC)
         CALL E1MES (5, 3, 'The order of the matrix must be '//
     &               'less than or equal to its leading dimension '//
     &               'while N = %(I1) and LDFAC = %(I2) are given.')
         GO TO 9000
      END IF
C                                  Preserve a copy of the input matrix
      CALL DCRGRG (N, A, LDA, FAC, LDFAC)
C                                  Compute the infinity norm of each row
C                                  of A for scaling purpose
      DO 10  I=1, N
         INDJ = IDAMAX(N,FAC(I,1),LDFAC)
         SCALE(I) = DABS(FAC(I,INDJ))
   10 CONTINUE
C                                  Gaussian elimination with scaled
C                                  partial pivoting
      INFO = 0
      SMALL = DMACH(1)
      BIG = DMACH(2)
      IF (SMALL*BIG .LT. 1.0D0) SMALL = 1.0D0/BIG
      DO 30  K=1, N - 1
C                                  Find L = pivot index
         L = K
         CURMAX = 0.0D0
         DO 20  I=K, N
            IF (SCALE(I) .GE. SMALL) THEN
               VALUE = DABS(FAC(I,K))/SCALE(I)
            ELSE
               VALUE = DABS(FAC(I,K))
            END IF
            IF (VALUE .GT. CURMAX) THEN
               CURMAX = VALUE
               L = I
            END IF
   20    CONTINUE
         IPVT(K) = L
C                                  Zero pivot implies this column
C                                  already triangularized
         IF (FAC(L,K) .NE. 0.0D0) THEN
C                                  Interchange if necessary
            IF (L .NE. K) THEN
               T = FAC(L,K)
               FAC(L,K) = FAC(K,K)
               FAC(K,K) = T
               SCALE(L) = SCALE(K)
            END IF
C                                  Compute multipliers
            IF (DABS(FAC(K,K)) .GT. SMALL) THEN
               CALL DSCAL (N-K, -1.0D0/FAC(K,K), FAC(K+1,K), 1)
            END IF
C                                  Row elimination with column indexing
            CALL DSWAP (N-K, FAC(K,K+1), LDFAC, FAC(L,K+1), LDFAC)
            CALL DGER (N-K, N-K, 1.0D0, FAC(K+1,K), 1, FAC(K,K+1),
     &                 LDFAC, FAC(K+1,K+1), LDFAC)
         ELSE
            INFO = K
         END IF
   30 CONTINUE
      IPVT(N) = N
      IF (DABS(FAC(N,N)) .LE. SMALL) INFO = N
C
      IF (INFO .NE. 0) THEN
         CALL E1MES (4, 2, 'The input matrix is singular.  '//
     &               'Some of the diagonal elements of the upper '//
     &               'triangular matrix U of the LU factorization '//
     &               'are close to zero.')
      END IF
C
 9000 CALL E1POP ('DL2TRG ')
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  LFIRG/DLFIRG (Single/Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    April 9, 1987
C
C  Purpose:    Use iterative refinement to improve the solution of a
C              real general system of linear equations.
C
C  Usage:      CALL LFIRG (N, A, LDA, FAC, LDFAC, IPVT, B, IPATH,
C                          X, RES)
C
C  Arguments:
C     N      - Number of equations.  (Input)
C     A      - N by N matrix containing the coefficient matrix of the
C              linear system.  (Input)
C     LDA    - Leading dimension of A exactly as specified in the
C              dimension statement of the calling program.  (Input)
C     FAC    - N by N matrix containing the LU factorization of the
C              coefficient matrix A as output from subroutine
C              LFCRG/DLFCRG or LFTRG/DLFTRG.  (Input)
C     LDFAC  - Leading dimension of FAC exactly as specified in the
C              dimension statement of the calling program.  (Input)
C     IPVT   - Vector of length N containing the pivoting information
C              for the LU factorization of A as output from subroutine
C              LFCRG/DLFCRG or LFTRG/DLFTRG.  (Input)
C     B      - Vector of length N containing the right-hand side of the
C              linear system.  (Input)
C     IPATH  - Path indicator.  (Input)
C              IPATH = 1 means the system A*X = B is solved.
C              IPATH = 2 means the system trans(A)*X = B is solved,
C                        where trans(A) is the transpose of A.
C     X      - Vector of length N containing the solution to the linear
C              system.  (Output)
C     RES    - Vector of length N containing the residual vector at the
C              improved solution.  (Output)
C
C  Remark:
C     Informational error
C     Type Code
C       3   2  The input matrix is too ill-conditioned for iterative
C              refinement to be effective.
C
C  GAMS:       D2a1
C
C  Chapter:    MATH/LIBRARY Linear Systems
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DLFIRG (N, A, LDA, FAC, LDFAC, IPVT, B, IPATH, X, RES)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, LDA, LDFAC, IPATH, IPVT(*)
      DOUBLE PRECISION A(LDA,*), FAC(LDFAC,*), B(*), X(*), RES(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, ITER
      DOUBLE PRECISION EPS, RATIO, RATIOP, RNORM, XNORM
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  DABS
      INTRINSIC  DABS
      DOUBLE PRECISION DABS
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1PSH, E1STI, DAXPY, DLFSRG
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   DMACH, IDAMAX, N1RCD, DQDDOT
      INTEGER    IDAMAX, N1RCD
      DOUBLE PRECISION DMACH, DQDDOT
C
      CALL E1PSH ('DLFIRG ')
C
      IF (N .LE. 0) THEN
         CALL E1STI (1, N)
         CALL E1MES (5, 1, 'The number of equations must be '//
     &               'positive while N = %(I1) is given.')
         GO TO 9000
      END IF
C
      IF (N .GT. LDA) THEN
         CALL E1STI (1, N)
         CALL E1STI (2, LDA)
         CALL E1MES (5, 2, 'The order of the matrix must be '//
     &               'less than or equal to its leading dimension '//
     &               'while N = %(I1) and LDA = %(I2) are given.')
         GO TO 9000
      END IF
C
      IF (N .GT. LDFAC) THEN
         CALL E1STI (1, N)
         CALL E1STI (2, LDFAC)
         CALL E1MES (5, 3, 'The order of the matrix must be '//
     &               'less than or equal to its leading dimension '//
     &               'while N = %(I1) and LDFAC = %(I2) are given.')
         GO TO 9000
      END IF
C
      IF (IPATH.NE.1 .AND. IPATH.NE.2) THEN
         CALL E1STI (1, IPATH)
         CALL E1MES (5, 4, 'IPATH must be either 1 or 2 while '//
     &               'a value of %(I1) is given.')
         GO TO 9000
      END IF
C
      EPS = DMACH(4)
      RATIOP = DMACH(2)
C                                  SOLVE THE LINEAR SYSTEM.
      CALL DLFSRG (N, FAC, LDFAC, IPVT, B, IPATH, X)
      XNORM = DABS(X(IDAMAX(N,X,1)))
      IF (XNORM .EQ. 0.0D0) GO TO 9000
      DO 30  ITER=1, 50
         IF (IPATH .EQ. 1) THEN
            DO 10  I=1, N
               RES(I) = DQDDOT(N,-B(I),A(I,1),LDA,X,1)
   10       CONTINUE
         ELSE IF (IPATH .EQ. 2) THEN
            DO 20  I=1, N
               RES(I) = DQDDOT(N,-B(I),A(1,I),1,X,1)
   20       CONTINUE
         END IF
         CALL DLFSRG (N, FAC, LDFAC, IPVT, RES, IPATH, RES)
         IF (N1RCD(1) .NE. 0) GO TO 9000
         CALL DAXPY (N, -1.0D0, RES, 1, X, 1)
         XNORM = DABS(X(IDAMAX(N,X,1)))
         RNORM = DABS(RES(IDAMAX(N,RES,1)))
C                                  CHECK FOR ERRATIC BEHAVIOR OF
C                                  RESIDUALS IN CASE OF
C                                  ILL-CONDITIONING.
         RATIO = RNORM/XNORM
         IF (RATIO .GE. RATIOP) GO TO 40
         IF (RATIO .LE. EPS) GO TO 9000
         RATIOP = RATIO
   30 CONTINUE
C
   40 CALL E1MES (3, 2, 'The matrix is too ill-conditioned '//
     &            'for iterative refinement to be effective.')
C
 9000 CALL E1POP ('DLFIRG ')
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  LFSRG/DLFSRG (Single/Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    February 27, 1985
C
C  Purpose:    Solve a real general system of linear equations given the
C              LU factorization of the coefficient matrix.
C
C  Usage:      CALL LFSRG (N, FAC, LDFAC, IPVT, B, IPATH, X)
C
C  Arguments:
C     N      - Number of equations.  (Input)
C     FAC    - N by N matrix containing the LU factorization of the
C              coefficient matrix A as output from subroutine
C              LFCRG/DLFCRG or LFTRG/DLFTRG.  (Input)
C     LDFAC  - Leading dimension of FAC exactly as specified in the
C              dimension statement of the calling program.  (Input)
C     IPVT   - Vector of length N containing the pivoting information
C              for the LU factorization of A as output from subroutine
C              LFCRG/DLFCRG or LFTRG/DLFTRG.  (Input)
C     B      - Vector of length N containing the right-hand side of the
C              linear system.  (Input)
C     IPATH  - Path indicator.  (Input)
C              IPATH = 1 means the system A*X = B is solved.
C              IPATH = 2 means the system trans(A)*X = B is solved where
C                        trans(A) is the transpose of A.
C     X      - Vector of length N containing the solution to the linear
C              system.  (Output)
C              If B is not needed, B and X can share the same storage
C              locations.
C
C  GAMS:       D2a1
C
C  Chapters:   MATH/LIBRARY Linear Systems
C              STAT/LIBRARY Mathematical Support
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DLFSRG (N, FAC, LDFAC, IPVT, B, IPATH, X)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, LDFAC, IPATH, IPVT(*)
      DOUBLE PRECISION FAC(LDFAC,*), B(*), X(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    K, L
      DOUBLE PRECISION BIG, SMALL, T
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  DABS
      INTRINSIC  DABS
      DOUBLE PRECISION DABS
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1PSH, E1STI, DAXPY, DCOPY, DTRSV
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   DMACH, DDOT
      DOUBLE PRECISION DMACH, DDOT
C
      CALL E1PSH ('DLFSRG ')
C
      IF (N .LE. 0) THEN
         CALL E1STI (1, N)
         CALL E1MES (5, 1, 'The number of equations must be '//
     &               'positive while N = %(I1) is given.')
         GO TO 9000
      END IF
C
      IF (N .GT. LDFAC) THEN
         CALL E1STI (1, N)
         CALL E1STI (2, LDFAC)
         CALL E1MES (5, 2, 'The order of the matrix must be '//
     &               'less than or equal to its leading dimension '//
     &               'while N = %(I1) and LDFAC = %(I2) are given.')
         GO TO 9000
      END IF
C                                  COPY B INTO X AND USE X TO PRESERVE
C                                  INPUT
      CALL DCOPY (N, B, 1, X, 1)
C
      SMALL = DMACH(1)
      BIG = DMACH(2)
      IF (SMALL*BIG .LT. 1.0D0) SMALL = 1.0D0/BIG
      IF (IPATH .EQ. 1) THEN
C                                  IPATH = 1 , SOLVE  A * X = B FIRST
C                                  SOLVE  L*Y = B
         DO 10  K=1, N - 1
            L = IPVT(K)
            T = X(L)
            IF (L .NE. K) THEN
               X(L) = X(K)
               X(K) = T
            END IF
            CALL DAXPY (N-K, T, FAC(K+1,K), 1, X(K+1), 1)
   10    CONTINUE
C                                  NOW SOLVE  U*X = Y
         DO 20  K=N, 1, -1
            IF (DABS(FAC(K,K)) .LE. SMALL) THEN
               CALL E1MES (5, 3, 'The input matrix is singular.  '//
     &                     'Some of the diagonal elements '//
     &                     'of the upper triangular matrix U of the '//
     &                     'LU factorization are close to zero.')
               GO TO 9000
            END IF
   20    CONTINUE
         CALL DTRSV ('U', 'N', 'N', N, FAC, LDFAC, X, 1)
C
      ELSE IF (IPATH .EQ. 2) THEN
C                                  IPATH = 2, SOLVE  TRANS(A) * X = B
C                                  FIRST SOLVE  TRANS(U)*Y = B
         DO 30  K=1, N
            IF (DABS(FAC(K,K)) .LE. SMALL) THEN
               CALL E1MES (5, 4, 'The input matrix is singular.  '//
     &                     'Some of the diagonal elements '//
     &                     'of the upper triangular matrix U of the '//
     &                     'LU factorization are close to zero.')
               GO TO 9000
            END IF
   30    CONTINUE
         CALL DTRSV ('U', 'T', 'N', N, FAC, LDFAC, X, 1)
C                                  NOW SOLVE TRANS(L)*X = Y
         DO 40  K=N - 1, 1, -1
            X(K) = X(K) + DDOT(N-K,FAC(K+1,K),1,X(K+1),1)
            L = IPVT(K)
            IF (L .NE. K) THEN
               T = X(L)
               X(L) = X(K)
               X(K) = T
            END IF
   40    CONTINUE
C
      ELSE
         CALL E1STI (1, IPATH)
         CALL E1MES (5, 5, 'IPATH must be either 1 or 2 while '//
     &               'a value of %(I1) is given.')
      END IF
C
 9000 CALL E1POP ('DLFSRG ')
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  LSARG/DLSARG (Single/Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    February 27, 1985
C
C  Purpose:    Solve a real general system of linear equations with
C              iterative refinement.
C
C  Usage:      CALL LSARG (N, A, LDA, B, IPATH, X)
C
C  Arguments:
C     N      - Number of equations.  (Input)
C     A      - N by N matrix containing the coefficients of the linear
C              system.  (Input)
C     LDA    - Leading dimension of A exactly as specified in the
C              dimension statement of the calling program.  (Input)
C     B      - Vector of length N containing the right-hand side of
C              the linear system.  (Input)
C     IPATH  - Path indicator.  (Input)
C              IPATH = 1 means the system A*X = B is solved.
C              IPATH = 2 means the system trans(A)*X = B is solved where
C                        trans(A) is the transpose of A.
C     X      - Vector of length N containing the solution to the linear
C              system.  (Output)
C
C  Remarks:
C  1. Automatic workspace usage is
C              LSARG     N**2 + 2*N    units, or
C              DLSARG    2*N**2 + 3*N  units.
C     Workspace may be explicitly provided, if desired, by use of
C     L2ARG/DL2ARG.  The reference is
C              CALL L2ARG (N, A, LDA, B, IPATH, X, FAC, IPVT, WK)
C     The additional arguments are as follows:
C     FAC    - Work vector of length N**2 containing the LU
C              factorization of A on output.
C     IPVT   - Integer work vector of length N containing the pivoting
C              information for the LU factorization of A on output.
C     WK     - Work vector of length N.
C
C  2. Informational errors
C     Type Code
C       3   1  The input matrix is too ill-conditioned.  The solution
C              might not be accurate.
C       4   2  The input matrix is singular.
C
C  Keywords:   Gaussian elimination; LU factorization
C
C  GAMS:       D2a1
C
C  Chapter:    MATH/LIBRARY Linear Systems
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DLSARG (N, A, LDA, B, IPATH, X)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, LDA, IPATH
      DOUBLE PRECISION A(LDA,*), B(*), X(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    INDFAC, INDPVT, INDW
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                                  SPECIFICATIONS FOR COMMON /WORKSP/
      REAL       RWKSP(5000)
      DOUBLE PRECISION RDWKSP(2500)
      DOUBLE PRECISION DWKSP(2500)
      COMPLEX    CWKSP(2500)
      COMPLEX    *16 CZWKSP(1250)
      COMPLEX    *16 ZWKSP(1250)
      INTEGER    IWKSP(5000)
      LOGICAL    LWKSP(5000)
      EQUIVALENCE (DWKSP(1), RWKSP(1))
      EQUIVALENCE (CWKSP(1), RWKSP(1)), (ZWKSP(1), RWKSP(1))
      EQUIVALENCE (IWKSP(1), RWKSP(1)), (LWKSP(1), RWKSP(1))
      EQUIVALENCE (RDWKSP(1), RWKSP(1)), (CZWKSP(1), RWKSP(1))
      COMMON     /WORKSP/ DWKSP
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1PSH, E1STI, DL2ARG
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   I1KGT, N1RCD
      INTEGER    I1KGT, N1RCD
C
      CALL E1PSH ('DLSARG ')
C
      IF (N .LE. 0) THEN
         CALL E1STI (1, N)
         CALL E1MES (5, 1, 'The number of equations must be '//
     &               'positive while N = %(I1) is given.')
      ELSE
C                                  ALLOCATE WORKSPACE FOR THE FACTOR,
C                                  THE PIVOT INFORMATION AND WORKSPACE
C                                  TO BE USED IN ESTIMATING THE
C                                  CONDITION NUMBER OF A AND IN
C                                  BALANCING A.
         INDFAC = I1KGT(N*N,4)
         INDW = I1KGT(N,4)
         INDPVT = I1KGT(N,2)
         IF (N1RCD(0) .NE. 0) THEN
            CALL E1MES (5, -1, ' ')
            CALL E1STI (1, N)
            CALL E1MES (5, 2, 'The workspace is based on N, '//
     &                  'where N = %(I1).')
         ELSE
            CALL DL2ARG (N, A, LDA, B, IPATH, X, RDWKSP(INDFAC),
     &                   IWKSP(INDPVT), RDWKSP(INDW))
         END IF
      END IF
C
      CALL E1POP ('DLSARG ')
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  DMACH (Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    March 15, 1984
C
C  Purpose:    Retrieve double precision machine constants.
C
C  Usage:      DMACH(N)
C
C  Arguments:
C     N      - Index of desired constant.  (Input)
C     DMACH  - Machine constant.  (Output)
C              DMACH(1) = B**(EMIN-1), the smallest positive magnitude.
C              DMACH(2) = B**EMAX*(1 - B**(-T)), the largest magnitude.
C              DMACH(3) = B**(-T), the smallest relative spacing.
C              DMACH(4) = B**(1-T), the largest relative spacing.
C              DMACH(5) = LOG10(B), the log, base 10, of the radix.
C              DMACH(6) = not-a-number.
C              DMACH(7) = positive machine infinity.
C              DMACH(8) = negative machine infinity.
C
C  GAMS:       R1
C
C  Chapters:   MATH/LIBRARY Reference Material
C              STAT/LIBRARY Reference Material
C              SFUN/LIBRARY Reference Material
C
C  Copyright:  1984 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION DMACH (N)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      DOUBLE PRECISION RMACH(8)
      SAVE       RMACH
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1PSH, E1STI
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    IRMACH(16)
C
      EQUIVALENCE (RMACH, IRMACH)
C                                  DEFINE CONSTANTS
      DATA RMACH(1)/2.22559D-308/
      DATA RMACH(2)/1.79728D+308/
      DATA RMACH(3)/1.11048D-16/
      DATA RMACH(4)/2.22096D-16/
      DATA RMACH(5)/.3010299956639811952137388947245D0/
      DATA IRMACH(11)/2146959360/
      DATA IRMACH(12)/0/
      DATA IRMACH(13)/2146435072/
      DATA IRMACH(14)/0/
      DATA IRMACH(15)/-1048576/
      DATA IRMACH(16)/0/
C
      IF (N.LT.1 .OR. N.GT.8) THEN
         CALL E1PSH ('DMACH ')
         DMACH = RMACH(6)
         CALL E1STI (1, N)
         CALL E1MES (5, 5, 'The argument must be between 1 '//
     &               'and 8 inclusive. N = %(I1)')
         CALL E1POP ('DMACH ')
      ELSE
         DMACH = RMACH(N)
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  NR1RR/DNR1RR (Single/Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    October 17, 1985
C
C  Purpose:    Compute the 1-norm of a real matrix.
C
C  Usage:      CALL NR1RR (NRA, NCA, A, LDA, ANORM)
C
C  Arguments:
C     NRA    - Number of rows of A.  (Input)
C     NCA    - Number of columns of A.  (Input)
C     A      - Real NRA by NCA matrix whose 1-norm is to be computed.
C              (Input)
C     LDA    - Leading dimension of A exactly as specified in the
C              dimension statement of the calling program.  (Input)
C     ANORM  - Real scalar containing the 1-norm of A.  (Output)
C
C  GAMS:       D1b2
C
C  Chapter:    MATH/LIBRARY Basic Matrix/Vector Operations
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DNR1RR (NRA, NCA, A, LDA, ANORM)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    NRA, NCA, LDA
      DOUBLE PRECISION ANORM, A(LDA,*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    J
      DOUBLE PRECISION ANORM1
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  DMAX1
      INTRINSIC  DMAX1
      DOUBLE PRECISION DMAX1
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1PSH, E1STI
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   N1RCD, DASUM
      INTEGER    N1RCD
      DOUBLE PRECISION DASUM
C
      CALL E1PSH ('DNR1RR ')
C                                  CHECK FOR INPUT ERRORS
      IF (NRA .GT. LDA) THEN
         CALL E1STI (1, NRA)
         CALL E1STI (2, LDA)
         CALL E1MES (5, 1, 'The number of rows of the input '//
     &               'matrix must be less than or equal to the '//
     &               'leading dimension while NRA = %(I1) and '//
     &               'LDA = %(I2) are given.')
         GO TO 9000
      END IF
C
      IF (NRA .LE. 0) THEN
         CALL E1STI (1, NRA)
         CALL E1MES (5, 2, 'The number of rows of the input '//
     &               'matrix must be greater than zero while '//
     &               'NRA = %(I1) is given.')
      END IF
C
      IF (NCA .LE. 0) THEN
         CALL E1STI (1, NCA)
         CALL E1MES (5, 3, 'The number of columns of the input '//
     &               'matrix must be greater than zero while '//
     &               'NCA = %(I1) is given.')
      END IF
C
      IF (N1RCD(0) .NE. 0) GO TO 9000
C                                  CALCULATE THE L1 NORM FOR A.
      ANORM = 0.0D0
      DO 10  J=1, NCA
         ANORM1 = DASUM(NRA,A(1,J),1)
         ANORM = DMAX1(ANORM1,ANORM)
   10 CONTINUE
C
 9000 CALL E1POP ('DNR1RR ')
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  Q10G/DQ10G (Single/Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    January 29, 1985
C
C  Purpose:    Integrate a function.
C
C  Usage:      CALL Q10G (LIMIT, LAST, MAXERR, ERMAX, ELIST, IORD,
C                         NRMAX)
C
C  Arguments:  (See comment block below)
C
C  Chapter:    MATH/LIBRARY Integration and Differentiation
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C
C     ..................................................................
C
C 1.        Q10G
C           ORDERING ROUTINE
C              STANDARD FORTRAN SUBROUTINE
C              REAL VERSION
C
C 2.        PURPOSE
C              THIS ROUTINE MAINTAINS THE DESCENDING ORDERING
C              IN THE LIST OF THE LOCAL ERROR ESTIMATES RESULTING FROM
C              THE INTERVAL SUBDIVISION PROCESS. AT EACH CALL TWO ERROR
C              ESTIMATES ARE INSERTED USING THE SEQUENTIAL SEARCH
C              METHOD, TOP-DOWN FOR THE LARGEST ERROR ESTIMATE
C              AND BOTTOM-UP FOR THE SMALLEST ERROR ESTIMATE.
C
C 3.        CALLING SEQUENCE
C              CALL Q10G(LIMIT,LAST,MAXERR,ERMAX,ELIST,IORD,NRMAX)
C
C           PARAMETERS (MEANING AT OUTPUT)
C              LIMIT  - INTEGER
C                       MAXIMUM NUMBER OF ERROR ESTIMATES THE LIST
C                       CAN CONTAIN
C
C              LAST   - INTEGER
C                       NUMBER OF ERROR ESTIMATES CURRENTLY IN THE LIST
C
C              MAXERR - INTEGER
C                       MAXERR POINTS TO THE NRMAX-TH LARGEST ERROR
C                       ESTIMATE CURRENTLY IN THE LIST
C
C              ERMAX  - REAL
C                       NRMAX-TH LARGEST ERROR ESTIMATE
C                       ERMAX = ELIST(MAXERR)
C
C              ELIST  - REAL
C                       VECTOR OF DIMENSION LAST CONTAINING THE ERROR
C                       ESTIMATES
C
C              IORD   - INTEGER
C                       VECTOR OF DIMENSION LAST, THE FIRST K ELEMENTS
C                       OF WHICH CONTAIN POINTERS TO THE ERROR
C                       ESTIMATES, SUCH THAT
C                       ELIST(IORD(1)),...,  ELIST(IORD(K))
C                       FORM A DECREASING SEQUENCE, WITH
C                       K = LAST IF LAST.LE.(LIMIT/2+2), AND
C                       K = LIMIT+1-LAST OTHERWISE
C
C              NRMAX  - INTEGER
C                       MAXERR = IORD(NRMAX)
C
C 4.        NO SUBROUTINES OR FUNCTIONS NEEDED
C
C     ..................................................................
C
C
C           CHECK WHETHER THE LIST CONTAINS MORE THAN
C           TWO ERROR ESTIMATES.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DQ10G (LIMIT, LAST, MAXERR, ERMAX, ELIST, IORD, NRMAX)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    LIMIT, LAST, MAXERR, NRMAX, IORD(*)
      DOUBLE PRECISION ERMAX, ELIST(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, IBEG, IDO, ISUCC, J, JBND, JUPBN, K
      DOUBLE PRECISION ERRMAX, ERRMIN
C
      IF (LAST .GT. 2) GO TO 10
C                                  THIS ROUTINE IS A NUCLEI OF QDAG,
C                                  BUT IS ALSO USED BY Q3AGI, Q3AGP,
C                                  Q3AGS, Q3AWC, Q3AWO, AND Q3AWS
C
      IORD(1) = 1
      IORD(2) = 2
      GO TO 90
C                                  THIS PART OF THE ROUTINE IS ONLY
C                                  EXECUTED IF, DUE TO A DIFFICULT
C                                  INTEGRAND, SUBDIVISION INCREASED THE
C                                  ERROR ESTIMATE.  IN THE NORMAL CASE
C                                  THE INSERT PROCEDURE SHOULD START
C                                  AFTER THE NRMAX-TH LARGEST ERROR
C                                  ESTIMATE
   10 ERRMAX = ELIST(MAXERR)
      IF (NRMAX .EQ. 1) GO TO 30
      IDO = NRMAX - 1
      DO 20  I=1, IDO
         ISUCC = IORD(NRMAX-1)
C                                  JUMP OUT OF DO-LOOP
         IF (ERRMAX .LE. ELIST(ISUCC)) GO TO 30
         IORD(NRMAX) = ISUCC
         NRMAX = NRMAX - 1
   20 CONTINUE
C                                  COMPUTE THE NUMBER OF ELEMENTS IN THE
C                                  LIST TO BE MAINTAINED IN DESCENDING
C                                  ORDER.  THIS NUMBER DEPENDS ON THE
C                                  NUMBER OF SUBDIVISIONS STILL ALLOWED
   30 JUPBN = LAST
      IF (LAST .GT. (LIMIT/2+2)) JUPBN = LIMIT + 3 - LAST
      ERRMIN = ELIST(LAST)
C                                  INSERT ERRMAX BY TRAVERSING THE LIST
C                                  TOP-DOWN, STARTING COMPARISON FROM
C                                  THE ELEMENT ELIST(IORD(NRMAX+1))
      JBND = JUPBN - 1
      IBEG = NRMAX + 1
      IF (IBEG .GT. JBND) GO TO 50
      DO 40  I=IBEG, JBND
         ISUCC = IORD(I)
C                                  JUMP OUT OF DO-LOOP
         IF (ERRMAX .GE. ELIST(ISUCC)) GO TO 60
         IORD(I-1) = ISUCC
   40 CONTINUE
   50 IORD(JBND) = MAXERR
      IORD(JUPBN) = LAST
      GO TO 90
C                                  INSERT ERRMIN BY TRAVERSING THE LIST
C                                  BOTTOM-UP
   60 IORD(I-1) = MAXERR
      K = JBND
      DO 70  J=I, JBND
         ISUCC = IORD(K)
C                                  JUMP OUT OF DO-LOOP
         IF (ERRMIN .LT. ELIST(ISUCC)) GO TO 80
         IORD(K+1) = ISUCC
         K = K - 1
   70 CONTINUE
      IORD(I) = LAST
      GO TO 90
   80 IORD(K+1) = LAST
C                                  SET MAXERR AND ERMAX.
   90 MAXERR = IORD(NRMAX)
      ERMAX = ELIST(MAXERR)
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  Q2AGS/DQ2AGS (Single/Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    January 29, 1985
C
C  Purpose:    Integrate a function with endpoint singularities.
C
C  Usage:      CALL Q2AGS (F, A, B, ERRABS, ERRREL, RESULT, ERREST,
C                          MAXSUB, NEVAL, NSUBIN, ALIST, BLIST, RLIST,
C                          ELIST, IORD)
C
C  Arguments:  (See QDAGS)
C
C  Chapter:    MATH/LIBRARY Integration and Differentiation
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DQ2AGS (F, A, B, ERRABS, ERRREL, RESULT, ERREST,
     &                   MAXSUB, NEVAL, NSUBIN, ALIST, BLIST, RLIST,
     &                   ELIST, IORD)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    MAXSUB, NEVAL, NSUBIN, IORD(*)
      DOUBLE PRECISION F, A, B, ERRABS, ERRREL, RESULT, ERREST,
     &           ALIST(*), BLIST(*), RLIST(*), ELIST(*)
      EXTERNAL   F
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    IER
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1PSH, E1STI, E1STD, DQ3AGS
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   N1RTY
      INTEGER    N1RTY
C
      CALL E1PSH ('DQ2AGS ')
C                                  CHECK MAXSUB
      IF (MAXSUB .LT. 1) THEN
         CALL E1STI (1, MAXSUB)
         CALL E1MES (5, 1, 'The maximum number of subintervals '//
     &               'MAXSUB = %(I1).  It must be at least 1.')
      END IF
C                                  CHECK ERRABS
      IF (ERRABS .LT. 0.0D0) THEN
         CALL E1STD (1, ERRABS)
         CALL E1MES (5, 2, 'The absolute error desired ERRABS '//
     &               '= %(D1).  It must be at least zero.')
      END IF
C                                  CHECK ERRREL
      IF (ERRREL .LT. 0.0D0) THEN
         CALL E1STD (1, ERRREL)
         CALL E1MES (5, 3, 'The relative error desired ERRREL '//
     &               '= %(D1).  It must be at least zero.')
      END IF
C                                  CHECK ERRABS AND ERRREL
C
      IF (ERRABS.EQ.0.0D0 .AND. ERRREL.EQ.0.0D0) THEN
         CALL E1MES (5, 4, 'The error tolerance arguments ERRABS '//
     &               'and ERRREL are both equal to zero.  At '//
     &               'least one of them must be greater than zero.')
      END IF
C                                  CHECK ERRREL .GE. 1
      IF (ERRREL .GE. 1.0D0) THEN
         CALL E1STD (1, ERRREL)
         CALL E1MES (5, 5, 'The relative error desired ERRREL = '//
     &               '%(D1).  When ERRREL is greater than or equal '//
     &               'to one, zero can always be returned as an '//
     &               'answer.')
      END IF
C
      IF (N1RTY(0) .NE. 0) GO TO 9000
C
      CALL DQ3AGS (F, A, B, ERRABS, ERRREL, MAXSUB, RESULT, ERREST,
     &             NEVAL, IER, ALIST, BLIST, RLIST, ELIST, IORD,
     &             NSUBIN)
C
      IF (IER .EQ. 1) THEN
         CALL E1STI (1, MAXSUB)
         CALL E1MES (4, 1, 'The maximum number of subintervals '//
     &               'allowed MAXSUB = %(I1) has been reached.  '//
     &               'Use DQ2AGS and increase MAXSUB.  Dimension '//
     &               'adjustments may be necessary.')
      ELSE IF (IER .EQ. 2) THEN
         CALL E1STD (1, ERRABS)
         CALL E1STD (2, ERRREL)
         CALL E1MES (3, 2, 'Roundoff error has been detected.  '//
     &               'The requested tolerances, ERRABS = %(D1) and '//
     &               'ERRREL = %(D2), cannot be reached.')
      ELSE IF (IER .EQ. 3) THEN
         CALL E1STD (1, ALIST(IORD(1)))
         CALL E1STD (2, BLIST(IORD(1)))
         CALL E1MES (3, 3, 'Precision is degraded due to too fine a '//
     &               'subdivision relative to the requested '//
     &               'tolerance.  This may be due to bad '//
     &               'integrand behavior in the interval (%(D1),'//
     &               '%(D2)).  Higher precision may alleviate '//
     &               'this problem.')
      ELSE IF (IER .EQ. 4) THEN
         CALL E1STD (1, ERRABS)
         CALL E1STD (2, ERRREL)
         CALL E1MES (3, 4, 'Roundoff error has been detected '//
     &               'in the extrapolation table.  The requested '//
     &               'tolerances, ERRABS = %(D1) and ERRREL = '//
     &               '%(D2) cannot be reached.')
      ELSE IF (IER .EQ. 5) THEN
         CALL E1MES (4, 5, 'Integral is probably divergent '//
     &               'or slowly convergent.')
      END IF
C
 9000 CONTINUE
      CALL E1POP ('DQ2AGS ')
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  Q3AGS/DQ3AGS (Single/Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    January 29, 1985
C
C  Purpose:    Integrate a function.
C
C  Usage:      CALL Q3AGS (F, A, B, EPSABS, EPSREL, LIMIT, RESULT,
C                          ABSERR, NEVAL, IER, ALIST, BLIST, RLIST,
C                          ELIST, IORD, LAST)
C
C  Arguments:  (See comment block below)
C
C  Chapter:    MATH/LIBRARY Integration and Differentiation
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C
C ......................................................................
C
C 1.     Q3AGS
C        COMPUTATION OF A DEFINITE INTEGRAL
C           STANDARD FORTRAN SUBROUTINE
C           REAL VERSION
C
C 2.     PURPOSE
C           THE ROUTINE CALCULATES AN APPROXIMATION  RESULT  TO A GIVEN
C           DEFINITE INTEGRAL   I = INTEGRAL OF  F  OVER (A,B),
C           HOPEFULLY SATISFYING FOLLOWING CLAIM FOR ACCURACY
C           ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I)).
C
C 3.     CALLING SEQUENCE
C           CALL Q3AGS(F,A,B,EPSABS,EPSREL,RESULT,ABSERR,NEVAL,IER)
C
C        PARAMETERS
C         ON ENTRY
C            F      - REAL
C                     FUNCTION SUBPROGRAM DEFINING THE INTEGRAND
C                     FUNCTION F(X). THE ACTUAL NAME FOR F NEEDS TO BE
C                     DECLARED E X T E R N A L IN THE DRIVER PROGRAM.
C
C            A      - REAL
C                     LOWER LIMIT OF INTEGRATION
C
C            B      - REAL
C                     UPPER LIMIT OF INTEGRATION
C
C            EPSABS - REAL
C                     ABSOLUTE ACCURACY REQUESTED
C            EPSREL - REAL
C                     RELATIVE ACCURACY REQUESTED
C                     IF  EPSABS.LT.0 AND EPSREL.LT.0,
C                     THE ROUTINE WILL END WITH IER = 6.
C
C         ON RETURN
C            RESULT - REAL
C                     APPROXIMATION TO THE INTEGRAL
C
C            ABSERR - REAL
C                     ESTIMATE OF THE MODULUS OF THE ABSOLUTE ERROR,
C                     WHICH SHOULD EQUAL OR EXCEED ABS(I-RESULT)
C
C            NEVAL  - INTEGER
C                     NUMBER OF INTEGRAND EVALUATIONS
C
C            IER    - INTEGER
C                     IER = 0 NORMAL AND RELIABLE TERMINATION OF THE
C                             ROUTINE. IT IS ASSUMED THAT THE REQUESTED
C                             ACCURACY HAS BEEN ACHIEVED.
C                     IER.GT.0 ABNORMAL TERMINATION OF THE ROUTINE
C                             THE ESTIMATES FOR INTEGRAL AND ERROR ARE
C                             LESS RELIABLE. IT IS ASSUMED THAT THE
C                             REQUESTED ACCURACY HAS NOT BEEN ACHIEVED.
C                         = 1 MAXIMUM NUMBER OF SUBDIVISIONS ALLOWED
C                             HAS BEEN ACHIEVED. ONE CAN ALLOW MORE SUB-
C                             DIVISIONS BY INCREASING THE DATA VALUE OF
C                             LIMIT IN Q3AGS (AND TAKING THE ACCORDING
C                             DIMENSION ADJUSTMENTS INTO ACCOUNT).
C                             HOWEVER, IF THIS YIELDS NO IMPROVEMENT
C                             IT IS ADVISED TO ANALYZE THE INTEGRAND
C                             IN ORDER TO DETERMINE THE INTEGRATION
C                             DIFFICULTIES. IF THE POSITION OF A
C                             LOCAL DIFFICULTY CAN BE DETERMINED (E.G.
C                             SINGULARITY, DISCONTINUITY WITHIN THE
C                             INTERVAL) ONE WILL PROBABLY GAIN FROM
C                             SPLITTING UP THE INTERVAL AT THIS POINT
C                             AND CALLING THE INTEGRATOR ON THE SUB-
C                             RANGES. IF POSSIBLE, AN APPROPRIATE
C                             SPECIAL-PURPOSE INTEGRATOR SHOULD BE USED,
C                             WHICH IS DESIGNED FOR HANDLING THE TYPE
C                             OF DIFFICULTY INVOLVED.
C                         = 2 THE OCCURRENCE OF ROUNDOFF ERROR IS DETEC-
C                             TED, WHICH PREVENTS THE REQUESTED
C                             TOLERANCE FROM BEING ACHIEVED.
C                             THE ERROR MAY BE UNDER-ESTIMATED.
C                         = 3 EXTREMELY BAD INTEGRAND BEHAVIOUR
C                             OCCURS AT SOME  POINTS OF THE INTEGRATION
C                             INTERVAL.
C                         = 4 THE ALGORITHM DOES NOT CONVERGE. ROUNDOFF
C                             ERROR IS DETECTED IN THE EXTRAPOLATION
C                             TABLE. IT IS PRESUMED THAT THE REQUESTED
C                             TOLERANCE CANNOT BE ACHIEVED, AND THAT THE
C                             RETURNED RESULT IS THE BEST WHICH CAN BE
C                             OBTAINED.
C                         = 5 THE INTEGRAL IS PROBABLY DIVERGENT, OR
C                             SLOWLY CONVERGENT. IT MUST BE NOTED
C                             THAT DIVERGENCE CAN OCCUR WITH ANY OTHER
C                             VALUE OF IER.
C                         = 6 THE INPUT IS INVALID, BECAUSE
C                             EPSABS.LT.0 AND EPSREL.LT.0,
C                             RESULT, ABSERR, NEVAL ARE SET TO ZERO.
C
C 4.      SUBROUTINES OR FUNCTIONS NEEDED
C              - Q9AG
C              - Q10G
C              - Q4AWO
C              - F (USER-PROVIDED FUNCTION)
C              - Q4NG
C              - FORTRAN ABS, AMAX1, AMIN1
C
C ......................................................................
C
C
C
C            THE DIMENSION OF RLIST2 IS DETERMINED BY THE VALUE OF
C            LIMEXP IN SUBROUTINE Q4AWO (RLIST2 SHOULD BE OF
C            DIMENSION (LIMEXP+2) AT LEAST).
C
C
C            LIMIT IS THE MAXIMUM NUMBER OF SUBINTERVALS ALLOWED IN THE
C            SUBDIVISION PROCESS OF Q3AGS. TAKE CARE THAT LIMIT.GE.1.
C
C            LIST OF MAJOR VARIABLES
C            -----------------------
C
C           ALIST     - LIST OF LEFT END POINTS OF ALL SUBINTERVALS
C                       CONSIDERED UP TO NOW
C           BLIST     - LIST OF RIGHT END POINTS OF ALL SUBINTERVALS
C                       CONSIDERED UP TO NOW
C           RLIST(I)  - APPROXIMATION TO THE INTEGRAL OVER
C                       (ALIST(I),BLIST(I))
C           RLIST2    - ARRAY OF DIMENSION AT LEAST LIMEXP+2
C                       CONTAINING THE PART OF THE EPSILON TABLE
C                       WHICH IS STILL NEEDED FOR FURTHER COMPUTATIONS
C           ELIST(I)  - ERROR ESTIMATE APPLYING TO RLIST(I)
C           MAXERR    - POINTER TO THE INTERVAL WITH LARGEST ERROR
C                       ESTIMATE
C           ERRMAX    - ELIST(MAXERR)
C           ERLAST    - ERROR ON THE INTERVAL CURRENTLY SUBDIVIDED
C                       (BEFORE THAT SUBDIVISION HAS TAKEN PLACE)
C           AREA      - SUM OF THE INTEGRALS OVER THE SUBINTERVALS
C           ERRSUM    - SUM OF THE ERRORS OVER THE SUBINTERVALS
C           ERRBND    - REQUESTED ACCURACY MAX(EPSABS,EPSREL*
C                       ABS(RESULT))
C           *****1    - VARIABLE FOR THE LEFT INTERVAL
C           *****2    - VARIABLE FOR THE RIGHT INTERVAL
C           LAST      - INDEX FOR SUBDIVISION
C           NRES      - NUMBER OF CALLS TO THE EXTRAPOLATION ROUTINE
C           NUMRL2    - NUMBER OF ELEMENTS CURRENTLY IN RLIST2. IF AN
C                       APPROPRIATE APPROXIMATION TO THE COMPOUNDED
C                       INTEGRAL HAS BEEN OBTAINED IT IS PUT IN
C                       RLIST2(NUMRL2) AFTER NUMRL2 HAS BEEN INCREASED
C                       BY ONE.
C           SMALL     - LENGTH OF THE SMALLEST INTERVAL CONSIDERED
C                       UP TO NOW, MULTIPLIED BY 1.5
C           ERLARG    - SUM OF THE ERRORS OVER THE INTERVALS LARGER
C                       THAN THE SMALLEST INTERVAL CONSIDERED UP TO NOW
C           EXTRAP    - LOGICAL VARIABLE DENOTING THAT THE ROUTINE
C                       IS ATTEMPTING TO PERFORM EXTRAPOLATION
C                       I.E. BEFORE SUBDIVIDING THE SMALLEST INTERVAL
C                       WE TRY TO DECREASE THE VALUE OF ERLARG.
C           NOEXT     - LOGICAL VARIABLE DENOTING THAT EXTRAPOLATION
C                       IS NO LONGER ALLOWED (TRUE VALUE)
C
C            MACHINE DEPENDENT CONSTANTS
C            ---------------------------
C
C           EPMACH IS THE LARGEST RELATIVE SPACING.
C           UFLOW IS THE SMALLEST POSITIVE MAGNITUDE.
C           OFLOW IS THE LARGEST POSITIVE MAGNITUDE.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DQ3AGS (F, A, B, EPSABS, EPSREL, LIMIT, RESULT,
     &                   ABSERR, NEVAL, IER, ALIST, BLIST, RLIST,
     &                   ELIST, IORD, LAST)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    LIMIT, NEVAL, IER, LAST, IORD(*)
      DOUBLE PRECISION F, A, B, EPSABS, EPSREL, RESULT, ABSERR,
     &           ALIST(*), BLIST(*), RLIST(*), ELIST(*)
      EXTERNAL   F
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    ID, IERRO, IROFF1, IROFF2, IROFF3, JUPBND, K, KSGN,
     &           KTMIN, MAXERR, NRES, NRMAX, NUMRL2
      DOUBLE PRECISION A1, A2, ABSEPS, AREA, AREA1, AREA12, AREA2, B1,
     &           B2, CORREC, DEFAB1, DEFAB2, DEFABS, DRES, EPMACH,
     &           ERLARG, ERLAST, ERRBND, ERRMAX, ERRO12, ERROR1,
     &           ERROR2, ERRSUM, ERTEST, OFLOW, RES3LA(3), RESABS,
     &           RESEPS, RLIST2(52), SMALL, UFLOW
      LOGICAL    EXTRAP, NOEXT
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  DABS,DMAX1
      INTRINSIC  DABS, DMAX1
      DOUBLE PRECISION DABS, DMAX1
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   DQ10G, DQ4AWO, DQ4NG, DQ9AG
C
      CALL DQ4NG (EPMACH, UFLOW, OFLOW)
C                                  TEST ON VALIDITY OF PARAMETERS
      IER = 0
      NEVAL = 0
      LAST = 0
      RESULT = 0.0D+00
      ABSERR = 0.0D+00
      ALIST(1) = A
      BLIST(1) = B
      RLIST(1) = 0.0D+00
      ELIST(1) = 0.0D+00
      IF (EPSABS.LT.0.0D+00 .AND. EPSREL.LT.0.0D+00) IER = 6
      IF (IER .EQ. 6) GO TO 180
C                                  FIRST APPROXIMATION TO THE INTEGRAL
      IERRO = 0
      CALL DQ9AG (F, A, B, RESULT, ABSERR, DEFABS, RESABS)
C
C                                  TEST ON ACCURACY.
      DRES = DABS(RESULT)
      ERRBND = DMAX1(EPSABS,EPSREL*DRES)
      LAST = 1
      RLIST(1) = RESULT
      ELIST(1) = ABSERR
      IORD(1) = 1
      IF (ABSERR.LE.1.0D+02*EPMACH*DEFABS .AND. ABSERR.GT.ERRBND)
     &    IER = 2
      IF (LIMIT .EQ. 1) IER = 1
      IF (IER.NE.0 .OR. (ABSERR.LE.ERRBND.AND.ABSERR.NE.RESABS) .OR.
     &    ABSERR.EQ.0.0D+00) GO TO 170
C                                  INITIALIZATION
      RLIST2(1) = RESULT
      ERRMAX = ABSERR
      MAXERR = 1
      AREA = RESULT
      ERRSUM = ABSERR
      ABSERR = OFLOW
      NRMAX = 1
      NRES = 0
      NUMRL2 = 2
      KTMIN = 0
      EXTRAP = .FALSE.
      NOEXT = .FALSE.
      IROFF1 = 0
      IROFF2 = 0
      IROFF3 = 0
      KSGN = -1
      IF (DRES .GE. (1.0D+00-5.0D+01*EPMACH)*DEFABS) KSGN = 1
C
C                                  MAIN DO-LOOP
      DO 100  LAST=2, LIMIT
C                                  BISECT THE SUBINTERVAL WITH THE
C                                  NRMAX-TH LARGEST ERROR ESTIMATE.
         A1 = ALIST(MAXERR)
         B1 = 5.0D-01*(ALIST(MAXERR)+BLIST(MAXERR))
         A2 = B1
         B2 = BLIST(MAXERR)
         ERLAST = ERRMAX
         CALL DQ9AG (F, A1, B1, AREA1, ERROR1, RESABS, DEFAB1)
         CALL DQ9AG (F, A2, B2, AREA2, ERROR2, RESABS, DEFAB2)
C
C                                  IMPROVE PREVIOUS APPROXIMATIONS TO
C                                  INTEGRAL AND ERROR AND TEST FOR
C                                  ACCURACY.
         AREA12 = AREA1 + AREA2
         ERRO12 = ERROR1 + ERROR2
         ERRSUM = ERRSUM + ERRO12 - ERRMAX
         AREA = AREA + AREA12 - RLIST(MAXERR)
         IF (DEFAB1.EQ.ERROR1 .OR. DEFAB2.EQ.ERROR2) GO TO 20
         IF (DABS(RLIST(MAXERR)-AREA12).GT.1.0D-05*DABS(AREA12) .OR.
     &       ERRO12.LT.9.9D-01*ERRMAX) GO TO 10
         IF (EXTRAP) IROFF2 = IROFF2 + 1
         IF (.NOT.EXTRAP) IROFF1 = IROFF1 + 1
   10    IF (LAST.GT.10 .AND. ERRO12.GT.ERRMAX) IROFF3 = IROFF3 + 1
   20    RLIST(MAXERR) = AREA1
         RLIST(LAST) = AREA2
         ERRBND = DMAX1(EPSABS,EPSREL*DABS(AREA))
C                                  TEST FOR ROUNDOFF ERROR AND
C                                  EVENTUALLY SET ERROR FLAG.
C
         IF (IROFF1+IROFF2.GE.10 .OR. IROFF3.GE.20) IER = 2
         IF (IROFF2 .GE. 5) IERRO = 3
C                                  SET ERROR FLAG IN THE CASE THAT THE
C                                  NUMBER OF SUBINTERVALS EQUALS
C                                  LIMIT.
         IF (LAST .EQ. LIMIT) IER = 1
C                                  SET ERROR FLAG IN THE CASE OF BAD
C                                  INTEGRAND BEHAVIOUR AT A POINT OF
C                                  THE INTEGRATION RANGE.
C
         IF (DMAX1(DABS(A1),DABS(B2)) .LE. (1.0D+00+1.0D+03*EPMACH)*
     &       (DABS(A2)+1.0D+03*UFLOW)) IER = 4
C                                  APPEND THE NEWLY-CREATED INTERVALS
C                                  TO THE LIST.
         IF (ERROR2 .GT. ERROR1) GO TO 30
         ALIST(LAST) = A2
         BLIST(MAXERR) = B1
         BLIST(LAST) = B2
         ELIST(MAXERR) = ERROR1
         ELIST(LAST) = ERROR2
         GO TO 40
   30    ALIST(MAXERR) = A2
         ALIST(LAST) = A1
         BLIST(LAST) = B1
         RLIST(MAXERR) = AREA2
         RLIST(LAST) = AREA1
         ELIST(MAXERR) = ERROR2
         ELIST(LAST) = ERROR1
C                                  CALL SUBROUTINE DQ10G TO MAINTAIN
C                                  THE DESCENDING ORDERING IN THE
C                                  LIST OF ERROR ESTIMATES AND SELECT
C                                  THE SUBINTERVAL WITH NRMAX-TH
C                                  LARGEST ERROR ESTIMATE (TO BE
C                                  BISECTED NEXT).
C
   40    CALL DQ10G (LIMIT, LAST, MAXERR, ERRMAX, ELIST, IORD, NRMAX)
C
C                                  JUMP OUT OF DO-LOOP
         IF (ERRSUM .LE. ERRBND) GO TO 140
C                                  JUMP OUT OF DO-LOOP
         IF (IER .NE. 0) GO TO 110
         IF (LAST .EQ. 2) GO TO 90
         IF (NOEXT) GO TO 100
         ERLARG = ERLARG - ERLAST
         IF (DABS(B1-A1) .GT. SMALL) ERLARG = ERLARG + ERRO12
         IF (EXTRAP) GO TO 50
C                                  TEST WHETHER THE INTERVAL TO BE
C                                  BISECTED NEXT IS THE SMALLEST
C                                  INTERVAL.
C
         IF (DABS(BLIST(MAXERR)-ALIST(MAXERR)) .GT. SMALL) GO TO 100
         EXTRAP = .TRUE.
         NRMAX = 2
   50    IF (IERRO.EQ.3 .OR. ERLARG.LE.ERTEST) GO TO 70
C
C                                  THE SMALLEST INTERVAL HAS THE
C                                  LARGEST ERROR. BEFORE BISECTING
C                                  DECREASE THE SUM OF THE ERRORS
C                                  OVER THE LARGER INTERVALS (ERLARG)
C                                  AND PERFORM EXTRAPOLATION.
         ID = NRMAX
         JUPBND = LAST
         IF (LAST .GT. (2+LIMIT/2)) JUPBND = LIMIT + 3 - LAST
         DO 60  K=ID, JUPBND
            MAXERR = IORD(NRMAX)
            ERRMAX = ELIST(MAXERR)
C                                  JUMP OUT OF DO-LOOP
C
            IF (DABS(BLIST(MAXERR)-ALIST(MAXERR)) .GT. SMALL) GO TO 100
            NRMAX = NRMAX + 1
   60    CONTINUE
C                                  PERFORM EXTRAPOLATION.
   70    NUMRL2 = NUMRL2 + 1
         RLIST2(NUMRL2) = AREA
         CALL DQ4AWO (NUMRL2, RLIST2, RESEPS, ABSEPS, RES3LA, NRES)
         KTMIN = KTMIN + 1
         IF (KTMIN.GT.5 .AND. ABSERR.LT.1.0D-03*ERRSUM) IER = 5
         IF (ABSEPS .GE. ABSERR) GO TO 80
         KTMIN = 0
         ABSERR = ABSEPS
         RESULT = RESEPS
         CORREC = ERLARG
         ERTEST = DMAX1(EPSABS,EPSREL*DABS(RESEPS))
C                                  JUMP OUT OF DO-LOOP
         IF (ABSERR .LE. ERTEST) GO TO 110
C                                  PREPARE BISECTION OF THE SMALLEST
C                                  INTERVAL.
   80    IF (NUMRL2 .EQ. 1) NOEXT = .TRUE.
         IF (IER .EQ. 5) GO TO 110
         MAXERR = IORD(1)
         ERRMAX = ELIST(MAXERR)
         NRMAX = 1
         EXTRAP = .FALSE.
         SMALL = SMALL*5.0D-01
         ERLARG = ERRSUM
         GO TO 100
   90    SMALL = DABS(B-A)*3.75D-01
         ERLARG = ERRSUM
         ERTEST = ERRBND
         RLIST2(2) = AREA
  100 CONTINUE
C                                  SET FINAL RESULT AND ERROR ESTIMATE.
  110 IF (ABSERR .EQ. OFLOW) GO TO 140
      IF (IER+IERRO .EQ. 0) GO TO 130
      IF (IERRO .EQ. 3) ABSERR = ABSERR + CORREC
      IF (IER .EQ. 0) IER = 3
      IF (RESULT.NE.0.0D+00 .AND. AREA.NE.0.0D+00) GO TO 120
      IF (ABSERR .GT. ERRSUM) GO TO 140
      IF (AREA .EQ. 0.0D+00) GO TO 160
      GO TO 130
  120 IF (ABSERR/DABS(RESULT) .GT. ERRSUM/DABS(AREA)) GO TO 140
C
C                                  TEST ON DIVERGENCE.
C
  130 IF (KSGN.EQ.(-1) .AND. DMAX1(DABS(RESULT),DABS(AREA)).LE.DEFABS*
     &    1.0D-02) GO TO 160
      IF (1.0D-02.GT.(RESULT/AREA) .OR. (RESULT/AREA).GT.1.0D+02 .OR.
     &    ERRSUM.GT.DABS(AREA)) IER = 6
      GO TO 160
C                                  COMPUTE GLOBAL INTEGRAL SUM.
  140 RESULT = 0.0D+00
      DO 150  K=1, LAST
         RESULT = RESULT + RLIST(K)
  150 CONTINUE
      ABSERR = ERRSUM
  160 IF (IER .GT. 2) IER = IER - 1
  170 NEVAL = 42*LAST - 21
  180 RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  Q4AWO/DQ4AWO (Single/Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    January 29, 1985
C
C  Purpose:    Integrate a function.
C
C  Usage:      CALL Q4AWO (N, EPSTAB, RESULT, ABSERR, RES3LA, NRES)
C
C  Arguments:  (See comment block below)
C
C  Chapter:    MATH/LIBRARY Integration and Differentiation
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C
C     ................................................................
C
C 1.        Q4AWO
C           EPSILON ALGORITHM
C              STANDARD FORTRAN SUBROUTINE
C              REAL VERSION
C
C 2.        PURPOSE
C              THE ROUTINE DETERMINES THE LIMIT OF A GIVEN SEQUENCE OF
C              APPROXIMATIONS, BY MEANS OF THE EPSILON ALGORITHM
C              OF P. WYNN.
C              AN ESTIMATE OF THE ABSOLUTE ERROR IS ALSO GIVEN.
C              THE CONDENSED EPSILON TABLE IS COMPUTED. ONLY THOSE
C              ELEMENTS NEEDED FOR THE COMPUTATION OF THE NEXT DIAGONAL
C              ARE PRESERVED.
C
C 3.        CALLING SEQUENCE
C              CALL Q4AWO(N,EPSTAB,RESULT,ABSERR,RES3LA,NRES)
C
C           PARAMETERS
C              N      - INTEGER
C                       EPSTAB(N) CONTAINS THE NEW ELEMENT IN THE
C                       FIRST COLUMN OF THE EPSILON TABLE.
C
C              EPSTAB - REAL
C                       VECTOR OF DIMENSION 52 CONTAINING THE ELEMENTS
C                       OF THE TWO LOWER DIAGONALS OF THE
C                       TRIANGULAR EPSILON TABLE
C                       THE ELEMENTS ARE NUMBERED STARTING AT THE
C                       RIGHT-HAND CORNER OF THE TRIANGLE.
C
C              RESULT - REAL
C                       RESULTING APPROXIMATION TO THE INTEGRAL
C
C              ABSERR - REAL
C                       ESTIMATE OF THE ABSOLUTE ERROR COMPUTED FROM
C                       RESULT AND THE 3 PREVIOUS RESULTS
C
C              RES3LA - REAL
C                       VECTOR OF DIMENSION 3 CONTAINING THE LAST 3
C                       RESULTS
C
C              NRES   - INTEGER
C                       NUMBER OF CALLS TO THE ROUTINE
C                       (SHOULD BE ZERO AT FIRST CALL)
C
C 4.        SUBROUTINES OR FUNCTIONS NEEDED
C                     - Q4NG
C                     - FORTRAN ABS, AMAX1
C
C     ..................................................................
C
C
C           LIST OF MAJOR VARIABLES
C           -----------------------
C
C           E0     - THE 4 ELEMENTS ON WHICH THE
C           E1       COMPUTATION OF A NEW ELEMENT IN
C           E2       THE EPSILON TABLE IS BASED
C           E3                 E0
C                        E3    E1    NEW
C                              E2
C           NEWELM - NUMBER OF ELEMENTS TO BE COMPUTED IN THE NEW
C                    DIAGONAL
C           ERROR  - ERROR = ABS(E1-E0)+ABS(E2-E1)+ABS(NEW-E2)
C           RESULT - THE ELEMENT IN THE NEW DIAGONAL WITH LEAST VALUE
C                    OF ERROR
C
C           MACHINE DEPENDENT CONSTANTS
C           ---------------------------
C
C           EPMACH IS THE LARGEST RELATIVE SPACING.
C           UFLOW IS THE SMALLEST POSITIVE MAGNITUDE.
C           OFLOW IS THE LARGEST POSITIVE MAGNITUDE.
C           LIMEXP IS THE MAXIMUM NUMBER OF ELEMENTS THE EPSILON
C           TABLE CAN CONTAIN. IF THIS NUMBER IS REACHED, THE UPPER
C           DIAGONAL OF THE EPSILON TABLE IS DELETED.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DQ4AWO (N, EPSTAB, RESULT, ABSERR, RES3LA, NRES)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, NRES
      DOUBLE PRECISION RESULT, ABSERR, EPSTAB(*), RES3LA(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, IB, IB2, IE, INDX, K1, K2, K3, LIMEXP, NEWELM, NUM
      DOUBLE PRECISION DELTA1, DELTA2, DELTA3, E0, E1, E1ABS, E2, E3,
     &           EPMACH, EPSINF, ERR1, ERR2, ERR3, ERROR, OFLOW, RES,
     &           SS, TOL1, TOL2, TOL3, UFLOW
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  DABS,DMAX1
      INTRINSIC  DABS, DMAX1
      DOUBLE PRECISION DABS, DMAX1
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   DQ4NG
C
      CALL DQ4NG (EPMACH, UFLOW, OFLOW)
      NRES = NRES + 1
      ABSERR = OFLOW
      RESULT = EPSTAB(N)
      IF (N .LT. 3) GO TO 100
      LIMEXP = 50
      EPSTAB(N+2) = EPSTAB(N)
      NEWELM = (N-1)/2
      EPSTAB(N) = OFLOW
      NUM = N
      K1 = N
      DO 40  I=1, NEWELM
         K2 = K1 - 1
         K3 = K1 - 2
         RES = EPSTAB(K1+2)
         E0 = EPSTAB(K3)
         E1 = EPSTAB(K2)
         E2 = RES
         E1ABS = DABS(E1)
         DELTA2 = E2 - E1
         ERR2 = DABS(DELTA2)
         TOL2 = DMAX1(DABS(E2),E1ABS)*EPMACH
         DELTA3 = E1 - E0
         ERR3 = DABS(DELTA3)
         TOL3 = DMAX1(E1ABS,DABS(E0))*EPMACH
         IF (ERR2.GT.TOL2 .OR. ERR3.GT.TOL3) GO TO 10
C                                  IF E0, E1 AND E2 ARE EQUAL TO WITHIN
C                                  MACHINE ACCURACY, CONVERGENCE IS
C                                  ASSUMED. RESULT = E2 ABSERR =
C                                  ABS(E1-E0)+ABS(E2-E1)
         RESULT = RES
         ABSERR = ERR2 + ERR3
C                                  JUMP OUT OF DO-LOOP
         GO TO 100
   10    E3 = EPSTAB(K1)
         EPSTAB(K1) = E1
         DELTA1 = E1 - E3
         ERR1 = DABS(DELTA1)
         TOL1 = DMAX1(E1ABS,DABS(E3))*EPMACH
C                                  IF TWO ELEMENTS ARE VERY CLOSE TO
C                                  EACH OTHER, OMIT A PART OF THE
C                                  TABLE BY ADJUSTING THE VALUE OF N
C
         IF (ERR1.LE.TOL1 .OR. ERR2.LE.TOL2 .OR. ERR3.LE.TOL3) GO TO 20
         SS = 1.0D+00/DELTA1 + 1.0D+00/DELTA2 - 1.0D+00/DELTA3
         EPSINF = DABS(SS*E1)
C                                  TEST TO DETECT IRREGULAR BEHAVIOUR
C                                  IN THE TABLE, AND EVENTUALLY OMIT
C                                  A PART OF THE TABLE ADJUSTING THE
C                                  VALUE OF N.
         IF (EPSINF .GT. 1.0D-04) GO TO 30
   20    N = I + I - 1
C                                  JUMP OUT OF DO-LOOP
         GO TO 50
C                                  COMPUTE A NEW ELEMENT AND EVENTUALLY
C                                  ADJUST THE VALUE OF RESULT.
   30    RES = E1 + 1.0D+00/SS
         EPSTAB(K1) = RES
         K1 = K1 - 2
         ERROR = ERR2 + DABS(RES-E2) + ERR3
         IF (ERROR .GT. ABSERR) GO TO 40
         ABSERR = ERROR
         RESULT = RES
   40 CONTINUE
C                                  SHIFT THE TABLE.
   50 IF (N .EQ. LIMEXP) N = 2*(LIMEXP/2) - 1
      IB = 1
      IF ((NUM/2)*2 .EQ. NUM) IB = 2
      IE = NEWELM + 1
      DO 60  I=1, IE
         IB2 = IB + 2
         EPSTAB(IB) = EPSTAB(IB2)
         IB = IB2
   60 CONTINUE
      IF (NUM .EQ. N) GO TO 80
      INDX = NUM - N + 1
      DO 70  I=1, N
         EPSTAB(I) = EPSTAB(INDX)
         INDX = INDX + 1
   70 CONTINUE
   80 IF (NRES .GE. 4) GO TO 90
      RES3LA(NRES) = RESULT
      ABSERR = OFLOW
      GO TO 100
C                                  COMPUTE ERROR ESTIMATE
C
   90 ABSERR = DABS(RESULT-RES3LA(3)) + DABS(RESULT-RES3LA(2)) +
     &         DABS(RESULT-RES3LA(1))
      RES3LA(1) = RES3LA(2)
      RES3LA(2) = RES3LA(3)
      RES3LA(3) = RESULT
  100 ABSERR = DMAX1(ABSERR,5.0D+00*EPMACH*DABS(RESULT))
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  Q4NG/DQ4NG (Single/Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    January 29, 1985
C
C  Purpose:    Integrate a function using a non-adaptive rule.
C
C  Usage:      CALL Q4NG (EPMACH, UFLOW, OFLOW)
C
C  Arguments:  (See comment block below)
C
C  Chapter:    MATH/LIBRARY Integration and Differentiation
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C.......................................................................
C
C  REAL MACHINE CONSTANTS
C
C  EPMACH = THE LARGEST RELATIVE SPACING
C
C  UFLOW  = THE SMALLEST POSITIVE MAGNITUDE
C
C  OFLOW  = THE LARGEST MAGNITUDE
C
C.......................................................................
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DQ4NG (EPMACH, UFLOW, OFLOW)
C                                  SPECIFICATIONS FOR ARGUMENTS
      DOUBLE PRECISION EPMACH, UFLOW, OFLOW
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   DMACH
      DOUBLE PRECISION DMACH
C
      EPMACH = DMACH(4)
      UFLOW = DMACH(1)
      OFLOW = DMACH(2)
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  Q9AG/DQ9AG (Single/Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    January 29, 1985
C
C  Purpose:    Integrate a function.
C
C  Usage:      CALL Q9AG (F, A, B, RESULT, ABSERR, RESABS, RESASC)
C
C  Arguments:  (See comment block below)
C
C  Chapter:    MATH/LIBRARY Integration and Differentiation
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C
C     ..................................................................
C
C 1.        Q9AG
C           INTEGRATION RULES
C              STANDARD FORTRAN SUBROUTINE
C              REAL VERSION
C
C 2.        PURPOSE
C              TO COMPUTE I = INTEGRAL OF F OVER (A,B), WITH ERROR
C                             ESTIMATE
C                         J = INTEGRAL OF ABS(F) OVER (A,B)
C
C 3.        CALLING SEQUENCE
C              CALL Q9AG(F,A,B,RESULT,ABSERR,RESABS,RESASC)
C
C           PARAMETERS
C            ON ENTRY
C              F      - REAL
C                       FUNCTION SUBPROGRAM DEFINING THE INTEGRAND
C                       FUNCTION F(X). THE ACTUAL NAME FOR F NEEDS TO BE
C                       DECLARED E X T E R N A L IN THE CALLING PROGRAM.
C
C              A      - REAL
C                       LOWER LIMIT OF INTEGRATION
C
C              B      - REAL
C                       UPPER LIMIT OF INTEGRATION
C
C            ON RETURN
C              RESULT - REAL
C                       APPROXIMATION TO THE INTEGRAL I
C                       RESULT IS COMPUTED BY APPLYING THE 21-POINT
C                       KRONROD RULE (RESK) OBTAINED BY OPTIMAL ADDITION
C                       OF ABSCISSAE TO THE 10-POINT GAUSS RULE (RESG).
C
C              ABSERR - REAL
C                       ESTIMATE OF THE MODULUS OF THE ABSOLUTE ERROR,
C                       WHICH SHOULD NOT EXCEED ABS(I-RESULT)
C
C              RESABS - REAL
C                       APPROXIMATION TO THE INTEGRAL J
C
C              RESASC - REAL
C                       APPROXIMATION TO THE INTEGRAL OF ABS(F-I/(B-A))
C                       OVER (A,B)
C
C 4.        SUBROUTINES OR FUNCTIONS NEEDED
C                 - F (USER-PROVIDED FUNCTION)
C                 - Q4NG
C                 - FORTRAN ABS, AMAX1, AMIN1
C
C     ..................................................................
C
C
C
C           THE ABSCISSAE AND WEIGHTS ARE GIVEN FOR THE INTERVAL (-1,1).
C           BECAUSE OF SYMMETRY ONLY THE POSITIVE ABSCISSAE AND THEIR
C           CORRESPONDING WEIGHTS ARE GIVEN.
C
C           XGK    - ABSCISSAE OF THE 21-POINT KRONROD RULE
C                    XGK(2), XGK(4), ...  ABSCISSAE OF THE 10-POINT
C                    GAUSS RULE
C                    XGK(1), XGK(3), ...  ABSCISSAE WHICH ARE OPTIMALLY
C                    ADDED TO THE 10-POINT GAUSS RULE
C
C           WGK    - WEIGHTS OF THE 21-POINT KRONROD RULE
C
C           WG     - WEIGHTS OF THE 10-POINT GAUSS RULE
C
C
C
C           LIST OF MAJOR VARIABLES
C           -----------------------
C
C           CENTR  - MID POINT OF THE INTERVAL
C           HLGTH  - HALF-LENGTH OF THE INTERVAL
C           ABSC   - ABSCISSA
C           FVAL*  - FUNCTION VALUE
C           RESG   - RESULT OF THE 10-POINT GAUSS FORMULA
C           RESK   - RESULT OF THE 21-POINT KRONROD FORMULA
C           RESKH  - APPROXIMATION TO THE MEAN VALUE OF F OVER (A,B),
C                    I.E. TO I/(B-A)
C
C
C           MACHINE DEPENDENT CONSTANTS
C           ---------------------------
C
C           EPMACH IS THE LARGEST RELATIVE SPACING.
C           UFLOW IS THE SMALLEST POSITIVE MAGNITUDE.
C           OFLOW IS THE LARGEST MAGNITUDE.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DQ9AG (F, A, B, RESULT, ABSERR, RESABS, RESASC)
C                                  SPECIFICATIONS FOR ARGUMENTS
      DOUBLE PRECISION F, A, B, RESULT, ABSERR, RESABS, RESASC
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    J, JTW, JTWM1
      DOUBLE PRECISION ABSC, CENTR, DHLGTH, EPMACH, FC, FSUM, FV1(10),
     &           FV2(10), FVAL1, FVAL2, HLGTH, OFLOW, RESG, RESK,
     &           RESKH, UFLOW
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      DOUBLE PRECISION WG(5), WGK(11), XGK(11)
      SAVE       WG, WGK, XGK
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  DABS,DMAX1,DMIN1
      INTRINSIC  DABS, DMAX1, DMIN1
      DOUBLE PRECISION DABS, DMAX1, DMIN1
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1USR, DQ4NG
C
      DATA XGK(1)/0.995657163025808080735527280689D0/
      DATA XGK(2)/0.973906528517171720077964012085D0/
      DATA XGK(3)/0.93015749135570822600120718006D0/
      DATA XGK(4)/0.865063366688984510732096688424D0/
      DATA XGK(5)/0.780817726586416897063717578345D0/
      DATA XGK(6)/0.679409568299024406234327365115D0/
      DATA XGK(7)/0.562757134668604683339000099273D0/
      DATA XGK(8)/0.433395394129247190799265943166D0/
      DATA XGK(9)/0.294392862701460198131126603104D0/
      DATA XGK(10)/0.14887433898163121088482600113D0/
      DATA XGK(11)/0.0D0/
      DATA WGK(1)/0.0116946388673718742780643960622D0/
      DATA WGK(2)/0.0325581623079647274788189724594D0/
      DATA WGK(3)/0.0547558965743519960313813002446D0/
      DATA WGK(4)/0.0750396748109199527670431409162D0/
      DATA WGK(5)/0.0931254545836976055350654650834D0/
      DATA WGK(6)/0.109387158802297641899210590326D0/
      DATA WGK(7)/0.123491976262065851077958109831D0/
      DATA WGK(8)/0.134709217311473325928054001772D0/
      DATA WGK(9)/0.142775938577060080797094273139D0/
      DATA WGK(10)/0.147739104901338491374841515972D0/
      DATA WGK(11)/0.14944555400291690566493646839D0/
      DATA WG(1)/0.0666713443086881375935688098933D0/
      DATA WG(2)/0.149451349150580593145776339658D0/
      DATA WG(3)/0.219086362515982043995534934228D0/
      DATA WG(4)/0.26926671930999635509122692157D0/
      DATA WG(5)/0.295524224714752870173892994651D0/
C
      CALL DQ4NG (EPMACH, UFLOW, OFLOW)
C
      CENTR = 5.0D-01*(A+B)
      HLGTH = 5.0D-01*(B-A)
      DHLGTH = DABS(HLGTH)
C                                  COMPUTE THE 21-POINT KRONROD
C                                  APPROXIMATION TO THE INTEGRAL, AND
C                                  ESTIMATE THE ABSOLUTE ERROR
      RESG = 0.0D+00
      CALL E1USR ('ON')
      FC = F(CENTR)
      CALL E1USR ('OFF')
      RESK = WGK(11)*FC
      RESABS = DABS(RESK)
      DO 10  J=1, 5
         JTW = 2*J
         ABSC = HLGTH*XGK(JTW)
         CALL E1USR ('ON')
         FVAL1 = F(CENTR-ABSC)
         FVAL2 = F(CENTR+ABSC)
         CALL E1USR ('OFF')
         FV1(JTW) = FVAL1
         FV2(JTW) = FVAL2
         FSUM = FVAL1 + FVAL2
         RESG = RESG + WG(J)*FSUM
         RESK = RESK + WGK(JTW)*FSUM
         RESABS = RESABS + WGK(JTW)*(DABS(FVAL1)+DABS(FVAL2))
   10 CONTINUE
      DO 20  J=1, 5
         JTWM1 = 2*J - 1
         ABSC = HLGTH*XGK(JTWM1)
         CALL E1USR ('ON')
         FVAL1 = F(CENTR-ABSC)
         FVAL2 = F(CENTR+ABSC)
         CALL E1USR ('OFF')
         FV1(JTWM1) = FVAL1
         FV2(JTWM1) = FVAL2
         FSUM = FVAL1 + FVAL2
         RESK = RESK + WGK(JTWM1)*FSUM
         RESABS = RESABS + WGK(JTWM1)*(DABS(FVAL1)+DABS(FVAL2))
   20 CONTINUE
      RESKH = RESK*5.0D-01
      RESASC = WGK(11)*DABS(FC-RESKH)
      DO 30  J=1, 10
         RESASC = RESASC + WGK(J)*(DABS(FV1(J)-RESKH)+DABS(FV2(J)-
     &            RESKH))
   30 CONTINUE
      RESULT = RESK*HLGTH
      RESABS = RESABS*DHLGTH
      RESASC = RESASC*DHLGTH
      ABSERR = DABS((RESK-RESG)*HLGTH)
      IF (RESASC.NE.0.0D+00 .AND. ABSERR.NE.0.0D+00) ABSERR = RESASC*
     &    DMIN1(1.0D+00,(2.0D+02*ABSERR/RESASC)**1.5D+00)
      IF (RESABS .GT. UFLOW/(5.0D+01*EPMACH)) ABSERR =
     &    DMAX1((EPMACH*5.0D+01)*RESABS,ABSERR)
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  QDAGS/DQDAGS (Single/Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    January 29, 1985
C
C  Purpose:    Integrate a function (which may have endpoint
C              singularities).
C
C  Usage:      CALL QDAGS (F, A, B, ERRABS, ERRREL, RESULT, ERREST)
C
C  Arguments:
C     F      - User-supplied FUNCTION to be integrated.  The form is
C              F(X), where
C              X      - Independent variable.  (Input)
C              F      - The function value.  (Output)
C              F must be declared EXTERNAL in the calling program.
C     A      - Lower limit of integration.  (Input)
C     B      - Upper limit of integration.  (Input)
C     ERRABS - Absolute accuracy desired.  (Input)
C     ERRREL - Relative accuracy desired.  (Input)
C     RESULT - Estimate of the integral from A to B of F.  (Output)
C     ERREST - Estimate of the absolute value of the error.  (Output)
C
C  Remarks:
C  1. Automatic workspace usage is
C              QDAGS    2500 units, or
C              DQDAGS   4500 units.
C     Workspace may be explicitly provided, if desired, by use of
C     Q2AGS/DQ2AGS.  The reference is
C              CALL Q2AGS (F, A, B, ERRABS, ERRREL, RESULT, ERREST
C                          MAXSUB, NEVAL, NSUBIN, ALIST, BLIST,
C                          RLIST, ELIST, IORD)
C     The additional arguments are as follows:
C     MAXSUB - Number of subintervals allowed.  (Input)
C              A value of 500 is used by QDAGS.
C     NEVAL  - Number of evaluations of F.  (Output)
C     NSUBIN - Number of subintervals generated.  (Output)
C     ALIST  - Array of length MAXSUB containing a list of the NSUBIN
C              left endpoints.  (Output)
C     BLIST  - Array of length MAXSUB containing a list of the NSUBIN
C              right endpoints.  (Output)
C     RLIST  - Array of length MAXSUB containing approximations to
C              the NSUBIN integrals over the intervals defined by ALIST,
C              BLIST.  (Output)
C     ELIST  - Array of length MAXSUB containing the error estimates of
C              the NSUBIN values in RLIST.  (Output)
C     IORD   - Array of length MAXSUB.  (Output)
C              Let K be
C                 NSUBIN          if NSUBIN .LE. (MAXSUB/2+2)
C                 MAXSUB+1-NSUBIN otherwise.
C              The first K locations contain pointers to the error
C              estimates over the subintervals, such that
C              ELIST(IORD(1)), ..., ELIST(IORD(K)) form a decreasing
C              sequence.
C
C  2. Informational errors
C     Type Code
C       4   1  The maximum number of subintervals allowed has been
C              reached.
C       3   2  Roundoff error, preventing the requested tolerance from
C              being achieved, has been detected.
C       3   3  A degradation in precision has been detected.
C       3   4  Roundoff error in the extrapolation table, preventing
C              the requested tolerance from being achieved, has been
C              detected.
C       4   5  Integral is probably divergent or slowly convergent.
C
C  3. If EXACT is the exact value, QDAGS attempts to find RESULT
C     such that ABS(EXACT-RESULT) .LE. MAX(ERRABS,ERRREL*ABS(EXACT)).
C     To specify only a relative error, set ERRABS to zero.  Similarly,
C     to specify only an absolute error, set ERRREL to zero.
C
C  GAMS:       H2a1a1
C
C  Chapters:   MATH/LIBRARY Integration and Differentiation
C              STAT/LIBRARY Mathematical Support
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DQDAGS (F, A, B, ERRABS, ERRREL, RESULT, ERREST)
C                                  SPECIFICATIONS FOR ARGUMENTS
      DOUBLE PRECISION F, A, B, ERRABS, ERRREL, RESULT, ERREST
      EXTERNAL   F
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    IIWK, IRWK, KALIST, KBLIST, KELIST, KIORD, KRLIST,
     &           MAXSUB, NEVAL, NSUBIN
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                                  SPECIFICATIONS FOR COMMON /WORKSP/
      REAL       RWKSP(5000)
      DOUBLE PRECISION RDWKSP(2500)
      DOUBLE PRECISION DWKSP(2500)
      COMPLEX    CWKSP(2500)
      COMPLEX    *16 CZWKSP(1250)
      COMPLEX    *16 ZWKSP(1250)
      INTEGER    IWKSP(5000)
      LOGICAL    LWKSP(5000)
      EQUIVALENCE (DWKSP(1), RWKSP(1))
      EQUIVALENCE (CWKSP(1), RWKSP(1)), (ZWKSP(1), RWKSP(1))
      EQUIVALENCE (IWKSP(1), RWKSP(1)), (LWKSP(1), RWKSP(1))
      EQUIVALENCE (RDWKSP(1), RWKSP(1)), (CZWKSP(1), RWKSP(1))
      COMMON     /WORKSP/ DWKSP
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1POP, E1PSH, DQ2AGS
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   I1KGT, N1RTY
      INTEGER    I1KGT, N1RTY
C
      CALL E1PSH ('DQDAGS ')
C                                  ALLOCATE WORKSPACE
      MAXSUB = 500
      IRWK = I1KGT(4*MAXSUB,4)
      IIWK = I1KGT(MAXSUB,2)
      IF (N1RTY(0) .NE. 0) GO TO 9000
C                                  PARTITION WORKSPACE
      KALIST = IRWK
      KBLIST = KALIST + MAXSUB
      KRLIST = KBLIST + MAXSUB
      KELIST = KRLIST + MAXSUB
      KIORD = IIWK
C
      CALL DQ2AGS (F, A, B, ERRABS, ERRREL, RESULT, ERREST, MAXSUB,
     &             NEVAL, NSUBIN, RDWKSP(KALIST), RDWKSP(KBLIST),
     &             RDWKSP(KRLIST), RDWKSP(KELIST), IWKSP(KIORD))
C
 9000 CONTINUE
      CALL E1POP ('DQDAGS ')
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  DQDDOT (Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    August 9, 1986
C
C  Purpose:    Compute the sum of a double-precision scalar and a
C              double-precision dot product, a + x*y, using an
C              extended-precision accumulator.
C
C  Usage:      DQDDOT(N, DB, DX, INCX, DY, INCY)
C
C  Arguments:
C     N      - Length of vectors DX and DY.  (Input)
C     DB     - Double precision scalar.  (Input)
C     DX     - Double precision vector of length MAX(N*IABS(INCX),1).
C              (Input)
C     INCX   - Displacenent between elements of DX.  (Input)
C              DX(I) is devined to be
C                 DX(1+(I-1)*INCX) if INCX.GE.0 or
C                 DX(1+(I-N)*INCX) if INCX.LT.0.
C     DY     - Double precision vector of length MAX(N*IABS(INCY),1).
C              (Input)
C     INCY   - Displacement between elements of DY.  (Input)
C              DY(I) is defined to be
C                 DY(1+(I-1)*INCY) if INCY.GE.0 or
C                 DY(1+(I-N)*INCY) if INCY.LT.0.
C     DQDDOT - Sum from I=1 to N of DX(I)*DY(I) + DB.  (Output)
C
C  Keyword:    Level 1 BLAS; Inner product; Scalar product
C
C  GAMS:       D1a4
C
C  Chapters:   MATH/LIBRARY Basic Matrix/Vector Operations
C              STAT/LIBRARY Mathematical Support
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION DQDDOT (N, DB, DX, INCX, DY, INCY)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, INCX, INCY
      DOUBLE PRECISION DB, DX(*), DY(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, KX, KY, NS
      DOUBLE PRECISION ACC(2), DSUM
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   DQADD, DQMUL, DQSTO
C
      ACC(1) = 0.0D0
      ACC(2) = 0.0D0
      CALL DQADD (DB, ACC)
      CALL DQSTO (ACC, DSUM)
      IF (N .GT. 0) THEN
         IF (INCX.NE.INCY .OR. INCX.LE.0) THEN
            KX = 1
            KY = 1
            IF (INCX .LT. 0) KX = 1 + (1-N)*INCX
            IF (INCY .LT. 0) KY = 1 + (1-N)*INCY
            DO 10  I=1, N
               CALL DQMUL (DX(KX), DY(KY), ACC)
               KX = KX + INCX
               KY = KY + INCY
   10       CONTINUE
         ELSE
            NS = N*INCX
            DO 20  I=1, NS, INCX
               CALL DQMUL (DX(I), DY(I), ACC)
   20       CONTINUE
         END IF
         CALL DQSTO (ACC, DSUM)
      END IF
      DQDDOT = DSUM
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  DSCAL (Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    August 9, 1986
C
C  Purpose:    Multiply a vector by a scalar, y = ay, both double
C              precision.
C
C  Usage:      CALL DSCAL (N, DA, DX, INCX)
C
C  Arguments:
C     N      - Length of vector X.  (Input)
C     DA     - Double precision scalar.  (Input)
C     DX     - Double precision vector of length N*INCX.  (Input/Output)
C              DSCAL replaces X(I) with DA*X(I) for I=1,...,N. X(I)
C              refers to a specific element of DX. See INCX argument
C              description.
C     INCX   - Displacement between elements of DX.  (Input)
C              X(I) is defined to be DX(1+(I-1)*INCX). INCX must be
C              greater than zero.
C
C  GAMS:       D1a6
C
C  Chapters:   MATH/LIBRARY Basic Matrix/Vector Operations
C              STAT/LIBRARY Mathematical Support
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DSCAL (N, DA, DX, INCX)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, INCX
      DOUBLE PRECISION DA, DX(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, M, MP1, NS
C
      IF (N .GT. 0) THEN
         IF (INCX .NE. 1) THEN
C                                  CODE FOR INCREMENTS NOT EQUAL TO 1.
            NS = N*INCX
            DO 10  I=1, NS, INCX
               DX(I) = DA*DX(I)
   10       CONTINUE
         ELSE
C                                  CODE FOR INCREMENTS EQUAL TO 1.
C                                  CLEAN-UP LOOP SO REMAINING VECTOR
C                                  LENGTH IS A MULTIPLE OF 5.
            M = N - (N/5)*5
            DO 30  I=1, M
               DX(I) = DA*DX(I)
   30       CONTINUE
            MP1 = M + 1
            DO 40  I=MP1, N, 5
               DX(I) = DA*DX(I)
               DX(I+1) = DA*DX(I+1)
               DX(I+2) = DA*DX(I+2)
               DX(I+3) = DA*DX(I+3)
               DX(I+4) = DA*DX(I+4)
   40       CONTINUE
         END IF
      END IF
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  DSET (Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    August 9, 1986
C
C  Purpose:    Set the components of a vector to a scalar, all double
C              precision.
C
C  Usage:      CALL DSET (N, DA, DX, INCX)
C
C  Arguments:
C     N      - Length of vector X.  (Input)
C     DA     - Double precision scalar.  (Input)
C     DX     - Double precison vector of length N*INCX.  (Input/Output)
C              DSET replaces X(I) with DA for I=1,...,N.  X(I) refers to
C              a specific element of DX. See INCX argument description.
C     INCX   - Displacement between elements of DX.  (Input)
C              X(I) is defined to be DX(1+(I-1)*INCX).  INCX must be
C              greater than zero.
C
C  GAMS:       D1a1
C
C  Chapters:   MATH/LIBRARY Basic Matrix/Vector Operations
C              STAT/LIBRARY Mathematical Support
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DSET (N, DA, DX, INCX)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, INCX
      DOUBLE PRECISION DA, DX(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, M, MP1, NINCX
C                                  SPECIFICATIONS FOR SPECIAL CASES
C     INTRINSIC  MOD
      INTRINSIC  MOD
      INTEGER    MOD
C
      IF (N .GT. 0) THEN
         IF (INCX .NE. 1) THEN
C                                  CODE FOR INCREMENT NOT EQUAL TO 1
            NINCX = N*INCX
            DO 10  I=1, NINCX, INCX
               DX(I) = DA
   10       CONTINUE
         ELSE
C                                  CODE FOR INCREMENT EQUAL TO 1
            M = MOD(N,8)
C                                  CLEAN-UP LOOP
            DO 30  I=1, M
               DX(I) = DA
   30       CONTINUE
            MP1 = M + 1
            DO 40  I=MP1, N, 8
               DX(I) = DA
               DX(I+1) = DA
               DX(I+2) = DA
               DX(I+3) = DA
               DX(I+4) = DA
               DX(I+5) = DA
               DX(I+6) = DA
               DX(I+7) = DA
   40       CONTINUE
         END IF
      END IF
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  E1INIT
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    March 13, 1984
C
C  Purpose:    Initialization.
C
C  Usage:      CALL E1INIT
C
C  Arguments:  None
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE E1INIT
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    L
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      INTEGER    ISINIT
      SAVE       ISINIT
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM1/
      INTEGER    CALLVL, MAXLEV, MSGLEN, ERTYPE(51), ERCODE(51),
     &           PRINTB(7), STOPTB(7), PLEN, IFERR6, IFERR7,
     &           IALLOC(51), HDRFMT(7), TRACON(7)
      COMMON     /ERCOM1/ CALLVL, MAXLEV, MSGLEN, ERTYPE, ERCODE,
     &           PRINTB, STOPTB, PLEN, IFERR6, IFERR7, IALLOC, HDRFMT,
     &           TRACON
      SAVE       /ERCOM1/
C                              SPECIFICATIONS FOR COMMON /ERCOM2/
      CHARACTER  MSGSAV(255), PLIST(300), RNAME(51)*6
      COMMON     /ERCOM2/ MSGSAV, PLIST, RNAME
      SAVE       /ERCOM2/
C                              SPECIFICATIONS FOR COMMON /ERCOM3/
      DOUBLE PRECISION ERCKSM
      COMMON     /ERCOM3/ ERCKSM
      SAVE       /ERCOM3/
C                              SPECIFICATIONS FOR COMMON /ERCOM4/
      LOGICAL    ISUSER(51)
      COMMON     /ERCOM4/ ISUSER
      SAVE       /ERCOM4/
C                              SPECIFICATIONS FOR COMMON /ERCOM8/
      INTEGER    PROLVL, XXLINE(10), XXPLEN(10), ICALOC(10), INALOC(10)
      COMMON     /ERCOM8/ PROLVL, XXLINE, XXPLEN, ICALOC, INALOC
      SAVE       /ERCOM8/
C                              SPECIFICATIONS FOR COMMON /ERCOM9/
      CHARACTER  XXPROC(10)*31
      COMMON     /ERCOM9/ XXPROC
      SAVE       /ERCOM9/
C
      DATA ISINIT/0/
C
      IF (ISINIT .EQ. 0) THEN
C                                  INITIALIZE
         CALLVL = 1
         ERCODE(1) = 0
         ERTYPE(1) = 0
         IALLOC(1) = 0
         ISUSER(1) = .TRUE.
         IFERR6 = 0
         IFERR7 = 0
         PLEN = 1
         MAXLEV = 50
         DO 10  L=2, 51
            ERTYPE(L) = -1
            ERCODE(L) = -1
            IALLOC(L) = 0
            ISUSER(L) = .FALSE.
   10    CONTINUE
         DO 20  L=1, 7
            HDRFMT(L) = 1
            TRACON(L) = 1
   20    CONTINUE
         PROLVL = 1
         DO 30  L=1, 10
   30    ICALOC(L) = 0
         XXLINE(1) = 0
         XXPLEN(1) = 1
         XXPROC(1) = '?'
         RNAME(1) = 'USER'
         PRINTB(1) = 0
         PRINTB(2) = 0
         DO 40  L=3, 7
   40    PRINTB(L) = 1
         STOPTB(1) = 0
         STOPTB(2) = 0
         STOPTB(3) = 0
         STOPTB(4) = 1
         STOPTB(5) = 1
         STOPTB(6) = 0
         STOPTB(7) = 1
         ERCKSM = 0.0D0
C                                  SET FLAG TO INDICATE THAT
C                                    INITIALIZATION HAS OCCURRED
         ISINIT = 1
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  E1INPL
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    March 2, 1984
C
C  Purpose:    To store a character string in the parameter list PLIST
C              for use by the error message handler.
C
C  Usage:      CALL E1INPL(FORM,NUM,SLEN,STRUP)
C
C  Arguments:
C     FORM   - A character string of length one to be inserted into
C              PLIST which specifies the form of the string.  (Input)
C              For example, 'L' for string, 'A' for character array,
C              'I' for integer, 'K' for keyword (PROTRAN only).  An
C              asterisk is inserted into PLIST preceding FORM.
C     NUM    - Integer to be inserted as a character into PLIST
C              immediately following FORM.  (Input)  NUM must be between
C              1 and 9.
C     SLEN   - The number of characters in STRUP.  (Input)  LEN must be
C              less than or equal to 255.  The character representation
C              of SLEN is inserted into PLIST after NUM and an asterisk.
C     STRUP  - A character string of length LEN which is to be inserted
C              into PLIST.  (Input)  Trailing blanks are ignored.
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE E1INPL (FORM, NUM, SLEN, STRUP)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    NUM, SLEN
      CHARACTER  FORM, STRUP(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    IER, L, LEN2, LENCK, LOC, NLEN, NNUM
      CHARACTER  STRNCH(3)
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      CHARACTER  BLANK, PRCNT(1), TEMP(4)
      SAVE       BLANK, PRCNT, TEMP
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM1/
      INTEGER    CALLVL, MAXLEV, MSGLEN, ERTYPE(51), ERCODE(51),
     &           PRINTB(7), STOPTB(7), PLEN, IFERR6, IFERR7,
     &           IALLOC(51), HDRFMT(7), TRACON(7)
      COMMON     /ERCOM1/ CALLVL, MAXLEV, MSGLEN, ERTYPE, ERCODE,
     &           PRINTB, STOPTB, PLEN, IFERR6, IFERR7, IALLOC, HDRFMT,
     &           TRACON
      SAVE       /ERCOM1/
C                              SPECIFICATIONS FOR COMMON /ERCOM2/
      CHARACTER  MSGSAV(255), PLIST(300), RNAME(51)*6
      COMMON     /ERCOM2/ MSGSAV, PLIST, RNAME
      SAVE       /ERCOM2/
C                              SPECIFICATIONS FOR COMMON /ERCOM3/
      DOUBLE PRECISION ERCKSM
      COMMON     /ERCOM3/ ERCKSM
      SAVE       /ERCOM3/
C                              SPECIFICATIONS FOR COMMON /ERCOM4/
      LOGICAL    ISUSER(51)
      COMMON     /ERCOM4/ ISUSER
      SAVE       /ERCOM4/
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  IABS
      INTRINSIC  IABS
      INTEGER    IABS
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   C1TIC, M1VE
C
      DATA TEMP/'*', ' ', ' ', '*'/, PRCNT/'%'/, BLANK/' '/
C
      NNUM = IABS(NUM)
      LENCK = PLEN + SLEN + 8
      IF (NNUM.GE.1 .AND. NNUM.LE.9 .AND. LENCK.LE.300) THEN
         TEMP(2) = FORM
         CALL C1TIC (NNUM, TEMP(3), 1, IER)
         LOC = PLEN + 1
         IF (LOC .EQ. 2) LOC = 1
         CALL M1VE (TEMP, 1, 4, 4, PLIST(LOC), 1, 4, 262, IER)
         LOC = LOC + 4
         IF (NUM .LT. 0) THEN
            LEN2 = SLEN
         ELSE
            DO 10  L=1, SLEN
               LEN2 = SLEN - L + 1
               IF (STRUP(LEN2) .NE. BLANK) GO TO 20
   10       CONTINUE
            LEN2 = 1
   20       CONTINUE
         END IF
         NLEN = 1
         IF (LEN2 .GE. 10) NLEN = 2
         IF (LEN2 .GE. 100) NLEN = 3
         CALL C1TIC (LEN2, STRNCH, NLEN, IER)
         CALL M1VE (STRNCH, 1, NLEN, 3, PLIST(LOC), 1, NLEN, 262, IER)
         LOC = LOC + NLEN
         CALL M1VE (PRCNT, 1, 1, 1, PLIST(LOC), 1, 1, 262, IER)
         LOC = LOC + 1
         CALL M1VE (STRUP, 1, LEN2, LEN2, PLIST(LOC), 1, LEN2, 262,
     &              IER)
         PLEN = LOC + LEN2 - 1
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  E1MES
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    March 2, 1984
C
C  Purpose:    Set an error state for the current level in the stack.
C              The message is printed immediately if the error type is
C              5, 6, or 7 and the print attribute for that type is YES.
C
C  Usage:      CALL E1MES(IERTYP,IERCOD,MSGPKD)
C
C  Arguments:
C     IERTYP - Integer specifying the error type.  (Input)
C                IERTYP=1,  informational/note
C                IERTYP=2,  informational/alert
C                IERTYP=3,  informational/warning
C                IERTYP=4,  informational/fatal
C                IERTYP=5,  terminal
C                IERTYP=6,  PROTRAN/warning
C                IERTYP=7,  PROTRAN/fatal
C     IERCOD - Integer specifying the error code.  (Input)
C     MSGPKD - A character string containing the message.
C              (Input)  Within the message, any of following may appear
C                %(A1),%(A2),...,%(A9) for character arrays
C                %(C1),%(C2),...,%(C9) for complex numbers
C                %(D1),%(D2),...,%(D9) for double precision numbers
C                %(I1),%(I2),...,%(I9) for integer numbers
C                %(K1),%(K2),...,%(K9) for keywords
C                %(L1),%(L2),...,%(L9) for literals (strings)
C                %(R1),%(R2),...,%(R9) for real numbers
C                %(Z1),%(Z2),...,%(Z9) for double complex numbers
C              This provides a way to insert character arrays, strings,
C              numbers, and keywords into the message.  See remarks
C              below.
C
C  Remarks:
C     The number of characters in the message after the insertion of
C     the corresponding strings, etc. should not exceed 255.  If the
C     limit is exceeded, only the first 255 characters will be used.
C     The appropriate strings, etc. need to have been previously stored
C     in common via calls to E1STA, E1STD, etc.  Line breaks may be
C     specified by inserting the two characters '%/' into the message
C     at the desired locations.
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE E1MES (IERTYP, IERCOD, MSGPKD)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    IERTYP, IERCOD
      CHARACTER  MSGPKD*(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    ERTYP2, I, IER, IPLEN, ISUB, LAST, LEN2, LOC, M, MS,
     &           NLOC, NUM, PBEG
      CHARACTER  MSGTMP(255)
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      INTEGER    IFINIT, NFORMS
      CHARACTER  BLNK, DBB(3), FIND(4), FORMS(9), INREF(25), LPAR,
     &           NCHECK(3), PERCNT, RPAR
      SAVE       BLNK, DBB, FIND, FORMS, IFINIT, INREF, LPAR, NCHECK,
     &           NFORMS, PERCNT, RPAR
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM1/
      INTEGER    CALLVL, MAXLEV, MSGLEN, ERTYPE(51), ERCODE(51),
     &           PRINTB(7), STOPTB(7), PLEN, IFERR6, IFERR7,
     &           IALLOC(51), HDRFMT(7), TRACON(7)
      COMMON     /ERCOM1/ CALLVL, MAXLEV, MSGLEN, ERTYPE, ERCODE,
     &           PRINTB, STOPTB, PLEN, IFERR6, IFERR7, IALLOC, HDRFMT,
     &           TRACON
      SAVE       /ERCOM1/
C                              SPECIFICATIONS FOR COMMON /ERCOM2/
      CHARACTER  MSGSAV(255), PLIST(300), RNAME(51)*6
      COMMON     /ERCOM2/ MSGSAV, PLIST, RNAME
      SAVE       /ERCOM2/
C                              SPECIFICATIONS FOR COMMON /ERCOM3/
      DOUBLE PRECISION ERCKSM
      COMMON     /ERCOM3/ ERCKSM
      SAVE       /ERCOM3/
C                              SPECIFICATIONS FOR COMMON /ERCOM4/
      LOGICAL    ISUSER(51)
      COMMON     /ERCOM4/ ISUSER
      SAVE       /ERCOM4/
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  LEN,MIN0
      INTRINSIC  LEN, MIN0
      INTEGER    LEN, MIN0
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   C1TCI, E1INIT, E1PRT, E1UCS, M1VE, M1VECH
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   I1DX
      INTEGER    I1DX
C
      DATA FORMS/'A', 'C', 'D', 'I', 'K', 'L', 'R', 'S', 'Z'/,
     &     NFORMS/9/
      DATA PERCNT/'%'/, LPAR/'('/, RPAR/')'/, BLNK/' '/
      DATA INREF/' ', 'i', 'n', ' ', 'r', 'e', 'f', 'e', 'r',
     &     'e', 'n', 'c', 'e', ' ', 't', 'o', ' ', 'k', 'e',
     &     'y', 'w', 'o', 'r', 'd', ' '/
      DATA NCHECK/'N', '1', '*'/, DBB/'.', ' ', ' '/
      DATA FIND/'*', ' ', ' ', '*'/
      DATA IFINIT/0/
C                                  INITIALIZE ERROR TABLE IF NECESSARY
      IF (IFINIT .EQ. 0) THEN
         CALL E1INIT
         IFINIT = 1
      END IF
C                                  CHECK AND SET ERROR TYPE IF NECESSARY
      IF (IERTYP .NE. -1) THEN
         ERTYPE(CALLVL) = IERTYP
      ELSE IF (IERTYP.LT.-1 .OR. IERTYP.GT.7) THEN
         MSGLEN = 51
         CALL M1VECH ('.  Error from E1MES.  Illegal error type'//
     &                ' specified. ', MSGLEN, MSGSAV, MSGLEN)
         CALL E1PRT
         STOP
      END IF
C
      ERTYP2 = ERTYPE(CALLVL)
C                                  SET ERROR CODE IF NECESSARY
      IF (IERCOD .GT. -1) ERCODE(CALLVL) = IERCOD
      LEN2 = LEN(MSGPKD)
C
      IF (IERTYP.EQ.0 .OR. IERCOD.EQ.0) THEN
C                                  REMOVE THE ERROR STATE
         MSGLEN = 0
      ELSE IF (LEN2.EQ.0 .OR. (LEN2.EQ.1.AND.MSGPKD(1:1).EQ.BLNK)) THEN
         IF (ERTYP2 .EQ. 6) IFERR6 = 1
         IF (ERTYP2 .EQ. 7) IFERR7 = 1
C                                  UPDATE CHECKSUM PARAMETER ERCKSM
         CALL E1UCS
C                                  PRINT MESSAGE IF NECESSARY
         IF (ERTYP2.GE.5 .AND. PRINTB(ERTYP2).EQ.1) CALL E1PRT
      ELSE
C                                  FILL UP MSGSAV WITH EXPANDED MESSAGE
         LEN2 = MIN0(LEN2,255)
         DO 10  I=1, LEN2
            MSGTMP(I) = MSGPKD(I:I)
   10    CONTINUE
         MS = 0
         M = 0
C                                  CHECK PLIST FOR KEYWORD NAME
         NLOC = I1DX(PLIST,PLEN,NCHECK,3)
         IF (NLOC.GT.0 .AND. HDRFMT(ERTYP2).EQ.3) THEN
C                                  M1VE INREF INTO MSGSAV
            CALL M1VE (INREF, 1, 25, 25, MSGSAV, 1, 25, 25, IER)
C                                  GET LENGTH OF KEYWORD NAME
            CALL C1TCI (PLIST(NLOC+3), 3, IPLEN, IER)
            PBEG = NLOC + 3 + IER
C                                  M1VE KEYWORD NAME INTO MSGSAV
            CALL M1VE (PLIST, PBEG, PBEG+IPLEN-1, PLEN, MSGSAV, 26,
     &                 IPLEN+25, 255, IER)
C                                  UPDATE POINTER
            MS = IPLEN + 25
         END IF
C                                  INSERT DOT, BLANK, BLANK
         CALL M1VE (DBB, 1, 3, 3, MSGSAV, MS+1, MS+3, 255, IER)
         MS = MS + 3
C                                  LOOK AT NEXT CHARACTER
   20    M = M + 1
         ISUB = 0
         IF (M .GT. LEN2-4) THEN
            LAST = LEN2 - M + 1
            DO 30  I=1, LAST
   30       MSGSAV(MS+I) = MSGTMP(M+I-1)
            MSGLEN = MS + LAST
            GO TO 40
         ELSE IF (MSGTMP(M).EQ.PERCNT .AND. MSGTMP(M+1).EQ.LPAR .AND.
     &           MSGTMP(M+4).EQ.RPAR) THEN
            CALL C1TCI (MSGTMP(M+3), 1, NUM, IER)
            IF (IER.EQ.0 .AND. NUM.NE.0 .AND. I1DX(FORMS,NFORMS,
     &          MSGTMP(M+2),1).NE.0) THEN
C                                  LOCATE THE ITEM IN THE PARAMETER LIST
               CALL M1VE (MSGTMP(M+2), 1, 2, 2, FIND, 2, 3, 4, IER)
               LOC = I1DX(PLIST,PLEN,FIND,4)
               IF (LOC .GT. 0) THEN
C                                  SET IPLEN = LENGTH OF STRING
                  CALL C1TCI (PLIST(LOC+4), 4, IPLEN, IER)
                  PBEG = LOC + 4 + IER
C                                  ADJUST IPLEN IF IT IS TOO BIG
                  IPLEN = MIN0(IPLEN,255-MS)
C                                  M1VE STRING FROM PLIST INTO MSGSAV
                  CALL M1VE (PLIST, PBEG, PBEG+IPLEN-1, PLEN, MSGSAV,
     &                       MS+1, MS+IPLEN, 255, IER)
                  IF (IER.GE.0 .AND. IER.LT.IPLEN) THEN
C                                  UPDATE POINTERS
                     M = M + 4
                     MS = MS + IPLEN - IER
C                                  BAIL OUT IF NO MORE ROOM
                     IF (MS .GE. 255) THEN
                        MSGLEN = 255
                        GO TO 40
                     END IF
C                                  SET FLAG TO SHOW SUBSTITION WAS MADE
                     ISUB = 1
                  END IF
               END IF
            END IF
         END IF
         IF (ISUB .EQ. 0) THEN
            MS = MS + 1
            MSGSAV(MS) = MSGTMP(M)
         END IF
         GO TO 20
   40    ERTYP2 = ERTYPE(CALLVL)
         IF (ERTYP2 .EQ. 6) IFERR6 = 1
         IF (ERTYP2 .EQ. 7) IFERR7 = 1
C                                  UPDATE CHECKSUM PARAMETER ERCKSM
         CALL E1UCS
C                                  PRINT MESSAGE IF NECESSARY
         IF (ERTYP2.GE.5 .AND. PRINTB(ERTYP2).EQ.1) CALL E1PRT
      END IF
C                                  CLEAR PARAMETER LIST
      PLEN = 1
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  E1POP
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    March 13, 1984
C
C  Purpose:    To pop a subroutine name from the error control stack.
C
C  Usage:      CALL E1POP(NAME)
C
C  Arguments:
C     NAME   - A character string of length six specifying the name
C              of the subroutine.  (Input)
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE E1POP (NAME)
C                                  SPECIFICATIONS FOR ARGUMENTS
      CHARACTER  NAME*(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    IERTYP, IR
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM1/
      INTEGER    CALLVL, MAXLEV, MSGLEN, ERTYPE(51), ERCODE(51),
     &           PRINTB(7), STOPTB(7), PLEN, IFERR6, IFERR7,
     &           IALLOC(51), HDRFMT(7), TRACON(7)
      COMMON     /ERCOM1/ CALLVL, MAXLEV, MSGLEN, ERTYPE, ERCODE,
     &           PRINTB, STOPTB, PLEN, IFERR6, IFERR7, IALLOC, HDRFMT,
     &           TRACON
      SAVE       /ERCOM1/
C                              SPECIFICATIONS FOR COMMON /ERCOM2/
      CHARACTER  MSGSAV(255), PLIST(300), RNAME(51)*6
      COMMON     /ERCOM2/ MSGSAV, PLIST, RNAME
      SAVE       /ERCOM2/
C                              SPECIFICATIONS FOR COMMON /ERCOM3/
      DOUBLE PRECISION ERCKSM
      COMMON     /ERCOM3/ ERCKSM
      SAVE       /ERCOM3/
C                              SPECIFICATIONS FOR COMMON /ERCOM4/
      LOGICAL    ISUSER(51)
      COMMON     /ERCOM4/ ISUSER
      SAVE       /ERCOM4/
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1PRT, E1PSH, E1STI, E1STL, I1KRL
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   I1KST
      INTEGER    I1KST
C
      IF (CALLVL .LE. 1) THEN
         CALL E1PSH ('E1POP ')
         CALL E1STL (1, NAME)
         CALL E1MES (5, 1, 'Error condition in E1POP.  Cannot pop '//
     &               'from %(L1) because stack is empty.')
         STOP
      ELSE IF (NAME .NE. RNAME(CALLVL)) THEN
         CALL E1STL (1, NAME)
         CALL E1STL (2, RNAME(CALLVL))
         CALL E1MES (5, 2, 'Error condition in E1POP.  %(L1) does '//
     &               'not match the name %(L2) in the stack.')
         STOP
      ELSE
         IERTYP = ERTYPE(CALLVL)
         IF (IERTYP .NE. 0) THEN
C                                  M1VE ERROR TYPE AND ERROR CODE TO
C                                    PREVIOUS LEVEL FOR ERROR TYPES 2-7
            IF (IERTYP.GE.2 .AND. IERTYP.LE.7) THEN
               ERTYPE(CALLVL-1) = ERTYPE(CALLVL)
               ERCODE(CALLVL-1) = ERCODE(CALLVL)
            END IF
C                                  CHECK PRINT TABLE TO DETERMINE
C                                    WHETHER TO PRINT STORED MESSAGE
            IF (IERTYP .LE. 4) THEN
               IF (ISUSER(CALLVL-1) .AND. PRINTB(IERTYP).EQ.1)
     &             CALL E1PRT
            ELSE
               IF (PRINTB(IERTYP) .EQ. 1) CALL E1PRT
            END IF
C                                  CHECK STOP TABLE AND ERROR TYPE TO
C                                    DETERMINE WHETHER TO STOP
            IF (IERTYP .LE. 4) THEN
               IF (ISUSER(CALLVL-1) .AND. STOPTB(IERTYP).EQ.1) THEN
                  STOP
               END IF
            ELSE IF (IERTYP .EQ. 5) THEN
               IF (STOPTB(IERTYP) .EQ. 1) THEN
                  STOP
               END IF
            ELSE IF (HDRFMT(IERTYP) .EQ. 1) THEN
               IF (ISUSER(CALLVL-1)) THEN
                  IF (N1RGB(0) .NE. 0) THEN
                     STOP
                  END IF
               END IF
            END IF
         END IF
C                                  SET ERROR TYPE AND CODE
         IF (CALLVL .LT. MAXLEV) THEN
            ERTYPE(CALLVL+1) = -1
            ERCODE(CALLVL+1) = -1
         END IF
C                                  SET IR = AMOUNT OF WORKSPACE
C                                  ALLOCATED AT THIS LEVEL
         IR = I1KST(1) - IALLOC(CALLVL-1)
         IF (IR .GT. 0) THEN
C                                  RELEASE WORKSPACE
            CALL I1KRL (IR)
            IALLOC(CALLVL) = 0
         ELSE IF (IR .LT. 0) THEN
            CALL E1STI (1, CALLVL)
            CALL E1STI (2, IALLOC(CALLVL-1))
            CALL E1STI (3, I1KST(1))
            CALL E1MES (5, 3, 'Error condition in E1POP. '//
     &                  ' The number of workspace allocations at '//
     &                  'level %(I1) is %(I2).  However, the total '//
     &                  'number of workspace allocations is %(I3).')
            STOP
         END IF
C                                  DECREASE THE STACK POINTER BY ONE
         CALLVL = CALLVL - 1
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  E1POS
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    March 2, 1984
C
C  Purpose:    Set or retrieve print and stop attributes.
C
C  Usage:      CALL E1POS(IERTYP,IPATT,ISATT)
C
C  Arguments:
C     IERTYP - Integer specifying the error type for which print and
C              stop attributes are to be set or retrieved.  (Input)  If
C              IERTYP is 0 then the settings apply to all error types.
C              If IERTYP is between 1 and 7, then the settings only
C              apply to that specified error type.  If IERTYP is
C              negative then the current print and stop attributes will
C              be returned in IPATT and ISATT.
C     IPATT  - If IERTYP is positive, IPATT is an integer specifying the
C              desired print attribute as follows: -1 means no change,
C              0 means NO, 1 means YES, and 2 means assign the default
C              setting.  (Input)  If IERTYP is negative, IPATT is
C              returned as 1 if print is YES or 0 if print is NO for
C              error type IABS(IERTYP).  (Output)
C     ISATT  - If IERTYP is positive, ISATT is an integer specifying the
C              desired stop attribute as follows: -1 means no change,
C              0 means NO, 1 means YES, and 2 means assign the default
C              setting.  (Input)  If IERTYP is negative, ISATT is
C              returned as 1 if print is YES or 0 if print is NO for
C              error type IABS(IERTYP).  (Output)
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE E1POS (IERTYP, IPATT, ISATT)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    IERTYP, IPATT, ISATT
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, IER
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      INTEGER    DEFLTP(7), DEFLTS(7), IFINIT
      SAVE       DEFLTP, DEFLTS, IFINIT
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM1/
      INTEGER    CALLVL, MAXLEV, MSGLEN, ERTYPE(51), ERCODE(51),
     &           PRINTB(7), STOPTB(7), PLEN, IFERR6, IFERR7,
     &           IALLOC(51), HDRFMT(7), TRACON(7)
      COMMON     /ERCOM1/ CALLVL, MAXLEV, MSGLEN, ERTYPE, ERCODE,
     &           PRINTB, STOPTB, PLEN, IFERR6, IFERR7, IALLOC, HDRFMT,
     &           TRACON
      SAVE       /ERCOM1/
C                              SPECIFICATIONS FOR COMMON /ERCOM2/
      CHARACTER  MSGSAV(255), PLIST(300), RNAME(51)*6
      COMMON     /ERCOM2/ MSGSAV, PLIST, RNAME
      SAVE       /ERCOM2/
C                              SPECIFICATIONS FOR COMMON /ERCOM3/
      DOUBLE PRECISION ERCKSM
      COMMON     /ERCOM3/ ERCKSM
      SAVE       /ERCOM3/
C                              SPECIFICATIONS FOR COMMON /ERCOM4/
      LOGICAL    ISUSER(51)
      COMMON     /ERCOM4/ ISUSER
      SAVE       /ERCOM4/
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  IABS
      INTRINSIC  IABS
      INTEGER    IABS
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1INIT, E1MES, E1STI
C
      DATA IFINIT/0/
      DATA DEFLTP/0, 0, 1, 1, 1, 1, 1/, DEFLTS/0, 0, 0, 1, 1, 0, 1/
C                                  INITIALIZE ERROR TABLE IF NECESSARY
      IF (IFINIT .EQ. 0) THEN
         CALL E1INIT
         IFINIT = 1
      END IF
      IER = 0
      IF (IERTYP .GE. 0) THEN
         IF (IPATT.LT.-1 .OR. IPATT.GT.2) THEN
            CALL E1STI (1, IPATT)
            CALL E1MES (5, 1, 'Invalid value specified for print '//
     &                  'table attribute.  IPATT must be -1, 0, 1, '//
     &                  'or 2.  IPATT = %(I1)')
            IER = 1
         END IF
         IF (ISATT.LT.-1 .OR. ISATT.GT.2) THEN
            CALL E1STI (1, ISATT)
            CALL E1MES (5, 1, 'Invalid value specified for stop '//
     &                  'table attribute.  ISATT must be -1, 0, 1, '//
     &                  'or 2.  ISATT = %(I1)')
            IER = 1
         END IF
      END IF
      IF (IER .EQ. 0) THEN
         IF (IERTYP .EQ. 0) THEN
            IF (IPATT.EQ.0 .OR. IPATT.EQ.1) THEN
               DO 10  I=1, 7
   10          PRINTB(I) = IPATT
            ELSE IF (IPATT .EQ. 2) THEN
C                                  ASSIGN DEFAULT SETTINGS
               DO 20  I=1, 7
   20          PRINTB(I) = DEFLTP(I)
            END IF
            IF (ISATT.EQ.0 .OR. ISATT.EQ.1) THEN
               DO 30  I=1, 7
   30          STOPTB(I) = ISATT
            ELSE IF (ISATT .EQ. 2) THEN
C                                  ASSIGN DEFAULT SETTINGS
               DO 40  I=1, 7
   40          STOPTB(I) = DEFLTS(I)
            END IF
         ELSE IF (IERTYP.GE.1 .AND. IERTYP.LE.7) THEN
            IF (IPATT.EQ.0 .OR. IPATT.EQ.1) THEN
               PRINTB(IERTYP) = IPATT
            ELSE IF (IPATT .EQ. 2) THEN
C                                  ASSIGN DEFAULT SETTING
               PRINTB(IERTYP) = DEFLTP(IERTYP)
            END IF
            IF (ISATT.EQ.0 .OR. ISATT.EQ.1) THEN
               STOPTB(IERTYP) = ISATT
            ELSE IF (ISATT .EQ. 2) THEN
C                                  ASSIGN DEFAULT SETTING
               STOPTB(IERTYP) = DEFLTS(IERTYP)
            END IF
         ELSE IF (IERTYP.LE.-1 .AND. IERTYP.GE.-7) THEN
            I = IABS(IERTYP)
            IPATT = PRINTB(I)
            ISATT = STOPTB(I)
         END IF
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  E1PRT
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    March 14, 1984
C
C  Purpose:    To print an error message.
C
C  Usage:      CALL E1PRT
C
C  Arguments:  None
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE E1PRT
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    ALL, I, IBEG, IBLOC, IBLOC2, IEND, IER, IHDR, J,
     &           LERTYP, LOC, LOCM1, LOCX, MAXLOC, MAXTMP, MLOC, MOD,
     &           NCBEG, NLOC, NOUT
      CHARACTER  MSGTMP(70), STRING(10)
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      CHARACTER  ATLINE(9), BLANK(1), DBB(3), FROM(6), MSGTYP(8,7),
     &           PERSLA(2), QMARK, UNKNOW(8)
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM1/
      INTEGER    CALLVL, MAXLEV, MSGLEN, ERTYPE(51), ERCODE(51),
     &           PRINTB(7), STOPTB(7), PLEN, IFERR6, IFERR7,
     &           IALLOC(51), HDRFMT(7), TRACON(7)
      COMMON     /ERCOM1/ CALLVL, MAXLEV, MSGLEN, ERTYPE, ERCODE,
     &           PRINTB, STOPTB, PLEN, IFERR6, IFERR7, IALLOC, HDRFMT,
     &           TRACON
      SAVE       /ERCOM1/
C                              SPECIFICATIONS FOR COMMON /ERCOM2/
      CHARACTER  MSGSAV(255), PLIST(300), RNAME(51)*6
      COMMON     /ERCOM2/ MSGSAV, PLIST, RNAME
      SAVE       /ERCOM2/
C                              SPECIFICATIONS FOR COMMON /ERCOM3/
      DOUBLE PRECISION ERCKSM
      COMMON     /ERCOM3/ ERCKSM
      SAVE       /ERCOM3/
C                              SPECIFICATIONS FOR COMMON /ERCOM4/
      LOGICAL    ISUSER(51)
      COMMON     /ERCOM4/ ISUSER
      SAVE       /ERCOM4/
C                              SPECIFICATIONS FOR COMMON /ERCOM8/
      INTEGER    PROLVL, XXLINE(10), XXPLEN(10), ICALOC(10), INALOC(10)
      COMMON     /ERCOM8/ PROLVL, XXLINE, XXPLEN, ICALOC, INALOC
      SAVE       /ERCOM8/
C                              SPECIFICATIONS FOR COMMON /ERCOM9/
      CHARACTER  XXPROC(10)*31
      COMMON     /ERCOM9/ XXPROC
      SAVE       /ERCOM9/
      SAVE       ATLINE, BLANK, DBB, FROM, MSGTYP, PERSLA, QMARK,
     &           UNKNOW
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  MIN0
      INTRINSIC  MIN0
      INTEGER    MIN0
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   C1TIC, M1VE, UMACH
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   I1DX, I1ERIF
      INTEGER    I1DX, I1ERIF
C
      DATA MSGTYP/'N', 'O', 'T', 'E', ' ', ' ', ' ', ' ', 'A',
     &     'L', 'E', 'R', 'T', ' ', ' ', ' ', 'W', 'A', 'R',
     &     'N', 'I', 'N', 'G', ' ', 'F', 'A', 'T', 'A', 'L',
     &     ' ', ' ', ' ', 'T', 'E', 'R', 'M', 'I', 'N', 'A',
     &     'L', 'W', 'A', 'R', 'N', 'I', 'N', 'G', ' ', 'F',
     &     'A', 'T', 'A', 'L', ' ', ' ', ' '/
      DATA UNKNOW/'U', 'N', 'K', 'N', 'O', 'W', 'N', ' '/
      DATA ATLINE/' ', 'a', 't', ' ', 'l', 'i', 'n', 'e', ' '/
      DATA BLANK/' '/, FROM/' ', 'f', 'r', 'o', 'm', ' '/
      DATA DBB/'.', ' ', ' '/, PERSLA/'%', '/'/
      DATA QMARK/'?'/
C
      IF (MSGLEN .LE. 0) RETURN
      CALL UMACH (2, NOUT)
      MAXTMP = 70
      MOD = 0
      LERTYP = ERTYPE(CALLVL)
      IHDR = HDRFMT(LERTYP)
      IF (IHDR .EQ. 3) THEN
         IF (XXPROC(PROLVL)(1:1).EQ.QMARK .AND. XXLINE(PROLVL).EQ.0)
     &       THEN
            IHDR = 1
         END IF
      END IF
      IEND = 0
      IF (IHDR.EQ.1 .AND. ERTYPE(CALLVL).LE.4) THEN
         MSGTMP(1) = BLANK(1)
         IEND = 1
C                                  CONVERT ERROR CODE INTO CHAR STRING
         CALL C1TIC (ERCODE(CALLVL), STRING, 10, IER)
C                                  LOCATE START OF NON-BLANK CHARACTERS
         IBEG = I1ERIF(STRING,10,BLANK,1)
C                                  M1VE IT TO MSGTMP
         CALL M1VE (STRING, IBEG, 10, 10, MSGTMP, IEND+1,
     &              IEND+11-IBEG, MAXTMP, IER)
         IEND = IEND + 11 - IBEG
      END IF
      IF (IHDR .NE. 2) THEN
         CALL M1VE (FROM, 1, 6, 6, MSGTMP, IEND+1, IEND+6, MAXTMP, IER)
         IEND = IEND + 6
      END IF
      IF (IHDR .EQ. 3) THEN
C                                  THIS IS A PROTRAN RUN TIME ERROR MSG.
C                                  RETRIEVE THE PROCEDURE NAME
         CALL M1VE (XXPROC(PROLVL), 1, XXPLEN(PROLVL), 31, MSGTMP,
     &              IEND+1, IEND+XXPLEN(PROLVL), MAXTMP, IER)
         MLOC = IEND + XXPLEN(PROLVL) + 1
         MSGTMP(MLOC) = BLANK(1)
         IEND = IEND + I1DX(MSGTMP(IEND+1),XXPLEN(PROLVL)+1,BLANK,1) -
     &          1
         IF (XXLINE(PROLVL) .GT. 0) THEN
C                                  INSERT ATLINE
            CALL M1VE (ATLINE, 1, 9, 9, MSGTMP, IEND+1, IEND+9,
     &                 MAXTMP, IER)
            IEND = IEND + 9
C                                  CONVERT PROTRAN GLOBAL LINE NUMBER
            CALL C1TIC (XXLINE(PROLVL), STRING, 10, IER)
C                                  LOCATE START OF NON-BLANK CHARACTERS
            IBEG = I1ERIF(STRING,10,BLANK,1)
C                                  M1VE GLOBAL LINE NUMBER TO MSGTMP
            CALL M1VE (STRING, IBEG, 10, 10, MSGTMP, IEND+1,
     &                 IEND+11-IBEG, MAXTMP, IER)
            IEND = IEND + 11 - IBEG
         END IF
      ELSE
C                                  THIS IS EITHER A LIBRARY ERROR MSG
C                                  OR A PROTRAN PREPROCESSOR ERROR MSG
         IF (IHDR .EQ. 1) THEN
C                                  THIS IS A LIBRARY ERROR MESSAGE.
C                                  RETRIEVE ROUTINE NAME
            CALL M1VE (RNAME(CALLVL), 1, 6, 6, MSGTMP, IEND+1, IEND+6,
     &                 MAXTMP, IER)
            MSGTMP(IEND+7) = BLANK(1)
            IEND = IEND + I1DX(MSGTMP(IEND+1),7,BLANK,1) - 1
         END IF
C                                  ADD DOT, BLANK, BLANK IF NEEDED
         IF (I1DX(MSGSAV,3,DBB,3) .NE. 1) THEN
            CALL M1VE (DBB, 1, 3, 3, MSGTMP, IEND+1, IEND+3, MAXTMP,
     &                 IER)
            IEND = IEND + 3
            MOD = 3
         END IF
      END IF
C                                  MSGTMP AND MSGSAV NOW CONTAIN THE
C                                   ERROR MESSAGE IN FINAL FORM.
      NCBEG = 59 - IEND - MOD
      ALL = 0
      IBLOC = I1DX(MSGSAV,MSGLEN,PERSLA,2)
      IF (IBLOC.NE.0 .AND. IBLOC.LT.NCBEG) THEN
         LOCM1 = IBLOC - 1
         LOC = IBLOC + 1
      ELSE IF (MSGLEN .LE. NCBEG) THEN
         LOCM1 = MSGLEN
         ALL = 1
      ELSE
         LOC = NCBEG
C                                  CHECK FOR APPROPRIATE PLACE TO SPLIT
   10    CONTINUE
         IF (MSGSAV(LOC) .NE. BLANK(1)) THEN
            LOC = LOC - 1
            IF (LOC .GT. 1) GO TO 10
            LOC = NCBEG + 1
         END IF
         LOCM1 = LOC - 1
      END IF
C                                  NO BLANKS FOUND IN FIRST NCBEG CHARS
      IF (LERTYP.GE.1 .AND. LERTYP.LE.7) THEN
         WRITE (NOUT,99995) (MSGTYP(I,LERTYP),I=1,8),
     &                     (MSGTMP(I),I=1,IEND), (MSGSAV(I),I=1,LOCM1)
      ELSE
         WRITE (NOUT,99995) (UNKNOW(I),I=1,8), (MSGTMP(I),I=1,IEND),
     &                     (MSGSAV(I),I=1,LOCM1)
      END IF
      IF (ALL .EQ. 0) THEN
C                                  PREPARE TO WRITE CONTINUATION OF
C                                    MESSAGE
C
C                                  FIND WHERE TO BREAK MESSAGE
C                                    LOC = NUMBER OF CHARACTERS OF
C                                          MESSAGE WRITTEN SO FAR
   20    LOCX = LOC + 64
         NLOC = LOC + 1
         IBLOC2 = IBLOC
         MAXLOC = MIN0(MSGLEN-LOC,64)
         IBLOC = I1DX(MSGSAV(NLOC),MAXLOC,PERSLA,2)
         IF (MSGSAV(NLOC).EQ.BLANK(1) .AND. IBLOC2.EQ.0) NLOC = NLOC +
     &       1
         IF (IBLOC .GT. 0) THEN
C                                  PAGE BREAK FOUND AT IBLOC
            LOCX = NLOC + IBLOC - 2
            WRITE (NOUT,99996) (MSGSAV(I),I=NLOC,LOCX)
            LOC = NLOC + IBLOC
            GO TO 20
C                                  DON'T BOTHER LOOKING FOR BLANK TO
C                                    BREAK AT IF LOCX .GE. MSGLEN
         ELSE IF (LOCX .LT. MSGLEN) THEN
C                                  CHECK FOR BLANK TO BREAK THE LINE
   30       CONTINUE
            IF (MSGSAV(LOCX) .EQ. BLANK(1)) THEN
C                                  BLANK FOUND AT LOCX
               WRITE (NOUT,99996) (MSGSAV(I),I=NLOC,LOCX)
               LOC = LOCX
               GO TO 20
            END IF
            LOCX = LOCX - 1
            IF (LOCX .GT. NLOC) GO TO 30
            LOCX = LOC + 64
C                                  NO BLANKS FOUND IN NEXT 64 CHARS
            WRITE (NOUT,99996) (MSGSAV(I),I=NLOC,LOCX)
            LOC = LOCX
            GO TO 20
         ELSE
C                                  ALL THE REST WILL FIT ON 1 LINE
            LOCX = MSGLEN
            WRITE (NOUT,99996) (MSGSAV(I),I=NLOC,LOCX)
         END IF
      END IF
C                                  SET LENGTH OF MSGSAV AND PLEN
C                                    TO SHOW THAT MESSAGE HAS
C                                    ALREADY BEEN PRINTED
 9000 MSGLEN = 0
      PLEN = 1
      IF (TRACON(LERTYP).EQ.1 .AND. CALLVL.GT.2) THEN
C                                  INITIATE TRACEBACK
         WRITE (NOUT,99997)
         DO 9005  J=CALLVL, 1, -1
            IF (J .GT. 1) THEN
               IF (ISUSER(J-1)) THEN
                  WRITE (NOUT,99998) RNAME(J), ERTYPE(J), ERCODE(J)
               ELSE
                  WRITE (NOUT,99999) RNAME(J), ERTYPE(J), ERCODE(J)
               END IF
            ELSE
               WRITE (NOUT,99998) RNAME(J), ERTYPE(J), ERCODE(J)
            END IF
 9005    CONTINUE
      END IF
C
      RETURN
99995 FORMAT (/, ' *** ', 8A1, ' ERROR', 59A1)
99996 FORMAT (' *** ', 9X, 64A1)
99997 FORMAT (14X, 'Here is a traceback of subprogram calls',
     &       ' in reverse order:', /, 14X, '      Routine    Error ',
     &       'type    Error code', /, 14X, '      -------    ',
     &       '----------    ----------')
99998 FORMAT (20X, A6, 5X, I6, 8X, I6)
99999 FORMAT (20X, A6, 5X, I6, 8X, I6, 4X, '(Called internally)')
      END
C-----------------------------------------------------------------------
C  IMSL Name:  E1PSH
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    March 2, 1984
C
C  Purpose:    To push a subroutine name onto the error control stack.
C
C  Usage:      CALL E1PSH(NAME)
C
C  Arguments:
C     NAME   - A character string of length six specifing the name of
C              the subroutine.  (Input)
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE E1PSH (NAME)
C                                  SPECIFICATIONS FOR ARGUMENTS
      CHARACTER  NAME*(*)
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      INTEGER    IFINIT
      SAVE       IFINIT
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM1/
      INTEGER    CALLVL, MAXLEV, MSGLEN, ERTYPE(51), ERCODE(51),
     &           PRINTB(7), STOPTB(7), PLEN, IFERR6, IFERR7,
     &           IALLOC(51), HDRFMT(7), TRACON(7)
      COMMON     /ERCOM1/ CALLVL, MAXLEV, MSGLEN, ERTYPE, ERCODE,
     &           PRINTB, STOPTB, PLEN, IFERR6, IFERR7, IALLOC, HDRFMT,
     &           TRACON
      SAVE       /ERCOM1/
C                              SPECIFICATIONS FOR COMMON /ERCOM2/
      CHARACTER  MSGSAV(255), PLIST(300), RNAME(51)*6
      COMMON     /ERCOM2/ MSGSAV, PLIST, RNAME
      SAVE       /ERCOM2/
C                              SPECIFICATIONS FOR COMMON /ERCOM3/
      DOUBLE PRECISION ERCKSM
      COMMON     /ERCOM3/ ERCKSM
      SAVE       /ERCOM3/
C                              SPECIFICATIONS FOR COMMON /ERCOM4/
      LOGICAL    ISUSER(51)
      COMMON     /ERCOM4/ ISUSER
      SAVE       /ERCOM4/
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1INIT, E1MES, E1STI
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   I1KST
      INTEGER    I1KST
C
      DATA IFINIT/0/
C                                  INITIALIZE ERROR TABLE IF NECESSARY
      IF (IFINIT .EQ. 0) THEN
         CALL E1INIT
         IFINIT = 1
      END IF
      IF (CALLVL .GE. MAXLEV) THEN
         CALL E1STI (1, MAXLEV)
         CALL E1MES (5, 1, 'Error condition in E1PSH.  Push would '//
     &               'cause stack level to exceed %(I1). ')
         STOP
      ELSE
C                                  STORE ALLOCATION LEVEL
         IALLOC(CALLVL) = I1KST(1)
C                                  INCREMENT THE STACK POINTER BY ONE
         CALLVL = CALLVL + 1
C                                  PUT SUBROUTINE NAME INTO STACK
         RNAME(CALLVL) = NAME
C                                  SET ERROR TYPE AND ERROR CODE
         ERTYPE(CALLVL) = 0
         ERCODE(CALLVL) = 0
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  E1STD
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    March 6, 1984
C
C  Purpose:    To store a real number for subsequent use within an error
C              message.
C
C  Usage:      CALL E1STD(ID, DVALUE)
C
C  Arguments:
C     ID     - Integer specifying the substitution index.  ID must be
C              between 1 and 9.  (Input)
C     DVALUE - The double precision number to be stored.  (Input)
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE E1STD (ID, DVALUE)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    ID
      DOUBLE PRECISION DVALUE
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, IBEG, ILEN
      CHARACTER  ARRAY(24), SAVE*24
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      INTEGER    IFINIT
      CHARACTER  BLANK(1)
      SAVE       BLANK, IFINIT
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM1/
      INTEGER    CALLVL, MAXLEV, MSGLEN, ERTYPE(51), ERCODE(51),
     &           PRINTB(7), STOPTB(7), PLEN, IFERR6, IFERR7,
     &           IALLOC(51), HDRFMT(7), TRACON(7)
      COMMON     /ERCOM1/ CALLVL, MAXLEV, MSGLEN, ERTYPE, ERCODE,
     &           PRINTB, STOPTB, PLEN, IFERR6, IFERR7, IALLOC, HDRFMT,
     &           TRACON
      SAVE       /ERCOM1/
C                              SPECIFICATIONS FOR COMMON /ERCOM2/
      CHARACTER  MSGSAV(255), PLIST(300), RNAME(51)*6
      COMMON     /ERCOM2/ MSGSAV, PLIST, RNAME
      SAVE       /ERCOM2/
C                              SPECIFICATIONS FOR COMMON /ERCOM3/
      DOUBLE PRECISION ERCKSM
      COMMON     /ERCOM3/ ERCKSM
      SAVE       /ERCOM3/
C                              SPECIFICATIONS FOR COMMON /ERCOM4/
      LOGICAL    ISUSER(51)
      COMMON     /ERCOM4/ ISUSER
      SAVE       /ERCOM4/
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1INIT, E1INPL
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   I1ERIF
      INTEGER    I1ERIF
C
      DATA BLANK/' '/, IFINIT/0/
C                                  INITIALIZE IF NECESSARY
      IF (IFINIT .EQ. 0) THEN
         CALL E1INIT
         IFINIT = 1
      END IF
      IF (DVALUE .EQ. 0.0D0) THEN
         WRITE (SAVE,'(D24.15)') DVALUE
      ELSE
         WRITE (SAVE,'(1PD24.15)') DVALUE
      END IF
      DO 40  I=1, 24
   40 ARRAY(I) = SAVE(I:I)
      IBEG = I1ERIF(ARRAY,24,BLANK,1)
      IF (ID.GE.1 .AND. ID.LE.9) THEN
         ILEN = 25 - IBEG
         CALL E1INPL ('D', ID, ILEN, ARRAY(IBEG))
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  E1STI
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    March 6, 1984
C
C  Purpose:    To store an integer for subsequent use within an error
C              message.
C
C  Usage:      CALL E1STI(II, IVALUE)
C
C  Arguments:
C     II     - Integer specifying the substitution index.  II must be
C              between 1 and 9.  (Input)
C     IVALUE - The integer to be stored.  (Input)
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE E1STI (II, IVALUE)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    II, IVALUE
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    IBEG, IER, ILEN
      CHARACTER  ARRAY(14)
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      INTEGER    IFINIT
      CHARACTER  BLANK(1)
      SAVE       BLANK, IFINIT
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM1/
      INTEGER    CALLVL, MAXLEV, MSGLEN, ERTYPE(51), ERCODE(51),
     &           PRINTB(7), STOPTB(7), PLEN, IFERR6, IFERR7,
     &           IALLOC(51), HDRFMT(7), TRACON(7)
      COMMON     /ERCOM1/ CALLVL, MAXLEV, MSGLEN, ERTYPE, ERCODE,
     &           PRINTB, STOPTB, PLEN, IFERR6, IFERR7, IALLOC, HDRFMT,
     &           TRACON
      SAVE       /ERCOM1/
C                              SPECIFICATIONS FOR COMMON /ERCOM2/
      CHARACTER  MSGSAV(255), PLIST(300), RNAME(51)*6
      COMMON     /ERCOM2/ MSGSAV, PLIST, RNAME
      SAVE       /ERCOM2/
C                              SPECIFICATIONS FOR COMMON /ERCOM3/
      DOUBLE PRECISION ERCKSM
      COMMON     /ERCOM3/ ERCKSM
      SAVE       /ERCOM3/
C                              SPECIFICATIONS FOR COMMON /ERCOM4/
      LOGICAL    ISUSER(51)
      COMMON     /ERCOM4/ ISUSER
      SAVE       /ERCOM4/
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   C1TIC, E1INIT, E1INPL
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   I1ERIF
      INTEGER    I1ERIF
C
      DATA BLANK/' '/, IFINIT/0/
C                                  INITIALIZE IF NECESSARY
      IF (IFINIT .EQ. 0) THEN
         CALL E1INIT
         IFINIT = 1
      END IF
      CALL C1TIC (IVALUE, ARRAY, 14, IER)
      IBEG = I1ERIF(ARRAY,14,BLANK,1)
      IF (II.GE.1 .AND. II.LE.9 .AND. IER.EQ.0) THEN
         ILEN = 15 - IBEG
         CALL E1INPL ('I', II, ILEN, ARRAY(IBEG))
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  E1STL
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    November 8, 1985
C
C  Purpose:    To store a string for subsequent use within an error
C              message.
C
C  Usage:      CALL E1STL(IL,STRING)
C
C  Arguments:
C     IL     - Integer specifying the substitution index.  IL must be
C              between 1 and 9.  (Input)
C     STRING - A character string.  (Input)
C
C  Copyright:  1985 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE E1STL (IL, STRING)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    IL
      CHARACTER  STRING*(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, LEN2
      CHARACTER  STRGUP(255)
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      INTEGER    IFINIT
      SAVE       IFINIT
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM1/
      INTEGER    CALLVL, MAXLEV, MSGLEN, ERTYPE(51), ERCODE(51),
     &           PRINTB(7), STOPTB(7), PLEN, IFERR6, IFERR7,
     &           IALLOC(51), HDRFMT(7), TRACON(7)
      COMMON     /ERCOM1/ CALLVL, MAXLEV, MSGLEN, ERTYPE, ERCODE,
     &           PRINTB, STOPTB, PLEN, IFERR6, IFERR7, IALLOC, HDRFMT,
     &           TRACON
      SAVE       /ERCOM1/
C                              SPECIFICATIONS FOR COMMON /ERCOM2/
      CHARACTER  MSGSAV(255), PLIST(300), RNAME(51)*6
      COMMON     /ERCOM2/ MSGSAV, PLIST, RNAME
      SAVE       /ERCOM2/
C                              SPECIFICATIONS FOR COMMON /ERCOM3/
      DOUBLE PRECISION ERCKSM
      COMMON     /ERCOM3/ ERCKSM
      SAVE       /ERCOM3/
C                              SPECIFICATIONS FOR COMMON /ERCOM4/
      LOGICAL    ISUSER(51)
      COMMON     /ERCOM4/ ISUSER
      SAVE       /ERCOM4/
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  IABS,LEN,MIN0
      INTRINSIC  IABS, LEN, MIN0
      INTEGER    IABS, LEN, MIN0
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1INIT, E1INPL
C
      DATA IFINIT/0/
C                                  INITIALIZE IF NECESSARY
      IF (IFINIT .EQ. 0) THEN
         CALL E1INIT
         IFINIT = 1
      END IF
      LEN2 = LEN(STRING)
      LEN2 = MIN0(LEN2,255)
      DO 10  I=1, LEN2
         STRGUP(I) = STRING(I:I)
   10 CONTINUE
      IF (IABS(IL).GE.1 .AND. IABS(IL).LE.9) THEN
         CALL E1INPL ('L', IL, LEN2, STRGUP)
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  E1UCS
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    March 8, 1984
C
C  Purpose:    To update the checksum number for error messages.
C
C  Usage:      CALL E1UCS
C
C  Arguments:  None
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE E1UCS
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, IBEG, IBEG2, IEND, ILOC, IPOS, JLOC, NCODE, NLEN
      DOUBLE PRECISION DNUM
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      DOUBLE PRECISION DMAX
      CHARACTER  BLANK(1), COMMA(1), EQUAL(1), LPAR(1)
      SAVE       BLANK, COMMA, DMAX, EQUAL, LPAR
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM1/
      INTEGER    CALLVL, MAXLEV, MSGLEN, ERTYPE(51), ERCODE(51),
     &           PRINTB(7), STOPTB(7), PLEN, IFERR6, IFERR7,
     &           IALLOC(51), HDRFMT(7), TRACON(7)
      COMMON     /ERCOM1/ CALLVL, MAXLEV, MSGLEN, ERTYPE, ERCODE,
     &           PRINTB, STOPTB, PLEN, IFERR6, IFERR7, IALLOC, HDRFMT,
     &           TRACON
      SAVE       /ERCOM1/
C                              SPECIFICATIONS FOR COMMON /ERCOM2/
      CHARACTER  MSGSAV(255), PLIST(300), RNAME(51)*6
      COMMON     /ERCOM2/ MSGSAV, PLIST, RNAME
      SAVE       /ERCOM2/
C                              SPECIFICATIONS FOR COMMON /ERCOM3/
      DOUBLE PRECISION ERCKSM
      COMMON     /ERCOM3/ ERCKSM
      SAVE       /ERCOM3/
C                              SPECIFICATIONS FOR COMMON /ERCOM4/
      LOGICAL    ISUSER(51)
      COMMON     /ERCOM4/ ISUSER
      SAVE       /ERCOM4/
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  DMOD
      INTRINSIC  DMOD
      DOUBLE PRECISION DMOD
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   S1ANUM
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   ICASE, I1X
      INTEGER    ICASE, I1X
C
      DATA BLANK(1)/' '/, COMMA(1)/','/, LPAR(1)/'('/
      DATA EQUAL(1)/'='/, DMAX/1.0D+9/
C
      IF (MSGLEN .GT. 1) THEN
         IPOS = 0
         IBEG2 = 1
   10    IBEG = IBEG2
         IEND = MSGLEN
C                                  LOOK FOR BLANK, COMMA, LEFT PAREN.,
C                                  OR EQUAL SIGN
         ILOC = I1X(MSGSAV(IBEG),IEND-IBEG+1,BLANK,1)
         JLOC = I1X(MSGSAV(IBEG),IEND-IBEG+1,COMMA,1)
         IF (ILOC.EQ.0 .OR. (JLOC.GT.0.AND.JLOC.LT.ILOC)) ILOC = JLOC
         JLOC = I1X(MSGSAV(IBEG),IEND-IBEG+1,LPAR,1)
         IF (ILOC.EQ.0 .OR. (JLOC.GT.0.AND.JLOC.LT.ILOC)) ILOC = JLOC
         JLOC = I1X(MSGSAV(IBEG),IEND-IBEG+1,EQUAL,1)
         IF (ILOC.EQ.0 .OR. (JLOC.GT.0.AND.JLOC.LT.ILOC)) ILOC = JLOC
         IF (ILOC .GE. 1) THEN
            CALL S1ANUM (MSGSAV(IBEG+ILOC), IEND-IBEG-ILOC+1, NCODE,
     &                   NLEN)
            IF (NCODE.EQ.2 .OR. NCODE.EQ.3) THEN
C                                  FLOATING POINT NUMBER FOUND.
C                                  SET POINTERS TO SKIP OVER IT
               IBEG2 = IBEG + ILOC + NLEN
               IF (IBEG2 .LE. MSGLEN) THEN
                  CALL S1ANUM (MSGSAV(IBEG2), IEND-IBEG2+1, NCODE,
     &                         NLEN)
                  IF ((MSGSAV(IBEG2).EQ.'+'.OR.MSGSAV(IBEG2).EQ.
     &                '-') .AND. (NCODE.EQ.1.OR.NCODE.EQ.2)) THEN
C                                  INTEGER IMMEDIATELY FOLLOWS A REAL AS
C                                  WITH SOME CDC NOS. LIKE 1.2345678+123
C                                  SET POINTERS TO SKIP OVER IT
                     IF (NCODE.EQ.2 .AND. MSGSAV(IBEG2+NLEN-1).EQ.
     &                   '.') THEN
C                                  DO NOT SKIP AN END-OF-SENTENCE PERIOD
                        IBEG2 = IBEG2 + NLEN - 1
                     ELSE
                        IBEG2 = IBEG2 + NLEN
                     END IF
                  END IF
               END IF
            ELSE
               IBEG2 = IBEG + ILOC
            END IF
            IEND = IBEG + ILOC - 1
         END IF
C                                  UPDATE CKSUM USING PART OF MESSAGE
         DO 20  I=IBEG, IEND
            IPOS = IPOS + 1
            DNUM = ICASE(MSGSAV(I))
            ERCKSM = DMOD(ERCKSM+DNUM*IPOS,DMAX)
   20    CONTINUE
C                                  GO BACK FOR MORE IF NEEDED
         IF (IEND.LT.MSGLEN .AND. IBEG2.LT.MSGLEN) GO TO 10
C                                  UPDATE CKSUM USING ERROR TYPE
         DNUM = ERTYPE(CALLVL)
         ERCKSM = DMOD(ERCKSM+DNUM*(IPOS+1),DMAX)
C                                  UPDATE CKSUM USING ERROR CODE
         DNUM = ERCODE(CALLVL)
         ERCKSM = DMOD(ERCKSM+DNUM*(IPOS+2),DMAX)
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  I1CSTR (Single precision version)
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    September 10, 1985
C
C  Purpose:    Case insensitive comparison of two character arrays.
C
C  Usage:      I1CSTR(STR1, LEN1, STR2, LEN2)
C
C  Arguments:
C     STR1   - First character array.  (Input)
C     LEN1   - Length of STR1.  (Input)
C     STR2   - Second character array.  (Input)
C     LEN2   - Length of STR2.  (Input)
C     I1CSTR - Integer function.  (Output) Where
C              I1CSTR = -1  if STR1 .LT. STR2,
C              I1CSTR =  0  if STR1 .EQ. STR2,
C              I1CSTR =  1  if STR1 .GT. STR2.
C
C  Remarks:
C  1. If the two arrays, STR1 and STR2,  are of unequal length, the
C     shorter array is considered as if it were extended with blanks
C     to the length of the longer array.
C
C  2. If one or both lengths are zero or negative the I1CSTR output is
C     based on comparison of the lengths.
C
C  GAMS:       N5c
C
C  Chapter:    MATH/LIBRARY Utilities
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION I1CSTR (STR1, LEN1, STR2, LEN2)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    LEN1, LEN2
      CHARACTER  STR1(LEN1), STR2(LEN2)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    IC1, IC2, ICB, IS, L, LENM
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  ISIGN,MIN0
      INTRINSIC  ISIGN, MIN0
      INTEGER    ISIGN, MIN0
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   ICASE
      INTEGER    ICASE
C
      IF (LEN1.GT.0 .AND. LEN2.GT.0) THEN
C                                  COMPARE FIRST LENM CHARACTERS
         LENM = MIN0(LEN1,LEN2)
         DO 10  L=1, LENM
            IC1 = ICASE(STR1(L))
            IC2 = ICASE(STR2(L))
            IF (IC1 .NE. IC2) THEN
               I1CSTR = ISIGN(1,IC1-IC2)
               RETURN
            END IF
   10    CONTINUE
      END IF
C                                  COMPARISON BASED ON LENGTH OR
C                                  TRAILING BLANKS
      IS = LEN1 - LEN2
      IF (IS .EQ. 0) THEN
         I1CSTR = 0
      ELSE
         IF (LEN1.LE.0 .OR. LEN2.LE.0) THEN
C                                  COMPARISON BASED ON LENGTH
            I1CSTR = ISIGN(1,IS)
         ELSE
C                                  COMPARISON BASED ON TRAILING BLANKS
C                                  TO EXTEND SHORTER ARRAY
            LENM = LENM + 1
            ICB = ICASE(' ')
            IF (IS .GT. 0) THEN
C                                  EXTEND STR2 WITH BLANKS
               DO 20  L=LENM, LEN1
                  IC1 = ICASE(STR1(L))
                  IF (IC1 .NE. ICB) THEN
                     I1CSTR = ISIGN(1,IC1-ICB)
                     RETURN
                  END IF
   20          CONTINUE
            ELSE
C                                  EXTEND STR1 WITH BLANKS
               DO 30  L=LENM, LEN2
                  IC2 = ICASE(STR2(L))
                  IF (ICB .NE. IC2) THEN
                     I1CSTR = ISIGN(1,ICB-IC2)
                     RETURN
                  END IF
   30          CONTINUE
            END IF
C
            I1CSTR = 0
         END IF
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  I1DX (Single precision version)
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    September 9, 1985
C
C  Purpose:    Determine the array subscript indicating the starting
C              element at which a key character sequence begins.
C              (Case-insensitive version)
C
C  Usage:      I1DX(CHRSTR, I1LEN, KEY, KLEN)
C
C  Arguments:
C     CHRSTR - Character array to be searched.  (Input)
C     I1LEN  - Length of CHRSTR.  (Input)
C     KEY    - Character array that contains the key sequence.  (Input)
C     KLEN   - Length of KEY.  (Input)
C     I1DX   - Integer function.  (Output)
C
C  Remarks:
C  1. Returns zero when there is no match.
C
C  2. Returns zero if KLEN is longer than ISLEN.
C
C  3. Returns zero when any of the character arrays has a negative or
C     zero length.
C
C  GAMS:       N5c
C
C  Chapter:    MATH/LIBRARY Utilities
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION I1DX (CHRSTR, I1LEN, KEY, KLEN)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    I1LEN, KLEN
      CHARACTER  CHRSTR(*), KEY(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, II, J
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   ICASE, I1CSTR
      INTEGER    ICASE, I1CSTR
C
      I1DX = 0
      IF (KLEN.LE.0 .OR. I1LEN.LE.0) GO TO 9000
      IF (KLEN .GT. I1LEN) GO TO 9000
C
      I = 1
      II = I1LEN - KLEN + 1
   10 IF (I .LE. II) THEN
         IF (ICASE(CHRSTR(I)) .EQ. ICASE(KEY(1))) THEN
            IF (KLEN .NE. 1) THEN
               J = KLEN - 1
               IF (I1CSTR(CHRSTR(I+1),J,KEY(2),J) .EQ. 0) THEN
                  I1DX = I
                  GO TO 9000
               END IF
            ELSE
               I1DX = I
               GO TO 9000
            END IF
         END IF
         I = I + 1
         GO TO 10
      END IF
C
 9000 RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  I1ERIF
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    March 13, 1984
C
C  Purpose:    Return the position of the first element of a given
C              character array which is not an element of another
C              character array.
C
C  Usage:      I1ERIF(STR1, LEN1, STR2, LEN2)
C
C  Arguments:
C     STR1   - Character array to be searched.  (Input)
C     LEN1   - Length of STR1.  (Input)
C     STR2   - Character array to be searched for.  (Input)
C     LEN2   - Length of STR2.  (Input)
C     I1ERIF - Integer function.  (Output)
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION I1ERIF (STR1, LEN1, STR2, LEN2)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    LEN1, LEN2
      CHARACTER  STR1(*), STR2(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   I1X
      INTEGER    I1X
C                              FIRST EXECUTABLE STATEMENT
      IF (LEN1.LE.0 .OR. LEN2.LE.0) THEN
         I1ERIF = 1
      ELSE
         DO 10  I=1, LEN1
            IF (I1X(STR2,LEN2,STR1(I),1) .EQ. 0) THEN
               I1ERIF = I
               RETURN
            END IF
   10    CONTINUE
         I1ERIF = 0
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  I1KGT
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    January 17, 1984
C
C  Purpose:    Allocate numerical workspace.
C
C  Usage:      I1KGT(NELMTS,ITYPE)
C
C  Arguments:
C     NELMTS - Number of elements of data type ITYPE to be
C              allocated.  (Input)
C     ITYPE  - Data type of array to be allocated.  (Input)
C                 1 - logical
C                 2 - integer
C                 3 - real
C                 4 - double precision
C                 5 - complex
C                 6 - double complex
C     I1KGT  - Integer function.  (Output)  Returns the index of the
C              first element in the current allocation.
C
C  Remarks:
C  1. On return, the array will occupy
C     WKSP(I1KGT), WKSP(I1KGT+1), ..., WKSP(I1KGT+NELMTS-1) where
C     WKSP is an array of data type ITYPE equivalenced to RWKSP.
C
C  2. If I1KGT is negative, the absolute value of I1KGT is the
C     additional workspace needed for the current allocation.
C
C  3. The allocator reserves the first sixteen integer locations of
C     the stack for its own internal bookkeeping.  These are initialized
C     by the function IWKIN upon the first call to the allocation
C     package.
C
C  4. The use of the first ten integer locations is as follows:
C      WKSP( 1) - LOUT    The number of current allocations
C      WKSP( 2) - LNOW    The current active length of the stack
C      WKSP( 3) - LUSED   The maximum value of WKSP(2) achieved
C                         thus far
C      WKSP( 4) - LBND    The lower bound of permanent storage which
C                         is one numeric storage unit more than the
C                         maximum allowed length of the stack.
C      WKSP( 5) - LMAX    The maximum length of the storage array
C      WKSP( 6) - LALC    The total number of allocations handled by
C                         I1KGT
C      WKSP( 7) - LNEED   The number of numeric storage units by which
C                         the array size must be increased for all past
C                         allocations to succeed
C      WKSP( 8) - LBOOK   The number of numeric storage units used for
C                         bookkeeping
C      WKSP( 9) - LCHAR   The pointer to the portion of the permanent
C                         stack which contains the bookkeeping and
C                         pointers for the character workspace
C                         allocation.
C      WKSP(10) - LLCHAR  The length of the array beginning at LCHAR
C                         set aside for character workspace bookkeeping
C                         and pointers.
C                 NOTE -  If character workspace is not being used,
C                         LCHAR and LLCHAR can be ignored.
C  5. The next six integer locations contain values describing the
C     amount of storage allocated by the allocation system to the
C     various data types.
C      WKSP(11) - Numeric storage units allocated to LOGICAL
C      WKSP(12) - Numeric storage units allocated to INTEGER
C      WKSP(13) - Numeric storage units allocated to REAL
C      WKSP(14) - Numeric storage units allocated to DOUBLE PRECISION
C      WKSP(15) - Numeric storage units allocated to COMPLEX
C      WKSP(16) - Numeric storage units allocated to DOUBLE COMPLEX
C
C  Copyright:  1984 by IMSL, Inc. All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION I1KGT (NELMTS, ITYPE)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    NELMTS, ITYPE
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, IDUMAL, IGAP, ILEFT, IPA, IPA7, ISA, ISA7,
     &           ISIZE(6), JTYPE, LALC, LBND, LBOOK, LMAX, LNEED,
     &           LNEED1, LNOW, LOUT, LUSED
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      LOGICAL    FIRST
      SAVE       FIRST
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM8/
      INTEGER    PROLVL, XXLINE(10), XXPLEN(10), ICALOC(10), INALOC(10)
      COMMON     /ERCOM8/ PROLVL, XXLINE, XXPLEN, ICALOC, INALOC
      SAVE       /ERCOM8/
C                              SPECIFICATIONS FOR COMMON /ERCOM9/
      CHARACTER  XXPROC(10)*31
      COMMON     /ERCOM9/ XXPROC
      SAVE       /ERCOM9/
C                                  SPECIFICATIONS FOR COMMON /WORKSP/
      REAL       RWKSP(5000)
      REAL       RDWKSP(5000)
      DOUBLE PRECISION DWKSP(2500)
      COMPLEX    CWKSP(2500)
      COMPLEX    CZWKSP(2500)
      COMPLEX    *16 ZWKSP(1250)
      INTEGER    IWKSP(5000)
      LOGICAL    LWKSP(5000)
      EQUIVALENCE (DWKSP(1), RWKSP(1))
      EQUIVALENCE (CWKSP(1), RWKSP(1)), (ZWKSP(1), RWKSP(1))
      EQUIVALENCE (IWKSP(1), RWKSP(1)), (LWKSP(1), RWKSP(1))
      EQUIVALENCE (RDWKSP(1), RWKSP(1)), (CZWKSP(1), RWKSP(1))
      COMMON     /WORKSP/ DWKSP
C                                  SPECIFICATIONS FOR EQUIVALENCE
      EQUIVALENCE (LOUT, IWKSP(1))
      EQUIVALENCE (LNOW, IWKSP(2))
      EQUIVALENCE (LUSED, IWKSP(3))
      EQUIVALENCE (LBND, IWKSP(4))
      EQUIVALENCE (LMAX, IWKSP(5))
      EQUIVALENCE (LALC, IWKSP(6))
      EQUIVALENCE (LNEED, IWKSP(7))
      EQUIVALENCE (LBOOK, IWKSP(8))
      EQUIVALENCE (ISIZE(1), IWKSP(11))
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  IABS,MAX0,MOD
      INTRINSIC  IABS, MAX0, MOD
      INTEGER    IABS, MAX0, MOD
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1POS, E1PSH, E1STI, IWKIN
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   I1KQU
      INTEGER    I1KQU
C
      DATA FIRST/.TRUE./
C
      CALL E1PSH ('I1KGT ')
C
      IF (FIRST) THEN
C                                  INITIALIZE WORKSPACE IF NEEDED
         FIRST = .FALSE.
         CALL IWKIN (0)
      END IF
C                                  NUMBER OF ELEMENTS LESS THAN 0
      IF (NELMTS .LT. 0) THEN
         CALL E1STI (1, NELMTS)
         CALL E1MES (5, 2, 'Number of elements is not positive.%/'//
     &               'NELMTS = %(I1).')
         CALL E1POP ('I1KGT ')
         GO TO 9000
      END IF
C                                  ILLEGAL DATA TYPE REQUESTED
      IF (ITYPE.EQ.0 .OR. IABS(ITYPE).GE.7) THEN
         CALL E1MES (5, 3, 'Illegal data type requested.')
         CALL E1POP ('I1KGT ')
         GO TO 9000
      END IF
C                                  BOOKKEEPING OVERWRITTEN
      IF (LNOW.LT.LBOOK .OR. LNOW.GT.LUSED .OR. LUSED.GT.LMAX .OR.
     &    LNOW.GE.LBND .OR. LOUT.GT.LALC) THEN
         CALL E1MES (5, 4, 'One or more of the first eight '//
     &               'bookkeeping locations in IWKSP have been '//
     &               'overwritten.')
         CALL E1POP ('I1KGT ')
         GO TO 9000
      END IF
C
      CALL E1POP ('I1KGT ')
C                                  DETERMINE NUMBER OF LOCATIONS STILL
C                                  AVAILABLE FOR DATA TYPE ITYPE
C                                  NOTE: I1KQU ALLOWS FOR 2 INTEGER
C                                        POINTERS WHICH MUST BE HANDLED
C                                        ARTIFICIALLY IF ILEFT = 0.
      ILEFT = I1KQU(IABS(ITYPE))
C
      IF (ITYPE .GT. 0) THEN
C                                  RELEASABLE STORAGE
         IF (ILEFT .GE. NELMTS) THEN
            I1KGT = (LNOW*ISIZE(2)-1)/ISIZE(ITYPE) + 2
            I = ((I1KGT-1+NELMTS)*ISIZE(ITYPE)-1)/ISIZE(2) + 3
C                                  IWKSP(I-1) CONTAINS THE DATA TYPE FOR
C                                  THIS ALLOCATION. IWKSP(I) CONTAINS
C                                  LNOW FOR THE PREVIOUS ALLOCATION.
            IWKSP(I-1) = ITYPE
            IWKSP(I) = LNOW
            LOUT = LOUT + 1
            LALC = LALC + 1
            LNOW = I
            LUSED = MAX0(LUSED,LNOW)
            LNEED = 0
         ELSE
C                                  RELEASABLE STORAGE WAS REQUESTED
C                                  BUT THE STACK WOULD OVERFLOW.
C                                  THEREFORE, ALLOCATE RELEASABLE
C                                  SPACE THROUGH THE END OF THE STACK
            IF (LNEED .EQ. 0) THEN
               IDUMAL = (LNOW*ISIZE(2)-1)/ISIZE(ITYPE) + 2
               I = ((IDUMAL-1+ILEFT)*ISIZE(ITYPE)-1)/ISIZE(2) + 3
C                                  ADVANCE COUNTERS AND STORE POINTERS
C                                  IF THERE IS ROOM TO DO SO
               IF (I .LT. LBND) THEN
C                                  IWKSP(I-1) CONTAINS THE DATA TYPE FOR
C                                  THIS ALLOCATION. IWKSP(I) CONTAINS
C                                  LNOW FOR THE PREVIOUS ALLOCATION.
                  IWKSP(I-1) = ITYPE
                  IWKSP(I) = LNOW
                  LOUT = LOUT + 1
                  LALC = LALC + 1
                  LNOW = I
                  LUSED = MAX0(LUSED,LNOW)
               END IF
            END IF
C                                  CALCULATE AMOUNT NEEDED TO ACCOMODATE
C                                  THIS ALLOCATION REQUEST
            LNEED1 = (NELMTS-ILEFT)*ISIZE(ITYPE)
            IF (ILEFT .EQ. 0) THEN
               IGAP = ISIZE(ITYPE) - MOD(LNOW+LNEED,ISIZE(ITYPE))
               IF (IGAP .EQ. ISIZE(ITYPE)) IGAP = 0
               LNEED1 = LNEED1 + 2*ISIZE(2) + IGAP
            END IF
C                                  MODIFY LNEED ACCORDING TO THE SIZE
C                                  OF THE BASE BEING USED (D.P. HERE)
            LNEED = LNEED + ((LNEED1+ISIZE(3)-1)/ISIZE(3))
C                                  SINCE CURRENT ALLOCATION IS ILLEGAL,
C                                  RETURN THE NEGATIVE OF THE ADDITIONAL
C                                  AMOUNT NEEDED TO MAKE IT LEGAL
            I1KGT = -LNEED
         END IF
      ELSE
C                                  PERMANENT STORAGE
         IF (ILEFT .GE. NELMTS) THEN
            JTYPE = -ITYPE
            I1KGT = (LBND*ISIZE(2)-1)/ISIZE(JTYPE) + 1 - NELMTS
            I = ((I1KGT-1)*ISIZE(JTYPE))/ISIZE(2) - 1
C                                  IWKSP(I) CONTAINS LBND FOR PREVIOUS
C                                  PERMANENT STORAGE ALLOCATION.
C                                  IWKSP(I+1) CONTAINS THE DATA TYPE FOR
C                                  THIS ALLOCATION.
            IWKSP(I) = LBND
            IWKSP(I+1) = JTYPE
            LALC = LALC + 1
            LBND = I
            LNEED = 0
         ELSE
C                                  PERMANENT STORAGE WAS REQUESTED
C                                  BUT THE STACK WOULD OVERFLOW,
C                                  THEREFORE, ALLOCATE RELEASABLE
C                                  SPACE THROUGH THE END OF THE STACK
            IF (LNEED .EQ. 0) THEN
               JTYPE = -ITYPE
               IDUMAL = (LNOW*ISIZE(2)-1)/ISIZE(JTYPE) + 2
               I = ((IDUMAL-1+ILEFT)*ISIZE(JTYPE)-1)/ISIZE(2) + 3
C                                  ADVANCE COUNTERS AND STORE POINTERS
C                                  IF THERE IS ROOM TO DO SO
               IF (I .LT. LBND) THEN
C                                  IWKSP(I-1) CONTAINS THE DATA TYPE FOR
C                                  THIS ALLOCATION. IWKSP(I) CONTAINS
C                                  LNOW FOR THE PREVIOUS ALLOCATION.
                  IWKSP(I-1) = JTYPE
                  IWKSP(I) = LNOW
                  LOUT = LOUT + 1
                  LALC = LALC + 1
                  LNOW = I
                  LUSED = MAX0(LUSED,LNOW)
               END IF
            END IF
C                                  CALCULATE AMOUNT NEEDED TO ACCOMODATE
C                                  THIS ALLOCATION REQUEST
            LNEED1 = (NELMTS-ILEFT)*ISIZE(-ITYPE)
            IF (ILEFT .EQ. 0) THEN
               IGAP = ISIZE(-ITYPE) - MOD(LNOW+LNEED,ISIZE(-ITYPE))
               IF (IGAP .EQ. ISIZE(-ITYPE)) IGAP = 0
               LNEED1 = LNEED1 + 2*ISIZE(2) + IGAP
            END IF
C                                  MODIFY LNEED ACCORDING TO THE SIZE
C                                  OF THE BASE BEING USED (D.P. HERE)
            LNEED = LNEED + ((LNEED1+ISIZE(3)-1)/ISIZE(3))
C                                  SINCE CURRENT ALLOCATION IS ILLEGAL,
C                                  RETURN THE NEGATIVE OF THE ADDITIONAL
C                                  AMOUNT NEEDED TO MAKE IT LEGAL
            I1KGT = -LNEED
         END IF
      END IF
C                                  STACK OVERFLOW - UNRECOVERABLE ERROR
 9000 IF (LNEED .GT. 0) THEN
         CALL E1POS (-5, IPA, ISA)
         CALL E1POS (5, 0, 0)
         CALL E1POS (-7, IPA7, ISA7)
         CALL E1POS (7, 0, 0)
         CALL E1PSH ('I1KGT ')
         CALL E1STI (1, LNEED+(LMAX/ISIZE(3)))
         IF (XXLINE(PROLVL).GE.1 .AND. XXLINE(PROLVL).LE.999) THEN
            CALL E1MES (7, 1, 'Insufficient workspace for current '//
     &                  'allocation(s).  Correct by inserting the '//
     &                  'following PROTRAN line: $OPTIONS;WORKSPACE=%'//
     &                  '(I1)')
         ELSE
            CALL E1MES (5, 5, 'Insufficient workspace for current '//
     &                  'allocation(s). Correct by calling IWKIN '//
     &                  'from main program with the three following '//
     &                  'statements:  (REGARDLESS OF PRECISION)%/'//
     &                  '      COMMON /WORKSP/  RWKSP%/      REAL '//
     &                  'RWKSP(%(I1))%/      CALL IWKIN(%(I1))')
         END IF
         CALL E1POP ('I1KGT ')
         CALL E1POS (5, IPA, ISA)
         CALL E1POS (7, IPA7, ISA7)
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  I1KQU
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    January 17, 1984
C
C  Purpose:    Return number of elements of data type ITYPE that
C              remain to be allocated in one request.
C
C  Usage:      I1KQU(ITYPE)
C
C  Arguments:
C     ITYPE  - Type of storage to be checked (Input)
C                 1 - logical
C                 2 - integer
C                 3 - real
C                 4 - double precision
C                 5 - complex
C                 6 - double complex
C     I1KQU  - Integer function. (Output) Returns number of elements
C              of data type ITYPE remaining in the stack.
C
C  Copyright:  1983 by IMSL, Inc.  All Rights Reserved
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION I1KQU (ITYPE)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    ITYPE
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    ISIZE(6), LALC, LBND, LBOOK, LMAX, LNEED, LNOW, LOUT,
     &           LUSED
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      LOGICAL    FIRST
      SAVE       FIRST
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                                  SPECIFICATIONS FOR COMMON /WORKSP/
      REAL       RWKSP(5000)
      REAL       RDWKSP(5000)
      DOUBLE PRECISION DWKSP(2500)
      COMPLEX    CWKSP(2500)
      COMPLEX    CZWKSP(2500)
      COMPLEX    *16 ZWKSP(1250)
      INTEGER    IWKSP(5000)
      LOGICAL    LWKSP(5000)
      EQUIVALENCE (DWKSP(1), RWKSP(1))
      EQUIVALENCE (CWKSP(1), RWKSP(1)), (ZWKSP(1), RWKSP(1))
      EQUIVALENCE (IWKSP(1), RWKSP(1)), (LWKSP(1), RWKSP(1))
      EQUIVALENCE (RDWKSP(1), RWKSP(1)), (CZWKSP(1), RWKSP(1))
      COMMON     /WORKSP/ DWKSP
C                                  SPECIFICATIONS FOR EQUIVALENCE
      EQUIVALENCE (LOUT, IWKSP(1))
      EQUIVALENCE (LNOW, IWKSP(2))
      EQUIVALENCE (LUSED, IWKSP(3))
      EQUIVALENCE (LBND, IWKSP(4))
      EQUIVALENCE (LMAX, IWKSP(5))
      EQUIVALENCE (LALC, IWKSP(6))
      EQUIVALENCE (LNEED, IWKSP(7))
      EQUIVALENCE (LBOOK, IWKSP(8))
      EQUIVALENCE (ISIZE(1), IWKSP(11))
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  MAX0
      INTRINSIC  MAX0
      INTEGER    MAX0
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1PSH, IWKIN
C
      DATA FIRST/.TRUE./
C
      CALL E1PSH ('I1KQU ')
C
      IF (FIRST) THEN
C                                  INITIALIZE WORKSPACE IF NEEDED
         FIRST = .FALSE.
         CALL IWKIN (0)
      END IF
C                                  BOOKKEEPING OVERWRITTEN
      IF (LNOW.LT.LBOOK .OR. LNOW.GT.LUSED .OR. LUSED.GT.LMAX .OR.
     &    LNOW.GE.LBND .OR. LOUT.GT.LALC) THEN
         CALL E1MES (5, 7, 'One or more of the first eight '//
     &               'bookkeeping locations in IWKSP have been '//
     &               'overwritten.')
      ELSE IF (ITYPE.LE.0 .OR. ITYPE.GE.7) THEN
C                                  ILLEGAL DATA TYPE REQUESTED
         CALL E1MES (5, 8, 'Illegal data type requested.')
      ELSE
C                                  THIS CALCULATION ALLOWS FOR THE
C                                  TWO POINTER LOCATIONS IN THE STACK
C                                  WHICH ARE ASSIGNED TO EACH ALLOCATION
         I1KQU = MAX0(((LBND-3)*ISIZE(2))/ISIZE(ITYPE)-(LNOW*ISIZE(2)-
     &           1)/ISIZE(ITYPE)-1,0)
      END IF
C
      CALL E1POP ('I1KQU ')
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  I1KRL
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    August 9, 1983
C
C  Purpose:    Deallocate the last N allocations made in the workspace.
C              stack by I1KGT
C
C  Usage:      CALL I1KRL(N)
C
C  Arguments:
C     N      - Number of allocations to be released top down (Input)
C
C  Copyright:  1983 by IMSL, Inc.  All Rights Reserved
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE I1KRL (N)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, IN, LALC, LBND, LBOOK, LMAX, LNEED, LNOW, LOUT,
     &           LUSED, NDX, NEXT
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      LOGICAL    FIRST
      SAVE       FIRST
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                                  SPECIFICATIONS FOR COMMON /WORKSP/
      REAL       RWKSP(5000)
      REAL       RDWKSP(5000)
      DOUBLE PRECISION DWKSP(2500)
      COMPLEX    CWKSP(2500)
      COMPLEX    CZWKSP(2500)
      COMPLEX    *16 ZWKSP(1250)
      INTEGER    IWKSP(5000)
      LOGICAL    LWKSP(5000)
      EQUIVALENCE (DWKSP(1), RWKSP(1))
      EQUIVALENCE (CWKSP(1), RWKSP(1)), (ZWKSP(1), RWKSP(1))
      EQUIVALENCE (IWKSP(1), RWKSP(1)), (LWKSP(1), RWKSP(1))
      EQUIVALENCE (RDWKSP(1), RWKSP(1)), (CZWKSP(1), RWKSP(1))
      COMMON     /WORKSP/ DWKSP
C                                  SPECIFICATIONS FOR EQUIVALENCE
      EQUIVALENCE (LOUT, IWKSP(1))
      EQUIVALENCE (LNOW, IWKSP(2))
      EQUIVALENCE (LUSED, IWKSP(3))
      EQUIVALENCE (LBND, IWKSP(4))
      EQUIVALENCE (LMAX, IWKSP(5))
      EQUIVALENCE (LALC, IWKSP(6))
      EQUIVALENCE (LNEED, IWKSP(7))
      EQUIVALENCE (LBOOK, IWKSP(8))
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1STI, IWKIN
C
      DATA FIRST/.TRUE./
C
      IF (FIRST) THEN
C                                  INITIALIZE WORKSPACE IF NEEDED
         FIRST = .FALSE.
         CALL IWKIN (0)
      END IF
C                                  CALLING I1KRL(0) WILL CONFIRM
C                                  INTEGRITY OF SYSTEM AND RETURN
      IF (N .LT. 0) THEN
         CALL E1MES (5, 10, 'Error from subroutine I1KRL:  Attempt'//
     &               ' to release a negative number of workspace'//
     &               ' allocations. ')
         GO TO 9000
      END IF
C                                  BOOKKEEPING OVERWRITTEN
      IF (LNOW.LT.LBOOK .OR. LNOW.GT.LUSED .OR. LUSED.GT.LMAX .OR.
     &    LNOW.GE.LBND .OR. LOUT.GT.LALC) THEN
         CALL E1MES (5, 11, 'Error from subroutine I1KRL:  One or '//
     &               'more of the first eight bookkeeping locations '//
     &               'in IWKSP have been overwritten.  ')
         GO TO 9000
      END IF
C                                  CHECK ALL THE POINTERS IN THE
C                                  PERMANENT STORAGE AREA.  THEY MUST
C                                  BE MONOTONE INCREASING AND LESS THAN
C                                  OR EQUAL TO LMAX, AND THE INDEX OF
C                                  THE LAST POINTER MUST BE LMAX+1.
      NDX = LBND
      IF (NDX .NE. LMAX+1) THEN
         DO 10  I=1, LALC
            NEXT = IWKSP(NDX)
            IF (NEXT .EQ. LMAX+1) GO TO 20
C
            IF (NEXT.LE.NDX .OR. NEXT.GT.LMAX) THEN
               CALL E1MES (5, 12, 'Error from subroutine I1KRL:  '//
     &                     'A pointer in permanent storage has been '//
     &                     ' overwritten. ')
               GO TO 9000
            END IF
            NDX = NEXT
   10    CONTINUE
         CALL E1MES (5, 13, 'Error from subroutine I1KRL:  A '//
     &               'pointer in permanent storage has been '//
     &               'overwritten. ')
         GO TO 9000
      END IF
   20 IF (N .GT. 0) THEN
         DO 30  IN=1, N
            IF (LNOW .LE. LBOOK) THEN
               CALL E1MES (5, 14, 'Error from subroutine I1KRL:  '//
     &                     'Attempt to release a nonexistant '//
     &                     'workspace  allocation. ')
               GO TO 9000
            ELSE IF (IWKSP(LNOW).LT.LBOOK .OR. IWKSP(LNOW).GE.LNOW-1)
     &              THEN
C                                  CHECK TO MAKE SURE THE BACK POINTERS
C                                  ARE MONOTONE.
               CALL E1STI (1, LNOW)
               CALL E1MES (5, 15, 'Error from subroutine I1KRL:  '//
     &                     'The pointer at IWKSP(%(I1)) has been '//
     &                     'overwritten.  ')
               GO TO 9000
            ELSE
               LOUT = LOUT - 1
               LNOW = IWKSP(LNOW)
            END IF
   30    CONTINUE
      END IF
C
 9000 RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  I1KST
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    August 9, 1983
C
C  Purpose:    Return control information about the workspace stack.
C
C  Usage:      I1KST(NFACT)
C
C  Arguments:
C     NFACT  - Integer value between 1 and 6 inclusive returns the
C                 following information: (Input)
C                   NFACT = 1 - LOUT: number of current allocations
C                               excluding permanent storage. At the
C                               end of a run, there should be no
C                               active allocations.
C                   NFACT = 2 - LNOW: current active length
C                   NFACT = 3 - LTOTAL: total storage used thus far
C                   NFACT = 4 - LMAX: maximum storage allowed
C                   NFACT = 5 - LALC: total number of allocations made
C                               by I1KGT thus far
C                   NFACT = 6 - LNEED: number of numeric storage units
C                               by which the stack size must be
C                               increased for all past allocations
C                               to succeed
C     I1KST  - Integer function. (Output) Returns a workspace stack
C              statistic according to value of NFACT.
C
C  Copyright:  1983 by IMSL, Inc.  All Rights Reserved
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION I1KST (NFACT)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    NFACT
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    ISTATS(7)
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      LOGICAL    FIRST
      SAVE       FIRST
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                                  SPECIFICATIONS FOR COMMON /WORKSP/
      REAL       RWKSP(5000)
      REAL       RDWKSP(5000)
      DOUBLE PRECISION DWKSP(2500)
      COMPLEX    CWKSP(2500)
      COMPLEX    CZWKSP(2500)
      COMPLEX    *16 ZWKSP(1250)
      INTEGER    IWKSP(5000)
      LOGICAL    LWKSP(5000)
      EQUIVALENCE (DWKSP(1), RWKSP(1))
      EQUIVALENCE (CWKSP(1), RWKSP(1)), (ZWKSP(1), RWKSP(1))
      EQUIVALENCE (IWKSP(1), RWKSP(1)), (LWKSP(1), RWKSP(1))
      EQUIVALENCE (RDWKSP(1), RWKSP(1)), (CZWKSP(1), RWKSP(1))
      COMMON     /WORKSP/ DWKSP
C                                  SPECIFICATIONS FOR EQUIVALENCE
      EQUIVALENCE (ISTATS(1), IWKSP(1))
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, IWKIN
C
      DATA FIRST/.TRUE./
C
      IF (FIRST) THEN
C                                  INITIALIZE WORKSPACE IF NEEDED
         FIRST = .FALSE.
         CALL IWKIN (0)
      END IF
C
      IF (NFACT.LE.0 .OR. NFACT.GE.7) THEN
         CALL E1MES (5, 9, 'Error from subroutine I1KST:  Argument'//
     &               ' for I1KST must be between 1 and 6 inclusive.')
      ELSE IF (NFACT .EQ. 1) THEN
C                                  LOUT
         I1KST = ISTATS(1)
      ELSE IF (NFACT .EQ. 2) THEN
C                                  LNOW + PERMANENT
         I1KST = ISTATS(2) + (ISTATS(5)-ISTATS(4)+1)
      ELSE IF (NFACT .EQ. 3) THEN
C                                  LUSED + PERMANENT
         I1KST = ISTATS(3) + (ISTATS(5)-ISTATS(4)+1)
      ELSE IF (NFACT .EQ. 4) THEN
C                                  LMAX
         I1KST = ISTATS(5)
      ELSE IF (NFACT .EQ. 5) THEN
C                                  LALC
         I1KST = ISTATS(6)
      ELSE IF (NFACT .EQ. 6) THEN
C                                  LNEED
         I1KST = ISTATS(7)
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  I1X (Single precision version)
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    August 30, 1985
C
C  Purpose:    Determine the array subscript indicating the starting
C              element at which a key character sequence begins.
C              (Case-sensitive version)
C
C  Usage:      I1X(CHRSTR, I1LEN, KEY, KLEN)
C
C  Arguments:
C     CHRSTR - Character array to be searched.  (Input)
C     I1LEN  - Length of CHRSTR.  (Input)
C     KEY    - Character array that contains the key sequence.  (Input)
C     KLEN   - Length of KEY.  (Input)
C     I1X    - Integer function.  (Output)
C
C  Remarks:
C  1. Returns zero when there is no match.
C
C  2. Returns zero if KLEN is longer than ISLEN.
C
C  3. Returns zero when any of the character arrays has a negative or
C     zero length.
C
C  GAMS:       N5c
C
C  Chapter:    MATH/LIBRARY Utilities
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION I1X (CHRSTR, I1LEN, KEY, KLEN)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    I1LEN, KLEN
      CHARACTER  CHRSTR(*), KEY(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, II, J
C
      I1X = 0
      IF (KLEN.LE.0 .OR. I1LEN.LE.0) GO TO 9000
      IF (KLEN .GT. I1LEN) GO TO 9000
C
      I = 1
      II = I1LEN - KLEN + 1
   10 IF (I .LE. II) THEN
         IF (CHRSTR(I) .EQ. KEY(1)) THEN
            DO 20  J=2, KLEN
               IF (CHRSTR(I+J-1) .NE. KEY(J)) GO TO 30
   20       CONTINUE
            I1X = I
            GO TO 9000
   30       CONTINUE
         END IF
         I = I + 1
         GO TO 10
      END IF
C
 9000 RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  IACHAR (Single precision version)
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    September 9, 1985
C
C  Purpose:    Return the integer ASCII value of a character argument.
C
C  Usage:      IACHAR(CH)
C
C  Arguments:
C     CH     - Character argument for which the integer ASCII value
C              is desired.  (Input)
C     IACHAR - Integer ASCII value for CH.  (Output)
C              The character CH is in the IACHAR-th position of the
C              ASCII collating sequence.
C
C  Keywords:   Utilities; Character string manipulation;
C              Character conversion
C
C  GAMS:       N3
C
C  Chapter:    MATH/LIBRARY Utilities
C              STAT/LIBRARY Utilities
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION IACHAR (CH)
C                                  SPECIFICATIONS FOR ARGUMENTS
      CHARACTER  CH
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      IACHAR = ICHAR(CH)
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  ICASE (Single precision version)
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    September 9, 1985
C
C  Purpose:    Convert from character to the integer ASCII value without
C              regard to case.
C
C  Usage:      ICASE(CH)
C
C  Arguments:
C     CH     - Character to be converted.  (Input)
C     ICASE  - Integer ASCII value for CH without regard to the case
C              of CH.  (Output)
C              ICASE returns the same value as IMSL routine IACHAR for
C              all but lowercase letters.  For these, it returns the
C              IACHAR value for the corresponding uppercase letter.
C
C  GAMS:       N3
C
C  Chapter:    MATH/LIBRARY Utilities
C              STAT/LIBRARY Utilities
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION ICASE (CH)
C                                  SPECIFICATIONS FOR ARGUMENTS
      CHARACTER  CH
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   IACHAR
      INTEGER    IACHAR
C
      ICASE = IACHAR(CH)
      IF (ICASE.GE.97 .AND. ICASE.LE.122) ICASE = ICASE - 32
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  IDAMAX (Single precision version)
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    August 9, 1986
C
C  Purpose:    Find the smallest index of the component of a
C              double-precision vector having maximum absolute value.
C
C  Usage:      IDAMAX(N, DX, INCX)
C
C  Arguments:
C     N      - Length of vector X.  (Input)
C     DX     - Double precision vector of length N*INCX.  (Input)
C     INCX   - Displacement between elements of DX.  (Input)
C              X(I) is defined to be DX(1+(I-1)*INCX). INCX must be
C              greater than zero.
C     IDAMAX - The smallest index I such that DABS(X(I)) is the maximum
C              of DABS(X(J)) for J=1 to N.  (Output)
C              X(I) refers to a specific element of DX. See INCX
C              argument description.
C
C  GAMS:       D1a2
C
C  Chapters:   MATH/LIBRARY Basic Matrix/Vector Operations
C              STAT/LIBRARY Mathematical Support
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION IDAMAX (N, DX, INCX)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, INCX
      DOUBLE PRECISION DX(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, II, NS
      DOUBLE PRECISION DMAX, XMAG
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  DABS
      INTRINSIC  DABS
      DOUBLE PRECISION DABS
C
      IDAMAX = 0
      IF (N .GE. 1) THEN
         IDAMAX = 1
         IF (N .GT. 1) THEN
            IF (INCX .NE. 1) THEN
C                                  CODE FOR INCREMENTS NOT EQUAL TO 1.
               DMAX = DABS(DX(1))
               NS = N*INCX
               II = 1
               DO 10  I=1, NS, INCX
                  XMAG = DABS(DX(I))
                  IF (XMAG .GT. DMAX) THEN
                     IDAMAX = II
                     DMAX = XMAG
                  END IF
                  II = II + 1
   10          CONTINUE
            ELSE
C                                  CODE FOR INCREMENTS EQUAL TO 1.
               DMAX = DABS(DX(1))
               DO 20  I=2, N
                  XMAG = DABS(DX(I))
                  IF (XMAG .GT. DMAX) THEN
                     IDAMAX = I
                     DMAX = XMAG
                  END IF
   20          CONTINUE
            END IF
         END IF
      END IF
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  IMACH (Single precision version)
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    March 26, 1984
C
C  Purpose:    Retrieve integer machine constants.
C
C  Usage:      IMACH(N)
C
C  Arguments:
C     N      - Index of desired constant.  (Input)
C     IMACH  - Machine constant.  (Output)
C
C  Remark:
C     Following is a description of the assorted integer machine
C     constants.
C
C     Words
C
C        IMACH( 1) = Number of bits per integer storage unit.
C        IMACH( 2) = Number of characters per integer storage unit.
C
C     Integers
C
C        Assume integers are represented in the S-DIGIT, BASE-A form
C        SIGN ( X(S-1)*A**(S-1) + ... + X(1)*A + X(0) )
C        where 0 .LE. X(I) .LT. A for I=0,...,S-1.  Then
C
C        IMACH( 3) = A, the base.
C        IMACH( 4) = S, number of BASE-A digits.
C        IMACH( 5) = A**S - 1, largest magnitude.
C
C     Floating-point numbers
C
C        Assume floating-point numbers are represented in the T-DIGIT,
C        BASE-B form SIGN (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
C        where 0 .LE. X(I) .LT. B for I=1,...,T,
C        0 .LT. X(1), and EMIN .LE. E .LE. EMAX.  Then
C
C        IMACH( 6) = B, the base.
C
C        Single precision
C
C           IMACH( 7) = T, number of BASE-B digits.
C           IMACH( 8) = EMIN, smallest exponent E.
C           IMACH( 9) = EMAX, largest exponent E.
C
C        Double precision
C
C           IMACH(10) = T, number of BASE-B digits.
C           IMACH(11) = EMIN, smallest exponent E.
C           IMACH(12) = EMAX, largest exponent E.
C
C  GAMS:       R1
C
C  Chapters:   MATH/LIBRARY Reference Material
C              STAT/LIBRARY Reference Material
C              SFUN/LIBRARY Reference Material
C
C  Copyright:  1984 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION IMACH (N)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    NOUT
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      INTEGER    IMACHV(12)
      SAVE       IMACHV
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   UMACH
C                                  DEFINE CONSTANTS
      DATA IMACHV(1)/32/
      DATA IMACHV(2)/4/
      DATA IMACHV(3)/2/
      DATA IMACHV(4)/31/
      DATA IMACHV(5)/2147483647/
      DATA IMACHV(6)/2/
      DATA IMACHV(7)/24/
      DATA IMACHV(8)/-125/
      DATA IMACHV(9)/128/
      DATA IMACHV(10)/53/
      DATA IMACHV(11)/-1021/
      DATA IMACHV(12)/1024/
C
      IF (N.LT.1 .OR. N.GT.12) THEN
C                                  ERROR.  INVALID RANGE FOR N.
         CALL UMACH (2, NOUT)
         WRITE (NOUT,99999) N
99999    FORMAT (/, ' *** TERMINAL ERROR 5 from IMACH.  The argument',
     &          /, ' ***          must be between 1 and 12 inclusive.'
     &          , /, ' ***          N = ', I6, '.', /)
         IMACH = 0
         STOP
C
      ELSE
         IMACH = IMACHV(N)
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  IWKIN (Single precision version)
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    January 17, 1984
C
C  Purpose:    Initialize bookkeeping locations describing the
C              workspace stack.
C
C  Usage:      CALL IWKIN (NSU)
C
C  Argument:
C     NSU    - Number of numeric storage units to which the workspace
C              stack is to be initialized
C
C  GAMS:       N4
C
C  Chapters:   MATH/LIBRARY Reference Material
C              STAT/LIBRARY Reference Material
C
C  Copyright:  1984 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE IWKIN (NSU)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    NSU
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    ISIZE(6), LALC, LBND, LBOOK, LMAX, LNEED, LNOW, LOUT,
     &           LUSED, MELMTS, MTYPE
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      LOGICAL    FIRST
      SAVE       FIRST
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                                  SPECIFICATIONS FOR COMMON /WORKSP/
      REAL       RWKSP(5000)
      REAL       RDWKSP(5000)
      DOUBLE PRECISION DWKSP(2500)
      COMPLEX    CWKSP(2500)
      COMPLEX    CZWKSP(2500)
      COMPLEX    *16 ZWKSP(1250)
      INTEGER    IWKSP(5000)
      LOGICAL    LWKSP(5000)
      EQUIVALENCE (DWKSP(1), RWKSP(1))
      EQUIVALENCE (CWKSP(1), RWKSP(1)), (ZWKSP(1), RWKSP(1))
      EQUIVALENCE (IWKSP(1), RWKSP(1)), (LWKSP(1), RWKSP(1))
      EQUIVALENCE (RDWKSP(1), RWKSP(1)), (CZWKSP(1), RWKSP(1))
      COMMON     /WORKSP/ DWKSP
C                                  SPECIFICATIONS FOR EQUIVALENCE
      EQUIVALENCE (LOUT, IWKSP(1))
      EQUIVALENCE (LNOW, IWKSP(2))
      EQUIVALENCE (LUSED, IWKSP(3))
      EQUIVALENCE (LBND, IWKSP(4))
      EQUIVALENCE (LMAX, IWKSP(5))
      EQUIVALENCE (LALC, IWKSP(6))
      EQUIVALENCE (LNEED, IWKSP(7))
      EQUIVALENCE (LBOOK, IWKSP(8))
      EQUIVALENCE (ISIZE(1), IWKSP(11))
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  MAX0
      INTRINSIC  MAX0
      INTEGER    MAX0
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1STI
C
      DATA FIRST/.TRUE./
C
      IF (.NOT.FIRST) THEN
         IF (NSU .NE. 0) THEN
            CALL E1STI (1, LMAX)
            CALL E1MES (5, 100, 'Error from subroutine IWKIN:  '//
     &                  'Workspace stack has previously been '//
     &                  'initialized to %(I1). Correct by making the '//
     &                  'call to IWKIN the first executable '//
     &                  'statement in the main program.  ')
C
            STOP
C
         ELSE
            RETURN
         END IF
      END IF
C
      IF (NSU .EQ. 0) THEN
C                                  IF NSU=0 USE DEFAULT SIZE 5000
         MELMTS = 5000
      ELSE
         MELMTS = NSU
      END IF
C                                  NUMBER OF ITEMS .LT. 0
      IF (MELMTS .LE. 0) THEN
         CALL E1STI (1, MELMTS)
         CALL E1MES (5, 1, 'Error from subroutine IWKIN:  Number '//
     &               'of numeric storage units is not positive. NSU '//
     &               '= %(I1) ')
      ELSE
C
         FIRST = .FALSE.
C                                  HERE TO INITIALIZE
C
C                                  SET DATA SIZES APPROPRIATE FOR A
C                                  STANDARD CONFORMING FORTRAN SYSTEM
C                                  USING THE FORTRAN
C                                  *NUMERIC STORAGE UNIT* AS THE
C                                  MEASURE OF SIZE.
C
C                                  TYPE IS REAL
         MTYPE = 3
C                                  LOGICAL
         ISIZE(1) = 1
C                                  INTEGER
         ISIZE(2) = 1
C                                  REAL
         ISIZE(3) = 1
C                                  DOUBLE PRECISION
         ISIZE(4) = 2
C                                  COMPLEX
         ISIZE(5) = 2
C                                  DOUBLE COMPLEX
         ISIZE(6) = 4
C                                  NUMBER OF WORDS USED FOR BOOKKEEPING
         LBOOK = 16
C                                  CURRENT ACTIVE LENGTH OF THE STACK
         LNOW = LBOOK
C                                  MAXIMUM VALUE OF LNOW ACHIEVED THUS
C                                  FAR
         LUSED = LBOOK
C                                  MAXIMUM LENGTH OF THE STORAGE ARRAY
         LMAX = MAX0(MELMTS,((LBOOK+2)*ISIZE(2)+ISIZE(3)-1)/ISIZE(3))
C                                  LOWER BOUND OF THE PERMANENT STORAGE
C                                  WHICH IS ONE WORD MORE THAN THE
C                                  MAXIMUM ALLOWED LENGTH OF THE STACK
         LBND = LMAX + 1
C                                  NUMBER OF CURRENT ALLOCATIONS
         LOUT = 0
C                                  TOTAL NUMBER OF ALLOCATIONS MADE
         LALC = 0
C                                  NUMBER OF WORDS BY WHICH THE ARRAY
C                                  SIZE MUST BE INCREASED FOR ALL PAST
C                                  ALLOCATIONS TO SUCCEED
         LNEED = 0
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  M1VE
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    March 5, 1984
C
C  Purpose:    Move a subset of one character array to another.
C
C  Usage:      CALL M1VE(INSTR, INBEG, INEND, INLEN, OUTSTR, OUTBEG,
C                         OUTEND, OUTLEN, IER)
C
C  Arguments:
C     INSTR  - Source character array.  (Input)
C     INBEG  - First element of INSTR to be moved.  (Input)
C     INEND  - Last element of INSTR to be moved.  (Input)
C              The source subset is INSTR(INBEG),...,INSTR(INEND).
C     INLEN  - Length of INSTR.  (Input)
C     OUTSTR - Destination character array.  (Output)
C     IUTBEG - First element of OUTSTR destination.  (Input)
C     IUTEND - Last element of OUTSTR  destination.  (Input)
C              The destination subset is OUTSRT(IUTBEG),...,
C              OUTSTR(IUTEND).
C     IUTLEN - Length of OUTSTR.  (Input)
C     IER    - Completion code.  (Output)
C              IER = -2  indicates that the input parameters, INBEG,
C                        INEND, INLEN, IUTBEG, IUTEND are not
C                        consistent.  One of the conditions
C                        INBEG.GT.0, INEND.GE.INBEG, INLEN.GE.INEND,
C                        IUTBEG.GT.0, or IUTEND.GE.IUTBEG is not
C                        satisfied.
C              IER = -1  indicates that the length of OUTSTR is
C                        insufficient to hold the subset of INSTR.
C                        That is, IUTLEN is less than IUTEND.
C              IER =  0  indicates normal completion
C              IER >  0  indicates that the specified subset of OUTSTR,
C                        OUTSTR(IUTBEG),...,OUTSTR(IUTEND) is not long
C                        enough to hold the subset INSTR(INBEG),...,
C                        INSTR(INEND) of INSTR.  IER is set to the
C                        number of characters that were not moved.
C
C  Remarks:
C  1. If the subset of OUTSTR is longer than the subset of INSTR,
C     trailing blanks are moved to OUTSTR.
C  2. If the subset of INSTR is longer than the subset of OUTSTR,
C     the shorter subset is moved to OUTSTR and IER is set to the number
C     of characters that were not moved to OUTSTR.
C  3. If the length of OUTSTR is insufficient to hold the subset,
C     IER is set to -2 and nothing is moved.
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE M1VE (INSTR, INBEG, INEND, INLEN, OUTSTR, IUTBEG,
     &                 IUTEND, IUTLEN, IER)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    INBEG, INEND, INLEN, IUTBEG, IUTEND, IUTLEN, IER
      CHARACTER  INSTR(*), OUTSTR(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    IUTLAS, KI, KO
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      CHARACTER  BLANK
      SAVE       BLANK
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  MIN0
      INTRINSIC  MIN0
      INTEGER    MIN0
C
      DATA BLANK/' '/
C                                  CHECK INBEG, INEND, INLEN, IUTBEG,
C                                  AND IUTEND
C
      IF (INBEG.LE.0 .OR. INEND.LT.INBEG .OR. INLEN.LT.INEND .OR.
     &    IUTBEG.LE.0 .OR. IUTEND.LT.IUTBEG) THEN
         IER = -2
         RETURN
      ELSE IF (IUTLEN .LT. IUTEND) THEN
         IER = -1
         RETURN
      END IF
C                                  DETERMINE LAST CHARACTER TO M1VE
      IUTLAS = IUTBEG + MIN0(INEND-INBEG,IUTEND-IUTBEG)
C                                  M1VE CHARACTERS
      KI = INBEG
      DO 10  KO=IUTBEG, IUTLAS
         OUTSTR(KO) = INSTR(KI)
         KI = KI + 1
   10 CONTINUE
C                                   SET IER TO NUMBER OF CHARACTERS THAT
C                                   WHERE NOT MOVED
      IER = KI - INEND - 1
C                                   APPEND BLANKS IF NECESSARY
      DO 20  KO=IUTLAS + 1, IUTEND
         OUTSTR(KO) = BLANK
   20 CONTINUE
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  M1VECH
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    December 31, 1984
C
C  Purpose:    Character substring assignment.
C
C  Usage:      CALL M1VECH (STR1, LEN1, STR2, LEN2)
C
C  Arguments:
C     STR1   - Source substring.  (Input)
C              The source substring is STR1(1:LEN1).
C     LEN1   - Length of STR1.  (Input)
C     STR2   - Destination substring.  (Output)
C              The destination substring is STR2(1:LEN2).
C     LEN2   - Length of STR2.  (Input)
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE M1VECH (STR1, LEN1, STR2, LEN2)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    LEN1, LEN2
      CHARACTER  STR1(*), STR2(*)
C
      STR2(1:LEN2) = STR1(1:LEN1)
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  N1RCD
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    March 6, 1984
C
C  Purpose:    Retrieve an error code.
C
C  Usage:      N1RCD(IOPT)
C
C  Arguments:
C     IOPT   - Integer specifying the level.  (Input)
C              If IOPT=0 the error code for the current level is
C              returned.  If IOPT=1 the error code for the most
C              recently called routine (last pop) is returned.
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION N1RCD (IOPT)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    IOPT
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM1/
      INTEGER    CALLVL, MAXLEV, MSGLEN, ERTYPE(51), ERCODE(51),
     &           PRINTB(7), STOPTB(7), PLEN, IFERR6, IFERR7,
     &           IALLOC(51), HDRFMT(7), TRACON(7)
      COMMON     /ERCOM1/ CALLVL, MAXLEV, MSGLEN, ERTYPE, ERCODE,
     &           PRINTB, STOPTB, PLEN, IFERR6, IFERR7, IALLOC, HDRFMT,
     &           TRACON
      SAVE       /ERCOM1/
C                              SPECIFICATIONS FOR COMMON /ERCOM2/
      CHARACTER  MSGSAV(255), PLIST(300), RNAME(51)*6
      COMMON     /ERCOM2/ MSGSAV, PLIST, RNAME
      SAVE       /ERCOM2/
C                              SPECIFICATIONS FOR COMMON /ERCOM3/
      DOUBLE PRECISION ERCKSM
      COMMON     /ERCOM3/ ERCKSM
      SAVE       /ERCOM3/
C                              SPECIFICATIONS FOR COMMON /ERCOM4/
      LOGICAL    ISUSER(51)
      COMMON     /ERCOM4/ ISUSER
      SAVE       /ERCOM4/
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1PRT, M1VECH
C
      IF (IOPT.NE.0 .AND. IOPT.NE.1) THEN
         ERTYPE(CALLVL) = 5
         ERCODE(CALLVL) = 1
         MSGLEN = 47
         CALL M1VECH ('.  The argument passed to N1RCD must be 0 or '//
     &                '1. ', MSGLEN, MSGSAV, MSGLEN)
         CALL E1PRT
         STOP
      ELSE
         N1RCD = ERCODE(CALLVL+IOPT)
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  N1RGB
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    March 2, 1984
C
C  Purpose:    Return a positive number as a flag to indicated that a
C              stop should occur due to one or more global errors.
C
C  Usage:      N1RGB(IDUMMY)
C
C  Arguments:
C     IDUMMY - Integer scalar dummy argument.
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION N1RGB (IDUMMY)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    IDUMMY
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM1/
      INTEGER    CALLVL, MAXLEV, MSGLEN, ERTYPE(51), ERCODE(51),
     &           PRINTB(7), STOPTB(7), PLEN, IFERR6, IFERR7,
     &           IALLOC(51), HDRFMT(7), TRACON(7)
      COMMON     /ERCOM1/ CALLVL, MAXLEV, MSGLEN, ERTYPE, ERCODE,
     &           PRINTB, STOPTB, PLEN, IFERR6, IFERR7, IALLOC, HDRFMT,
     &           TRACON
      SAVE       /ERCOM1/
C                              SPECIFICATIONS FOR COMMON /ERCOM2/
      CHARACTER  MSGSAV(255), PLIST(300), RNAME(51)*6
      COMMON     /ERCOM2/ MSGSAV, PLIST, RNAME
      SAVE       /ERCOM2/
C                              SPECIFICATIONS FOR COMMON /ERCOM3/
      DOUBLE PRECISION ERCKSM
      COMMON     /ERCOM3/ ERCKSM
      SAVE       /ERCOM3/
C                              SPECIFICATIONS FOR COMMON /ERCOM4/
      LOGICAL    ISUSER(51)
      COMMON     /ERCOM4/ ISUSER
      SAVE       /ERCOM4/
C                                  INITIALIZE FUNCTION
      N1RGB = 0
C                                  CHECK FOR GLOBAL ERROR TYPE 6
      IF (IFERR6 .GT. 0) THEN
         N1RGB = STOPTB(6)
         IFERR6 = 0
      END IF
C                                  CHECK FOR GLOBAL ERROR TYPE 7
      IF (IFERR7 .GT. 0) THEN
         N1RGB = STOPTB(7)
         IFERR7 = 0
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  N1RTY
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    March 6, 1984
C
C  Purpose:    Retrieve an error type.
C
C  Usage:      N1RTY(IOPT)
C
C  Arguments:
C     IOPT   - Integer specifying the level.  (Input)
C              If IOPT=0 the error type for the current level is
C              returned.  If IOPT=1 the error type for the most
C              recently called routine (last pop) is returned.
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION N1RTY (IOPT)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    IOPT
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM1/
      INTEGER    CALLVL, MAXLEV, MSGLEN, ERTYPE(51), ERCODE(51),
     &           PRINTB(7), STOPTB(7), PLEN, IFERR6, IFERR7,
     &           IALLOC(51), HDRFMT(7), TRACON(7)
      COMMON     /ERCOM1/ CALLVL, MAXLEV, MSGLEN, ERTYPE, ERCODE,
     &           PRINTB, STOPTB, PLEN, IFERR6, IFERR7, IALLOC, HDRFMT,
     &           TRACON
      SAVE       /ERCOM1/
C                              SPECIFICATIONS FOR COMMON /ERCOM2/
      CHARACTER  MSGSAV(255), PLIST(300), RNAME(51)*6
      COMMON     /ERCOM2/ MSGSAV, PLIST, RNAME
      SAVE       /ERCOM2/
C                              SPECIFICATIONS FOR COMMON /ERCOM3/
      DOUBLE PRECISION ERCKSM
      COMMON     /ERCOM3/ ERCKSM
      SAVE       /ERCOM3/
C                              SPECIFICATIONS FOR COMMON /ERCOM4/
      LOGICAL    ISUSER(51)
      COMMON     /ERCOM4/ ISUSER
      SAVE       /ERCOM4/
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1PRT, M1VECH
C
      IF (IOPT.NE.0 .AND. IOPT.NE.1) THEN
         ERTYPE(CALLVL) = 5
         ERCODE(CALLVL) = 1
         MSGLEN = 47
         CALL M1VECH ('.  The argument passed to N1RTY must be 0 or '//
     &                '1. ', MSGLEN, MSGSAV, MSGLEN)
         CALL E1PRT
         STOP
      ELSE
         N1RTY = ERTYPE(CALLVL+IOPT)
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  S1ANUM
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    March 28, 1984
C
C  Purpose:    Scan a token and identify it as follows: integer, real
C              number (single/double), FORTRAN relational operator,
C              FORTRAN logical operator, or FORTRAN logical constant.
C
C  Usage:      CALL S1ANUM(INSTR, SLEN, CODE, OLEN)
C
C  Arguments:
C     INSTR  - Character string to be scanned.  (Input)
C     SLEN   - Length of INSTR.  (Input)
C     CODE   - Token code.  (Output)  Where
C                 CODE =  0  indicates an unknown token,
C                 CODE =  1  indicates an integer number,
C                 CODE =  2  indicates a (single precision) real number,
C                 CODE =  3  indicates a (double precision) real number,
C                 CODE =  4  indicates a logical constant (.TRUE. or
C                               .FALSE.),
C                 CODE =  5  indicates the relational operator .EQ.,
C                 CODE =  6  indicates the relational operator .NE.,
C                 CODE =  7  indicates the relational operator .LT.,
C                 CODE =  8  indicates the relational operator .LE.,
C                 CODE =  9  indicates the relational operator .GT.,
C                 CODE = 10  indicates the relational operator .GE.,
C                 CODE = 11  indicates the logical operator .AND.,
C                 CODE = 12  indicates the logical operator .OR.,
C                 CODE = 13  indicates the logical operator .EQV.,
C                 CODE = 14  indicates the logical operator .NEQV.,
C                 CODE = 15  indicates the logical operator .NOT..
C     OLEN   - Length of the token as counted from the first character
C              in INSTR.  (Output)  OLEN returns a zero for an unknown
C              token (CODE = 0).
C
C  Remarks:
C  1. Blanks are considered significant.
C  2. Lower and upper case letters are not significant.
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE S1ANUM (INSTR, SLEN, CODE, OLEN)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    SLEN, CODE, OLEN
      CHARACTER  INSTR(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, IBEG, IIBEG, J
      LOGICAL    FLAG
      CHARACTER  CHRSTR(6)
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      INTEGER    TABPTR(16), TDCNST, TICNST, TOKEN(13), TRCNST, TZERR
      CHARACTER  DIGIT(10), LETTER(52), MINUS, PERIOD, PLUS, TABLE(38)
      SAVE       DIGIT, LETTER, MINUS, PERIOD, PLUS, TABLE, TABPTR,
     &           TDCNST, TICNST, TOKEN, TRCNST, TZERR
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   I1X, I1CSTR
      INTEGER    I1X, I1CSTR
C
      DATA TOKEN/5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 4, 4/
      DATA TABLE/'D', 'E', 'E', 'Q', 'N', 'E', 'L', 'T', 'L',
     &     'E', 'G', 'T', 'G', 'E', 'A', 'N', 'D', 'O', 'R',
     &     'E', 'Q', 'V', 'N', 'E', 'Q', 'V', 'N', 'O', 'T',
     &     'T', 'R', 'U', 'E', 'F', 'A', 'L', 'S', 'E'/
      DATA TABPTR/1, 2, 3, 5, 7, 9, 11, 13, 15, 18, 20, 23, 27, 30,
     &     34, 39/
      DATA DIGIT/'0', '1', '2', '3', '4', '5', '6', '7', '8',
     &     '9'/
      DATA LETTER/'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I',
     &     'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S',
     &     'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c',
     &     'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
     &     'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w',
     &     'x', 'y', 'z'/
      DATA PERIOD/'.'/, PLUS/'+'/, MINUS/'-'/
      DATA TZERR/0/, TICNST/1/
      DATA TRCNST/2/, TDCNST/3/
C
      IF (SLEN .LE. 0) THEN
         CODE = 0
         OLEN = 0
         RETURN
      END IF
C                                  STATE 0 - ASSUME ERROR TOKEN
      IBEG = 1
      CODE = TZERR
C                                  CHECK SIGN
      IF (INSTR(IBEG).EQ.MINUS .OR. INSTR(IBEG).EQ.PLUS) THEN
         FLAG = .TRUE.
         IIBEG = IBEG
         IBEG = IBEG + 1
      ELSE
         FLAG = .FALSE.
      END IF
C                                  STATE 1 - ASSUME INTEGER CONSTANT
      IF (I1X(DIGIT,10,INSTR(IBEG),1) .NE. 0) THEN
         CODE = TICNST
         IIBEG = IBEG
         IBEG = IBEG + 1
C
   10    IF (IBEG .LE. SLEN) THEN
C
            IF (I1X(DIGIT,10,INSTR(IBEG),1) .NE. 0) THEN
               IIBEG = IBEG
               IBEG = IBEG + 1
               GO TO 10
C
            END IF
C
         ELSE
            GO TO 80
C
         END IF
C
         IF (INSTR(IBEG) .NE. PERIOD) GO TO 80
      END IF
C                                  STATE 2 - ASSUME REAL CONSTANT
      IF (CODE .EQ. TICNST) THEN
         CODE = TRCNST
         IIBEG = IBEG
         IBEG = IBEG + 1
         IF (IBEG .GT. SLEN) GO TO 80
      ELSE IF (INSTR(IBEG).EQ.PERIOD .AND. SLEN.GE.2) THEN
         IF (I1X(DIGIT,10,INSTR(IBEG+1),1) .NE. 0) THEN
            CODE = TRCNST
            IIBEG = IBEG + 1
            IBEG = IBEG + 2
            IF (IBEG .GT. SLEN) GO TO 80
         END IF
      END IF
C
      IF (I1X(DIGIT,10,INSTR(IBEG),1) .NE. 0) THEN
         CODE = TRCNST
         IIBEG = IBEG
         IBEG = IBEG + 1
C
   20    IF (IBEG .LE. SLEN) THEN
C
            IF (I1X(DIGIT,10,INSTR(IBEG),1) .NE. 0) THEN
               IIBEG = IBEG
               IBEG = IBEG + 1
               GO TO 20
C
            END IF
C
         ELSE
            GO TO 80
C
         END IF
C
      END IF
C
      IF (CODE .EQ. TZERR) THEN
         IF (INSTR(IBEG) .NE. PERIOD) GO TO 80
         IBEG = IBEG + 1
         IF (IBEG .GT. SLEN) GO TO 80
      END IF
C
      IF (I1X(LETTER,52,INSTR(IBEG),1) .EQ. 0) GO TO 80
      CHRSTR(1) = INSTR(IBEG)
C
      DO 30  I=2, 6
         IBEG = IBEG + 1
         IF (IBEG .GT. SLEN) GO TO 80
         IF (I1X(LETTER,52,INSTR(IBEG),1) .EQ. 0) GO TO 40
         CHRSTR(I) = INSTR(IBEG)
   30 CONTINUE
C
      GO TO 80
C
   40 CONTINUE
C
      DO 50  J=1, 15
         IF (I1CSTR(CHRSTR,I-1,TABLE(TABPTR(J)),TABPTR(J+1)-TABPTR(J))
     &        .EQ. 0) GO TO 60
   50 CONTINUE
C
      GO TO 80
C                                  STATE 4 - LOGICAL OPERATOR
   60 IF (J .GT. 2) THEN
C
         IF (CODE .EQ. TRCNST) THEN
C
            IF (INSTR(IBEG) .EQ. PERIOD) THEN
               CODE = TICNST
               IIBEG = IIBEG - 1
            END IF
C
            GO TO 80
C
         ELSE IF (INSTR(IBEG) .NE. PERIOD) THEN
            GO TO 80
C
         ELSE IF (FLAG) THEN
            GO TO 80
C
         ELSE
            CODE = TOKEN(J-2)
            IIBEG = IBEG
            GO TO 80
C
         END IF
C
      END IF
C                                  STATE 5 - DOUBLE PRECISION CONSTANT
      IF (CODE .NE. TRCNST) GO TO 80
      IF (INSTR(IBEG).EQ.MINUS .OR. INSTR(IBEG).EQ.PLUS) IBEG = IBEG +
     &    1
      IF (IBEG .GT. SLEN) GO TO 80
C
      IF (I1X(DIGIT,10,INSTR(IBEG),1) .EQ. 0) THEN
         GO TO 80
C
      ELSE
         IIBEG = IBEG
         IBEG = IBEG + 1
C
   70    IF (IBEG .LE. SLEN) THEN
C
            IF (I1X(DIGIT,10,INSTR(IBEG),1) .NE. 0) THEN
               IIBEG = IBEG
               IBEG = IBEG + 1
               GO TO 70
C
            END IF
C
         END IF
C
      END IF
C
      IF (J .EQ. 1) CODE = TDCNST
C
   80 CONTINUE
C
      IF (CODE .EQ. TZERR) THEN
         OLEN = 0
C
      ELSE
         OLEN = IIBEG
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  SSRCH (Single precision version)
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    October 16, 1985
C
C  Purpose:    Search a character vector, sorted in ascending ASCII
C              order, for a given string and return its index.
C
C  Usage:      CALL SSRCH (N, STRING, CHX, INCX, INDEX)
C
C  Arguments:
C     N      - Length of vector CHY.  (Input)
C     STRING - Character string to be searched for in CHY.  (Input)
C     CHX    - Vector of length N*INCX containing character strings.
C              (Input)
C              CHY is obtained from CHX for I = 1, 2, ..., N by
C              CHY(I) = CHX(1+(I-1)*INCX).  CHY(1), CHY(2), ..., CHY(N)
C              must be in ascending ASCII order.
C     INCX   - Displacement between elements of CHX.  (Input)
C              INCX must be greater than zero.
C     INDEX  - Index of CHY pointing to STRING.  (Output)
C              If INDEX is positive, STRING is found in CHY.
C              If INDEX is negative, STRING is not found in CHY.
C                INDEX      Location of STRING
C              ----------   ------------------------------------
C               1 thru N    STRING  = CHY(INDEX)
C                  -1       STRING < CHY(1) or N = 0
C              -N thru -2   CHY(-INDEX-1) < STRING < CHY(-INDEX)
C                -(N+1)     STRING > CHY(N)
C
C  Keywords:   Binary search; Logarithmic search; Bisection
C
C  GAMS:       N5b; N5c
C
C  Chapters:   MATH/LIBRARY Utilities
C              STAT/LIBRARY Utilities
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SSRCH (N, STRING, CHX, INCX, INDEX)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, INCX, INDEX
      CHARACTER  STRING*(*), CHX(*)*(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    IHIGH, ILOW, MID
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  LGT,LLT
      INTRINSIC  LGT, LLT
      LOGICAL    LGT, LLT
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1PSH, E1STI
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   N1RCD
      INTEGER    N1RCD
C
      CALL E1PSH ('SSRCH')
      IF (N .LT. 0) THEN
         CALL E1STI (1, N)
         CALL E1MES (5, 1, 'N = %(I1).  The length of CHX, N, must '//
     &               'be greater than or equal to 0.')
      END IF
      IF (INCX .LE. 0) THEN
         CALL E1STI (1, INCX)
         CALL E1MES (5, 2, 'INCX = %(I1).  The displacement between '//
     &               'elements of CHX, INCX, must be greater than 0.')
      END IF
      IF (N1RCD(0) .NE. 0) GO TO 9000
C                                  INCX EQUAL TO 1
      IF (INCX .EQ. 1) THEN
         ILOW = 1
         IHIGH = N
   10    MID = (ILOW+IHIGH)/2
C                                  STRING NOT FOUND IN CHX
C                                  INDEX TO BE INSERTED FOR STRING IS
C                                  RETURNED
         IF (ILOW .GT. IHIGH) THEN
            INDEX = -ILOW
            GO TO 9000
         END IF
C                                  SEARCH LEFT OF MIDPOINT
         IF (LLT(STRING,CHX(MID)) .EQV. .TRUE.) THEN
            IHIGH = MID - 1
            GO TO 10
C                                  SEARCH RIGHT OF MIDPOINT
         ELSE IF (LGT(STRING,CHX(MID)) .EQV. .TRUE.) THEN
            ILOW = MID + 1
            GO TO 10
C                                  STRING FOUND IN CHX
C                                  INDEX OF STRING RETURNED
         ELSE
            INDEX = MID
         END IF
C                                  INCX GREATER THAN 1
      ELSE
         ILOW = 1
         IHIGH = N
   20    MID = (ILOW+IHIGH)/2
C                                  STRING NOT FOUND IN CHX
C                                  INDEX TO BE INSERTED FOR STRING IS
C                                  RETURNED
         IF (ILOW .GT. IHIGH) THEN
            INDEX = -ILOW
            GO TO 9000
         END IF
C                                  SEARCH LEFT OF MIDPOINT
         IF (LLT(STRING,CHX((MID-1)*INCX+1)) .EQV. .TRUE.) THEN
            IHIGH = MID - 1
            GO TO 20
C                                  SEARCH RIGHT OF MIDPOINT
         ELSE IF (LGT(STRING,CHX((MID-1)*INCX+1)) .EQV. .TRUE.) THEN
            ILOW = MID + 1
            GO TO 20
C                                  STRING FOUND IN CHX
C                                  INDEX OF STRING RETURNED
         ELSE
            INDEX = MID
         END IF
      END IF
C                                  EXIT SECTION
 9000 CALL E1POP ('SSRCH')
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  UMACH (Single precision version)
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    March 21, 1984
C
C  Purpose:    Set or retrieve input or output device unit numbers.
C
C  Usage:      CALL UMACH (N, NUNIT)
C
C  Arguments:
C     N      - Index of desired unit.  (Input)
C              The values of N are defined as follows:
C              N = 1, corresponds to the standard input unit.
C              N = 2, corresponds to the standard output unit.
C     NUNIT  - I/O unit.  (Input or Output)
C              If the value of N is negative, the unit corresponding
C              to the index is reset to the value given in NUNIT.
C              Otherwise, the value corresponding to the index is
C              returned in NUNIT.
C
C  GAMS:       R1
C
C  Chapters:   MATH/LIBRARY Reference Material
C              STAT/LIBRARY Reference Material
C              SFUN/LIBRARY Reference Material
C
C  Copyright:  1984 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE UMACH (N, NUNIT)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, NUNIT
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    NN, NOUT
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      INTEGER    UNIT(2)
      SAVE       UNIT
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  IABS
      INTRINSIC  IABS
      INTEGER    IABS
C
      DATA UNIT(1)/5/
      DATA UNIT(2)/6/
C
      NN = IABS(N)
      IF (NN.NE.1 .AND. NN.NE.2) THEN
C                                  ERROR.  INVALID RANGE FOR N.
         NOUT = UNIT(2)
         WRITE (NOUT,99999) NN
99999    FORMAT (/, ' *** TERMINAL ERROR 5 from UMACH.  The absolute',
     &          /, ' ***          value of the index variable must be'
     &          , /, ' ***          1 or 2.  IABS(N) = ', I6,
     &          '.', /)
         STOP
C                                  CHECK FOR RESET OR RETRIEVAL
      ELSE IF (N .LT. 0) THEN
C                                  RESET
         UNIT(NN) = NUNIT
      ELSE
C                                  RETRIEVE
         NUNIT = UNIT(N)
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  DCOPY (Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    August 9, 1986
C
C  Purpose:    Copy a vector X to a vector Y, both double precision.
C
C  Usage:      CALL DCOPY (N, DX, INCX, DY, INCY)
C
C  Arguments:
C     N      - Length of vectors X and Y.  (Input)
C     DX     - Double precision vector of length MAX(N*IABS(INCX),1).
C              (Input)
C     INCX   - Displacement between elements of DX.  (Input)
C              X(I) is defined to be.. DX(1+(I-1)*INCX) if INCX .GE. 0
C              or DX(1+(I-N)*INCX) if INCX .LT. 0.
C     DY     - Double precision vector of length MAX(N*IABS(INCY),1).
C              (Output)
C              DCOPY copies X(I) to Y(I) for I=1,...,N. X(I) and Y(I)
C              refer to specific elements of DX and DY, respectively.
C              See INCX and INCY argument descriptions.
C     INCY   - Displacement between elements of DY.  (Input)
C              Y(I) is defined to be.. DY(1+(I-1)*INCY) if INCY .GE. 0
C              or DY(1+(I-N)*INCY) if INCY .LT. 0.
C
C  GAMS:       D1a
C
C  Chapters:   MATH/LIBRARY Basic Matrix/Vector Operations
C              STAT/LIBRARY Mathematical Support
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DCOPY (N, DX, INCX, DY, INCY)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, INCX, INCY
      DOUBLE PRECISION DX(*), DY(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, IX, IY, M, MP1
C                                  SPECIFICATIONS FOR SPECIAL CASES
C     INTRINSIC  MOD
      INTRINSIC  MOD
      INTEGER    MOD
C
      IF (N .GT. 0) THEN
         IF (INCX.NE.1 .OR. INCY.NE.1) THEN
C                                  CODE FOR UNEQUAL INCREMENTS.
            IX = 1
            IY = 1
            IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
            IF (INCY .LT. 0) IY = (-N+1)*INCY + 1
            DO 10  I=1, N
               DY(IY) = DX(IX)
               IX = IX + INCX
               IY = IY + INCY
   10       CONTINUE
         ELSE
C                                  CODE FOR BOTH INCREMENTS EQUAL TO 1
C                                  CLEAN-UP LOOP SO REMAINING VECTOR
C                                  LENGTH IS A MULTIPLE OF 7.
            M = MOD(N,7)
            DO 30  I=1, M
               DY(I) = DX(I)
   30       CONTINUE
            MP1 = M + 1
            DO 40  I=MP1, N, 7
               DY(I) = DX(I)
               DY(I+1) = DX(I+1)
               DY(I+2) = DX(I+2)
               DY(I+3) = DX(I+3)
               DY(I+4) = DX(I+4)
               DY(I+5) = DX(I+5)
               DY(I+6) = DX(I+6)
   40       CONTINUE
         END IF
      END IF
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  CRGRG/DCRGRG (Single/Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    June 5, 1985
C
C  Purpose:    Copy a real general matrix.
C
C  Usage:      CALL CRGRG (N, A, LDA, B, LDB)
C
C  Arguments:
C     N      - Order of the matrices.  (Input)
C     A      - Matrix of order N.  (Input)
C     LDA    - Leading dimension of A exactly as specified in the
C              dimension statement of the calling program.  (Input)
C     B      - Matrix of order N containing a copy of A.  (Output)
C     LDB    - Leading dimension of B exactly as specified in the
C              dimension statement of the calling program.  (Input)
C
C  GAMS:       D1b8
C
C  Chapter:    MATH/LIBRARY Basic Matrix/Vector Operations
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DCRGRG (N, A, LDA, B, LDB)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, LDA, LDB
      DOUBLE PRECISION A(LDA,*), B(LDB,*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    J
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1PSH, E1STI, DCOPY
C
      CALL E1PSH ('DCRGRG ')
C                                  Check N
      IF (N .LT. 1) THEN
         CALL E1STI (1, N)
         CALL E1MES (5, 1, 'The argument N = %(I1).  It must be at '//
     &               'least 1.')
         GO TO 9000
      END IF
C                                  Check LDA
      IF (LDA .LT. N) THEN
         CALL E1STI (1, LDA)
         CALL E1STI (2, N)
         CALL E1MES (5, 2, 'The argument LDA = %(I1).  It must be at '//
     &               'least as large as N = %(I2).')
         GO TO 9000
      END IF
C                                  Check LDB
      IF (LDB .LT. N) THEN
         CALL E1STI (1, LDB)
         CALL E1STI (2, N)
         CALL E1MES (5, 3, 'The argument LDB = %(I1).  It must be at '//
     &               'least as large as N = %(I2).')
         GO TO 9000
      END IF
C                                  Copy
      IF (LDA.EQ.N .AND. LDB.EQ.N) THEN
         CALL DCOPY (N*N, A, 1, B, 1)
      ELSE IF (LDA .GE. LDB) THEN
         DO 10  J=1, N
            CALL DCOPY (N, A(1,J), 1, B(1,J), 1)
   10    CONTINUE
      ELSE
         DO 20  J=N, 1, -1
            CALL DCOPY (N, A(1,J), -1, B(1,J), -1)
   20    CONTINUE
      END IF
C
 9000 CONTINUE
      CALL E1POP ('DCRGRG ')
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  DGER  (Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    July 17, 1986
C
C  Purpose:    Perform the rank-one matrix update A = alpha*x*y' + A,
C              all double precision.
C
C  Usage:      CALL DGER (M, N, ALPHA, X, INCX, Y, INCY, A, LDA)
C
C  Arguments:
C     M      - Number of rows in A.  (Input)
C     N      - Number of columns in A.  (Input)
C     ALPHA  - Double precision scalar.  (Input)
C     X      - Double precision vector of length (M-1)*IABS(INCX)+1.
C              (Input)
C     INCX   - Displacement between elements of X.  (Input)
C     Y      - Double precision vector of length (N-1)*IABS(INCY)+1.
C              (Input)
C     INCY   - Displacement between elements of Y.  (Input)
C     A      - Double precision array of size M by N.  (Input/Output)
C              On input, A contains the matrix to be updated.
C              On output, A contains the updated matrix.
C     LDA    - Leading dimension of A exactly as specified in the
C              calling routine.  (Input)
C
C  GAMS:       D1b
C
C  Chapter:    MATH/LIBRARY Basic Matrix/Vector Operations
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DGER (M, N, ALPHA, X, INCX, Y, INCY, A, LDA)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    M, N, INCX, INCY, LDA
      DOUBLE PRECISION ALPHA, X(*), Y(*)
      DOUBLE PRECISION A(*)
      INTEGER    I1X
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    IY, J
C                                  SPECIFICATIONS FOR SPECIAL CASES
      EXTERNAL   DAXPY
C                                  Quick return if possible
      IF (M.EQ.0 .OR. N.EQ.0 .OR. ALPHA.EQ.0.0D0) GO TO 9000
C
      IY = 1
      IF (INCY .LT. 0) IY = (-N+1)*INCY + 1
C
      I1X = 1
      DO 10  J=1, N
         CALL DAXPY (M, ALPHA*Y(IY), X, INCX, A(I1X), 1)
         IY = IY + INCY
         I1X = I1X + LDA
   10 CONTINUE
C
 9000 RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  DQADD (Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    January 1, 1985
C
C  Purpose:    Add a double precision scalar to the accumulator in
C              extended precision.
C
C  Usage:      CALL DQADD (A, ACC)
C
C  Arguments:
C     A      - Double precision number to be added to the accumulator.
C              (Input)
C     ACC    - Accumulator.  (Input/Output)
C              ACC is a double precision vector of length 2. On output,
C              ACC contains the sum of C input ACC and A.
C
C  Remark:
C     DQADD adds the double precision number A to the extended
C     precision accumulator, ACC. The subroutine assumes that an
C     extended precision number is already in the accumulator.
C
C  GAMS:       D1a10
C
C  Chapter:    MATH/LIBRARY Basic Matrix/Vector Operations
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DQADD (A, ACC)
C                                  SPECIFICATIONS FOR ARGUMENTS
      DOUBLE PRECISION A, ACC(2)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      DOUBLE PRECISION Q, Z, ZZ
C                                  FIRST EXECUTABLE STATEMENT
C
C                                  USE ALGORITHM ATTRIBUTED TO KAHAN
      Z = A + ACC(1)
      Q = A - Z
      ZZ = ((Q+ACC(1))+(A-(Q+Z))) + ACC(2)
      ACC(1) = Z + ZZ
      ACC(2) = (Z-ACC(1)) + ZZ
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  DQMUL (Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    January 1, 1985
C
C  Purpose:    Multiply double precision scalars in extended precision.
C
C  Usage:      CALL DQMUL (DA, DB, DACC)
C
C  Arguments:
C     DA      - Double precision multiplier.  (Input)
C     DB      - Double precision multiplicand.  (Input)
C     DACC    - Accumulator.  (Input/Output)
C               DACC is a double precision vector of length 2.
C               On output, DACC contains the sum of input DACC and
C               DA*DB.
C
C  Remark:
C     DQMUL adds the product DA*DB to the extended precision
C     accumulator, DACC. The subroutine assumes that an extended
C     precision number is already in the accumulator.
C
C  Keyword:    Level 1 BLAS
C
C  GAMS:       A3c
C
C  Chapter:    MATH/LIBRARY Basic Matrix/Vector Operations
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DQMUL (DA, DB, DACC)
C                                  SPECIFICATIONS FOR ARGUMENTS
      DOUBLE PRECISION DA, DB, DACC(2)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
C
C
C                                  USE ALGORITHM ATTRIBUTED TO KAHAN
      INTEGER    I, J
      DOUBLE PRECISION X(2), H(2), T(2), P, Q, R
      DOUBLE PRECISION S, TOP, BOT
      EQUIVALENCE (P, X(1)), (Q, X(2)), (R, T(2))
C                                  MACHINE DEPENDENT CONSTANTS
C                                  S = 2**((M/2)+H) WHERE M IS THE
C                                      NUMBER OF DIGITS IN THE
C                                      MANTISSA OF A DOUBLE
C                                      PRECISION WORD AND H IS
C                                      THE NO. OF EXTRA BITS CARRIED
C                                      DURING INTERMEDIATE ARITHMETIC.
C                                  TOP = LARGEST D.P. NUMBER/S
C                                  BOT = SMALLEST D.P. NUMBER*S**2
      DATA S/134217728.D0/
      DATA TOP/1.3390779D300/
      DATA BOT/4.0092665D-292/
C                                  FIRST EXECUTABLE STATEMENT
      J = 0
      X(1) = DA
      X(2) = DB
      DO 15  I=1, 2
         IF (DABS(X(I)) .LE. TOP) GO TO 5
C                                  SCALE DOWN BIG X
         J = J + 1
         X(I) = X(I)/S**2
    5    IF (DABS(X(I)) .GT. BOT) GO TO 10
C                                  SCALE UP SMALL X
         J = J - 1
         X(I) = X(I)*S**2
C                                  ROUND OFF HALF OF BITS IN X TO GET H
   10    R = S*X(I) + X(I)
         H(I) = (X(I)-R) + R
         T(I) = X(I) - H(I)
   15 CONTINUE
C                                  PREPARE X(1)*X(2)=P+Q EXACTLY
      P = X(1)*X(2)
      Q = H(1)*H(2)
      IF (Q .EQ. 0.0D0) GO TO 20
C                                  IN CASE Q UNDERFLOWS TO ZERO
      Q = ((H(1)*T(2)+H(2)*T(1))+(Q-P)) + T(1)*T(2)
C                                  AVOID UNNECESSARY RESCALING
   20 IF (J.EQ.0 .OR. (P.EQ.0.0D0.AND.Q.EQ.0.0D0)) GO TO 25
C                                  ELSE UNDO SCALING, PERHAPS
C                                  OVER OR UNDERFLOWING
      R = S**(J+J)
      P = P*R
      Q = Q*R
   25 CALL DQADD (P, DACC)
      CALL DQADD (Q, DACC)
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  DQSTO (Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    January 1, 1985
C
C  Purpose:    Store a double precision approximation to an
C              extended-precision scalar.
C
C  Usage:      CALL DQSTO (DACC, DA)
C
C  Arguments:
C     DACC    - Accumulator.  (Input)
C               DACC is a double precision vector of length 2. DACC is
C               assumed to be the result computed by calling IMSL
C               extended precision routines.
C     DA      - Double precision scalar.  (Output)
C               On output, DA contains a double precision approximation
C               to the value of the extended precision accumulator.
C
C  Keyword:    Level 1 BLAS
C
C  GAMS:       A3c
C
C  Chapter:    MATH/LIBRARY Basic Matrix/Vector Operations
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DQSTO (DACC, DA)
C                                  SPECIFICATIONS FOR ARGUMENTS
      DOUBLE PRECISION DA, DACC(2)
C
      DA = DACC(1) + DACC(2)
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  DSWAP (Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    August 9, 1986
C
C  Purpose:    Interchange vectors X and Y, both double precision.
C
C  Usage:      CALL DSWAP (N, DX, INCX, DY, INCY)
C
C  Arguments:
C     N      - Length of vectors X and Y.  (Input)
C     DX     - Double precision vector of length MAX(N*IABS(INCX),1).
C              (Input/Output)
C     INCX   - Displacement between elements of DX.  (Input)
C              X(I) is defined to be
C                 DX(1+(I-1)*INCX) if INCX.GE.0  or
C                 DX(1+(I-N)*INCX) if INCX.LT.0.
C     DY     - Double precision vector of length MAX(N*IABS(INCY),1).
C              (Input/Output)
C     INCY   - Displacement between elements of DY.  (Input)
C              Y(I) is defined to be
C                 DY(1+(I-1)*INCY) if INCY.GE.0  or
C                 DY(1+(I-N)*INCY) if INCY.LT.0.
C
C  Keywords:   Level 1 BLAS; DSWAP; Swap; Exchange
C
C  GAMS:       D1a5
C
C  Chapters:   MATH/LIBRARY Basic Matrix/Vector Operations
C              STAT/LIBRARY Mathematical Support
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DSWAP (N, DX, INCX, DY, INCY)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, INCX, INCY
      DOUBLE PRECISION DX(*), DY(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, IX, IY, M, MP1
      DOUBLE PRECISION DTEMP
C                                  SPECIFICATIONS FOR SPECIAL CASES
C     INTRINSIC  MOD
      INTRINSIC  MOD
      INTEGER    MOD
C
      IF (N .GT. 0) THEN
         IF (INCX.NE.1 .OR. INCY.NE.1) THEN
C                                  CODE FOR UNEQUAL INCREMENTS OR EQUAL
C                                    INCREMENTS NOT EQUAL TO 1
            IX = 1
            IY = 1
            IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
            IF (INCY .LT. 0) IY = (-N+1)*INCY + 1
            DO 10  I=1, N
               DTEMP = DX(IX)
               DX(IX) = DY(IY)
               DY(IY) = DTEMP
               IX = IX + INCX
               IY = IY + INCY
   10       CONTINUE
         ELSE
C                                  CODE FOR BOTH INCREMENTS EQUAL TO 1
            M = MOD(N,3)
C                                  CLEAN-UP LOOP
            DO 30  I=1, M
               DTEMP = DX(I)
               DX(I) = DY(I)
               DY(I) = DTEMP
   30       CONTINUE
            MP1 = M + 1
            DO 40  I=MP1, N, 3
               DTEMP = DX(I)
               DX(I) = DY(I)
               DY(I) = DTEMP
               DTEMP = DX(I+1)
               DX(I+1) = DY(I+1)
               DY(I+1) = DTEMP
               DTEMP = DX(I+2)
               DX(I+2) = DY(I+2)
               DY(I+2) = DTEMP
   40       CONTINUE
         END IF
      END IF
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  DTRSV  (Double precision version)
C
C  Computer:   sgruxs/DOUBLE
C
C  Revised:    January 10, 1986
C
C  Purpose:    Solve a triangular system, x = T**(-1)*x, where T is a
C              a triangular matrix, all double precision.
C
C  Usage:      CALL DTRSV (UPLO, TRANS, DIAG, N, A, LDA, X, INCX)
C
C  Arguments:
C     UPLO   - Character specifing the storage structure.
C              (Input)
C                 UPLO              Structure
C              'U' or 'u'      Matrix is upper triangular
C              'L' or 'l'      Matrix is lower triangular
C     TRANS  - Character specifing if the transpose solution is to be
C              computed.  (Input)
C                 TRANS              Meaning
C              'N' or 'n'      Compute x = A**(-1)*x
C              'T' or 't'      Compute x = A'**(-1)*x
C              'C' or 'c'      Compute x = A'**(-1)*x
C     DIAG   - Character specifing if the matrix has a unit diagonal.
C              (Input)
C              If DIAG is 'U' or 'u' then the elements in diagonal of A
C              are assumed to be one and are not referenced.  If DIAG is
C              'N' or 'n' then the actual diagonal elements of A are
C              used
C     N      - Order of the matrix A.  (Input)
C     A      - Double precision triangular matrix of order N.  (Input)
C     LDA    - Leading dimension of A exactly as specified in the
C              calling routine.  (Input)
C     X      - Double precision vector of length (N-1)*IABS(INCX)+1.
C              (Input/Output)
C     INCX   - Displacement between elements of X.  (Input)
C
C  GAMS:       D1b
C
C  Chapter:    MATH/LIBRARY Basic Matrix/Vector Operations
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DTRSV (UPLO, TRANS, DIAG, N, A, LDA, X, INCX)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, LDA, INCX
      DOUBLE PRECISION A(*), X(*)
      CHARACTER  UPLO*1, TRANS*1, DIAG*1
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, IX
      LOGICAL    NOUNIT, TRAN, UPPER
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   DDOT
      DOUBLE PRECISION DDOT
C                                  Quick return if possible.
C
      IF (N .EQ. 0) RETURN
      NOUNIT = DIAG.EQ.'N' .OR. DIAG.EQ.'n'
      UPPER = UPLO.EQ.'U' .OR. UPLO.EQ.'u'
      TRAN = TRANS.EQ.'T' .OR. TRANS.EQ.'t' .OR. TRANS.EQ.'C' .OR.
     &       TRANS.EQ.'c'
C
      IF (UPPER) THEN
         IF (TRAN) THEN
            IF (INCX .GT. 0) THEN
               IX = 1
               DO 10  I=1, N
                  X(IX) = X(IX) - DDOT(I-1,A(1+LDA*(I-1)),1,X,INCX)
                  IF (NOUNIT) X(IX) = X(IX)/A(I+LDA*(I-1))
                  IX = IX + INCX
   10          CONTINUE
            ELSE
               IX = (-N+1)*INCX + 1
               DO 20  I=1, N
                  X(IX) = X(IX) - DDOT(I-1,A(1+LDA*(I-1)),1,X(IX-INCX),
     &                    INCX)
                  IF (NOUNIT) X(IX) = X(IX)/A(I+LDA*(I-1))
                  IX = IX + INCX
   20          CONTINUE
            END IF
         ELSE
            IF (INCX .GT. 0) THEN
               IX = (N-1)*INCX + 1
               DO 30  I=N, 1, -1
                  IF (I .LT. N) X(IX) = X(IX) - DDOT(N-I,A(I+LDA*I),
     &                LDA,X(IX+INCX),INCX)
                  IF (NOUNIT) X(IX) = X(IX)/A(I+LDA*(I-1))
                  IX = IX - INCX
   30          CONTINUE
            ELSE
               IX = 1
               DO 40  I=N, 1, -1
                  IF (I .LT. N) X(IX) = X(IX) - DDOT(N-I,A(I+LDA*I),
     &                LDA,X,INCX)
                  IF (NOUNIT) X(IX) = X(IX)/A(I+LDA*(I-1))
                  IX = IX - INCX
   40          CONTINUE
            END IF
         END IF
      ELSE
         IF (TRAN) THEN
            IF (INCX .GT. 0) THEN
               IX = (N-1)*INCX + 1
               DO 50  I=N, 1, -1
                  IF (I .LT. N) X(IX) = X(IX) - DDOT(N-I,A(I+1+LDA*(I-
     &                1)),1,X(IX+INCX),INCX)
                  IF (NOUNIT) X(IX) = X(IX)/A(I+LDA*(I-1))
                  IX = IX - INCX
   50          CONTINUE
            ELSE
               IX = 1
               DO 60  I=N, 1, -1
                  IF (I .LT. N) X(IX) = X(IX) - DDOT(N-I,A(I+1+LDA*(I-
     &                1)),1,X,INCX)
                  IF (NOUNIT) X(IX) = X(IX)/A(I+LDA*(I-1))
                  IX = IX - INCX
   60          CONTINUE
            END IF
         ELSE
            IF (INCX .GT. 0) THEN
               IX = 1
               DO 70  I=1, N
                  X(IX) = X(IX) - DDOT(I-1,A(I),LDA,X,INCX)
                  IF (NOUNIT) X(IX) = X(IX)/A(I+LDA*(I-1))
                  IX = IX + INCX
   70          CONTINUE
            ELSE
               IX = (-N+1)*INCX + 1
               DO 80  I=1, N
                  X(IX) = X(IX) - DDOT(I-1,A(I),LDA,X(IX-INCX),INCX)
                  IF (NOUNIT) X(IX) = X(IX)/A(I+LDA*(I-1))
                  IX = IX + INCX
   80          CONTINUE
            END IF
         END IF
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  E1USR
C
C  Computer:   sgruxs/SINGLE
C
C  Revised:    November 2, 1984
C
C  Purpose:    Set USER CODE switch.
C
C  Usage:      CALL E1USR(SWITCH)
C
C  Arguments:
C     SWITCH - Character string.  (Input)
C                'ON'  Indicates that USER CODE mode is being entered.
C                'OFF' Indicates that USER CODE mode is being exited.
C  Remarks:
C     When E1POP is called from a routine while in USER CODE mode,
C     then an error message of type 1-4 will be printed (if an error
C     condition is in effect and the print table allows it).
C     However, an error message of type 1-4 will never be printed
C     if USER CODE mode is not in effect.
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE E1USR (SWITCH)
C                                  SPECIFICATIONS FOR ARGUMENTS
      CHARACTER  SWITCH*(*)
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      INTEGER    IFINIT
      SAVE       IFINIT
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM1/
      INTEGER    CALLVL, MAXLEV, MSGLEN, ERTYPE(51), ERCODE(51),
     &           PRINTB(7), STOPTB(7), PLEN, IFERR6, IFERR7,
     &           IALLOC(51), HDRFMT(7), TRACON(7)
      COMMON     /ERCOM1/ CALLVL, MAXLEV, MSGLEN, ERTYPE, ERCODE,
     &           PRINTB, STOPTB, PLEN, IFERR6, IFERR7, IALLOC, HDRFMT,
     &           TRACON
      SAVE       /ERCOM1/
C                              SPECIFICATIONS FOR COMMON /ERCOM2/
      CHARACTER  MSGSAV(255), PLIST(300), RNAME(51)*6
      COMMON     /ERCOM2/ MSGSAV, PLIST, RNAME
      SAVE       /ERCOM2/
C                              SPECIFICATIONS FOR COMMON /ERCOM3/
      DOUBLE PRECISION ERCKSM
      COMMON     /ERCOM3/ ERCKSM
      SAVE       /ERCOM3/
C                              SPECIFICATIONS FOR COMMON /ERCOM4/
      LOGICAL    ISUSER(51)
      COMMON     /ERCOM4/ ISUSER
      SAVE       /ERCOM4/
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1INIT, E1MES, E1STL
C
      DATA IFINIT/0/
C                                  INITIALIZE ERROR TABLE IF NECESSARY
      IF (IFINIT .EQ. 0) THEN
         CALL E1INIT
         IFINIT = 1
      END IF
      IF (SWITCH.EQ.'ON' .OR. SWITCH.EQ.'on') THEN
         ISUSER(CALLVL) = .TRUE.
      ELSE IF (SWITCH.EQ.'OFF' .OR. SWITCH.EQ.'off') THEN
         ISUSER(CALLVL) = .FALSE.
      ELSE
         CALL E1STL (1, SWITCH)
         CALL E1MES (5, 1, 'Invalid value for SWITCH in call to'//
     &               ' E1USR.  SWITCH must be set to ''ON'' or '//
     &               '''OFF''.  SWITCH = ''%(L1)'' ')
      END IF
C
      RETURN
      END
