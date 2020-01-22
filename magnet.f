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
     1                LOOPS(MAX), CUR(MAX), WIRDIA, FOILTH, BEXT,
     2                TRAFLX(MAX),BCNTR
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
      INTEGER ISHI, ISOL, I
      LOGICAL HOLE
      CHARACTER*1 ANSW
C           (* ISHI : index of the shield part                    *)
C           (* ISOL : index of the solenoid                       *)
C           (* HOLE : .TRUE. if there is a hole through the whole *)
C           (*        shield,  so  that  there  can  be a trapped *)
C           (*        flux ; .FALSE. otherwise                    *)
C                  ...............................
C
C     initialize variables
      DO 2800 I = 1, MAX
        LENSHI(I) = 0
        RADSHI(I) = 0
        RHOLE(I) = 0
        TRAFLX(I) = 0
        CENSHI(I) = 0
        LENSOL(I) = 0
        RADSOL(I) = 0
        CENSOL(I) = 0
        LOOPS(I) = 0
        CUR(I) = 0
        SYMSHI(I) = .FALSE.
        SYMSOL(I) = .FALSE.
 2800 CONTINUE


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
