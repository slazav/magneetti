#!/usr/bin/perl
use strict;
use warnings;

# all parameters for the default calculation

# Shield is a full cilyndrical box
my $Ls = 140; # Shield length
my $Rs =  29; # Shield inner radius

# Main solenoid
my $I0 = 1;     # current in the main solenoid
my $L0 = 104;   # Solenoid length
my $R0 = 21;    # Solenoid inner radius
my $N0 = 10;    # Layers
my $w  = 0.084; # Wire thickness (insulated)

# Compensation coils
my $IC = 1;     # current in the compensation solenoid
my $EC = $L0/2; # compensation coil edge
my $LC = 13.6;  # compensation coil length
my $NC = 2;     # Layers
my $LC2 = 0;    # compensation coil second block
my $PosC = 0;   # position of the comp.coils:
                # 0 - below main solenoid
                # 1 - above main solenoid
                # 2 - above main solenoid and gradient coils

# Gradient coils
my $IG = 1;   # current in the gradient coil
my $EG = 52;    # gradient coil edge
my $LG = 34.2;  # gradient coil length
my $NG = 2;     # Layers
my $LG2 = 44.6; # gradient coil second layer

# Quadratic coils
my $IQ = 1;    # current in the quadratic coil
my $LQ = 35.2; # quadratic coil length
my $NQ = 2;    # Layers
my $LQ2 = 0;   # quadratic coil second layer

# 4-th power coils
my $IF = 0;  # current in the coil
my $LF1 = 14.8;  # gap length
my $LF2 = 6.8;   # coil length
#my $LF1 = 5.0;   # coil length
#my $LF2 = 21;    # coil length
my $NF = 2;    # Layers

# Calculation range
my $rm = 28;
my $dr = 1;
my $zm = 70;
my $dz = 2;

# the calculation itself
# -- run the calculation with magneettiSH + magnet programs
# -- read data and calculate best fits and differences
# Parameters:
#  base - basename for result files
#  func - function type 0 - const, 1-lin, 2-quadr, 4-four-power
sub calc{
  my $base = shift;
  my $func = shift;
  my $cmd;

  # Shields
  # len rad hrad center flux sym
  $cmd.= sprintf("shield %f %f 0  0 0 0\n", $Ls, $Rs);
  $cmd.= sprintf("shield  0 %f 0 %f 0 1\n", $Rs, $Ls/2);

  # Solenoids
  # len rad center layers turns current sym

  # Main solenoid
  if ($I0!=0){
    $cmd.= sprintf("coil %f %f 0 %i %i %d 0\n",
      $L0, $R0, $N0, int($L0/$w), $I0);
  }

  # Compensation
  if ($IC!=0){
    my $RC = $R0;
    $RC-=$NC*$w if ($PosC == 0);
    $RC+=$N0*$w if ($PosC == 1);
    $RC+=($N0+$NG)*$w   if ($PosC == 1 && $LG2==0);
    $RC+=($N0+2*$NG)*$w if ($PosC == 1 && $LG2>0);
    $cmd.= sprintf("coil %f %f %f %i %i %f 1\n",
      $LC, $RC, $EC-$LC/2, $NC, int($LC/$w), $IC);
    # 2nd block
    my $RC2 = $RC;
    $RC2-=$NC*$w if ($PosC == 0);
    $RC2+=$NC*$w if ($PosC >0 );
    if ($LC2>0){
      $cmd.= sprintf("coil %f %f %f %i %i %f 1\n",
        $LC2, $RC2, $EC-$LC2/2, $NC, int($LC2/$w), $IC);
    }
  }

  # Gradient
  if ($IG!=0){
    $cmd.= sprintf("coil %f %f %f %i %i  %f 0\n",
      $LG, $R0+$N0*$w, $EG-$LG/2, $NG, int($LG/$w), $IG);
    $cmd.= sprintf("coil %f %f %f %i %i -%f 0\n",
      $LG, $R0+$N0*$w, -$EG+$LG/2, $NG, int($LG/$w), $IG);
    if ($LG2>0){
      $cmd.= sprintf("coil %f %f %f %i %i  %f 0\n",
        $LG2, $R0+($N0+$NG)*$w, $EG-$LG2/2, $NG, int($LG2/$w), $IG);
      $cmd.= sprintf("coil %f %f %f %i %i -%f 0\n",
        $LG2, $R0+($N0+$NG)*$w, -$EG+$LG2/2, $NG, int($LG2/$w), $IG);
    }
  }

  # Quadratic
  if ($IQ!=0){
    $cmd.= sprintf("coil %f %f %f %i %i  %f 0\n",
      $LQ, $R0+$N0*$w, 0, $NQ, int($LQ/$w), $IQ);
    if ($LQ2>0){
      $cmd.= sprintf("coil %f %f %f %i %i %f 0\n",
        $LQ2, $R0+($N0+$NQ)*$w, 0, $NQ, int($LQ2/$w), $IQ);
    }
  }

  # 4-th power
  if ($IF!=0){
#    $cmd.= sprintf("coil %f %f %f %i %i  %f 0\n",
#      $LF1, $R0+($N0+$NQ)*$w, 0, $NF, int($LF1/$w), $IF);
    $cmd.= sprintf("coil %f %f %f %i %i  %f 1\n",
      $LF2, $R0+($N0+$NQ)*$w, $LF1/2+$LF2/2, $NF, int($LF2/$w), $IF);
  }

  # wire and calculation range
  $cmd.= sprintf("wire %f %f\n", 0.12, 0);
  $cmd.= sprintf("field 0 %f %f %f %f %f\n", $dr, $rm, -$zm, $dz, $zm);

  my $txtfile = $base . '.txt';
  my $cmdfile = $base . '.cmd';
  my $resfile = $base . '.fld';
  my $fitfile = $base . '.fit';

  # run the actual calculation program
  open C, "> $txtfile" or die "can't open $txtfile: $!\n";
  printf C $cmd;
  close C;
  system("../magneettiSH $txtfile");
  system("../magnet &>/dev/null < $cmdfile");

  ## read the calculated field profile, calculate integrals
  ## integral definitions see in ../opt_doc/
  open R, $resfile or die "can't open $resfile: $!\n";
  my ($V0,$I1,$I2) = (0,0,0);
  my ($IDH,$ID1,$ID2) = (0,0,0);
  my ($IQH,$IQ1,$IQ2) = (0,0,0);
  my ($IFH,$IF1,$IF2) = (0,0,0);

  my $z0 = 0;
  my $r0 = 0;
  foreach(<R>){
    $_=~s/#.*//;
    $_=~s/^\s+//;
    next if (/^\s*$/);
    my ($z,$r,$Bz,$Br) = split /\s+/;

    my $f=0;
    if    ($func==1) {$f = $z;}
    elsif ($func==2) {$f = $z**2-$r**2/2.0;}
    elsif ($func==4) {$f = $z**4-3*$r**2*$z**2+3.0/8*$r**4;}

    $V0  += $r;
    $I1 += $r*$Bz;
    $I2 += $r*$Bz**2;
    $IFH += $r*$f*$Bz;
    $IF1 += $r*$f;
    $IF2 += $r*$f**2;
    $z0 = abs($z) if $z0 < abs($z);
    $r0 = abs($r) if $r0 < abs($r);
  }
  $I1 /= $V0;
  $I2 /= $V0;
  $IFH /= $V0;
  $IF1 /= $V0;
  $IF2 /= $V0;

  # calculate best fits
  my $A0 = $func==0 ? 0:($IFH-$IF1*$I1)/($IF2-$IF1**2);
  my $H0 = $I1 - $A0*$IF1;


  # calculate dimensionless, current-independent quality parameters
  my $R0 = sqrt($I2 - 2*$H0*$I1 + $H0**2 + $A0*($A0*$IF2 - 2*$IFH + 2*$H0*$IF1));
  if ($func==0){ $R0/=abs($H0); }
  else { $R0/=abs($A0); }

  if    ($func==1) {$R0/=$z0;}
  elsif ($func==2) {$R0/=$z0**2;}
  elsif ($func==4) {$R0/=$z0**4;}

  # write fits into a file
  open F, "> $fitfile" or die "Can't open $fitfile: $!\n";
  for (my $z=-$z0; $z<$z0; $z+=$z0/20){
    my $f1 = 0;
    my $f2 = 0;
    if    ($func==1) {$f1 = $z;}
    elsif ($func==2) {$f1 = $z**2;}
    elsif ($func==4) {$f1 = $z**4;}
    if    ($func==1) {$f2 = $z;}
    elsif ($func==2) {$f2 = $z**2-$r0**2/2.0;}
    elsif ($func==4) {$f2 = $z**4-3*$r0**2*$z**2+3.0/8*$r0**4;}
    printf F "%f %f %f\n", $z, $H0+$f1*$A0, $H0+$f2*$A0;
  }
  close F;

  # return parameters
  return ($R0, $H0, $A0);
}

#############################################################################
# just calculate and print all the parameters
sub calc_fixed{
  # main solenoid
  $I0=1; $IC=0; $IG = 0; $IQ = 0; $IF=0;
  my ($RR00,$H00,$A00) = calc("sol0", 0);

  # compensation coils
  $I0=1; $IC=1; $IG = 0; $IQ = 0; $IF=0;
  my ($RR0,$H0,$A0) = calc("sol1", 0);

  # gradient coils
  $I0=0; $IC=0; $IG = 1; $IQ = 0; $IF=0;
  my ($RR1,$H1,$A1) = calc("sol2", 1);

  # quadratic coils
  $I0=0; $IC=0; $IG = 0; $IQ = 1; $IF=0;
  my ($RR2,$H2,$A2) = calc("sol3",2);

  # 4-th power coils
  $I0=0; $IC=0; $IG = 0; $IQ = 0; $IF = 1;
  my ($RR3,$H3,$A3) = calc("sol4",4);

  printf "\nField inhomogeneity and profile in %.1fx%.1f volume:\n", 2*$rm, 2*$zm;
  printf "Main: R = %e, H = %f G/A\n", $RR00, $H00;
  printf "Comp: R = %e, H = %f G/A\n", $RR0, $H0;
  printf "Grad: R = %e, H = %F z, G/A\n", $RR1, $A1*10;
  printf "Quad: R = %e, H = %f - %F (z^2-r^2/2), G/A\n", $RR2, $H2, -$A2*100;
  printf "Four: R = %e, H = %f - %F f_4, G/A\n", $RR3, $H3, -$A3*1e4;
}


#############################################################################
# optimization region
$rm = 5;
$dr = 0.2;
$zm = 5;
$dz = 0.2;


#############################################################################
# optimize compensation coils
$I0=1; $IC=1; $IG = 0; $IQ = 0;
my $RR0 = 1e6;
if (0){  # do optimization
  my ($NCm, $ECm, $LCm, $LC2m) = ($NC, $EC, $LC, $LC2);
  open OO, "> sol1.opt" or die "can't open sol1.opt: $!";
  #for ($NC = 2; $NC<=10; $NC+=2){
    #for ($EC = 52; $EC > 42; $EC-=1){
      for ($LC = 13; $LC < 16; $LC+=0.1){
        #for ($LC2 = 0; $LC2 < 50; $LC2+=0.2){
          my $RR = (calc("sol1",0))[0];
          printf "min1: %f %f %f %f -- %e\n", $NC, $LC, $LC2, $EC, $RR;
          printf OO "%f %f %f %f  %e\n", $NC, $LC, $LC2, $EC, $RR;
          if ($RR0 > $RR) {
            $RR0 = $RR;
            $NCm = $NC;
            $LCm = $LC;
            $LC2m = $LC2;
            $ECm = $EC;
          }
        }
        printf OO "\n";
      #}
    #}
  #}
  close OO;
  $LC = $LCm;
  $LC2 = $LC2m;
  $EC = $ECm;
  $NC = $NCm;
  printf "res1:  %f %f %f %f -- %e\n", $NC, $LC, $LC2, $EC, $RR0;
}


# gradient coils
$I0=0; $IC=0; $IG = 1; $IQ = 0;
my $RR1 = 1e6;
if (0){  # do optimization
  my ($EGm, $LGm, $LG2m) = ($EG, $LG, $LG2);
  open OO, "> sol2.opt" or die "can't open sol2.opt: $!";
  $EG=52;
  #for ($EG = 39; $EG > 10; $EG-=1){
    for ($LG = 34.2; $LG < 35; $LG+=1){
      for ($LG2 = 43; $LG2 < 46; $LG2+=0.2){
        my $RR = (calc("sol2",1))[0];
        printf "min2: %f %f %f %f -- %.12f\n", $NG, $LG, $LG2, $EG, $RR;
        printf OO "%f %f %f %f  %e\n", $NG, $LG, $LG2, $EG, $RR;
        if ($RR1 > $RR) {
          $RR1 = $RR;
          $LGm = $LG;
          $LG2m = $LG2;
          $EGm = $EG;
        }
      }
      printf OO "\n";
    }
  #}
  close OO;
  $LG = $LGm;
  $LG2 = $LG2m;
  $EG = $EGm;
  printf "res2: %f %f %f %f -- %.12f\n", $NG, $LG, $LG2, $EG, $RR1;
}


# quadratic coils
$I0=0; $IC=0; $IG = 0; $IQ = 1;
my $RR2 = 1e6;
if (0){  # do optimization
  my ($LQm, $LQ2m) = ($LQ, $LQ2);
  open OO, "> sol3.opt" or die "can't open sol3.opt: $!";
  for ($LQ = 34; $LQ < 38; $LQ+=0.2){
#    for ($LQ2 = 27; $LQ2 <=34; $LQ2+=0.2){
      my $RR = (calc("sol3",2))[0];
      printf "min3: %f %f -- %.12f\n", $LQ, $LQ2, $RR;
      printf OO "%f %f %.12f\n", $LQ, $LQ2, $RR;
      if ($RR2 > $RR) {
        $RR2 = $RR;
        $LQm = $LQ;
        $LQ2m = $LQ2;
      }
    }
    printf OO "\n";
#  }
  close OO;
  $LQ = $LQm;
  $LQ2 = $LQ2m;
  printf "res3: %f %f -- %.12f\n", $LQ, $LQ2, $RR2;
}

# 4-power coils
$I0=0; $IC=0; $IG = 0; $IQ = 0; $IF=1;
my $RR3 = 1e6;
if (0){  # do optimization
  my ($LF1m, $LF2m) = ($LF1, $LF2);
  open OO, "> sol4.opt" or die "can't open sol4.opt: $!";
  for ($LF1 = 14.8; $LF1 < 14.81; $LF1+=0.2){
    for ($LF2 = 5.5; $LF2 <7.3; $LF2+=0.1){
      my $RR = (calc("sol4",4))[0];
      printf "min4: %f %f -- %.12f\n", $LF1, $LF2, $RR;
      printf OO "%f %f %.12f\n", $LF1, $LF2, $RR;
      if ($RR3 > $RR) {
        $RR3 = $RR;
        $LF1m = $LF1;
        $LF2m = $LF2;
      }
    }
    printf OO "\n";
  }
  close OO;
  $LF1 = $LF1m;
  $LF2 = $LF2m;
  printf "res4:  %f %f -- %.12f\n", $LF1, $LF2, $RR3;
}

calc_fixed();

# optimize fild homogeneity using quadratic coil:
my $RR4=1e6;
if (0){  # do optimization
  my ($IQm) = ($IQ);
  open OO, "> sol4.opt" or die "can't open sol4.opt: $!";
  for ($IQ = 0; $IQ < 0.001; $IQ+=0.0001){
    my $RR = (calc("sol4"))[0];
    printf "min4: %f -- %e\n", $IQ, $RR;
    printf OO "%f %e\n", $IQ, $RR;
    if ($RR4 > $RR) {
      $RR4 = $RR;
      $IQm = $IQ;
    }
  }
  close OO;
  $IQ = $IQm;
  printf "res3: %f %f -- %.12f\n", $LQ, $LQ2, $RR2;
}
#$RR4 = (calc("sol4"))[0];


#############################################################################
#############################################################################
# half region
if (1){
$rm = 10;
$dr = 0.5;
$zm = 10;
$dz = 0.5;
calc_fixed();
}

