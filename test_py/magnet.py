#!/usr/bin/python3

import subprocess
import numpy
import math
import matplotlib.pyplot as plt
import matplotlib.cm as cm

###################################################
### Calculate magnetic field profile for he coil system
### using the `magnet` program
def field_calc(shields, coils, wire, field):
  inp = ''
  for i in shields:
    inp += 'shield %f %f %f %f %f %d\n' %\
           (i.get('len',0), i.get('rad',0), i.get('hole',0),\
            i.get('cnt',0), i.get('flux',0), i.get('sym',0))
  for i in coils:
    clen=i.get('len',0)
    crad=i.get('rad',0)
    ccnt=i.get('cnt',0)
    clay=i.get('layers',0)
    ctrn=i.get('turns',0)
    curr=i.get('curr',0)
    sym=i.get('sym',0)
    if (curr==0) or (clen==0) or (crad==0) or\
       (clay==0) or (ctrn==0): continue
    inp += 'coil %f %f %f %d %d %f %d\n' %\
           (clen,crad,ccnt,clay,ctrn,curr,sym)

  inp += 'wire %f %f\n' % (wire.get('th',0), wire.get('foil',0))
  r1=field.get('r1',0)
  dr=field.get('dr',0)
  r2=field.get('r2',0)
  z1=field.get('z1',0)
  dz=field.get('dz',0)
  z2=field.get('z2',0)
  inp += 'field %f %f %f %f %f %f\n' % (r1,dr,r2,z1,dz,z2)

  # print(inp) # `inp` contains magnet description for magnet_new/magnetSH programs
  proc = subprocess.Popen('../magnet_new', stdin=subprocess.PIPE, stdout=subprocess.PIPE)
  try:
    outs, errs = proc.communicate(timeout=15, input=inp.encode())
  except subprocess.TimeoutExpired:
    proc.kill()
    outs, errs = proc.communicate()

  rr = numpy.arange(r1, r2+dr/2, dr);
  zz = numpy.arange(z1, z2+dz/2, dz);
  Brr = numpy.zeros((rr.size, zz.size));
  Bzz = numpy.zeros((rr.size, zz.size));

  ir=0; iz=0
  for line in outs.decode().split('\n'):
    if (line == ''): continue
    a = line.split()
    try:
      z = float(a[0])
      r = float(a[1])
      Bz = float(a[2])
      Br = float(a[3])
    except ValueError:
      continue
    if (iz>0) and (z<zz[iz-1]):
      iz=0; ir+=1;
    if (abs(rr[ir]-r)>1e-10) or (abs(zz[iz]-z)>1e-10):
      raise NameError('wrong r-z values: r: %e vs %s z: %e vs %s' % (rr[ir], r, zz[iz], z))
    Bzz[ir,iz] = Bz
    Brr[ir,iz] = Br
    iz+=1;
  return (zz, rr, Bzz, Brr)

###################################################
# Calculate field quality (constant, gradient, quadratic,
# fit with fixed or variable central value).
# See opt_doc/opt.pdf

### constant field Bz
### If H0 is None then it is used as free parameter
def field_fit(zz, rr, Bzz, Brr, H0, z0, func):

  (V0,I1,I2)    = (0,0,0);
  (IFH,IF1,IF2) = (0,0,0);

  for ir in range(rr.size):
    for iz in range(zz.size):

      z = zz[iz]-z0
      r = rr[ir]
      Bz = Bzz[ir,iz]

      if   func==0 : f = 0
      elif func==1 : f = z
      elif func==2 : f = z**2-r**2/2.0
      elif func==4 : f = z**4-3*r**2*z**2+3.0/8*r**4
      else: raise NameError('wrong func setting')

      V0 += r
      I1 += r*Bz
      I2 += r*Bz**2

      IFH += r*f*Bz
      IF1 += r*f
      IF2 += r*f**2

  I1 /= V0
  I2 /= V0
  IFH /= V0
  IF1 /= V0
  IF2 /= V0

  # calculate best fits
  if func==0: A0 = 0
  else: A0 = (IFH-IF1*I1)/(IF2-IF1**2)

  if H0 is None: H0 = I1 - A0*IF1
  R0 = math.sqrt(I2 - 2*H0*I1 + H0**2 + A0*(A0*IF2 - 2*IFH + 2*H0*IF1))

  # return parameters
  return(R0, H0, A0)

###################################################
def plot_all(zz,rr,Bzz,Brr, shields, coils, wire, field, fname):
  fig, axs = plt.subplots(nrows=3, sharex=True)
  im = axs[0].imshow(Bzz, interpolation='bilinear', cmap=cm.seismic,
              origin='upper', extent=[field['z1'], field['z2'], field['r1'], field['r2']],
              vmax=abs(Bzz).max(), vmin=-abs(Bzz).max())
  im = axs[1].imshow(Brr, interpolation='bilinear', cmap=cm.seismic,
            origin='upper', extent=[field['z1'], field['z2'], field['r1'], field['r2']],
            vmax=abs(Bzz).max(), vmin=-abs(Bzz).max())
  fig.colorbar(im, ax=axs[1], orientation='horizontal', fraction=0.05, pad=0.2)
  fig.set_tight_layout(True)
  plt.xlabel('z [mm]')
  plt.xlim([zz[0],zz[-1]])
  axs[0].set_title('Bz [G]')
  axs[1].set_title('Br [G]')

  wth = wire.get('th', 0)
  fth = wire.get('foil', 0)

  for a in axs[0:2]:
    a.set_ylabel('r [mm]')
    for i in coils:
      clen=i.get('len',0)
      crad=i.get('rad',0)
      ccnt=i.get('cnt',0)
      clay=i.get('layers',0)
      ctrn=i.get('turns',0)
      curr=i.get('curr',0)
      sym=i.get('sym',0)
      if (curr==0) or (clen==0) or (crad==0) or\
         (clay==0) or (ctrn==0): continue
      r1 = crad
      r2 = crad+clay*(wth+fth)
      for i in range(sym+1):
        z1 = (1-2*i)*ccnt-clen/2
        z2 = (1-2*i)*ccnt+clen/2
        a.plot([z1,z1,z2,z2,z1], [r1,r2,r2,r1,r1], 'g-')

    for i in shields:
      slen=i.get('len',0)
      srad=i.get('rad',0)
      shol=i.get('hole',0)
      scnt=i.get('cnt',0)
      sym=i.get('sym',0)

      for i in range(sym+1):
        if slen==0: # disk
          z1 = (1-2*i)*scnt
          a.plot([z1,z1], [shol,srad], 'm-')
        else:
          z1 = (1-2*i)*scnt-slen/2
          z2 = (1-2*i)*scnt+slen/2
          a.plot([z1,z2], [srad,srad], 'm-')

    axs[2].plot([zz[0], zz[-1]], [0,0], 'k-')
    axs[2].plot(zz, Bzz[0,:], 'r-')
    axs[2].set_ylabel('Bz(r=0) [G]')

  fig.set_size_inches(12,8)
  plt.savefig(fname, format='png', dpi='figure')
  plt.close(fig)
  #plt.show()

###################################################
def plot_fit(zz,rr,Bzz,Brr, R0, H0, A0, z0, func, fname):
  fig, ax = plt.subplots(nrows=1, sharex=True)
  fig.set_tight_layout(True)
  plt.xlim([zz[0],zz[-1]])
  plt.ylabel('Bz [G]')
  plt.xlabel('z [mm]')

  ax.plot([zz[0], zz[-1]], [0,0], 'k-')

  Bzc = numpy.zeros((rr.size, zz.size)); # fit
  for ir in range(rr.size):
    ax.plot(zz, Bzz[ir,:], 'r.-', linewidth=0.3)


    for iz in range(zz.size):
      z = zz[iz]-z0
      r = rr[ir]

      if   func==0 : f = 0
      elif func==1 : f = z
      elif func==2 : f = z**2-r**2/2.0
      elif func==4 : f = z**4-3*r**2*z**2+3.0/8*r**4
      else: raise NameError('wrong func setting')

      Bzc[ir,iz] = H0 + A0*f

    ax.plot(zz, Bzc[ir,:], 'b-', linewidth=0.3)

  fig.set_size_inches(12,8)
  plt.savefig(fname, format='png', dpi='figure')
  plt.close(fig)
  #plt.show()

###################################################
