#!/usr/bin/python3

import subprocess
import numpy
import matplotlib.pyplot as plt

### Shields
#  len:   length [mm], 0 for a disc, default: 0
#  rad:   radius [mm], should be set to some positive value
#  hole:  hole radius [mm] (for discs only), default 0
#  cnt:   center [mm], default 0
#  flux:  trapped flux [gauss/cm2], default 0
#  sym:   add symmetric shield (z->-z), default 0

shields = ( {'len':180, 'rad':20.01},\
            {'rad':20.01, 'cnt': -90},\
            {'rad':20.01, 'hole': 10.01, 'cnt':  90},\
            {'len':10, 'rad':5.01} )

### Coils
#  len:    coil length [mm], should be set to some positive value
#  rad:    inner radius [mm], should be set to some positive value
#  cnt:    center of solenoid [mm], default 0
#  layers: number of layers, default 0
#  turns:  turns per layer, default 0
#  curr:   current [A], default 0
#  sym:    add symmetric coil, default 0

coils = ( {'len':170, 'rad':16, 'layers':4, 'turns':585, 'curr':0.85},\
          {'len':6.5, 'rad':10, 'cnt':47.75, 'layers':4, 'turns':50, 'curr':-0.6, 'sym':1} )

### Wire
#  th:    diameter, mm.
#  foil:  foil thickness (putting wire onto the notch.
#         between adjacent turns in the previous leyer.
#         give "foil" = -0.134 times wire diameter).

wire = { 'th':0.12, 'foil': 0}

### Calculation region
##    r1 dr r2  z1  dz z2

field = {'r1':0, 'dr':0.5, 'r2':20,\
         'z1':-100, 'dz':1, 'z2':100}

###################################################
### Calculate magnetic field profile for he coil system
### using the `magnet` program
def magnet_calc(shields, coils, wire, field):
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
    (z, r, Bz, Br) = line.split()
    if (iz>0) and (float(z)<zz[iz-1]):
      iz=0; ir+=1;
    if (rr[ir]!=float(r)) or (zz[iz]!=float(z)):
      raise NameError('wrong r-z values: r: %e vs %s z: %e vs %s' % (rr[ir], r, zz[iz], z))
    Bzz[ir,iz] = float(Bz)
    Brr[ir,iz] = float(Br)
    iz+=1;
  return (zz, rr, Bzz, Brr)

###################################################

(zz, rr, Bzz, Brr) = magnet_calc(shields, coils, wire, field)



import matplotlib.cm as cm
fig, axs = plt.subplots(nrows=3, sharex=True)
Bzz=numpy.flip(Bzz,axis=0)
Brr=numpy.flip(Brr,axis=0)
im = axs[0].imshow(Bzz, interpolation='bilinear', cmap=cm.seismic,
            origin='lower', extent=[field['z1'], field['z2'], field['r1'], field['r2']],
            vmax=abs(Bzz).max(), vmin=-abs(Bzz).max())
im = axs[1].imshow(Brr, interpolation='bilinear', cmap=cm.seismic,
            origin='lower', extent=[field['z1'], field['z2'], field['r1'], field['r2']],
            vmax=abs(Bzz).max(), vmin=-abs(Bzz).max())
fig.colorbar(im, ax=axs[1], orientation='horizontal', fraction=0.05, pad=0.2)

wth = wire.get('th', 0)
fth = wire.get('foil', 0)
for a in axs[0:2]:
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
  axs[2].plot(zz, Bzz[-1,:], 'r-')


plt.show()

