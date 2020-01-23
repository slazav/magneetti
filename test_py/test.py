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
#  layers: number of layers, default 1
#  turns:  turns per layer, default 1
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
         'z1':-100, 'dz':5, 'z2':100}

###################################################

def magnet_data_test(shields, coils, wire, field):
  for i in shields:
    if not 'len'  in i: i['len']  = 0
    if not 'hole' in i: i['hole'] = 0
    if not 'cnt'  in i: i['cnt']  = 0
    if not 'flux' in i: i['flux']  = 0
    if not 'sym'  in i: i['sym']  = 0
    if i['sym']!=0: i['sym'] = 1
    if not 'rad' in i: raise NameError('shield radius is not set')
    if i['rad'] <= 0:  raise NameError('non-positive shield radius')

  for i in coils:
    if not 'len'  in i: raise NameError('coil length is not set')
    if i['len'] <= 0:   raise NameError('non-positive coil length')
    if not 'rad'  in i: raise NameError('coil radius is not set')
    if i['rad'] <= 0:   raise NameError('non-positive coil radius')
    if not 'cnt'    in i: i['cnt']  = 0
    if not 'layers' in i: i['layers'] = 1
    if not 'turns'  in i: i['turns']  = 1
    if not 'curr'   in i: i['curr']   = 0
    if not 'sym'    in i: i['sym']  = 0
    if i['sym']!=0: i['sym'] = 1

  if not 'th'   in wire: raise NameError('wire thickness is not set')
  if not 'foil' in wire: wire['foil'] = -0.134

  if not 'dr'   in field: raise NameError('field dr is not set')
  if not 'r1'   in field: raise NameError('field r1 is not set')
  if not 'r2'   in field: raise NameError('field r2 is not set')
  if not 'dz'   in field: raise NameError('field dz is not set')
  if not 'z1'   in field: raise NameError('field z1 is not set')
  if not 'z2'   in field: raise NameError('field z2 is not set')

###################################################
def magnet_calc(shields, coils, wire, field):
  inp = ''
  for i in shields:
    inp += 'shield %f %f %f %f %f %d\n' %\
           (i['len'], i['rad'], i['hole'], i['cnt'], i['flux'], i['sym'])
  for i in coils:
    inp += 'coil %f %f %f %d %d %f %d\n' %\
           (i['len'], i['rad'], i['cnt'], i['layers'], i['turns'], i['curr'], i['sym'])

  inp += 'wire %f %f\n' % (wire['th'], wire['foil'])
  inp += 'field %f %f %f %f %f %f\n' %\
          (field['r1'], field['dr'], field['r2'],\
           field['z1'], field['dz'], field['z2'])

  print(inp)
  proc = subprocess.Popen('../magnet_new', stdin=subprocess.PIPE, stdout=subprocess.PIPE)
  try:
    outs, errs = proc.communicate(timeout=15, input=inp.encode())
  except subprocess.TimeoutExpired:
    proc.kill()
    outs, errs = proc.communicate()

  rr = numpy.arange(field['r1'], field['r2']+field['dz']/2, field['dr']);
  zz = numpy.arange(field['z1'], field['z2']+field['dz']/2, field['dz']);
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
magnet_data_test(shields, coils, wire, field)

(zz, rr, Bzz, Brr) = magnet_calc(shields, coils, wire, field)

import matplotlib.cm as cm

fig, (ax1,ax2) = plt.subplots(nrows=2)
Bzz=numpy.flip(Bzz,axis=0)
Brr=numpy.flip(Brr,axis=0)
im = ax1.imshow(Bzz, interpolation='bilinear', cmap=cm.seismic,
               origin='lower', extent=[field['z1'], field['z2'], field['r1'], field['r2']],
               vmax=abs(Bzz).max(), vmin=-abs(Bzz).max())
im = ax2.imshow(Brr, interpolation='bilinear', cmap=cm.seismic,
               origin='lower', extent=[field['z1'], field['z2'], field['r1'], field['r2']],
               vmax=abs(Bzz).max(), vmin=-abs(Bzz).max())
fig.colorbar(im, ax=ax2, orientation='horizontal', fraction=0.05, pad=0.2)


plt.show()

