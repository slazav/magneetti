#!/usr/bin/python3

import magnet
import matplotlib.pyplot as plt
import os

# find magnet program
os.environ["PATH"] += os.pathsep + ".."

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

field = {'r1':0, 'dr':0.3, 'r2':22, 'z1':-100, 'dz':1, 'z2':100}
field1 = {'r1':0, 'dr':0.5, 'r2':5, 'z1':-90, 'dz':1, 'z2':-80}

#################################

# calculate the full range and plot the whole image:
(zz, rr, Bzz, Brr) = magnet.field_calc(shields, coils, wire, field)
#magnet.plot_all(zz,rr,Bzz,Brr, shields, coils, wire, field, 'all.png')

# calculate small range, fit it with linear functon, plot the fit
#(zz, rr, Bzz, Brr) = magnet.field_calc(shields, coils, wire, field1)
#(R0, H0, A0) = magnet.field_fit(zz, rr, Bzz, Brr, None, -85, 1);
#magnet.plot_fit(zz,rr,Bzz,Brr, R0, H0,A0, -85, 1, 'fit.png')


fig, axs = plt.subplots(nrows=4)
fig.set_tight_layout(True)
im1=magnet.plot_field(axs[0], zz, rr, Bzz)
fig.colorbar(im1, ax=axs[0], orientation='vertical', fraction=0.015, pad=0, aspect=30, shrink=0.82)
im2=magnet.plot_field(axs[1], zz, rr, Brr)
fig.colorbar(im2, ax=axs[1], orientation='vertical', fraction=0.015, pad=0, aspect=30, shrink=0.82)
axs[1].set_xlabel('z [mm]')

for a in axs[0:2]:
  a.set_ylabel('r [mm]')
  magnet.plot_magnets(a, shields, coils[-1:], wire)

axs[2].plot([zz[0], zz[-1]], [0,0], 'k-', linewidth=0.3)
axs[2].plot([zz[0], zz[-1]], [100,100], 'k-', linewidth=0.3)
axs[2].plot(zz, Bzz[0,:], 'r-')
axs[2].set_ylabel('Bz(r=0) [G]')
axs[2].set_xlabel('z [mm]')


#######################################
# calculate, fit and plot the central region

(zz, rr, Bzz, Brr) = magnet.field_calc(shields, coils, wire, field1)
(R0, H0, A0) = magnet.field_fit(zz, rr, Bzz, Brr, None, -85, 1);

magnet.plot_fit(axs[3], zz, rr, Bzz, R0, H0, A0, -85, 1)
axs[3].set_xlabel('z [mm]')

fig.set_size_inches(10,8)
plt.savefig('test.png', format='png', dpi='figure')
plt.close(fig)

