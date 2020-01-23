#!/usr/bin/python3

import magnet

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

#################################

# calculate the full range and plot the whole image:
(zz, rr, Bzz, Brr) = magnet.field_calc(shields, coils, wire, field)
magnet.plot_all(zz,rr,Bzz,Brr, shields, coils, wire, field, 'all.png')

# calculate small range, fit it with linear functon, plot the fit
field1 = {'r1':0, 'dr':0.5, 'r2':5, 'z1':-90, 'dz':1, 'z2':-80}
(zz, rr, Bzz, Brr) = magnet.field_calc(shields, coils, wire, field1)
(R0, H0, A0) = magnet.field_fit(zz, rr, Bzz, Brr, None, -85, 1);
magnet.plot_fit(zz,rr,Bzz,Brr, R0, H0,A0, -85, 1, 'fit.png')

