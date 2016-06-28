#include "magnet.h"
#include <iostream>
#include <sstream>
#include <string>

/* New command line interface for magneetti program.
 *
 *       slazav, 2016.
 */

/*
### shields
# * len, mm (0 for a disk)
# * rad, mm (outer rad for disk)
# * hole radius (only for disk, len=0)
# * shield center, mm
# * trapped flux, gauss/cm2
# * add symmetric shield

shield 0 20 0 100 0 0

### coils
# * coil length, mm
# * inner radius, mm
# * center of solenoid, mm
# * number of leyers
# * turns per layer
# * current, A
# * add symmetric coil

coil 76  16     0 4 585 0.85 0
coil 6.5 16 41.25 6  50 0.85 1
coil 6.5 16 47.75 9  50 0.85 1

### wire
# * diameter, mm.
# * foil thickness (putting wire onto the notch.
#   between adjacent turns in the previous leyer.
#   give "foil" = -0.134 times wire diameter).

wire 0.12 -0.01608

### field calculation region [mm]
##    r1 dr r2  z1  dz z2

field  0  1 13 -105 1 105

*/



using namespace std;
int
main(){
  magnet_data_t d;

  int max = 30;
  int nn = 100;

  // initialize data
  magnet_data_init(max, nn, &d);

  int nc=0, ns=0; // coil and shield number
  double z1=0,z2=0,dz=0,r1=0,r2=0,dr=0;

  // read parameters
  while (cin){
    string line;
    getline(cin, line);
    // remove comments and empty lines
    line = line.substr(0,line.find('#'));
    if (line.find_first_not_of(' ') == string::npos) continue;

    // start parcing line, read a command
    istringstream sline(line);
    string cmd;
    sline >> cmd;

    if (ns>=max) { cerr << "Shield limit reached: " << max << "\n"; break; }
    if (nc>=max) { cerr << "Coil limit reached: " << max << "\n"; break; }

    if (cmd == "shield"){
      sline >> d.lenshi[ns] >> d.radshi[ns] >> d.rhole[ns]
            >> d.censhi[ns] >> d.traflx[ns] >> d.symshi[ns];
      d.lenshi[ns]/=1000;
      d.radshi[ns]/=1000;
      d.censhi[ns]/=1000;
      d.rhole[ns]/=1000;
      d.difshi = 1;
      ns++;
      continue;
    }
    if (cmd == "coil"){
      sline >> d.lensol[nc] >> d.radsol[nc] >> d.censol[nc]
            >> d.layers[nc] >> d.loops[nc] >> d.cur[nc]
            >> d.symsol[nc];
      d.lensol[nc]/=1000;
      d.radsol[nc]/=1000;
      d.censol[nc]/=1000;
      nc++;
      continue;
    }
    if (cmd == "wire"){
      sline >> d.wirdia >> d.foilth;
      d.wirdia/=1000;
      d.foilth/=1000;
      continue;
    }
    if (cmd == "field"){
      sline >> r1 >> dr >> r2 >> z1 >> dz >> z2;
      continue;
    }
    cerr << "Unknown command: " << cmd << "\n";
  }
  d.nshi = ns;

  if (dz<=0 || z1>=z2 || dr<=0 || r1>=r2){
    cerr << "Calculation range does not set\n";
    return 1;
  }

  if (d.nshi>0)  curren(&d);
  for (double z = z1; z<=z2; z+=dz){
    for (double r = r1; r<=r2; r+=dr){
      double Bz, Br;
      field(z/1000, r/1000, &d, &Bz, &Br);
      // flux(z, r, &d, &magflu);
      cout << z << " " << r << "  " << Bz*1e4 << " " << Br*1e4 << "\n";
    }
    cout << "\n";
  }
  magnet_data_free(&d);
}
