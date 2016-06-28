#include <iostream>
#include <sstream>
#include <string>

extern "C"{
#include "magnet.h"
}

/* New command line interface for magneetti program.
 * It implements uses the same interface as magneettiSH,
 * But now shield calculations do not work.
 *
 *       slazav, 2016.
 */


using namespace std;
int
main(){
  magnet_data_t d;

  // initialize data
  magnet_data_init(&d);

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

    if (ns>=d.max) { cerr << "Shield limit reached: " << d.max << "\n"; break; }
    if (nc>=d.max) { cerr << "Coil limit reached: " << d.max << "\n"; break; }

    if (cmd == "shield"){
      sline >> d.lenshi[ns] >> d.radshi[ns] >> d.rhole[ns]
            >> d.censhi[ns] >> d.traflx[ns] >> d.symshi[ns];
      d.lenshi[ns]/=1000;
      d.radshi[ns]/=1000;
      d.censhi[ns]/=1000;
      d.rhole[ns]/=1000;
      d.traflx[ns]*=1e-8; // G*cm^2 -> Si
      d.difshi = 1;
      if (d.symshi[ns]){
        ns++;
        if (ns>=d.max) { cerr << "Shield limit reached: " << d.max << "\n"; break; }
        d.lenshi[ns]=d.lenshi[ns-1];
        d.radshi[ns]=d.radshi[ns-1];
        d.censhi[ns]=-d.censhi[ns-1];
        d.rhole[ns]=d.rhole[ns-1];
        d.traflx[ns]=d.traflx[ns-1];
        d.symshi[ns]=1;
      }
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
      if (d.symsol[nc]){
        nc++;
        if (nc>=d.max) { cerr << "Coil limit reached: " << d.max << "\n"; break; }
        d.lensol[nc] = d.lensol[nc-1];
        d.radsol[nc] = d.radsol[nc-1];
        d.censol[nc] = -d.censol[nc-1];
        d.layers[nc] = d.layers[nc-1];
        d.loops[nc]  = d.loops[nc-1];
        d.cur[nc] = d.cur[nc-1];
        d.symsol[nc] = 1;
      }
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
  for (double r = r1; r<=r2; r+=dr){
    for (double z = z1; z<=z2; z+=dz){
      double Bz, Br;
      field(z/1000, r/1000, &d, &Bz, &Br);
      // flux(z, r, &d, &magflu);
      cout << z << " " << r << "  " << Bz*1e4 << " " << Br*1e4 << "\n";
    }
    cout << "\n";
  }
  magnet_data_free(&d);
}
