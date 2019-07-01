//////////////////////////////////////////////////////////////////////////
// HepMC reader for test case
//////////////////////////////////////////////////////////////////////////

#include <iostream>
#include "HepMC/GenEvent.h"
#include "HepMC/IO_GenEvent.h"

using namespace HepMC;

void read_event_file()
{
    std::cout << "Reading HepMC event file:" << std::endl;
     {
     // Open input stream
     std::ifstream istr( "hepmc_6_p.hepmc" );
     if( !istr ) {
       std::cerr << "Cannot open HepMC event file" << std::endl;
       exit(-1);
     }
     HepMC::IO_GenEvent ascii_in(istr);
     
     // Now read the file
     int icount=0;
     // Empty does not do anything yet.
     }
}

int main() {
  read_event_file();
  return 0;
}
