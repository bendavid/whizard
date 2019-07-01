#include <iostream>
#include "cpp_whizard.h"
// #include "HepMC/GenEvent.h"

int main()
{
  // HepMC::GenEvent* cur_evt = new HepMC::GenEvent();

  Whizard* whizard = new Whizard ();

  char str_model[] = "MSSM";
  whizard->model (str_model);
  char str_proc[] = "EEScatCPP";
  char str_in[] = "e1, e1";
  char str_out[] = "e1, e1";
  whizard->process (str_proc, str_in, str_out);
  char str_sqrts[] = "sqrts = 360 GeV";
  whizard->process_string (str_sqrts);

  //  cur_evt = whizard.hepmc_test (str_proc,1,1);
  // cur_evt->print();

  // delete cur_evt;
  return 0;
}
