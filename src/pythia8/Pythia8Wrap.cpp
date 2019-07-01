#include "Pythia8/Pythia.h"
#include <iostream>

using namespace Pythia8;

extern "C" {

  // Tell the caller that this is the true Fastjet library
  bool pythia8_available() {
    return true;
  }

}
