// #include "HepMC/GenEvent.h"

class Whizard {

  // The class is a wrapper around the whizard instance,
  // which is an opaque pointer, to be resolved only by the Fortran
  // routines that we call.

  // As long as the Fortran code uses global data (very limited, but still),
  // we should not generate more than one whizard instance at a time.
  // TODO: enforce this by some global flag.
 private:
  void* w_instance;

  // The public methods call subroutines from the Whizard C interface
  //
 public:

  // Default constructor needs no arguments, initializes Whizard.
  Whizard();

  // Destructor finalizes Whizard.
  ~Whizard();

  // Selected methods that implement Sindarin commands
  void model (char *id);
  void process (char *id, char *in, char *out);

  // Generic method for executing a command string
  void process_string (char *str);

  //  HepMC::GenEvent* hepmc_test(char *id, int proc_id, int event_id);

};
