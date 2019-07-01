#include <iostream>
#include "cpp_whizard.h"
// #include "HepMC/GenEvent.h"


extern "C" { 
  void c_whizard_init(void *w_instance);
  void c_whizard_model(void *w_instance, char *id);
  void c_whizard_process(void *w_instance, char *id, char *in, char *out);
  void c_whizard_process_string(void *w_instance, char *str);
  //  HepMC::GenEvent** c_whizard_hepmc_test(void *w_instance, char *id, int proc_id, int event_id);
  void c_whizard_finalize(void *w_instance);
}

Whizard::Whizard() {
  std::cout << "initializing\n";
  c_whizard_init( &w_instance );
}
Whizard::~Whizard() {
  c_whizard_finalize( &w_instance );
}
void Whizard::model( char *id ) {
  c_whizard_model( &w_instance, id );
}
void Whizard::process( char *id, char *in, char *out ) {
  c_whizard_process( &w_instance, id, in, out );
}
void Whizard::process_string( char *str ) {
  c_whizard_process_string( &w_instance, str );
}

// HepMC::GenEvent* c_whizard::hepmc_test(void *whizard_instance, char *id, int proc_id, int event_id){
//  return *c_whizard_hepmc_test(whizard_instance, id, proc_id, event_id);}
