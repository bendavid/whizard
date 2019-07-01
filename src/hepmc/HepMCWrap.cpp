//////////////////////////////////////////////////////////////////////////
// Interface for building HEPMC events
//////////////////////////////////////////////////////////////////////////

#include "HepMC/GenEvent.h"
#include "HepMC/IO_GenEvent.h"

using namespace HepMC;

// Tell the caller that this is the true HepMC library
extern "C" bool hepmc_available() {
  return true;
}

//////////////////////////////////////////////////////////////////////////
// GenEvent functions

extern "C" GenEvent* new_gen_event( int proc_id, int event_id ) {
  GenEvent* evt = new GenEvent( proc_id, event_id );
  evt->use_units(HepMC::Units::GEV, HepMC::Units::MM);
  return evt;
}

extern "C" void gen_event_delete( GenEvent* evt) {
  delete evt;
}

extern "C" void gen_event_print( GenEvent* evt ) {
  evt->print();
}

extern "C" int gen_event_event_number( GenEvent* evt ) {
  return evt->event_number();
}

extern "C" void gen_event_set_signal_process_id( GenEvent* evt, int id ) {
  evt->set_signal_process_id( id );
}

extern "C" int gen_event_signal_process_id( GenEvent* evt ) {
  return evt->signal_process_id();
}

extern "C" void gen_event_set_event_scale( GenEvent* evt, double scale ) {
  evt->set_event_scale( scale );
}

extern "C" double gen_event_event_scale( GenEvent* evt) {
  return evt->event_scale();
}

extern "C" void gen_event_set_alpha_qcd( GenEvent* evt, double a ) {
  evt->set_alphaQCD( a );
}

extern "C" double gen_event_alpha_qcd( GenEvent* evt) {
  return evt->alphaQCD();
}

extern "C" void gen_event_set_alpha_qed( GenEvent* evt, double a ) {
  evt->set_alphaQED( a );
}

extern "C" double gen_event_alpha_qed( GenEvent* evt) {
  return evt->alphaQED();
}

extern "C" void gen_event_clear_weights( GenEvent* evt ) {
  evt->weights().clear();
}

extern "C" void gen_event_add_weight( GenEvent* evt, double w ) {
  evt->weights().push_back( w );
}

extern "C" int gen_event_weights_size( GenEvent* evt ) {
  return evt->weights().size();
}

extern "C" double gen_event_weight( GenEvent* evt, int i ) {
  if (0 <= i && i <= evt->weights().size()) {
    return evt->weights()[i];
  } else {
    return 0;
  }
}

extern "C" void gen_event_add_vertex( GenEvent* evt, GenVertex* v ) {
  evt->add_vertex( v );
}

extern "C" void gen_event_set_signal_process_vertex
( GenEvent* evt, GenVertex* v ) {
  evt->set_signal_process_vertex( v );
}

extern "C" bool gen_event_set_beam_particles
( GenEvent* evt, GenParticle* prt1, GenParticle* prt2) {
  evt->set_beam_particles( prt1, prt2 );
}

extern "C" void gen_event_set_cross_section
( GenEvent* evt, double xs, double xs_err) {
  GenCrossSection xsec;
  xsec.set_cross_section (xs, xs_err);
  evt->set_cross_section( xsec );
}

//////////////////////////////////////////////////////////////////////////
// GenEvent particle iterator functions

extern "C" GenEvent::particle_const_iterator* 
new_event_particle_const_iterator( GenEvent* evt ) {
  GenEvent::particle_const_iterator* it = 
    new GenEvent::particle_const_iterator();
  (*it) = evt->particles_begin();
  return it;
}

extern "C" void event_particle_const_iterator_delete
( GenEvent::particle_const_iterator* it ) {
  delete it;
}

extern "C" void event_particle_const_iterator_advance
( GenEvent::particle_const_iterator* it ) {
  ++(*it);
}

extern "C" void event_particle_const_iterator_reset
( GenEvent::particle_const_iterator* it, GenEvent* evt ) {
  (*it) = evt->particles_begin();
}

extern "C" bool event_particle_const_iterator_is_valid
( GenEvent::particle_const_iterator* it, GenEvent* evt ) {
  return ((*it) != evt->particles_end());
}

extern "C" GenParticle* event_particle_const_iterator_get
( GenEvent::particle_const_iterator* it ) {
  return *(*it);
}

//////////////////////////////////////////////////////////////////////////
// GenVertex functions

extern "C" GenVertex* new_gen_vertex() {
  return new GenVertex();
}

extern "C" GenVertex* new_gen_vertex_pos( FourVector* pos ) {
  return new GenVertex( *pos );
}

extern "C" void gen_vertex_delete( GenVertex* v ) {
  delete v;
}

extern "C" void gen_vertex_add_particle_in( GenVertex* v, GenParticle* p ) {
  v->add_particle_in( p );
}

extern "C" void gen_vertex_add_particle_out( GenVertex* v, GenParticle* p ) {
  v->add_particle_out( p );
}

extern "C" bool gen_vertex_is_valid( GenVertex* v ) {
  return v != 0;
}

extern "C" int gen_vertex_particles_in_size( GenVertex* v ) {
  return v->particles_in_size();
}

extern "C" int gen_vertex_particles_out_size( GenVertex* v ) {
  return v->particles_out_size();
}

//////////////////////////////////////////////////////////////////////////
// GenVertex iterator over in-particles

extern "C" GenVertex::particles_in_const_iterator* 
new_vertex_particles_in_const_iterator( GenVertex* v ) {
  GenVertex::particles_in_const_iterator* it = 
    new GenVertex::particles_in_const_iterator();
  (*it) = v->particles_in_const_begin();
  return it;
}

extern "C" void vertex_particles_in_const_iterator_delete
( GenVertex::particles_in_const_iterator* it ) {
  delete it;
}

extern "C" void vertex_particles_in_const_iterator_advance
( GenVertex::particles_in_const_iterator* it ) {
  ++(*it);
}

extern "C" void vertex_particles_in_const_iterator_reset
( GenVertex::particles_in_const_iterator* it, GenVertex* v ) {
  (*it) = v->particles_in_const_begin();
}

extern "C" bool vertex_particles_in_const_iterator_is_valid
( GenVertex::particles_in_const_iterator* it, GenVertex* v ) {
  return ((*it) != v->particles_in_const_end());
}

extern "C" GenParticle* vertex_particles_in_const_iterator_get
( GenVertex::particles_in_const_iterator* it ) {
  return *(*it);
}

//////////////////////////////////////////////////////////////////////////
// GenVertex iterator over out-particles

extern "C" GenVertex::particles_out_const_iterator* 
new_vertex_particles_out_const_iterator( GenVertex* v ) {
  GenVertex::particles_out_const_iterator* it = 
    new GenVertex::particles_out_const_iterator();
  (*it) = v->particles_out_const_begin();
  return it;
}

extern "C" void vertex_particles_out_const_iterator_delete
( GenVertex::particles_out_const_iterator* it ) {
  delete it;
}

extern "C" void vertex_particles_out_const_iterator_advance
( GenVertex::particles_out_const_iterator* it ) {
  ++(*it);
}

extern "C" void vertex_particles_out_const_iterator_reset
( GenVertex::particles_out_const_iterator* it, GenVertex* v ) {
  (*it) = v->particles_out_const_begin();
}

extern "C" bool vertex_particles_out_const_iterator_is_valid
( GenVertex::particles_out_const_iterator* it, GenVertex* v ) {
  return ((*it) != v->particles_out_const_end());
}

extern "C" GenParticle* vertex_particles_out_const_iterator_get
( GenVertex::particles_out_const_iterator* it ) {
  return *(*it);
}

//////////////////////////////////////////////////////////////////////////
// GenParticle functions

extern "C" GenParticle* new_gen_particle
(FourVector* momentum, int pdg_id, int status) {
  return new GenParticle( *momentum, pdg_id, status );
}

extern "C" void gen_particle_delete( GenParticle* prt ) {
  delete prt;
}

extern "C" void gen_particle_set_flow
( GenParticle* prt, int code_index, int code ) {
  prt->set_flow( code_index, code );
}

extern "C" void gen_particle_set_polarization
( GenParticle* prt, Polarization* pol) {
  prt->set_polarization( *pol );
}

extern "C" int gen_particle_barcode( GenParticle* prt ) {
  return prt->barcode();
}

extern "C" FourVector* gen_particle_momentum( GenParticle* prt ) {
  return new FourVector( prt->momentum() );
}

extern "C" double gen_particle_generated_mass( GenParticle* prt ) {
  return prt->generated_mass();
}

extern "C" int gen_particle_pdg_id( GenParticle* prt ) {
  return prt->pdg_id();
}

extern "C" int gen_particle_status( GenParticle* prt ) {
  return prt->status();
}

extern "C" GenVertex* gen_particle_production_vertex( GenParticle* prt ) {
  return prt->production_vertex();
}

extern "C" GenVertex* gen_particle_end_vertex( GenParticle* prt ) {
  return prt->end_vertex();
}

extern "C" Polarization* gen_particle_polarization( GenParticle* prt ) {
  return new Polarization(prt->polarization());
}

extern "C" int gen_particle_flow( GenParticle* prt, int code_index ) {
  return prt->flow( code_index );
}

//////////////////////////////////////////////////////////////////////////
// FourVector functions

extern "C" FourVector* new_four_vector_xyzt
( double x, double y, double z, double t) {
  return new FourVector( x, y, z, t);
}

extern "C" FourVector* new_four_vector_xyz( double x, double y, double z) {
  return new FourVector( x, y, z);
}

extern "C" void four_vector_delete( FourVector* p ) {
  delete p;
}

extern "C" double four_vector_px( FourVector* p ) {
  return p->px();
}

extern "C" double four_vector_py( FourVector* p ) {
  return p->py();
}

extern "C" double four_vector_pz( FourVector* p ) {
  return p->pz();
}

extern "C" double four_vector_e( FourVector* p ) {
  return p->e();
}

//////////////////////////////////////////////////////////////////////////
// Polarization functions

extern "C" Polarization* new_polarization( double theta, double phi ) {
  return new Polarization( theta, phi );
}

extern "C" void polarization_delete( Polarization* pol ) {
  delete pol;
}


extern "C" double polarization_theta( Polarization* pol ) {
  return pol->theta();
}

extern "C" double polarization_phi( Polarization* pol ) {
  return pol->phi();
}

//////////////////////////////////////////////////////////////////////////
// IO_GenEvent functions

extern "C" IO_GenEvent* new_io_gen_event_in( char* filename ) {
  return new IO_GenEvent( filename, std::ios::in );
}

extern "C" IO_GenEvent* new_io_gen_event_out( char* filename ) {
  return new IO_GenEvent( filename, std::ios::out );
}

extern "C" void io_gen_event_delete( IO_GenEvent* iostream ) {
  delete iostream;
}

extern "C" void io_gen_event_write_event
( IO_GenEvent* iostream, const GenEvent* evt) {
  iostream->write_event( evt);
}

extern "C" bool io_gen_event_read_event
( IO_GenEvent* iostream, GenEvent* evt) {
  return iostream->fill_next_event( evt);
}

