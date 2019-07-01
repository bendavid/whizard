//////////////////////////////////////////////////////////////////////////
// Interface for building LCIO events
//////////////////////////////////////////////////////////////////////////

#include "lcio.h"
#include "IO/LCWriter.h"
#include "IMPL/LCEventImpl.h"
#include "IMPL/LCRunHeaderImpl.h"
#include "IMPL/MCParticleImpl.h"
#include "IMPL/LCCollectionVec.h"
#include "IMPL/LCTOOLS.h"
#include "UTIL/LCTime.h"

using namespace std;
using namespace lcio;

// Tell the caller that this is the true LCIO library
extern "C" bool lcio_available() {
  return true;
}

//////////////////////////////////////////////////////////////////////////
// LCEventImpl functions

extern "C" LCEventImpl* new_lcio_event ( int proc_id, int event_id ) {
  LCEventImpl* evt = new LCEventImpl();
  evt->setRunNumber ( proc_id );
  evt->setEventNumber ( event_id );
  LCTime now;
  evt->setTimeStamp ( now.timeStamp() );
  return evt;
}

extern "C" void lcio_event_delete( LCEventImpl* evt) {
  delete evt;
}

extern "C" void lcio_set_weight( LCEventImpl* evt, double wgt ) {
  evt->setWeight ( wgt );
}

// dump the event to the screen

extern "C" void dump_lcio_event ( LCEventImpl* evt) {
  LCTOOLS::dumpEvent ( evt );
}

//////////////////////////////////////////////////////////////////////////
// MCParticle functions

extern "C" LCCollectionVec* new_lccollection() {
  return new LCCollectionVec;
}

extern "C" MCParticleImpl* new_lcio_particle 
(double mom[3], int pdg, double mass, int status) {
  MCParticleImpl* mcp = new MCParticleImpl() ;
  mcp->setPDG ( pdg );
  mcp->setMomentum ( mom );
  mcp->setMass ( mass );
  mcp->setGeneratorStatus ( status );
  mcp->setCreatedInSimulation (true);
  return mcp; 
}

extern "C" MCParticleImpl* lcio_set_color_flow
(MCParticleImpl* mcp, int cflow[2]) {
  mcp->setColorFlow ( cflow );
}

extern "C" const int* lcio_particle_get_flow
( MCParticleImpl* mcp) {  
  return mcp->getColorFlow();
}

//////////////////////////////////////////////////////////////////////////
// LCWriter functions

extern "C" LCWriter* open_lcio_writer_new 
( char* filename ) {
  LCWriter* lcWrt = LCFactory::getInstance()->createLCWriter();  
  lcWrt->open( filename, LCIO::WRITE_NEW );
}

extern "C" LCWriter* open_lcio_writer_append
( char* filename ) {
  LCWriter* lcWrt = LCFactory::getInstance()->createLCWriter();    
  lcWrt->open( filename, LCIO::WRITE_APPEND );
}

// write the event

extern "C" LCWriter* lcio_write_event
( LCWriter* lcWrt, LCEventImpl* evt) {
  lcWrt->writeEvent( evt );
}

// destructor

extern "C" LCWriter* lcio_writer_delete ( LCWriter* lcWrt ) {
  lcWrt->close();
  delete lcWrt;
}

//////////////////////////////////////////////////////////////////////////
// LCRunHeader functions

extern "C" LCRunHeaderImpl* new_lcio_run_header( int rn ) {
  LCRunHeaderImpl* runHdr = new LCRunHeaderImpl;
  runHdr->setRunNumber (rn);
  return runHdr;
}

extern "C" void write_run_header 
(LCWriter* lcWrt, const LCRunHeaderImpl* runHdr, int complevel) {
  lcWrt->writeRunHeader (runHdr);
  lcWrt->setCompressionLevel (complevel);
}

