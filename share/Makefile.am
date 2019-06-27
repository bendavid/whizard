## Makefile.am -- Makefile for WHIZARD data files
##
## Process this file with automake to produce Makefile.in

## Subdirectories to configure
SUBDIRS = doc susy models cuts test examples

## Data files needed for running WHIZARD
## to be installed with the 'models' prefix kept
modelsdir = $(pkgdatadir)/models
dist_models_DATA = \
  models/GravTest.mdl \
  models/Littlest_Eta.mdl \
  models/Littlest_Tpar.mdl \
  models/Littlest.mdl \
  models/MSSM_CKM.mdl \
  models/MSSM_Grav.mdl \
  models/MSSM.mdl \
  models/NMSSM.mdl \
  models/NMSSM_CKM.mdl \
  models/PSSSM.mdl \
  models/QCD.mdl \
  models/QED.mdl \
  models/Simplest.mdl \
  models/Simplest_univ.mdl \
  models/SM_ac_CKM.mdl \
  models/SM_ac.mdl \
  models/SM_CKM.mdl \
  models/SM_km.mdl \
  models/SM_top.mdl \
  models/SM.mdl \
  models/Template.mdl \
  models/UED.mdl \
  models/Xdim.mdl \
  models/Zprime.mdl \
  models/Threeshl.mdl \
  models/Threeshl_nohf.mdl \
  models/Test.mdl

## SLHA parameter input files
## to be installed with the 'susy' prefix kept
susydir = $(pkgdatadir)/susy
dist_susy_DATA = \
  susy/sps1a.slha \
  susy/sps1ap_decays.slha \
  susy/nmssm.slha 

## Files containing predefined cut sets
## to be installed with the 'cuts' prefix kept
cutsdir = $(pkgdatadir)/cuts
dist_cuts_DATA = \
  cuts/default_cuts.sin

## Files containing complete examples
examplesdir = $(pkgdatadir)/examples
dist_examples_DATA = \
  examples/Z-lineshape.sin \
  examples/W-endpoint.sin \
  examples/casc_dec.sin \
  examples/Zprime.sin \
  examples/LEP_higgs.sin \
  examples/LEP_cc10.sin
