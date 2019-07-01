int main()
{ void *w_instance;
  int sqrt = 360;
  c_whizard_init (&w_instance);
  c_whizard_model (&w_instance, "MSSM");
  c_whizard_process (&w_instance, "EEScatC", "e1, E1", "e1, E1");
  c_whizard_sqrts (&w_instance, &sqrt, "GeV");
  c_whizard_process_string (&w_instance, "n_events = 1");
  c_whizard_process_string (&w_instance, "sample_format = lhef");
  c_whizard_simulate (&w_instance, "EEScatC");
  c_whizard_finalize (&w_instance); 
  return 0;
}
