import pywhizard

pywhizard.model("MSSM")
pywhizard.process("EEScatPy", "e1, E1", "e1, E1")
pywhizard.sqrts(360, "GeV")
pywhizard.process_string("n_events = 1")
pywhizard.process_string("sample_format = lhef")
pywhizard.integrate("EEScatPy")
pywhizard.sqrts(520, "GeV")
pywhizard.integrate("EEScatPy")
pywhizard.finalize()
