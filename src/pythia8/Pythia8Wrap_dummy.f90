! Tell the caller that this is NOT the true Fastjet library
logical(c_bool) function pythia8_available () bind(C)
  use iso_c_binding
  pythia8_available = .false.
end function pythia8_available
