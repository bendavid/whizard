# This is not FeynRules output coresponding to a realistic model.
# It's a handcrafted UFO model for testing exotic color representations.
# Everything ignored by O'Mega has been stripped.
# Don't expect Madgraph to be able to use it.
########################################################################

V_1 = Vertex(name = 'V_1',
              particles = [ P.g, P.g, P.g ],
              color = [ 'f(1,2,3)' ],
              lorentz = [ L.VVV1 ],
              couplings = {(0,0):C.GC_1})

V_2 = Vertex(name = 'V_2',
              particles = [ P.g, P.g, P.g, P.g ],
              color = [ 'f(-1,1,2)*f(3,4,-1)', 'f(-1,1,3)*f(2,4,-1)', 'f(-1,1,4)*f(2,3,-1)' ],
              lorentz = [ L.VVVV1, L.VVVV3, L.VVVV4 ],
              couplings = {(1,1):C.GC_2,(0,0):C.GC_2,(2,2):C.GC_2})


V_3 = Vertex(name = 'V_3',
             particles = [ P.f3__tilde__, P.f3, P.g ],
             color = [ 'T(3,2,1)' ],
             lorentz = [ L.FFV1 ],
             couplings = {(0,0):C.GC_1})

V_4 = Vertex(name = 'V_4',
             particles = [ P.s3, P.s3, P.s6__tilde__ ],
             color = [ 'K6(3,2,1)' ],
             lorentz = [ L.SSS1 ],
             couplings = {(0,0):C.GC_1})

V_5 = Vertex(name = 'V_5',
             particles = [ P.s3__tilde__, P.s3__tilde__, P.s6 ],
             color = [ 'K6Bar(3,2,1)' ],
             lorentz = [ L.SSS1 ],
             couplings = {(0,0):C.GC_1})

V_6 = Vertex(name = 'V_6',
             particles = [ P.s, P.s, P.ss__tilde__ ],
             color = [ '1' ],
             lorentz = [ L.SSS1 ],
             couplings = {(0,0):C.GC_1})

V_7 = Vertex(name = 'V_7',
             particles = [ P.s__tilde__, P.s__tilde__, P.ss ],
             color = [ '1' ],
             lorentz = [ L.SSS1 ],
             couplings = {(0,0):C.GC_1})

V_8 = Vertex(name = 'V_8',
             particles = [ P.g, P.s3__tilde__, P.s3 ],
             color = [ 'T(1,3,2)' ],
             lorentz = [ L.VSS1 ],
             couplings = {(0,0):C.GC_1})

V_9 = Vertex(name = 'V_9',
             particles = [ P.g, P.s6__tilde__, P.s6 ],
             color = [ 'T6(1,3,2)' ],
             lorentz = [ L.VSS1 ],
             couplings = {(0,0):C.GC_1})

