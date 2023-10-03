# This is not FeynRules output coresponding to a realistic model.
# It's a handcrafted UFO model for testing exotic color representations.
# Everything ignored by O'Mega has been stripped.
# Don't expect Madgraph to be able to use it.
########################################################################

g = Particle(pdg_code = 21,
             name = 'g',
             antiname = 'g',
             spin = 3,
             color = 8,
             mass = Param.ZERO,
             width = Param.ZERO,
             texname = 'g',
             antitexname = 'g',
             charge = 0,
             GhostNumber = 0,
             LeptonNumber = 0,
             Y = 0)

f3 = Particle(pdg_code = 1,
             name = 'f3',
             antiname = 'f3~',
             spin = 2,
             color = 3,
             mass = Param.ZERO,
             width = Param.ZERO,
             texname = 'f_3',
             antitexname = '\bar f_3',
             charge = -1)

f3__tilde__ = f3.anti()

s = Particle(pdg_code = 2,
             name = 's',
             antiname = 's~',
             spin = 1,
             color = 1,
             mass = Param.ZERO,
             width = Param.ZERO,
             texname = 's',
             antitexname = '\bar s',
             charge = -1)

s__tilde__ = s.anti()

ss = Particle(pdg_code = 22,
             name = 'ss',
             antiname = 'ss~',
             spin = 1,
             color = 1,
             mass = Param.ZERO,
             width = Param.ZERO,
             texname = 'ss',
             antitexname = '\bar s\bar s',
              charge = -2)

ss__tilde__ = ss.anti()

s3 = Particle(pdg_code = 3,
             name = 's3',
             antiname = 's3~',
             spin = 1,
             color = 3,
             mass = Param.ZERO,
             width = Param.ZERO,
             texname = 's_3',
             antitexname = '\bar s_3',
             charge = -1)

s3__tilde__ = s3.anti()

s6 = Particle(pdg_code = 4,
             name = 's6',
             antiname = 's6~',
             spin = 1,
             color = 6,
             mass = Param.ZERO,
             width = Param.ZERO,
             texname = 's_6',
             antitexname = '\bar s_6',
             charge = -2)

s6__tilde__ = s6.anti()
