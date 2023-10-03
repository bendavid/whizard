# This is not FeynRules output coresponding to a realistic model.
# It's a handcrafted UFO model for testing exotic color representations.
# Everything ignored by O'Mega has been stripped.
# Don't expect Madgraph to be able to use it.
########################################################################

ZERO = Parameter(name = 'ZERO',
                 nature = 'internal',
                 type = 'real',
                 value = '0.0',
                 texname = '0')

g = Parameter(name = 'g',
              nature = 'external',
              type = 'real',
              value = 1,
              texname = 'g',
              lhablock = 'FRBlock',
              lhacode = [ 1 ])
