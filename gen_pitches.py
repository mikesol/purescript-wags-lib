pitches = dict(cflat = 59.0, c = 60.0, csharp = 61.0,
dflat = 61.0, d = 62.0, dsharp = 63.0,
eflat = 63.0, e = 64.0, esharp = 65.0,
fflat = 64.0, f = 65.0, fsharp = 66.0,
gflat = 66.0, g = 67.0, gsharp = 68.0,
aflat = 68.0, a = 69.0, asharp = 70.0,
bflat = 70.0, b = 71.0, bsharp = 72.0)

for pitch, v in pitches.items():
  for x in range(0, 14):
    print('  , %s%d: %.1f' % (pitch, x, v + (x-4)*12))