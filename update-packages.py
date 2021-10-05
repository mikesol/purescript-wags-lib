import os
import shutil

a = os.listdir('examples')
for x in a:
  print(x)
  shutil.copy("packages.dhall", f'examples/{x}/packages.dhall')
