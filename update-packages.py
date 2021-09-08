import os
import shutil

a = os.listdir('examples')
for x in a:
  shutil.copy("packages.dhall", f'examples/{x}/packages.dhall')
