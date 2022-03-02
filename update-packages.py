import os
import shutil
import json

a = os.listdir('examples')
pj = None
with open('package.json','r') as pjs:
  pj = json.loads(pjs.read())

for x in a:
  print(x)
  shutil.copy("packages.dhall", f'examples/{x}/packages.dhall')
  with open(f'examples/{x}/package.json','r') as _pjs:
    _pj = json.loads(_pjs.read())
    _pj["devDependencies"]["purescript"] = pj["devDependencies"]["purescript"]
    _pj["devDependencies"]["spago"] = pj["devDependencies"]["spago"]
    _pj["devDependencies"]["purs-tidy"] = pj["devDependencies"]["purs-tidy"]
    with open(f'examples/{x}/package.json','w') as __pjs:
      __pjs.write(json.dumps(_pj, indent=2))