import string
RN = {':':"Colon", '<':'LessThan', '>':'GreaterThan','[':'OpenSquare',']':'CloseSquare','*':'Star',' ':'Space' }
def ltgtnFails(o,x,nm):
  o = o + ['else instance %s%sClean :: %s "%s" "" Z Z' % (nm[0].lower()+nm[1:],RN[x] if x in RN else x,nm,x)]
  o = o + ['else instance %s%sFailLtGt :: Fail "Parser found a < without a closing >" => %s "%s" "" Z (S n)' % (nm[0].lower()+nm[1:],RN[x] if x in RN else x,nm,x)]
  o = o + ['else instance %s%sFailSq :: Fail "Parser found a [ without a closing ]" => %s "%s" "" (S n) x' % (nm[0].lower()+nm[1:],RN[x] if x in RN else x,nm,x)]
  return o

def genInstances(o, strs, nm, nxt, sq, ltgt):
  for x in strs:
    o = ltgtnFails(o,x,nm)
    o = o + ['else instance %s%s :: (Cons head tail rest, %s head tail %s %s) => %s "%s" rest sq ltgt' % (nm[0].lower()+nm[1:],RN[x] if x in RN else x,nxt, sq, ltgt, nm,x)]
  return o

def genClosers(o, nm):
  for (a,b,c) in [('Gt','>','greater-than signs'),('Sq',']','square brackets')]:
    o = o+['else instance %sValid%s :: %s "%s" "" Z (S Z)' % (nm[0].lower()+nm[1:],a,nm,b)]
    o = o+['else instance %sTooMany%s :: Fail "Too many closing %s" => %s "%s" "" Z Z' % (nm[0].lower()+nm[1:],a,c,nm,b)]
    o = o+['else instance %sNotEnough%s :: Fail "Not enough closing %s" => %s "%s" "" Z (S ltgt)' % (nm[0].lower()+nm[1:],a,c,nm,b)]
    o = o+['else instance %sContinuing%s :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => %s "%s" rest Z (S ltgt)' % (nm[0].lower()+nm[1:],a,nm,b)]
    o = o+['else instance %sUnderfull%s :: Fail "Too many closing %s" => %s "%s" rest Z Z' % (nm[0].lower()+nm[1:],a,c,nm,b)]
  return o

def genSpurious(o, nm):
  o = o+['else instance %sSpurious :: Fail "Not a valid symbol" => %s invalid rest Z Z' % (nm[0].lower()+nm[1:],nm)]
  return o
M = []
M = genInstances(M, string.ascii_lowercase+string.ascii_uppercase+string.digits+':', 'AfterNominal', 'AfterNominal', 'sq','ltgt')
M = genInstances(M, ' ', 'AfterNominal', 'AfterSpace', 'sq','ltgt')
M = genInstances(M, '*', 'AfterNominal', 'AfterMult', 'sq','ltgt')
M = genInstances(M, '[', 'AfterNominal', 'AfterSpecialChar', '(S sq)','ltgt')
M = genInstances(M, '<', 'AfterNominal', 'AfterSpecialChar', 'sq','(S ltgt)')
M = genClosers(M, 'AfterNominal')
M = genSpurious(M, 'AfterNominal')

for x in M:
  print(x)