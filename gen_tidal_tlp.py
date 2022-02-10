import string
RN = {':': "Colon", '<': 'LessThan', ';': 'SemiColon', '>': 'GreaterThan',
      '[': 'OpenSquare', ']': 'CloseSquare', '*': 'Star', ' ': 'Space', '~': 'Tilde', '\n': "NewLine", '\t': "Tab",',': "Comma"}


def sanitize(x): return '\\n' if x == '\n' else '\\t' if x == '\t' else x

def ltgtnFails(o, x, nm):
    o = o + ['else instance %s%sClean :: %s "%s" "" Z Z' %
             (nm[0].lower()+nm[1:], RN[x] if x in RN else x, nm, sanitize(x))]
    o = o + ['else instance %s%sFailLtGt :: Fail (Text "Parser found a < without a closing >") => %s "%s" "" Z (S n)' % (
        nm[0].lower()+nm[1:], RN[x] if x in RN else x, nm, sanitize(x))]
    o = o + ['else instance %s%sFailSq :: Fail (Text "Parser found a [ without a closing ]") => %s "%s" "" (S n) x' % (
        nm[0].lower()+nm[1:], RN[x] if x in RN else x, nm, sanitize(x))]
    return o


def genInstances(o, strs, nm, nxt, sq, ltgt):
    for x in strs:
        o = ltgtnFails(o, x, nm)
        o = o + ['else instance %s%s :: (Cons head tail rest, %s head tail %s %s) => %s "%s" rest sq ltgt' % (
            nm[0].lower()+nm[1:], RN[x] if x in RN else x, nxt, sq, ltgt, nm, sanitize(x))]
    return o


def genClosers(o, nm):
  o = o +['else instance %sValidGt :: %s ">" "" Z (S Z)' % (
          nm[0].lower()+nm[1:], nm)]
  o = o + ['else instance %sValidSq :: %s "]" "" (S Z) Z' % (
      nm[0].lower()+nm[1:], nm)]
  ####
  o = o+['else instance %sTooManyGt :: Fail (Text "Too many closing >") => %s ">" "" Z Z' %
          (nm[0].lower()+nm[1:], nm)]
  o = o+['else instance %sTooManySq :: Fail (Text "Too many closing ]") => %s "]" "" Z Z' %
         (nm[0].lower()+nm[1:], nm)]
  ###
  o = o+['else instance %sNotEnoughGt :: Fail (Text "Not enough closing >") => %s ">" "" Z (S ltgt)' % (
      nm[0].lower()+nm[1:], nm)]
  o = o+['else instance %sNotEnoughSq :: Fail (Text "Not enough closing ]") => %s "]" "" (S sq) Z' % (
      nm[0].lower()+nm[1:], nm)]
  ###
  o = o+['else instance %sContinuingGt :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => %s ">" rest sq (S ltgt)' % (
      nm[0].lower()+nm[1:], nm)]
  o = o+['else instance %sContinuingSq :: (Cons head tail rest, AfterSpecialChar head tail sq ltgt) => %s "]" rest (S sq) ltgt' % (
      nm[0].lower()+nm[1:], nm)]
  ###
  o = o+['else instance %sUnderfullGt :: Fail (Text "Too many closing >") => %s ">" rest sq Z' %
         (nm[0].lower()+nm[1:], nm)]
  o = o+['else instance %sUnderfullSq :: Fail (Text "Too many closing ]") => %s "]" rest Z ltgt' %
         (nm[0].lower()+nm[1:], nm)]
  return o


def genSpurious(o, nm):
    o = o+['else instance %sSpurious :: Fail (Text "Not a valid symbol") => %s invalid rest Z Z' %
           (nm[0].lower()+nm[1:], nm)]
    return o


M = []


def genStartingBloc(o, k):
    return o + ['-- starting bloc %s' % k]


def makeKlsDef(o, k):
    return o + ['class %s (h :: Symbol) (t :: Symbol) (sq :: Nat) (ltgt :: Nat)' % k]


for myClass in ['AfterNominal', 'AfterSpecialChar', 'AfterTag']:
    M = genStartingBloc(M, myClass)
    M = makeKlsDef(M, myClass)
    M = genInstances(M, string.ascii_lowercase+string.ascii_uppercase +
                     string.digits+':~', myClass, 'AfterNominal', 'sq', 'ltgt')
    M = genInstances(M, ' \t\n,', myClass, 'AfterSpecialChar', 'sq', 'ltgt')
    if myClass == 'AfterNominal':
        M = genInstances(M, '*', myClass, 'AfterMult', 'sq', 'ltgt')
        M = genInstances(M, ';', myClass, 'AfterTag', 'sq', 'ltgt')
    M = genInstances(M, '[', myClass, 'AfterSpecialChar', '(S sq)', 'ltgt')
    M = genInstances(M, '<', myClass, 'AfterSpecialChar', 'sq', '(S ltgt)')
    M = genClosers(M, myClass)
    M = genSpurious(M, myClass)

M = genStartingBloc(M, 'AfterMult')
M = makeKlsDef(M, 'AfterMult')
M = genInstances(M, '123456789', 'AfterMult', 'AfterFirstDigit', 'sq', 'ltgt')
M = genSpurious(M, 'AfterMult')

for myClass in ['AfterFirstDigit', 'AfterSubsequentDigit']:
    M = genStartingBloc(M, myClass)
    M = makeKlsDef(M, myClass)
    M = genInstances(M, '0123456789', myClass,
                     'AfterSubsequentDigit', 'sq', 'ltgt')
    M = genInstances(M, ' \t\n,', myClass, 'AfterSpecialChar', 'sq', 'ltgt')
    M = genInstances(M, '[', myClass, 'AfterSpecialChar', '(S sq)', 'ltgt')
    M = genInstances(M, '<', myClass, 'AfterSpecialChar', 'sq', '(S ltgt)')
    M = genClosers(M, myClass)
    M = genSpurious(M, myClass)

for x in range(len(M)):
    if M[x][:5] == 'class':
        M[x+1] = M[x+1][5:]

M = [
    'module WAGS.Lib.Tidal.TLP where\n\nimport Prim.TypeError(class Fail, Text)\nimport Prim.Symbol (class Cons)\n\ndata Nat\nforeign import data Z :: Nat\nforeign import data S :: Nat -> Nat\n\nclass MiniNotation (sym :: Symbol)\ninstance miniNotationEmpty :: MiniNotation ""\nelse instance miniNotationInteresting :: (Cons head tail sym, AfterSpecialChar head tail Z Z) => MiniNotation sym'] + M

with open('src/WAGS/Lib/Tidal/TLP.purs', 'w') as f:
    for x in M:
        f.write(x+'\n')
