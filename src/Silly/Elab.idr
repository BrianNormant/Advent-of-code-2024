module Silly.Elab

import Derive.Prelude

%language ElabReflection

data Data = Him
          | Her
          | They

%runElab derive "Data" [Eq, Show]


testEq1 : (Him == Him) = True
testEq1 = Refl

testEq2 : (Her == Her) = True
testEq2 = Refl

testEq3 : (They == They) = True
testEq3 = Refl

failing "While processing"
  testEqFail : (Him == Her) = True
  testEqFail = Refl
