Needs["MUnit`"];

testFile = $TestFileName;
testDir = DirectoryName[testFile];
projectRoot = ParentDirectory[testDir];
packageFile = FileNameJoin[{projectRoot, "src", "CVTPulley", "Kernel", "Package.wl"}];

Get[packageFile];

params0 = CVTPulley`TypesAndValidation`MakeParameters[<|
   "rMin" -> 10,
   "rMax" -> 30,
   "d" -> 70,
   "nGrid" -> 100,
   "workingPrecision" -> 30
|>];

params = CVTPulley`BeltLength`SetReferenceBeltLength[params0];

xi = CVTPulley`QuadraticApproximation`MidpointRadiusValue[params];
coeffs = CVTPulley`QuadraticApproximation`QuadraticCoefficientsFromMidpoint[xi, params];
errData = CVTPulley`ErrorAnalysis`ImplicitMapResidualData[params];

VerificationTest[
  Abs[xi - 20.91580089] < 10^-6,
  True,
  TestID -> "Repr-01-MidpointValue"
]

VerificationTest[
  Abs[coeffs["a"] + 3.66320356] < 10^-6,
  True,
  TestID -> "Repr-02-CoefficientA"
]

VerificationTest[
  Abs[coeffs["b"] - 23.66320356] < 10^-6,
  True,
  TestID -> "Repr-03-CoefficientB"
]

VerificationTest[
  Abs[coeffs["c"] - 10] < 10^-10,
  True,
  TestID -> "Repr-04-CoefficientC"
]

VerificationTest[
  errData["Summary", "MaxAbsResidual"] <= 10^-10,
  True,
  TestID -> "Repr-05-ImplicitMapResidualScale"
]