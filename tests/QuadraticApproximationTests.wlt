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
   "nGrid" -> 10,
   "workingPrecision" -> 30
|>];

params = CVTPulley`BeltLength`SetReferenceBeltLength[params0];

xi = CVTPulley`QuadraticApproximation`MidpointRadiusValue[params];
coeffs = CVTPulley`QuadraticApproximation`QuadraticCoefficientsFromMidpoint[xi, params];
quadData = CVTPulley`QuadraticApproximation`QuadraticApproximationData[params];
quadVals = CVTPulley`QuadraticApproximation`QuadraticApproximationValuesOnGrid[params];
fr = quadData["RadiusFunction"];
fR = quadData["MirrorRadiusFunction"];

VerificationTest[
  NumericQ[xi] && params["rMin"] <= xi <= params["rMax"],
  True,
  TestID -> "Quad-01-MidpointValueInRange"
]

VerificationTest[
  CVTPulley`QuadraticApproximation`QuadraticCoefficientQ[coeffs],
  True,
  TestID -> "Quad-02-CoefficientStructure"
]

VerificationTest[
  Abs[fr[0] - params["rMin"]] < 10^-10,
  True,
  TestID -> "Quad-03-LeftEndpointCondition"
]

VerificationTest[
  Abs[fr[1] - params["rMax"]] < 10^-10,
  True,
  TestID -> "Quad-04-RightEndpointCondition"
]

VerificationTest[
  Abs[fr[1/2] - xi] < 10^-10,
  True,
  TestID -> "Quad-05-MidpointInterpolationCondition"
]

VerificationTest[
  Abs[fR[0] - params["rMax"]] < 10^-10,
  True,
  TestID -> "Quad-06-MirrorLeftEndpointCondition"
]

VerificationTest[
  Abs[fR[1] - params["rMin"]] < 10^-10,
  True,
  TestID -> "Quad-07-MirrorRightEndpointCondition"
]

VerificationTest[
  AssociationQ[quadData] && AssociationQ[quadVals],
  True,
  TestID -> "Quad-08-DataStructures"
]

VerificationTest[
  Length[quadVals["tGrid"]] == params["nGrid"] + 1 &&
   Length[quadVals["rApproxGrid"]] == params["nGrid"] + 1 &&
   Length[quadVals["RApproxGrid"]] == params["nGrid"] + 1,
  True,
  TestID -> "Quad-09-GridLengths"
]

VerificationTest[
  Max[
    Abs[
      quadVals["RApproxGrid"] -
      Reverse[quadVals["rApproxGrid"]]
    ]
  ] < 10^-10,
  True,
  TestID -> "Quad-10-DiscreteSymmetry"
]