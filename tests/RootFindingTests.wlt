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

rootAtLeft = CVTPulley`RootFinding`FindRadiusRoot[
   params["rMin"],
   {params["rMin"], params["rMax"]},
   params
];

rootAtMid = CVTPulley`RootFinding`FindRadiusRoot[
   20,
   {params["rMin"], params["rMax"]},
   params
];

VerificationTest[
  CVTPulley`RootFinding`RootResultQ[rootAtLeft],
  True,
  TestID -> "Root-01-EndpointRootResultQ"
]

VerificationTest[
  rootAtLeft["Converged"],
  True,
  TestID -> "Root-02-EndpointConverged"
]

VerificationTest[
  Abs[rootAtLeft["Residual"]] <= params["epsBis"],
  True,
  TestID -> "Root-03-EndpointResidualWithinTolerance"
]

VerificationTest[
  rootAtLeft["Root"] >= params["rMin"] && rootAtLeft["Root"] <= params["rMax"],
  True,
  TestID -> "Root-04-EndpointRootInsideBracket"
]

VerificationTest[
  rootAtLeft["Iterations"] == 0,
  True,
  TestID -> "Root-05-EndpointAcceptedImmediately"
]

VerificationTest[
  CVTPulley`RootFinding`RootResultQ[rootAtMid],
  True,
  TestID -> "Root-06-InteriorRootResultQ"
]

VerificationTest[
  rootAtMid["Converged"],
  True,
  TestID -> "Root-07-InteriorConverged"
]

VerificationTest[
  Abs[rootAtMid["Residual"]] <= params["epsBis"],
  True,
  TestID -> "Root-08-InteriorResidualWithinTolerance"
]

VerificationTest[
  rootAtMid["Root"] >= params["rMin"] && rootAtMid["Root"] <= params["rMax"],
  True,
  TestID -> "Root-09-InteriorRootInsideBracket"
]

VerificationTest[
  rootAtMid["Iterations"] >= 0,
  True,
  TestID -> "Root-10-InteriorIterationCountNonnegative"
]