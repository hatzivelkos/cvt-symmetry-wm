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

pairRes = CVTPulley`Symmetrization`IterateSymmetricPair[2, params, "StoreHistory" -> False];
midRes = CVTPulley`Symmetrization`IterateMidpoint[params, "StoreHistory" -> False];
symData = CVTPulley`Symmetrization`SymmetrizationData[params, "StoreHistory" -> False];
profiles = CVTPulley`Symmetrization`BuildSymmetricDiscreteProfiles[symData];

VerificationTest[
  AssociationQ[pairRes] && pairRes["Type"] === "PairIteration",
  True,
  TestID -> "Sym-01-PairIterationStructure"
]

VerificationTest[
  TrueQ[pairRes["Converged"]],
  True,
  TestID -> "Sym-02-PairIterationConverged"
]

VerificationTest[
  pairRes["FinalState", "MaxCrossError"] <= pairRes["Tolerance"],
  True,
  TestID -> "Sym-03-PairFinalErrorWithinTolerance"
]

VerificationTest[
  AssociationQ[midRes] && midRes["Type"] === "MidpointIteration",
  True,
  TestID -> "Sym-04-MidpointStructure"
]

VerificationTest[
  TrueQ[midRes["Converged"]],
  True,
  TestID -> "Sym-05-MidpointConverged"
]

VerificationTest[
  Abs[midRes["FinalR"] - midRes["Finalr"]] <= midRes["Tolerance"],
  True,
  TestID -> "Sym-06-MidpointSymmetryCondition"
]

VerificationTest[
  AssociationQ[symData] && symData["Type"] === "FullSymmetrization",
  True,
  TestID -> "Sym-07-FullSymmetrizationStructure"
]

VerificationTest[
  AssociationQ[profiles] &&
   Length[profiles["tGrid"]] == params["nGrid"] + 1 &&
   Length[profiles["rGrid"]] == params["nGrid"] + 1 &&
   Length[profiles["RGrid"]] == params["nGrid"] + 1,
  True,
  TestID -> "Sym-08-ProfileLengths"
]

VerificationTest[
  Max[Abs[profiles["RGrid"] - Reverse[profiles["rGrid"]]]] <= params["epsSym"],
  True,
  TestID -> "Sym-09-DiscreteSymmetry"
]

VerificationTest[
  VectorQ[profiles["Pairs"], MatchQ[#, {_?NumericQ, _?NumericQ}] &],
  True,
  TestID -> "Sym-10-PairListStructure"
]