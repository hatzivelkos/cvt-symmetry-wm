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
   "workingPrecision" -> 30,
   "exportBaseName" -> "smoketest"
|>];

params = CVTPulley`BeltLength`SetReferenceBeltLength[params0];

VerificationTest[
  AssociationQ[params] && !FailureQ[params],
  True,
  TestID -> "Smoke-01-Parameters"
]

VerificationTest[
  AssociationQ[
    CVTPulley`Geometry`GeometrySummary[
      params["rMin"], params["rMax"], params["d"]
    ]
  ],
  True,
  TestID -> "Smoke-02-Geometry"
]

VerificationTest[
  CVTPulley`RootFinding`RootResultQ[
    CVTPulley`RootFinding`FindRadiusRoot[
      params["rMin"],
      {params["rMin"], params["rMax"]},
      params
    ]
  ],
  True,
  TestID -> "Smoke-03-RootFinding"
]

VerificationTest[
  AssociationQ[CVTPulley`ImplicitMap`ImplicitMapData[params]],
  True,
  TestID -> "Smoke-04-ImplicitMap"
]

symData = CVTPulley`Symmetrization`SymmetrizationData[params];
symProfiles = CVTPulley`Symmetrization`BuildSymmetricDiscreteProfiles[symData];

VerificationTest[
  AssociationQ[symData] && AssociationQ[symProfiles],
  True,
  TestID -> "Smoke-05-Symmetrization"
]

errData = CVTPulley`ErrorAnalysis`ImplicitMapResidualData[params];

VerificationTest[
  AssociationQ[errData] &&
   CVTPulley`ErrorAnalysis`ErrorSummaryQ[errData["Summary"]],
  True,
  TestID -> "Smoke-06-ErrorAnalysis"
]

quadData = CVTPulley`QuadraticApproximation`QuadraticApproximationData[params];

VerificationTest[
  AssociationQ[quadData] &&
   CVTPulley`QuadraticApproximation`QuadraticCoefficientQ[
     quadData["Coefficients"]
   ],
  True,
  TestID -> "Smoke-07-QuadraticApproximation"
]

impTab = CVTPulley`TablesAndFigures`ImplicitMapTable[params];
symTab = CVTPulley`TablesAndFigures`SymmetricProfilesTable[params];
quadTab = CVTPulley`TablesAndFigures`QuadraticApproximationTable[params];
devTab = CVTPulley`TablesAndFigures`DeviationTable[params];

VerificationTest[
  ListQ[impTab] && ListQ[symTab] && ListQ[quadTab] && ListQ[devTab],
  True,
  TestID -> "Smoke-08-Tables"
]

impFig = CVTPulley`TablesAndFigures`ImplicitMapFigure[params];
symFig = CVTPulley`TablesAndFigures`SymmetricProfilesFigure[params];
quadFig = CVTPulley`TablesAndFigures`QuadraticApproximationFigure[params];
devFig = CVTPulley`TablesAndFigures`DeviationFigure[params];

VerificationTest[
  (Head[impFig] === Graphics || Head[impFig] === Legended) &&
   (Head[symFig] === Graphics || Head[symFig] === Legended) &&
   (Head[quadFig] === Graphics || Head[quadFig] === Legended) &&
   (Head[devFig] === Graphics || Head[devFig] === Legended),
  True,
  TestID -> "Smoke-09-Figures"
]

rawRes = CVTPulley`ExportTools`ExportRawWL[errData, "errdata", params];
tabRes = CVTPulley`ExportTools`ExportTableCSV[devTab, "deviationtable", params];
logRes = CVTPulley`ExportTools`ExportLogText["Smoke test completed.", "runlog", params];
figRes = CVTPulley`ExportTools`ExportFigure[devFig, "deviationfigure", "png", params];

VerificationTest[
  CVTPulley`ExportTools`ExportResultQ[rawRes] &&
   CVTPulley`ExportTools`ExportResultQ[tabRes] &&
   CVTPulley`ExportTools`ExportResultQ[logRes] &&
   CVTPulley`ExportTools`ExportResultQ[figRes],
  True,
  TestID -> "Smoke-10-Export"
]