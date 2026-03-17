Needs["MUnit`"];

testFile = $TestFileName;
testDir = DirectoryName[testFile];
projectRoot = ParentDirectory[testDir];
packageFile = FileNameJoin[{projectRoot, "src", "CVTPulley", "Kernel", "Package.wl"}];

Get[packageFile];

r = 10;
R = 30;
d = 70;

geom = CVTPulley`Geometry`GeometrySummary[r, R, d];

VerificationTest[
  CVTPulley`Geometry`GeometryDomainQ[r, R, d],
  True,
  TestID -> "Geom-01-DomainQ"
]

VerificationTest[
  AssociationQ[CVTPulley`Geometry`GeometryDomainCheck[r, R, d]],
  True,
  TestID -> "Geom-02-DomainCheck"
]

VerificationTest[
  Abs[
    CVTPulley`Geometry`ContactAngleOffset[r, R, d] -
    ArcSin[(R - r)/d]
  ] < 10^-12,
  True,
  TestID -> "Geom-03-ContactAngleOffset"
]

VerificationTest[
  Abs[
    CVTPulley`Geometry`StraightSpanLength[r, R, d] -
    Sqrt[d^2 - (R - r)^2]
  ] < 10^-12,
  True,
  TestID -> "Geom-04-StraightSpanLength"
]

VerificationTest[
  Abs[
    CVTPulley`Geometry`TotalStraightLength[r, R, d] -
    2 CVTPulley`Geometry`StraightSpanLength[r, R, d]
  ] < 10^-12,
  True,
  TestID -> "Geom-05-TotalStraightLength"
]

VerificationTest[
  Abs[
    CVTPulley`Geometry`WrapAngleSmall[r, R, d] +
    CVTPulley`Geometry`WrapAngleLarge[r, R, d] -
    2 Pi
  ] < 10^-12,
  True,
  TestID -> "Geom-06-WrapAngleSum"
]

VerificationTest[
  AssociationQ[geom],
  True,
  TestID -> "Geom-07-GeometrySummaryStructure"
]

VerificationTest[
  Abs[
    geom["totalStraightLength"] -
    CVTPulley`Geometry`TotalStraightLength[r, R, d]
  ] < 10^-12,
  True,
  TestID -> "Geom-08-SummaryStraightLength"
]

VerificationTest[
  Abs[
    geom["totalArcLength"] -
    CVTPulley`Geometry`TotalArcLength[r, R, d]
  ] < 10^-12,
  True,
  TestID -> "Geom-09-SummaryArcLength"
]

VerificationTest[
  Abs[
    geom["radiusDifference"] - (R - r)
  ] < 10^-12,
  True,
  TestID -> "Geom-10-RadiusDifference"
]