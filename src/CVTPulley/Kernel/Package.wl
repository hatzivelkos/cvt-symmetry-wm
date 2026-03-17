(* ::Package:: *)

BeginPackage["CVTPulley`"];

Module[{base},
  base = DirectoryName[$InputFileName];

  Get[FileNameJoin[{base, "TypesAndValidation.wl"}]];
  Get[FileNameJoin[{base, "Geometry.wl"}]];
  Get[FileNameJoin[{base, "BeltLength.wl"}]];
  Get[FileNameJoin[{base, "RootFinding.wl"}]];
  Get[FileNameJoin[{base, "ImplicitMap.wl"}]];
  Get[FileNameJoin[{base, "Symmetrization.wl"}]];
  Get[FileNameJoin[{base, "ExportTools.wl"}]];
  Get[FileNameJoin[{base, "ErrorAnalysis.wl"}]];
  Get[FileNameJoin[{base, "QuadraticApproximation.wl"}]];
  Get[FileNameJoin[{base, "TablesAndFigures.wl"}]];
];

EndPackage[];