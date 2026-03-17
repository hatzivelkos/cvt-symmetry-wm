(* ::Package:: *)

BeginPackage["CVTPulley`TablesAndFigures`"];

Needs["CVTPulley`TypesAndValidation`"];
Needs["CVTPulley`ImplicitMap`"];
Needs["CVTPulley`Symmetrization`"];
Needs["CVTPulley`QuadraticApproximation`"];
Needs["CVTPulley`ErrorAnalysis`"];
Needs["CVTPulley`BeltLength`"];

ImplicitMapTable::usage =
  "ImplicitMapTable[params] returns tabular data for the default implicit map \
with columns {i,t_i,r_i,R_i}.";

SymmetricProfilesTable::usage =
  "SymmetricProfilesTable[params] returns tabular data for the final symmetric \
discrete profiles with columns {i,t_i,r_i,R_i}.";

QuadraticApproximationTable::usage =
  "QuadraticApproximationTable[params] returns tabular data for the quadratic \
approximation with columns {i,t_i,rApprox_i,RApprox_i}.";

DeviationTable::usage =
  "DeviationTable[params] returns tabular data for the quadratic-approximation \
belt-length deviation with columns {i,t_i,LApprox_i,Residual_i,PercentDeviation_i}.";

ImplicitMapFigure::usage =
  "ImplicitMapFigure[params] returns a figure comparing the initial linear profile \
r(t_i) and the implicit-map values R(t_i).";

SymmetricProfilesFigure::usage =
  "SymmetricProfilesFigure[params] returns a figure of the final symmetric \
discrete profiles.";

QuadraticApproximationFigure::usage =
  "QuadraticApproximationFigure[params] returns a figure comparing the symmetric \
discrete profiles and their quadratic approximations.";

DeviationFigure::usage =
  "DeviationFigure[params] returns a figure of the belt-length deviation induced \
by the quadratic approximation.";

Begin["`Private`"];

(* ========================================= *)
(* Messages                                  *)
(* ========================================= *)

ImplicitMapTable::badparams =
  "Input must be a validated parameter Association.";

SymmetricProfilesTable::badparams =
  "Input must be a validated parameter Association.";

QuadraticApproximationTable::badparams =
  "Input must be a validated parameter Association.";

DeviationTable::badparams =
  "Input must be a validated parameter Association.";

ImplicitMapFigure::badparams =
  "Input must be a validated parameter Association.";

SymmetricProfilesFigure::badparams =
  "Input must be a validated parameter Association.";

QuadraticApproximationFigure::badparams =
  "Input must be a validated parameter Association.";

DeviationFigure::badparams =
  "Input must be a validated parameter Association.";

(* ========================================= *)
(* Helpers                                   *)
(* ========================================= *)

failure[tag_, msg_, assoc_: <||>] :=
  Failure[tag, Join[<|"MessageTemplate" -> msg|>, assoc]];

indexColumn[n_Integer?Positive] := Range[0, n];

(* ========================================= *)
(* Tables                                    *)
(* ========================================= *)

ImplicitMapTable[params_Association] := Module[
  {validated, data, n},
  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated],
    Message[ImplicitMapTable::badparams];
    Return[validated];
  ];

  data = CVTPulley`ImplicitMap`ImplicitMapData[validated];
  If[FailureQ[data], Return[data]];

  n = validated["nGrid"];

  Prepend[
    Transpose[{
      indexColumn[n],
      data["tGrid"],
      data["rGrid"],
      data["RGrid"]
    }],
    {"i", "t", "r", "R"}
  ]
];

ImplicitMapTable[_] := (
  Message[ImplicitMapTable::badparams];
  failure["InvalidParameters", "Input must be a validated parameter Association."]
);

SymmetricProfilesTable[params_Association] := Module[
  {validated, symData, prof, n},
  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated],
    Message[SymmetricProfilesTable::badparams];
    Return[validated];
  ];

  symData = CVTPulley`Symmetrization`SymmetrizationData[validated];
  If[FailureQ[symData], Return[symData]];

  prof = CVTPulley`Symmetrization`BuildSymmetricDiscreteProfiles[symData];
  If[FailureQ[prof], Return[prof]];

  n = validated["nGrid"];

  Prepend[
    Transpose[{
      indexColumn[n],
      prof["tGrid"],
      prof["rGrid"],
      prof["RGrid"]
    }],
    {"i", "t", "rSym", "RSym"}
  ]
];

SymmetricProfilesTable[_] := (
  Message[SymmetricProfilesTable::badparams];
  failure["InvalidParameters", "Input must be a validated parameter Association."]
);

QuadraticApproximationTable[params_Association] := Module[
  {validated, vals, n},
  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated],
    Message[QuadraticApproximationTable::badparams];
    Return[validated];
  ];

  vals = CVTPulley`QuadraticApproximation`QuadraticApproximationValuesOnGrid[validated];
  If[FailureQ[vals], Return[vals]];

  n = validated["nGrid"];

  Prepend[
    Transpose[{
      indexColumn[n],
      vals["tGrid"],
      vals["rApproxGrid"],
      vals["RApproxGrid"]
    }],
    {"i", "t", "rApprox", "RApprox"}
  ]
];

QuadraticApproximationTable[_] := (
  Message[QuadraticApproximationTable::badparams];
  failure["InvalidParameters", "Input must be a validated parameter Association."]
);

DeviationTable[params_Association] := Module[
  {validated, vals, tGrid, rA, RA, L0, LApprox, residuals, pct, n},
  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated],
    Message[DeviationTable::badparams];
    Return[validated];
  ];

  vals = CVTPulley`QuadraticApproximation`QuadraticApproximationValuesOnGrid[validated];
  If[FailureQ[vals], Return[vals]];

  tGrid = vals["tGrid"];
  rA = vals["rApproxGrid"];
  RA = vals["RApproxGrid"];
  L0 = validated["beltLength"];
  n = validated["nGrid"];

  residuals =
    MapThread[
      CVTPulley`BeltLength`BeltResidualFromParameters[#1, #2, validated] &,
      {rA, RA}
    ];

  If[!VectorQ[residuals, NumericQ],
    Return @ failure[
      "DeviationComputationFailed",
      "Could not compute numeric residuals for the quadratic approximation."
    ]
  ];

  LApprox = residuals + L0;
  pct = 100 residuals/L0;

  Prepend[
    Transpose[{
      indexColumn[n],
      tGrid,
      LApprox,
      residuals,
      pct
    }],
    {"i", "t", "LApprox", "Residual", "PercentDeviation"}
  ]
];

DeviationTable[_] := (
  Message[DeviationTable::badparams];
  failure["InvalidParameters", "Input must be a validated parameter Association."]
);

(* ========================================= *)
(* Figures                                   *)
(* ========================================= *)

ImplicitMapFigure[params_Association] := Module[
  {validated, data, p1, p2},
  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated],
    Message[ImplicitMapFigure::badparams];
    Return[validated];
  ];

  data = CVTPulley`ImplicitMap`ImplicitMapData[validated];
  If[FailureQ[data], Return[data]];

  p1 = Transpose[{data["tGrid"], data["rGrid"]}];
  p2 = Transpose[{data["tGrid"], data["RGrid"]}];

  ListLinePlot[
    {p1, p2},
    PlotLegends -> {"r(t)", "R(t)"},
    AxesLabel -> {"t", "radius"},
    PlotRange -> All
  ]
];

ImplicitMapFigure[_] := (
  Message[ImplicitMapFigure::badparams];
  failure["InvalidParameters", "Input must be a validated parameter Association."]
);

SymmetricProfilesFigure[params_Association] := Module[
  {validated, symData, prof, p1, p2},
  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated],
    Message[SymmetricProfilesFigure::badparams];
    Return[validated];
  ];

  symData = CVTPulley`Symmetrization`SymmetrizationData[validated];
  If[FailureQ[symData], Return[symData]];

  prof = CVTPulley`Symmetrization`BuildSymmetricDiscreteProfiles[symData];
  If[FailureQ[prof], Return[prof]];

  p1 = Transpose[{prof["tGrid"], prof["rGrid"]}];
  p2 = Transpose[{prof["tGrid"], prof["RGrid"]}];

  ListLinePlot[
    {p1, p2},
    PlotLegends -> {"rSym(t)", "RSym(t)"},
    AxesLabel -> {"t", "radius"},
    PlotRange -> All
  ]
];

SymmetricProfilesFigure[_] := (
  Message[SymmetricProfilesFigure::badparams];
  failure["InvalidParameters", "Input must be a validated parameter Association."]
);

QuadraticApproximationFigure[params_Association] := Module[
  {validated, symData, prof, vals, p1, p2, q1, q2},
  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated],
    Message[QuadraticApproximationFigure::badparams];
    Return[validated];
  ];

  symData = CVTPulley`Symmetrization`SymmetrizationData[validated];
  If[FailureQ[symData], Return[symData]];

  prof = CVTPulley`Symmetrization`BuildSymmetricDiscreteProfiles[symData];
  If[FailureQ[prof], Return[prof]];

  vals = CVTPulley`QuadraticApproximation`QuadraticApproximationValuesOnGrid[validated];
  If[FailureQ[vals], Return[vals]];

  p1 = Transpose[{prof["tGrid"], prof["rGrid"]}];
  p2 = Transpose[{prof["tGrid"], prof["RGrid"]}];
  q1 = Transpose[{vals["tGrid"], vals["rApproxGrid"]}];
  q2 = Transpose[{vals["tGrid"], vals["RApproxGrid"]}];

  ListLinePlot[
    {p1, p2, q1, q2},
    PlotLegends -> {"rSym", "RSym", "rQuad", "RQuad"},
    AxesLabel -> {"t", "radius"},
    PlotRange -> All
  ]
];

QuadraticApproximationFigure[_] := (
  Message[QuadraticApproximationFigure::badparams];
  failure["InvalidParameters", "Input must be a validated parameter Association."]
);

DeviationFigure[params_Association] := Module[
  {validated, tab, pts},
  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated],
    Message[DeviationFigure::badparams];
    Return[validated];
  ];

  tab = DeviationTable[validated];
  If[FailureQ[tab], Return[tab]];

  pts = tab[[2 ;;, {2, 5}]];

  ListLinePlot[
    pts,
    PlotLegends -> {"Percent deviation"},
    AxesLabel -> {"t", "% deviation"},
    PlotRange -> All
  ]
];

DeviationFigure[_] := (
  Message[DeviationFigure::badparams];
  failure["InvalidParameters", "Input must be a validated parameter Association."]
);

End[];
EndPackage[];