(* ::Package:: *)

BeginPackage["CVTPulley`BeltLength`"];

Needs["CVTPulley`TypesAndValidation`"];
Needs["CVTPulley`Geometry`"];

BeltLength::usage =
  "BeltLength[r, R, d] returns the total open-belt length for pulley radii \
r, R and center distance d.";

BeltLengthFromParameters::usage =
  "BeltLengthFromParameters[r, R, params] returns the total open-belt length \
using d taken from params.";

ReferenceBeltLength::usage =
  "ReferenceBeltLength[params] returns the reference belt length L0 from \
params. If params[\"beltLength\"] is Missing[\"ToBeComputed\"], it is \
computed from (rMin, rMax, d).";

SetReferenceBeltLength::usage =
  "SetReferenceBeltLength[params] returns params with the key \
\"beltLength\" filled in if it was previously Missing[\"ToBeComputed\"].";

BeltResidual::usage =
  "BeltResidual[r, R, d, L0] returns BeltLength[r, R, d] - L0.";

BeltResidualFromParameters::usage =
  "BeltResidualFromParameters[r, R, params] returns BeltLength[r, R, d] - L0 \
using d and L0 extracted from params.";

BeltLengthComponents::usage =
  "BeltLengthComponents[r, R, d] returns an Association with the component \
contributions to total belt length.";

BeltLengthGradient::usage =
  "BeltLengthGradient[r, R, d] returns an Association containing the partial \
derivatives of the belt length with respect to r and R.";

BeltLengthData::usage =
  "BeltLengthData[r, R, params] returns a structured Association containing \
geometric data, total belt length, reference length, and residual.";

Begin["`Private`"];

(* ========================================= *)
(* Messages                                  *)
(* ========================================= *)

ReferenceBeltLength::badparams =
  "Input must be a validated parameter Association containing at least \
rMin, rMax, d, and beltLength.";

SetReferenceBeltLength::badparams =
  "Input must be a validated parameter Association.";

BeltResidual::badnum =
  "Inputs r, R, d, L0 must be positive real numbers, with valid geometry for r, R, d.";

BeltLengthGradient::domain =
  "Cannot compute belt-length gradient outside the open-belt geometry domain.";

(* ========================================= *)
(* Local helper                              *)
(* ========================================= *)

positiveRealQ[x_] := NumericQ[x] && TrueQ[x > 0];

failure[tag_, msg_, assoc_: <||>] :=
  Failure[tag, Join[<|"MessageTemplate" -> msg|>, assoc]];

(* ========================================= *)
(* Main belt-length function                 *)
(* ========================================= *)

BeltLength[r_, R_, d_] /; GeometryDomainQ[r, R, d] :=
  TotalStraightLength[r, R, d] +
  ArcLengthSmall[r, R, d] +
  ArcLengthLarge[r, R, d];

BeltLength[r_, R_, d_] :=
  failure[
    "InvalidGeometryDomain",
    "Cannot compute belt length outside the open-belt geometry domain.",
    <|"r" -> r, "R" -> R, "d" -> d|>
  ];

BeltLengthFromParameters[r_, R_, params_Association] := Module[
  {d},
  d = GetParameter[params, "d"];
  If[MissingQ[d],
    Return @ failure[
      "MissingParameter",
      "Parameter d is missing.",
      <|"Key" -> "d"|>
    ]
  ];
  BeltLength[r, R, d]
];

(* ========================================= *)
(* Reference length L0                       *)
(* ========================================= *)

ReferenceBeltLength[params_Association] := Module[
  {rMin, rMax, d, L0},

  If[!ParameterQ[params],
    Message[ReferenceBeltLength::badparams];
    Return @ failure[
      "InvalidParameters",
      "Input must be a validated parameter Association."
    ];
  ];

  L0 = params["beltLength"];

  If[L0 === Missing["ToBeComputed"],
    rMin = params["rMin"];
    rMax = params["rMax"];
    d = params["d"];
    Return[BeltLength[rMin, rMax, d]];
  ];

  L0
];

ReferenceBeltLength[_] := (
  Message[ReferenceBeltLength::badparams];
  failure[
    "InvalidParameters",
    "Input must be a validated parameter Association."
  ]
);

SetReferenceBeltLength[params_Association] := Module[
  {validated, L0},
  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated], Return[validated]];

  If[validated["beltLength"] === Missing["ToBeComputed"],
    L0 = BeltLength[validated["rMin"], validated["rMax"], validated["d"]];
    If[FailureQ[L0], Return[L0]];
    AssociateTo[validated, "beltLength" -> L0];
  ];

  validated
];

SetReferenceBeltLength[_] := (
  Message[SetReferenceBeltLength::badparams];
  failure[
    "InvalidParameters",
    "Input must be a validated parameter Association."
  ]
);

(* ========================================= *)
(* Residual                                  *)
(* ========================================= *)

BeltResidual[r_, R_, d_, L0_] /;
   GeometryDomainQ[r, R, d] && positiveRealQ[L0] :=
  BeltLength[r, R, d] - L0;

BeltResidual[r_, R_, d_, L0_] := (
  Message[BeltResidual::badnum];
  failure[
    "InvalidResidualInputs",
    "Inputs to BeltResidual are invalid.",
    <|"r" -> r, "R" -> R, "d" -> d, "L0" -> L0|>
  ]
);

BeltResidualFromParameters[r_, R_, params_Association] := Module[
  {d, L0},
  d = GetParameter[params, "d"];
  L0 = ReferenceBeltLength[params];

  If[MissingQ[d],
    Return @ failure[
      "MissingParameter",
      "Parameter d is missing.",
      <|"Key" -> "d"|>
    ]
  ];

  If[FailureQ[L0], Return[L0]];

  BeltResidual[r, R, d, L0]
];

(* ========================================= *)
(* Structured decomposition                  *)
(* ========================================= *)

BeltLengthComponents[r_, R_, d_] /; GeometryDomainQ[r, R, d] := Module[
  {straightOne, straightTwo, arcSmall, arcLarge, total},
  straightOne = StraightSpanLength[r, R, d];
  straightTwo = StraightSpanLength[r, R, d];
  arcSmall = ArcLengthSmall[r, R, d];
  arcLarge = ArcLengthLarge[r, R, d];
  total = straightOne + straightTwo + arcSmall + arcLarge;

  <|
    "straightSpan1" -> straightOne,
    "straightSpan2" -> straightTwo,
    "totalStraightLength" -> straightOne + straightTwo,
    "arcLengthSmall" -> arcSmall,
    "arcLengthLarge" -> arcLarge,
    "totalArcLength" -> arcSmall + arcLarge,
    "beltLength" -> total
  |>
];

BeltLengthComponents[r_, R_, d_] :=
  failure[
    "InvalidGeometryDomain",
    "Cannot compute belt-length components outside the open-belt domain.",
    <|"r" -> r, "R" -> R, "d" -> d|>
  ];

(* ========================================= *)
(* Gradient                                  *)
(* ========================================= *)

BeltLengthGradient[r_, R_, d_] /; GeometryDomainQ[r, R, d] := Module[
  {dr, dR, expr},
  expr = 2 Sqrt[d^2 - (R - r)^2] +
    r (Pi - 2 ArcSin[(R - r)/d]) +
    R (Pi + 2 ArcSin[(R - r)/d]);

  dr = D[expr, r];
  dR = D[expr, R];

  <|
    "dLdr" -> Simplify[dr, Assumptions -> {r > 0, R > 0, d > 0, Abs[R - r] < d}],
    "dLdR" -> Simplify[dR, Assumptions -> {r > 0, R > 0, d > 0, Abs[R - r] < d}]
  |>
];

BeltLengthGradient[r_, R_, d_] := (
  Message[BeltLengthGradient::domain];
  failure[
    "InvalidGeometryDomain",
    "Cannot compute belt-length gradient outside the open-belt domain.",
    <|"r" -> r, "R" -> R, "d" -> d|>
  ]
);

(* ========================================= *)
(* Combined structured output                *)
(* ========================================= *)

BeltLengthData[r_, R_, params_Association] := Module[
  {d, geom, comps, L, L0, res},

  d = GetParameter[params, "d"];
  If[MissingQ[d],
    Return @ failure[
      "MissingParameter",
      "Parameter d is missing.",
      <|"Key" -> "d"|>
    ]
  ];

  geom = GeometrySummary[r, R, d];
  If[FailureQ[geom], Return[geom]];

  comps = BeltLengthComponents[r, R, d];
  If[FailureQ[comps], Return[comps]];

  L = comps["beltLength"];
  L0 = ReferenceBeltLength[params];
  If[FailureQ[L0], Return[L0]];

  res = L - L0;

  <|
    "geometry" -> geom,
    "components" -> comps,
    "beltLength" -> L,
    "referenceBeltLength" -> L0,
    "residual" -> res
  |>
];

BeltLengthData[___] :=
  failure[
    "InvalidArguments",
    "BeltLengthData expects arguments (r, R, params_Association)."
  ];

End[];
EndPackage[];