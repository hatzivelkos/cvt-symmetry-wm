(* ::Package:: *)

BeginPackage["CVTPulley`Geometry`"];

Needs["CVTPulley`TypesAndValidation`"];

RadiusDifference::usage =
  "RadiusDifference[r, R] returns R - r.";

RadiusRatio::usage =
  "RadiusRatio[r, R] returns R/r.";

GeometryDomainQ::usage =
  "GeometryDomainQ[r, R, d] returns True if the open-belt geometry domain \
condition Abs[R-r] < d is satisfied.";

GeometryDomainCheck::usage =
  "GeometryDomainCheck[r, R, d] returns the input triple if the geometry \
domain is valid, otherwise returns Failure.";

ContactAngleOffset::usage =
  "ContactAngleOffset[r, R, d] returns alpha = ArcSin[(R-r)/d] for the \
open-belt configuration.";

WrapAngleSmall::usage =
  "WrapAngleSmall[r, R, d] returns the belt wrap angle on the smaller pulley.";

WrapAngleLarge::usage =
  "WrapAngleLarge[r, R, d] returns the belt wrap angle on the larger pulley.";

StraightSpanLength::usage =
  "StraightSpanLength[r, R, d] returns the length of one straight belt span \
between the tangent points.";

TotalStraightLength::usage =
  "TotalStraightLength[r, R, d] returns the total length of the two straight \
belt spans.";

ArcLengthSmall::usage =
  "ArcLengthSmall[r, R, d] returns the belt contact arc length on the smaller pulley.";

ArcLengthLarge::usage =
  "ArcLengthLarge[r, R, d] returns the belt contact arc length on the larger pulley.";

TotalArcLength::usage =
  "TotalArcLength[r, R, d] returns the total arc length on both pulleys.";

TangentPointAngles::usage =
  "TangentPointAngles[r, R, d] returns an Association with the main contact-angle data.";

GeometrySummary::usage =
  "GeometrySummary[r, R, d] returns an Association containing the main \
geometric quantities for the open-belt configuration.";

Begin["`Private`"];

(* ========================================= *)
(* Messages                                  *)
(* ========================================= *)

GeometryDomainCheck::badnum =
  "Inputs r, R, d must be positive real numbers. Got r = `1`, R = `2`, d = `3`.";

GeometryDomainCheck::domain =
  "Open-belt geometry requires Abs[R-r] < d. Got Abs[R-r] = `1`, d = `2`.";

ContactAngleOffset::domain =
  "Cannot compute ArcSin[(R-r)/d] because the open-belt condition Abs[R-r] < d is not satisfied.";

StraightSpanLength::domain =
  "Cannot compute straight span length because the quantity d^2-(R-r)^2 is not positive.";

(* ========================================= *)
(* Basic numeric predicates                  *)
(* ========================================= *)

positiveRealQ[x_] := NumericQ[x] && TrueQ[x > 0];

(* ========================================= *)
(* Domain logic                              *)
(* ========================================= *)

GeometryDomainQ[r_, R_, d_] :=
  positiveRealQ[r] && positiveRealQ[R] && positiveRealQ[d] &&
   TrueQ[Abs[R - r] < d];

failure[tag_, msg_, assoc_: <||>] :=
  Failure[tag, Join[<|"MessageTemplate" -> msg|>, assoc]];

GeometryDomainCheck[r_, R_, d_] := Module[{},
  If[!(positiveRealQ[r] && positiveRealQ[R] && positiveRealQ[d]),
    Message[GeometryDomainCheck::badnum, r, R, d];
    Return @ failure[
      "InvalidGeometryInputs",
      "Inputs r, R, d must be positive real numbers.",
      <|"r" -> r, "R" -> R, "d" -> d|>
    ];
  ];

  If[!TrueQ[Abs[R - r] < d],
    Message[GeometryDomainCheck::domain, Abs[R - r], d];
    Return @ failure[
      "InvalidGeometryDomain",
      "Open-belt geometry requires Abs[R-r] < d.",
      <|"AbsDifference" -> Abs[R - r], "d" -> d|>
    ];
  ];

  <|"r" -> r, "R" -> R, "d" -> d|>
];

(* ========================================= *)
(* Elementary geometric quantities           *)
(* ========================================= *)

RadiusDifference[r_, R_] := R - r;

RadiusRatio[r_?positiveRealQ, R_?positiveRealQ] := R/r;

ContactAngleOffset[r_, R_, d_] /; GeometryDomainQ[r, R, d] :=
  ArcSin[(R - r)/d];

ContactAngleOffset[r_, R_, d_] := (
  Message[ContactAngleOffset::domain];
  failure[
    "InvalidGeometryDomain",
    "Cannot compute contact-angle offset outside the open-belt domain.",
    <|"r" -> r, "R" -> R, "d" -> d|>
  ]
);

WrapAngleSmall[r_, R_, d_] /; GeometryDomainQ[r, R, d] :=
  Pi - 2 ContactAngleOffset[r, R, d];

WrapAngleLarge[r_, R_, d_] /; GeometryDomainQ[r, R, d] :=
  Pi + 2 ContactAngleOffset[r, R, d];

StraightSpanLength[r_, R_, d_] /; GeometryDomainQ[r, R, d] :=
  Sqrt[d^2 - (R - r)^2];

StraightSpanLength[r_, R_, d_] := (
  Message[StraightSpanLength::domain];
  failure[
    "InvalidGeometryDomain",
    "Cannot compute straight span length outside the open-belt domain.",
    <|"r" -> r, "R" -> R, "d" -> d|>
  ]
);

TotalStraightLength[r_, R_, d_] /; GeometryDomainQ[r, R, d] :=
  2 StraightSpanLength[r, R, d];

ArcLengthSmall[r_, R_, d_] /; GeometryDomainQ[r, R, d] :=
  r WrapAngleSmall[r, R, d];

ArcLengthLarge[r_, R_, d_] /; GeometryDomainQ[r, R, d] :=
  R WrapAngleLarge[r, R, d];

TotalArcLength[r_, R_, d_] /; GeometryDomainQ[r, R, d] :=
  ArcLengthSmall[r, R, d] + ArcLengthLarge[r, R, d];

(* ========================================= *)
(* Structured summaries                      *)
(* ========================================= *)

TangentPointAngles[r_, R_, d_] /; GeometryDomainQ[r, R, d] := Module[
  {alpha, thetaSmall, thetaLarge},
  alpha = ContactAngleOffset[r, R, d];
  thetaSmall = WrapAngleSmall[r, R, d];
  thetaLarge = WrapAngleLarge[r, R, d];

  <|
    "alpha" -> alpha,
    "wrapAngleSmall" -> thetaSmall,
    "wrapAngleLarge" -> thetaLarge
  |>
];

TangentPointAngles[r_, R_, d_] :=
  failure[
    "InvalidGeometryDomain",
    "Cannot compute tangent-point angles outside the open-belt domain.",
    <|"r" -> r, "R" -> R, "d" -> d|>
  ];

GeometrySummary[r_, R_, d_] /; GeometryDomainQ[r, R, d] := Module[
  {
    alpha, wrapSmall, wrapLarge,
    span, totalSpan,
    arcSmall, arcLarge, totalArc
  },

  alpha = ContactAngleOffset[r, R, d];
  wrapSmall = WrapAngleSmall[r, R, d];
  wrapLarge = WrapAngleLarge[r, R, d];
  span = StraightSpanLength[r, R, d];
  totalSpan = 2 span;
  arcSmall = r wrapSmall;
  arcLarge = R wrapLarge;
  totalArc = arcSmall + arcLarge;

  <|
    "r" -> r,
    "R" -> R,
    "d" -> d,
    "radiusDifference" -> (R - r),
    "radiusRatio" -> (R/r),
    "alpha" -> alpha,
    "wrapAngleSmall" -> wrapSmall,
    "wrapAngleLarge" -> wrapLarge,
    "straightSpanLength" -> span,
    "totalStraightLength" -> totalSpan,
    "arcLengthSmall" -> arcSmall,
    "arcLengthLarge" -> arcLarge,
    "totalArcLength" -> totalArc
  |>
];

GeometrySummary[r_, R_, d_] :=
  failure[
    "InvalidGeometryDomain",
    "Cannot compute geometry summary outside the open-belt domain.",
    <|"r" -> r, "R" -> R, "d" -> d|>
  ];

End[];
EndPackage[];