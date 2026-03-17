(* ::Package:: *)

BeginPackage["CVTPulley`QuadraticApproximation`"];

Needs["CVTPulley`TypesAndValidation`"];
Needs["CVTPulley`Symmetrization`"];

QuadraticCoefficientsFromMidpoint::usage =
  "QuadraticCoefficientsFromMidpoint[xi, params] returns the coefficients \
<|\"a\"->a,\"b\"->b,\"c\"->c|> of the quadratic approximation \
f_r(t)=a t^2+b t+c determined by f_r(0)=rMin, f_r(1)=rMax, f_r(1/2)=xi.";

QuadraticRadiusFunction::usage =
  "QuadraticRadiusFunction[xi, params] returns the quadratic approximation \
f_r(t) for the symmetric radius profile.";

QuadraticMirrorRadiusFunction::usage =
  "QuadraticMirrorRadiusFunction[xi, params] returns the symmetric companion \
f_R(t)=f_r(1-t).";

MidpointRadiusValue::usage =
  "MidpointRadiusValue[params] returns the midpoint value xi obtained from \
the midpoint symmetrization procedure.";

QuadraticApproximationData::usage =
  "QuadraticApproximationData[params] builds the quadratic approximation \
data for the symmetric solution using the midpoint value xi.";

QuadraticApproximationValuesOnGrid::usage =
  "QuadraticApproximationValuesOnGrid[params] returns an Association with \
the values of the quadratic approximations f_r and f_R on the default grid.";

QuadraticApproximationPairs::usage =
  "QuadraticApproximationPairs[params] returns an Association with \
{{t_i,f_r(t_i)}} and {{t_i,f_R(t_i)}} on the default grid.";

QuadraticCoefficientQ::usage =
  "QuadraticCoefficientQ[expr] returns True if expr is an Association with \
keys \"a\", \"b\", and \"c\".";

Begin["`Private`"];

(* ========================================= *)
(* Messages                                  *)
(* ========================================= *)

QuadraticCoefficientsFromMidpoint::badparams =
  "Input must be a validated parameter Association.";

QuadraticCoefficientsFromMidpoint::badxi =
  "Midpoint value xi must be a numeric real number in the interval [rMin,rMax]. Got `1`.";

MidpointRadiusValue::badparams =
  "Input must be a validated parameter Association.";

MidpointRadiusValue::badmid =
  "Midpoint symmetrization did not return a valid midpoint result.";

QuadraticApproximationData::badparams =
  "Input must be a validated parameter Association.";

(* ========================================= *)
(* Helpers                                   *)
(* ========================================= *)

failure[tag_, msg_, assoc_: <||>] :=
  Failure[tag, Join[<|"MessageTemplate" -> msg|>, assoc]];

QuadraticCoefficientQ[expr_] :=
  AssociationQ[expr] &&
   KeyExistsQ[expr, "a"] &&
   KeyExistsQ[expr, "b"] &&
   KeyExistsQ[expr, "c"];

(* ========================================= *)
(* Midpoint value                            *)
(* ========================================= *)

MidpointRadiusValue[params_Association] := Module[
  {validated, mid},
  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated],
    Message[MidpointRadiusValue::badparams];
    Return[validated];
  ];

  mid = CVTPulley`Symmetrization`IterateMidpoint[
    validated,
    "StoreHistory" -> False,
    "ImplicitStoreHistory" -> False
  ];
  If[FailureQ[mid], Return[mid]];

  If[!AssociationQ[mid] || !KeyExistsQ[mid, "Finalr"] || !KeyExistsQ[mid, "FinalR"],
    Message[MidpointRadiusValue::badmid];
    Return @ failure[
      "InvalidMidpointResult",
      "Midpoint symmetrization did not return a valid midpoint result.",
      <|"MidpointResult" -> mid|>
    ];
  ];

  mid["Finalr"]
];

MidpointRadiusValue[_] := (
  Message[MidpointRadiusValue::badparams];
  failure["InvalidParameters", "Input must be a validated parameter Association."]
);

(* ========================================= *)
(* Quadratic coefficients                    *)
(* ========================================= *)

QuadraticCoefficientsFromMidpoint[xi_?NumericQ, params_Association] := Module[
  {validated, rMin, rMax, a, b, c},
  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated],
    Message[QuadraticCoefficientsFromMidpoint::badparams];
    Return[validated];
  ];

  rMin = validated["rMin"];
  rMax = validated["rMax"];

  If[!(rMin <= xi <= rMax),
    Message[QuadraticCoefficientsFromMidpoint::badxi, xi];
    Return @ failure[
      "InvalidMidpointValue",
      "Midpoint value xi must lie in [rMin,rMax].",
      <|"xi" -> xi, "rMin" -> rMin, "rMax" -> rMax|>
    ];
  ];

  a = 2 rMin + 2 rMax - 4 xi;
  b = 4 xi - rMax - 3 rMin;
  c = rMin;

  <|
    "a" -> a,
    "b" -> b,
    "c" -> c
  |>
];

QuadraticCoefficientsFromMidpoint[_, params_Association] := Module[
  {validated, rMin, rMax, xi},
  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated],
    Message[QuadraticCoefficientsFromMidpoint::badparams];
    Return[validated];
  ];

  rMin = validated["rMin"];
  rMax = validated["rMax"];
  xi = Unevaluated[Sequence[]];

  Message[QuadraticCoefficientsFromMidpoint::badxi, xi];
  failure[
    "InvalidMidpointValue",
    "Midpoint value xi must be numeric and lie in [rMin,rMax].",
    <|"rMin" -> rMin, "rMax" -> rMax|>
  ]
];

QuadraticCoefficientsFromMidpoint[_, _] := (
  Message[QuadraticCoefficientsFromMidpoint::badparams];
  failure["InvalidArguments", "QuadraticCoefficientsFromMidpoint expects (xi_?NumericQ, params_Association)."]
);

(* ========================================= *)
(* Quadratic functions                       *)
(* ========================================= *)

QuadraticRadiusFunction[xi_?NumericQ, params_Association] := Module[
  {coeffs},
  coeffs = QuadraticCoefficientsFromMidpoint[xi, params];
  If[FailureQ[coeffs], Return[coeffs]];

  Function[{t}, coeffs["a"] t^2 + coeffs["b"] t + coeffs["c"]]
];

QuadraticMirrorRadiusFunction[xi_?NumericQ, params_Association] := Module[
  {fr},
  fr = QuadraticRadiusFunction[xi, params];
  If[FailureQ[fr], Return[fr]];

  Function[{t}, fr[1 - t]]
];

(* ========================================= *)
(* Full approximation data                   *)
(* ========================================= *)

QuadraticApproximationData[params_Association] := Module[
  {validated, xi, coeffs, fr, fR},
  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated],
    Message[QuadraticApproximationData::badparams];
    Return[validated];
  ];

  xi = MidpointRadiusValue[validated];
  If[FailureQ[xi], Return[xi]];

  coeffs = QuadraticCoefficientsFromMidpoint[xi, validated];
  If[FailureQ[coeffs], Return[coeffs]];

  fr = QuadraticRadiusFunction[xi, validated];
  If[FailureQ[fr], Return[fr]];

  fR = QuadraticMirrorRadiusFunction[xi, validated];
  If[FailureQ[fR], Return[fR]];

  <|
    "Type" -> "QuadraticApproximation",
    "MidpointValue" -> xi,
    "Coefficients" -> coeffs,
    "RadiusFunction" -> fr,
    "MirrorRadiusFunction" -> fR
  |>
];

QuadraticApproximationData[_] := (
  Message[QuadraticApproximationData::badparams];
  failure["InvalidParameters", "Input must be a validated parameter Association."]
);

QuadraticApproximationValuesOnGrid[params_Association] := Module[
  {validated, data, n, tGrid, fr, fR},
  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated], Return[validated]];

  data = QuadraticApproximationData[validated];
  If[FailureQ[data], Return[data]];

  n = validated["nGrid"];
  tGrid = N[Range[0, n]/n];
  fr = data["RadiusFunction"];
  fR = data["MirrorRadiusFunction"];

  <|
    "tGrid" -> tGrid,
    "rApproxGrid" -> (fr /@ tGrid),
    "RApproxGrid" -> (fR /@ tGrid)
  |>
];

QuadraticApproximationPairs[params_Association] := Module[
  {vals},
  vals = QuadraticApproximationValuesOnGrid[params];
  If[FailureQ[vals], Return[vals]];

  <|
    "rPairs" -> Transpose[{vals["tGrid"], vals["rApproxGrid"]}],
    "RPairs" -> Transpose[{vals["tGrid"], vals["RApproxGrid"]}]
  |>
];

End[];
EndPackage[];