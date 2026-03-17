(* ::Package:: *)

BeginPackage["CVTPulley`RootFinding`"];

Needs["CVTPulley`TypesAndValidation`"];
Needs["CVTPulley`BeltLength`"];
Needs["CVTPulley`Geometry`"];

BracketSignChangeQ::usage =
  "BracketSignChangeQ[f, {a, b}] returns True if f[a] and f[b] have opposite \
signs or if one endpoint is already a root.";

BisectionRoot::usage =
  "BisectionRoot[f, {a, b}] applies the bisection method to find a root of f \
on the interval {a, b}.";

FindRadiusRoot::usage =
  "FindRadiusRoot[r, {RLeft, RRight}, params] solves BeltResidual[r, R, d, L0]==0 \
for R by bisection on the interval {RLeft, RRight}.";

DefaultRadiusBracket::usage =
  "DefaultRadiusBracket[r, params] returns a default search bracket for R \
compatible with the open-belt domain.";

RootResultQ::usage =
  "RootResultQ[expr] returns True if expr is an Association representing a \
successful root-finding result.";

Begin["`Private`"];

(* ========================================= *)
(* Messages                                  *)
(* ========================================= *)

BracketSignChangeQ::badbracket =
  "Bracket must be a numeric pair {a,b} with a < b. Got `1`.";

BisectionRoot::badbracket =
  "Bracket must be a numeric pair {a,b} with a < b. Got `1`.";

BisectionRoot::nosign =
  "Bisection requires a sign-changing bracket or a root at an endpoint. \
Got f[a] = `1`, f[b] = `2`.";

BisectionRoot::maxiter =
  "Bisection did not converge within the allowed number of iterations.";

FindRadiusRoot::badparams =
  "Input must be a validated parameter Association.";

FindRadiusRoot::badbracket =
  "Radius bracket must be a numeric pair {RLeft,RRight} with RLeft < RRight.";

DefaultRadiusBracket::badparams =
  "Input must be a validated parameter Association.";

DefaultRadiusBracket::domain =
  "Cannot construct a default radius bracket because the open-belt domain \
would be violated.";

(* ========================================= *)
(* Helpers                                   *)
(* ========================================= *)

positiveRealQ[x_] := NumericQ[x] && TrueQ[x > 0];

numericBracketQ[br_] :=
  MatchQ[br, {_?NumericQ, _?NumericQ}] && TrueQ[br[[1]] < br[[2]]];

failure[tag_, msg_, assoc_: <||>] :=
  Failure[tag, Join[<|"MessageTemplate" -> msg|>, assoc]];

RootResultQ[expr_] :=
  AssociationQ[expr] &&
   KeyExistsQ[expr, "Root"] &&
   KeyExistsQ[expr, "Residual"] &&
   KeyExistsQ[expr, "Iterations"] &&
   KeyExistsQ[expr, "Converged"];

(* ========================================= *)
(* Bracket check                             *)
(* ========================================= *)

BracketSignChangeQ[f_, br_] /; !numericBracketQ[br] := (
  Message[BracketSignChangeQ::badbracket, br];
  False
);

BracketSignChangeQ[f_, {a_?NumericQ, b_?NumericQ}] := Module[
  {fa, fb},
  fa = f[a];
  fb = f[b];

  If[!NumericQ[fa] || !NumericQ[fb], Return[False]];

  TrueQ[fa == 0 || fb == 0 || fa fb < 0]
];

(* ========================================= *)
(* Default bracket for R                     *)
(* ========================================= *)

DefaultRadiusBracket[r_?NumericQ, params_Association] := Module[
  {validated, d, rMin, rMax, delta, left, right},

  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated],
    Message[DefaultRadiusBracket::badparams];
    Return[validated];
  ];

  d = validated["d"];
  rMin = validated["rMin"];
  rMax = validated["rMax"];

  (* Small safety margin to stay strictly inside Abs[R-r] < d *)
  delta = Max[10^-10, 100 validated["epsBis"]];

  left = Max[rMin, r - d + delta];
  right = Min[rMax + d, r + d - delta];

  If[!(NumericQ[left] && NumericQ[right] && left < right),
    Message[DefaultRadiusBracket::domain];
    Return @ failure[
      "InvalidDefaultBracket",
      "Failed to construct a valid default radius bracket.",
      <|"r" -> r, "Left" -> left, "Right" -> right|>
    ];
  ];

  {left, right}
];

DefaultRadiusBracket[_, _] := (
  Message[DefaultRadiusBracket::badparams];
  failure["InvalidParameters", "Input must be a validated parameter Association."]
);

(* ========================================= *)
(* Core bisection                            *)
(* ========================================= *)

Options[BisectionRoot] = {
  "Tolerance" -> 10^-12,
  "MaxIterations" -> 200,
  "WorkingPrecision" -> MachinePrecision,
  "StoreHistory" -> False
};

BisectionRoot[f_, br_, OptionsPattern[]] /; !numericBracketQ[br] := (
  Message[BisectionRoot::badbracket, br];
  failure["InvalidBracket", "Bracket must be a numeric pair {a,b} with a < b.", <|"Bracket" -> br|>]
);

BisectionRoot[f_, {a0_?NumericQ, b0_?NumericQ}, OptionsPattern[]] := Module[
  {
    a, b, c, fa, fb, fc,
    tol, maxIter, wp, storeHistory,
    iter = 0, history = {}, converged = False
  },

  tol = OptionValue["Tolerance"];
  maxIter = OptionValue["MaxIterations"];
  wp = OptionValue["WorkingPrecision"];
  storeHistory = TrueQ[OptionValue["StoreHistory"]];

  a = SetPrecision[a0, wp];
  b = SetPrecision[b0, wp];
  fa = N[f[a], wp];
  fb = N[f[b], wp];

  If[!NumericQ[fa] || !NumericQ[fb],
    Return @ failure[
      "NonNumericFunctionValues",
      "Function values at bracket endpoints are not numeric.",
      <|"a" -> a, "b" -> b, "f[a]" -> fa, "f[b]" -> fb|>
    ]
  ];

 If[Abs[fa] <= tol,
  Return @ <|
    "Root" -> a,
    "Residual" -> fa,
    "Iterations" -> 0,
    "Converged" -> True,
    "Method" -> "Bisection",
    "Bracket" -> {a0, b0},
    "FinalBracket" -> {a, a},
    "BracketWidth" -> 0,
    "History" -> If[storeHistory, {}, Missing["NotStored"]]
  |>
];

If[Abs[fb] <= tol,
  Return @ <|
    "Root" -> b,
    "Residual" -> fb,
    "Iterations" -> 0,
    "Converged" -> True,
    "Method" -> "Bisection",
    "Bracket" -> {a0, b0},
    "FinalBracket" -> {b, b},
    "BracketWidth" -> 0,
    "History" -> If[storeHistory, {}, Missing["NotStored"]]
  |>
];

  If[!(fa fb < 0),
    Message[BisectionRoot::nosign, fa, fb];
    Return @ failure[
      "NoSignChange",
      "Bisection requires a sign-changing bracket or a root at an endpoint.",
      <|"a" -> a, "b" -> b, "f[a]" -> fa, "f[b]" -> fb|>
    ]
  ];

  While[iter < maxIter,
    iter++;
    c = N[(a + b)/2, wp];
    fc = N[f[c], wp];

    If[storeHistory,
      AppendTo[history, <|
        "Iteration" -> iter,
        "a" -> a,
        "b" -> b,
        "c" -> c,
        "f[a]" -> fa,
        "f[b]" -> fb,
        "f[c]" -> fc,
        "BracketWidth" -> (b - a)
      |>]
    ];

    If[!NumericQ[fc],
      Return @ failure[
        "NonNumericMidpointValue",
        "Function value at midpoint is not numeric.",
        <|"c" -> c, "f[c]" -> fc, "Iteration" -> iter|>
      ]
    ];

    If[Abs[fc] <= tol || Abs[b - a] <= tol,
      converged = True;
      Return @ <|
        "Root" -> c,
        "Residual" -> fc,
        "Iterations" -> iter,
        "Converged" -> converged,
        "Method" -> "Bisection",
        "Bracket" -> {a0, b0},
        "FinalBracket" -> {a, b},
        "BracketWidth" -> Abs[b - a],
        "History" -> If[storeHistory, history, Missing["NotStored"]]
      |>
    ];

    If[fa fc < 0,
      b = c;
      fb = fc,
      a = c;
      fa = fc
    ];
  ];

  Message[BisectionRoot::maxiter];
  failure[
    "MaxIterationsExceeded",
    "Bisection did not converge within the allowed number of iterations.",
    <|
      "Bracket" -> {a0, b0},
      "FinalBracket" -> {a, b},
      "Iterations" -> iter,
      "LastResidualEstimate" -> fc,
      "History" -> If[storeHistory, history, Missing["NotStored"]]
    |>
  ]
];

(* ========================================= *)
(* Root finder specialized to the model      *)
(* ========================================= *)

Options[FindRadiusRoot] = {
  "Tolerance" -> Automatic,
  "MaxIterations" -> Automatic,
  "WorkingPrecision" -> Automatic,
  "StoreHistory" -> False
};

FindRadiusRoot[r_?NumericQ, br_, params_Association, OptionsPattern[]] := Module[
  {
    validated, tol, maxIter, wp, storeHistory,
    f, result
  },

  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated],
    Message[FindRadiusRoot::badparams];
    Return[validated];
  ];

  If[!numericBracketQ[br],
    Message[FindRadiusRoot::badbracket];
    Return @ failure[
      "InvalidRadiusBracket",
      "Radius bracket must be a numeric pair {RLeft,RRight} with RLeft < RRight}.",
      <|"Bracket" -> br|>
    ];
  ];

  tol = Replace[OptionValue["Tolerance"], Automatic -> validated["epsBis"]];
  maxIter = Replace[OptionValue["MaxIterations"], Automatic -> validated["maxBisIter"]];
  wp = Replace[OptionValue["WorkingPrecision"], Automatic -> validated["workingPrecision"]];
  storeHistory = TrueQ[OptionValue["StoreHistory"]];

  f = Function[{R},
    CVTPulley`BeltLength`BeltResidualFromParameters[r, R, validated]
  ];

  result = BisectionRoot[
    f,
    br,
    "Tolerance" -> tol,
    "MaxIterations" -> maxIter,
    "WorkingPrecision" -> wp,
    "StoreHistory" -> storeHistory
  ];

  If[FailureQ[result], Return[result]];

  Join[
    result,
    <|
      "InputRadius" -> r,
      "RadiusBracket" -> br
    |>
  ]
];

End[];
EndPackage[];