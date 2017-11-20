(* ::Package:: *)

BeginPackage["basis`", {"commons`"}]

basis::usage = 
  "basis[gamma][expr]. Example: expr = a + b \
gamma[1] + c gamma[1, 2] + ..., it will extract {\
gamma[1], gamma[1,2]}.
  Use addUnitBasis[gamma][expr] to add gamma[] to \
coefficient a. ";

basisSort::usage = 
  "basisSort[gamma][expr] assumes gamma has antisymmetric indices and sorts them, e.g. gamma[2,1] = -gamma[1,2], gamma[1,1,..]= 0 ";

basisCollect::usage = "basisCollect[gamma][expr] collects the gamma heads of expr. 
basisCollect[gamma, function][expr] applies function to the coefficients.";

basisCoefRule::usage = 
  "basisCoefRule[gamma][expr], where expr contains \
gamma, will return: {basis1 -> coefficient1, ...}. If \
there's no unit basis, it will be named as Identity.";

addUnitBasis::usage = 
  "addUnitBasis[gamma][expr] adds unity, which is \
represented by gamma[] to expr if it doesn't have it. This \
helps to treat all the terms of the expression at the same footing \
under, for example, NonCommutativeMultiply.";




Begin["`Private`"]
Needs["commons`"]


basis[gamma_][expr_?AtomQ] := {}
basis[gamma_][expr_] := If[
  ToString@Head@expr == ToString@gamma, 
  {expr},
  DeleteDuplicates@Cases[
     Apply[
        List, expr /. {Plus -> List, Times -> List}//Flatten
          ], 
     _gamma]
  ]

sort[f_[]] := f[]; (*f[] as a unit basis*)
sort[f_[x__]] := Signature[{x}] f @@ Sort[{x}]
sort[g__ f_[x__]] := g Signature[{x}] f @@ Sort[{x}]
sort::usage = 
  "It sorts the argument of a function and multiplies it the \
signature.";


basisSort[gamma_][x_] := Module[{base, rule, z},
  base = basis[gamma][x];
  rule = Table[
    base[[i]] -> (sort /@ base)[[i]], {i, 1, Length@base}];
  z = x /. rule
  ]

basisCollect[gamma_][x_] := Collect[x, basis[gamma][x]]
basisCollect[gamma_,function_][x_] := Collect[x, basis[gamma][x], function]

addUnitBasis[gamma_][expr__] := Module[{coefId, x, gammapart},
  x = expr;
  coefId = x /. gamma -> zero;
  If[(coefId == 0) /. Equal[x__, y_] -> False,
   expr,
   gammapart = basisCollect[gamma][x - coefId];
   coefId*gamma[] + gammapart
   ]
  ]  

basisCoefRule[gamma_][expr_] := Module[{new, base},
  new = addUnitBasis[gamma]@expr;
  base = basis[gamma][new];
  listsToRule[base, Coefficient[new, base]]
  ]




End[]

EndPackage[]


(*
simplifyCoef[gamma_][expr_] := Module[{base},
  base = basis[gamma][expr];
  Collect[expr, base, Simplify]
simplifyCoef::usage =
   "simplifyCoef[gamma_][expr_] simplifies the coefficient of a basis with the head gamma.";
  ]
*)
