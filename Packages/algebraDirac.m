(* ::Package:: *)

BeginPackage["algebraDirac`", {"basis`"}]

NonCommutativeMultiply::usage="It applies to nc head only. The rules are defined for 2 input arguments, with Plus or nc head.";

nonCommutativeHead::usage="nonCommutativeHead[f] defines f as a noncommutative function.";

mergeArguments::usage="mergeArguments[G_][G[], a G[1], b G[2,3], ...] recursively applies NonCommutativeMultiply and does the contraction for the head declared in nonCommutativeHead. 
Example:  nc[G][G[1], G[1, 2], G[3]] = G[1, 1, 2, 3]
Note that one can use G[] as the identity element of the basis G[x].";

productGammas::usage="productGammas[sign][1,2,...] evaluates a product of flat Dirac gamma matrices, where sign=0 uses (+,+,+,...), sign=-1 uses (-,+,+,...) and sign=1 uses (+,-,-,...). 
It is used in evalGammas. Note that it's NOT the antisymmetrized product of gamma matrices, i.e. productGammas[0][1,1] is \!\(\*SubscriptBox[\(G\), \(1\)]\)\!\(\*SubscriptBox[\(G\), \(1\)]\) and not \!\(\*SubscriptBox[\(G\), \(1, 1\)]\), which is 0.";

evalGammas::usage = "evalGammas[signature][G][exprs], where signature is the signature of the metric (1 for mostly minus, -1 for mostly plus and 0 for Euclidean), and exprs are expressions we want to contract, separated by commas. \
Example:  evalGammas[0][G][G[1], G[1, 2], G[3]] = G[2, 3] ";  

commuteGammas::usage = "commuteGammas[G1, G2] returns G2**G1 with the appropiate sign. Note that it's signature independent.";

Begin["`Private`"]
Needs["basis`"]
Needs["commons`"]


Unprotect[NonCommutativeMultiply];

a_**0 = 0**a_ = 0;
a_**1 = 1**a_ := a;

nonCommutativeHead[nc_]:= Module[{},
 (* Distribution over Plus *)
  NonCommutativeMultiply[a_Plus,nc[x___]]:=Plus@@(NonCommutativeMultiply[#,nc[x]]&/@Expand@a);
  NonCommutativeMultiply[nc[x___],b_Plus]:=Plus@@(NonCommutativeMultiply[nc[x],#]&/@Expand@b);
  NonCommutativeMultiply[a_Plus,f__ nc[x___]]:=f Plus@@(NonCommutativeMultiply[#,nc[x]]&/@Expand@a);
  NonCommutativeMultiply[f__ nc[x___],b_Plus]:=f Plus@@(NonCommutativeMultiply[nc[x],#]&/@Expand@b);
  NonCommutativeMultiply[a_Plus,b_Plus]:= Module[{l1, temp},
     l1=List@@Expand@a;
     temp=Table[l1[[i]]**Expand@b,{i,1,Length@l1}]//Flatten;
    (Plus@@temp)//Expand 
    ];

  NonCommutativeMultiply[f__  nc[x___],nc[y___]]:=f NonCommutativeMultiply[nc[x],nc[y]];
  NonCommutativeMultiply[ nc[x___],f__ nc[y___]]:=f NonCommutativeMultiply[nc[x],nc[y]];
  NonCommutativeMultiply[f__ nc[x___],g__ nc[y___]]:=f g NonCommutativeMultiply[nc[x],nc[y]];
];



mergeArguments[G_][x___]:=mergeArguments[G][
  mergeArguments[G]@@Most@{x},
  Last@{x}
]
mergeArguments[G_][x_, y_]:=Module[{a,b},
  Expand[NonCommutativeMultiply[x,y]/. G[a___]**G[b___]->G[a,b]]
]



productGammas[signature_Integer/; -1 <= signature <= 1][] = 1; 
productGammas[signature_Integer/; -1 <= signature <= 1][b___,a_,c___,a_,d___] := Module[{l1, l2, g},
  l1={c};
  l2={b,c,d};
  g=productGammas[signature]@@l2;
  Which[
    signature==0, If[EvenQ[Length@l1]==True,            g,           -g],
    a==1,         If[EvenQ[Length@l1]==True,  signature*g, -signature*g],
    True,         If[EvenQ[Length@l1]==True, -signature*g,  signature*g]
  ]
]


evalGammas[signature_Integer/; -1 <= signature <= 1][gamma_][x_]:= x /.gamma :> productGammas[signature]/. productGammas[signature] -> antisym[gamma]/. gamma[]->1 

evalGammas[signature_Integer/; -1 <= signature <= 1][gamma_][x___]:= evalGammas[signature][gamma][
  evalGammas[signature][gamma]@@Most@{x},
  Last@{x}
]

evalGammas[signature_Integer/; -1 <= signature <= 1][gamma_][x_, y_]:= Expand[
	NonCommutativeMultiply[
		Expand@addUnitBasis[gamma][x]
		,
		Expand@addUnitBasis[gamma][y]
	]/.gamma[a___]**gamma[b___] :>  productGammas[signature][a,b]/. productGammas[signature] -> antisym[gamma]/. gamma[]->1  
  ]

commuteGammas[G_[x___], G_[y___]]:= G[y]**G[x] evalGammas[0][G][G[x], G[y]]/evalGammas[0][G][G[y], G[x]]  (*signature independent*)


End[]

EndPackage[]


(*

(* Old code, slower for evalGammas since mergeArguments carries all the arguments of gamma *)

mergeArguments[gamma_][x___]:=mergeArguments[gamma][
  mergeArguments[gamma]@@Most@{x},
  Last@{x}
]
mergeArguments[gamma_][x_, y_]:=Module[{a,b},
  Expand[NonCommutativeMultiply[x,y]/.gamma[a___]**gamma[b___] -> gamma[a,b]]
]
mergeArguments::usage="mergeArguments[gamma_][gamma[], a gamma[1], b gamma[2,3], ...] recursively applies NonCommutativeMultiply and does the contraction for the head declared in nonCommutativeHead. \
Note that one can use gamma[] as the identity element of the basis gamma[x].";

evalGammas[signature_Integer/; -1 <= signature <= 1][gamma_][exprs___] := Module[{exprId, gammaRule1, gammaRule2},
  	exprId = addUnitBasis[gamma] /@ {exprs};
  	gammaRule1 = gamma[x__] :> productGammas[signature][x];
  	gammaRule2 = productGammas[signature] -> antisym[gamma];
    Apply[mergeArguments[gamma], exprId] /. gammaRule1 /. gammaRule2 
  ]

*)
