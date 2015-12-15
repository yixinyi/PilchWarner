(* ::Package:: *)

Unprotect[NonCommutativeMultiply];

a_**0=0**a_=0;
a_**1=1**a_=a;

nonCommutativeHead[nc_]:=Module[{},
 (*Distribution over Plus *)
  NonCommutativeMultiply[a_Plus,nc[x___]]:=Plus@@(NonCommutativeMultiply[#,nc[x]]&/@Expand@a);
  NonCommutativeMultiply[nc[x___],b_Plus]:=Plus@@(NonCommutativeMultiply[nc[x],#]&/@Expand@b);
  NonCommutativeMultiply[a_Plus,f__ nc[x___]]:=f Plus@@(NonCommutativeMultiply[#,nc[x]]&/@Expand@a);
  NonCommutativeMultiply[f__ nc[x___],b_Plus]:=f Plus@@(NonCommutativeMultiply[nc[x],#]&/@Expand@b);
  NonCommutativeMultiply[a_Plus,b_Plus]:=Module[{l1, temp},
  l1=List@@Expand@a;
  temp=Table[l1[[i]]**Expand@b,{i,1,Length@l1}]//Flatten;
  (Plus@@temp)//Expand
];

NonCommutativeMultiply[f__  nc[x___],nc[y___]]:=f NonCommutativeMultiply[nc[x],nc[y]];
NonCommutativeMultiply[ nc[x___],f__ nc[y___]]:=f NonCommutativeMultiply[nc[x],nc[y]];
NonCommutativeMultiply[f__ nc[x___],g__ nc[y___]]:=f g NonCommutativeMultiply[nc[x],nc[y]];
NonCommutativeMultiply::usage="It applies to nc head only. The rules are defined for 2 input arguments, with Plus or nc head.";
];
nonCommutativeHead::usage="nonCommutativeHead[f] defines f as a noncommutative function.";


ncContract[\[CapitalGamma]_][x___]:=ncContract[\[CapitalGamma]][
  ncContract[\[CapitalGamma]]@@Most@{x},
  Last@{x}
]
ncContract[\[CapitalGamma]_][x_, y_]:=Module[{a,b},
  Expand[NonCommutativeMultiply[x,y]/.\[CapitalGamma][a___]**\[CapitalGamma][b___]->\[CapitalGamma][a,b]]
]
ncContract::usage="ncContract[\[CapitalGamma]_][\[CapitalGamma][], a \[CapitalGamma][1], b \[CapitalGamma][2,3], ...] recursively applies NonCommutativeMultiply and does the contraction for the head declared in nonCommutativeHead. 
Note that one can use \[CapitalGamma][] as the identity element of the basis \[CapitalGamma][x].";

gammaContract[signature_Integer/; -1 <= signature <= 1][] = 1; 
gammaContract[signature_Integer/; -1 <= signature <= 1][b___,a_,c___,a_,d___] := Module[{l1, l2, g},
  l1={c};
  l2={b,c,d};
  g=gammaContract[signature]@@l2;
  Which[
    signature==0, If[EvenQ[Length@l1]==True,            g,           -g],
    a==1,         If[EvenQ[Length@l1]==True,  signature*g, -signature*g],
    True,         If[EvenQ[Length@l1]==True, -signature*g,  signature*g]
  ]
]
gammaContract::usage="gammaContract[sign][1,2,...] means the product of Dirac gamma matrices, where sign=0 uses (+,+,+,...), sign=-1 uses (-,+,+,...) and sign=1 uses (+,-,-,...). ";
