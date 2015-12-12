(* ::Package:: *)

(************************************************************************)
(* This file was generated automatically by the Mathematica front end.  *)
(* It contains Initialization cells from a Notebook file, which         *)
(* typically will have the same name as this file except ending in      *)
(* ".nb" instead of ".m".                                               *)
(*                                                                      *)
(* This file is intended to be loaded into the Mathematica kernel using *)
(* the package loading commands Get or Needs.  Doing so is equivalent   *)
(* to using the Evaluate Initialization Cells menu command in the front *)
(* end.                                                                 *)
(*                                                                      *)
(* DO NOT EDIT THIS FILE.  This entire file is regenerated              *)
(* automatically each time the parent Notebook file is saved in the     *)
(* Mathematica front end.  Any changes you make to this file will be    *)
(* overwritten.                                                         *)
(************************************************************************)



metric[sign_Integer/;sign==1||sign==-1||sign==0][vielb_List]:=Module[{dim,ans,\[Eta]},
dim=Length[vielb];\[Eta]=IdentityMatrix[dim];\[Eta][[1,1]]=-1;If[sign!=0,minkMetric=-sign \[Eta];ans=Transpose[vielb].minkMetric.vielb//FullSimplify,
ans=Transpose[vielb].vielb//FullSimplify
];
ans]
metric::usage="First argument: 1 -> (+, -, -, ...), -1 -> (-, +, +, ...), 0-> Euclidean. Second argument: vielbein as a matrix. It also returns minkMetric in the specified convention.";

induced[matrix_List,coord_List,inducedCoord_List,pullback_List]:=Module[
{pullbackRule=listsToRule[coord, pullback]},
Transpose[Grad[pullback,inducedCoord]].(matrix/.pullbackRule).Grad[pullback,inducedCoord]
]
induced::usage="The inputs are: the coordinates, the induced coordinates and the pullback of the coordinates.";

hodgeDual[component_,position_List, metric_List]:=Module[{dim,len,posHX,invMetric,ans},
dim=Length@metric;
len=Length@position;
posHX=Complement[Range[dim],position];
invMetric=Inverse@metric;

(* The combinatoric factor 1/(dim-len)! is not needed because we return a specific component. *)
ans=Times[Sqrt[Abs[Det@metric]],
Times@@(invMetric[[#,#]]&/@position),
Signature@Join[position,posHX](*this plays the Role of Levi-Civita symbol*),
component]/.Abs->Identity (*Abs has already removed the explicit minus*);
{ans,posHX}
]
hodgeDual::usage="(*A\!\(\*SubscriptBox[\()\), \(\*SubscriptBox[\(\[Mu]\), \(1\)] \[Ellipsis] \*SubscriptBox[\(\[Mu]\), \(n\)]\)]\)\[Congruent]\!\(\*FractionBox[\(1\), \(p!\)]\)\!\(\*SqrtBox[\(\(|\)\(g\)\(|\)\)]\)\!\(\*SubscriptBox[\(\[Epsilon]\), \(\*SubscriptBox[\(\[Mu]\), \(1\)] \[Ellipsis] \*SubscriptBox[\(\[Mu]\), \(\(n\)\(-\)\(p\)\(\\\ \)\)] \*SubscriptBox[\(\[Mu]\), \(n - p + 1\)] \[Ellipsis] \*SubscriptBox[\(\[Mu]\), \(n\)]\)]\) \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(n - p + 1\)] \*SubscriptBox[\(\[Nu]\), \(1\)]\)]\)\[Ellipsis] \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(n\)] \*SubscriptBox[\(\[Nu]\), \(p\)]\)]\) \!\(\*SubscriptBox[\(A\), \(\*SubscriptBox[\(\[Nu]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Nu]\), \(p\)]\)]\), where \!\(\*SubscriptBox[\(\[Epsilon]\), \(\(123 ... \) n\)]\) = 1. 
Note that if we perform a parity transformation like \!\(\*SuperscriptBox[\(x\), \(1\)]\) -> -\!\(\*SuperscriptBox[\(x\), \(1\)]\), the new \!\(\*SubscriptBox[\(\[Epsilon]\), \(\(123 ... \) n\)]\) = -1. This case is not considered here. ";


christoffelS[X_?ListQ, G_?ListQ]:=Module[{dim,invMetric,array},
dim=Length@X;invMetric=Inverse[G];
array=ConstantArray[0,{dim,dim,dim}];
Table[
array[[\[Lambda],\[Mu],\[Nu]]]=Sum[1/2 invMetric[[\[Lambda],\[Rho]]](D[G[[\[Rho],\[Nu]]],X[[\[Mu]]]]+D[G[[\[Rho],\[Mu]]],X[[\[Nu]]]]-D[G[[\[Mu],\[Nu]]],X[[\[Rho]]]]),{\[Rho],1,dim}];
(* Use the symmetric property of \[Mu] and \[Nu] indices *)
array[[\[Lambda],\[Nu],\[Mu]]]=array[[\[Lambda],\[Mu],\[Nu]]],
{\[Lambda],1,dim},{\[Mu],1,dim},{\[Nu],\[Mu],dim}]//FullSimplify;
array
]
christoffelS::usage="christoffelS[coord, metric]. Notation Christoffel Symbols:\!\(\*FormBox[\(TraditionalForm\`\(\(\\\ \)\(\*SubscriptBox[SuperscriptBox[\(\[CapitalGamma]\), \(1\)], \(23\)] = \[CapitalGamma][\([1, 2, 3]\)]\)\)\),
TraditionalForm]\)";


spinConnection[X_?ListQ, G_?ListQ, ee_?ListQ]:=Module[{dim,iee, \[CapitalGamma]},
dim=Length@X;iee=Inverse[ee];
\[CapitalGamma]=christoffelS[X, G];
 (* ee[[a,\[Mu]]] \[Equal] \!\(
\*SubsuperscriptBox[\(E\), \(\[Mu]\), \(a\)]\ and\ inverse\ \(iee[\([\)\(\[Mu], a\)\(]\)]\)\) \[Equal] Subsuperscript[E, a, \[Mu]] *)
Table[
Sum[ee[[a,\[Nu]]]iee[[\[Lambda],b]]\[CapitalGamma][[\[Nu],\[Mu],\[Lambda]]],{\[Lambda],1,dim},{\[Nu],1,dim}]
-Sum[iee[[\[Lambda],b]]D[ee[[a,\[Lambda]]],X[[\[Mu]]]],{\[Lambda],1,dim}],{\[Mu],1,dim},
{a,1,dim},{b,1,dim}
]//FullSimplify
]
spinConnection::usage="spinConnection[coord, metric, vierbein]'s definition follows Sean Carroll's book (eq. J21). It uses christoffelS[] function.
Notation: \!\(\*FormBox[\(TraditionalForm\`\(\(\\\ \)\(\*SubsuperscriptBox[\(\[CapitalOmega]\), \(\[Mu]\\\ b\), \(a\)] = \[CapitalOmega][\([\[Mu], a, b]\)]\)\)\),
TraditionalForm]\). Note that for its lower indices version: \!\(\*SubscriptBox[\(\[CapitalOmega]\), \(\[Mu]\\\ a\\\ b\)]\) = -\!\(\*SubscriptBox[\(\[CapitalOmega]\), \(\[Mu]\\\ b\\\ a\)]\)";


riemannT[coord_List,metric_List]:=Module[{cs,dim,array},
dim=Length@coord;
cs=christoffelS[coord,metric];
array=ConstantArray[0,{dim,dim,dim,dim}];
Table[
array[[r,s,m,n]]=D[cs[[r,n,s]],coord[[m]]]-D[cs[[r,m,s]],coord[[n]]]+Sum[cs[[r,m,l]]cs[[l,n,s]]-cs[[r,n,l]]cs[[l,m,s]],{l,1,dim}];
(* use the antisymmetric property of m and n *)
array[[r,s,n,m]]=-array[[r,s,m,n]],
{r,1,dim},{s,1,dim},{m,1,dim},{n,m+1,dim} ];
array
]
riemannT::usage = "riemannT[coord, metric][[\[Rho],\[Sigma],\[Mu],\[Nu]]] = \!\(\*SubscriptBox[SuperscriptBox[\(R\), \(\[Rho]\)], \(\[Sigma]\\\ \[Mu]\\\ \[Nu]\)]\)";

ricciT[coord_List,metric_List]:=Module[{riemann,dim,array},
dim=Length@coord;
array=ConstantArray[0,{dim,dim}];
riemann=riemannT[coord,metric];
Table[
array[[m,n]]=Sum[riemann[[r,m,r,n]],{r,1,dim}];
(* use the symmetric property of m and n *)
array[[n,m]]=array[[m,n]],
{m,1,dim},{n,m,dim}];
array
]
ricciT::usage = "ricciT[coord, metric][[\[Mu],\[Nu]]] = \!\(\*SubscriptBox[\(R\), \(\(\\\ \)\(\[Mu]\\\ \[Nu]\)\)]\) = \!\(\*SubscriptBox[SuperscriptBox[\(R\), \(\[Rho]\)], \(\[Mu]\\\ \[Rho]\\\ \[Nu]\)]\)";

ricciScalar[coord_List,metric_List]:=Module[{invMetric},
invMetric=Inverse@metric;
Tr[invMetric.ricciT[coord,metric]]
]
ricciScalar::usage = "The trace of the Ricci tensor: \!\(\*SuperscriptBox[\(g\), \(\[Mu]\\\ \[Nu]\)]\) \!\(\*SubscriptBox[\(R\), \(\(\\\ \)\(\[Mu]\\\ \[Nu]\)\)]\)";



formSkeleton[name_][listOflists_List][i__]:=Module[{input,sorted,slist},
input={i}; (* input component *)
sorted=Sort@input;
slist=Sort/@listOflists;(* sort the list of non-vanishing components *)
If[
Or@@(Equal[sorted,#]&/@slist),
Signature[input]HoldForm@name@@sorted,
0
]
]
formSkeleton::usage="It returns the HoldForm of the component of the name. The non-vanishing component is specified by listOflists.";


Unprotect[NonCommutativeMultiply];
a_**0=0**a_=0;
a_**1=1**a_=a;

nonCommutativeHead[nc_]:=Module[{},
(* Distribution over Plus *)
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
nonCommutativeHead::usage="Defines the head of the noncommutative function";



Clear[antisym,gammaContract,ncContract,basis,basisSort]

ncContract[\[CapitalGamma]_][x___]:=ncContract[\[CapitalGamma]][
ncContract[\[CapitalGamma]]@@Most@{x},
Last@{x}
]
ncContract[\[CapitalGamma]_][x_,y_]:=Module[{a,b},
Expand[NonCommutativeMultiply[x,y]/.\[CapitalGamma][a___]**\[CapitalGamma][b___]->\[CapitalGamma][a,b]]
]
ncContract::usage="Recursively applies NonCommutativeMultiply and does the contraction for the head declared in nonCommutativeHead. Note that one can use \[CapitalGamma][] as the identity element of the basis \[CapitalGamma][x].";

antisym[\[CapitalGamma]_][x__]:=Module[{list,sortedList},
list={x};
sortedList=Sort[list];
Signature[list] \[CapitalGamma]@@sortedList
];
antisym::usage="It builds a function antisymmetric in its indices. The head of the antisymmetric function must be especified.";

gammaContract[signature_Integer/;-1<= signature<= 1][b___,a_,c___,a_,d___]:=Module[{l1, l2,g},
l1={c};
l2={b,c,d};
g=gammaContract[signature]@@l2;
Which[
signature==0,If[EvenQ[Length@l1]==True,g, - g],
a==1,If[EvenQ[Length@l1]==True,signature*g, -signature*g],
True,If[EvenQ[Length@l1]==True,-signature*g, signature*g]
]
]
gammaContract::usage="gammaContract[sign][1,2,...] means the product of Dirac gamma matrices, where sign=0 uses (+,+,+,...), sign=-1 uses (-,+,+,...) and sign=1 uses (+,-,-,...). ";
gammaContract[signature_?IntegerQ/;-1<= signature<= 1][]=1; 


zero[x___]:=0;

basis[\[CapitalGamma]_][x_]:=Flatten[
Cases[#,_\[CapitalGamma]]& @(List@@x/.{Plus->List,Times->List}//Flatten)
]
basis::usage = "It looks for functions with the specified head. It works only for level 1 expressions.";

sort[f_[]]:=1; (*f[] as a unit basis*)
sort[f_[x__]]:=Signature[{x}]f@@Sort[{x}]
sort[g__ f_[x__]]:=g Signature[{x}]f@@Sort[{x}]
sort::usage="It sorts the argument of a function and multiplies it the signature.";

basisSort[\[CapitalGamma]_][x_]:=Module[{base,rule,y,z},
y=Expand[x];
base=basis[\[CapitalGamma]][y];
rule=Table[base[[i]]->(sort/@base)[[i]],{i,1,Length@base}];
z=y/.rule;
Collect[z,basis[\[CapitalGamma]][z]]
]
basisSort::usage="It collects the expression according to its basis (i.e. function with the specified head). ";

baseCoefForm[\[CapitalGamma]_][expr_]:=Module[{new, base},
new=basisSort[\[CapitalGamma]][expr];
base=basis[\[CapitalGamma]][new];
listsToRule[
Prepend[base,Identity], 
Prepend[Coefficient[new,base],new/.\[CapitalGamma]-> nil]
]
]
baseCoefForm::usage="For the given head of the basis and the expression, it will return: {basis1 -> coefficient1, ...}, Unit basis, named as Identity, is included.";

addUnitBasis[\[CapitalGamma]_][x__]:=Module[{coefId},
coefId=x/.\[CapitalGamma]->nil;
coefId*\[CapitalGamma][]-coefId +x
]
addUnitBasis::usage="For basis with head \[CapitalGamma], it adds unity, which is represented by \[CapitalGamma][]. This helps to treat all the terms of the expression at the same footing under, for example, NonCommutativeMultiply.";


blockDiagonalMatrix[a_?ArrayQ,b_?ArrayQ]:=Module[
{la=Length@a, lb=Length@b,l,M},
l=la+lb;
M=ConstantArray[0,{l,l}];
Table[M[[i,j]]=a[[i,j]],{i,1,la},{j,1,la}];
Table[M[[i+la,j+la]]=b[[i,j]],{i,1,lb},{j,1,lb}];
M
]

listsToRule[l1_List,l2_List]:=Module[{l},
l=Length@l1;
(l1[[#]]->l2[[#]])&/@Range[l]
]
listsToRule::usage=" Rule applied elementwise for two lists.";

listsToRuleBox[list_List]:=listsToRule[list,Table[\[Placeholder],Range[10]]];
listsToRuleBox::usage="It helps to define a list of rules output of an input list."

toSuperscriptRule[\[CapitalGamma]_]:={\[CapitalGamma][x__]-> Superscript[\[CapitalGamma],{x}]};
toSubscriptRule[\[CapitalGamma]_]:={\[CapitalGamma][x__]-> Subscript[\[CapitalGamma],{x}]};


poincarePatch[n_][x_,z_]:=Module[{coord,vierb},
coord={ToExpression@Table[ToString@x<> ToString[i],{i,1,n-1}],z}//Flatten;
vierb=DiagonalMatrix@Table[1/z,{i,1,n}];
{coord,vierb}
]
horosphericAdS[n_][x_,r_]:=Module[{coord,vierb},
coord={ToExpression@Table[ToString@x<> ToString[i],{i,1,n-1}],r}//Flatten;
vierb=DiagonalMatrix[Append[Table[E^r,{i,1,n-1}],1]];
{coord,vierb}
]


spherical[n_][\[Phi]_]:=Module[{coord,vierb},
coord=Table[ToExpression[ToString[\[Phi]]<>ToString[i]],{i,1,n}];

vierb=DiagonalMatrix@Table[
Product[Sin[ToExpression[ToString[\[Phi]]<>ToString[k]]],{k,1,i-1}],
{i,1,n}
];
{coord,vierb}
]

volSph[n_]:=\[Pi]^(n/2)/Gamma[n/2+1]


poincareSph[x_,z_,\[Theta]_]:=Module[{coord1, vierb1, vierb1Matrix,coord, vierb},
Clear[ee];
{coord1,vierb1Matrix}=poincarePatch[5][x,z];
coord=Join[
coord1,
{\[Theta]},
ToExpression@Table[ToString@x<> ToString[i],{i,7,10}]];
vierb1=(Most@ArrayRules[vierb1Matrix])[[;;,2]];

Print[Style["Vierbeins of S4 sphere: ee ", FontColor->Red]];
vierb=DiagonalMatrix@Join[
vierb1,
{1},
Table[ Sin[\[Theta]]ee[i],{i,7,10}](* ee are the vierbeins of S4 sphere *)
] ;
{coord, vierb}
]

poincareHopf[x_,z_,\[Theta]_,\[Phi]_]:=Module[{coord1, vierb1, vierb1Matrix, coord, vierb},
Clear[ee];
{coord1,vierb1Matrix}=poincarePatch[5][x,z];
coord=Join[
coord1,
{\[Theta]},
ToExpression@Table[ToString@x<> ToString[i],{i,7,9}],
{\[Phi]}];
vierb1=(Most@ArrayRules[vierb1Matrix])[[;;,2]];

Print[Style["Vierbeins of S3 sphere: ee ", FontColor->Red]];
vierb=DiagonalMatrix@Join[
vierb1,
{1},
Table[ Cos[\[Theta]]ee[i],{i,7,9}],(* ee are the vierbeins of S3 sphere *)
{Sin[\[Theta]]}
] ;
{coord, vierb}
]


