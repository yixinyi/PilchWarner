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



AppendTo[$Path,ToFileName[{NotebookDirectory[]}]];
<<matrixEDC.m
<<algebraDirac`


lagrWZ[coord_List][gauge_,potentials_List]:= Module[{dim,temp},
dim=Length@coord;
temp=Sum[1/n! wedgePower[gauge,n],{n,0,dim+1}]\[Wedge]Plus@@potentials;
projectDim[temp,dim]
]


partition[dim_,n_]:=Module[{perm,ans},perm=Sort/@Permutations[Range[dim],{n}];
ans=DeleteDuplicates@perm](*It's inefficient*)


wedgePower[G_,n_/;n>=1]:=wedgePower[G,n-1]\[Wedge]G
wedgePower[G_,0]:=1
(*Alternative definition:*)
(*wedgePower[G_,n_/;n\[GreaterEqual]1]:= Wedge@@Table[G,{i,1,n}]*)
wedgeForm[coord_List][coef_List, base_List]:=coef.(Wedge@@d[Part[coord,#]]&/@base)
wedgeForm::usage="wedgeForm[coord][coefList, basisNumericalList] constructs the form with wedge products, e.g. f d[x]\[Wedge]d[y] + g d[x]\[Wedge]d[z] ";

formConstruct[coord_List][\[Gamma]_,0]:=\[Gamma][0]
formConstruct[coord_List][\[Gamma]_,n_]:=Module[{components,dim},
dim=Length@coord;
components=partition[dim,n];
wedgeForm[coord][\[Gamma]@@@components,components]
]
formConstruct::usage="formConstruct[coord][name, dim] constructs a form of given name and dimension from partitions of the dimension of the manifold specified by coord. It uses wedgeForm[]. ";


Clear[\[Gamma],\[CapitalGamma],\[ScriptCapitalK]];
nonCommutativeHead[\[Gamma]];nonCommutativeHead[\[CapitalGamma]];
Protect[\[Gamma],\[CapitalGamma],\[ScriptCapitalK]];


getDimBasis[expr_]:=Module[{base, dimList},
base=basis[Wedge][expr];
dimList=Count[#,_d]&/@base;
listsToRule[base, dimList]
]
getDimBasis::usage="getDimBasis[expr] returns a list of type {d[x]\[Wedge]d[y] -> 2, ... }.";

projectDim[expr_,0]:=expr/.Wedge->zero
projectDim[expr_,dim_]:=Module[{projected},
projected=Select[Association@getDimBasis[expr],#==dim &];
Coefficient[expr,Keys@projected].Keys[projected]
]
projectDim::usage="projectDim[expr, dim] selects only the term with given dim from expr.";

dBraneProj[coord_List][Fcoef_List, Fbasis_List]:=Module[{dim,Fform, \[Gamma]Form, expr,\[Xi]},
dim=Length@coord;
(* Brute force approach: build the most general Subscript[\[Gamma], (n)], contract it with \[ExponentialE]^F and project to the dimension we want *)
\[Gamma]Form[n_]:=formConstruct[coord][\[Gamma],n];
Fform=wedgeForm[coord][Fcoef, Fbasis];
expr=Sum[1/n! wedgePower[Fform,n],{n,0,dim+1}]\[Wedge]Sum[\[Gamma]Form[n]\[ScriptCapitalK]^Mod[n/2,2],{n,0,dim,2}]; 
projectDim[expr,dim]/.listsToRule[d[coord],d/@Array[\[Xi],dim]]/.Wedge[x__]-> 1/.\[CapitalGamma]-> antisym[\[CapitalGamma]]
(* The names of the coordinates are replaced to an ordered array, in order to sort the basis. Then, I make the basis disappear in the last replacement rule *)
]
dBraneProj::usage="dBraneProj[coord][Fcoef, Fbasis] returns the STRIPPED, i.e. no prefactor (-1/LDBI/exp(Dilaton)), nor \[ScriptCapitalI] in the rightmost side, kappa symmetry projector of the Dbrane specified by coord with field F = {coef_List, basis_List}. \[ScriptCapitalK]\[Psi] = \!\(\*SuperscriptBox[\(\[Psi]\), \(*\)]\) and \[ScriptCapitalI]\[Psi] = -i\[Psi], where \[Psi] is a spinor. See Skenderis-Taylor hep-th/0204054.";



\[Gamma]Components[signature_][vierbein_,coord_,inducedCoord_,pullback_][i__]:=Module[{l,list,selectComponents},
(*Subscript[\[Gamma], (n)] = 1/n!Subscript[\[Gamma], {Subscript[a, 1],...,Subscript[a, n]}](d\[Xi]^Subscript[a, 1])\[Wedge]...\[Wedge]d\[Xi]^Subscript[a, n]*)
l=Length@coord;
list=Transpose[Grad[pullback,inducedCoord]].Array[\[Gamma],l];
selectComponents=list[[#]]&/@{i};
mergeArguments[\[Gamma]]@@selectComponents/.\[Gamma]->antisym[\[Gamma]]
]
\[Gamma]Components[signature_][vierbein_,coord_,inducedCoord_,pullback_][i_]:=1;
\[Gamma]Components::usage="\[Gamma]Components[signature][vielb, coord, inducedCoord, pullback][i__] returns \!\(\*SubscriptBox[\(\[Gamma]\), \({\*SubscriptBox[\(a\), \(1\)],  ... , \*SubscriptBox[\(a\), \(n\)]}\)]\) = \!\(\*SubscriptBox[\(\[PartialD]\), SubscriptBox[\(a\), \(1\)]]\)\!\(\*SuperscriptBox[\(X\), SubscriptBox[\(\[Mu]\), \(1\)]]\)...\!\(\*SubscriptBox[\(\[PartialD]\), SubscriptBox[\(a\), \(n\)]]\)\!\(\*SuperscriptBox[\(X\), SubscriptBox[\(\[Mu]\), \(n\)]]\) \!\(\*SubscriptBox[\(\[Gamma]\), \(\(\*SubscriptBox[\(\[Mu]\), \(1\)] ... \) \*SubscriptBox[\(\[Mu]\), \(n\)]\)]\), where \!\(\*SubscriptBox[\(\[Gamma]\), \(\(\*SubscriptBox[\(\[Mu]\), \(1\)] ... \) \*SubscriptBox[\(\[Mu]\), \(n\)]\)]\) are curved space gamma matrices. 
\!\(\*SubscriptBox[\(\[Gamma]\), \((n)\)]\) = \!\(\*FractionBox[\(1\), \(n!\)]\)\!\(\*SubscriptBox[\(\[Gamma]\), \({\*SubscriptBox[\(a\), \(1\)],  ... , \*SubscriptBox[\(a\), \(n\)]}\)]\)\!\(\*SuperscriptBox[\(d\[Xi]\), SubscriptBox[\(a\), \(1\)]]\)\[Wedge]...\[Wedge]\!\(\*SuperscriptBox[\(d\[Xi]\), SubscriptBox[\(a\), \(n\)]]\).";

\[Gamma]ComponentsLocal[signature_][vierbein_,coord_,inducedCoord_,pullback_][i__]:=Module[{l,list,selectComponents,pullbackRule},
(*Subscript[\[Gamma], (n)] = 1/n!\!\(
\*SubscriptBox[\(\[PartialD]\), 
SubscriptBox[\(a\), \(1\)]]
\*SuperscriptBox[\(X\), 
SubscriptBox[\(\[Mu]\), \(1\)]]\)...\!\(
\*SubscriptBox[\(\[PartialD]\), 
SubscriptBox[\(a\), \(n\)]]
\*SuperscriptBox[\(X\), 
SubscriptBox[\(\[Mu]\), \(n\)]]\) Subsuperscript[E, Subscript[\[Mu], 1], Subscript[M, 1]]...Subsuperscript[E, Subscript[\[Mu], n], Subscript[M, n]] Subscript[\[CapitalGamma], Subscript[M, 1]...Subscript[M, n]](d\[Xi]^Subscript[a, 1])\[Wedge]...\[Wedge]d\[Xi]^Subscript[a, n]*)
l=Length@coord;
pullbackRule=listsToRule[coord,pullback];
list=Transpose[Grad[pullback,inducedCoord]].(vierbein/.pullbackRule).Array[\[CapitalGamma],l];
selectComponents=list[[#]]&/@{i};
mergeArguments[\[CapitalGamma]]@@selectComponents/.\[CapitalGamma]->antisym[\[CapitalGamma]]
]
\[Gamma]ComponentsLocal[signature_][vierbein_,coord_,inducedCoord_,pullback_][i_]:=1;
\[Gamma]ComponentsLocal::usage="\[Gamma]ComponentsLocal[signature][vielb, coord, inducedCoord, pullback][i__] returns \!\(\*SubscriptBox[\(\[Gamma]\), \({\*SubscriptBox[\(a\), \(1\)],  ... , \*SubscriptBox[\(a\), \(n\)]}\)]\) = \!\(\*SubscriptBox[\(\[PartialD]\), SubscriptBox[\(a\), \(1\)]]\)\!\(\*SuperscriptBox[\(X\), SubscriptBox[\(\[Mu]\), \(1\)]]\)...\!\(\*SubscriptBox[\(\[PartialD]\), SubscriptBox[\(a\), \(n\)]]\)\!\(\*SuperscriptBox[\(X\), SubscriptBox[\(\[Mu]\), \(n\)]]\) \!\(\*SubsuperscriptBox[\(E\), SubscriptBox[\(\[Mu]\), \(1\)], SubscriptBox[\(M\), \(1\)]]\)...\!\(\*SubsuperscriptBox[\(E\), SubscriptBox[\(\[Mu]\), \(n\)], SubscriptBox[\(M\), \(n\)]]\) \!\(\*SubscriptBox[\(\[CapitalGamma]\), \(\(\*SubscriptBox[\(M\), \(1\)] ... \) \*SubscriptBox[\(M\), \(n\)]\)]\), where \!\(\*SubscriptBox[\(\[CapitalGamma]\), \(\(\*SubscriptBox[\(M\), \(1\)] ... \) \*SubscriptBox[\(M\), \(n\)]\)]\) are flat space gamma matrices. 
\!\(\*SubscriptBox[\(\[Gamma]\), \((n)\)]\) = \!\(\*FractionBox[\(1\), \(n!\)]\)\!\(\*SubscriptBox[\(\[Gamma]\), \({\*SubscriptBox[\(a\), \(1\)],  ... , \*SubscriptBox[\(a\), \(n\)]}\)]\)\!\(\*SuperscriptBox[\(d\[Xi]\), SubscriptBox[\(a\), \(1\)]]\)\[Wedge]...\[Wedge]\!\(\*SuperscriptBox[\(d\[Xi]\), SubscriptBox[\(a\), \(n\)]]\).";
