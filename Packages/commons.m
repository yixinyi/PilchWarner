(* ::Package:: *)

BeginPackage["commons`"]

zero::usage = "zero[expr] = 0"
antisym::usage = "antisym[g][i,j,...] builds a function with head g that is antisymmetric in its indices.";
listsToRule::usage = " listsToRule[list1, list2] zips two lists with Rule.";

mapParallel::usage="mapParallel[{f,g,...},{x,y,...}]={f/@{x,y,...}, g/@{x,y,...}}";
dotFunctor::usage="dotFunctor[f][operator, vector] generalizes Dot such that the function between the matrix element of the operator and the vector components can be specified as f. 
Example 1: Dot[M,V] = dotFunctor[Times][M,V]
Example 2: dotApply[operator, vector] = dotFunctor[Apply][operator, vector]
Note that operator and vector must be inputed as matrices, i.e. 
dotFunctor[f][{{a11, a12,...},...,{an1,...,ann}},{{x1},...,{xn}}]
and dotFunctor[f][{a,b},{x,y}] will not work!";
dotApply::usage="See dotFunctor";
blockDiagonalMatrix::usage = " blockDiagonalMatrix[m1, m2] combines m1 and m2 to a block diagonal matrix."

intByParts::usage = "intByParts[u, dv][x] does the integration by parts: \[Integral]u dv = u v - \[Integral]v du ";
solveFor::usage = "solveFor[expr1, expr2,...][eq] uses Solve to solve for expressions that are not only variables. eq is the same input as for Solve.";
derivList::usage = "derivList[f[x],n] returns a list of derivatives of f[x] up to order n";
toSuperscriptRule
toSubscriptRule

realPart::usage = " realPart[expr] returns the part of expr that does not contain explicit I";
imaginaryPart ::usage = "imaginaryPart[expr] returns the part of expr that contains explicit I";
containIQ::usage = "containIQ[expr] returns True if the expr has imaginary parts";

notNumericQ::usage="Not@NumericQ";


Begin["`Private`"]


zero[x___]:=0;

antisym[g_][]:= g[]
antisym[g_][x__]:=Module[{list,sortedList},
  list={x};
  sortedList=Sort[list];
  Signature[list] g@@sortedList
];


mapParallel[functions_List, xs_List]:=Module[
	{len=Length@functions},
	Table[
		functions[[i]]/@xs,{i,1,len}
	]
]

dotFunctor[f_][m1_List, m2_List]:=Module[{r1,r2,c1,c2,i,j,k},
{r1, c1} = Dimensions@m1;
{r2, c2} = Dimensions@m2;
	If [ c1 == r2,
		Table[
			Plus@@Table[
				f[m1[[i,j]], m2[[j,k]]],{j, 1, c1}
			],
		{i, 1, r1},{k, 1, c2}
		],
	Print["Objects of unequal length!"]
	]
]

dotApply[m1_List, m2_List]:=dotFunctor[(#1@#2)&][m1, m2]


listsToRule[l1_List,l2_List]:= Module[{l=Length@l1},
  (l1[[#]]->l2[[#]])&/@Range[l]
]

toSuperscriptRule[g_]:={g[x__] :>  Superscript[g,{x}]}

toSubscriptRule[g_]:={g[x__] :>  Subscript[g,{x}]}

blockDiagonalMatrix[a_?ArrayQ,b_?ArrayQ]:= Module[{la=Length@a, lb=Length@b,l,M},
  l=la+lb;
  M=ConstantArray[0,{l,l}];
  Table[M[[i,j]]=a[[i,j]],{i,1,la},{j,1,la}];
  Table[M[[i+la,j+la]]=b[[i,j]],{i,1,lb},{j,1,lb}];
  M
]

derivList[f_[x_], n_]:= Table[D[f[x], {x, i}], {i, 0, n}]
  
intByParts[u_,dv_][x_]:=Module[{v},
v=Integrate[dv,x];
u v - Integrate[v D[u,x],x]
]

  
solveFor[expr__][eq_] := Module[{len,list,x},
len=Length@{expr};
list=Array[x,len];
  Solve[eq /. listsToRule[{expr} , list], list] /. listsToRule[list,{expr}]
  ] 

(*
Can be useful for large expressions of the form a + \[ImaginaryI] b, since Simplify is not used.
realPart[expr_]:= expr/.Complex[x_, y_]:> x
imaginaryPart[expr_]:= (Last@expr)/\[ImaginaryI]
*)

realPart[expr_]:= Expand[expr]/.\!\(\*
TagBox[
StyleBox["Complex",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\)[x_, y_]:> x//Simplify
imaginaryPart[expr_]:= (expr - realPart@expr)/I //Simplify

containIQ[expr_]:=(expr - realPart[expr]!= 0)/. Unequal[x___,0]-> True 

notNumericQ[x_]:=Not[NumericQ[x]]


End[]

EndPackage[]


(*
partition[dim_,n_]:=Module[{perm, ans},
perm=Sort/@Permutations[Range[dim],{n}]; 
ans=DeleteDuplicates@perm
]
(*It's inefficient*)
*)
