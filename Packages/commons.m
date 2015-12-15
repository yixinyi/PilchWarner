(* ::Package:: *)

zero[x___]:=0;

antisym[\[CapitalGamma]_][x__]:=Module[{list,sortedList},
  list={x};
  sortedList=Sort[list];
  Signature[list] \[CapitalGamma]@@sortedList
];
antisym::usage="antisym[\[CapitalGamma]][i,j,...] builds a function with head \[CapitalGamma] that is antisymmetric in its indices.";


listsToRule[l1_List,l2_List]:= Module[{l=Length@l1},
  (l1[[#]]->l2[[#]])&/@Range[l]
]
listsToRule::usage=" listsToRule[list1, list2] zips two lists with Rule.";


toSuperscriptRule[\[CapitalGamma]_]:={\[CapitalGamma][x__] :>  Superscript[\[CapitalGamma],{x}]};

toSubscriptRule[\[CapitalGamma]_]:={\[CapitalGamma][x__] :>  Subscript[\[CapitalGamma],{x}]};

blockDiagonalMatrix[a_?ArrayQ,b_?ArrayQ]:= Module[{la=Length@a, lb=Length@b,l,M},
  l=la+lb;
  M=ConstantArray[0,{l,l}];
  Table[M[[i,j]]=a[[i,j]],{i,1,la},{j,1,la}];
  Table[M[[i+la,j+la]]=b[[i,j]],{i,1,lb},{j,1,lb}];
  M
]


partition[dim_,n_]:=Module[{perm, ans},
perm=Sort/@Permutations[Range[dim],{n}]; 
ans=DeleteDuplicates@perm
](*It's inefficient*)
