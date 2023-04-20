(* ::Package:: *)

BeginPackage["LieFunctions`QuadraticLieAlgebras`"]


GetSymmetricInvariantBilinearForm::usage = "GetSymmetricInvariantBilinearForm[var, adjointList] gets all posible quadratic forms for the given algebra defined by adjointList using variable var.";
SkewSymmetricRespectToQ::usage="SkewSymmetricRespectToQ[matrix1, matrix2]";
SkewSymmetricRespectToQFast::usage="SkewSymmetricRespectToQ[matrix1, matrix2]";
InvariantQ::usage="InvariantQ[matrix, adjointList] checks if matrix is invariant with respect to the Lie bracket defined by the adjoint list given in adjointList.";
InvariantBilinearFormQ::usage="InvariantBilinearFormQ[matrix, adjointList] checks if matrix is invariant respect adjointList.";
InvariantBilinearFormQFast::usage="InvariantBilinearFormQ[matrix, adjointList] checks if matrix is invariant respect adjointList.";


Begin["Private`"]
Needs["LieFunctions`Matrices`"]
Needs["LieFunctions`General`"]


Clear[GetSymmetricInvariantBilinearForm];
GetSymmetricInvariantBilinearForm[var_,adjointList_]:=Module[
{sAdjointList = SortBy[adjointList,NumberOfVariables],
dim,mat,varList,cond,matI},
dim=Length[sAdjointList];
mat=GenericSymmetricMatrix[dim,var];
varList=Variables[mat];
cond[1]=SkewSymmetricRespectToQ[mat,sAdjointList[[1]]];
matI[1]=mat/.Solve[cond[1],varList][[1]];
cond[i_]:=(cond[i]=And@@SkewSymmetricRespectToQFast[matI[i-1],sAdjointList[[i]]])/;i>1;
matI[i_]:=(matI[i]=(matI[i-1]/.Solve[cond[i],Intersection[varList,Variables[cond[i]]]][[1]]))/;i>1;
Return[matI[dim]]
]


(* Conditions to matrices - Symmetry *)
Clear[SkewSymmetricRespectToQ, SkewSymmetricRespectToQFast, SkewSymmetricRespectToQU, SkewSymmetricRespectToQUNR];


SkewSymmetricRespectToQFast[matrix_, respectTo_] := SkewSymmetricRespectToQUNR[matrix, respectTo] /;
 And[
  If[SquareMatrixQ[matrix],
    True,
    Message[SkewSymmetricRespectToQFast::nMatrix, matrix, 1]; False
  ],
  If[SquareMatrixQ[respectTo],
    True,
    Message[SkewSymmetricRespectToQFast::nMatrix, respectTo, 1]; False
  ],
  If[Length[matrix] == Length[respectTo],
    True,
    Message[SkewSymmetricRespectToQFast::nLength, matrix, 1 respectTo, 2]; False
  ]
];

SkewSymmetricRespectToQ[matrix_, respectTo_] := SkewSymmetricRespectToQU[matrix, respectTo] /;
 And[
  If[SquareMatrixQ[matrix],
    True,
    Message[SkewSymmetricRespectToQ::nMatrix, matrix, 1]; False
  ],
  If[SquareMatrixQ[respectTo],
    True,
    Message[SkewSymmetricRespectToQ::nMatrix, respectTo, 1]; False
  ],
  If[Length[matrix] == Length[respectTo],
    True,
    Message[SkewSymmetricRespectToQ::nLength, matrix, 1 respectTo, 2]; False
  ]
];

SkewSymmetricRespectToQ::nMatrix = "Argument `1` at position `2` is not a matrix.";
SkewSymmetricRespectToQ::nLength = "Argument `1` at position `2` is not of the same lenght as argument `3` at position `4`.";
SkewSymmetricRespectToQFast::nMatrix = SkewSymmetricRespectToQ::nMatrix; 
SkewSymmetricRespectToQFast::nLength = SkewSymmetricRespectToQ::nLength;

SkewSymmetricRespectToQU[matrix_, respectTo_] := Reduce[Map[# == 0&, Flatten[Transpose[respectTo] . matrix + matrix . respectTo]]];
SkewSymmetricRespectToQUNR[matrix_, respectTo_] := Map[# == 0&, Flatten[Transpose[respectTo] . matrix + matrix . respectTo]];


(* Conditions to matrices - Quadratic conditions *)

Clear[InvariantQ, InvariantQU];

InvariantQ[matrix_, adjointList_] := InvariantQU[matrix, adjointList] /;
And[
  If[MatrixQ[matrix],
    True,
    Message[InvariantQ::nMatrix, matrix, 1]; False
  ],
  If[CubicMatrixQ[adjointList],
    True,
    Message[InvariantQ::nCubic, matrix, 1]; False
  ],
  If[Length[matrix] == Length[adjointList],
    True,
    Message[InvariantQ::nLength, matrix, 1, adjointList, 2]; False
  ],
  If[AdjointListQ[adjointList],
    True,
    Message[InvariantQ::nAdjoint, adjointList, 3]; False
  ]
];

InvariantQ::nMatrix  = "Argument `1` at position `2` is not a matrix.";
InvariantQ::nCubic   = "Argument `1` at position `2` is not a balanced list of matrices.";
InvariantQ::nLength  = "Argument `1` at position `2` is not of the same lenght as argument `3` at position `4`.";
InvariantQ::nAdjoint = "Argument `1` at position `2` is not a valid list of adjoint matrices.";

InvariantQU[matrix_, adjointList_] := Reduce[And @@ Table[SkewSymmetricRespectToQU[
  matrix, adjoint], {adjoint, adjointList}]];

Clear[InvariantBilinearFormQ, InvariantBilinearFormQU, InvariantBilinearFormQUNR];

InvariantBilinearFormQ[matrix_, adjointList_] := InvariantBilinearFormQU[matrix, adjointList] /;
And[
  If[MatrixQ[matrix],
    True,
    Message[InvariantBilinearFormQ::nMatrix, matrix, 1]; False
  ],
  If[CubicMatrixQ[adjointList],
    True,
    Message[InvariantBilinearFormQ::nCubic, matrix, 1]; False
  ],
  If[Length[matrix] == Length[adjointList],
    True,
    Message[InvariantBilinearFormQ::nLength, matrix, 1, adjointList, 2]
      ; False
  ],
  If[AdjointListQ[adjointList],
    True,
    Message[InvariantBilinearFormQ::nAdjoint, adjointList, 3]; False
  ]
];

InvariantBilinearFormQFast[matrix_, adjointList_] := InvariantBilinearFormQUNR[matrix, adjointList] /;
And[
  If[MatrixQ[matrix],
    True,
    Message[InvariantBilinearFormQ::nMatrix, matrix, 1]; False
  ],
  If[CubicMatrixQ[adjointList],
    True,
    Message[InvariantBilinearFormQ::nCubic, matrix, 1]; False
  ],
  If[Length[matrix] == Length[adjointList],
    True,
    Message[InvariantBilinearFormQ::nLength, matrix, 1, adjointList, 2]
      ; False
  ],
  If[AdjointListQ[adjointList],
    True,
    Message[InvariantBilinearFormQ::nAdjoint, adjointList, 3]; False
  ]
];

InvariantBilinearFormQFast::nMatrix  = InvariantBilinearFormQ::nMatrix  = InvariantQ::nMatrix;
InvariantBilinearFormQFast::nCubic   = InvariantBilinearFormQ::nCubic   = InvariantQ::nCubic;
InvariantBilinearFormQFast::nLength  = InvariantBilinearFormQ::nLength  = InvariantQ::nLength;
InvariantBilinearFormQFast::nAdjoint = InvariantBilinearFormQ::nAdjoint = InvariantQ::nAdjoint;

InvariantBilinearFormQU[matrix_List, adjointList_List] := Reduce[And[SymmetricQU[matrix], InvariantQU[matrix, adjointList]]];
InvariantBilinearFormQUNR[matrix_List, adjointList_List] := And[SymmetricQU[matrix], InvariantQU[matrix, adjointList]];


End[]
EndPackage[]
