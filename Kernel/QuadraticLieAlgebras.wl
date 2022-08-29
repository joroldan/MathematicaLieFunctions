(* ::Package:: *)

BeginPackage["LieFunctions`QuadraticLieAlgebras`"]


GetQuadraticForm::usage = "GetQuadraticForm[var, adjointList] gets all posible quadratic forms for the given algebra defined by adjointList using variable var.";
SkewSymmetricRespectToQ::usage="SkewSymmetricRespectToQ[matrix1, matrix2]";
SkewSymmetricRespectToQFast::usage="SkewSymmetricRespectToQ[matrix1, matrix2]";
InvariantQ::usage="InvariantQ[matrix, adjointList] checks if matrix is invariant with respect to the Lie bracket defined by the adjoint list given in adjointList.";
LinearQuadraticFormQ::usage="LinearQuadraticFormQ[matrix, adjointList] checks if matrix is symmetric and invariant respect adjointList.";
LinearQuadraticFormQFast::usage="LinearQuadraticFormQ[matrix, adjointList] checks if matrix is symmetric and invariant respect adjointList.";


Begin["Private`"]
Needs["LieFunctions`Matrices`"]
Needs["LieFunctions`General`"]


Clear[GetQuadraticForm];
GetQuadraticForm[var_,adjointList_]:=Module[
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

InvariantQ::nMatrix = LieFunctions`General`DerivationQ::nMatrix;
InvariantQ::nCubic = LieFunctions`General`DerivationQ::nCubic;
InvariantQ::nLength = LieFunctions`General`DerivationQ::nLength;
InvariantQ::nAdjoint = LieFunctions`General`DerivationQ::nAdjoint;

InvariantQU[matrix_, adjointList_] := Reduce[And @@ Table[SkewSymmetricRespectToQU[
  matrix, adjoint], {adjoint, adjointList}]];

Clear[LinearQuadraticFormQ, LinearQuadraticFormQU, LinearQuadraticFormQUNR];

LinearQuadraticFormQ[matrix_, adjointList_] := LinearQuadraticFormQU[matrix, adjointList] /;
And[
  If[MatrixQ[matrix],
    True,
    Message[LinearQuadraticFormQ::nMatrix, matrix, 1]; False
  ],
  If[CubicMatrixQ[adjointList],
    True,
    Message[LinearQuadraticFormQ::nCubic, matrix, 1]; False
  ],
  If[Length[matrix] == Length[adjointList],
    True,
    Message[LinearQuadraticFormQ::nLength, matrix, 1, adjointList, 2]
      ; False
  ],
  If[AdjointListQ[adjointList],
    True,
    Message[LinearQuadraticFormQ::nAdjoint, adjointList, 3]; False
  ]
];

LinearQuadraticFormQFast[matrix_, adjointList_] := LinearQuadraticFormQUNR[matrix, adjointList] /;
And[
  If[MatrixQ[matrix],
    True,
    Message[LinearQuadraticFormQ::nMatrix, matrix, 1]; False
  ],
  If[CubicMatrixQ[adjointList],
    True,
    Message[LinearQuadraticFormQ::nCubic, matrix, 1]; False
  ],
  If[Length[matrix] == Length[adjointList],
    True,
    Message[LinearQuadraticFormQ::nLength, matrix, 1, adjointList, 2]
      ; False
  ],
  If[AdjointListQ[adjointList],
    True,
    Message[LinearQuadraticFormQ::nAdjoint, adjointList, 3]; False
  ]
];

LinearQuadraticFormQFast::nMatrix = LinearQuadraticFormQ::nMatrix = InvariantQ::nMatrix;
LinearQuadraticFormQFast::nCubic = LinearQuadraticFormQ::nCubic = InvariantQ::nCubic;
LinearQuadraticFormQFast::nLength = LinearQuadraticFormQ::nLength = InvariantQ::nLength;
LinearQuadraticFormQFast::nAdjoint = LinearQuadraticFormQ::nAdjoint = InvariantQ::nAdjoint;

LinearQuadraticFormQU[matrix_List, adjointList_List] := Reduce[And[SymmetricQU[matrix], InvariantQU[matrix, adjointList]]];
LinearQuadraticFormQUNR[matrix_List, adjointList_List] := And[SymmetricQU[matrix], InvariantQU[matrix, adjointList]];


End[]
EndPackage[]
