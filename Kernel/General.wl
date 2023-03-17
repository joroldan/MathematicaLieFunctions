(* ::Package:: *)

BeginPackage["LieFunctions`General`"]


AdjointListSymmetricQ::usage="AdjointListSymmetricQ[adjointList] checks if ad_x(y) = -ad_y(x).";
AdjointListJacobiQ::usage="AdjointListJacobiQ[adjointList] checks if adjointList satisfies Jacobi identy.";
AdjointListQ::usage="AdjointListQ[adjointList] checks if adjointList is a valid list of adjoint matrices.";
ProductCoordinates::usage="ProductCoordinates[v1, v2, adjointList] multiplies elements of coordinates v1 and v2 using adjointList as the list of adjoint matrices of the algebra.";
DerivationQ::usage="DerivationQ[matrix, adjointList] checks if matrix defines a derivation when the product is given by the adjoint list given in parameter adjointList.";
ProductTableList::usage="ProductTableList[basis,adjointList] lists all Lie brackets given by the adjointList using notation from basis.";
NonNullProductTableList::usage="NonNullProductTableList[basis,adjointList] lists all non null Lie brackets given by the adjointList using notation from basis.";
ProductTablePrint::usage="ProductTablePrint[basis,adjointList] prints in a human readable way all Lie brackets given by the adjointList using notation from basis.";
NonNullProductTablePrint::usage="NonNullProductTablePrint[basis,adjointList] prints in a human readable all non null Lie brackets given by the adjointList using notation from basis.";
GetDerivation::usage="GetDerivation[var,adjointList] returns a generic derivation of algebra given by the adjointList using variable var";
GetInnerDerivations::usage="GetInnerDerivations[var,adjointList] returns an inner derivation of algebra given by the adjointList using variable var";
GetSkewDerivation::usage="GetSkewDerivation[var,adjointList,bForm] returns a derivation of algebra given by the adjointList using variable var wich is skew in respect to bForm";
AutomorphismQ::usage="AutomorphismQ[m,adjointList] checks if m is an automorphism of algebra given by the adjointList";
GetAutomorphism::usage="GetAutomorphism[var,adjointList] returns a generic automorphism of algebra given by the adjointList using variable var";
ChangeAdjointListBasis::usage:="ChangeAdjointListBasis[newBasis,adjointList] from a newBasis matrices which representes the matrix which change coordinates from the new basis to the old one, it changes adjointList to that new basis.";
AdjointListQuotient::usage:="AdjointListQuotient[complementBasis,idealBasis,adjointList] given a complement and an ideal, it finds the new adjoint in the quoting given by that ideal";


Begin["Private`"]


Needs["LieFunctions`Matrices`"]
Needs["LieFunctions`QuadraticLieAlgebras`"]


AdjointListSymmetricQ[adjL_]:=Flatten[Table[{i,j,adjL[[i,All,j]]+adjL[[j,All,i]]},{i,Length[adjL]},{j,i+1,Length[adjL]}],1]
AdjointListJacobiQ[adjL_]:=Flatten[Table[
{i,j,k,adjL[[i]] . adjL[[j,All,k]]+
adjL[[j]] . adjL[[k,All,i]]+
adjL[[k]] . adjL[[i,All,j]]},{i,Length[adjL]},{j,i+1,Length[adjL]},{k,j+1,Length[adjL]}],2];
AdjointListQ[adjointList_]:=Module[{b1,b2,b3},
b1=(Dimensions[adjointList]===Length[adjointList]*{1,1,1});
b2=And@@Map[#==0&,Flatten[AdjointListSymmetricQ[adjointList][[All,3]]]];
b3=And@@Map[#==0&,Flatten[AdjointListJacobiQ[adjointList][[All,4]]]];
Return[b1&&b2&&b3];
];

ProductTableList[basis_,adjointList_]:=Flatten[Table[{basis[[i]],basis[[j]],({basis} . adjointList[[i,All,j]])[[1]]},{i,Length[basis]},{j,i+1,Length[basis]}],1];
NonNullProductTableList[basis_,adjointList_]:=Select[ProductTableList[basis,adjointList],#[[3]]=!=0&];
ProductTablePrint[basis_,adjointList_]:=Column[Map["["<>ToString[#[[1]]]<>", "<>ToString[#[[2]]]<>"] = "<>ToString[#[[3]]]&,ProductTableList[basis,adjointList]]];
NonNullProductTablePrint[basis_,adjointList_]:=Column[Map["["<>ToString[#[[1]]]<>", "<>ToString[#[[2]]]<>"] = "<>ToString[#[[3]]]&,NonNullProductTableList[basis,adjointList]]];


(* Product coordinates *)
Clear[ProductCoordinates, ProductCoordinatesU];

ProductCoordinates[c1_, c2_, adjointList_] := ProductCoordinatesU[c1, c2, adjointList] /;
And[
  If[VectorQ[c1],
    True,
    Message[ProductCoordinates::nVector, c1, 1]; False
  ],
  If[VectorQ[c2],
    True,
    Message[ProductCoordinates::nVector, c2, 2]; False
  ],
  If[Length[c1] == Length[c2],
    True,
    Message[ProductCoordinates::nVLength, c1, 1, c2, 2]; False
  ],
  If[CubicMatrixQ[adjointList],
    True,
    Message[ProductCoordinates::nCubic, adjointList, 3]; False
  ],
  If[Length[c1] == Length[adjointList],
    True,
    Message[ProductCoordinates::naLength, Length[c1], adjointList, 3]; False
  ],
  If[AdjointListQ[adjointList],
    True,
    Message[ProductCoordinates::nAdjoint, adjointList, 3]; False
  ]
];

ProductCoordinates::nVector  = "Argument `1` at position `2` which is not a vector.";
ProductCoordinates::nVLength = "Argument `1` at position `2` and argument `3` at position `4` are not of the same length.";
ProductCoordinates::nCubic   = "Argument `1` at position `2` is not a balanced list of matrices.";
ProductCoordinates::naLength = "Argument `2` at position `3` is not of lenght `1` as the vectors.";
ProductCoordinates::nAdjoint = "Argument `1` at position `2` is not a valid list of adjoint matrices.";

ProductCoordinatesU[c1_, c2_, adjointList_] := c1 . Table[adjointList[[i]] . c2, {i, Length[c1]}];


(* Conditions to matrices - Derivation *)
Clear[DerivationQ, DerivationQU];

DerivationQ[matrix_, adjointList_] := DerivationQU[matrix, adjointList] /; 
And[
  If[MatrixQ[matrix],
    True,
    Message[DerivationQ::nMatrix, matrix, 1]; False
  ],
  If[CubicMatrixQ[adjointList],
    True,
    Message[DerivationQ::nCubic, matrix, 1]; False
  ],
  If[Length[matrix] == Length[adjointList],
    True,
    Message[DerivationQ::nLength, matrix, 1, adjointList, 2]; False
  ],
  If[AdjointListQ[adjointList],
    True,
    Message[DerivationQ::nAdjoint, adjointList, 3]; False
  ]
];

DerivationQ::nMatrix  = "Argument `1` at position `2` is not a matrix.";
DerivationQ::nCubic   = "Argument `1` at position `2` is not a balanced list of matrices.";
DerivationQ::nLength  = "Argument `1` at position `2` is not of the same lenght as argument `3` at position `4`.";
DerivationQ::nAdjoint = "Argument `1` at position `2` is not a valid list of adjoint matrices.";

DerivationQU[matrix_, adjointList_] := Module[
  {dim = Length[adjointList], der = matrix},
  Reduce[Map[# == 0&, Flatten[Table[
    der . ProductCoordinatesU[EC[i, dim], EC[j, dim], adjointList] -
    ProductCoordinatesU[der . EC[i, dim], EC[j, dim], adjointList] -
    ProductCoordinatesU[EC[i, dim], der . EC[j, dim], adjointList], {i, dim}, {j, dim}]]]]
];

GetDerivation[var_,adjointList_]:=Module[{m=GenericMatrix[Length[adjointList],var]},
Return[m/.Solve[DerivationQ[m,adjointList]][[1]]]];
GetInnerDerivations[var_,adjointList_]:=Sum[var[i]*adjointList[[i]],{i,Length[adjointList]}];
GetSkewDerivation[var_,adjointList_,bForm_]:=Module[{m=GetDerivation[var,adjointList]},Return[m/.Solve[SkewSymmetricRespectToQ[bForm,m]][[1]]]];
AutomorphismQ[m_,adjointList_]:=Module[{n=Length[adjointList]},
Return[Reduce[Flatten[Table[m . ProductCoordinates[EC[i,n],EC[j,n],adjointList]==ProductCoordinates[m . EC[i,n],m . EC[j,n],adjointList],{i,n},{j,i+1,n}]]]]];
GetAutomorphism[var_,adjointList_]:=Module[{n=Length[adjointList],m,i,j,tmp1,tmp2},
tmp1=tmp2=PrintTemporary["0% complete"];
m=GenericMatrix[n,var];
For[i=1,i<=n,i++,
For[j=i+1,j<=n,j++,
tmp1=tmp2;
tmp2=PrintTemporary[N[100*(i-i^2-2 n+2 i n+2j-2i)/(n(n-1)),2],"% complete"];
NotebookDelete[tmp1];
m=m/.Solve[m . ProductCoordinates[EC[i,n],EC[j,n],adjointList]==ProductCoordinates[m . EC[i,n],m . EC[j,n],adjointList]][[1]]]];
Return[m]];

ChangeAdjointListBasis[newBasis_,adjointList_]:=Module[{b=Transpose[newBasis],cB=Inverse[newBasis],n=Length[adjointList]},
Return[Table[Transpose[Table[cB . ProductCoordinates[b[[i]],b[[j]],adjointList],{j,n}]],{i,n}]]
]
AdjointListQuotient[complementBasis_,idealBasis_,adjointList_]:=Module[{newBasis,newAdjointList,n=Length[adjointList]-Length[idealBasis]},
newBasis = Transpose[Join[complementBasis,idealBasis]];
newAdjointList=ChangeAdjointListBasis[newBasis,adjointList];
Return[Map[#[[;;n,;;n]]&,newAdjointList[[;;n]]]]];


End[]
EndPackage[]
