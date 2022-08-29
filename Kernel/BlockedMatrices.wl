(* ::Package:: *)

BeginPackage["LieFunctions`BlockedMatrices`"]


BTranspose::usage = "BTranspose[bMatrix] finds the transposed of the block-matrix bMatrix";
BDot::usage = "BDot[bMatrix1, bMatrix2, \[Ellipsis]] finds the product of the block-matrices bMatrix1\[ThinSpace]\[Times]\[ThinSpace]bMatrix2\[ThinSpace]\[Times]\[ThinSpace]\[Ellipsis]";


Begin["Private`"]


1; (*Identity matrix*)
0; (*Null matrix*)
BTranspose[mat_]:=Map[Transpose,Transpose[mat],{2}]/.{Transpose[1]->1,Transpose[0]->0,Transpose[Transpose[x_]]->x};
BDot[mat1_,mat2_]:=Table[Sum[mat1[[i,k]]**mat2[[k,j]],{k,Length[mat2]}],{i,Length[mat1]},{j,Length[Transpose[mat2]]}]/.{1**x_->x,0**x_->0,x_**1->x,x_**0->0};
BDot[mat1_,mat2_,matN__]:=bDot[bDot[mat1,mat2],matN];


End[]
EndPackage[]
