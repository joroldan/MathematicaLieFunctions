(* ::Package:: *)

BeginPackage["LieFunctions`HallBasis`"]


HallBasisAdjointList::usage="HallBasisAdjointList[type, nilpotency] returns the adjoint list in the Hall Basis of the free nilpotent Lie algebra."


Begin["Private`"]
Needs["HallBasis`"] 


HallBasisAdjointList[type_, nilpotency_]:=Table[
Transpose[Table[HallBasis`getCoordinates[{el1,el2},type,nilpotency],{el2,HallBasis`basisUntilLevel[type,nilpotency]}]]
,{el1,HallBasis`basisUntilLevel[type,nilpotency]}];


End[]
EndPackage[]
