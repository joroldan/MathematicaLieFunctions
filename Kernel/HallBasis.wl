(* ::Package:: *)

BeginPackage["LieFunctions`HallBasis`"]


HallBasisAdjointList::usage="HallBasisAdjointList[type, nilpotency] returns the adjoint list in the Hall Basis of the free nilpotent Lie algebra."
HallBasisLevel::usage = "basis[\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic]] returns the basis of degree \*StyleBox[\"level\", FontSlant->Italic] and \*StyleBox[\"num\", FontSlant->Italic] generators.";
HallBasisUntilLevel::usage = "HallBasisUntilLevel[\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic]] returns the basis of degree less or equal than \*StyleBox[\"level\", FontSlant->Italic] and \*StyleBox[\"num\", FontSlant->Italic] generators.";
HallBasisLevelDimension::usage = "HallBasisLevelDimension[\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic]] returns the dimension of the basis of degree \*StyleBox[\"level\", FontSlant->Italic] and \*StyleBox[\"num\", FontSlant->Italic] generators.";
HallBasisUntilLevelDimension::usage = "HallBasisUntilLevelDimension[\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic]] returns the dimension of the basis of degree less or equal than \*StyleBox[\"level\", FontSlant->Italic] and \*StyleBox[\"num\", FontSlant->Italic] generators.";

HallGrid::usage = "HallGrid[\*StyleBox[\"mat\", FontSlant->Italic],\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic]] returns matrix \*StyleBox[\"mat\", FontSlant->Italic] with a grid indicating basis levels.
HallGrid[\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic]] applied over a matrix returns it with a grid indicating basis levels."

$HallVar::usage = "$HallVar is the variable used for polynomials in HallBasis package.";


Begin["Private`"]
Needs["LieFunctions`Matrices`"]


(*
greaterEqual::usage="greaterEqual[\*StyleBox[\"u\", FontSlant->Italic],\*StyleBox[\"v\", FontSlant->Italic]] returns the result of evaluating (\*StyleBox[\"u\", FontSlant->Italic]\[GreaterEqual]v).";
greater::usage="greater[\*StyleBox[\"u\", FontSlant->Italic],v] returns the result of evaluating (\*StyleBox[\"u\", FontSlant->Italic]>\*StyleBox[\"v\", FontSlant->Italic]).";
equal::usage="equal[\*StyleBox[\"u\", FontSlant->Italic],v] returns the result of evaluating (\*StyleBox[\"u\", FontSlant->Italic]\[Equal]\*StyleBox[\"v\", FontSlant->Italic]).";
less::usage="less[\*StyleBox[\"u\", FontSlant->Italic],v] returns the result of evaluating (\*StyleBox[\"u\", FontSlant->Italic]<\*StyleBox[\"v\", FontSlant->Italic]).";
lessEqual::usage="lessEqual[\*StyleBox[\"u\", FontSlant->Italic],v] returns the result of evaluating (\*StyleBox[\"u\", FontSlant->Italic]\[LessEqual]\*StyleBox[\"v\", FontSlant->Italic]).";

degree::usage = "degree[\*StyleBox[\"u\", FontSlant->Italic]] returns the number of terms (degree) of the monomial \*StyleBox[\"u\", FontSlant->Italic]";
orderArray::usage = "orderArray[\*StyleBox[\"u\", FontSlant->Italic],v] analizes the lexicographic order of \*StyleBox[\"u\", FontSlant->Italic] and \*StyleBox[\"v\", FontSlant->Italic] and returns 1 if \*StyleBox[\"u\", FontSlant->Italic]>\*StyleBox[\"v\", FontSlant->Italic], 0 if \*StyleBox[\"u\", FontSlant->Italic]=\*StyleBox[\"v\", FontSlant->Italic] and -1 if \*StyleBox[\"u\", FontSlant->Italic]<\*StyleBox[\"v\", FontSlant->Italic].";

isCanon::usage = "isCanon[\*StyleBox[\"u\", FontSlant->Italic]] return whether if \*StyleBox[\"u\", FontSlant->Italic] is in its canonic form or not.";

$hallBasisDefinitions::usage = "$hallBasisDefinitions is the list of HallBasis package definitions.";
$hallBasisDocumentation::usage="$hallBasisDocumentation is the documentation of HallBasis package.";

$hallBasisDocumentation := Scan[Information[#,LongForm->False]&,$hallBasisDefinitions];
$hallBasisDefinitions = {greaterEqual, greater, equal,less,lessEqual,degree,orderArray,HallBasisLevel,HallBasisUntilLevel,HallBasisLevelDimension,HallBasisUntilLevelDimension,getCanonicCoordinates,getCoordinates,invariantSymmetricBilinearMatrix,quadraticDimension,HallGrid,getDerivationRule,genericDerivation,applyDerivation,genericDerivationFast,getAutomorphismRule,genericAutomorphism,applyAutomorphism,genericAutomorphismFast,mE,mN,mH,getHat,getHatFast,$HallVar,$hallMatrixVar};

quadraticDimension::usage ="quadraticDimension[\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic]] returns the number of variables in the corresponding invariantSymmetricBilinearMatrix[\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic]].";

mE::usage = "mE[\*StyleBox[\"i\", FontSlant->Italic],\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic]] returns the projection endomorphism to level \*StyleBox[\"i\", FontSlant->Italic] for an element in a free Lie algebra of \*StyleBox[\"num\", FontSlant->Italic] generators and nilpotency index \*StyleBox[\"level\", FontSlant->Italic]."
mH::usage = "mH[\*StyleBox[\"mat\", FontSlant->Italic],\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic]] returns the H(\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic])-element associated to matrix \*StyleBox[\"mat\", FontSlant->Italic].
mH[\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic]] returns the H(\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic])-element associated to a generic matrix."
mN::usage = "mN[\*StyleBox[\"mat\", FontSlant->Italic],\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic]] returns the N(\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic])-element associated to matrix \*StyleBox[\"mat\", FontSlant->Italic].
mN[\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic]] returns the N(\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic])-element associated to a generic matrix."

getHat::usage = "getHat[\*StyleBox[\"mat\", FontSlant->Italic]] applies hat operator to \*StyleBox[\"mat\", FontSlant->Italic] matrix."
getHatFast::usage = "getHatFast[\*StyleBox[\"mat\", FontSlant->Italic]] applies hat operator to \*StyleBox[\"mat\", FontSlant->Italic] matrix in a faster way."

invariantSymmetricBilinearMatrix::usage ="invarianSymmetricBilinearMatrix[\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic]] returns the matrix associated to a generic invariant symmetric bilinear form of a \*StyleBox[\"level\", FontSlant->Italic]-nilpotent Lie algebra of \*StyleBox[\"num\", FontSlant->Italic] generators."

getDerivationRule::usage = "getDerivationRule[\*StyleBox[\"mat\", FontSlant->Italic],\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic]] returns the rules \*StyleBox[\"mat\", FontSlant->Italic] has to satify to be a derivation expressed in a Hall basis with \*StyleBox[\"num\", FontSlant->Italic] generators and \*StyleBox[\"level\", FontSlant->Italic] nilpotency index."
genericDerivation::usage = "genericDerivation[\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic]] returns a generic derivation matrix in a Hall basis with \*StyleBox[\"num\", FontSlant->Italic] generators and \*StyleBox[\"level\", FontSlant->Italic] nilpotency index."
applyDerivation::usage = "applyDerivation[\*StyleBox[\"mat\", FontSlant->Italic],\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic]] modifies \*StyleBox[\"mat\", FontSlant->Italic] matrix to be a derivation expressed in a Hall basis with \*StyleBox[\"num\", FontSlant->Italic] generators and \*StyleBox[\"level\", FontSlant->Italic] nilpotency index."
genericDerivationFast::usage = "genericDerivationFast[\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic]] returns a generic derivation matrix in a Hall basis with \*StyleBox[\"num\", FontSlant->Italic] generators and \*StyleBox[\"level\", FontSlant->Italic] nilpotency index in a faster way."

getAutomorphismRule::usage = "getAutomorphismRule[\*StyleBox[\"mat\", FontSlant->Italic],\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic]] returns the rules \*StyleBox[\"mat\", FontSlant->Italic] has to satify to be an automorphism expressed in a Hall basis with \*StyleBox[\"num\", FontSlant->Italic] generators and \*StyleBox[\"level\", FontSlant->Italic] nilpotency index."
genericAutomorphism::usage = "genericAutomorphism[\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic]] returns a generic automorphism matrix in a Hall basis with \*StyleBox[\"num\", FontSlant->Italic] generators and \*StyleBox[\"level\", FontSlant->Italic] nilpotency index."
applyAutomorphism::usage = "applyAutomorphism[\*StyleBox[\"mat\", FontSlant->Italic],\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic]] modifies \*StyleBox[\"mat\", FontSlant->Italic] matrix to be an automorphism expressed in a Hall basis with \*StyleBox[\"num\", FontSlant->Italic] generators and \*StyleBox[\"level\", FontSlant->Italic] nilpotency index."
genericAutomorphismFast::usage = "genericAutomorphismFast[\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic]] returns a generic automorphism matrix in a Hall basis with \*StyleBox[\"num\", FontSlant->Italic] generators and \*StyleBox[\"level\", FontSlant->Italic] nilpotency index in a faster way."

$hallMatrixVar::usage =  "$hallMatrixVar is the variable used for matrices in HallBasis package.";
*)


HallBasisAdjointList[type_, nilpotency_]:=Table[
Transpose[Table[getCoordinates[{el1,el2},type,nilpotency],{el2,HallBasisUntilLevel[type,nilpotency]}]]
,{el1,HallBasisUntilLevel[type,nilpotency]}];


(* ::Text:: *)
(*Basis comparisons of greater degree monomials*)


(* ::Input::Initialization:: *)
degree[u_]:=Length[Flatten[u]];


(* ::Input::Initialization:: *)
greater[u_,v_]:=Module[{uf=Flatten[u],vf=Flatten[v]},
If[Not[isCanon[u]&&isCanon[v]],Throw[Message[isCanon::ncanon]]];
If[Length[uf]>Length[vf],True,If[Length[uf]<Length[vf],False,
orderArray[uf,vf]==1
]]];
equal[u_,v_]:=Module[{uf=Flatten[u],vf=Flatten[v]},If[Not[isCanon[u]&&isCanon[v]],Throw[Message[isCanon::ncanon]]];
If[Length[uf]!=1,u===v,Flatten[{u}]===Flatten[{v}]]];


(* ::Text:: *)
(*More types of comparisons*)


(* ::Input::Initialization:: *)
greaterEqual[u_,v_]:=Or[greater[u,v],equal[u,v]];
lessEqual[u_,v_]:=greaterEqual[v,u];
less[u_,v_]:=greater[v,u];


(* ::Text:: *)
(*Auxiliary function for sorting same degree monomials. Lexicographic order*)


(* ::Input::Initialization:: *)
orderArray[u_List,v_List]:=Order[u[[All,1]],v[[All,1]]];
orderArray[u_,v_]:=Order[u[[1]],v[[1]]];


(* ::Text:: *)
(*Check if a vector is in its canonic form*)


(* ::Input::Initialization:: *)
isCanon[u_]:=Module[{u1b,u2b,u11b,u12b},If[degree[u]==1,True,
u1b=isCanon[u[[1]]];
u2b=isCanon[u[[2]]];
If[!u1b||!u2b||greaterEqual[u[[2]],u[[1]]],
False,
If[degree[u[[1]]]>1,
u11b=isCanon[u[[1,1]]];
u12b=isCanon[u[[1,2]]];
If[u11b&&u12b&&greaterEqual[u[[2]],u[[1,2]]],True,False],
True
]]]];
isCanon::ncanon="One of the arguments is not canonic";


(* ::Text:: *)
(*Basis functions*)


(* ::Input::Initialization:: *)
HallBasisLevel[num_Integer,1]:=Table[$HallVar[i],{i,num,1,-1}];
HallBasisLevel[num_Integer,level_Integer]:=HallBasisLevel[num,level]=Module[{k},Sort[Select[Flatten[Table[Tuples[{HallBasisLevel[num,level-k],HallBasisLevel[num,k]}],{k,level/2}],1],isCanon[#]&],less]];
HallBasisUntilLevel[num_Integer,level_Integer]:=Join@@Table[HallBasisLevel[num,i],{i,level}];

HallBasisLevelDimension[num_Integer,level_Integer]:=1/level Sum[MoebiusMu[a]*num^(level/a),{a,Divisors[level]}];
HallBasisUntilLevelDimension[num_Integer,level_Integer]:=Sum[HallBasisLevelDimension[num,i],{i,level}];


(* ::Text:: *)
(*Coordinates functions*)


(* ::Input::Initialization:: *)
getCanonicCoordinates[u_,num_,level_]:=Module[{i,position=Position[HallBasisUntilLevel[num,Min[level,Length[Flatten[u]]]],u]},If[Length[position]==0,position={{-1}};Throw[Message[isCanon::ncanon]]];position=position[[1,1]];Table[If[i==position,1,0],{i,HallBasisUntilLevelDimension[num,level]}]];

getCoordinates[u_,num_,level_]:=Module[{u1,u2,c1,c2,u11,u12,c11,c12,base=HallBasisUntilLevel[num,level],zero},
(*Print["Method called with ",u];*)
zero=Table[0,Length[base]];
If[degree[u]>level,
zero,
If[isCanon[u],
getCanonicCoordinates[u,num,level],
(* If u is not canonic *)
u1=u[[1]];
u2=u[[2]];
If[isCanon[u1]&&isCanon[u2],
(* Both are canonic *)
If[greater[u1,u2],
(* As u is not canonic u2<u12 *)
u11=u1[[1]];
u12=u1[[2]];
getCoordinates[{{u11,u2},u12},num,level]-getCoordinates[{{u12,u2},u11},num,level]
,
If[equal[u1,u2],
zero,
-getCoordinates[{u2,u1},num,level]
]
]
(* One of them is not canonic *)
,
c1=getCoordinates[u1,num,level];
c2=getCoordinates[u2,num,level];
Plus@@Flatten[Table[If[c1[[i]]!=0,c1[[i]]*c2[[j]]*getCoordinates[{base[[i]],base[[j]]},num,level],zero],{i,Length[base]},{j,Length[base]}],1]
]
]
]
];


(* ::Text:: *)
(*Invariant Symmetric bilinear form*)


(* ::Input::Initialization:: *)
tuplesToCheck[num_,level_]:=Tuples[{HallBasisUntilLevel[num,level][[num+1;;]],HallBasisUntilLevel[num,level]}];
tuplesLeft[num_,level_]:=Table[{getCanonicCoordinates[elto[[1]],num,level],getCanonicCoordinates[elto[[2]],num,level]},{elto,tuplesToCheck[num,level]}];
tuplesRight[num_,level_]:=Table[{getCanonicCoordinates[elto[[1,1]],num,level],getCoordinates[{elto[[1,2]],elto[[2]]},num,level]},{elto, tuplesToCheck[num,level]}];

conditions[num_,level_]:=Module[{dim=HallBasisUntilLevelDimension[num,level],mS,t1=tuplesLeft[num,level],t2=tuplesRight[num,level]},mS=GenericSymmetricMatrix[dim,$hallMatrixVar];Off[Solve::svars];Return[Solve[Table[t1[[i,1]] . mS . t1[[i,2]]==t2[[i,1]] . mS . t2[[i,2]],{i,Length[t1]}],Table[$hallMatrixVar[i],{i,(dim(dim+1))/2}]][[1]]];On[Solve::svars]];

invariantSymmetricBilinearMatrix[num_,level_]:=GenericSymmetricMatrix[HallBasisUntilLevelDimension[num,level],$hallMatrixVar]/.conditions[num,level];
quadraticDimension[num_,level_]:=Length[Complement[DeleteDuplicates[Abs[Flatten[invarianSymmetricBilinearMatrix[num,level]]]],{0}]];


(* ::Text:: *)
(*Style functions*)


(* ::Input::Initialization:: *)
HallGrid[mat_List,num_Integer,level_Integer]:=Module[{aux=Table[1+n->True,{n,Join[{0},Table[HallBasisUntilLevelDimension[num,j],{j,level}]]}]},Grid[mat,Dividers-> {aux,aux}]];
HallGrid[num_Integer,level_Integer]:=HallGrid[#,num,level]&;


(* ::Text:: *)
(*Auxiliary functions*)


(* ::Input::Initialization:: *)
"getPosition[\*StyleBox[\"u\", FontSlant->Italic],\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic]] get position of element \*StyleBox[\"u\", FontSlant->Italic] in hallBasis[\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic]]."
"applyGenericRule[\*StyleBox[\"rule\", FontSlant->Italic],\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic]] applies \*StyleBox[\"rule\", FontSlant->Italic] to a generic matrix expressed in hallBasis[\*StyleBox[\"num\", FontSlant->Italic],\*StyleBox[\"level\", FontSlant->Italic]]."
"zonedMatrix[\*StyleBox[\"var\", FontSlant->Italic],\*StyleBox[\"dimRow\", FontSlant->Italic],\*StyleBox[\"dimCol\", FontSlant->Italic],\*StyleBox[\"dimColZone\", FontSlant->Italic]] returns a matrix \*StyleBox[\"dimRow\", FontSlant->Italic] \[Times] \*StyleBox[\"dimCol\", FontSlant->Italic] whose \*StyleBox[\"dimColZone\", FontSlant->Italic] first columns are generic and the rest is null."

"zonedMatrix[\*StyleBox[\"var\", FontSlant->Italic],\*StyleBox[\"dim\", FontSlant->Italic],\*StyleBox[\"dimColZone\", FontSlant->Italic]] returns a matrix \*StyleBox[\"dim\", FontSlant->Italic] \[Times] \*StyleBox[\"dim\", FontSlant->Italic] whose \*StyleBox[\"dimColZone\", FontSlant->Italic] first columns are generic and the rest is null."


(* ::Input::Initialization:: *)
getPosition[u_,num_,level_]:=FirstPosition[HallBasisUntilLevel[num,level],u][[1]];
applyGenericRule[rule_,num_Integer,level_Integer]:=Module[{mat=GenericMatrix[HallBasisUntilLevelDimension[num,level],$hallMatrixVar]},mat/.rule[mat,num,level][[1]]];
zonedMatrix[var_,dimRow_Integer,dimCol_Integer,dimColZone_Integer]:=Join[GenericMatrix[dimRow,dimColZone,var],NullMatrix[dimRow,dimCol-dimColZone],2];
zonedMatrix[var_,dim_Integer,dimColZone_Integer]:=zonedMatrix[var,dim,dim,dimColZone];


(* ::Text:: *)
(*Derivations*)


(* ::Input::Initialization:: *)
getDerivationRule[mat_List,num_Integer,level_Integer]:=Module[{base=HallBasisUntilLevel[num,level],eq1=Transpose[mat[[All,num+1;;]]],dim,eq21,eq22,eq31,eq32},
dim=Length[base];
eq21=mat . Transpose[Table[getCanonicCoordinates[base[[i,1]],num,level],{i,num+1,dim}]];
eq22=mat . Transpose[Table[getCanonicCoordinates[base[[i,2]],num,level],{i,num+1,dim}]];
eq31=Table[Sum[eq21[[j,i-num]]*getCoordinates[{base[[j]],base[[i,2]]},num,level],{j,dim}],{i,num+1,dim}];
eq32=Table[Sum[eq22[[j,i-num]]*getCoordinates[{base[[i,1]],base[[j]]},num,level],{j,dim}],{i,num+1,dim}];
Solve[eq1==eq31+eq32]];
genericDerivation[num_Integer,level_Integer]:=applyGenericRule[getDerivationRule,num,level];

applyDerivation[mat_List,num_Integer,level_Integer]:=Module[{base=HallBasisUntilLevel[num,level],auxMat=mat,dim,i,j},
dim=Length[base];Table[auxMat[[All,i]]=Sum[auxMat[[j]] . getCanonicCoordinates[base[[i,1]],num,level]*getCoordinates[{base[[j]],base[[i,2]]},num,level]+auxMat[[j]] . getCanonicCoordinates[base[[i,2]],num,level]*getCoordinates[{base[[i,1]],base[[j]]},num,level],{j,dim}]
,{i,num+1,dim}];
auxMat];
genericDerivationFast[num_Integer,level_Integer]:=applyDerivation[zonedMatrix[$hallMatrixVar,HallBasisUntilLevelDimension[num,level],num],num,level];


(* ::Text:: *)
(*Automorphism*)


(* ::Input::Initialization:: *)
getAutomorphismRule[mat_List,num_Integer,level_Integer]:=Module[{base=HallBasisUntilLevel[num,level],dim},
dim=Length[base];
Solve[Table[mat[[All,i]]==Sum[mat[[j,getPosition[base[[i,1]],num,level]]]*mat[[k,getPosition[base[[i,2]],num,level]]]*getCoordinates[{base[[j]],base[[k]]},num,level],{j,dim},{k,dim}],{i,num+1,dim}]]];
genericAutomorphism[num_Integer,level_Integer]:=applyGenericRule[getAutomorphismRule,num,level];

applyAutomorphism[mat_List,num_Integer,level_Integer]:=Module[{base=HallBasisUntilLevel[num,level],auxMat=mat,dim},
dim=Length[base];
Table[auxMat[[All,i]]=Sum[auxMat[[j,getPosition[base[[i,1]],num,level]]]*auxMat[[k,getPosition[base[[i,2]],num,level]]]*getCoordinates[{base[[j]],base[[k]]},num,level],{j,dim},{k,dim}],{i,num+1,dim}];
auxMat];
genericAutomorphismFast[num_Integer,level_Integer]:=applyAutomorphism[zonedMatrix[$hallMatrixVar,HallBasisUntilLevelDimension[num,level],num],num,level];


(* ::Text:: *)
(*Matrices E, N and H*)


(* ::Input::Initialization:: *)
mE[i_Integer,num_Integer,level_Integer]:=Module[{preDim=HallBasisUntilLevelDimension[num,i-1],curDim=HallBasisLevelDimension[num,i],dim=HallBasisUntilLevelDimension[num,level]},DiagonalMatrix[Join[Table[0,preDim],Table[1,curDim],Table[0,dim-preDim-curDim]]]]/;i<=level;
mH[mat_List,num_Integer,level_Integer]:=Sum[mE[i,num,level] . mat . mE[i,num,level],{i,level}];
mN[mat_List,num_Integer,level_Integer]:=applyAutomorphism[(IdentityMatrix[HallBasisUntilLevelDimension[num,level]]+Sum[mE[j,num,level] . mat . mE[k,num,level],{j,2,level},{k,j-1}])];
mH[num_Integer,level_Integer]:=mH[GenericMatrix[HallBasisUntilLevelDimension[num,level],$hallMatrixVar],num,level];
mN[num_Integer,level_Integer]:=mN[GenericMatrix[HallBasisUntilLevelDimension[num,level],$hallMatrixVar],num,level];


(* ::Text:: *)
(*Hat operator*)


(* ::Input::Initialization:: *)
getHat[mat_]:=Module[{num=Length[mat],dim, auxMat},
dim=HallBasisUntilLevelDimension[num,2];
auxMat=Table[If[Max[i,j]<=num,mat[[i,j]],0],{i,dim},{j,dim}];
applyAutomorphism[auxMat,num,2][[num+1;;,num+1;;]]];
getHatFast[mat_]:=Minors[mat,2];


End[]
EndPackage[]
