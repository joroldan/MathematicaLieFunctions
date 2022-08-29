(* ::Package:: *)

BeginPackage["LieFunctions`Chains`"]

SL2ModuleSizes::usage = "SL2ModuleSizes[n, \[Ellipsis]] shows thex modules sizes of the \!\(\*SubscriptBox[\(\[GothicS]\[GothicL]\), \(2\)]\) chain with the given modules up to 4.";
SL2ModuleDimensions::usage = "SL2ModuleDimensions[n, \[Ellipsis]] shows the modules sizes of the \!\(\*SubscriptBox[\(\[GothicS]\[GothicL]\), \(2\)]\) chain with the given modules up to 4.";
SL2ChainDescription::usage="SL2ChainDescription[n, \[Ellipsis]] shows the structure of the \!\(\*SubscriptBox[\(\[GothicS]\[GothicL]\), \(2\)]\) chain with the given modules up to 4."
SL2ChainDimensionList::usage = "SL2ChainDimensionList[n, \[Ellipsis]] shows the total dimension and each link's dimension of the \!\(\*SubscriptBox[\(\[GothicS]\[GothicL]\), \(2\)]\) chain with the given modules up to 4.";
SL2ChainGraph::usage = "SL2ChainGraph[n, \[Ellipsis]] shows the ideal graph associated to the \!\(\*SubscriptBox[\(\[GothicS]\[GothicL]\), \(2\)]\) chain with the given modules up to 4.";
SL2AdjointList::usage = "SL2AdjointList lists the adjoint of \!\(\*SubscriptBox[\(\[GothicS]\[GothicL]\), \(2\)]\).";
VM::usage="VM[n] list the module V(n) standard basis";
SL2Module::usage="SL2Module[coords_, elto_] given the coordinates coords of an sl2 element finds its action over elto.";
Transvection::usage="Transvection[f, g, k, n, m] finds the value of transvection (\[Square], \[Square])\!\(\*SubscriptBox[\(\[InvisiblePrefixScriptBase]\), \(k\)]\):\[ThinSpace]V(n)\[Cross]\[ThinSpace]V(m)\[Rule]\[ThinSpace]V(n+m-2k) for (f, g)\!\(\*SubscriptBox[\(\[InvisiblePrefixScriptBase]\), \(k\)]\).
Transvection[f, g, k] finds the value of transvection (\[Square], \[Square])\!\(\*SubscriptBox[\(\[InvisiblePrefixScriptBase]\), \(k\)]\):\[ThinSpace]V(n)\[Cross]\[ThinSpace]V(m)\[Rule]\[ThinSpace]V(n+m-2k) for (f, g)\!\(\*SubscriptBox[\(\[InvisiblePrefixScriptBase]\), \(k\)]\) infering the values of n and m from f and g.";
PolynomialDegree::usage = "PolynomialDegree[pol] finds the degree of all variables in pol if it is uniform.";
SL2GetElementCoordinates::usage = "SL2GetElementCoordinates[elem, n, \[Ellipsis]] finds the coordinates of elem in the \!\(\*SubscriptBox[\(\[GothicS]\[GothicL]\), \(2\)]\) chain with the given modules up to 4.";
SL2ChainProduct::usage = "SL2ChainProduct[e1, e2, n, \[Ellipsis]] finds [e1, e2] product of the \!\(\*SubscriptBox[\(\[GothicS]\[GothicL]\), \(2\)]\) chain with the given modules up to 4.";
SL2ChainProductCoordinates::usage = "SL2ChainProductCoordinates[e1, e2, n, \[Ellipsis]] finds the coordinates of [e1, e2] product given e1 and e2 coordinates of the \!\(\*SubscriptBox[\(\[GothicS]\[GothicL]\), \(2\)]\) chain with the given modules up to 4.";
SL2ChainProductCoordinates::usage = "SL2ChainProductCoordinates[e1, e2, n, \[Ellipsis]] finds the coordinates of [e1, e2] product given e1 and e2 coordinates of the \!\(\*SubscriptBox[\(\[GothicS]\[GothicL]\), \(2\)]\) chain with the given modules up to 4.";
SL2ChainBasis::usage = "SL2ChainBasis[n, \[Ellipsis]] lists the basis of the \!\(\*SubscriptBox[\(\[GothicS]\[GothicL]\), \(2\)]\) chain with the given modules up to 4.";
SL2ChainAdjointList::usage="SL2ChainAdjointList[n, \[Ellipsis]] lists the adjoint matrices of the \!\(\*SubscriptBox[\(\[GothicS]\[GothicL]\), \(2\)]\) chain with the given modules up to 4.";


Begin["Private`"]
Needs["LieFunctions`Matrices`"]
Needs["LieFunctions`General`"]


Clear[SL2ChainAdjointList,SL2ChainAdjointList2];
SL2ChainAdjointList[param__]:=Table[Transpose[Table[SL2ChainProductCoordinates[el1,el2,param],{el2,SL2ChainBasis[param]}]],{el1,SL2ChainBasis[param]}];
SL2ChainAdjointList2[param__]:=Table[Transpose[Table[SL2GetElementCoordinates[SL2ChainProduct[el1,el2,param],param],{el2,SL2ChainBasis[param]}]],{el1,SL2ChainBasis[param]}];


Clear[SL2ModuleSizes,SL2ModuleDimensions,SL2ModuleDimensionsU];
SL2ModuleSizes[n_]:={n};
SL2ModuleSizes[n_,k_]:={n,2n-2k};
SL2ModuleSizes[n_,k_,r_]:={n,2n-2k,3n-2k-2r};
SL2ModuleSizes[n_,k_,r_,p_]:={n,2n-2k,3n-2k-2r,4n-2k-2r-2p};
SL2ModuleDimensions[param__]:=SL2ModuleDimensionsU[param]/;Length[{param}]<=4;
SL2ModuleDimensionsU[param__]:=SL2ModuleSizes[param]+1;


Clear[SL2ChainDescription,SL2ChainDescriptionU];
SL2ChainDescription[param__]:=SL2ChainDescriptionU[param]/;Length[{param}]<=4;
SL2ChainDescriptionU[param__]:=StringForm[StringJoin@@Prepend[Table["\[ThinSpace]\[CirclePlus]\[ThinSpace]V(``)",Length[{param}]],"\!\(\*SubscriptBox[\(\[GothicS]\[GothicL]\), \(2\)]\)"],##]&@@SL2ModuleSizes[param];


Clear[SL2ChainDimensionList,SL2ChainDimensionListU];
SL2ChainDimensionList[param__]:=SL2ChainDimensionListU[param]/;Length[{param}]<=4;
SL2ChainDimensionListU[param__]:={Plus@@#,#}&@Prepend[SL2ModuleDimensionsU[param],3];


Clear[sl2ModuleNames,vertexLabels,vertexCoordinates,SL2ChainGraph,SL2ChainGraphU];
sl2ModuleNames[i_,param__]:=StringForm[StringJoin@@Prepend[Table["\[ThinSpace]\[CirclePlus]\[ThinSpace]V(``)",i-1],"V(``)"],##]&@@SL2ModuleSizes[param][[-i;;]]/;
And[
If[PositiveIntegerQ[i],True,False],
If[i<=Length[{param}],True,False]];
vertexLabels[param__]:=Join[{1->"0",Length[{param}]+2->SL2ChainDescription[param]},Table[i->sl2ModuleNames[i-1,param],{i,2,Length[{param}]+1}]];
vertexCoordinates[param__]:=Table[i->{0,i},{i,0,Length[{param}]+2}];

Options[SL2ChainGraph]={VertexLabels->Default,VertexCoordinates->Default};
Options[SL2ChainGraphU]={VertexLabels->Default,VertexCoordinates->Default};
SL2ChainGraph[param__,opt:OptionsPattern[]]:=SL2ChainGraphU[param,opt]/;Length[{param}]<=4;
SL2ChainGraphU[param__,opt:OptionsPattern[]]:=Graph[Table[i<->i+1,{i,Length[{param}]+1}],##]&@@{
If[OptionValue[VertexLabels]===Default,VertexLabels->vertexLabels[param],Nothing],
If[OptionValue[VertexCoordinates]===Default,VertexCoordinates->vertexCoordinates[param],Nothing],opt};


SL2AdjointList={\!\(\*
TagBox[
RowBox[{"(", GridBox[{
{"0", "0", 
RowBox[{"-", "2"}]},
{"0", "0", "0"},
{"0", "1", "0"}
},
GridBoxAlignment->{"Columns" -> {{Center}}, "Rows" -> {{Baseline}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.7]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}], ")"}],
Function[BoxForm`e$, MatrixForm[BoxForm`e$]]]\),\!\(\*
TagBox[
RowBox[{"(", GridBox[{
{"0", "0", "0"},
{"0", "0", "2"},
{
RowBox[{"-", "1"}], "0", "0"}
},
GridBoxAlignment->{"Columns" -> {{Center}}, "Rows" -> {{Baseline}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.7]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}], ")"}],
Function[BoxForm`e$, MatrixForm[BoxForm`e$]]]\),\!\(\*
TagBox[
RowBox[{"(", GridBox[{
{"2", "0", "0"},
{"0", 
RowBox[{"-", "2"}], "0"},
{"0", "0", "0"}
},
GridBoxAlignment->{"Columns" -> {{Center}}, "Rows" -> {{Baseline}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.7]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}], ")"}],
Function[BoxForm`e$, MatrixForm[BoxForm`e$]]]\)};


Clear[VM];
VM[n_]:=Table[x^(n-i) y^i,{i,0,n}];


Clear[SL2Module];
SL2Module[{e1_,f1_,h1_},elto_]:=e1 x D[elto,y]+f1 y D[elto,x]+h1 x D[elto,x]-h1 y D[elto,y];


Clear[Transvection,PolynomialDegree]
PolynomialDegree[exp_]:=Module[{deg=Map[Function[monomial,Plus@@Map[Exponent[monomial,#]&,Variables[monomial]]],MonomialList[exp]]},If[SameQ@@deg,First[deg],Message[PolynomialDegree::error,exp]]];
PolynomialDegree::error = "The degrees in polynomial `1` are not uniform";
Transvection[0,g_,k_,n_,m_]=0;
Transvection[f_,0,k_,n_,m_]=0;
Transvection[f_,g_,0,n_,m_]:=f g;
Transvection[f_,g_,k_,n_,m_]:=0/;k>Min[n,m];
(*Transvection[f_,g_,k_,n_,m_]:=Transvection[f,g,k,n,m]=(m-k)!/m!*(n-k)!/n!*\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(a = 0\), \(k\)]\((
\*SuperscriptBox[\((\(-1\))\), \(a\)]*
\*FractionBox[\(k!\), \(\(a!\)*\(\((k - a)\)!\)\)]*D[f, {x, k - a}, {y, a}]*D[g, {x, a}, {y, k - a}])\)\);*)
Transvection[f_,g_,k_,n_,m_]:=((m-k)!*(n-k)!)/(m!n!)*Plus@@Table[(-1)^i*Binomial[k,i]*D[f,{x,k-i},{y,i}]*D[g,{x,i},{y,k-i}],{i,0,k}];
Transvection[f_,g_,k_]:=If[IntegerQ[k],Transvection[f,g,k,PolynomialDegree[f],PolynomialDegree[g]],UNK[f,g,k]];


Clear[getTransvectionList];
getTransvectionList[n_]:={};
getTransvectionList[n_,k_]:=({
\[Alpha][1,1]*EC[3,3]*Transvection[#1,#2,k]&
})/;OddQ[k];
getTransvectionList[n_,k_,r_]:=({
\[Alpha][1,1]*EC[3,4]*Transvection[#1,#2,k]&,Abort[],
\[Alpha][2,0]*EC[4,4]*Transvection[#1,#2,k+r-n/2]&,
\[Alpha][3,1]*EC[4,4]*Transvection[#1,#2,r]&
}/.{If[OddQ[k+r-n/2],Nothing,\[Alpha][2,0]->0]})/;OddQ[k];
getTransvectionList[n_,k_,r_,p_]:=({
\[Alpha][1,1]*EC[3,5]*Transvection[#1,#2,k]&,
\[Alpha][2,0]*EC[4,5]*Transvection[#1,#2,k+r-n/2]&,
\[Alpha][3,0]*EC[5,5]*Transvection[#1,#2,k-n+p+r]&,
\[Alpha][4,1]*EC[4,5]*Transvection[#1,#2,r]&,
\[Alpha][5,0]*EC[5,5]*Transvection[#1,#2,r+p-n/2]&,
\[Alpha][6,1]*EC[5,5]*Transvection[#1,#2,p]&,
\[Alpha][7,0]*EC[5,5]*Transvection[#1,#2,r+p-k]&
}/.{If[EvenQ[r-n/2],Nothing,\[Alpha][2,0]->0],
If[EvenQ[p-n+r],Nothing,\[Alpha][3,0]->0],
If[EvenQ[n],Nothing,\[Alpha][5,0]->0],
If[OddQ[r+p-k],Nothing,\[Alpha][7,0]->0]})/;OddQ[k];

Clear[iCoefficient,zArray,getModuleCoordinates,getTransvectionListC];
iCoefficient[elto_,1]:=elto;
iCoefficient[e1_,e2_]:=Coefficient[e1,e2]; (* Extends coefficient to independent terms (degree 0 monomials) *)

zArray[n_]:=ConstantArray[0,n]; (* Creates a zero array of length n *)

getModuleCoordinates[elto_,n_]:=Map[iCoefficient[elto,#]&,VM[n]]; (* Get the coordinates of some polynomial in V(n) *)

getTransvectionListC[n_]:={};

getTransvectionListC[n_,k_]:=(Apply[Function[{d1,d2},{
Function[{f,g},\[Alpha][1,1]*{zArray[3],zArray[d1],getModuleCoordinates[Transvection[f,g,k],d2-1]}]
}],SL2ModuleDimensions[n,k]])/;OddQ[k];

getTransvectionListC[n_,k_,r_]:=(Apply[Function[{d1,d2,d3},{
Function[{f,g},\[Alpha][1,1]*{zArray[3],zArray[d1],getModuleCoordinates[Transvection[f,g,k],d2-1],zArray[d3]}],
Function[{f,g},\[Alpha][2,0]*{zArray[3],zArray[d1],zArray[d2],getModuleCoordinates[Transvection[f,g,k+r-n/2],d3-1]}],
Function[{f,g},\[Alpha][3,1]*{zArray[3],zArray[d1],zArray[d2],getModuleCoordinates[Transvection[f,g,r],d3-1]}]
}],SL2ModuleDimensions[n,k,r]]/.{If[OddQ[k+r-n/2],Nothing,\[Alpha][2,0]->0]})/;OddQ[k];

getTransvectionListC[n_,k_,r_,p_]:=(Apply[Function[{d1,d2,d3,d4},{
Function[{f,g},\[Alpha][1,1]*{zArray[3],zArray[d1],getModuleCoordinates[Transvection[f,g,k],d2-1],zArray[d3],zArray[d4]}],
Function[{f,g},\[Alpha][2,0]*{zArray[3],zArray[d1],zArray[d2],getModuleCoordinates[Transvection[f,g,k+r-n/2],d3-1],zArray[d4]}],
Function[{f,g},\[Alpha][3,0]*{zArray[3],zArray[d1],zArray[d2],zArray[d3],getModuleCoordinates[Transvection[f,g,k-n+p+r],d4-1]}],
Function[{f,g},\[Alpha][4,1]*{zArray[3],zArray[d1],zArray[d2],getModuleCoordinates[Transvection[f,g,r],d3-1],zArray[d4]}],
Function[{f,g},\[Alpha][5,0]*{zArray[3],zArray[d1],zArray[d2],zArray[d3],getModuleCoordinates[Transvection[f,g,r+p-n/2],d4-1]}],
Function[{f,g},\[Alpha][6,1]*{zArray[3],zArray[d1],zArray[d2],zArray[d3],getModuleCoordinates[Transvection[f,g,p],d4-1]}],
Function[{f,g},\[Alpha][7,0]*{zArray[3],zArray[d1],zArray[d2],zArray[d3],getModuleCoordinates[Transvection[f,g,r+p-k],d4-1]}]
}],SL2ModuleDimensions[n,k,r,p]]/.{If[EvenQ[r-n/2],Nothing,\[Alpha][2,0]->0],
If[EvenQ[p-n+r],Nothing,\[Alpha][3,0]->0],
If[EvenQ[n],Nothing,\[Alpha][5,0]->0],
If[OddQ[r+p-k],Nothing,\[Alpha][7,0]->0]})/;OddQ[k];

(*getTransveccionListC[n_,k_,r_,p_]:={
\[Alpha][1,1]*{zArray[3],zArray[n+1],GetModuleCoordinates[Transveccion[#1,#2,k],2n-2k],zArray[3n-2k-2r+1],zArray[4n-2k-2r-2p+1]}&,
\[Alpha][2,0]*{zArray[3],zArray[n+1],zArray[2n-2k+1],GetModuleCoordinates[Transveccion[#1,#2,k+r-n/2],3n-2k-2r],zArray[4n-2k-2r-2p+1]}&,
\[Alpha][3,0]*{zArray[3],zArray[n+1],zArray[2n-2k+1],zArray[3n-2k-2r+1],GetModuleCoordinates[Transveccion[#1,#2,k-n+p+r],4n-2k-2r-2p]}&,
\[Alpha][4,1]*{zArray[3],zArray[n+1],zArray[2n-2k+1],GetModuleCoordinates[Transveccion[#1,#2,r],3n-2k-2r],0}&,
\[Alpha][5,0]*{zArray[3],zArray[n+1],zArray[2n-2k+1],zArray[3n-2k-2r+1],GetModuleCoordinates[Transveccion[#1,#2,r+p-n/2],4n-2k-2r-2p]}&,
\[Alpha][6,1]*{zArray[3],zArray[n+1],zArray[2n-2k+1],zArray[3n-2k-2r+1],GetModuleCoordinates[Transveccion[#1,#2,p],4n-2k-2r-2p]}&,
\[Alpha][7,0]*{zArray[3],zArray[n+1],zArray[2n-2k+1],zArray[3n-2k-2r+1],GetModuleCoordinates[Transveccion[#1,#2,r+p-k],4n-2k-2r-2p]}&
}*)


Clear[getModuleList,getModuleListC];
getModuleList[param__]:=Map[Function[{e1,e2},
Prepend[
If[SL2Module[e1,e2]=!=0,
EC[#,Length[{param}]]*SL2Module[e1,e2],
zArray[Length[{param}]]
],
zArray[3]]]&,Range[Length[{param}]]];
(*GetModuleList[n_,k_,r_,p_]:={
{zArray[3],SL2Module[#1,#2],0,0,0}&,
{zArray[3],0,SL2Module[#1,#2],0,0}&,
{zArray[3],0,0,SL2Module[#1,#2],0}&,
{zArray[3],0,0,0,SL2Module[#1,#2]}&};*)

getModuleListC[param__]:=Map[Function[i,Function[{coords,elto},Prepend[Map[If[i==#,getModuleCoordinates[SL2Module[coords,elto],SL2ModuleSizes[param][[#]]],zArray[SL2ModuleDimensions[param][[#]]]]&,Range[Length[{param}]]],zArray[3]]]],Range[Length[{param}]]];
(*
GetModuleListC[n_,k_,r_,p_]:={
Function[{coords,elto},{zArray[3],GetModuleCoordinates[SL2Module[coords,elto]],zArray[#2],zArray[#3],zArray[#4]}],
Function[{coords,elto},{zArray[3],zArray[#1],GetModuleCoordinates[SL2Module[coords,elto]],zArray[#3],zArray[#4]}],
Function[{coords,elto},{zArray[3],zArray[#1],zArray[#2],GetModuleCoordinates[SL2Module[coords,elto]],zArray[#4]}],
Function[{coords,elto},{zArray[3],zArray[#1],zArray[#2],zArray[#3],GetModuleCoordinates[SL2Module[coords,elto]]}]}&@@SL2ModuleDimensions[n,k,r,p];
*)

(*
GetModuleListC[n_,k_,r_,p_]:={
{zArray[3],GetModuleCoordinates[SL2Module[#1,#2]],zArray[2n-2k+1],zArray[3n-2k-2r+1],zArray[4n-2k-2r-2p+1]}&,
{zArray[3],zArray[n+1],GetModuleCoordinates[SL2Module[#1,#2]],zArray[3n-2k-2r+1],zArray[4n-2k-2r-2p+1]}&,
{zArray[3],zArray[n+1],zArray[2n-2k+1],GetModuleCoordinates[SL2Module[#1,#2]],zArray[4n-2k-2r-2p+1]}&,
{zArray[3],zArray[n+1],zArray[2n-2k+1],zArray[3n-2k-2r+1],GetModuleCoordinates[SL2Module[#1,#2]]}&};
*)


Clear[SL2GetElementCoordinates];
SL2GetElementCoordinates[elem_,param__]:=Flatten[Table[If[i==1,elem[[i]],getModuleCoordinates[elem[[i]],SL2ModuleSizes[param][[i-1]]]],{i,Length[elem]}]];


Clear[chainProductO,SL2ChainProduct, SL2ChainProductCoordinates]
chainProductO[LP_,{p1_},{},{s1_,t1_},{s2_,t2_}]:=LP[s1,s2]+p1[s1,t2]-p1[s2,t1];

chainProductO[LP_,{p1_,p2_},{p112_},{s1_,t1_,u1_},{s2_,t2_,u2_}]:=LP[s1,s2]+
p1[s1,t2]+p2[s1,u2]-
p1[s2,t1]-p2[s2,u1]+
p112[t1,t2];

chainProductO[LP_,{p1_,p2_,p3_},{p112_,p113_,p123_},{s1_,t1_,u1_,v1_},{s2_,t2_,u2_,v2_}]:=LP[s1,s2]+
p1[s1,t2]+p2[s1,u2]+p3[s1,v2]-
p1[s2,t1]-p2[s2,u1]-p3[s2,v1]+
p112[t1,t2]+p113[t1,t2]+
p123[t1,u2]-p123[t2,u1];

chainProductO[LP_,{p1_,p2_,p3_,p4_},{p112_,p113_,p114_,p123_,p124_,p134_,p224_},{s1_,t1_,u1_,v1_,w1_},{s2_,t2_,u2_,v2_,w2_}]:=LP[s1,s2]+
p1[s1,t2]+p2[s1,u2]+p3[s1,v2]+p4[s1,w2]-
p1[s2,t1]-p2[s2,u1]-p3[s2,v1]-p4[s2,w1]+
p112[t1,t2]+p113[t1,t2]+p114[t1,t2]+
p123[t1,u2]-p123[t2,u1]+
p124[t1,u2]-p124[t2,u1]+
p134[t1,v2]-p134[t2,v1]+p224[u1,u2];

(*ChainProduct[n_,k_,r_,p_,{s1_,t1_,u1_,v1_,w1_},{s2_,t2_,u2_,v2_,w2_}]:=
ChainProductO[{ProductCoordinates[#1, #2, SL2AdjointList],0,0,0,0}&,GetModuleList[n,k,r,p],GetTransveccionList[n,k,r,p],{s1,t1,u1,v1,w1},{s2,t2,u2,v2,w2}];*)
SL2ChainProduct[elto1_,elto2_,param__]:=
chainProductO[Prepend[zArray[Length[{param}]],ProductCoordinates[#1, #2, SL2AdjointList]]&,getModuleList[param],getTransvectionList[param],elto1,elto2];
SL2ChainProductCoordinates[elto1_,elto2_,param__]:=
Flatten[chainProductO[Prepend[Map[zArray,SL2ModuleDimensions[param]],ProductCoordinates[#1, #2, SL2AdjointList]]&,getModuleListC[param],getTransvectionListC[param],elto1,elto2]];


Clear[SL2ChainBasis]
SL2ChainBasis[param__]:=Join[
Table[Join[{EC[i,3]},zArray[Length[{param}]]],{i,3}],
Flatten[Table[Map[Join[{zArray[3]},EC[i,Length[{param}]]*#]&,VM[SL2ModuleSizes[param][[i]]]],{i,Length[{param}]}],1]];
(*
ChainBasis[n_,k_,r_,p_]:=Join[
Table[Join[{EC[i,3]},{0,0,0,0}],{i,3}],
Map[Join[{zArray[3]},{#,0,0,0}]&,VM[n]],
Map[Join[{zArray[3]},{0,#,0,0}]&,VM[2n-2k]],
Map[Join[{zArray[3]},{0,0,#,0}]&,VM[3n-2k-2r]],
Map[Join[{zArray[3]},{0,0,0,#}]&,VM[4n-2k-2r-2p]]];*
*)


End[]
EndPackage[]
