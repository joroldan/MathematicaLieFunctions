(* ::Package:: *)

BeginPackage["LieFunctions`Utilities`"]
UsageGenerator::usage = "UsageGenerator[strList, exprList] generates a list of usages messages using strList as definitions and exprList as symbols.
UsageGenerator[str, exprList] generates a single usage message using str as definition and exprList as symbols.
UsageGenerator[str, expr, \[Ellipsis]] generates a single usage message using str as definition and expr and so on as symbols.
UsageGenerator[strList, expr, \[Ellipsis]] generates a list of usages using strList as definitions and expr and so on as symbols."

GenerateUsage::usage = "GenerateUsage[funName, varList, definition] generates usage for a function named funName with variables varList and with description definition.
GenerateUsage[funName, varList, definition, moreVars] generates usage for a function named funName with variables varList and with description definition. All variables included in moreVars will be substituted in definition.";
TeXToMatrix::usage = "TeXToMatrix[string, mode] converts string tex to Mathematica matrix. When mode=1 (default) converts arrays, when mode=2 converts pmatrix."
ReindexVariable::usage = "ReindexVariable[var,expresion] resets all variables var indices in expresion."
NumberOfVariables::usage = "NumberOfVariables[expresion] gives the number of different variables in expresion."
MaxIndexOfVariable::usage = "MaxIndexOfVariable[var,expresion] gives the maximum index of variable var in expresion."
IndicesOfVariable::usage = "IndicesOfVariable[var,expresion] gives the list of indices of variable var involved in expresion."
MinIndexOfVariable::usage = "MinIndexOfVariable[var,expresion] gives the minimum index of variable var in expresion."


Begin["Private`"]


(* ::Text:: *)
(*Generate pretty usage for functions*)


(* ::Input::Initialization:: *)
GenerateUsage[funName_,varList_,definition_]:=GenerateUsage[funName,varList,definition,{}];
GenerateUsage[funName_,varList_,definition_,moreVars_]:=prepareName[funName,varList]<>prepareDefinition[definition,Join[varList,moreVars]];
prepareName[funName_,{}]:=funName<>" ";
prepareName[funName_,varList_]:="\!\(\*RowBox[{\""<>funName<>"\",\"[\","<>prepareVars[varList]<>"\"]\"}]\) ";
prepareVars[varList_]:=StringRiffle[Map[ "StyleBox[\""<>#<>"\", \"TI\"], "&,varList],"\",\", "];
prepareDefinition[definition_,varList_]:=StringReplace[definition,Map[RegularExpression["\\b"<>#<>"\\b"]-> "\!\(\*StyleBox[\""<>#<>"\", \"TI\"]\)"&,varList]];


(* Usage generator *)
Clear[UsageGenerator];
UsageGenerator[Private`elto_String,Private`list2_List]:=UsageGenerator[{Private`elto},Private`list2];
UsageGenerator[Private`elto_String,Private`arb___]:=UsageGenerator[Private`elto,{Private`arb}];
UsageGenerator[Private`list1_List,Private`arb___]:=UsageGenerator[Private`list1,{Private`arb}];
UsageGenerator[Private`list1_List,Private`list2_List]:=
StringDrop[StringJoin@@Table[ToString[StringForm@@Prepend[Map["\*StyleBox[\""<>#<>"\", \"TI\"]"&,Private`list2],elto]]<>"\n",{elto,Private`list1}],-1];


(* ::Text:: *)
(*Convert tex syntax string to Mathematica matrix (reverse is already available by default)*)


TeXToMatrix[string_, mode_.] := Switch[mode,
  1, Map[ToExpression, 
       InputForm[ToExpression[string, TeXForm]][[1, 1, 1, All, All, 1, 1, 1]], {2}],
  2, Map[ToExpression,
       InputForm[ToExpression[string, TeXForm]][[1, 1, 1, All, All, 1, 1, 2, 1]], {2}]
];
Default[TeXToMatrix] = 1;
SetAttributes[TeXToMatrix, ReadProtected]


ReindexVariable[var_, expresion_] := expresion /. MapIndexed[#1 -> var[
  #2[[1]]]&, Sort[Select[Variables[expresion], #[[0]] == var&], #1[[1]]
   < #2[[1]]&]];

ReindexVariable[var_][expresion_] := ReindexVariable[var, expresion];

NumberOfVariables[expresion_] := Length[Variables[expresion]];

MaxIndexOfVariable[var_, expresion_] :=
  Last[Sort[Select[Variables[expresion], #[[0]] == var&], #1[[1]] < #2[[1]]&]][[1]];

MaxIndexOfVariable[var_][expresion_] := MaxIndexOfVariable[var, expresion];

MinIndexOfVariable[var_, expresion_] := 
  First[Sort[Select[Variables[expresion], #[[0]] == var&], #1[[1]] < #2[[1]]&]][[1]];

MinIndexOfVariable[var_][expresion_] := MinIndexOfVariable[var, expresion];

IndicesOfVariable[var_, expresion_] := 
  Sort[Select[Variables[expresion], #[[0]] == var&][[All, 1]]];

IndicesOfVariable[var_][expresion_] := IndicesOfVariable[var, expresion];


End[]
EndPackage[]
