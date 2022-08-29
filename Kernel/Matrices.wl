(* ::Package:: *)

BeginPackage["LieFunctions`Matrices`"]


SymbolQ::usage = "SymbolQ[expr] checks if expr is a symbol."
UndefinedSymbolQ::usage = "UndefinedSymbolQ[expr] checks if expr is an undefined symbol."
PositiveIntegerQ::usage = "PositiveIntegerQ[expr] checks if expr is a positive integer."
CubicMatrixQ::usage = "CubicMatrixQ[expr] checks if expr is a balanced list of matrices."
GenericMatrix::usage = "GenericMatrix[dim, var] creates a square matrix of dimension dim\[ThinSpace]\[Times]\[ThinSpace]dim using var as its indexed variable.
GenericMatrix[rows, columns, var] creates a matrix of dimension rows\[ThinSpace]*\[ThinSpace]columns using var as its indexed variable."
NullMatrix::usage = "NullMatrix[dim] creates a null matrix of dimension dim\[ThinSpace]\[Times]\[ThinSpace]dim.
NullMatrix[rows, columns] creates a null matrix of dimension rows\[ThinSpace]\[Times]\[ThinSpace]columns."
GenericSymmetricMatrix::usage = "GenericSymmetricMatrix[dim, var] creates a symmetric matrix of dimension dim\[ThinSpace]\[Times]\[ThinSpace]dim using var as its indexed variable."
GenericSkewSymmetricMatrix::usage = "GenericSkewSymmetricMatrix[dim, var] creates a skew-symmetric matrix of dimension dim\[ThinSpace]\[Times]\[ThinSpace]dim using var as its indexed variable."
EC::usage = "EC[i, dim] gets the coordinates of the i-th element in the standard basis of dimension dim."
SymmetricQ::usage = "SymmetricQ[matrix] checks if matrix is a symmetric matrix."
SkewSymmetricQ::usage = "SkewSymmetricQ[matrix] checks if matrix is a skew-symmetric matrix."
NonDegenerateQ::usage = "NonDegenerateQ[matrix] checks if matrix is a non-degenerate matrix (maximum rank)."


(*
SymbolQ::usage="\!\(\*FormBox[InterpretationBox[\"\\\"SymbolQ[\\\\!\\\\(\\\\*StyleBox[\\\\\\\"expr\\\\\\\", \\\\\\\"TI\\\\\\\"]\\\\)] checks if \\\\!\\\\(\\\\*StyleBox[\\\\\\\"expr\\\\\\\", \\\\\\\"TI\\\\\\\"]\\\\) is a symbol.\\\"\", StringForm[\"SymbolQ[`1`] checks if `1` is a symbol.\", \"\\!\\(\\*StyleBox[\\\"expr\\\", \\\"TI\\\"]\\)\"], Rule[Editable, False]], TraditionalForm]\)";
UndefinedSymbolQ::usage="\!\(\*FormBox[InterpretationBox[\"\\\"UndefinedSymbolQ[\\\\!\\\\(\\\\*StyleBox[\\\\\\\"expr\\\\\\\", \\\\\\\"TI\\\\\\\"]\\\\)] checks if \\\\!\\\\(\\\\*StyleBox[\\\\\\\"expr\\\\\\\", \\\\\\\"TI\\\\\\\"]\\\\) is an undefined symbol.\\\"\", StringForm[\"UndefinedSymbolQ[`1`] checks if `1` is an undefined symbol.\", \"\\!\\(\\*StyleBox[\\\"expr\\\", \\\"TI\\\"]\\)\"], Rule[Editable, False]], TraditionalForm]\)";
PositiveIntegerQ::usage="\!\(\*FormBox[InterpretationBox[\"\\\"\<PositiveIntegerQ[\!\(\*\
StyleBox[\\\\\\\"expr\\\\\\\", \\\\\\\"TI\\\\\\\"]\)] checks if \
\!\(\*StyleBox[\\\\\\\"expr\\\\\\\", \\\\\\\"TI\\\\\\\"]\) is a \
positive integer.\>\\\"\",
StringForm[\"PositiveIntegerQ[`1`] checks if `1` is a positive \
integer.\", \"\!\(\*StyleBox[\\\"expr\\\", \\\"TI\\\"]\)\"],\n\
Editable->False],
TraditionalForm]\)";
CubicMatrixQ::usage="\!\(\*FormBox[InterpretationBox[\"\\\"\<CubicMatrixQ[\!\(\*StyleBox[\
\\\\\\\"expr\\\\\\\", \\\\\\\"TI\\\\\\\"]\)] checks if \
\!\(\*StyleBox[\\\\\\\"expr\\\\\\\", \\\\\\\"TI\\\\\\\"]\) is a \
balanced list of matrices.\>\\\"\",
StringForm[\"CubicMatrixQ[`1`] checks if `1` is a balanced list of \
matrices.\", \"\!\(\*StyleBox[\\\"expr\\\", \\\"TI\\\"]\)\"],\n\
Editable->False],
TraditionalForm]\)";
GenericMatrix::usage="\!\(\*FormBox[InterpretationBox[\"\\\"\<GenericMatrix[\!\(\*StyleBox[\
\\\\\\\"dim\\\\\\\", \\\\\\\"TI\\\\\\\"]\), \
\!\(\*StyleBox[\\\\\\\"var\\\\\\\", \\\\\\\"TI\\\\\\\"]\)] creates a \
square matrix of dimension \!\(\*StyleBox[\\\\\\\"dim\\\\\\\", \\\\\\\
\"TI\\\\\\\"]\)\[ThinSpace]\[Times]\[ThinSpace]\!\(\*StyleBox[\\\\\\\"\
dim\\\\\\\", \\\\\\\"TI\\\\\\\"]\) using \!\(\*StyleBox[\\\\\\\"var\\\
\\\\\", \\\\\\\"TI\\\\\\\"]\) as its indexed variable.\>\\\"\",
StringForm[\"GenericMatrix[`1`, `2`] creates a square matrix of \
dimension `1`\[ThinSpace]\[Times]\[ThinSpace]`1` using `2` as its \
indexed variable.\", \"\!\(\*StyleBox[\\\"dim\\\", \\\"TI\\\"]\)\", \
\"\!\(\*StyleBox[\\\"var\\\", \\\"TI\\\"]\)\", \
\"\!\(\*StyleBox[\\\"rows\\\", \\\"TI\\\"]\)\", \
\"\!\(\*StyleBox[\\\"columns\\\", \\\"TI\\\"]\)\"],\n\
Editable->False],
TraditionalForm]\)
\!\(\*FormBox[InterpretationBox[\"\\\"\<GenericMatrix[\!\(\*StyleBox[\
\\\\\\\"rows\\\\\\\", \\\\\\\"TI\\\\\\\"]\), \
\!\(\*StyleBox[\\\\\\\"columns\\\\\\\", \\\\\\\"TI\\\\\\\"]\), \
\!\(\*StyleBox[\\\\\\\"var\\\\\\\", \\\\\\\"TI\\\\\\\"]\)] creates a \
matrix of dimension \!\(\*StyleBox[\\\\\\\"rows\\\\\\\", \\\\\\\"TI\\\
\\\\\"]\)\[ThinSpace]\[Times]\[ThinSpace]\!\(\*StyleBox[\\\\\\\"\
columns\\\\\\\", \\\\\\\"TI\\\\\\\"]\) using \
\!\(\*StyleBox[\\\\\\\"var\\\\\\\", \\\\\\\"TI\\\\\\\"]\) as its \
indexed variable.\>\\\"\",
StringForm[\"GenericMatrix[`3`, `4`, `2`] creates a matrix of \
dimension `3`\[ThinSpace]\[Times]\[ThinSpace]`4` using `2` as its \
indexed variable.\", \"\!\(\*StyleBox[\\\"dim\\\", \\\"TI\\\"]\)\", \
\"\!\(\*StyleBox[\\\"var\\\", \\\"TI\\\"]\)\", \
\"\!\(\*StyleBox[\\\"rows\\\", \\\"TI\\\"]\)\", \
\"\!\(\*StyleBox[\\\"columns\\\", \\\"TI\\\"]\)\"],\n\
Editable->False],
TraditionalForm]\)";
EC::usage="\!\(\*FormBox[InterpretationBox[\"\\\"\<EC[\!\(\*StyleBox[\\\\\\\"i\\\
\\\\\", \\\\\\\"TI\\\\\\\"]\), \!\(\*StyleBox[\\\\\\\"dim\\\\\\\", \\\
\\\\\"TI\\\\\\\"]\)] gets the coordinates of the \
\!\(\*StyleBox[\\\\\\\"i\\\\\\\", \\\\\\\"TI\\\\\\\"]\)-th element in \
the standard basis of dimension \!\(\*StyleBox[\\\\\\\"dim\\\\\\\", \
\\\\\\\"TI\\\\\\\"]\).\>\\\"\",
StringForm[\"EC[`1`, `2`] gets the coordinates of the `1`-th element \
in the standard basis of dimension `2`.\", \
\"\!\(\*StyleBox[\\\"i\\\", \\\"TI\\\"]\)\", \"\!\(\*StyleBox[\\\"dim\
\\\", \\\"TI\\\"]\)\"],\nEditable->False],
TraditionalForm]\)";
SymmetricQ::usage="\!\(\*FormBox[InterpretationBox[\"\\\"\<SymmetricQ[\!\(\*StyleBox[\\\
\\\\\"matrix\\\\\\\", \\\\\\\"TI\\\\\\\"]\)] checks if \
\!\(\*StyleBox[\\\\\\\"matrix\\\\\\\", \\\\\\\"TI\\\\\\\"]\) is a \
symmetric matrix.\>\\\"\",
StringForm[\"SymmetricQ[`1`] checks if `1` is a symmetric matrix.\", \
\"\!\(\*StyleBox[\\\"matrix\\\", \\\"TI\\\"]\)\"],\nEditable->False],
TraditionalForm]\)";
SkewSymmetricQ::usage="\!\(\*FormBox[InterpretationBox[\"\\\"\<SkewSymmetricQ[\!\(\*\
StyleBox[\\\\\\\"matrix\\\\\\\", \\\\\\\"TI\\\\\\\"]\)] checks if \
\!\(\*StyleBox[\\\\\\\"matrix\\\\\\\", \\\\\\\"TI\\\\\\\"]\) is a \
skew-symmetric matrix.\>\\\"\",
StringForm[\"SkewSymmetricQ[`1`] checks if `1` is a skew-symmetric \
matrix.\", \"\!\(\*StyleBox[\\\"matrix\\\", \\\"TI\\\"]\)\"],\n\
Editable->False],
TraditionalForm]\)";
NonDegenerateQ::usage="\!\(\*FormBox[InterpretationBox[\"\\\"\<NonDegenerateQ[\!\(\*\
StyleBox[\\\\\\\"matrix\\\\\\\", \\\\\\\"TI\\\\\\\"]\)] checks if \
\!\(\*StyleBox[\\\\\\\"matrix\\\\\\\", \\\\\\\"TI\\\\\\\"]\) is a \
non-degenerate matrix (maximum rank).\>\\\"\",
StringForm[\"NonDegenerateQ[`1`] checks if `1` is a non-degenerate \
matrix (maximum rank).\", \"\!\(\*StyleBox[\\\"matrix\\\", \
\\\"TI\\\"]\)\"],\nEditable->False],
TraditionalForm]\)";
UsageGenerator::usage="\!\(\*FormBox[InterpretationBox[\"\\\"\<UsageGenerator[\!\(\*\
StyleBox[\\\\\\\"strList\\\\\\\", \\\\\\\"TI\\\\\\\"]\), \
\!\(\*StyleBox[\\\\\\\"exprList\\\\\\\", \\\\\\\"TI\\\\\\\"]\)] \
generates a list of usages messages using \
\!\(\*StyleBox[\\\\\\\"strList\\\\\\\", \\\\\\\"TI\\\\\\\"]\) as \
definitions and \!\(\*StyleBox[\\\\\\\"exprList\\\\\\\", \\\\\\\"TI\\\
\\\\\"]\) as symbols.\>\\\"\",
StringForm[\"UsageGenerator[`1`, `2`] generates a list of usages \
messages using `1` as definitions and `2` as symbols.\", \
\"\!\(\*StyleBox[\\\"strList\\\", \\\"TI\\\"]\)\", \
\"\!\(\*StyleBox[\\\"exprList\\\", \\\"TI\\\"]\)\", \"\!\(\*StyleBox[\
\\\"str\\\", \\\"TI\\\"]\)\", \"\!\(\*StyleBox[\\\"expr\\\", \\\"TI\\\
\"]\)\"],\nEditable->False],
TraditionalForm]\)
\!\(\*FormBox[InterpretationBox[\"\\\"\<UsageGenerator[\!\(\*StyleBox[\
\\\\\\\"str\\\\\\\", \\\\\\\"TI\\\\\\\"]\), \
\!\(\*StyleBox[\\\\\\\"exprList\\\\\\\", \\\\\\\"TI\\\\\\\"]\)] \
generates a single usage message using \!\(\*StyleBox[\\\\\\\"str\\\\\
\\\", \\\\\\\"TI\\\\\\\"]\) as definition and \
\!\(\*StyleBox[\\\\\\\"exprList\\\\\\\", \\\\\\\"TI\\\\\\\"]\) as \
symbols.\>\\\"\",
StringForm[\"UsageGenerator[`3`, `2`] generates a single usage \
message using `3` as definition and `2` as symbols.\", \
\"\!\(\*StyleBox[\\\"strList\\\", \\\"TI\\\"]\)\", \
\"\!\(\*StyleBox[\\\"exprList\\\", \\\"TI\\\"]\)\", \"\!\(\*StyleBox[\
\\\"str\\\", \\\"TI\\\"]\)\", \"\!\(\*StyleBox[\\\"expr\\\", \\\"TI\\\
\"]\)\"],\nEditable->False],
TraditionalForm]\)
\!\(\*FormBox[InterpretationBox[\"\\\"\<UsageGenerator[\!\(\*StyleBox[\
\\\\\\\"str\\\\\\\", \\\\\\\"TI\\\\\\\"]\), \
\!\(\*StyleBox[\\\\\\\"expr\\\\\\\", \\\\\\\"TI\\\\\\\"]\), \
\[Ellipsis]] generates a single usage message using \!\(\*StyleBox[\\\
\\\\\"str\\\\\\\", \\\\\\\"TI\\\\\\\"]\) as definition and \
\!\(\*StyleBox[\\\\\\\"expr\\\\\\\", \\\\\\\"TI\\\\\\\"]\) and so on \
as symbols.\>\\\"\",
StringForm[\"UsageGenerator[`3`, `4`, \[Ellipsis]] generates a single \
usage message using `3` as definition and `4` and so on as \
symbols.\", \"\!\(\*StyleBox[\\\"strList\\\", \\\"TI\\\"]\)\", \"\!\(\
\*StyleBox[\\\"exprList\\\", \\\"TI\\\"]\)\", \
\"\!\(\*StyleBox[\\\"str\\\", \\\"TI\\\"]\)\", \
\"\!\(\*StyleBox[\\\"expr\\\", \\\"TI\\\"]\)\"],\nEditable->False],
TraditionalForm]\)
\!\(\*FormBox[InterpretationBox[\"\\\"\<UsageGenerator[\!\(\*StyleBox[\
\\\\\\\"strList\\\\\\\", \\\\\\\"TI\\\\\\\"]\), \!\(\*StyleBox[\\\\\\\
\"expr\\\\\\\", \\\\\\\"TI\\\\\\\"]\), \[Ellipsis]] generates a list \
of usages using \!\(\*StyleBox[\\\\\\\"strList\\\\\\\", \
\\\\\\\"TI\\\\\\\"]\) as definitions and \
\!\(\*StyleBox[\\\\\\\"expr\\\\\\\", \\\\\\\"TI\\\\\\\"]\) and so on \
as symbols.\>\\\"\",
StringForm[\"UsageGenerator[`1`, `4`, \[Ellipsis]] generates a list \
of usages using `1` as definitions and `4` and so on as symbols.\", \
\"\!\(\*StyleBox[\\\"strList\\\", \\\"TI\\\"]\)\", \
\"\!\(\*StyleBox[\\\"exprList\\\", \\\"TI\\\"]\)\", \"\!\(\*StyleBox[\
\\\"str\\\", \\\"TI\\\"]\)\", \"\!\(\*StyleBox[\\\"expr\\\", \\\"TI\\\
\"]\)\"],\nEditable->False],
TraditionalForm]\)";
*)


(*
UsageGenerator["SymbolQ[`1`] checks if `1` is a symbol.","expr"]
UsageGenerator["UndefinedSymbolQ[`1`] checks if `1` is an undefined symbol.","expr"]
UsageGenerator["PositiveIntegerQ[`1`] checks if `1` is a positive integer.","expr"]
UsageGenerator["CubicMatrixQ[`1`] checks if `1` is a balanced list of matrices.","expr"]
UsageGenerator[{"GenericMatrix[`1`, `2`] creates a square matrix of dimension `1`\[ThinSpace]\[Times]\[ThinSpace]`1` using `2` as its indexed variable.","GenericMatrix[`3`, `4`, `2`] creates a matrix of dimension `3`\[ThinSpace]\[Times]\[ThinSpace]`4` using `2` as its indexed variable."},{"dim","var","rows","columns"}]
UsageGenerator["EC[`1`, `2`] gets the coordinates of the `1`-th element in the standard basis of dimension `2`.",{"i","dim"}]
UsageGenerator["SymmetricQ[`1`] checks if `1` is a symmetric matrix.","matrix"]
UsageGenerator["SkewSymmetricQ[`1`] checks if `1` is a skew-symmetric matrix.","matrix"]
UsageGenerator["NonDegenerateQ[`1`] checks if `1` is a non-degenerate matrix (maximum rank).","matrix"]
UsageGenerator[{"UsageGenerator[`1`, `2`] generates a list of usages messages using `1` as definitions and `2` as symbols.",
"UsageGenerator[`3`, `2`] generates a single usage message using `3` as definition and `2` as symbols.",
"UsageGenerator[`3`, `4`, \[Ellipsis]] generates a single usage message using `3` as definition and `4` and so on as symbols.",
"UsageGenerator[`1`, `4`, \[Ellipsis]] generates a list of usages using `1` as definitions and `4` and so on as symbols."},{"strList","exprList","str","expr"}]
*)


Begin["Private`"]


(* Auxiliar tests *)
SymbolQ = MatchQ[#, t_Symbol /; AtomQ[t]]&;
UndefinedSymbolQ = MatchQ[#, t_Symbol /; And[AtomQ[t], DownValues[t] === {}]]&;
PositiveIntegerQ = MatchQ[#, t_Integer /; t >= 1]&;
CubicMatrixQ = MatchQ[#, t_List /; And[Length[t] * {1, 1, 1} == Dimensions[t], And @@ Map[MatrixQ, t]]]&;


(* Generic matrix *)
Clear[GenericMatrix, GenericMatrixU];

GenericMatrix[dim_, var_] := GenericMatrixU[dim, dim, var] /;
And[
  If[PositiveIntegerQ[dim],
    True,
    Message[GenericMatrix::nDimension, dim, 1]; False
  ],
  If[UndefinedSymbolQ[var],
    True,
    Message[GenericMatrix::nValue, var, 2]; False
  ]
];

GenericMatrix[nRows_, nColumns_, var_] := GenericMatrixU[nRows, nColumns, var] /;
And[
  If[PositiveIntegerQ[nRows],
    True,
    Message[GenericMatrix::nRows, nRows, 1]; False
  ],
  If[PositiveIntegerQ[nColumns],
    True,
    Message[GenericMatrix::nColumns, nColumns, 2]; False
  ],
  If[UndefinedSymbolQ[var],
    True,
    Message[GenericMatrix::nValue, var, 3]; False
  ]
];

GenericMatrix::nDimension = "Argument `1` at position `2` which represents the dimension of the matrix is not a positive integer.";
GenericMatrix::nValue = "Argument `1` at position `2` is not an undefined symbol.";
GenericMatrix::nRows = "Argument `1` at position `2` which represents the number of rows of the matrix is not a positive integer.";
GenericMatrix::nColumns = "Argument `1` at position `2` which represents the number of columns of the matrix is not a positive integer.";
GenericMatrixU[dim_, var_] := GenericMatrixU[dim, dim, var];

GenericMatrixU[nRows_, nColumns_, var_] := Table[var[nColumns * (i - 1) + j], {i, nRows}, {j, nColumns}];


Clear[NullMatrix, NullMatrixU, GenericSymmetricMatrix, GenericSymmetricMatrixU, GenericSkewSymmetricMatrix, GenericSkewSymmetricMatrixU];

NullMatrix[dim_]:=NullMatrixU[dim, dim]/;If[PositiveIntegerQ[dim], True, Message[NullMatrix::nDimension,dim,1];False];
NullMatrix[nRows_, nColumns_]:=NullMatrixU[nRows, nColumns]/;
And[
  If[PositiveIntegerQ[nRows],
    True,
    Message[NullMatrix::nRows, nRows, 1];False
  ],
  If[PositiveIntegerQ[nColumns],
    True,
    Message[NullMatrix::nColumns, nColumns, 2];False
  ]
];
NullMatrix::nDimesion = GenericMatrix::nDimension;
NullMatrix::nRows = GenericMatrix::nRows;
NullMatrix::nColumns = GenericMatrix::nColumns;
NullMatrixU[nRows_, nColumns_]:=ConstantArray[0, {nRows, nColumns}];

GenericSymmetricMatrix[dim_, var_]:=GenericSymmetricMatrixU[dim, var]/;
And[
  If[PositiveIntegerQ[dim],
    True,
    Message[GenericSymmetricMatrix::nDimension, dim, 1]; False
  ],
  If[UndefinedSymbolQ[var],
    True,
    Message[GenericSymmetricMatrix::nValue, var, 2]; False
  ]
];
GenericSymmetricMatrix::nDimension = GenericMatrix::nDimension;
GenericSymmetricMatrix::nValue = GenericMatrix::nValue;
GenericSymmetricMatrixU[dim_, var_]:=Table[If[i<=j,var[((2dim-i)(i-1))/2+j],var[((2dim-j)(j-1))/2+i]],{i,dim},{j,dim}];

GenericSkewSymmetricMatrix[dim_, var_]:=GenericSkewSymmetricMatrix[dim, var]/;
And[
  If[PositiveIntegerQ[dim],
    True,
    Message[GenericSkewSymmetricMatrix::nDimension, dim, 1]; False
  ],
  If[UndefinedSymbolQ[var],
    True,
    Message[GenericSkewSymmetricMatrix::nValue, var, 2]; False
  ]
];
GenericSkewSymmetricMatrix::nDimension = GenericMatrix::nDimension;
GenericSkewSymmetricMatrix::nValue = GenericMatrix::nValue;
GenericSkewSymmetricMatrixU[dim_, var_]:=Table[If[i<j,var[((2dim-i)(i-1))/2+j-i],If[i>j,-var[((2dim-j)(j-1))/2+i-j],0]],{i,dim},{j,dim}];


(* Canonic vector *)
Clear[EC, ECU];

EC[i_, dim_] := ECU[i, dim] /;
And[
  If[PositiveIntegerQ[i],
    True,
    Message[EC::nPos, i, 1]; False
  ],
  If[PositiveIntegerQ[dim],
    True,
    Message[EC::nDim, i, 2]; False
  ],
  If[i <= dim,
    True,
    Message[EC::nGreater, i, 1, dim, 2]; False
  ]
];

EC::nPos = "Argument `1` at position `2` which represents the basis position is not a positive integer.";
EC::nDim = "Argument `1` at position `2` which represents the dimension is not a positive integer.";
EC::nGreater = "Argument `3` at position `4` which represents the dimension is not greater or equal than argument `1` at position `2` which represents the basis position.";

ECU[i_, dim_] := Array[Boole[i == #]&, dim];


(* Conditions to matrices - Symmetry *)
Clear[SymmetricQ, SkewSymmetricQ, SymmetricQU,
   SkewSymmetricQU];

SymmetricQ[matrix_] := SymmetricQU[matrix] /; If[SquareMatrixQ[matrix],
  True,
  Message[SymmetricQ::nMatrix, matrix]; False
];

SkewSymmetricQ[matrix_] := SkewSymmetricQU[matrix] /; If[SquareMatrixQ[matrix],
  True,
  Message[SkewSymmetricQ::nMatrix, matrix]; False
];

SymmetricQ::nMatrix = SkewSymmetricQ::nMatrix = "Argument `1` is not a square matrix.";

SymmetricQU[matrix_] := Reduce[Map[# == 0&, Flatten[matrix - Transpose[matrix]]]];
SkewSymmetricQU[matrix_] := Reduce[Map[# == 0&, Flatten[matrix + Transpose[matrix]]]];


(* Conditions to matrices - Non-degeneration *)

Clear[NonDegenerateQ, NonDegenerateQU];

NonDegenerateQ[matrix_] := NonDegenerateQU[matrix] /; If[SquareMatrixQ[matrix],
  True,
  Message[SymmetricQ::nMatrix, matrix]; False
];

NonDegenerateQ::nMatrix = "Argument `1` is not a square matrix.";

NonDegenerateQU[matrix_] := Reduce[Det[matrix] != 0];


End[]
EndPackage[]
