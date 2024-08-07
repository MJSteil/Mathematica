(* ::Package:: *)

(* ::Title:: *)
(*CompiledFunctionTools - Additional functions*)


(* ::Text:: *)
(*by M.J. Steil - msteil@theorie.ikp.physik.tu-darmstadt.de - TU Darmstadt - 2020.10.03 - Wolfram Mathematica 12.1.1.0 Student Edition*)


Needs["CompiledFunctionTools`"];
BeginPackage["CompiledFunctionTools`"]


(* ::Input::Initialization:: *)
ContainsMainEvaluateQ//Attributes={ReadProtected}
ContainsMainEvaluateQ[function_CompiledFunction,printQ_:True]:=Block[{string,bool},
	string=CompiledFunctionTools`CompilePrint@function;
	bool=!StringFreeQ[string,"MainEvaluate"];
	If[printQ,
		StringCases[string,RegularExpression["\n\\d+\t(.*)MainEvaluate(.*)"]]//StringJoin//Print
	];
	bool
]
ContainsMainEvaluateQ::usage="ContainsMainEvaluateQ[\!\(\*
StyleBox[\"function\",\nFontSlant->\"Italic\"]\)_CompiledFunction,\!\(\*
StyleBox[\"printQ\",\nFontSlant->\"Italic\"]\)_:True]: True if the CompiledFunction \!\(\*
StyleBox[\"function\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)contains MainEvaluate[] calls.";


(* ::Input::Initialization:: *)
DelayedCompile[args__]:=Inactive[Compile][args]//Activate
DelayedCompile::usage="DelayedCompile[\*
StyleBox[\(\!\(\*
StyleBox[\"args\",\nFontSlant->\"Italic\"]\)__\)]]: Compile after potential evaluations in \!\(\*
StyleBox[\"args\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\".\",\nFontSlant->\"Italic\"]\)";


EndPackage[]
