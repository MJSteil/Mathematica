(* ::Package:: *)

(* ::Title:: *)
(*StylesheetTools*)


(* ::Text:: *)
(*by M.J. Steil - msteil@theorie.ikp.physik.tu-darmstadt.de - TU Darmstadt - 2022.08.31 - Wolfram Mathematica 13.0.1.0 Student Edition*)


BeginPackage["StylesheetTools`"]


GetStyleDefinitions[st`nb_NotebookObject]:=Options[st`nb,StyleDefinitions][[1,2]]
GetStyleDefinitions[st`file_String]:=Module[{st`nb,st`style},
	st`nb=NotebookOpen[st`file,Visible->False];
	st`style=Options[st`nb,StyleDefinitions][[1,2]];
	NotebookClose[st`nb];
	Return[st`style];
]
GetStyleDefinitions[]:=GetStyleDefinitions[EvaluationNotebook[]]

GetStyleDefinitions::usage = "GetStyleDefinitions[st`nb_NotebookObject:EvaluationNotebook[]]: Get StyleDefinitions from NotebookObject `st`nb` \[LongDash] defaults to EvaluationNotebook[].\nGetStyleDefinitions[st`file_String]: Get StyleDefinitions from `st`file`.";


SetStyleDefinitions[st`nb_NotebookObject,st`style_Notebook,st`saveQ_:True,st`closeQ_:True]:=Block[{},
	SetOptions[st`nb,StyleDefinitions->st`style];
	If[st`saveQ,
		NotebookSave[st`nb];
		If[st`closeQ,
			NotebookClose[st`nb];
		];
	];
];
SetStyleDefinitions[st`style_Notebook]:=SetStyleDefinitions[EvaluationNotebook[],st`style,False,False]

SetStyleDefinitions[st`nbList_List,st`style_Notebook,st`saveQ_:True,st`closeQ_:True]:=Map[
	SetStyleDefinitions[If[Head[#]=!=NotebookObject,NotebookOpen[#,Visible->False],#],st`style,st`saveQ,st`closeQ]&,
st`nbList]

SetStyleDefinitions::usage = "SetStyleDefinitions[st`nb_NotebookObject,st`style_Notebook,st`saveQ_:True,st`closeQ_:True]: Set StyleDefinitions of `st`nb` to `st`style` and save `st`nb` if `st`saveQ` and close `st`nb` if `st`closeQ`.\nSetStyleDefinitions[st`style_Notebook]:=SetStyleDefinitions[EvaluationNotebook[],st`style,False,False]\nSetStyleDefinitions[nbList_List,st`style_Notebook,st`saveQ_:True,st`closeQ_:True]: Applies SetStyleDefinitions to all elements of `nbList`.";


SetStyleDefinitionsFromFile[st`file_:"D:/Github/Mathematica/Templates/notebook_plain.nb",st`nb_:EvaluationNotebook[],st`saveQ_:False,st`closeQ_:False]:=Module[{st`style},
	st`style=GetStyleDefinitions[st`file];
	SetStyleDefinitions[EvaluationNotebook[],st`style,st`saveQ,st`closeQ];
]
SetStyleDefinitionsFromFile::usage = "SetStyleDefinitionsFromFile[st`file_,st`nb_:EvaluationNotebook[],st`saveQ_:False,st`closeQ_:False]: Set StyleDefinitions directly from `st`file`.";


EndPackage[]
