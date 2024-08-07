(* ::Package:: *)

(* ::Title:: *)
(*UpdateTaggedMethod*)


(* ::Text:: *)
(*by M.J. Steil - msteil@theorie.ikp.physik.tu-darmstadt.de - TU Darmstadt - 2022.08.31 - Wolfram Mathematica 13.0.1.0 Student Edition*)


BeginPackage["UpdateTaggedMethod`"]


FindTaggedMethods[utm`nb_NotebookObject]:=With[{FindTaggedMethods`s=Symbol[#]},
	If[0=!=Length@Flatten[Join[UpValues[FindTaggedMethods`s],DownValues[FindTaggedMethods`s],SubValues[FindTaggedMethods`s]]],#,Nothing]
]&/@NotebookTools`NotebookCellTags[utm`nb]
FindTaggedMethods[]:=FindTaggedMethods[EvaluationNotebook[]]
FindTaggedMethods::usage = "FindTaggedMethods[utm`nb_NotebookObject:EvaluationNotebook[]]: Find tags in `utm`nb` which have Up-, Sub-, and/or DownValues assigned to Symbols of the same name \[LongDash] Methods.";


UpdateTaggedMethod[utm`tag_String,utm`nbA_NotebookObject,utm`nbB_NotebookObject]:=Module[{cellA,cellB,versionA,versionB},
	{cellA,cellB}=Block[{},NotebookFind[#,utm`tag,All,CellTags,AutoScroll->False];NotebookRead[#]]&/@{utm`nbA,utm`nbB};
	If[cellA===$Failed||cellB===$Failed,Return[];];
	
	{versionA,versionB}=StringCases[ToString[#],RegularExpression["RowBox\\[\\{v(\\d), .(\\d*)\\}\\]"]->"$1.$2"]&/@{cellA,cellB};
	If[Length[versionA]=!=1||Length[versionB]=!=1,Return[];];
	{versionA,versionB}=ToExpression[First[#]]&/@{versionA,versionB};
	If[versionA===versionB,Return[];];
	If[versionA>versionB,
		Print[Row[{"Updating ",utm`tag," from v",versionB," in ",ToString@utm`nbB," to v",versionA," of ",ToString@utm`nbA,"."}]];
		NotebookFind[utm`nbB,utm`tag,All,CellTags];
		NotebookWrite[utm`nbB,cellA];,(*else*)
		Print[Row[{"Updating ",utm`tag," from v",versionA," in ",ToString@utm`nbA," to v",versionB," of ",ToString@utm`nbB,"."}]];
		NotebookFind[utm`nbA,utm`tag,All,CellTags];
		NotebookWrite[utm`nbA,cellB];
	]	
]

UpdateTaggedMethod[utm`tag_String,utm`fileA_String,utm`nbB_NotebookObject:EvaluationNotebook[],saveQ_:True,closeQ_:True]:=Module[{utm`nbA},
	utm`nbA=NotebookOpen[utm`fileA,Visible->False];
	UpdateTaggedMethod[utm`tag,utm`nbA,utm`nbB];
	If[saveQ,NotebookSave[utm`nbA]];
	If[closeQ,NotebookClose[utm`nbA]];
]

UpdateTaggedMethod::usage = "UpdateTaggedMethod[utm`tag_String,utm`nbA_NotebookObject,utm`nbB_NotebookObject]: Update Cell tagged with `utm`tag` in `utm`nbA` and `utm`nbB` based on version of the tagged Method.\n
UpdateTaggedMethod[utm`tag_String,utm`fileA_String,utm`nbB_NotebookObject:EvaluationNotebook[]]: Open `utm`fileA` to update Cell tagged with `utm`tag` between it and `utm`nbB`.";


UpdateTaggedMethods[utm`fileA_String]:=Module[{methods,utm`nbA,utm`nbB},
	utm`nbA=EvaluationNotebook[];
	utm`nbB=NotebookOpen[utm`fileA,Visible->False];
	methods=FindTaggedMethods[utm`nbA];
	If[Length[methods]===0,Return[];];
	Print[Row[{"Updating Methods: ",methods}]];
	UpdateTaggedMethod[#,utm`nbA,utm`nbB]&/@methods;
	NotebookSave[utm`nbB];
	NotebookClose[utm`nbB];
]
UpdateTaggedMethods::usage = "UpdateTaggedMethod[utm`fileA_String]: Update methods between EvaluationNotebook and `utm`filaA`.";


EndPackage[]
