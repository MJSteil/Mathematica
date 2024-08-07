(* ::Package:: *)

(* ::Title:: *)
(*AutoSaveEvaluationNotebook*)


(* ::Text:: *)
(*by M.J. Steil - msteil@theorie.ikp.physik.tu-darmstadt.de - TU Darmstadt - 2022.10.06 - Wolfram Mathematica 13.0.1.0 Student Edition*)


BeginPackage["AutoSaveEvaluationNotebook`"]


Options[AutoSaveEvaluationNotebook]={"deleteNotebookOutputQ"->True,"maxBackups"->5,"minBackupInterval"->Quantity[5,"Minutes"],"saveBackupsHiddenQ"->True,"uuids"->{}};
AutoSaveEvaluationNotebook[OptionsPattern[]]:=Module[{nb,nbName,lastBackups,maxBackups,deltaT,backupName},
	If[!MemberQ[OptionValue["uuids"],"ExpressionUUID"/.NotebookInformation[]],
		Return[];
	];
	
	nbName=StringTake[NotebookFileName[],;;-4];
	lastBackups=Sort@FileNames[nbName<>"_backup*"];
	
	If[Length[lastBackups]>0,
		deltaT=DateObject[]-DateObject[StringReplace[
			Last@lastBackups,RegularExpression[".*backup(\\d\\d\\d\\d)-(\\d\\d)-(\\d\\d)T(\\d\\d)-(\\d\\d)-(\\d\\d).*"]->"$1-$2-$3T$4:$5:$6"]
		];
		If[deltaT<OptionValue["minBackupInterval"],
			Return[];
		];
	];
	
	maxBackups=OptionValue["maxBackups"];
	If[maxBackups===1,
		DeleteFile[lastBackups];,
		If[IntegerQ[maxBackups]&&maxBackups>1,
			If[Length[lastBackups]>=maxBackups,
				DeleteFile[lastBackups[[;;-maxBackups]]];
			];
		];
	];
	
	backupName=nbName<>"_backup"<>StringReplace[DateString["ISODateTime"],":"->"-"];
	PrintTemporary["Generating backup: "<>backupName<>" ..."];
	nb=NotebookPut[NotebookGet[EvaluationNotebook[]],Visible->False];
	If[OptionValue["deleteNotebookOutputQ"],
		FrontEndExecute[{FrontEndToken[nb,"DeleteGeneratedCells"]}];
	];
	
	NotebookSave[nb,backupName];
	If[OptionValue["saveBackupsHiddenQ"],
		RunProcess[{"cmd","/c","attrib +h "<>backupName<>".nb"}]
	];
	NotebookClose[nb];
]

AutoSaveEvaluationNotebook::usage = "Save backup copies of the current EvaluationNotebook.";

EnableAutoSaveEvaluationNotebookPre[]:=Module[{},
	SetOptions[AutoSaveEvaluationNotebook,
		"uuids"->DeleteDuplicates@Append[OptionValue[AutoSaveEvaluationNotebook,"uuids"],"ExpressionUUID"/.NotebookInformation[]]
	];
	$Pre=(AutoSaveEvaluationNotebook[];#)&;
]
EnableAutoSaveEvaluationNotebookPre::usage = "Add the current Notebook UUID to the ones for which AutoSaveEvaluationNotebook[] works.";

DisableAutoSaveEvaluationNotebookPre[]:=Module[{},
	SetOptions[AutoSaveEvaluationNotebook,
		"uuids"->DeleteCases[OptionValue[AutoSaveEvaluationNotebook,"uuids"],"ExpressionUUID"/.NotebookInformation[]]
	];
	$Pre=.;
]
DisableAutoSaveEvaluationNotebook::usage = "Remove the current Notebook UUID from the ones for which AutoSaveEvaluationNotebook[] works.";

EnableAutoSaveEvaluationNotebookPreRead[]:=Module[{},
	SetOptions[AutoSaveEvaluationNotebook,
		"uuids"->DeleteDuplicates@Append[OptionValue[AutoSaveEvaluationNotebook,"uuids"],"ExpressionUUID"/.NotebookInformation[]]
	];
	$PreRead=(AutoSaveEvaluationNotebook[];#)&;
]
EnableAutoSaveEvaluationNotebookPreRead::usage = "Add the current Notebook UUID to the ones for which AutoSaveEvaluationNotebook[] works.";

DisableAutoSaveEvaluationNotebookPreRead[]:=Module[{},
	SetOptions[AutoSaveEvaluationNotebook,
		"uuids"->DeleteCases[OptionValue[AutoSaveEvaluationNotebook,"uuids"],"ExpressionUUID"/.NotebookInformation[]]
	];
	$PreRead=.;
]
DisableAutoSaveEvaluationNotebookPreRead::usage = "Remove the current Notebook UUID from the ones for which AutoSaveEvaluationNotebook[] works.";

CleanUpBackups[]:=DeleteFile[FileNames[StringTake[NotebookFileName[],;;-4]<>"_backup*T*.nb"]]
CleanUpBackups::usage = "Delete auto-saved notebooks.";

CleanUpAutoSavesOnWindowClose[]:=Module[{actions},
	actions=NotebookEventActions/.Options[EvaluationNotebook[]];
	If[!ListQ[actions],
		actions={}
	];
	actions=actions~Join~{"WindowClose":>CleanUpBackups[]};
	SetOptions[EvaluationNotebook[],
		NotebookEventActions:>Evaluate[actions]
	];
]
CleanUpAutoSavesOnWindowClose::usage = "Delete auto-saved notebooks on Window Close.";


EndPackage[]
