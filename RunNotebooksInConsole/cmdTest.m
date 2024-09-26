(* ::Package:: *)

(** Script to run file (.nb) without the frontend **)
(* Usage: Run this script using 'wolfram -script cmdTest.m' in the console *)
(* See also: https://mathematica.stackexchange.com/q/98549/42436 *)

file="cmdTest";
logfolder="log/";

(* Logfiles *)
timestamp=DateString[{"Year","Month","Day","_","Hour24","Minute"}];

outputlogfile = OpenWrite[logfolder<>file<>"_"<>timestamp<>"_output.log", FormatType -> OutputForm];
$PrePrint = (Write[outputlogfile, #]; #) &;

printlogfile = OpenWrite[logfolder<>file<>"_"<>timestamp<>"_print.log", FormatType -> StandardForm];
AppendTo[$Output, printlogfile];

messageslogfile = OpenWrite[logfolder<>file<>"_"<>timestamp<>"_messages.log", FormatType -> OutputForm];
AppendTo[$Messages, messageslogfile];

(* Evaluation *) 
SetOptions[First[$Output],FormatType->StandardForm];
UsingFrontEnd[NotebookEvaluate[file<>".nb",InsertResults->True]] 

(* Cleanup *)
Close[messageslogfile];$Messages=$Messages[[{1}]];
Close[printlogfile];$Output=$Output[[{1}]];
Close[outputlogfile];
