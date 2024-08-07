(* ::Package:: *)

(* ::Title:: *)
(*Modules for General Relativity and Riemannian manifolds (GR)*)


(* ::Text:: *)
(*by M.J. Steil - msteil@theorie.ikp.physik.tu-darmstadt.de - TU Darmstadt - 2016.06.07 - Wolfram Mathematica 10.0.2.0 Student Edition*)


BeginPackage["GR`"]


(* ::Section::Closed:: *)
(*General methods*)


(* ::Subsection:: *)
(*Equation Manipulation*)


coef[exp_,coefList_,rule_:{},list_:True,simplifyLvL_:1]:=Module[{dim,i,expR,out,rest,simpl,PrintRed},
simpl=Function[{in},
Switch[simplifyLvL,0,in,1,Simplify[in],2,FullSimplify[in],_,in]
];

PrintRed=Function[{in},
Print[Style[in,Red]]
];

dim=Length@coefList;
expR=exp//.rule//simpl;
out=Table[coefList[[i]]*simpl[Coefficient[expR,coefList[[i]]]],{i,1,dim}];
rest=simpl[Expand[expR-Total[out]]];
(*If[IntegerQ[rest],"",PrintRed["Rest:"]PrintRed[rest]PrintRed["-------------------------------------"]];*)
If[list==False,Total[out]+rest,Join[out,{rest}]]
];
coef::usage = "[exp_,coef_,rule_:{},list_:False,simplifyLvL_:2]: Splits exp in a sum/list (for list=True) of its coefficients coef and a rest.";


oneSide=(Head[#][Subtract@@#,0]&)
oneSide::usage = "brings the equation to one sided form: eq==0";


(* ::Subsection:: *)
(*Matrix Generators*)


M2[f_,mod_:Function[{$x},$x],dim_:4]:=Array[mod[f[#1,#2]]&,{dim,dim}];
M3[f_,mod_:Function[{$x},$x],dim_:4]:=Array[mod[f[#1,#2,#3]]&,{dim,dim,dim}];
M4[f_,mod_:Function[{$x},$x],dim_:4]:=Array[mod[f[#1,#2,#3,#4]]&,{dim,dim,dim,dim}];

M2uT[exp_,mod_:Function[{$x},$x],dim_:4]:=Array[If[#1<=#2,mod[exp[#1,#2]],Null]&,{dim,dim}];

M2s[exp_,mod_:Function[{$x},$x],dim_:4]:=Module[{Msy,M},
Msy=M2uT[exp,mod];
M=Array[If[#1<=#2,Msy[[#1,#2]],Msy[[#2,#1]]]&,{dim,dim}]
]

M3antiSym[M_,dim_:4]:=1/3!Array[
M[[#1,#2,#3]]+M[[#2,#3,#1]]+M[[#3,#1,#2]]
-M[[#1,#3,#2]]-M[[#2,#1,#3]]-M[[#3,#2,#1]]
&,{dim,dim,dim}]
M3Sym[M_,dim_:4]:=1/3!Array[
M[[#1,#2,#3]]+M[[#2,#3,#1]]+M[[#3,#1,#2]]
+M[[#1,#3,#2]]+M[[#2,#1,#3]]+M[[#3,#2,#1]]
&,{dim,dim,dim}]



(* ::Subsection:: *)
(*Summations*)


contractSum[exp_,index_,dim_:4]:=Module[{sum,j},
sum=0;
For[j=1,j<=dim,j++,
sum+=exp/.index->j;
];
sum
]
contract[exp_,indicies_,dim_:4]:=ReleaseHold@Fold[contractSum[#1,#2,dim]&,exp,indicies]


(* ::Subsection:: *)
(*Symbolic Definitions and Identities*)


(* ::Text:: *)
(*Unset Summation Indices are set to m,n,l... and marked in cyan.*)


(* ::Subsubsection:: *)
(*Symbolic Placeholders*)


\[CapitalGamma]Text[i_,j_,k_]:=Subsuperscript[\[CapitalGamma],Row[{" ",j,k}],i]
dText[i_]:=Subscript["\[PartialD]",i]
goText[i_,j_]:=Superscript[g,Row[{i,j}]]
guText[i_,j_]:=Subscript[g,Row[{i,j}]]


red[exp_]:=Style[exp,Red]
green[exp_]:=Style[exp,Darker@Green]
blue[exp_]:=Style[exp,Blue]
magenta[exp_]:=Style[exp,Darker@Magenta]
cyan[exp_]:=Style[exp,Darker@Cyan]


(* ::Subsubsection:: *)
(*Definitions*)


\[CapitalGamma]Def[i_,j_,k_,s1_:cyan@m]:={\[CapitalGamma]Text[i,j,k],1/2 goText[i,s1],
Row[{dText[j],guText[s1,k]}],
Row[{dText[k],guText[s1,j]}],
-Row[{dText[s1],guText[j,k]}]
}

RMDef[a_,b_,c_,d_,s1_:cyan@m]:={Subsuperscript[R,Row[{" ",b,c,d}],a],Row[{
dText[c],\[CapitalGamma]Text[a,d,b]}],
-Row[{dText[d],\[CapitalGamma]Text[a,c,b]}],
\[CapitalGamma]Text[a,c,s1]\[CapitalGamma]Text[s1,d,b],
-\[CapitalGamma]Text[a,d,s1]\[CapitalGamma]Text[s1,c,b]
}
RicciDef[i_,k_,s1_:cyan@m,s2_:cyan@n]:={Row[{
Subscript["R",Row[{i,k}]]}],
Row[{dText[s1],\[CapitalGamma]Text[s1,i,k]}],
-Row[{dText[k],\[CapitalGamma]Text[s1,i,s1]}],
\[CapitalGamma]Text[s1,i,k]\[CapitalGamma]Text[s2,s1,s2],
-\[CapitalGamma]Text[s2,i,s1]\[CapitalGamma]Text[s1,k,s2]
}


(* ::Subsubsection:: *)
(*Identities*)


id868[k_,m_,l_,i_]:={
\[CapitalGamma]Text[k,m,l]goText[i,m],
-goText[m,k]\[CapitalGamma]Text[i,m,l],
-Row[{dText[l],goText[i,k]
}]}
id868b[i_,k_,l_,s1_:cyan@m]:={
Row[{dText[l],goText[i,k]}],
-goText[s1,k]\[CapitalGamma]Text[i,s1,l],
-\[CapitalGamma]Text[k,s1,l]goText[i,s1]
}


(* ::Subsection:: *)
(*Output*)


clean[expr_]:=HoldForm[expr]/.(h:Except[HoldForm|Equal|Rule|List|Row|Power|Times|Plus|Log|Subscript|Subsuperscript|Superscript])[args___]:>h


PrintExp[expr_,color_:Blue]:=CellPrint@ExpressionCell[Style[Row[expr],color,TextAlignment->Left],{"Output","DisplayFormulaNumbered"},CellLabel->"",CellFrameColor->color]


(* ::Section:: *)
(*Metric derivatives g_ab,c  and g_ab,cd*)


(* ::Subsection::Closed:: *)
(*Generators*)


genMdg[Mg_,q_]:=Module[{dg,dim},
dim=Length@q;
dg=Array[
If[#1<=#2,
D[Mg[[#1,#2]],q[[#3]]]//Expand//Simplify,
Null
]&
,{dim,dim,dim}]
];
genMdg::usage = "[Mg_,q_]: Generates the symmetric components \[Mu]<=\[Nu] of the first derivative matrix (\!\(\*SubscriptBox[\(g\), \(\[Mu]\[Nu], \[Kappa]\)]\)). ";

genMddg[Mdg_,q_]:=Module[{ddg,dim},
dim=Length@q;
ddg=Array[
If[#3<=#4,
If[#1<=#2,
D[Mdg[[#1,#2,#3]],q[[#4]]]//Expand//Simplify,
Null],Null
]&
,{dim,dim,dim,dim}]
];
genMddg::usage = "[Mdg_,q_]: Generates the symmetric components \[Mu]<=\[Nu] and \[Kappa]<=\[Lambda] of the second derivative matrix (\!\(\*SubscriptBox[\(g\), \(\[Mu]\[Nu], \[Kappa]\[Lambda]\)]\)). ";


(* ::Subsection::Closed:: *)
(*Access methods*)


dg[Mdg_]:=Function[{i,j,k},
If[i<=j,Mdg[[i,j,k]],Mdg[[j,i,k]]]
];
dg::usage ="[Mdg_]: Access method for \!\(\*SubscriptBox[\(g\), \(\[Mu]\[Nu], \[Kappa]\)]\).";

ddg[Mddg_]:=Function[{i,j,k,l},
Module[{out},
If[k<=l&&i<=j,out=Mddg[[i,j,k,l]]];
If[k<=l&&i>j,out=Mddg[[j,i,k,l]]];
If[k>l&&i<=j,out=Mddg[[i,j,l,k]]];
If[k>l&&i>j,out=Mddg[[j,i,l,k]]];
out
]
];
ddg::usage ="[Mddg_]: Access method for \!\(\*SubscriptBox[\(g\), \(\[Mu]\[Nu], \[Kappa]\[Lambda]\)]\).";


(* ::Subsection::Closed:: *)
(*High level method*)


Dg[Mg_,q_]:=Module[{Mdg,Mddg},
Mdg=genMdg[Mg,q];
Mddg=genMddg[Mdg,q];
{dg@Mdg,ddg@Mddg}
]
Dg::usage ="[Mg_,q_]: High level method for the metric derivatives which generates \!\(\*SubscriptBox[\(g\), \(\[Mu]\[Nu], \[Kappa]\)]\) and \!\(\*SubscriptBox[\(g\), \(\[Mu]\[Nu], \[Kappa]\[Lambda]\)]\) and returns the access methods";


(* ::Section:: *)
(*Christoffel symbols of the first and second kind*)


(* ::Subsection::Closed:: *)
(*Generators*)


gen\[CapitalGamma]1[dg_,{k_,i_,j_}]:=1/2(dg[k,j,i]+dg[k,i,j]-dg[i,j,k]);
gen\[CapitalGamma]1::usage ="[dg_,{k_,i_,j_}]: Computes the Christoffel symbol of the first kind \!\(\*SubscriptBox[\(\[CapitalGamma]\), \(\[Kappa]\[Mu]\[Nu]\)]\).";

gen\[CapitalGamma]2[M\[CapitalGamma]1_,MgInv_,{k_,i_,j_}]:=Module[{m},Sum[MgInv[[k,m]]*M\[CapitalGamma]1[[m,i,j]],{m,1,M\[CapitalGamma]1//Length}]];
gen\[CapitalGamma]2::usage ="[dg_,gInv_,{k_,i_,j_}]: Computes the Christoffel symbol of the second kind \!\(\*SubsuperscriptBox[\(\[CapitalGamma]\), \(\[Mu]\[Nu]\), \(\[Kappa]\)]\).";

genM\[CapitalGamma]1All[dg_,dim_:4]:=Array[If[#2<=#3,gen\[CapitalGamma]1[dg,{#1,#2,#3}],Null]&,{dim,dim,dim}];
genM\[CapitalGamma]1All::usage = "[dg_,dim_]: Generates the symmetric components \[Mu]<=\[Nu] of the Christoffel symbols of the first kind \!\(\*SubscriptBox[\(\[CapitalGamma]\), \(\[Kappa]\[Mu]\[Nu]\)]\).";

genM\[CapitalGamma]2All[\[CapitalGamma]1M_,MgInv_]:=Array[If[#2<=#3,gen\[CapitalGamma]2[\[CapitalGamma]1M,MgInv,{#1,#2,#3}],Null]&,{MgInv//Length,MgInv//Length,MgInv//Length}];
genM\[CapitalGamma]2All::usage = "[\[CapitalGamma]1M_,MgInv_]: Generates the symmetric components \[Mu]<=\[Nu] of the Christoffel symbols of the second kind \!\(\*SubsuperscriptBox[\(\[CapitalGamma]\), \(\[Mu]\[Nu]\), \(\[Kappa]\)]\).";


(* ::Subsection::Closed:: *)
(*Access methods*)


\[CapitalGamma][\[CapitalGamma]M_]:=Function[{k,i,j},
If[i<=j,\[CapitalGamma]M[[k,i,j]],\[CapitalGamma]M[[k,j,i]]]
];
\[CapitalGamma]::usage = "[\[CapitalGamma]M_]: Access method for the Christoffel symbols of the first \!\(\*SubscriptBox[\(\[CapitalGamma]\), \(\[Kappa]\[Mu]\[Nu]\)]\) and second kind \!\(\*SubsuperscriptBox[\(\[CapitalGamma]\), \(\[Mu]\[Nu]\), \(\[Kappa]\)]\).";


(* ::Subsection:: *)
(*High level method*)


\[CapitalGamma]1[dg_,dim_:4]:=Module[{M\[CapitalGamma]1All},
M\[CapitalGamma]1All=genM\[CapitalGamma]1All[dg,dim];
\[CapitalGamma][M\[CapitalGamma]1All]
];
\[CapitalGamma]1::usage ="[dg_,dim_:4]: High level method for the Christoffel symbols of the first kind \!\(\*SubscriptBox[\(\[CapitalGamma]\), \(\[Kappa]\[Mu]\[Nu]\)]\) which generates all \!\(\*SubscriptBox[\(\[CapitalGamma]\), \(\[Kappa]\[Mu]\[Nu]\)]\) and returns the access method.";

\[CapitalGamma]2[dg_,MgInv_]:=Module[{M\[CapitalGamma]2All},
M\[CapitalGamma]2All=genM\[CapitalGamma]2All[genM\[CapitalGamma]1All[dg,MgInv//Length],MgInv];
\[CapitalGamma][M\[CapitalGamma]2All]
];
\[CapitalGamma]2::usage ="[dg_,MgInv_]: High level method for the Christoffel symbols of the second kind \!\(\*SubsuperscriptBox[\(\[CapitalGamma]\), \(\[Mu]\[Nu]\), \(\[Kappa]\)]\) which generates all \!\(\*SubsuperscriptBox[\(\[CapitalGamma]\), \(\[Mu]\[Nu]\), \(\[Kappa]\)]\) and returns the access method.";


(* ::Subsection::Closed:: *)
(*Output*)


\[CapitalGamma]2`print[\[CapitalGamma]_,d_:4]:=Module[{\[CapitalGamma]current,Mout,Mlist},
Mout=Table[
If[\[CapitalGamma]current=Simplify[\[CapitalGamma][i,j,k]]/.Null->0;NumberQ[\[CapitalGamma]current],
Null,{{i,j,k},\[CapitalGamma]current}
],{i,1,d},{j,1,d},{k,j,d}];
Mlist=DeleteCases[Flatten[Mout,2],Null];
Table[{Mlist[[i,1]],Mlist[[i,2]],Row[{\[CapitalGamma]Text[Mlist[[i,1,1]],Mlist[[i,1,2]],Mlist[[i,1,3]]],"=",\[CapitalGamma]Text[Mlist[[i,1,1]],Mlist[[i,1,3]],Mlist[[i,1,2]]],"="}]},{i,1,Length@Mlist}]
]


(* ::Section::Closed:: *)
(*Covariant Derivative*)


CoDerivativeT2[{T_,struc_},\[CapitalGamma]2_,q_,{l_,i_,j_}]:=Module[{dim,m,der,iSum,jSum,res},
dim=Length@q;
If[MemberQ[{"uu","ud","du","dd"},struc],
der=D[T[[i,j]],q[[l]]];
If[Characters[struc][[1]]=="u",
iSum=Sum[+\[CapitalGamma]2[i,m,l]*T[[m,j]],{m,1,dim}];,
iSum=Sum[-\[CapitalGamma]2[m,i,l]*T[[m,j]],{m,1,dim}];
];
If[Characters[struc][[2]]=="u",
jSum=Sum[+\[CapitalGamma]2[j,m,l]*T[[i,m]],{m,1,dim}];,
jSum=Sum[-\[CapitalGamma]2[m,j,l]*T[[i,m]],{m,1,dim}];
];
res=der+iSum+jSum;
,(*else*)res=Null;Message[CoDerivativeT2::nnarg,struc];
];
res
]
CoDerivativeT2::nnarg="The argument `1` is not a falid tensor structure. Valid structures are 'uu', 'ud', 'du' or 'dd'.";
CoDerivativeT2::usage ="[{T_,struc_},\[CapitalGamma]2_,q_,{l_,i_,j_}]: Computes the covariant derivative \!\(\*SubscriptBox[\(\[Del]\), \(l\)]\)\!\(\*SuperscriptBox[\(T\), \(ij\)]\) of the second rank tensor T. Valid structures for T are 'uu', 'ud', 'du' or 'dd'.";


CoDerivativeV1[{V_,struc_},\[CapitalGamma]2_,q_,{i_,j_}]:=Module[{dim,m,der,iSum,res},
dim=Length@q;
If[MemberQ[{"u","d"},struc],
der=D[V[[i]],q[[j]]];
If[Characters[struc][[1]]=="u",
iSum=Sum[+\[CapitalGamma]2[i,j,m]*V[[m]],{m,1,dim}];,
iSum=Sum[-\[CapitalGamma]2[m,i,j]*V[[m]],{m,1,dim}];
];
res=der+iSum;
,(*else*)res=Null;Message[CoDerivativeV1::nnarg,struc];
];
res
]
CoDerivativeV1::nnarg="The argument `1` is not a falid tensor structure. Valid structures are 'u' or 'd'.";
CoDerivativeV1::usage ="[{T_,struc_},\[CapitalGamma]2_,q_,{li_,j_}]: Computes the covariant derivative (\!\(\*SubscriptBox[\(\[Del]\), \(j\)]\)V\!\(\*SubscriptBox[\()\), \(i\)]\) or (\!\(\*SubscriptBox[\(\[Del]\), \(j\)]\)V\!\(\*SuperscriptBox[\()\), \(i\)]\) of the vector/one-form V. Valid structures for V are 'u' or 'd'.";


(* ::Section::Closed:: *)
(*Riemann curvature tensor*)


(* ::Subsection::Closed:: *)
(*Generators*)


genRM[Mg_,{dg_,ddg_},\[CapitalGamma]2_,{i_,k_,l_,m_}]:=Module[{dim,n,p,Rdd,R\[CapitalGamma]1,R\[CapitalGamma]2},
dim=Length@Mg;
Rdd=(ddg[i,m,k,l]+ddg[k,l,i,m]-ddg[i,l,k,m]-ddg[k,m,i,l]);
R\[CapitalGamma]1=Sum[Mg[[n,p]]*\[CapitalGamma]2[n,k,l]*\[CapitalGamma]2[p,i,m],{n,dim},{p,dim}];
R\[CapitalGamma]2=Sum[Mg[[n,p]]*\[CapitalGamma]2[n,k,m]*\[CapitalGamma]2[p,i,l],{n,dim},{p,dim}];
(1/2*Rdd+R\[CapitalGamma]1-R\[CapitalGamma]2)
];
genRM::usage ="[Mg_,{dg_,ddg_},\[CapitalGamma]2_,{i_,k_,l_,m_}]: Computes a component of the Riemann curvature tensor \!\(\*SubscriptBox[\(R\), \(\[Mu]\[Nu]\[Kappa]\[Lambda]\)]\).";

genMRMAll[Mg_,{dg_,ddg_},\[CapitalGamma]2_]:=Module[{out,k,dim,dim2,syRtuples},
k=1;
dim=Length@Mg;
dim2=dim (dim-1)/2;
syRtuples=Array[Null,dim2];
Do[If[i<j,syRtuples[[k]]={i,j};k++;,Null];,{i,1,dim},{j,1,dim}];
out={syRtuples,Array[
If[#1<=#2,
genRM[Mg,{dg,ddg},\[CapitalGamma]2,Join[syRtuples[[#1]],syRtuples[[#2]]]]
,Null]&
,{dim2,dim2}]};

out
];
genMRMAll::usage ="[Mg_,{dg_,ddg_},\[CapitalGamma]2_]: Computes all independent components of the Riemann curvature tensor \!\(\*SubscriptBox[\(R\), \(\[Mu]\[Nu]\[Kappa]\[Lambda]\)]\) 
taking the two interchange anti-symmetries and the screw symmetry into account.";


(* ::Subsection::Closed:: *)
(*Access method*)


RMac[MRM_]:=Function[{i,k,l,m},
Module[{out,a,b,sign,aPos,bPos},
If[i!=k&&l!=m,
sign=1;
a=If[i<k,{i,k},sign=sign*-1;{k,i}];
b=If[l<m,{l,m},sign=sign*-1;{m,l}];
aPos=Position[MRM[[1]],a][[1,1]];
bPos=Position[MRM[[1]],b][[1,1]];
If[aPos<=bPos,
out=sign*MRM[[2]][[aPos,bPos]];,
out=sign*MRM[[2]][[bPos,aPos]];];,
out=0;
];
out]
];
RMac::usage ="[MRM_]: Access method for the Riemann curvature tensor \!\(\*SubscriptBox[\(R\), \(\[Mu]\[Nu]\[Kappa]\[Lambda]\)]\).";


(* ::Subsection::Closed:: *)
(*High level method*)


RM[Mg_,{dg_,ddg_},\[CapitalGamma]2_]:=Module[{MRMAll},
MRMAll=genMRMAll[Mg,{dg,ddg},\[CapitalGamma]2];
RMac[MRMAll]
];
RM::usage ="[Mg_,{dg_,ddg_},\[CapitalGamma]2_]: High level method for the Riemann curvature tensor \!\(\*SubscriptBox[\(R\), \(\[Mu]\[Nu]\[Kappa]\[Lambda]\)]\) which computes all \!\(\*SubscriptBox[\(R\), \(\[Mu]\[Nu]\[Kappa]\[Lambda]\)]\) and returns the access method.";


(* ::Subsection::Closed:: *)
(*Output*)


RM`print[RM_]:=Module[{tuples,RMmatrix,RMcurrent,Mout,Mlist},
tuples=RM[[1]];
RMmatrix=RM[[2]];

Mout=Table[
If[RMcurrent=Simplify[RMmatrix[[i,j]]]/.Null->0;NumberQ[RMcurrent],
Null,{{i,j},RMcurrent}
],{i,1,Length@tuples},{j,1,Length@tuples}];

Mlist=DeleteCases[Flatten[Mout,1],Null];
Table[
{Flatten[{tuples[[Mlist[[i,1,1]]]],tuples[[Mlist[[i,1,2]]]]}],
Mlist[[i,2]],
Row[{
Subscript["R",Row@Flatten[{tuples[[Mlist[[i,1,1]]]],tuples[[Mlist[[i,1,2]]]]}]],
"=",
-Subscript["R",Row@Flatten[{Reverse@tuples[[Mlist[[i,1,1]]]],tuples[[Mlist[[i,1,2]]]]}]],
"=",
-Subscript["R",Row@Flatten[{tuples[[Mlist[[i,1,1]]]],Reverse@tuples[[Mlist[[i,1,2]]]]}]],
"=",
Subscript["R",Row@Flatten[{Reverse@tuples[[Mlist[[i,1,1]]]],Reverse@tuples[[Mlist[[i,1,2]]]]}]],
"(=",
Subscript["R",Row@Flatten[{tuples[[Mlist[[i,1,2]]]],tuples[[Mlist[[i,1,1]]]]}]],
"=...)"
}]
}
,{i,1,Length@Mlist}]
]


(* ::Section::Closed:: *)
(*Ricci tensor*)


(* ::Subsection:: *)
(*Generators*)


genRicci[RM_,MgInv_,{i_,j_}]:=Module[{m,k},Sum[MgInv[[k,m]]*RM[i,k,j,m],{k,1,MgInv//Length},{m,1,MgInv//Length}]];
genRicci::usage ="[RM_,MgInv_,{i_,j_}]: Computes a component of the Ricci tensor \!\(\*SubscriptBox[\(R\), \(\[Mu]\[Nu]\)]\) .";

genMRicciAll[RM_,MgInv_]:=Array[If[#1<=#2,genRicci[RM,MgInv,{#1,#2}],0]&,{MgInv//Length,MgInv//Length}];
genMRicciAll::usage ="[RM_,MgInv_]: Computes all independent components of the Ricci tensor \!\(\*SubscriptBox[\(R\), \(\[Mu]\[Nu]\)]\) taking the interchange symmetry into account.";


(* ::Subsection:: *)
(*Access method*)


Ricciac[MRicciAll_]:=Function[{i,j},
If[i<=j,MRicciAll[[i,j]],MRicciAll[[j,i]]]
];
Ricciac::usage ="[MRicciAll_]: Access method for \!\(\*SubscriptBox[\(R\), \(\[Mu]\[Nu]\)]\).";


(* ::Subsection:: *)
(*High level method*)


Ricci[RM_,MgInv_]:=Module[{MRMAll},
MRMAll=genMRicciAll[RM,MgInv];
Ricciac[MRMAll]
];
Ricci::usage ="[RM_,MgInv_]: High level method for the Ricci tensor \!\(\*SubscriptBox[\(R\), \(\[Mu]\[Nu]\)]\) which computes all \!\(\*SubscriptBox[\(R\), \(\[Mu]\[Nu]\)]\) and returns the access method.";


(* ::Section::Closed:: *)
(*Scalar Curvature*)


R[Ricci_,MgInv_]:=Module[{i,j},
Sum[MgInv[[i,j]]*Ricci[i,j],{i,1,Length@MgInv},{j,1,Length@MgInv}]
];
R::usage ="[Ricci_,MgInv_]: Computes the scalar Curvature R.";


(* ::Section::Closed:: *)
(*Einstein tensor*)


(* ::Subsection:: *)
(*Generator*)


genMGAll[Ricci_,R_,Mg_]:=Array[If[#1<=#2,Ricci[#1,#2]-1/2*Mg[[#1,#2]]*R,0]&,{Mg//Length,Mg//Length}];
genMGAll::usage ="[Ricci_,R_,Mg_]: Computes all indipendent components of the Einstein Tensor \!\(\*SubscriptBox[\(G\), \(\[Mu]\[Nu]\)]\).";


(* ::Subsection:: *)
(*Access method*)


Gac[genMGAll_]:=Function[{i,j},
If[i<=j,genMGAll[[i,j]],genMGAll[[j,i]]]
];
Gac::usage ="[genMGAll_]: Access method for \!\(\*SubscriptBox[\(G\), \(\[Mu]\[Nu]\)]\).";


(* ::Subsection:: *)
(*High level method*)


Ghigh[Ricci_,R_,Mg_]:=Module[{MGAll},
MGAll=genMGAll[Ricci,R,Mg];
Gac[MGAll]
];
Ghigh::usage ="[Ricci_,R_,Mg_]: High level method for the Einstein tensor \!\(\*SubscriptBox[\(G\), \(\[Mu]\[Nu]\)]\) which computes all \!\(\*SubscriptBox[\(G\), \(\[Mu]\[Nu]\)]\) and returns the access method.";


(* ::Section:: *)
(*Killing equation and Commutator*)


killingTensor[Ad_,\[CapitalGamma]_,q_]:=Module[{coDivM,dim},
dim=Length@q;
coDivM=Array[CoDerivativeV1[{Ad,"d"},\[CapitalGamma],q,{#1,#2}]&,{dim,dim}];
Sow[coDivM];
coDivM+coDivM\[Transpose]
]
killingTensor::usage ="[Ad_,\[CapitalGamma]_,q_]: Computes the 'Killing tensor' K=\!\(\*SubscriptBox[\(\[Del]\), \(\[Alpha]\)]\)\!\(\*SubscriptBox[\(A\), \(\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Del]\), \(\[Beta]\)]\)\!\(\*SubscriptBox[\(A\), \(\[Alpha]\)]\)=2\!\(\*SubscriptBox[\(\[Del]\), \((\[Beta]\)]\)\!\(\*SubscriptBox[\(A\), \(\(\[Alpha]\)\()\)\)]\). If K=0 then Ad is a Killing vector of the metric assosiated with \[CapitalGamma].";


GRcommutator[Au_,Bu_,q_]:=Module[{dim,m},
dim=Length@q;
Array[Sum[Au[[m]]D[Bu[[#]],q[[m]]]-Bu[[m]]D[Au[[#]],q[[m]]],{m,1,dim}]&,dim]
]
GRcommutator::usage ="[Au_,Bu_,q_]: Computes the commutator [A,B\!\(\*SuperscriptBox[\(]\), \(\[Alpha]\)]\)=\!\(\*SuperscriptBox[\(A\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\)\!\(\*SuperscriptBox[\(B\), \(\[Alpha]\)]\)-\!\(\*SuperscriptBox[\(B\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\)\!\(\*SuperscriptBox[\(A\), \(\[Alpha]\)]\)(=\!\(\*SuperscriptBox[\(A\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(\[Del]\), \(\[Mu]\)]\)\!\(\*SuperscriptBox[\(B\), \(\[Alpha]\)]\)-\!\(\*SuperscriptBox[\(B\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(\[Del]\), \(\[Mu]\)]\)\!\(\*SuperscriptBox[\(A\), \(\[Alpha]\)]\)).";


EndPackage[]
