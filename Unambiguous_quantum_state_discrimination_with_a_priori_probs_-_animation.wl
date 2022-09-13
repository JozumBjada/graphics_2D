(* ::Package:: *)

(* ::Title:: *)
(*Unambiguous quantum state discrimination for varying a priori probabilities*)


(* ::Subtitle:: *)
(*Wiki animation source code*)


(* ::Text:: *)
(*Source code for file: "Unambiguous_quantum_state_discrimination_with_a_priori_probs_-_animation.gif"*)
(*https://commons.wikimedia.org/wiki/File:Unambiguous_quantum_state_discrimination_with_a_priori_probs_-_animation.gif*)
(*Version: "Wolfram language 12.0.0 for Microsoft Windows (64-bit) (April 6, 2019)"*)


(* ::Section:: *)
(*Code*)


(* ::Input::Initialization:: *)
vec[char_,idx_,ang_,italic_:True,len_:1,off_:.15]:=If[len==0,{},{Arrow[{{0,0},len AngleVector[ang ]}],Text[Style[ToString[Ket[Subscript[char,idx]],TraditionalForm],If[italic,Italic,Plain],30,FontFamily->"Times",Background->White],(1+off)AngleVector[ang],{0,0},AngleVector[ang]]}]


(* ::Input::Initialization:: *)
f[x_]:=(1+Clip[1.07Sign[Sin[2\[Pi] x]]Abs[Sin[2\[Pi] x]]^3,{-1,1}])/2


(* ::Input::Initialization:: *)
usdInternal[ang1i_,ang2i_,prob_]:=Module[{ang1=ang1i,ang2=ang2i,p1=prob,p2=1-prob,psi1,psi2,q,r,e1,e2,eQ,ang1e,ang2e,angQe,prefac1,prefac2,prefacQ,disk},

If[prob<1/2,
{p1,p2}={p2,p1};
{ang1,ang2}={ang2,ang1};
];

{psi1,psi2}=AngleVector/@{ang1,ang2};
{q,r}={Abs[Cos[ang2i-ang1i]],Sqrt[p2/p1]};

If[q<=r,
(*generic case*)
e1=1/(1-q^2) Sqrt[1-q r](psi1-q psi2);
e2=1/(1-q^2) Sqrt[1-q/r](psi2-q psi1);
eQ=1/(1-q^2) Sqrt[q/r]((r-q)psi1+(1-q r) psi2);

{ang1e,ang2e,angQe}=ArcTan[#[[1]],#[[2]]]&/@{e1,e2,eQ};
{prefac1,prefac2,prefacQ}=Norm/@{e1,e2,eQ};
,
(*degenerate case*)
e1=1/Sqrt[1-q^2] (psi1-q psi2);
e2={0,0};
eQ=psi2;

{ang1e,ang2e,angQe}={ArcTan[e1[[1]],e1[[2]]],0,ang2};
{prefac1,prefac2,prefacQ}={1,0,1};
];

disk=If[q<=r,{},Disk[{0,0},.1,{ang1e,angQe}]];

If[prob<1/2,
{ang1e,ang2e}={ang2e,ang1e};
{prefac1,prefac2}={prefac2,prefac1};
];

{ang1e,ang2e,angQe,prefac1,prefac2,prefacQ,disk}
]


(* ::Input::Initialization:: *)
grUSDapriori[ang1i_,ang2i_,prob_]:=Module[{prefac1,prefac2,prefacQ,e1,e2,eQ,ang1e,ang2e,angQe,disk},

{ang1e,ang2e,angQe,prefac1,prefac2,prefacQ,disk}=usdInternal[ang1i,ang2i,prob];

Graphics[{
Circle[{0,0},1],
{Thickness[0.003],Dashing[0.02],Line[{{{-1,0},{1,0}},{{0,-1},{0,1}}}]},
{Thickness[0.005],Arrowheads[0.05],
{
{Opacity[Sqrt[prob],Orange],vec["\[Psi]",1,ang1i,False]},
{Opacity[Sqrt[1-prob],Orange],vec["\[Psi]",2,ang2i,False]}
},

{Blue,disk,
vec["e","1",ang1e,True,prefac1,prefac1-1+0.15],
vec["e","2",ang2e,True,prefac2,prefac2-1+0.15],
vec["e","?",angQe,True,prefacQ,Min[prefacQ-1+0.2,0.15]]
},

{
Text[Style[Row[{Style[Subscript["p",1],Italic],"\[VeryThinSpace]=\[VeryThinSpace]",ToString[NumberForm[Chop@N@prob,{2,2}]]}],30,FontFamily->"Times"],ImageScaled[{.83,.55}]],
Text[Style[Row[{Style[Subscript["p",2],Italic],"\[VeryThinSpace]=\[VeryThinSpace]",ToString[NumberForm[Chop@N@(1-prob),{2,2}]]}],30,FontFamily->"Times"],ImageScaled[{.7,.85}]]
},
{Black,PointSize[0.02],Point[{0,0}]}
}
},ImageSize->700,PlotRange->{.7+{-1.25,1.25},{-1.25,1.25}},Background->None]
];


(* ::Input::Initialization:: *)
animationUSDapriori[t_]:=grUSDapriori[0.1,0.8,f[t]]


(* ::Input::Initialization:: *)
exportAnimation[fun_,name_,resolution_:100]:=Module[{seq},
SetDirectory[NotebookDirectory[]];
seq=Table[fun[t],{t,0,1,.01}];
Export[name<>".gif",seq,AnimationRepetitions->Infinity,"DisplayDurations"->0.13(*0.05*)(*0.01*),"TransparentColor"->Automatic,ImageResolution->resolution]
]


(* ::Input:: *)
(*(*Plot[f[x],{x,0,1}]*)*)


(* ::Input:: *)
(*(*Manipulate[grUSDapriori[0.1,0.8,p],{p,0,1},SaveDefinitions->True]*)*)


(* ::Input:: *)
(*(*Manipulate[animationUSDapriori[t],{t,0,1},SaveDefinitions->True]*)*)


(* ::Input:: *)
(*exportAnimation[animationUSDapriori,"animUSDapriori"]*)
