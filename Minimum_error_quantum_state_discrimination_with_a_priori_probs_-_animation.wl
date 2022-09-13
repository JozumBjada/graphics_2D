(* ::Package:: *)

(* ::Title:: *)
(*Minimum error quantum state discrimination for varying a priori probabilities*)


(* ::Subtitle:: *)
(*Wiki animation source code*)


(* ::Text:: *)
(*Source code for file: "Minimum_error_quantum_state_discrimination_with_a_priori_probs_-_animation.gif"*)
(*https://commons.wikimedia.org/wiki/File:Minimum_error_quantum_state_discrimination_with_a_priori_probs_-_animation.gif*)
(*Version: "Wolfram language 12.0.0 for Microsoft Windows (64-bit) (April 6, 2019)"*)


(* ::Section:: *)
(*Code*)


(* ::Input::Initialization:: *)
vec[char_,idx_,ang_,italic_:True,len_:1,off_:.15]:={Arrow[{{0,0},len AngleVector[ang ]}],Text[Style[ToString[Ket[Subscript[char,idx]],TraditionalForm],If[italic,Italic,Plain],30,FontFamily->"Times",Background->White],(1+off)AngleVector[ang],{0,0},AngleVector[ang]]}


(* ::Input::Initialization:: *)
With[{m=0.1},f[x_]:=Piecewise[{{1/2 (1+Clip[-1.5TriangleWave[1/(1-2 m) (x-1/2)],{-1,1}]),m<=x<=1-m}},1/2]]


(* ::Input::Initialization:: *)
grMEapriori[ang1_,ang2_,prob_:.5]:=Module[{ang1e,ang2e,q,\[Kappa],\[Lambda],e1,e2},

q=Abs@Cos[ang2-ang1];
\[Kappa]=(1-2(1-prob)q^2)/Sqrt[(1-2(1-prob)q^2)^2+(2(1-prob)q Sqrt[1-q^2])^2];
\[Lambda]=2 q (q \[Kappa]-Sqrt[1-q^2] Sqrt[1-\[Kappa]^2]);
e1=1/Sqrt[2(1-q^2)] (Sqrt[1+\[Kappa]-\[Lambda]]AngleVector[ang1]-Sqrt[1-\[Kappa]]AngleVector[ang2]);
e2=1/Sqrt[2(1-q^2)] (-Sqrt[1-\[Kappa]+\[Lambda]]AngleVector[ang1]+Sqrt[1+\[Kappa]]AngleVector[ang2]);

ang1e=ArcTan[e1[[1]],e1[[2]]];
ang2e=ArcTan[e2[[1]],e2[[2]]];

Graphics[{
Circle[{0,0},1],
{Thickness[0.003],Dashing[0.02],Line[{{{-1,0},{1,0}},{{0,-1},{0,1}}}]},
{Thickness[0.005],Arrowheads[0.05],
{
{Opacity[Sqrt[prob],Orange],vec["\[Psi]",1,ang1,False]},
{Opacity[Sqrt[1-prob],Orange],vec["\[Psi]",2,ang2,False]}
},
{Blue,vec["e",1,ang1e],vec["e",2,ang2e],Disk[{0,0},.1,{ang1e,ang2e}]},
{
Text[Style[Row[{Style[Subscript["p",1],Italic],"\[VeryThinSpace]=\[VeryThinSpace]",ToString[NumberForm[Chop@N@prob,{2,2}]]}],30,FontFamily->"Times"],ImageScaled[{.83,.55}]],
Text[Style[Row[{Style[Subscript["p",2],Italic],"\[VeryThinSpace]=\[VeryThinSpace]",ToString[NumberForm[Chop[1.-prob],{2,2}]]}],30,FontFamily->"Times"],ImageScaled[{.7,.85}]]
},
{Black,PointSize[0.02],Point[{0,0}]}
}
},ImageSize->700,PlotRange->{.7+{-1.25,1.25},{-1.25,1.25}},Background->None]
];
grMEapriori[.1,.8,.5]


(* ::Input::Initialization:: *)
animationMEapriori[t_]:=grMEapriori[0.1,0.8,f[t]]


(* ::Input::Initialization:: *)
exportAnimation[fun_,name_,resolution_:100]:=Module[{seq},
SetDirectory[NotebookDirectory[]];
seq=Table[fun[t],{t,0,1,.01}];
Export[name<>".gif",seq,AnimationRepetitions->Infinity,"DisplayDurations"->0.01,"TransparentColor"->Automatic,ImageResolution->resolution]
]


(* ::Input:: *)
(*(*Plot[f[x],{x,0,1}]*)*)


(* ::Input:: *)
(*(*Manipulate[grMEapriori[0.1,0.8,p],{p,0,1}]*)*)


(* ::Input:: *)
(*(*Manipulate[animationMEapriori[t],{t,0,1}]*)*)


(* ::Input:: *)
(**)


(*exportAnimation[animationMEapriori,"animMEapriori"]*)
