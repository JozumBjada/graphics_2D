(* ::Package:: *)

(* ::Title:: *)
(*Unambiguous quantum state discrimination \[LongDash] animation*)


(* ::Subtitle:: *)
(*Wiki animation source code*)


(* ::Text:: *)
(*Source code for file: "Unambiguous_quantum_state_discrimination_-_animation.gif"*)
(*https://commons.wikimedia.org/wiki/File:Unambiguous_quantum_state_discrimination_-_animation.gif*)
(*Version: "Wolfram language 12.0.0 for Microsoft Windows (64-bit) (April 6, 2019)"*)


(* ::Section:: *)
(*Code*)


(* ::Input::Initialization:: *)
vec[char_,idx_,ang_,italic_:True,len_:1,off_:.15]:={Arrow[{{0,0},len AngleVector[ang ]}],Text[Style[ToString[Ket[Subscript[char,idx]],TraditionalForm],If[italic,Italic,Plain],30,FontFamily->"Times",Background->White],(1+off)AngleVector[ang],{0,0},AngleVector[ang]]}


(* ::Input::Initialization:: *)
grUSD[ang1_,ang2_,stage_]:=Module[{angavg=(ang1+ang2)/2,sceneAux1,sceneAux2,scenePro,prefac,prefacI,off},
prefac=1/Sqrt[1+Abs[Cos[ang2-ang1]]];
prefacI=prefac Sqrt[2Abs[Cos[ang2-ang1]]];
off=prefac-1+0.15;

sceneAux1={RGBColor[0.4, 0.4, 1.],Disk[{0,0},.12,{ang2,ang2-\[Pi]/2}],vec["x",1,ang2-\[Pi]/2]};
sceneAux2={RGBColor[0.4, 0.4, 1.],Disk[{0,0},.14,{ang1,ang1+\[Pi]/2}],vec["x",2,ang1+\[Pi]/2]};
scenePro={Blue,Disk[{0,0},.14,{ang1,ang1+\[Pi]/2}],Disk[{0,0},.12,{ang2,ang2-\[Pi]/2}],vec["e",2,ang1+\[Pi]/2,True,prefac,off],vec["e",1,ang2-\[Pi]/2,True,prefac,off],vec["e","?",(ang1+ang2)/2,True,prefacI,Min[prefacI-1+0.2,0.15]]};

Graphics[{
{Circle[{0,0},1]},
{Thickness[0.003],Dashing[0.02],Line[{{{-1,0},{1,0}},{{0,-1},{0,1}}}]},
{Thickness[0.005],Arrowheads[0.05],
{Orange,vec["\[Psi]",1,ang1,False],vec["\[Psi]",2,ang2,False]},

Switch[stage,
1,{},
2,{sceneAux1},
3,{sceneAux1,sceneAux2},
4,{scenePro}
],

{Black,PointSize[0.02],Point[{0,0}]}
}
},ImageSize->700,PlotRange->1.25,Background->None]
];


(* ::Input::Initialization:: *)
animationUSD[t_]:=Module[{tstages=0.5,ang1=0.2,ang2=0.8,t1,t2},
{t1,t2}=tstages+(1-tstages){3,6}/7;
Which[
t<tstages,grUSD[ang1,ang2,Quotient[Rescale[t,{0,tstages},{1,5}],1]],
tstages<=t<t1,grUSD[ang1+\[Pi]/6 UnitTriangle[Rescale[t,{tstages,t1},1.2{-1,1}]],ang2,4],
t1<=t<t2,grUSD[ang1,ang2+\[Pi]/4 UnitTriangle[Rescale[t,{t1,t2},1.2{-1,1}]],4],
t2<=t,grUSD[ang1,ang2,4]
]
]


(* ::Input::Initialization:: *)
exportAnimation[fun_,name_,resolution_:100]:=Module[{init,fin,seq,durs},
SetDirectory[NotebookDirectory[]];
init=fun/@{0,.2,.3,.4};
fin=Table[fun[t],{t,.5,1,.01}];
seq=Join[init,fin];
durs=Join[Table[1.5,Length[init]],Table[0.01,Length[fin]]];
Export[name<>".gif",seq,AnimationRepetitions->Infinity,"DisplayDurations"->durs,"TransparentColor"->Automatic,"ColorMapLength"->8,"QuantizationMethod"->"MedianCut",Dithering->None,ImageResolution->resolution]
]


(* ::Input:: *)
(*Manipulate[animationUSD[t],{t,0,1},SaveDefinitions->True]*)


(* ::Input:: *)
(*exportAnimation[animationUSD,"animUSD"]*)
