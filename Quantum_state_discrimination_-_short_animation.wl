(* ::Package:: *)

(* ::Title:: *)
(*Quantum state discrimination \[LongDash] short animation*)


(* ::Subtitle:: *)
(*Wiki animation source code*)


(* ::Text:: *)
(*Source code for file: "Quantum_state_discrimination_-_short_animation.gif"*)
(*https://commons.wikimedia.org/wiki/File:Quantum_state_discrimination_-_short_animation.gif*)
(*Version: "Wolfram language 12.0.0 for Microsoft Windows (64-bit) (April 6, 2019)"*)


(* ::Section:: *)
(*Code*)


(* ::Input::Initialization:: *)
(*credit to "J.M.'s discontentment"; https://mathematica.stackexchange.com/questions/49313/drawing-a-cuboid-with-rounded-corners *)
roundedCuboid[p1_?VectorQ,p2_?VectorQ,r_?NumericQ]:=Module[{csk,csw,cv,ei,fi,ocp,osk,owt},cv=Tuples[Transpose[{p1+r,p2-r}]];
ocp={{{1,0,0},{1,1,0},{0,1,0}},{{1,0,1},{1,1,1},{0,1,1}},{{0,0,1},{0,0,1},{0,0,1}}};
osk={{0,0,0,1,1,1},{0,0,0,1,1,1}};
owt={{1,1/Sqrt[2],1},{1/Sqrt[2],1/2,1/Sqrt[2]},{1,1/Sqrt[2],1}};
ei={{{4,8},{2,6},{1,5},{3,7}},{{6,8},{2,4},{1,3},{5,7}},{{7,8},{3,4},{1,2},{5,6}}};
csk={{0,0,1,1},{0,0,0,1,1,1}};
csw={{1,1/Sqrt[2],1},{1,1/Sqrt[2],1}};
fi={{8,6,5,7},{8,7,3,4},{8,4,2,6},{4,3,1,2},{2,1,5,6},{1,3,7,5}};
Flatten[{EdgeForm[],BSplineSurface3DBoxOptions->{Method->{"SplinePoints"->35}},MapIndexed[BSplineSurface[Map[AffineTransform[{RotationMatrix[\[Pi] Mod[#2[[1]]-1,4]/2,{0,0,1}],#1}],ocp . DiagonalMatrix[r {1,1,If[Mod[#2[[1]]-1,8]<4,1,-1]}],{2}],SplineDegree->2,SplineKnots->osk,SplineWeights->owt]&,cv[[{8,4,2,6,7,3,1,5}]]],MapIndexed[Function[{idx,pos},BSplineSurface[Outer[Plus,cv[[idx]],Composition[Insert[#,0,pos[[1]]]&,RotationTransform[\[Pi] (pos[[2]]-1)/2]]/@(r {{1,0},{1,1},{0,1}}),1],SplineDegree->{1,2},SplineKnots->csk,SplineWeights->csw]],ei,{2}],Polygon[MapThread[Map[TranslationTransform[r #2],cv[[#1]]]&,{fi,Join[#,-#]&[IdentityMatrix[3]]}]]}]]


(* ::Input::Initialization:: *)
With[{xlim=0.8},
trajFun[{pt1_,pt2_,pt3_,pt4_}][x_]:=Piecewise[{{BezierFunction[{pt1,pt2,pt3}][Rescale[x,{0,xlim},{0,1}]],0<=x<xlim},{pt3+Rescale[x,{xlim,1},{0,1}]pt4,True}}]
]


(* ::Input::Initialization:: *)
col=ColorData[3]/@{2,4,6};
diskx=-1.8;


(* ::Input::Initialization:: *)
ballFun[col_]:=Inset[Graphics3D[{col,Ball[{0,0,0},.3]},Boxed->False,Lighting->"Neutral"],ImageScaled[{1,1}/2.],ImageScaled[{1,1}/2.],1];


(* ::Input::Initialization:: *)
detFun[num_]:=Module[{det,i=1},
det={Gray,EdgeForm[],Cylinder[{{-1.7,0,0},{0,0,0}},.7],{If[num!=i,Blend[{#,Black},.8]&,Identity]@col[[i++]],Ball[{#,0,1.1},.25],Gray,Scale[Ball[{#,0,1.1},.3],{1,1,0.5}]}&/@Subdivide[-.8,.8,2],roundedCuboid[{-1.5,-1,-1},{1.5,1,1},.3]};
Inset[Graphics3D[{det},Boxed->False,ViewPoint->Above,Lighting->"Neutral"],{1.2,0},ImageScaled[{.1,1}/2.],3]
];


(* ::Input::Initialization:: *)
ClearAll[grMid]
grMid[input_,t_]:=Module[{diskxM=-.9,disks,movdisk,nondisks,ypos={1.,0.,-1.},detdefault=detFun[5]},
disks=Translate[ballFun[col[[#]]],{diskx,2 -#1}]&/@Range[3];
nondisks=Drop[disks,{input}];
movdisk=ballFun[col[[input]]];

Graphics[{nondisks,Translate[movdisk,trajFun[{{diskx,ypos[[input]]},{diskxM,ypos[[input]]},{diskxM,0},{1.2,0}}][t]],detdefault},ImageSize->700,PlotRange->{{-.4,3.8},1.3{-1,1}}]
];


(* ::Input::Initialization:: *)
grPost[input_,indicator_]:=Module[{disks,nondisks},
disks=Translate[ballFun[col[[#]]],{diskx,2 -#1}]&/@Range[3];
nondisks=Drop[disks,{input}];

Graphics[{nondisks,If[input==indicator,{},{Red,Disk[{1.62,0},{.3,.7}],Rectangle[{1.45,-.8},{3.8,.8},RoundingRadius->.3]}],detFun[indicator]},ImageSize->700,PlotRange->{{-.4,3.8},1.3{-1,1}}]
];


(* ::Input::Initialization:: *)
composeAnimation[]:=Module[{preseq,postseq,midseq,step=0.07,exc=3,predur=0.8,middur=0.01,postdur=3,seq,durs},
preseq=grMid[1,0];
midseq=Table[grMid[input,t],{input,3},{t,step,1-step,step}];
postseq=Table[grPost[input,If[input==exc,Mod[exc-2,3,1],input]],{input,3}];

seq=Flatten@Table[Join[{preseq},midseq[[input]],{postseq[[input]]}],{input,3}];
durs=Flatten@Table[Join[{predur},Table[middur,Length[midseq[[input]]]],{postdur}],{input,3}];

{seq,durs}
]


(* ::Input:: *)
(*(*ParametricPlot[trajFun[{{-1.8`,1.5`},{0,1.5`},{0,0},{3,0}}][x],{x,0,1}]*)*)


(* ::Input:: *)
(*(*Manipulate[grMid[input,t],{input,Range[3]},{t,0,1}]*)*)


(* ::Input:: *)
(*(*Manipulate[grPost[input,ind],{input,Range[3]},{ind,Range[3]}]*)*)


(* ::Input:: *)
(*SetDirectory[NotebookDirectory[]]*)
(*{seq,durs}=composeAnimation[];*)


(*Export["animDiscr_Short.gif",seq,AnimationRepetitions->Infinity,ImageResolution->100,"DisplayDurations"->durs]*)
