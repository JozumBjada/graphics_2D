(* ::Package:: *)

(* ::Title:: *)
(*Angle vs. OAM*)


(* ::Subtitle:: *)
(*Wiki animation source code*)


(* ::Text:: *)
(*Source code for file: "OAM_vs_angle_states_animation.gif"*)
(*https://commons.wikimedia.org/wiki/File:OAM_vs_angle_states_animation.gif*)
(*Version: "Wolfram language 12.0.0 for Microsoft Windows (64-bit) (April 6, 2019)"*)


(* ::Section:: *)
(*Routines*)


(* ::Subsection::Closed:: *)
(*Functions*)


(* ::Input::Initialization:: *)
beamLG[k_][\[Rho]_,\[Phi]_]:=Sqrt[2/(\[Pi] Abs[k]!)](Sqrt[2]\[Rho])^Abs[k]Exp[-\[Rho]^2]Exp[-I k \[Phi]]


(* ::Input::Initialization:: *)
angleDistrAmpl[\[Phi]_,\[Lambda]_]:=Surd[\[Lambda]/\[Pi],4]/Sqrt[Erf[\[Pi] Sqrt[\[Lambda]]]] Exp[-(\[Lambda]/2)\[Phi]^2]


(* ::Input::Initialization:: *)
oamDistrProb[ld_,\[Lambda]_]:=oamDistrProb[ld,\[Lambda]]=Abs[Surd[\[Lambda] \[Pi],-4]/Erf[\[Pi] Sqrt[\[Lambda]]]]^2 Abs[NIntegrate[Sinc[k \[Pi]]Exp[-(ld+k)^2/(2\[Lambda])],{k,-\[Infinity],\[Infinity]}(*,WorkingPrecision\[Rule]$MachinePrecision*)]]^2


(* ::Input::Initialization:: *)
deltaPhi[\[Lambda]_]:=Sqrt[1-(2Sqrt[\[Pi] \[Lambda]] Exp[-\[Pi]^2 \[Lambda]])/(Erf[\[Pi] Sqrt[\[Lambda]]])]/Sqrt[2\[Lambda]]


(* ::Input::Initialization:: *)
invDeltaPhi=Module[{lambdvals,dphis,angvals,ifun,lambdas},
lambdvals=Range[0.000001,100,.1];
dphis=Transpose[{lambdvals,Quiet[deltaPhi/@lambdvals]}];
ifun=InverseFunction@Interpolation[dphis];

(*FunctionInterpolation returns some artifacts, so Interpolation is used again*)
angvals=Range[0,2,.01];
lambdas=Transpose[{angvals,ifun/@angvals}];
Interpolation[lambdas]
];


(* ::Subsection::Closed:: *)
(*Plot of superpositions*)


(* ::Input::Initialization:: *)
plotOptsSuper={PlotRange->All,ImagePadding->{{50,0}, {0,0}},ImageSize->{Automatic,300},Exclusions->(#1<0&&#2==0&),PlotPoints->50,MaxRecursion->2,PlotRangePadding->None,Frame->False,ColorFunction->"SunsetColors"};


(* ::Input::Initialization:: *)
superpositionFun[\[Lambda]_,x_,y_,halfnum_:50]:=Module[{vals},
vals=Table[oamDistrProb[mode,\[Lambda]],{mode,-halfnum,halfnum}];
If[\[Lambda]<3,vals=Normalize[vals,Total]]; (*necessary due to numerical errors*)
Sum[Sqrt[vals[[mode+halfnum+1]]] beamLG[mode][Sqrt[x^2+y^2],ArcTan[y,x]],{mode,-halfnum,halfnum}]
]


(* ::Input::Initialization:: *)
plotSuperposition[\[Lambda]_,halfnum_:5]:=Module[{plotlim=3.5},DensityPlot[Evaluate[Abs[superpositionFun[\[Lambda],x,y,halfnum]]^2],{x,-plotlim,plotlim},{y,-plotlim,plotlim},RegionFunction->(#1^2+#2^2<=plotlim^2&),Evaluate[Sequence@@plotOptsSuper]]]


(* ::Input::Initialization:: *)
plotGauss[halfnum_:5]:=Module[{plotlim=3.5},DensityPlot[Evaluate[Abs[beamLG[0][Sqrt[x^2+y^2],ArcTan[y,x]]]^2],{x,-plotlim,plotlim},{y,-plotlim,plotlim},RegionFunction->(#1^2+#2^2<=plotlim^2&),Evaluate[Sequence@@plotOptsSuper]]]


(* ::Subsection::Closed:: *)
(*Plots of angle and OAM distributions*)


(* ::Input::Initialization:: *)
plotOpts={Frame->True,Axes->False,PlotStyle->Blue,PlotRangePadding->{0,Scaled[0.05]},ImageSize->{Automatic,300},ImagePadding->{{60,10}, {70,10}},GridLines->{None,Automatic},FrameStyle->Directive[Thickness[0.005]],LabelStyle->Directive[FontFamily->"Times",FontSize->20,FontColor->Black]};


(* ::Input::Initialization:: *)
plotOptsAngle={Filling->Bottom,PlotRange->{All,{0,4}},FrameLabel->{Style["\[Phi]",20],Style["p",20,Italic]},RotateLabel->False,FrameTicks->{{Automatic,None},{{-\[Pi],{-\[Pi]/2,"-\[Pi]/2"},0,{\[Pi]/2,"\[Pi]/2"},\[Pi]},None}}};


(* ::Input::Initialization:: *)
plotOptsOAM={ExtentSize->.8,PlotRange->{All,{0,1}},FrameLabel->{Style["\[ScriptL]",20],Style["p",20,Italic]},RotateLabel->False,FrameTicks->{{Automatic,None},{Automatic,None}}};


(* ::Input::Initialization:: *)
epilogFun[\[CapitalDelta]\[Phi]_]:={AbsoluteThickness[1],{Dashed,InfiniteLine[{0,0},{0,1}]},InfiniteLine[{\[CapitalDelta]\[Phi],0},{0,1}],InfiniteLine[{-\[CapitalDelta]\[Phi],0},{0,1}],Arrowheads[.05(-1)^Boole[\[CapitalDelta]\[Phi]<0.4] {-1,1}],
If[\[CapitalDelta]\[Phi]<0.5,Arrow@Line[{{\[CapitalDelta]\[Phi],1.5},{-\[CapitalDelta]\[Phi],1.5}}],{Arrow@Line[{{\[CapitalDelta]\[Phi],1.5},{0,1.5}}],Arrow@Line[{{0,1.5},{-\[CapitalDelta]\[Phi],1.5}}]}],If[\[CapitalDelta]\[Phi]>0.5,{Text[Style["\[CapitalDelta]\[Phi]",20,FontFamily->"Times"],{-\[CapitalDelta]\[Phi]/2,1.5},{0,-1.5}],Text[Style["\[CapitalDelta]\[Phi]",20,FontFamily->"Times"],{\[CapitalDelta]\[Phi]/2,1.5},{0,-1.5}]},{}](*If[\[CapitalDelta]\[Phi]>0.2,Text[Style["\[CapitalDelta]\[Phi]",20,FontFamily\[Rule]"Times"],{0,1.5},{0,-1}],{}]*)}
plotAngleDistr[\[Lambda]_]:=Plot[Abs[angleDistrAmpl[\[Phi],\[Lambda]]]^2,{\[Phi],-\[Pi],\[Pi]},Evaluate[Sequence@@plotOptsAngle],Evaluate[Sequence@@plotOpts],Epilog->epilogFun[deltaPhi[\[Lambda]]]];
plotAngleDistrGauss:=Plot[1/(2\[Pi]),{\[Phi],-\[Pi],\[Pi]},Evaluate[Sequence@@plotOptsAngle],Evaluate[Sequence@@plotOpts],Epilog->epilogFun[\[Pi]/Sqrt[3]]];


(* ::Input::Initialization:: *)
plotOAMDistr[\[Lambda]_]:=Module[{halfnum=5,vals,valsaux},
vals=Table[oamDistrProb[i,\[Lambda]],{i,-halfnum,halfnum}];
If[\[Lambda]<3,vals=Normalize[vals,Total]]; (*necessary due to numerical errors*)
DiscretePlot[vals[[i+halfnum+1]],{i,-halfnum,halfnum},Evaluate[Sequence@@plotOptsOAM],Evaluate[Sequence@@plotOpts]]
];
plotOAMDistrGauss:=Module[{halfnum=5},DiscretePlot[KroneckerDelta[i,0],{i,-halfnum,halfnum},Evaluate[Sequence@@plotOptsOAM],Evaluate[Sequence@@plotOpts]]];


(* ::Subsection::Closed:: *)
(*Uncertainty relation plot*)


(* ::Input::Initialization:: *)
plotOptsUncert={PlotStyle->Directive[Thickness[0.005],Blue],GridLines->{\[Pi]/8 {1,2,3,4},1/8 {1,2,3,4}},PlotRange->{{0,\[Pi]/Sqrt[3]+.1},{0,.6}},PlotRangePadding->None,FrameLabel->(Style[#,20]&/@{"\[CapitalDelta]\[Phi]","\[CapitalDelta]\[ScriptL]\[CenterDot]\[CapitalDelta]\[Phi]"}),FrameTicks->{{{0,{1/8,""},{.25,"1/4"},{3/8,""},{.5,"1/2"}},None},{{0,{\[Pi]/8,""},{\[Pi]/4,"\[Pi]/4"},{3\[Pi]/8,""},{\[Pi]/2,"\[Pi]/2"}},None}}};


(* ::Input::Initialization:: *)
plotUncertainty[\[CapitalDelta]\[Phi]0_]:=Module[{auxifun},
auxifun[\[CapitalDelta]\[Phi]_]:=Piecewise[{{invDeltaPhi[\[CapitalDelta]\[Phi]]\[CapitalDelta]\[Phi]^2,\[CapitalDelta]\[Phi]>.2}},1/2];
Plot[auxifun[\[CapitalDelta]\[Phi]],{\[CapitalDelta]\[Phi],0,2},PlotRangeClipping->False,Epilog->{Purple,PointSize[0.03],Point[{\[CapitalDelta]\[Phi]0,auxifun[\[CapitalDelta]\[Phi]0]}],Black,Dashed,Line[{{\[Pi]/Sqrt[3],1/2},{\[Pi]/Sqrt[3],-0.07}}],Text[Style["\[Pi]/\!\(\*SqrtBox[\(3\)]\)",20,FontFamily->"Times"],{\[Pi]/Sqrt[3],-0.07},{0,1}]},Evaluate[Sequence@@plotOptsUncert],Evaluate[Sequence@@plotOpts]]
];


(* ::Subsection::Closed:: *)
(*Composite plots*)


(* ::Input::Initialization:: *)
plotOAMAngleLinearDiscreteRphi[\[CapitalDelta]\[Phi]_]:=Grid[{{plotSuperposition[invDeltaPhi[\[CapitalDelta]\[Phi]],30],plotOAMDistr[invDeltaPhi[\[CapitalDelta]\[Phi]]]},{plotAngleDistr[invDeltaPhi[\[CapitalDelta]\[Phi]]],plotUncertainty[\[CapitalDelta]\[Phi]]}},Spacings->Spacer[15],Alignment->Center];


(* ::Input::Initialization:: *)
gaussGr:=Grid[{{plotGauss[30],plotOAMDistrGauss},{plotAngleDistrGauss,plotUncertainty[\[Pi]/Sqrt[3]]}},Spacings->Spacer[15],Alignment->Center]


(* ::Section:: *)
(*Generate animation and export*)


(* ::Input:: *)
(*ranphi=Subdivide[0.1,\[Pi]/Sqrt[3.]-0.1,40-1];*)


(* ::Input:: *)
(*AbsoluteTiming[rangr=ParallelMap[Quiet@*plotOAMAngleLinearDiscreteRphi,ranphi];]*)


(* ::Input:: *)
(*listgr=rangr;*)
(*listgr=Join[listgr,{gaussGr},Rest@Reverse[listgr]];*)
(*durs=Join[{2},Table[.25,Length[rangr]-1],{2},Table[.1,Length[rangr]-1]];*)


(* ::Input:: *)
(*(*Manipulate[listgr[[i]],{i,1,Length[listgr],1}]*)*)


(* ::Input:: *)
(*SetDirectory[NotebookDirectory[]]*)
(*Export["anim.gif",listgr,ImageResolution->100,AnimationRepetitions->Infinity,"DisplayDurations"->durs]*)


(* ::Input:: *)
(*(*SystemOpen[%]*)*)
