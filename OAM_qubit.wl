(* ::Package:: *)

(* ::Title:: *)
(*OAM qubit superpositions*)


(* ::Subtitle:: *)
(*Wiki image source code*)


(* ::Text:: *)
(*Source code for file: "OAM_qubit.svg"*)
(*https://commons.wikimedia.org/wiki/File:OAM_qubit.svg*)
(*Version: "Wolfram language 12.0.0 for Microsoft Windows (64-bit) (April 6, 2019)"*)


(* ::Section:: *)
(*Routines*)


(* ::Input::Initialization:: *)
beamLG[k_][\[Rho]_,\[Phi]_]:=Sqrt[2/(\[Pi] Abs[k]!)](Sqrt[2]\[Rho])^Abs[k]Exp[-\[Rho]^2]Exp[-I k \[Phi]]


(* ::Input::Initialization:: *)
colOpts={PlotRange->All,ImageSize->{Automatic,300},Exclusions->(#1<0&&#2==0&),MaxRecursion->Automatic,PlotRangePadding->None,Frame->False};


(* ::Input::Initialization:: *)
plotSuperpositionMagn[coefsi_,modes_,plotPoints_:50,circ_:False]:=Module[{plotlim=3.5,fun,x,y,coefs=Normalize[coefsi]},
fun=Total@MapThread[#1 beamLG[#2][Sqrt[x^2+y^2],ArcTan[y,x]]&,{coefs,modes}];
DensityPlot[Evaluate[Abs[fun]^2],{x,-plotlim,plotlim},{y,-plotlim,plotlim},
PlotPoints->plotPoints,ColorFunction->"SunsetColors",RegionFunction->If[circ,(#1^2+#2^2<=plotlim^2&),True],Evaluate[Sequence@@colOpts]]
]


(* ::Input::Initialization:: *)
plotSuperpositionPhas[coefsi_,modes_,plotPoints_:50,circ_:False]:=Module[{plotlim=3.5,fun,x,y,coefs=Normalize[coefsi],\[Rho],\[Phi]},
fun=Total@MapThread[#1 beamLG[#2][\[Rho],\[Phi]]&,{coefs,modes}];
fun=Mod[Arg[fun],2\[Pi]]/(2\[Pi])/.{\[Rho]->Sqrt[x^2+y^2],\[Phi]->ArcTan[y,x]};
DensityPlot[fun,{x,-plotlim,plotlim},{y,-plotlim,plotlim},
PlotPoints->plotPoints,ColorFunction->Hue,ColorFunctionScaling->False,RegionFunction->If[circ,(#1^2+#2^2<=plotlim^2&),True],Evaluate[Sequence@@colOpts]]
]


(* ::Input::Initialization:: *)
phaseLabel[ang_,pos_]:=Text[Style[ang,FontSize->35,FontFamily->"Times",Black],ImageScaled[pos]]
phaseGr[hue_,pol1_,pol2_,lab1_,lab2_]:=Graphics[{Hue[hue],Polygon[pol1],Hue[hue+1/2],Polygon[pol2],phaseLabel@@lab1,phaseLabel@@lab2},ImageSize->{Automatic,300},PlotRangePadding->None]


(* ::Input::Initialization:: *)
colorbarFun[colorfun_,{low_,mid_,high_},{minx_,maxx_},{miny_,maxy_},fontFamily_:"Times"]:=Module[{textoff=20,lineoff=50,fontSize=30,plot},
plot=DensityPlot[y+miny,{x,minx,maxx},{y,miny,maxy},ColorFunction->colorfun,Frame->False,PlotRangePadding->None];
plot=First@Cases[InputForm@plot,_GraphicsComplex,Infinity,1];
Graphics[{
Rectangle[{-2+minx,miny},{maxx+1,maxy}],
plot,
Line[{
{{-2+minx,miny},{lineoff+maxx,miny}},
{{-2+minx,maxy+1},{lineoff+maxx,maxy+1}},
{{-2+minx,0},{maxx+textoff/2,0}}
}],
Text[Style[low,FontFamily->fontFamily,FontSize->fontSize],{textoff+maxx,miny},{-1,-1}],
Text[Style[mid,FontFamily->fontFamily,FontSize->fontSize],{textoff+maxx,0},{-1,0}],
Text[Style[high,FontFamily->fontFamily,FontSize->fontSize],{textoff+maxx,maxy},{-1,1}]
},ImageSize->{Automatic,(maxy-miny)},PlotRangePadding->None]
]


(* ::Section:: *)
(*Qubit case*)


(* ::Input:: *)
(*colorBarMagn=colorbarFun["SunsetColors",{0,0.5,1},{0,35},{-150,150}];*)
(*(*colorBarPhas=colorbarFun[Hue,{Style[0,FontFamily\[Rule]"Times"],\[Pi],Row[{Style[2,FontFamily\[Rule]"Times"],"\[Pi]"}]},{0,35},{-150,150},Automatic];*)*)
(*colorBarPhas=colorbarFun[Hue,{0,\[Pi],2\[Pi]},{0,35},{-150,150}];*)


(* ::Input:: *)
(*kets={Row[{Ket[1],Spacer[{0,40,17}]}],Row[{Ket[-1],Spacer[{0,40,17}]}],Row[{1/Sqrt[2],"(",Ket[1],"\[ThinSpace]+\[ThinSpace]",Ket[-1],")"}],Row[{1/Sqrt[2],"(",Ket[1],"\[ThinSpace]-\[ThinSpace]",Ket[-1],")"}],Row[{1/Sqrt[2],"(",Ket[1],"\[ThinSpace]+\[ThinSpace]\[ImaginaryI]\[ThinSpace]", Ket[-1],")"}],Row[{1/Sqrt[2],"(",Ket[1],"\[ThinSpace]-\[ThinSpace]\[ImaginaryI]\[ThinSpace]", Ket[-1],")"}]};*)
(*kets=Framed[Style[Row[{Ket["\[Psi]"]," = ",#}],30,FontFamily->"Times"],ImageSize->{300,100},Alignment->Center]&/@kets;*)


(* ::Input:: *)
(*pts ={{0,1},{-1,1},{-1,-1},{0,-1},{1,-1},{1,1}};*)
(*w = {1,.5, .5, 1, .5, .5};*)
(*k ={0, 0, 0, 1/4, 1/2, 1/2, 3/4, 1, 1};*)
(*arrow={Arrowheads[.1],Thickness[0.01],Arrow@BSplineCurve[RotationTransform[\[Pi]/4][pts], SplineDegree -> 2, SplineKnots -> k, SplineWeights -> w]};*)


(* ::Input:: *)
(*coefs={{1},{1},{1,1},{1,-1},{1,I},{1,-I}};*)
(*modes={{1},{-1},{1,-1},{1,-1},{1,-1},{1,-1}};*)
(*magns=MapThread[plotSuperpositionMagn[#1,#2]&,{coefs,modes}];*)
(*(*phases could also be created as MapThread[plotSuperpositionPhas[#1,#2]&,{coefs,modes}], but this approach results in plots with jagged edges*)*)
(*phases={*)
(*Show[plotSuperpositionPhas[{1},{1}],Epilog->arrow],*)
(*Show[plotSuperpositionPhas[{1},{-1}],Epilog->Scale[arrow,{-1,1}]],*)
(*phaseGr[0/8,{{0,.5},{1,.5},{1,1},{0,1}},{{0,0},{1,0},{1,.5},{0,.5}},{\[Pi],{.5,.25}},{0,{.5,.75}}],*)
(*phaseGr[2/8,{{0,0},{.5,0},{.5,1},{0,1}},{{.5,0},{1,0},{1,1},{.5,1}},{\[Pi]/2,{.25,.5}},{3\[Pi]/2,{.75,.5}}],*)
(*phaseGr[1/8,{{0,0},{1,1},{0,1}},{{0,0},{1,0},{1,1}},{\[Pi]/4,{.25,.75}},{5\[Pi]/4,{.75,.25}}],*)
(*phaseGr[3/8,{{0,0},{1,0},{0,1}},{{1,0},{1,1},{0,1}},{3\[Pi]/4,{.25,.25}},{7\[Pi]/4,{.75,.75}}]*)
(*};*)


(* ::Input:: *)
(*grid=Grid[{Append[kets,""],Append[magns,colorBarMagn],Append[phases,colorBarPhas]},Spacings->{{3->3,5->3},Automatic},Dividers->{{3->Directive[Black,Thick],5->Directive[Black,Thick]},False}]*)


(* ::Input:: *)
(*SetDirectory[NotebookDirectory[]]*)


(* ::Input:: *)
(*Export["gridQubit.png",grid,ImageResolution->200]*)
(*Export["gridQubit.svg",grid,ImageResolution->200]*)
