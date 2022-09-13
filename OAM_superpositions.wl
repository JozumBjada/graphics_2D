(* ::Package:: *)

(* ::Title:: *)
(*OAM superpositions*)


(* ::Subtitle:: *)
(*Wiki image source code*)


(* ::Text:: *)
(*Source code for file: "OAM_superpositions.svg"*)
(*https://commons.wikimedia.org/wiki/File:OAM_superpositions.svg*)
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
(*Generation of figure and export*)


(* ::Input:: *)
(*colorBarMagn=colorbarFun["SunsetColors",{0,0.5,1},{0,35},{-150,150}];*)
(*(*colorBarPhas=colorbarFun[Hue,{Style[0,FontFamily\[Rule]"Times"],\[Pi],Row[{Style[2,FontFamily\[Rule]"Times"],"\[Pi]"}]},{0,35},{-150,150},Automatic];*)*)
(*colorBarPhas=colorbarFun[Hue,{0,\[Pi],2\[Pi]},{0,35},{-150,150}];*)


(* ::Input:: *)
(*coefs={{1,1},{1,2,3},{1,-2I,3}};*)
(*modes={{5,-5},{1,2,3},{1,-2,-3}};*)
(*{magns,phases}=Transpose@MapThread[{plotSuperpositionMagn[#1,#2],plotSuperpositionPhas[#1,#2,70]}&,{coefs,modes}];*)


(* ::Input:: *)
(*kets={Row[{Ket[5],Ket[-5]},"\[ThinSpace]+\[ThinSpace]"],Row[{Ket[1],2 Ket[2],3 Ket[3]},"\[ThinSpace]+\[ThinSpace]"],Row[{Ket[1],"\[ThinSpace]-\[ThinSpace]",2 I Ket[-2],"\[ThinSpace]+\[ThinSpace]",3 Ket[-3]}]};*)
(*kets=Framed[Style[Row[{Ket["\[Psi]"]," \[Proportional] ",#}],25,FontFamily->"Times"],ImageSize->{300,100},Alignment->Center]&/@kets;*)


(* ::Input:: *)
(*grid=Grid[{Append[kets,""],Append[magns,colorBarMagn],Append[phases,colorBarPhas]}];*)


(* ::Input:: *)
(*SetDirectory[NotebookDirectory[]]*)


(* ::Input:: *)
(*Export["gridRand.png",grid,ImageResolution->200]*)
(*Export["gridRand.svg",grid,ImageResolution->200]*)
