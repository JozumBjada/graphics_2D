(* ::Package:: *)

(* ::Title:: *)
(*Fractional OAM*)


(* ::Subtitle:: *)
(*Wiki image source code*)


(* ::Text:: *)
(*Source code for file: "Fractional_OAM_modes_3_to_4_with_vortices_svg.svg"*)
(*https://commons.wikimedia.org/wiki/File:Fractional_OAM_modes_3_to_4_with_vortices_svg.svg*)
(*Version: "Wolfram language 12.0.0 for Microsoft Windows (64-bit) (April 6, 2019)"*)


(* ::Section:: *)
(*Routines*)


(* ::Input::Initialization:: *)
colorbarFun[colorfun_,{low_,mid_,high_},{minx_:0,maxx_:35},{miny_:-350,maxy_:350},fontFamily_:"Times New Roman"]:=Module[{textoff=20,lineoff=50,fontSize=10,plot},

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


(* ::Input::Initialization:: *)
fourierShift[arr_]:=Module[{nx,ny},
{nx,ny}=Dimensions[arr];
RotateRight[#,Floor[nx/2]]&/@RotateRight[arr,Floor[ny/2]]
]


(* ::Input::Initialization:: *)
plotMagnitudePhase[charge_,zoomnum_:50,sanmplingnum_:2^10]:=Module[{num=sanmplingnum,farr,nx,ny,zoom,magn,phas,transfun,ftmode,inmode,w0=.1,dist=900,len=2,dx,\[Lambda]=808*^-9,imgSize=700},

dx=len/num;
inmode=Table[If[x==0&&y==0,1.,N@Exp[I charge ArcTan[x,y]]Exp[-(x^2+y^2)/w0^2]],{x,-(len/2)+dx,len/2,dx},{y,-(len/2)+dx,len/2,dx}];
inmode=inmode/Norm[inmode,"Frobenius"];
transfun=Table[N@Exp[I \[Pi] dist \[Lambda] (kx^2+ky^2)],{kx,-(1/(2dx)),1/(2dx)-1/len,1/len},{ky,-(1/(2dx)),1/(2dx)-1/len,1/len}];
transfun=fourierShift[transfun];

ftmode=Fourier[fourierShift[inmode]];
farr=fourierShift[InverseFourier[transfun ftmode]];

{nx,ny}=Dimensions[farr];
zoom=farr[[Floor[nx/2-zoomnum];;Floor[nx/2+zoomnum],Floor[ny/2-zoomnum];;Floor[ny/2+zoomnum]]];
zoom=Reverse@Transpose@Reverse[zoom];

magn=Normalize[Abs[zoom]^2,Max];
magn=ListDensityPlot[magn,ImageSize->imgSize,PlotRange->All,ColorFunction->ColorData["SunsetColors"],Frame->False,PlotRangePadding->None];

phas=Arg[zoom];
phas=ListDensityPlot[phas,ImageSize->imgSize,PlotRange->All,ColorFunction->Hue,Frame->False,PlotRangePadding->None];

{magn,phas}
]


(* ::Input::Initialization:: *)
annotatePlot[plot_,circpos_,rad_:ImageScaled[0.03],rest_:{}]:=Show[plot,Graphics[{EdgeForm[{Black,Thickness[.005]}],Thickness[.005],Circle[#,rad]&/@circpos,Transparent,rest},ImageSize->700]]


(* ::Section:: *)
(*Computation*)


(* ::Input:: *)
(*charges34={3,3.25,3.5,3.75,4};*)


(* ::Input:: *)
(*AbsoluteTiming[arrs34=plotMagnitudePhase/@charges34;]*)


(* ::Input:: *)
(*{magn34,phas34}=Transpose[arrs34];*)


(* ::Input:: *)
(*colorBarMagn=colorbarFun["SunsetColors",{0,0.5,1},{0,35},{-350,350}];*)
(*colorBarPhas=colorbarFun[Hue,{0,\[Pi],2\[Pi]},{0,35},{-350,350}];*)


(* ::Input:: *)
(*phas34aux={*)
(*annotatePlot[phas34[[1]],{ImageScaled@{0.5,0.5}},ImageScaled[0.07]],*)
(*annotatePlot[phas34[[2]],{{55.14`,54.86`},{49.14`,47.57`},{49.`,54.14`}}],*)
(*annotatePlot[phas34[[3]],{{55.5,56},{48,56},{46.5,48}},ImageScaled[0.03],Rectangle[ImageScaled[{0.61,0.35}],ImageScaled[{1,0.55}]]],*)
(*annotatePlot[phas34[[4]],{{55.`,56.14`},{48.29`,55.86`},{46.71`,48.43`},{60.`,44.57`}}],*)
(*annotatePlot[phas34[[5]],{ImageScaled@{0.5,0.5}},ImageScaled[0.07]]*)
(*};*)


(* ::Input:: *)
(*grid34aux=Grid[{Framed[Style["\[ScriptL] = "<>ToString[#],60,FontFamily->"Times"],Alignment->Center,ImageSize->{700,60},FrameStyle->Thick]&/@charges34,Join[magn34,{colorBarMagn}],Join[phas34aux,{colorBarPhas}]}]*)


(* ::Section:: *)
(*Export*)


(* ::Input:: *)
(*SetDirectory[NotebookDirectory[]]*)


(* ::Input:: *)
(*Export["grid34vortices.png",grid34aux,ImageResolution->200]*)
(*Export["grid34vortices.svg",grid34aux,ImageResolution->200]*)
