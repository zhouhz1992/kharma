(* ::Package:: *)

(* :Name: khPlot` *)
(* :Author: Hongzhe Zhou, Shanghai, 2024*)
(* :Summary:
    Define general plot styles, and functions that make plots.
*)


BeginPackage["khPlot`"]


(* ::Chapter:: *)
(*Usage messages*)


khHexColor::usage="Takes in a Hex color code and outputs the color."
khColors::usage="Some self-defined colors. Use khColors[colorName,{min,max}] for a data set where
0 will always be rescaled to 0.5, and the larger one in Abs[min] and max will be rescaled to 0 or 1.
Useful when plotting butterfly diagrams etc."
khLabelStyle::usage="Font and size.";
khPlotStyle::usage="Set some plot styles.";

khTicks::usage="Frame ticks in convenient forms."

khLegend::usage="khLegend[l,opts] uses DensityPlot to make a bar legend.
Input:
  l: List. MinMax[l] will specify the lower and upper bounds of the data.
Options:
  opts: Will be inherited by DensityPlot and overwrite the default ones.
        Usually one needs to adjust ImageSize, ImagePadding, and AspectRatio to have
        the best looking.
Output:
  A bar legend as an Image object, which can be aligned with other figures using Grid."

khDensityPlot::usage="khDensityPlot[data] plots 2D color plots using ArrayPlot. It has better performance
than ListDensityPlot and should be generally used on equidistant grids.
Input:
  data: Can either be a Nx by 3 List {gridx,gridy,gridf} or its Transpose. Need not be Sort'ed.
Options:
  All options inherit those of ArrayPlot."

khPolarDensityPlot::usage="khPolarDensityPlot[r,theta,f,opts] plots 2D color plots in the polar coordinates
using Graphics combined with Annulus. It has better performance than using ListDensityPlot.
Input:
  r: The radial coordinates. E.g., readGrid[sim][[1]].
  theta: The polar coordinates. E.g., readGrid[sim][[2]].
  f: A 2D array of dimension {Length[r],Length[theta]}.
Options:
  \"DataOptions\" should be a List with the following possible Rules:
    \"DownSamplingFractions\": A List with two positive real numbers {dsr,dsth}, to down-sample data.
    \"ColorFunction\": Could be ColorData[...] or khColors[...]. The default is khColors[\"Rainbow\"].
  \"PlotOptions\" will be inherited by the output Graphics[...].
Example:
  khPolarDensityPlot[{r,theta,lnrho},\[IndentingNewLine]    \"DataOptions\"->{\"DownSamplingFractions\"->{2,3},\"ColorFunction\"->ColorData[\"BlueGreenYellow\"]},\[IndentingNewLine]    \"PlotOptions\"->{FrameLabel->{\"x\",\"y\"}}\[IndentingNewLine]  ]"


Begin["`Private`"]


(* ::Chapter:: *)
(*Functions*)


(* ::Section:: *)
(*Self-defined colors*)


khHexColor[hex_]:=RGBColor@@(IntegerDigits[ToExpression@StringReplace[hex,"#"->"16^^"],256,3]/255.)

khColors[cf_Function]:=cf
khColors[name_String]:=Switch[name,
  "Red",RGBColor[{166,42,23}/255],
  "Blue",RGBColor[{28,77,124}/255],
  "Green",RGBColor[{1,113,0}/255],
  "Magenta",RGBColor[{151,14,83}/255],
  "RainbowR",ColorData[{"Rainbow","Reversed"}],
  "BlueBlackRed",Blend[{khColors["Blue"],khHexColor["#006C65"],Black,khHexColor["#E2792E"],khColors["Red"]},#]&,
  _,ColorData[name]
]
khColors[name_,{min_,max_}]:=khColors[name][If[Abs[min]>=max,-0.5/min*(#-min),0.5/max*(#-max)+1]]&


(* ::Section:: *)
(*Plot style related*)


khLabelStyle=Directive[AbsoluteThickness[1],Black,14,FontFamily->"Times"];
khPlotStyle[]:=Module[{setOps},
  setOps[ops_List,funcs_List]:=Map[SetOptions[#,ops]&,funcs];
  (*General options for all plots*)
  setOps[{
      PlotRange->All,Frame->True,LabelStyle->khLabelStyle,
      FrameStyle->khLabelStyle,ImageSize->{360,360/GoldenRatio},
      ImagePadding->{{50,50},{50,10}}
    },{
      Plot,LogPlot,LogLogPlot,LogLinearPlot,DensityPlot,
      ListPlot,ListLogPlot,ListLogLogPlot,ListLogLinearPlot,ListLinePlot,
      ListDensityPlot,ListVectorPlot,ListStreamPlot,
      Histogram,SmoothHistogram
    }];
  (*Options for 1D plots*)
  setOps[{
      Method->"DefaultPlotStyle"->Directive[Black,AbsoluteThickness[1]]
    },{
      Plot,LogPlot,LogLogPlot,LogLinearPlot,
      ListPlot,ListLogPlot,ListLogLogPlot,ListLogLinearPlot,ListLinePlot,
      SmoothHistogram
    }];
  (*Options for 1D List plots*)
  setOps[{
      Joined->True
    },{
      ListPlot,ListLogPlot,ListLogLogPlot,ListLogLinearPlot,ListLinePlot
    }];
  (*Options for 2D plots*)
  setOps[{
      PlotLegends->Automatic,ColorFunction->khColors["Rainbow"],
      PlotRangePadding->None
    },{
      DensityPlot,ListDensityPlot
    }];
  setOps[{
      RegionBoundaryStyle->None,RegionFillingStyle->None
    },{
      ListVectorPlot,ListStreamPlot,ListVectorDensityPlot,ListStreamDensityPlot
    }];
  (*Options for ListDensity Plot*)
  setOps[{
      InterpolationOrder->0
    },{
      ListDensityPlot
    }];
]

khTicks["10^i",max_:99]:=Table[{10^i,Superscript["10",ToString@i]},{i,-max,max}]
khTicks["Log10i",max_:99]:=Table[{10^i,ToString@i},{i,-max,max}]
khTicks["Range"][range_,pd_]:=List[
  {#,StringPadRight[ToString[#],pd,"0"]}&/@range, Automatic
]


(* ::Section:: *)
(*Bar legend*)


khLegend[l_List,opt:OptionsPattern[]]:=DensityPlot[y,{x,0,1},{y,Sequence@@MinMax[l]},opt,
  ColorFunction->khColors["Rainbow"],PlotLegends->None,
  FrameTicks->{{None,All},{None,None}},PlotRangePadding->None,
  AspectRatio->12,ImagePadding->{{5,40},{5,5}},ImageSize->{80,240}
]

khLegend[l_List,"h",opt:OptionsPattern[]]:=DensityPlot[x,{x,Sequence@@MinMax[l]},{y,0,1},opt,
  ColorFunction->ColorData[{"Rainbow","Reversed"}],PlotLegends->None,
  FrameTicks->{{None,None},{All,None}},PlotRangePadding->None,
  AspectRatio->1/12,ImagePadding->{{5,5},{40,5}},ImageSize->{240,80}
]


(* ::Section:: *)
(*Wrapper for ArrayPlot*)


(* to-do: All places where ListDensityPlot is used for a equidistant grid, for better performance *)
khDensityPlot[{gridx_,gridy_,data_},opts:OptionsPattern[]]:=Module[{x,y,f,frameLabel},
  {x,y,f}=Transpose[{gridx,gridy,data}]//Sort//Transpose;
  f=Reverse[Transpose@Partition[f,y//Union//Length]];
  
  (* by default ArrayPlot will transpose the xy frame labels; here is a fix *)
  frameLabel=With[{op=Association[opts]},
    If[KeyExistsQ[op,FrameLabel],
      If[Length[op[FrameLabel]]==2,Reverse[op[FrameLabel]],op[FrameLabel]],
      None
    ]
  ];
  ArrayPlot[f,FrameLabel->frameLabel,opts,
    DataRange->{x//MinMax,y//MinMax},
    AspectRatio->Abs[(Subtract@@MinMax[y])/(Subtract@@MinMax[x])],
    PlotRangePadding->None,ColorFunction->khColors["Rainbow"],
    FrameTicks->{{Subdivide[Sequence@@MinMax[y],4],Automatic},{Subdivide[Sequence@@MinMax[x],4],Automatic}},
    FrameStyle->Directive[Black,AbsoluteThickness[1]],
    FrameTicksStyle->ConstantArray[{khLabelStyle,Directive[FontOpacity->0,FontSize->0]},2],
    LabelStyle->khLabelStyle,PlotLegends->Automatic
  ]
]
khDensityPlot[data_List,opts:OptionsPattern[]]:=khDensityPlot[Transpose[data],opts]/;Dimensions[data][[-1]]==3


(* ::Section:: *)
(*Density plot in polar coordinates*)


Options[khPolarDensityPlot]={"DataOptions"->{},"PlotOptions"->{Frame->True}};
khPolarDensityPlot[{r0_List,theta0_List,f0_List},OptionsPattern[]]:=Module[{opts,dsr,dsth,r,th,f,cf,dr,dth,ann},
  opts=Association[OptionValue["DataOptions"]];
  
  (* remap polar angle from [0,\[Pi]] to [\[Pi]/2,-\[Pi]/2] *)
  r=r0;
  th=Reverse[\[Pi]/2-theta0];
  f=Reverse/@f0;
  
  (* down-sampling fractions *)
  (* e.g. If dsr==2 then down-sample r direction every 2 mesh points *)
  {dsr,dsth}=If[MemberQ[Keys[opts],"DownSamplingFractions"],opts["DownSamplingFractions"],{1,1}];
  r=ArrayResample[r,Scaled[1/dsr]];
  th=ArrayResample[th,Scaled[1/dsth]];
  f=ArrayResample[f,Scaled[1/#]&/@{dsr,dsth}];
  dr=1.02*Flatten@{0,r//Differences,0};
  dth=1.02Flatten@{0,th//Differences,0};
  
  (* color function *)
  cf[x_]:=If[MemberQ[Keys[opts],"ColorFunction"],opts["ColorFunction"],khColors["Rainbow"]]@Rescale[x,f//Flatten//MinMax];
  
  (* generate cells *)
  ann=Table[
    {EdgeForm[],cf[f[[i,j]]],Annulus[{0,0},{r[[i]]-dr[[i]]/2,r[[i]]+dr[[i+1]]/2},{th[[j]]-dth[[j]]/2,th[[j]]+dth[[j+1]]/2}]},
    {i,1,r//Length},{j,1,th//Length}
  ]//Flatten[#,1]&;
  
  (* plot *)
  Graphics[ann,
    OptionValue["PlotOptions"],
    Frame->True,LabelStyle->khLabelStyle,FrameStyle->khLabelStyle,
    ImagePadding->{{50,50},{50,10}},Background->Transparent
  ]
]


(* ::Chapter:: *)
(*End*)


End[]


Protect[
  khHexColor,khColors,
  khLabelStyle,khPlotStyle,khTicks,
  khLegend,
  khDensityPlot,khPolarDensityPlot
]


EndPackage[]
