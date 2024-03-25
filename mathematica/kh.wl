(* ::Package:: *)

(* :Name: kh` *)
(* :Author: Hongzhe Zhou, Shanghai, 2024*)
(* :Summary:
    Data processing package for Kharma.
*)


BeginPackage["kh`",
  "khRead`","khPlot`"
]


(* ::Chapter:: *)
(*Usage messages*)


khInitialize::usage="khInitialize[] initializes the package, e.g., setting the default plot style.";
khParallelize::usage="khParallelize[n] sets up the package for multiple kernels.
Input:
  n: Integer. Optional. Launches n subkernels. If not provided then launches all
     configured subkernels.
";

khFunctions::usage="khFunctions[] returns a list of all available functions in this package.
khFunctions[str] returns those whose names contain the String str."


Begin["`Private`"]


(* ::Chapter:: *)
(*Functions*)


(* ::Section:: *)
(*One-click initialization*)


khDirectory=$InputFileName//DirectoryName;
khInitialize[]:=Module[{},
  khPlotStyle[]
]
khParallelize[n_|PatternSequence[]]:=With[{dir=khDirectory},
  CloseKernels[];
  LaunchKernels[n]//Quiet;
  ParallelEvaluate[
    AppendTo[$Path,dir];
    Needs["kh`"];
    khInitialize[]
  ];
]


(* ::Section:: *)
(*Extract all available functions*)


khFunctions[]:=Cases[Names["kh*`*"],x_/;!StringContainsQ[x,"Private"]]
khFunctions[str_String]:=Cases[khFunctions[],_?(StringContainsQ[#,str]&)]


(* ::Chapter:: *)
(*End*)


End[]


Protect[
  khInitialize,khParallelize,
  khFunctions
]


EndPackage[]
