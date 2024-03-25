(* ::Package:: *)

(* :Name: khPlot` *)
(* :Author: Hongzhe Zhou, Shanghai, 2024*)
(* :Summary:
    Define general plot styles, and functions that make plots.
*)


BeginPackage["khRead`"]


(* ::Chapter:: *)
(*Usage messages*)


khReadPar::usage="khReadPar[sim] read the *.par file."


Begin["`Private`"]


(* ::Chapter:: *)
(*Functions*)


(* ::Section:: *)
(*Read *.par file*)


khReadPar[sim_]:=Module[{par,groupPos,groups,items,toRule},
  par=With[{f=FileNames["*.par",sim]},
    If[Length[f]>1,
      Return["Error: multiple *.par files found."],
     Import[f[[1]],"Text"]//StringDelete[#," "]&
                        //StringDelete[#,"&\n"]& (* continueing lines *)
                        //StringSplit[#,"\n"]&
                        //DeleteCases[#,""]& (* empty lines *)
                        //DeleteCases[#,x_String/;StringStartsQ[x,"#"]]& (* comments *)
    ]
  ];
  
  groupPos=Position[par,x_String/;StringStartsQ[x,"<"]]//Flatten;
  groups=StringTake[Extract[par,{#}],{2,-2}]&/@groupPos;
  items=Take[par,#]&/@(Partition[Flatten@{groupPos,0},2,1]+Threaded[{1,-1}]);
  
  (* make a string "a=b" to the form Rule["a",b], where b becomes an expression if necessary *)
  toRule[str_String]:=Module[{a,b},
    {a,b}=StringSplit[str,"="];
    If[b=="true" ||b=="false",Return[a->(b=="true")]];
    If[StringMatchQ[b,NumberString~~___],Return[a->ToExpression@StringReplace[b,"e"->"*^"]]];
    If[StringContainsQ[b,","],Return[a->StringSplit[b,","]]];
    Return[a->b]
  ];
  
  AssociationThread[groups->(Association/@Map[toRule,items,{2}])]
]


(* ::Chapter:: *)
(*End*)


End[]


Protect[
  khReadPar
]


EndPackage[]
