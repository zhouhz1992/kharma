(* ::Package:: *)

(* :Name: khPlot` *)
(* :Author: Hongzhe Zhou, Shanghai, 2024*)
(* :Summary:
    Define general plot styles, and functions that make plots.
*)


BeginPackage["khRead`"]


(* ::Chapter:: *)
(*Usage messages*)


readPar::usage="readPar[sim] reads the *.par file."
readVAR::usage="readVAR[sim,file] reads data files."


Begin["`Private`"]


(* ::Chapter:: *)
(*Functions*)


(* ::Section:: *)
(*Read *.par file*)


readPar[sim_]:=Module[{par,groupPos,groups,items,toRule},
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


(* ::Section:: *)
(*Read data*)


readVAR[sim_,file_]:=Module[{tmp=Import[file,"Data"],
  x,y,z,nB,nx,ny,nz,zeroArr,rho,uvec,u,Bvec},
  
  {x,y,z}=tmp["/VolumeLocations/"<>#]&/@{"x","y","z"};
  {{nB,nx},{nB,ny},{nB,nz}}=Dimensions/@{x,y,z};
  
  zeroArr[i_Integer]:=ConstantArray[0,{nB,i,ny,nx}]/;nz==1;
  zeroArr[i_Integer]:=ConstantArray[0,{nB,i,nz,ny,nx}];
  If[MemberQ[Keys[tmp],"/prims.rho"],rho=tmp["/prims.rho"],rho=zeroArr[1]];
  If[MemberQ[Keys[tmp],"/prims.uvec"],uvec=tmp["/prims.uvec"],uvec=zeroArr[3]];
  If[MemberQ[Keys[tmp],"/prims.u"],u=tmp["/prims.u"],u=zeroArr[1]];
  If[MemberQ[Keys[tmp],"/prims.B"],Bvec=tmp["/prims.B"],Bvec=zeroArr[3]];
  
  (* Transform data structure *)
  {rho,u}=#[[;;,1]]&/@{rho,u};
  {uvec,Bvec}=Transpose[#,1<->2]&/@{uvec,Bvec};
  
  Association[
    "Info"->AssociationThread[{"nBlock","nx","ny","nz"}->{nB,nx,ny,nz}],
    "Grid"->AssociationThread[{"x","y","z"}->{x,y,z}],
    "Field"->AssociationThread[{"rho","uvec","u","Bvec"}->{rho,uvec,u,Bvec}]
  ]
]


(* ::Chapter:: *)
(*End*)


End[]


Protect[
  readPar,readVAR
]


EndPackage[]
