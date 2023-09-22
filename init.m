(* ::Package:: *)

(* ::Text:: *)
(*\:6ce8\:ff1a\:8bf7\:5c06\:6b64\:6587\:4ef6\:7f6e\:4e8e%AppData%\Roaming\Mathematica\Autoload\cppHusky\Kernel\:6587\:4ef6\:5939\:4e0b*)


(* ::Text:: *)
(*Last Updated: cppHusky: Fri 22 Sep 2023 10:44:31 GMT+8*)


DateObject[]


(* ::Section:: *)
(*\:7269\:7406\:5b9e\:9a8c\:6570\:636e\:5904\:7406 Physical Experiment Data Processing*)


(* ::Subsection::Closed:: *)
(*\:6837\:672c\:6807\:51c6\:504f\:5dee*)


SampleStandardDeviation::usage="\:6837\:672c\:6807\:51c6\:504f\:5dee\n\:4f7f\:7528 List \:6c42\:5f97";
SampleStandardDeviation[list_/;Depth[list]===3]:= 
	Sqrt[Variance[list]*Length[list]/(Length[list]-1)]


(* ::Subsection::Closed:: *)
(*\:4e00\:952e\:5206\:6790\:6570\:636e\:7279\:5f81*)


DataAnalyze::usage="\:4e00\:952e\:5206\:6790\:6570\:636e\:7279\:5f81\n\:7b2c\:4e00\:53c2\:6570\:4e3a\:6570\:636e\:5217\:8868\n\:7b2c\:4e8c\:53c2\:6570\:4e3aB\:7c7b\:4e0d\:786e\:5b9a\:5ea6\:ff0c\:4e0d\:586b\:5219\:4e3a0";
DataAnalyze[list_List,UncertaintyB_:0]:=Module[
{ave,\[Sigma],s,u},
	ave=Mean[list];
	\[Sigma]=StandardDeviation[list];
	s=SampleStandardDeviation[list];
	u=Sqrt[s^2+UncertaintyB^2];
Labeled[
	Grid[{
			{"\:5e73\:5747\:503c",ave},
			{"\:6807\:51c6\:5dee",\[Sigma]},
			{"\:6837\:672c\:6807\:51c6\:504f\:5dee",s},
			{"\:5408\:6210\:4e0d\:786e\:5b9a\:5ea6",u},
			{"\:7ed3\:679c\:8868\:8fbe",Around[ave,u]},
			{"\:76f8\:5bf9\:5408\:6210\:4e0d\:786e\:5b9a\:5ea6",PercentForm[u/ave]}
		},
		Frame->{False,All},
		FrameStyle->Gray
	],
	"\!\(\*
StyleBox[\"\:6570\:636e\:7279\:5f81\",\nFontWeight->\"Bold\"]\)",
	Top
]]


(* ::Subsection::Closed:: *)
(*\:9010\:5dee\:6cd5\:6c42\:5747\:503c*)


MeanInDifferences::usage="\:9010\:5dee\:6cd5\:6c42\:5747\:503c\n\:4f7f\:7528 List[[2]]-List[[1]] \:5b9e\:73b0";
MeanInDifferences[list_]:=
	(Total[list[[2]]]-Total[list[[1]]])/Length[list[[1]]]^2;


(* ::Subsection::Closed:: *)
(*\:6837\:672c\:6807\:51c6\:504f\:5dee\:ff08\:9010\:5dee\:6cd5\:ff09*)


SampleStandardDeviationInDifferences::usage="\:9010\:5dee\:6cd5\:6837\:672c\:6807\:51c6\:504f\:5dee\n\:4f7f\:7528\:4e8c\:7ef4 List \:6c42\:5f97";
SampleStandardDeviationInDifferences[list_/;Length[list[[1]]]==Length[list[[2]]]]:=
	Sqrt[Variance[list[[2]]-list[[1]]]*Length[list[[1]]]/(Length[list[[1]]]-1)]


(* ::Subsection::Closed:: *)
(*\:4e00\:952e\:5206\:6790\:6570\:636e\:7279\:5f81\:ff08\:9010\:5dee\:6cd5\:ff09*)


DataAnalyzeInDifferences::usage="\:4e00\:952e\:5206\:6790\:6570\:636e\:7279\:5f81\:ff08\:9010\:5dee\:6cd5\:ff09\n\:7b2c\:4e00\:53c2\:6570\:4e3a\:4e8c\:7ef4\:6570\:7ec4\:ff08\:6bcf\:4e2a\:5143\:7d20\:90fd\:9700\:8981\:662f\:4e00\:4e32\:6570\:636e\:ff09\n\:7b2c\:4e8c\:53c2\:6570\:4e3aB\:7c7b\:4e0d\:786e\:5b9a\:5ea6\:ff0c\:4e0d\:586b\:5219\:4e3a0";
DataAnalyzeInDifferences[list_List,UncertaintyB_:0]:=Module[
{ave,\[Sigma],s,u},
	ave=MeanInDifferences[list];
	\[Sigma]=StandardDeviation[list[[2]]-list[[1]]];
	s=SampleStandardDeviationInDifferences[list];
	u=Sqrt[s^2+UncertaintyB^2];
Labeled[Grid[{
		{"\:5e73\:5747\:503c",ave},
		{"\:6807\:51c6\:5dee",\[Sigma]},
		{"\:6837\:672c\:6807\:51c6\:504f\:5dee",s},
		{"\:5408\:6210\:4e0d\:786e\:5b9a\:5ea6",u},
		{"\:7ed3\:679c\:8868\:8fbe",Around[ave,u]},
		{"\:76f8\:5bf9\:5408\:6210\:4e0d\:786e\:5b9a\:5ea6",PercentForm[u/ave]}
	},
	Frame->{False,All},
	FrameStyle->{Gray}],
	"\!\(\*
StyleBox[\"\:6570\:636e\:7279\:5f81\",\nFontWeight->\"Bold\"]\)",
	Top
]]


(* ::Subsection::Closed:: *)
(*\:7ebf\:6027\:56de\:5f52\:5206\:6790*)


LinearAnalyze::usage="\:7ebf\:6027\:56de\:5f52\:5206\:6790\n\:7b2c\:4e00\:53c2\:6570\:4e3aQuantityArray\:7c7b\:578b\n\:7b2c\:4e8c/\:4e09\:53c2\:6570\:5206\:522b\:662f\:6a2a\:7eb5\:5750\:6807\:540d\:79f0";
LinearAnalyze[datas_QuantityArray,xLabel_String:"x",yLabel_String:"y"]:=Module[
{values,units,lm,sy,sb,sa},
	values=QuantityMagnitude[datas];
	units=QuantityUnit[datas][[1]];
	lm=LinearModelFit[values,x,x];
	sy=Sqrt[Total[(values[[All,2]]-lm/@values[[All,1]])^2]/(Length[values]-2)];
	sb=sy/Sqrt[Mean[values[[All,1]]^2]-Mean[values[[All,1]]]^2];
	sa=Sqrt[Mean[values[[All,1]]^2]]sb;
Labeled[
	Row[{
		Show[
			ListPlot[values,PlotMarkers->"\[Times]",PlotStyle->Red],
			Plot[lm[x],{x,Min[values[[All,1]]-1],Max[values[[All,1]]]+1},PlotStyle->{Blue,Thickness[0.001]}],
			Frame->True,
			ImageSize->Medium,
			FrameLabel->{
				{Row[{yLabel," / ",units[[2]]}],None},
				{Row[{xLabel," / ",units[[1]]}],None}
		}],
		"   ",
		Grid[{
			{"\:81ea\:53d8\:91cf\:5747\:503c\!\(\*OverscriptBox[\(x\), \(_\)]\)",Mean[datas][[1]]},
			{"\:56e0\:53d8\:91cf\:5747\:503c\!\(\*OverscriptBox[\(y\), \(_\)]\)",Mean[datas][[2]]},
			{"\:7eb5\:622a\:8ddd\!\(\*OverscriptBox[\(a\), \(^\)]\)",Quantity[Coefficient[Normal[lm],x,0],units[[2]]]},
			{"\:659c\:7387\!\(\*OverscriptBox[\(b\), \(^\)]\)",Quantity[Coefficient[Normal[lm],x,1],units[[2]]]/Quantity[1,units[[1]]]},
			{"\:7ebf\:6027\:56de\:5f52\:4f30\:8ba1\:516c\:5f0f",Row[{" \!\(\*OverscriptBox[\(y\), \(^\)]\)=",Normal[lm]}]},
			{"\:76f8\:5173\:7cfb\:6570r",Correlation[values[[All,1]],values[[All,2]]]},
			{"y\:6807\:51c6\:504f\:5dees(y)",Quantity[sy,units[[2]]]},
			{"b\:6807\:51c6\:504f\:5dees(b)",Quantity[sb,units[[2]]]/Quantity[1,units[[1]]]},
			{"a\:6807\:51c6\:504f\:5dees(a)",Quantity[sa,units[[2]]]}
		},
		Frame->{False,All},
		FrameStyle->Gray
	]}],
	"\!\(\*
StyleBox[\"\:7ebf\:6027\:56de\:5f52\:5206\:6790\",\nFontWeight->\"Bold\"]\)",
	Top
]]


(* ::Subsection::Closed:: *)
(*\:66f2\:7ebf\:62df\:5408\:5206\:6790*)


CurveAnalyze::usage="\:66f2\:7ebf\:62df\:5408\:5206\:6790\n\:7b2c\:4e00\:53c2\:6570\:4e3aQuantityArray\:683c\:5f0f\n\:53ef\:9009\:7b2c\:4e8c\:53c2\:6570\:ff1a\"line\" \:7ebf\:6027\:ff1b\"exp\" \:6307\:6570\:ff1b\"log\" \:5bf9\:6570\:ff1b\"pow\" \:5e42\:51fd\:6570\:ff1b\"poly3\" 3\:6b21\:591a\:9879\:5f0f\:ff1b\"poly5\" 5\:6b21\:591a\:9879\:5f0f\:ff1b\"poly12\" 12\:6b21\:591a\:9879\:5f0f\:ff1b\"poly30\" 30\:6b21\:591a\:9879\:5f0f";
CurveAnalyze[datas_QuantityArray,model_String]:=Module[
{values,units,nlm},
	values=QuantityMagnitude[datas];
	units=QuantityUnit[datas][[1]];
	nlm=Switch[model,
		"line",LinearModelFit[values,x,x],
		"exp",NonlinearModelFit[values,\[Alpha] E^(\[Beta] x),{\[Alpha],\[Beta]},x],
		"log",NonlinearModelFit[values,Log[\[Alpha]+\[Beta] x],{\[Alpha],\[Beta]},x],
		"pow",NonlinearModelFit[values,\[Alpha] x^\[Beta],{\[Alpha],\[Beta]},x],
		"poly3",LinearModelFit[values,{1,x,x^2,x^3},x],
		"poly5",LinearModelFit[values,{1,x,x^2,x^3,x^4,x^5},x],
		"poly12",LinearModelFit[values,{1,x,x^2,x^3,x^4,x^5,x^6,x^7,x^8,x^9,x^10,x^11,x^12},x],
		"poly30",LinearModelFit[values,{1,x,x^2,x^3,x^4,x^5,x^6,x^7,x^8,x^9,x^10,x^11,x^12,x^13,x^14,x^15,x^16,x^17,x^18,x^19,x^20,x^21,x^22,x^23,x^24,x^25,x^26,x^27,x^28,x^29,x^30},x]
	];
Labeled[
	Column[{
		Column[{
			Switch[model,
				"line","\:7ebf\:6027",
				"exp","\:6307\:6570\:578b",
				"log","\:5bf9\:6570\:578b",
				"pow","\:5e42\:51fd\:6570\:578b",
				"poly5","5\:6b21\:591a\:9879\:5f0f",
				"poly3","3\:6b21\:591a\:9879\:5f0f",
				"poly12","12\:6b21\:591a\:9879\:5f0f",
				"poly30","30\:6b21\:591a\:9879\:5f0f"
			]<>" \:66f2\:7ebf\:62df\:5408\:65b9\:7a0b\:ff1a",
			Row[{"\!\(\*OverscriptBox[\(y\), \(^\)]\)=",Normal[nlm]}]
		}],
		Show[
			ListPlot[values,PlotMarkers->"\[Times]",PlotStyle->Red],
			Plot[nlm[x],{x,Min[values[[All,1]]],Max[values[[All,1]]]},PlotStyle->{Blue,Thickness[0.002]}],
			Frame->True,
			ImageSize->Large
		]
	},
	Alignment->Center
	],
	"\!\(\*
StyleBox[\"\:66f2\:7ebf\:62df\:5408\:5206\:6790\",\nFontWeight->\"Bold\"]\)",
	Top
]]
Clear[addCompletion];
addCompletion=FE`Evaluate[FEPrivate`AddSpecialArgCompletion[#]]&;
addCompletion["CurveAnalyze"->{{""},{"line","exp","log","pow","poly3","poly5","poly12","poly30"}}];


(* ::Subsection::Closed:: *)
(*\:7b80\:5355\:66f2\:7ebf\:6570\:636e\:7ebf\:6027\:5316\:62df\:5408\:5206\:6790*)


CurveLinearizeAnalyze::usage="\:7b80\:5355\:66f2\:7ebf\:6570\:636e\:7ebf\:6027\:5316\:62df\:5408\:5206\:6790\n\:7b2c\:4e00\:53c2\:6570\:4e3aQuantityArray\:7c7b\:578b\n\:53ef\:9009\:7b2c\:4e8c\:53c2\:6570\:ff1a\"exp\" \:6307\:6570\:ff1b\"log\" \:5bf9\:6570\:ff1b\"pow\" \:5e42\:51fd\:6570\n\:7b2c\:4e09/\:56db\:53c2\:6570\:5206\:522b\:662f\:6a2a\:7eb5\:5750\:6807\:540d\:79f0";
CurveLinearizeAnalyze[datas_QuantityArray,model_String,xLabel_String:"x",yLabel_String:"y"]:=Module[
{ori,values,units,nlm,llm,a,b,a0,b0},
	ori=QuantityMagnitude[datas];
	units=QuantityUnit[datas][[1]];
	nlm=Normal[Switch[model,
		"exp",NonlinearModelFit[ori,\[Alpha] E^(\[Beta] x),{\[Alpha],\[Beta]},x],
		"log",NonlinearModelFit[ori,Log[\[Alpha]+\[Beta] x],{\[Alpha],\[Beta]},x],
		"pow",NonlinearModelFit[ori,\[Alpha] x^\[Beta],{\[Alpha],\[Beta]},x]
	]];
	{a0,b0}=Switch[model,
		"exp",nlm/.\[Alpha]_ E^(\[Beta]_ x)->{\[Alpha],\[Beta]},
		"log",nlm/.Log[\[Alpha]_+\[Beta]_ x]->{\[Alpha],\[Beta]},
		"pow",nlm/.\[Alpha]_ x^\[Beta]_->{\[Alpha],\[Beta]}
	];
	values=Table[{ori[[i,1]],Switch[model,
		"exp",Log[ori[[i,2]]],
		"log",E^ori[[i,2]],
		"pow",ori[[i,2]]^(1/b0)
	]},{i,Length[datas]}];
	llm=LinearModelFit[values,x,x];
	{a,b}={Coefficient[Normal[llm],x,0],Coefficient[Normal[llm],x,1]};
Labeled[
	Row[{
		Show[
			ListPlot[values,PlotMarkers->"\[Times]",PlotStyle->Red],
			Plot[llm[x],{x,Min[values[[All,1]]]-1,Max[values[[All,1]]]+1},PlotStyle->{Blue,Thickness[0.001]}],
			Frame->True,
			ImageSize->Medium,
			FrameLabel->{
				{Row[{yLabel<>" - "<>ToString[Switch[model,
					"exp",HoldForm[Log[y]],
					"log",HoldForm[Exp[y]],
					"pow",HoldForm[y^(1/c)]/.{c->b}
				]]}],None},
				{Row[{xLabel<>" - x"}],None}
		}],
		"   ",
		Grid[{
			{"\:81ea\:53d8\:91cf\:5747\:503c\!\(\*OverscriptBox[\(x\), \(_\)]\)",Mean[values][[1]]},
			{Row[{"\:56e0\:53d8\:91cf\:5747\:503c",Switch[model,
				"exp",\!\(\*OverscriptBox[\(HoldForm[Log[y]]\), \(_\)]\),
				"log",\!\(\*OverscriptBox[\(HoldForm[
\*SuperscriptBox[\(\[ExponentialE]\), \(y\)]]\), \(_\)]\),
				"pow",\!\(\*OverscriptBox[\(HoldForm[
\*SuperscriptBox[\(y\), 
FractionBox[\(1\), \(c\)]]]\), \(_\)]\)/.{c->b}
			]}],Mean[values][[2]]},
			{"\:7eb5\:622a\:8ddd\!\(\*OverscriptBox[\(a\), \(^\)]\)",Switch[model,
				"exp",HoldForm[Log[c]]/.{c->a},
				"log",a,
				"pow",0
			]},
			{"\:659c\:7387\!\(\*OverscriptBox[\(b\), \(^\)]\)",Switch[model,
				"exp",b,
				"log",b,
				"pow",TraditionalForm[a^b]
			]},
			{"\:7ebf\:6027\:56de\:5f52\:4f30\:8ba1\:516c\:5f0f",Row[{
				Switch[model,
					"exp",HoldForm[Log[y]],
					"log",HoldForm[E^y],
					"pow",HoldForm[y]
				],
				"=",
				Switch[model,
					"exp",HoldForm[Log[c]+e x]/.{c->a,e->b},
					"log",HoldForm[c+e x]/.{c->a,e->b},
					"pow",HoldForm[c x^e]/.{c->a0,e->b0}
				]
			}]},
			{"\:76f8\:5173\:7cfb\:6570r",Correlation[values[[All,1]],values[[All,2]]]}
		},
		Frame->{False,All},
		FrameStyle->Gray
	]}],
	"\!\(\*
StyleBox[\"\:7ebf\:6027\:5316\:56de\:5f52\:5206\:6790\",\nFontWeight->\"Bold\"]\)",
	Top
]]
Clear[addCompletion];
addCompletion=FE`Evaluate[FEPrivate`AddSpecialArgCompletion[#]]&;
addCompletion["CurveLinearizeAnalyze"->{{""},{"exp","log","pow"}}];
