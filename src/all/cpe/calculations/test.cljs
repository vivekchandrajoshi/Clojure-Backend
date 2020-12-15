(ns cpe.calculations.test
  (:require [cpe.calculations.calculations :as c]
            [cljs.core.async :refer [put! take! chan promise-chan <! close!]]
            [cpe.log :as log])
  (:require-macros [cljs.core.async.macros :refer [go]]))
(def test-diesel-hdt-data [{:Recycle-H2-To-Pass-A 22675.5471069336, :Recycle-H2-To-Pass-B 22673.7760918511, :Make-Up-Gas-From-Unicrack-H2 12460.5075174967, :RX-A-1-Inlet-Pressure 913.707207743328, :RX-A-1-Feed-From-Heater-A 590.762476560804, :RX-A-1-Top-Bed-Up-A 609.136903381348, :RX-A-1-Top-Bed-Up-B 610.386888334486, :RX-A-1-Top-Bed-Up-C 608.937475585939, :RX-A-1-Top-Bed-Low-D 617.092998674181, :RX-A-1-Top-Bed-Low-E 616.63958023919, :RX-A-1-Top-Bed-Low-F 616.167563968235, :A-1-Average-Top-Bed-Low-Temp 616.866289456686, :H2-Quench-To-MID-RX-A-1 7060.68928256565, :RX-A-1-Bot-Bed-Up-G 595.717363993327, :RX-A-1-Bot-Bed-Up-H 599.994537565442, :RX-A-1-Bot-Bed-Up-I 601.307593960232, :A-1-Average-Bottom-Bed-Up-Temp 599.006498506334, :RX-A-1-Bot-Bed-Mid-J 599.517688030667, :RX-A-1-Bot-Bed-Mid-K 598.873632346259, :RX-A-1-Bot-Bed-Mid-L 598.726735432943, :RX-A-1-Bot-Bed-Low-M 606.470467758179, :RX-A-1-Bot-Bed-Low-N 603.75020480686, :RX-A-1-Bot-Bed-Low-O 609.423052512276, :A-1-Average-Bottom-Bed-Low-Temp 606.547908359105, :RX-A-1-Overall-Delta-P 46.6417584525214, :RX-B-1-Outlet-Temp 608.779513295492, :H2-Quench-RX-A-2-Inlet 3290.5318306817, :RX-A-2-Inlet-Top 600.002845128377, :A-2-Reactor-Bed-A 602.251323106554, :A-2-Reactor-Bed-B 601.455347866482, :A-2-Reactor-Bed-C 564.247447755603, :Reactor-A-2-Diff-Press 44.1513121922811, :Reactor-A-Inlet 863.539476945666, :Reactor-A-Outlet 603.730527072483, :RX-B-1-Inlet-Pressure 917.844505140516, :RX-Feed-From-Heater-B 593.839846886529, :RX-B-1-Top-Bed-Up-A 610.721715842354, :RX-B-1-Top-Bed-Up-B 610.553902774387, :RX-B-1-Top-Bed-Up-C 610.717639541626, :RX-B-1-Top-Bed-Low-D 620.255689875284, :RX-B-1-Top-Bed-Low-E 616.30127521091, :RX-B-1-Top-Bed-Low-F 617.534846496582, :B-1-Average-Top-Bed-Low-Temp 618.030603860926, :H2-Quench-To-MID-RX-B-1 8077.47633497451, :RX-B-1-Bot-Bed-Up-G 594.716981252034, :RX-B-1-Bot-Bed-Up-H 600.000953674316, :RX-B-1-Bot-Bed-Up-I 593.045855797663, :B-1-Average-Bottom-Bed-Up-Temp 595.921263574671, :RX-B-1-Bot-Bed-Mid-J 602.287260055543, :RX-B-1-Bot-Bed-Mid-K 599.314131969875, :RX-B-1-Bot-Bed-Mid-L 601.204281489053, :RX-B-1-Bot-Bed-Low-M 605.395085610285, :RX-B-1-Bot-Bed-Low-N 603.004189809162, :RX-B-1-Bot-Bed-Low-O 601.554199091593, :B-1-Average-Bottom-Bed-Low-Temp 603.317824837014, :RX-B-1-Overall-Delta-P 45.0443690829807, :H2-Quench-RX-B-2-Inlet 3230.72197960748, :RX-B-2-Inlet-Temp 599.997292624579, :B-2-Reactor-Bed-A 604.510950766669, :B-2-Reactor-Bed-B 601.457929441664, :B-2-Reactor-Bed-C 605.506826570298, :Reactor-B-2-Diff-Press 53.793916744656, :Reactor-B-2-Outlet 606.60969649421, :Recycle-Gas-H2 83.583, :Recycle-Gas-H2S 0.051, :Makeup-H2 99.8, :Gravity 41.7, :Initial-Boiling-Point-Test-Method-D2887 131.2, :Percent-5-Recovered-Test-Method-ASTM-D2887 0, :Percent-10-Recovered-Test-Method-ASTM-D2887 301.2, :Percent-20-Recovered-Test-Method-ASTM-D2887 364.5, :Percent-30-Recovered-Test-Method-ASTM-D2887 404.6, :Percent-40-Recovered-Test-Method-ASTM-D2887 439.5, :Percent-50-Recovered-Test-Method-ASTM-D2887 470.6, :Percent-60-Recovered-Test-Method-ASTM-D2887 500.2, :Percent-70-Recovered-Test-Method-ASTM-D2887 530.5, :Percent-80-Recovered-Test-Method-ASTM-D2887 566.4, :Percent-90-Recovered-Test-Method-ASTM-D2887 610.9, :Percent-95-Recovered-Test-Method-ASTM-D2887 0, :Final-Boiling-Point-Test-Method-D2887 649.8, :SULFUR-WT%-X-RA-Result 0.218, :Sulfur 3.4, :Net-Diesel-To-Storage 8660.28814663357, :Net-Kerosene-To-Storage 8013.01202629938, :Charge-N2 122, :Product-N2 0.31, :Kero-Sulfur 1.4, :Combined-Feed-Rate 26000.1919535319, :Coker-Debutanizer-Bottoms-Feed-Rate 911.534977637398, :Net-Coker-Diesel 3895.98116590712, :Prod-API-Gravity-Naphtha 57, :Purge-Rate 0, :ULSD-Prod-Gravity 0, :Sifeed 0.05, :Asfeed 0.02,:SOR-Ref-Day-Feed-Rate 30641.7590901692,:SOR-Ref-Day-Treat-Gas-Rate 3440616.87248018,:Feed-Exp 0.52,:Treat-Gas-Exp 0.84,:wt%-in-bed1 0.22,:wt%-in-bed2 0.32,:wt%-in-bed3 0.46,:dt-rule-in-bed1 0.77,:dt-rule-in-bed2 0.55,:dt-rule-in-bed3 0.54,:Est-oF-SCF-BBL 0.22,:eact  28500,:SOR-Ref-Day-k-Inh 1.12572708708572,:SOR-Ref-Day-A-Train-Topsoe-WABT 619.416018255731,:SOR-Ref-Day-K-sup 2.49242896416992,:SOR-Ref-Day-Kinh-Corr 1.42444265574578,:KH2S 1.1,:n 1.64,:Active-Catalyst-Volume 12385,:H2PP-Exp 1.2,:Eact-HDN 24000,:SOR-Ref-Day-H2-Partial-Pressure 861.118540931777,:SOR-Ref-Day-K-HDN 2.89725999014208,:dist-type "D86",:temp-type "F",}
           {:Recycle-H2-To-Pass-A 21887.807744005, :Recycle-H2-To-Pass-B 21888.8597045898, :Make-Up-Gas-From-Unicrack-H2 12782.9229695638, :RX-A-1-Inlet-Pressure 913.288882276747, :RX-A-1-Feed-From-Heater-A 591.06528447469, :RX-A-1-Top-Bed-Up-A 611.189068476359, :RX-A-1-Top-Bed-Up-B 612.238166215684, :RX-A-1-Top-Bed-Up-C 610.972743394639, :RX-A-1-Top-Bed-Low-D 620.890949842665, :RX-A-1-Top-Bed-Low-E 620.586556752523, :RX-A-1-Top-Bed-Low-F 619.803069008721, :A-1-Average-Top-Bed-Low-Temp 620.738753297594, :H2-Quench-To-MID-RX-A-1 8146.56568667942, :RX-A-1-Bot-Bed-Up-G 595.001244269477, :RX-A-1-Bot-Bed-Up-H 600.128783914777, :RX-A-1-Bot-Bed-Up-I 600.236108228896, :A-1-Average-Bottom-Bed-Up-Temp 598.455378804383, :RX-A-1-Bot-Bed-Mid-J 599.727761459351, :RX-A-1-Bot-Bed-Mid-K 598.96096534729, :RX-A-1-Bot-Bed-Mid-L 598.764834043715, :RX-A-1-Bot-Bed-Low-M 605.805415132309, :RX-A-1-Bot-Bed-Low-N 603.173959647284, :RX-A-1-Bot-Bed-Low-O 608.393228234186, :A-1-Average-Bottom-Bed-Low-Temp 605.79086767126, :RX-A-1-Overall-Delta-P 45.6479124704997, :RX-B-1-Outlet-Temp 609.911323208279, :H2-Quench-RX-A-2-Inlet 3650.41633269416, :RX-A-2-Inlet-Top 600.004033954055, :A-2-Reactor-Bed-A 601.800042682224, :A-2-Reactor-Bed-B 601.289785088433, :A-2-Reactor-Bed-C 591.406489011977, :Reactor-A-2-Diff-Press 44.3376737594605, :Reactor-A-Inlet 864.115045547485, :Reactor-A-Outlet 603.886607572768, :RX-B-1-Inlet-Pressure 917.147148429021, :RX-Feed-From-Heater-B 593.877651129828, :RX-B-1-Top-Bed-Up-A 612.684763463339, :RX-B-1-Top-Bed-Up-B 612.321923319499, :RX-B-1-Top-Bed-Up-C 612.864365768432, :RX-B-1-Top-Bed-Low-D 623.47630543179, :RX-B-1-Top-Bed-Low-E 619.736156802708, :RX-B-1-Top-Bed-Low-F 620.903410085042, :B-1-Average-Top-Bed-Low-Temp 621.371957439847, :H2-Quench-To-MID-RX-B-1 9132.8202605297, :RX-B-1-Bot-Bed-Up-G 592.698910268147, :RX-B-1-Bot-Bed-Up-H 600.119898054334, :RX-B-1-Bot-Bed-Up-I 590.856006071303, :B-1-Average-Bottom-Bed-Up-Temp 594.558271464595, :RX-B-1-Bot-Bed-Mid-J 602.062111663819, :RX-B-1-Bot-Bed-Mid-K 598.505505328709, :RX-B-1-Bot-Bed-Mid-L 600.975220574273, :RX-B-1-Bot-Bed-Low-M 604.134849251643, :RX-B-1-Bot-Bed-Low-N 601.646354124281, :RX-B-1-Bot-Bed-Low-O 600.089337539674, :B-1-Average-Bottom-Bed-Low-Temp 601.956846971866, :RX-B-1-Overall-Delta-P 43.9572763654921, :H2-Quench-RX-B-2-Inlet 3467.33871125681, :RX-B-2-Inlet-Temp 600.008215374417, :B-2-Reactor-Bed-A 603.963221020169, :B-2-Reactor-Bed-B 600.918534935844, :B-2-Reactor-Bed-C 604.72421353658, :Reactor-B-2-Diff-Press 53.9897221459283, :Reactor-B-2-Outlet 606.758389494154, :Recycle-Gas-H2 83.583, :Recycle-Gas-H2S 0.051, :Makeup-H2 99.8, :Gravity 41.7, :Initial-Boiling-Point-Test-Method-D2887 130.1, :Percent-5-Recovered-Test-Method-ASTM-D2887 0, :Percent-10-Recovered-Test-Method-ASTM-D2887 300.5, :Percent-20-Recovered-Test-Method-ASTM-D2887 365, :Percent-30-Recovered-Test-Method-ASTM-D2887 406.9, :Percent-40-Recovered-Test-Method-ASTM-D2887 442.1, :Percent-50-Recovered-Test-Method-ASTM-D2887 474.4, :Percent-60-Recovered-Test-Method-ASTM-D2887 504.5, :Percent-70-Recovered-Test-Method-ASTM-D2887 535.3, :Percent-80-Recovered-Test-Method-ASTM-D2887 571.8, :Percent-90-Recovered-Test-Method-ASTM-D2887 613.6, :Percent-95-Recovered-Test-Method-ASTM-D2887 0, :Final-Boiling-Point-Test-Method-D2887 651.5, :SULFUR-WT%-X-RA-Result 0.205, :Sulfur 3.8, :Net-Diesel-To-Storage 9103.64604536269, :Net-Kerosene-To-Storage 7549.03331324825, :Charge-N2 122, :Product-N2 0.31, :Kero-Sulfur 1.6, :Combined-Feed-Rate 26001.0888586539, :Coker-Debutanizer-Bottoms-Feed-Rate 792.848056676653, :Net-Coker-Diesel 3611.83463482326, :Prod-API-Gravity-Naphtha 57, :Purge-Rate 0, :ULSD-Prod-Gravity 0, :Sifeed 0.05, :Asfeed 0.02,:SOR-Ref-Day-Feed-Rate 30641.7590901692,:SOR-Ref-Day-Treat-Gas-Rate 3440616.87248018,:Feed-Exp 0.52,:Treat-Gas-Exp 0.84,:wt%-in-bed1 0.22,:wt%-in-bed2 0.32,:wt%-in-bed3 0.46,:dt-rule-in-bed1 0.77,:dt-rule-in-bed2 0.55,:dt-rule-in-bed3 0.54,:Est-oF-SCF-BBL 0.22,:eact  28500,:SOR-Ref-Day-k-Inh 1.12572708708572,:SOR-Ref-Day-A-Train-Topsoe-WABT 619.416018255731,:SOR-Ref-Day-K-sup 2.49242896416992,:SOR-Ref-Day-Kinh-Corr 1.42444265574578,:KH2S 1.1,:n 1.64,:Active-Catalyst-Volume 12385,:H2PP-Exp 1.2,:Eact-HDN 24000,:SOR-Ref-Day-H2-Partial-Pressure 861.118540931777,:SOR-Ref-Day-K-HDN 2.89725999014208,:dist-type "D86",:temp-type "F",}
           {:Recycle-H2-To-Pass-A 20834.2136352539, :Recycle-H2-To-Pass-B 20834.9066975912, :Make-Up-Gas-From-Unicrack-H2 13005.327549235, :RX-A-1-Inlet-Pressure 911.892385016548, :RX-A-1-Feed-From-Heater-A 589.162049399481, :RX-A-1-Top-Bed-Up-A 619.683113733928, :RX-A-1-Top-Bed-Up-B 621.212361399333, :RX-A-1-Top-Bed-Up-C 619.181997977363, :RX-A-1-Top-Bed-Low-D 628.930767186483, :RX-A-1-Top-Bed-Low-E 629.177432462904, :RX-A-1-Top-Bed-Low-F 627.60375726488, :A-1-Average-Top-Bed-Low-Temp 629.054099824693, :H2-Quench-To-MID-RX-A-1 10354.5765974257, :RX-A-1-Bot-Bed-Up-G 596.490072080825, :RX-A-1-Bot-Bed-Up-H 598.701994860613, :RX-A-1-Bot-Bed-Up-I 602.624132622612, :A-1-Average-Bottom-Bed-Up-Temp 599.27206652135, :RX-A-1-Bot-Bed-Mid-J 600.193368233575, :RX-A-1-Bot-Bed-Mid-K 599.508693186442, :RX-A-1-Bot-Bed-Mid-L 599.20919418335, :RX-A-1-Bot-Bed-Low-M 607.010194736058, :RX-A-1-Bot-Bed-Low-N 603.822937562731, :RX-A-1-Bot-Bed-Low-O 610.456900066799, :A-1-Average-Bottom-Bed-Low-Temp 607.096677455196, :RX-A-1-Overall-Delta-P 44.9649069044325, :RX-B-1-Outlet-Temp 606.612288623386, :H2-Quench-RX-A-2-Inlet 3123.75312063712, :RX-A-2-Inlet-Top 598.578923719901, :A-2-Reactor-Bed-A 604.217712487115, :A-2-Reactor-Bed-B 603.383334604899, :A-2-Reactor-Bed-C 593.880616378784, :Reactor-A-2-Diff-Press 43.6887272516886, :Reactor-A-Inlet 863.385543272231, :Reactor-A-Outlet 602.900247489082, :RX-B-1-Inlet-Pressure 915.551251475014, :RX-Feed-From-Heater-B 592.105521647135, :RX-B-1-Top-Bed-Up-A 620.145523071289, :RX-B-1-Top-Bed-Up-B 619.674890094334, :RX-B-1-Top-Bed-Up-C 620.154268137614, :RX-B-1-Top-Bed-Low-D 630.383794445462, :RX-B-1-Top-Bed-Low-E 627.108240678575, :RX-B-1-Top-Bed-Low-F 627.734523646037, :B-1-Average-Top-Bed-Low-Temp 628.408852923358, :H2-Quench-To-MID-RX-B-1 10948.1384297237, :RX-B-1-Bot-Bed-Up-G 594.920486492581, :RX-B-1-Bot-Bed-Up-H 598.706981870863, :RX-B-1-Bot-Bed-Up-I 593.194045554267, :B-1-Average-Bottom-Bed-Up-Temp 595.607171305904, :RX-B-1-Bot-Bed-Mid-J 603.352368503147, :RX-B-1-Bot-Bed-Mid-K 599.2965101454, :RX-B-1-Bot-Bed-Mid-L 602.238925170899, :RX-B-1-Bot-Bed-Low-M 605.613528993395, :RX-B-1-Bot-Bed-Low-N 603.668652555678, :RX-B-1-Bot-Bed-Low-O 600.798905097114, :B-1-Average-Bottom-Bed-Low-Temp 603.360362215396, :RX-B-1-Overall-Delta-P 42.8948904037476, :H2-Quench-RX-B-2-Inlet 3387.33525429055, :RX-B-2-Inlet-Temp 598.564688894484, :B-2-Reactor-Bed-A 605.57968466017, :B-2-Reactor-Bed-B 602.80954653422, :B-2-Reactor-Bed-C 607.064170159233, :Reactor-B-2-Diff-Press 53.3379759894477, :Reactor-B-2-Outlet 605.786575783623, :Recycle-Gas-H2 83.583, :Recycle-Gas-H2S 0.051, :Makeup-H2 99.8, :Gravity 41.8, :Initial-Boiling-Point-Test-Method-D2887 129.3, :Percent-5-Recovered-Test-Method-ASTM-D2887 0, :Percent-10-Recovered-Test-Method-ASTM-D2887 299.5, :Percent-20-Recovered-Test-Method-ASTM-D2887 363.6, :Percent-30-Recovered-Test-Method-ASTM-D2887 406.9, :Percent-40-Recovered-Test-Method-ASTM-D2887 442.7, :Percent-50-Recovered-Test-Method-ASTM-D2887 475.8, :Percent-60-Recovered-Test-Method-ASTM-D2887 506, :Percent-70-Recovered-Test-Method-ASTM-D2887 538.4, :Percent-80-Recovered-Test-Method-ASTM-D2887 573.7, :Percent-90-Recovered-Test-Method-ASTM-D2887 617.3, :Percent-95-Recovered-Test-Method-ASTM-D2887 0, :Final-Boiling-Point-Test-Method-D2887 654, :SULFUR-WT%-X-RA-Result 0.203, :Sulfur 5.1, :Net-Diesel-To-Storage 9614.44970106337, :Net-Kerosene-To-Storage 7541.56817691378, :Charge-N2 122, :Product-N2 0.31, :Kero-Sulfur 1.4, :Combined-Feed-Rate 26000.3068093081, :Coker-Debutanizer-Bottoms-Feed-Rate 611.621238613129, :Net-Coker-Diesel 3331.99218796624, :Prod-API-Gravity-Naphtha 57, :Purge-Rate 0, :ULSD-Prod-Gravity 0, :Sifeed 0.05, :Asfeed 0.02,:SOR-Ref-Day-Feed-Rate 30641.7590901692,:SOR-Ref-Day-Treat-Gas-Rate 3440616.87248018,:Feed-Exp 0.52,:Treat-Gas-Exp 0.84,:wt%-in-bed1 0.22,:wt%-in-bed2 0.32,:wt%-in-bed3 0.46,:dt-rule-in-bed1 0.77,:dt-rule-in-bed2 0.55,:dt-rule-in-bed3 0.54,:Est-oF-SCF-BBL 0.22,:eact  28500,:SOR-Ref-Day-k-Inh 1.12572708708572,:SOR-Ref-Day-A-Train-Topsoe-WABT 619.416018255731,:SOR-Ref-Day-K-sup 2.49242896416992,:SOR-Ref-Day-Kinh-Corr 1.42444265574578,:KH2S 1.1,:n 1.64,:Active-Catalyst-Volume 12385,:H2PP-Exp 1.2,:Eact-HDN 24000,:SOR-Ref-Day-H2-Partial-Pressure 861.118540931777,:SOR-Ref-Day-K-HDN 2.89725999014208,:dist-type "D86",:temp-type "F",}
           {:Recycle-H2-To-Pass-A 21366.9168928132, :Recycle-H2-To-Pass-B 21365.4119778103, :Make-Up-Gas-From-Unicrack-H2 12509.9937188043, :RX-A-1-Inlet-Pressure 911.254272376166, :RX-A-1-Feed-From-Heater-A 581.870965957641, :RX-A-1-Top-Bed-Up-A 610.650277286106, :RX-A-1-Top-Bed-Up-B 611.977951049805, :RX-A-1-Top-Bed-Up-C 610.216810438368, :RX-A-1-Top-Bed-Low-D 619.815023040772, :RX-A-1-Top-Bed-Low-E 620.126246303982, :RX-A-1-Top-Bed-Low-F 618.397109561497, :A-1-Average-Top-Bed-Low-Temp 619.970634672377, :H2-Quench-To-MID-RX-A-1 8520.35973867911, :RX-A-1-Bot-Bed-Up-G 593.529673343235, :RX-A-1-Bot-Bed-Up-H 594.151993415972, :RX-A-1-Bot-Bed-Up-I 599.239984173244, :A-1-Average-Bottom-Bed-Up-Temp 595.640550310817, :RX-A-1-Bot-Bed-Mid-J 597.080549494425, :RX-A-1-Bot-Bed-Mid-K 596.023236211141, :RX-A-1-Bot-Bed-Mid-L 596.008093049791, :RX-A-1-Bot-Bed-Low-M 603.809662585788, :RX-A-1-Bot-Bed-Low-N 600.146693123711, :RX-A-1-Bot-Bed-Low-O 607.227136145697, :A-1-Average-Bottom-Bed-Low-Temp 603.727830618399, :RX-A-1-Overall-Delta-P 44.5008124881321, :RX-B-1-Outlet-Temp 602.160087627834, :H2-Quench-RX-A-2-Inlet 3808.19666492039, :RX-A-2-Inlet-Top 592.071395979987, :A-2-Reactor-Bed-A 599.022738732231, :A-2-Reactor-Bed-B 598.176150512696, :A-2-Reactor-Bed-C 580.794162919787, :Reactor-A-2-Diff-Press 42.9784182018704, :Reactor-A-Inlet 863.230109447905, :Reactor-A-Outlet 596.148044840495, :RX-B-1-Inlet-Pressure 915.816050804984, :RX-Feed-From-Heater-B 585.405720138549, :RX-B-1-Top-Bed-Up-A 612.172369935778, :RX-B-1-Top-Bed-Up-B 611.551239988539, :RX-B-1-Top-Bed-Up-C 612.216054407755, :RX-B-1-Top-Bed-Low-D 621.684125306871, :RX-B-1-Top-Bed-Low-E 619.069430287679, :RX-B-1-Top-Bed-Low-F 619.466851933798, :B-1-Average-Top-Bed-Low-Temp 620.073469176116, :H2-Quench-To-MID-RX-B-1 9599.18282370108, :RX-B-1-Bot-Bed-Up-G 591.57180680169, :RX-B-1-Bot-Bed-Up-H 594.142023722331, :RX-B-1-Bot-Bed-Up-I 589.901737340292, :B-1-Average-Bottom-Bed-Up-Temp 591.871855954771, :RX-B-1-Bot-Bed-Mid-J 599.624533292983, :RX-B-1-Bot-Bed-Mid-K 595.959001159669, :RX-B-1-Bot-Bed-Mid-L 598.832399792141, :RX-B-1-Bot-Bed-Low-M 601.667406802707, :RX-B-1-Bot-Bed-Low-N 599.439915974936, :RX-B-1-Bot-Bed-Low-O 597.443818240696, :B-1-Average-Bottom-Bed-Low-Temp 599.517047006113, :RX-B-1-Overall-Delta-P 42.8361255009969, :H2-Quench-RX-B-2-Inlet 3773.06087847675, :RX-B-2-Inlet-Temp 592.090124112589, :B-2-Reactor-Bed-A 600.485599390666, :B-2-Reactor-Bed-B 597.674581697253, :B-2-Reactor-Bed-C 601.4136141883, :Reactor-B-2-Diff-Press 52.942720328437, :Reactor-B-2-Outlet 599.019111209445, :Recycle-Gas-H2 83.583, :Recycle-Gas-H2S 0.051, :Makeup-H2 99.8, :Gravity 41.7, :Initial-Boiling-Point-Test-Method-D2887 128.7, :Percent-5-Recovered-Test-Method-ASTM-D2887 0, :Percent-10-Recovered-Test-Method-ASTM-D2887 296.6, :Percent-20-Recovered-Test-Method-ASTM-D2887 361.5, :Percent-30-Recovered-Test-Method-ASTM-D2887 407.1, :Percent-40-Recovered-Test-Method-ASTM-D2887 443.7, :Percent-50-Recovered-Test-Method-ASTM-D2887 478, :Percent-60-Recovered-Test-Method-ASTM-D2887 508.9, :Percent-70-Recovered-Test-Method-ASTM-D2887 543.3, :Percent-80-Recovered-Test-Method-ASTM-D2887 575.9, :Percent-90-Recovered-Test-Method-ASTM-D2887 623.1, :Percent-95-Recovered-Test-Method-ASTM-D2887 0, :Final-Boiling-Point-Test-Method-D2887 658.2, :SULFUR-WT%-X-RA-Result 0.219, :Sulfur 4.3, :Net-Diesel-To-Storage 9776.48653091996, :Net-Kerosene-To-Storage 8296.56856105098, :Charge-N2 122, :Product-N2 0.31, :Kero-Sulfur 2.7, :Combined-Feed-Rate 26000.4471679461, :Coker-Debutanizer-Bottoms-Feed-Rate 608.01703183916, :Net-Coker-Diesel 3214.04086447822, :Prod-API-Gravity-Naphtha 57, :Purge-Rate 0, :ULSD-Prod-Gravity 0, :Sifeed 0.05, :Asfeed 0.02,:SOR-Ref-Day-Feed-Rate 30641.7590901692,:SOR-Ref-Day-Treat-Gas-Rate 3440616.87248018,:Feed-Exp 0.52,:Treat-Gas-Exp 0.84,:wt%-in-bed1 0.22,:wt%-in-bed2 0.32,:wt%-in-bed3 0.46,:dt-rule-in-bed1 0.77,:dt-rule-in-bed2 0.55,:dt-rule-in-bed3 0.54,:Est-oF-SCF-BBL 0.22,:eact  28500,:SOR-Ref-Day-k-Inh 1.12572708708572,:SOR-Ref-Day-A-Train-Topsoe-WABT 619.416018255731,:SOR-Ref-Day-K-sup 2.49242896416992,:SOR-Ref-Day-Kinh-Corr 1.42444265574578,:KH2S 1.1,:n 1.64,:Active-Catalyst-Volume 12385,:H2PP-Exp 1.2,:Eact-HDN 24000,:SOR-Ref-Day-H2-Partial-Pressure 861.118540931777,:SOR-Ref-Day-K-HDN 2.89725999014208,:dist-type "D86",:temp-type "F",}
           {:Recycle-H2-To-Pass-A 21140.737616645, :Recycle-H2-To-Pass-B 21141.0682888455, :Make-Up-Gas-From-Unicrack-H2 12679.6667175293, :RX-A-1-Inlet-Pressure 912.59612358941, :RX-A-1-Feed-From-Heater-A 580.666676372952, :RX-A-1-Top-Bed-Up-A 609.392192120022, :RX-A-1-Top-Bed-Up-B 610.740105438232, :RX-A-1-Top-Bed-Up-C 608.927882936265, :RX-A-1-Top-Bed-Low-D 618.821642388237, :RX-A-1-Top-Bed-Low-E 619.343699264526, :RX-A-1-Top-Bed-Low-F 617.277086724176, :A-1-Average-Top-Bed-Low-Temp 619.082670826382, :H2-Quench-To-MID-RX-A-1 9272.52269354926, :RX-A-1-Bot-Bed-Up-G 589.520663579305, :RX-A-1-Bot-Bed-Up-H 591.975159115261, :RX-A-1-Bot-Bed-Up-I 595.372022840711, :A-1-Average-Bottom-Bed-Up-Temp 592.289281845092, :RX-A-1-Bot-Bed-Mid-J 592.676859580146, :RX-A-1-Bot-Bed-Mid-K 591.71846224467, :RX-A-1-Bot-Bed-Mid-L 591.578953382705, :RX-A-1-Bot-Bed-Low-M 599.108795717027, :RX-A-1-Bot-Bed-Low-N 595.215864351061, :RX-A-1-Bot-Bed-Low-O 602.032922320896, :A-1-Average-Bottom-Bed-Low-Temp 598.785860796328, :RX-A-1-Overall-Delta-P 44.8888097551134, :RX-B-1-Outlet-Temp 599.403813129002, :H2-Quench-RX-A-2-Inlet 3556.96161532933, :RX-A-2-Inlet-Top 590.002114189995, :A-2-Reactor-Bed-A 593.862730789184, :A-2-Reactor-Bed-B 593.147030131021, :A-2-Reactor-Bed-C 578.038410101997, :Reactor-A-2-Diff-Press 43.8081694920858, :Reactor-A-Inlet 864.162581592136, :Reactor-A-Outlet 593.831745698716, :RX-B-1-Inlet-Pressure 916.278922398885, :RX-Feed-From-Heater-B 583.422714953952, :RX-B-1-Top-Bed-Up-A 610.097171317207, :RX-B-1-Top-Bed-Up-B 609.567359203762, :RX-B-1-Top-Bed-Up-C 610.169836001926, :RX-B-1-Top-Bed-Low-D 619.295673327977, :RX-B-1-Top-Bed-Low-E 617.366099421183, :RX-B-1-Top-Bed-Low-F 617.240514077081, :B-1-Average-Top-Bed-Low-Temp 617.96742894208, :H2-Quench-To-MID-RX-B-1 10029.5221320258, :RX-B-1-Bot-Bed-Up-G 587.387664752536, :RX-B-1-Bot-Bed-Up-H 591.994068357681, :RX-B-1-Bot-Bed-Up-I 585.647660742865, :B-1-Average-Bottom-Bed-Up-Temp 588.343131284361, :RX-B-1-Bot-Bed-Mid-J 595.093044704861, :RX-B-1-Bot-Bed-Mid-K 591.327036836412, :RX-B-1-Bot-Bed-Mid-L 594.155051634047, :RX-B-1-Bot-Bed-Low-M 597.146626493666, :RX-B-1-Bot-Bed-Low-N 595.030528471205, :RX-B-1-Bot-Bed-Low-O 592.564973661634, :B-1-Average-Bottom-Bed-Low-Temp 594.914042875501, :RX-B-1-Overall-Delta-P 42.6695597330729, :H2-Quench-RX-B-2-Inlet 3591.68817833795, :RX-B-2-Inlet-Temp 589.995225270588, :B-2-Reactor-Bed-A 595.526544952392, :B-2-Reactor-Bed-B 592.967658106485, :B-2-Reactor-Bed-C 597.075613191393, :Reactor-B-2-Diff-Press 53.3958230336507, :Reactor-B-2-Outlet 596.822798919678, :Recycle-Gas-H2 83.583, :Recycle-Gas-H2S 0.051, :Makeup-H2 99.8, :Gravity 41.7, :Initial-Boiling-Point-Test-Method-D2887 128.7, :Percent-5-Recovered-Test-Method-ASTM-D2887 0, :Percent-10-Recovered-Test-Method-ASTM-D2887 296.6, :Percent-20-Recovered-Test-Method-ASTM-D2887 361.5, :Percent-30-Recovered-Test-Method-ASTM-D2887 407.1, :Percent-40-Recovered-Test-Method-ASTM-D2887 443.7, :Percent-50-Recovered-Test-Method-ASTM-D2887 478, :Percent-60-Recovered-Test-Method-ASTM-D2887 508.9, :Percent-70-Recovered-Test-Method-ASTM-D2887 543.3, :Percent-80-Recovered-Test-Method-ASTM-D2887 575.9, :Percent-90-Recovered-Test-Method-ASTM-D2887 623.1, :Percent-95-Recovered-Test-Method-ASTM-D2887 0, :Final-Boiling-Point-Test-Method-D2887 658.2, :SULFUR-WT%-X-RA-Result 0.219, :Sulfur 6.2, :Net-Diesel-To-Storage 9950.70492418077, :Net-Kerosene-To-Storage 8391.35799628366, :Charge-N2 122, :Product-N2 0.31, :Kero-Sulfur 1.6, :Combined-Feed-Rate 26000.4235629612, :Coker-Debutanizer-Bottoms-Feed-Rate 636.275042661031, :Net-Coker-Diesel 2976.91132922702, :Prod-API-Gravity-Naphtha 57, :Purge-Rate 0, :ULSD-Prod-Gravity 0, :Sifeed 0.05, :Asfeed 0.02,:SOR-Ref-Day-Feed-Rate 30641.7590901692,:SOR-Ref-Day-Treat-Gas-Rate 3440616.87248018,:Feed-Exp 0.52,:Treat-Gas-Exp 0.84,:wt%-in-bed1 0.22,:wt%-in-bed2 0.32,:wt%-in-bed3 0.46,:dt-rule-in-bed1 0.77,:dt-rule-in-bed2 0.55,:dt-rule-in-bed3 0.54,:Est-oF-SCF-BBL 0.22,:eact  28500,:SOR-Ref-Day-k-Inh 1.12572708708572,:SOR-Ref-Day-A-Train-Topsoe-WABT 619.416018255731,:SOR-Ref-Day-K-sup 2.49242896416992,:SOR-Ref-Day-Kinh-Corr 1.42444265574578,:KH2S 1.1,:n 1.64,:Active-Catalyst-Volume 12385,:H2PP-Exp 1.2,:Eact-HDN 24000,:SOR-Ref-Day-H2-Partial-Pressure 861.118540931777,:SOR-Ref-Day-K-HDN 2.89725999014208,:dist-type "D86",:temp-type "F",}
           ])

(def test-naphtha-nht-data [{:Feed-Nitrogen 1.3 , :Product-Aromatics 0 , :IBP 81.2 ,   :FBP 382.7 , :Norm-WABT 600.429742595215 , :Reactor-Delta-P 9.96761322021484 , :API 67.5 , :Reactor-1-Inlet 608.684265136718 , :Reactor-Outlet-TI 611.16064453125 , :Recycle-Hydrogen-Gas-Rate 8371.201171875 , :Recycle-Gas-Hydrogen 95.467 , :Makeup-Hydrogen-Gas-Rate 2238.5302734375 , :Makeup-Gas-Hydrogen 99.8 , :Combined 22980.6484375 , :90-per 317.4 , :10-per 129.8 , :30-per 168.8 , :50-per 205.3 , :70-per 0 , :Reactor-Inlet 700.092124938964 , :Reactor-Delta-T 2.47637939453125}
                            {:Feed-Nitrogen 1.3 , :Product-Aromatics 0 , :IBP 81.2 ,   :FBP 382.7 , :Norm-WABT 600.360224376319 , :Reactor-Delta-P 9.90581512451171 , :API 67.5 , :Reactor-1-Inlet 609.531433105468 , :Reactor-Outlet-TI 610.769653320312 , :Recycle-Hydrogen-Gas-Rate 8445.4052734375 , :Recycle-Gas-Hydrogen 95.467 , :Makeup-Hydrogen-Gas-Rate 2233.18774414062 , :Makeup-Gas-Hydrogen 99.8 , :Combined 23001.734375 , :90-per 317.4 , :10-per 129.8 , :30-per 168.8 , :50-per 205.3 , :70-per 0 , :Reactor-Inlet 699.862480163574 , :Reactor-Delta-T 1.23822021484375}
                            {:Feed-Nitrogen 1.3 , :Product-Aromatics 0 , :IBP 81.2 , :FBP 382.7 , :Norm-WABT 600.259492459749 , :Reactor-Delta-P 10.087776184082 , :API 67.5 , :Reactor-1-Inlet 609.010131835937 , :Reactor-Outlet-TI 610.834838867187 , :Recycle-Hydrogen-Gas-Rate 8191.6025390625 , :Recycle-Gas-Hydrogen 95.467 , :Makeup-Hydrogen-Gas-Rate 2294.7216796875 , :Makeup-Gas-Hydrogen 99.8 , :Combined 22985.509765625 , :90-per 317.4 , :10-per 129.8 , :30-per 168.8 , :50-per 205.3 , :70-per 0 , :Reactor-Inlet 699.479866027832 , :Reactor-Delta-T 1.82470703125}
                            {:Feed-Nitrogen 1.3 , :Product-Aromatics 0 , :IBP 81.2 ,  :FBP 382.7 , :Norm-WABT 599.764045931819 , :Reactor-Delta-P 10.0128173828125 , :API 67.5 , :Reactor-1-Inlet 608.293334960937 , :Reactor-Outlet-TI 610.117919921875 , :Recycle-Hydrogen-Gas-Rate 8064.19189453125 , :Recycle-Gas-Hydrogen 95.467 , :Makeup-Hydrogen-Gas-Rate 2256.83740234375 , :Makeup-Gas-Hydrogen 99.8 , :Combined 22931.16015625 , :90-per 317.4 , :10-per 129.8 , :30-per 168.8 , :50-per 205.3 , :70-per 0 , :Reactor-Inlet 699.740600585937 , :Reactor-Delta-T 1.8245849609375}
                            {:Feed-Nitrogen 2.4 , :Product-Aromatics 0 , :IBP 81.7 ,  :FBP 378.3 , :Norm-WABT 600.879419390221 , :Reactor-Delta-P 10.1356506347656 , :API 68 , :Reactor-1-Inlet 609.857299804687 , :Reactor-Outlet-TI 611.16064453125 , :Recycle-Hydrogen-Gas-Rate 8097.62451171875 , :Recycle-Gas-Hydrogen 95.535 , :Makeup-Hydrogen-Gas-Rate 2250.93432617187 , :Makeup-Gas-Hydrogen 99.8 , :Combined 23102.724609375 , :90-per 323.6 , :10-per 129.4 , :30-per 168.7 , :50-per 206.3 , :70-per 0 , :Reactor-Inlet 700.53482055664 , :Reactor-Delta-T 1.3033447265625}
                            ])

(defonce test-data (atom {}))

(defn test-diesel-hdt-calculations [data]
  (map-indexed (fn [idx dm]
                 (if (= idx 0)
                   (do
                     (swap! test-data assoc :Normalized-HDS-WABT-Column-MI [])
                     (swap! test-data assoc :Normalized-HDB-WABT-Column-MS [])
                     (swap! test-data assoc :Feed-Sulfur-Column-CZ [])
                     (swap! test-data assoc :Product-Sulfur-Column-JT [])
                     (swap! test-data assoc :Feed-Nitrogen-Column-DA [])
                     (swap! test-data assoc :Product-Nitrogen-Column-JU [])
                     (swap! test-data assoc :Feed-Gravity-Column-CX [])
                     ;(swap! test-data assoc :Feed-Silicon-Column-DC [])
                     ;(swap! test-data assoc :Feed-Arsenic-Column-DD [])
                     (swap! test-data assoc :10-Per-Trendline-Column-EJ [])
                     (swap! test-data assoc :30-Per-Trendline-Column-EL [])
                     (swap! test-data assoc :50-Per-TrendlineColumn-EN [])
                     (swap! test-data assoc :70-Per-Trendline-Column-EP [])
                     (swap! test-data assoc :90-Per-Trendline-Column-ER [])
                     (swap! test-data assoc :100-Per-Trendline-Column-ET [])
                     (swap! test-data assoc :Feed-600ºF-Column-EW [])
                     (swap! test-data assoc :Feed-Rate-Column-C [])
                     (swap! test-data assoc :Feed-Ratio-Of-Cracked-Feed-Column-H [])
                     (swap! test-data assoc :Observed-WABT-Column-LJ [])
                     (swap! test-data assoc :Rate-Column-K [])
                     (swap! test-data assoc :Hydrogen-Purity-Column-L [])
                     (swap! test-data assoc :Rate-Column-M [])
                     (swap! test-data assoc :Hydrogen-Purity-Column-N [])
                     (swap! test-data assoc :Rate-Column-W [])
                     (swap! test-data assoc :Hydrogen-Purity-Column-P [])
                     (swap! test-data assoc :Rate-Column-AA [])
                     (swap! test-data assoc :Hydrogen-Purity-Column-AB [])
                     (swap! test-data assoc :Total-gas/oil-ratio-Column-LF [])
                     (swap! test-data assoc :Hydrogen/oil-ratio-Column-LG [])
                     (swap! test-data assoc :Train-A-Column-KU [])
                     (swap! test-data assoc :Train-B-Column-KV [])
                     (swap! test-data assoc :Hydrogen-Availability-Column-KZ [])
                     (swap! test-data assoc :Train-A-Inlet-Pressure-Column-AD [])
                     (swap! test-data assoc :Train-B-inlet-Pressure-Column-AH [])
                     (swap! test-data assoc
                            :Train-A-Hydrogen-Partial-Pressure-Column-LA [])
                     (swap! test-data assoc
                            :Train-B-Hydrogen-Partial-Pressure-Column-LB [])
                     (swap! test-data assoc :Total-Pressure-Drop-Column-AG [])
                     (swap! test-data assoc :Normalized-Pressure-Drop-Column-LC [])
                     (swap! test-data assoc :Total-Pressure-Drop-Column-AK [])
                     (swap! test-data assoc :Normalized-Pressure-Drop-Column-LD [])
                     (swap! test-data assoc :Train-A-DT-Column-AY [])
                     (swap! test-data assoc :Train-B-DT-Column-BM [])
                     (swap! test-data assoc :A1-Bed-1-DT-Column-AO [])
                     (swap! test-data assoc :A1-Bed-2-DT-Column-AS [])
                     (swap! test-data assoc :A2-DT-Column-AX [])
                     (swap! test-data assoc :B1-Bed-1-DT-Column-BC [])
                     (swap! test-data assoc :B1-Bed-2-DT-Column-BG [])
                     (swap! test-data assoc :B2-DT-Column-BL [])
                     (swap! test-data assoc :A1-Bed-1-inlet-T-Column-AL [])
                     (swap! test-data assoc :A1-Bed-1-outlet-T-Column-AN [])
                     (swap! test-data assoc :A1-Bed-2-outlet-T-Column-AR [])
                     (swap! test-data assoc :A2-outlet-T-Column-AW [])
                     (swap! test-data assoc :B1-Bed-1-inlet-T-Column-AZ [])
                     (swap! test-data assoc :B1-Bed-1-outlet-T-Column-BB [])
                     (swap! test-data assoc :B1-Bed-2-outlet-T-Column-BF [])
                     (swap! test-data assoc :B2-outlet-T-Column-BK [])
                     (swap! test-data assoc :Top-radial-spread-Column-BZ [])
                     (swap! test-data assoc :Bottom-radial-spread-Column-CA [])
                     (swap! test-data assoc :Top-radial-spread-Column-CB [])
                     (swap! test-data assoc :Middle-radial-spread-Column-CC [])
                     (swap! test-data assoc :Bottom-radial-spread-Column-CD [])
                     (swap! test-data assoc :Radial-spread-Column-CE [])
                     (swap! test-data assoc :Top-radial-spread-Column-CF [])
                     (swap! test-data assoc :Bottom-radial-spread-Column-CG [])
                     (swap! test-data assoc :Top-radial-spread-Column-CH [])
                     (swap! test-data assoc :Middle-radial-spread-Column-CI [])
                     (swap! test-data assoc :Bottom-radial-spread-Column-CJ [])
                     (swap! test-data assoc :Radial-spread-Column-CK [])))

                 ;(c/NHDSWABT)
                 ;(c/NHDNWABT)
                 ;Feed Sulfur Column CZ
                 (swap! test-data assoc :Feed-Sulfur-Column-CZ
                        (-> (get
                              @test-data :Feed-Sulfur-Column-CZ)
                            (conj
                              (c/Feed-Sulfur
                                (dm :SULFUR-WT%-X-RA-Result)))
                            ))

                 ;Product Sulfur Column JT
                 (swap! test-data assoc :Product-Sulfur-Column-JT
                        (-> (get
                              @test-data :Product-Sulfur-Column-JT)
                            (conj
                              (c/Product-Sulfur
                                (dm :Net-Diesel-To-Storage) (dm :ULSD-Prod-Gravity)
                                (dm :Sulfur) (dm :Net-Kerosene-To-Storage)
                                (dm :Prod-API-Gravity-Naphtha) (dm :Kero-Sulfur)))))

                 ;Feed Nitrogen Column DA
                 (swap! test-data assoc :Feed-Nitrogen-Column-DA
                        (-> (get
                              @test-data :Feed-Nitrogen-Column-DA)
                            (conj
                              (c/Feed-Nitrogen
                                (dm :Charge-N2)))))


                 ;Product Nitrogen Column JU
                 (swap! test-data assoc :Product-Nitrogen-Column-JU
                        (-> (get
                              @test-data :Product-Nitrogen-Column-JU)
                            (conj
                              (c/Product-Nitrogen
                                (dm :Product-N2)))))

                 ;Feed Gravity Column CX
                 (swap! test-data assoc :Feed-Gravity-Column-CX
                        (-> (get
                              @test-data :Feed-Gravity-Column-CX)
                            (conj
                              (c/Feed-Gravity
                                (dm :Gravity)))))

                 ;;Feed Silicon Column DC
                 ;(swap! test-data assoc :Feed-Silicon-Column-DC
                 ;       (-> (get
                 ;             @test-data :Feed-Silicon-Column-DC)
                 ;           (conj
                 ;             (c/Feed-Silicon
                 ;               (dm :Sifeed)))))
                 ;
                 ;;Feed Arsenic Column DD
                 ;(swap! test-data assoc :Feed-Arsenic-Column-DD
                 ;       (-> (get
                 ;             @test-data :Feed-Arsenic-Column-DD)
                 ;           (conj
                 ;             (c/Feed-Arsenic
                 ;               (dm :Asfeed)))))


                 ;;Feed Arsenic Column DD
                 ;(swap! test-data assoc :Feed-Arsenic-Column-DD
                 ;       (-> (get
                 ;             @test-data :Feed-Arsenic-Column-DD)
                 ;           (conj
                 ;             (c/Feed-Arsenic
                 ;               (dm :Asfeed)))))


                 ;10 Per Trendline Column EJ
                 (swap! test-data assoc :10-Per-Trendline-Column-EJ
                        (-> (get
                              @test-data :10-Per-Trendline-Column-EJ)
                            (conj
                              (c/Ten-Per-Trendline
                                (dm :dist-type)
                                (dm :temp-type)
                                (dm :Initial-Boiling-Point-Test-Method-D2887)
                                (dm :Percent-5-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-10-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-20-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-30-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-40-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-50-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-60-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-70-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-80-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-90-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-95-Recovered-Test-Method-ASTM-D2887)
                                (dm :Final-Boiling-Point-Test-Method-D2887)
                                ))))



                 ;30 Per Trendline Column EL
                 (swap! test-data assoc :30-Per-Trendline-Column-EL
                        (-> (get
                              @test-data :30-Per-Trendline-Column-EL)
                            (conj
                              (c/Thirty-Per-Trendline
                                (dm :dist-type)
                                (dm :temp-type)
                                (dm :Initial-Boiling-Point-Test-Method-D2887)
                                (dm :Percent-5-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-10-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-20-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-30-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-40-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-50-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-60-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-70-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-80-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-90-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-95-Recovered-Test-Method-ASTM-D2887)
                                (dm :Final-Boiling-Point-Test-Method-D2887)
                                ))))


                 ;50 Per TrendlineColumn EN
                 (swap! test-data assoc :50-Per-TrendlineColumn-EN
                        (-> (get
                              @test-data :50-Per-TrendlineColumn-EN)
                            (conj
                              (c/Fifty-Per-Trendline
                                (dm :dist-type)
                                (dm :temp-type)
                                (dm :Initial-Boiling-Point-Test-Method-D2887)
                                (dm :Percent-5-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-10-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-20-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-30-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-40-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-50-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-60-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-70-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-80-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-90-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-95-Recovered-Test-Method-ASTM-D2887)
                                (dm :Final-Boiling-Point-Test-Method-D2887)
                                ))))

                 ;70 Per Trendline Column EP
                 (swap! test-data assoc :70-Per-Trendline-Column-EP
                        (-> (get
                              @test-data :70-Per-Trendline-Column-EP)
                            (conj
                              (c/Seventy-Per-Trendline
                                (dm :dist-type)
                                (dm :temp-type)
                                (dm :Initial-Boiling-Point-Test-Method-D2887)
                                (dm :Percent-5-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-10-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-20-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-30-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-40-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-50-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-60-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-70-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-80-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-90-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-95-Recovered-Test-Method-ASTM-D2887)
                                (dm :Final-Boiling-Point-Test-Method-D2887)
                                ))))

                 ;90 Per Trendline Column ER
                 (swap! test-data assoc :90-Per-Trendline-Column-ER
                        (-> (get
                              @test-data :90-Per-Trendline-Column-ER)
                            (conj
                              (c/Ninety-Per-Trendline
                                (dm :dist-type)
                                (dm :temp-type)
                                (dm :Initial-Boiling-Point-Test-Method-D2887)
                                (dm :Percent-5-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-10-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-20-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-30-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-40-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-50-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-60-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-70-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-80-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-90-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-95-Recovered-Test-Method-ASTM-D2887)
                                (dm :Final-Boiling-Point-Test-Method-D2887)
                                ))))

                 ;100 Per Trendline Column ET
                 (swap! test-data assoc :100-Per-Trendline-Column-ET
                        (-> (get
                              @test-data :100-Per-Trendline-Column-ET)
                            (conj
                              (c/Hundred-Per-Trendline
                                (dm :dist-type)
                                (dm :temp-type)
                                (dm :Initial-Boiling-Point-Test-Method-D2887)
                                (dm :Percent-5-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-10-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-20-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-30-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-40-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-50-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-60-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-70-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-80-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-90-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-95-Recovered-Test-Method-ASTM-D2887)
                                (dm :Final-Boiling-Point-Test-Method-D2887)
                                ))))



                 ;Feed 600ºF Column EW
                 (swap! test-data assoc :Feed-600ºF-Column-EW
                        (-> (get
                              @test-data :Feed-600ºF-Column-EW)
                            (conj
                              (c/Feed-600F
                                (dm :dist-type)
                                (dm :temp-type)
                                (dm :Initial-Boiling-Point-Test-Method-D2887)
                                (dm :Percent-5-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-10-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-20-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-30-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-40-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-50-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-60-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-70-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-80-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-90-Recovered-Test-Method-ASTM-D2887)
                                (dm :Percent-95-Recovered-Test-Method-ASTM-D2887)
                                (dm :Final-Boiling-Point-Test-Method-D2887)
                                ))))

                 ;Feed Rate Column C
                 (swap! test-data assoc :Feed-Rate-Column-C
                        (-> (get
                              @test-data :Feed-Rate-Column-C)
                            (conj
                              (c/Feed-Rate
                                (dm :RX-FEED-TO-PASS-A)
                                (dm :RX-FEED-TO-PASS-B)))))

                 ;Feed Ratio Of Cracked Feed Column H
                 (swap! test-data assoc :Feed-Ratio-Of-Cracked-Feed-Column-H
                        (-> (get
                              @test-data :Feed-Ratio-Of-Cracked-Feed-Column-H)
                            (conj
                              (c/Feed-Ratio-Of-Cracked-Feed
                                (dm :RX-FEED-TO-PASS-A)
                                (dm :RX-FEED-TO-PASS-B)
                                (dm :Coker-Debutanizer-Bottoms-Feed-Rate)
                                (dm :Net-Coker-Diesel)))))

                 ;Observed WABT  Column LJ
                 (swap! test-data assoc :Observed-WABT-Column-LJ
                        (-> (get
                              @test-data :Observed-WABT-Column-LJ)
                            (conj
                              (c/OWABT
                                (dm :wt%-in-bed1)
                                (dm :wt%-in-bed2)
                                (dm :wt%-in-bed3)
                                (dm :dt-rule-in-bed1)
                                (dm :dt-rule-in-bed2)
                                (dm :dt-rule-in-bed3)
                                (dm :RX-A-1-Feed-From-Heater-A)
                                (dm :RX-A-1-Bot-Bed-Up-G)
                                (dm :RX-A-1-Bot-Bed-Up-H)
                                (dm :RX-A-1-Bot-Bed-Up-I)
                                (dm :RX-A-1-Top-Bed-Low-D)
                                (dm :RX-A-1-Top-Bed-Low-E)
                                (dm :RX-A-1-Top-Bed-Low-F)
                                (dm :RX-A-1-Bot-Bed-Low-M)
                                (dm :RX-A-1-Bot-Bed-Low-N)
                                (dm :RX-A-1-Bot-Bed-Low-O)
                                (dm :RX-B-1-Top-Bed-Low-D)
                                (dm :RX-B-1-Top-Bed-Low-E)
                                (dm :RX-B-1-Top-Bed-Low-F)
                                (dm :RX-B-1-Bot-Bed-Up-G)
                                (dm :RX-B-1-Bot-Bed-Up-H)
                                (dm :RX-B-1-Bot-Bed-Up-I)
                                (dm :RX-B-1-Bot-Bed-Low-M)
                                (dm :RX-B-1-Bot-Bed-Low-N)
                                (dm :RX-B-1-Bot-Bed-Low-O)
                                (dm :RX-A-2-Inlet-Top)
                                (dm :Reactor-A-Outlet)
                                (dm :RX-Feed-From-Heater-B)
                                (dm :RX-B-2-Inlet-Temp)
                                (dm :Reactor-B-2-Outlet)))))

                 ;Rate Column K
                 (swap! test-data assoc :Rate-Column-K
                        (-> (get
                              @test-data :Rate-Column-K)
                            (conj
                              (c/Make-Up-Gas-Rate
                                (dm :Make-Up-Gas-From-Unicrack-H2)))))

                 ;Hydrogen Purity: Column L
                 (swap! test-data assoc :Hydrogen-Purity-Column-L
                        (-> (get
                              @test-data :Hydrogen-Purity-Column-L)
                            (conj
                              (c/Make-up-gas-Hydrogen-Purity
                                (dm :Makeup-H2)))))

                 ;Rate: Column M
                 (swap! test-data assoc :Rate-Column-M
                        (-> (get
                              @test-data :Rate-Column-M)
                            (conj
                              (c/Recycle-Gas-Rate
                                (dm :Recycle-H2-To-Pass-A)
                                (dm :Recycle-H2-To-Pass-B)))))

                 ;Hydrogen Purity: Column N
                 (swap! test-data assoc :Hydrogen-Purity-Column-N
                        (-> (get
                              @test-data :Hydrogen-Purity-Column-N)
                            (conj
                              (c/Recycle-Gas-Hydrogen-Purity
                                (dm :Recycle-Gas-H2)))))

                 ;Rate: Column W
                 (swap! test-data assoc :Rate-Column-W
                        (-> (get
                              @test-data :Rate-Column-W)
                            (conj
                              (c/Quench-Gas-Rate
                                (dm :H2-Quench-To-MID-RX-A-1)
                                (dm :H2-Quench-RX-A-2-Inlet)
                                (dm :H2-Quench-RX-B-2-Inlet)
                                (dm :H2-Quench-To-MID-RX-B-1)))))

                 ;Hydrogen Purity: Column P
                 (swap! test-data assoc :Hydrogen-Purity-Column-P
                        (-> (get
                              @test-data :Hydrogen-Purity-Column-P)
                            (conj
                              (c/Quench-Gas-Hydrogen-Purity
                                (dm :Recycle-Gas-H2)))))

                 ;Rate: Column AA
                 (swap! test-data assoc :Rate-Column-AA
                        (-> (get
                              @test-data :Rate-Column-AA)
                            (conj
                              (c/Treat-Gas-Rate
                                (dm :Make-Up-Gas-From-Unicrack-H2)
                                (dm :Recycle-H2-To-Pass-A)
                                (dm :Recycle-H2-To-Pass-B)
                                (dm :H2-Quench-To-MID-RX-A-1)
                                (dm :H2-Quench-RX-A-2-Inlet)
                                (dm :H2-Quench-RX-B-2-Inlet)
                                (dm :H2-Quench-To-MID-RX-B-1)
                                (dm :Purge-Rate)))))

                 ;Hydrogen Purity: Column AB
                 (swap! test-data assoc :Hydrogen-Purity-Column-AB
                        (-> (get
                              @test-data :Hydrogen-Purity-Column-AB)
                            (conj
                              (c/Treat-Gas-Hydrogen-Purity
                                (dm :Make-Up-Gas-From-Unicrack-H2)
                                (dm :Makeup-H2)
                                (dm :Recycle-H2-To-Pass-A)
                                (dm :Recycle-Gas-H2)
                                (dm :Recycle-H2-To-Pass-B)
                                (dm :H2-Quench-To-MID-RX-A-1)
                                (dm :H2-Quench-RX-A-2-Inlet)
                                (dm :H2-Quench-To-MID-RX-B-1)
                                (dm :H2-Quench-RX-B-2-Inlet)))))

                 ;Total gas/oil ratio: Column LF
                 (swap! test-data assoc :Total-gas/oil-ratio-Column-LF
                        (-> (get
                              @test-data :Total-gas/oil-ratio-Column-LF)
                            (conj
                              (c/Total-Gas-oil-Ratio
                                (dm :Make-Up-Gas-From-Unicrack-H2)
                                (dm :Recycle-H2-To-Pass-A)
                                (dm :Recycle-H2-To-Pass-B)
                                (dm :H2-Quench-To-MID-RX-A-1)
                                (dm :H2-Quench-RX-A-2-Inlet)
                                (dm :H2-Quench-To-MID-RX-B-1)
                                (dm :H2-Quench-RX-B-2-Inlet)
                                (dm :Purge-Rate)
                                (dm :RX-FEED-TO-PASS-A)
                                (dm :RX-FEED-TO-PASS-B)))))



                 ;Hydrogen/oil ratio: Column LG
                 (swap! test-data assoc :Hydrogen/oil-ratio-Column-LG
                        (-> (get
                              @test-data :Hydrogen/oil-ratio-Column-LG)
                            (conj
                              (c/Hydrogen-Oil-ratio
                                (dm :Make-Up-Gas-From-Unicrack-H2)
                                (dm :Recycle-H2-To-Pass-A)
                                (dm :Recycle-H2-To-Pass-B)
                                (dm :H2-Quench-To-MID-RX-A-1)
                                (dm :H2-Quench-RX-A-2-Inlet)
                                (dm :H2-Quench-To-MID-RX-B-1)
                                (dm :H2-Quench-RX-B-2-Inlet)
                                (dm :Purge-Rate)
                                (dm :Makeup-H2)
                                (dm :Recycle-Gas-H2)
                                (dm :RX-FEED-TO-PASS-A)
                                (dm :RX-FEED-TO-PASS-B)))))


                 ;Train A: Column KU
                 (swap! test-data assoc :Train-A-Column-KU
                        (-> (get
                              @test-data :Train-A-Column-KU)
                            (conj
                              (c/Estimated-Hydrogen-Consumption-Train-A
                                (dm :Reactor-A-Outlet)
                                (dm :RX-A-2-Inlet-Top)
                                (dm :RX-A-1-Bot-Bed-Up-G)
                                (dm :RX-A-1-Bot-Bed-Up-H)
                                (dm :RX-A-1-Bot-Bed-Up-I)
                                ;(dm :A-1-Average-Bottom-Bed-Up-Temp)
                                (dm :RX-A-1-Top-Bed-Low-D)
                                (dm :RX-A-1-Top-Bed-Low-E)
                                (dm :RX-A-1-Top-Bed-Low-F)
                                ;(dm :A-1-Average-Top-Bed-Low-Temp)
                                (dm :RX-A-1-Bot-Bed-Low-M)
                                (dm :RX-A-1-Bot-Bed-Low-N)
                                (dm :RX-A-1-Bot-Bed-Low-O)
                                ;(dm :A-1-Average-Bottom-Bed-Low-Temp)
                                (dm :RX-A-1-Feed-From-Heater-A)
                                (dm :Est-oF-SCF-BBL)
                                ))))





                 ;Hydrogen availability Column KZ
                 (swap! test-data assoc :Hydrogen-Availability-Column-KZ
                        (-> (get
                              @test-data :Hydrogen-Availability-Column-KZ)
                            (conj
                              (c/Hydrogen-Availability
                                (dm :Est-oF-SCF-BBL)
                                (dm :Reactor-B-2-Outlet)
                                (dm :RX-B-2-Inlet-Temp)
                                (dm :RX-B-1-Top-Bed-Low-D)
                                (dm :RX-B-1-Top-Bed-Low-E)
                                (dm :RX-B-1-Top-Bed-Low-F)
                                ;(dm :B-1-Average-Top-Bed-Low-Temp)
                                (dm :RX-B-1-Bot-Bed-Up-G)
                                (dm :RX-B-1-Bot-Bed-Up-H)
                                (dm :RX-B-1-Bot-Bed-Up-I)
                                ;(dm :B-1-Average-Bottom-Bed-Up-Temp)
                                (dm :RX-B-1-Bot-Bed-Low-M)
                                (dm :RX-B-1-Bot-Bed-Low-N)
                                (dm :RX-B-1-Bot-Bed-Low-O)
                                ;(dm :B-1-Average-Bottom-Bed-Low-Temp)
                                (dm :RX-Feed-From-Heater-B)
                                (dm :Makeup-H2)
                                (dm :Recycle-Gas-H2)
                                (dm :Make-Up-Gas-From-Unicrack-H2)
                                (dm :Recycle-H2-To-Pass-A)
                                (dm :Recycle-H2-To-Pass-B)
                                (dm :H2-Quench-To-MID-RX-A-1)
                                (dm :H2-Quench-RX-A-2-Inlet)
                                (dm :H2-Quench-To-MID-RX-B-1)
                                (dm :H2-Quench-RX-B-2-Inlet)
                                (dm :Purge-Rate)
                                (dm :RX-FEED-TO-PASS-A)
                                (dm :RX-FEED-TO-PASS-B)
                                ))))

                 ;Train B: Column KV
                 (swap! test-data assoc :Train-B-Column-KV
                        (-> (get
                              @test-data :Train-B-Column-KV)
                            (conj
                              (c/Estimated-Hydrogen-Consumption-Train-B
                                (dm :Reactor-B-2-Outlet)
                                (dm :RX-B-2-Inlet-Temp)
                                (dm :RX-B-1-Top-Bed-Low-D)
                                (dm :RX-B-1-Top-Bed-Low-E)
                                (dm :RX-B-1-Top-Bed-Low-F)
                                ;(dm :B-1-Average-Top-Bed-Low-Temp)
                                (dm :RX-B-1-Bot-Bed-Up-G)
                                (dm :RX-B-1-Bot-Bed-Up-H)
                                (dm :RX-B-1-Bot-Bed-Up-I)
                                ;(dm :B-1-Average-Bottom-Bed-Up-Temp)
                                (dm :RX-B-1-Bot-Bed-Low-M)
                                (dm :RX-B-1-Bot-Bed-Low-N)
                                (dm :RX-B-1-Bot-Bed-Low-O)
                                ;(dm :B-1-Average-Bottom-Bed-Low-Temp)
                                (dm :RX-Feed-From-Heater-B)
                                (dm :Est-oF-SCF-BBL)))))



                 ;Train A inlet pressure: Column AD
                 (swap! test-data assoc :Train-A-Inlet-Pressure-Column-AD
                        (-> (get
                              @test-data :Train-A-Inlet-Pressure-Column-AD)
                            (conj
                              (c/Train-A-Inlet-Pressure
                                (dm :RX-A-1-Inlet-Pressure)
                                ))))

                 ;Train B inlet pressure: Column AH
                 (swap! test-data assoc :Train-B-inlet-Pressure-Column-AH
                        (-> (get
                              @test-data :Train-B-inlet-Pressure-Column-AH)
                            (conj
                              (c/Train-B-Inlet-Pressure
                                (dm :RX-B-1-Inlet-Pressure)
                                ))))

                 ;Train A hydrogen partial pressure: Column LA
                 (swap! test-data assoc :Train-A-Hydrogen-Partial-Pressure-Column-LA
                        (-> (get
                              @test-data :Train-A-Hydrogen-Partial-Pressure-Column-LA)
                            (conj
                              (c/Train-A-Hydrogen-Partial
                                (dm :Make-Up-Gas-From-Unicrack-H2)
                                (dm :Makeup-H2)
                                (dm :Recycle-H2-To-Pass-A)
                                (dm :Recycle-Gas-H2)
                                (dm :Recycle-H2-To-Pass-B)
                                (dm :H2-Quench-To-MID-RX-A-1)
                                (dm :H2-Quench-RX-B-2-Inlet)
                                (dm :H2-Quench-To-MID-RX-B-1)
                                (dm :H2-Quench-RX-A-2-Inlet)
                                (dm :RX-A-1-Overall-Delta-P)
                                (dm :Reactor-A-2-Diff-Press)
                                (dm :RX-A-1-Inlet-Pressure)
                                ))))

                 ;Train A hydrogen partial pressure: Column LB
                 (swap! test-data assoc :Train-B-Hydrogen-Partial-Pressure-Column-LB
                        (-> (get
                              @test-data :Train-B-Hydrogen-Partial-Pressure-Column-LB)
                            (conj
                              (c/Train-B-Hydrogen-Partial
                                (dm :Make-Up-Gas-From-Unicrack-H2)
                                (dm :Makeup-H2)
                                (dm :Recycle-H2-To-Pass-A)
                                (dm :Recycle-Gas-H2)
                                (dm :Recycle-H2-To-Pass-B)
                                (dm :H2-Quench-To-MID-RX-A-1)
                                (dm :H2-Quench-RX-B-2-Inlet)
                                (dm :H2-Quench-To-MID-RX-B-1)
                                (dm :H2-Quench-RX-A-2-Inlet)
                                (dm :RX-B-1-Inlet-Pressure)
                                (dm :RX-B-1-Overall-Delta-P)
                                (dm :Reactor-B-2-Diff-Press)
                                ))))

                 ;Total pressure drop: Column AG
                 (swap! test-data assoc :Total-Pressure-Drop-Column-AG
                        (-> (get
                              @test-data :Total-Pressure-Drop-Column-AG)
                            (conj
                              (c/Train-A-Total-Pressure-Drop
                                (dm :RX-A-1-Overall-Delta-P)
                                (dm :Reactor-A-2-Diff-Press)
                                ))))


                 ;Normalized pressure drop: Column LC
                 (swap! test-data assoc :Normalized-Pressure-Drop-Column-LC
                        (-> (get
                              @test-data :Normalized-Pressure-Drop-Column-LC)
                            (conj
                              (c/Train-A-Normalized-Pressure-Drop
                                (dm :SOR-Ref-Day-Feed-Rate)
                                (dm :SOR-Ref-Day-Treat-Gas-Rate)
                                (dm :Feed-Exp)
                                (dm :Treat-Gas-Exp)
                                (dm :RX-A-1-Overall-Delta-P)
                                (dm :Reactor-A-2-Diff-Press)
                                (dm :RX-FEED-TO-PASS-A)
                                (dm :RX-FEED-TO-PASS-B)
                                (dm :Make-Up-Gas-From-Unicrack-H2)
                                (dm :Recycle-H2-To-Pass-A)
                                (dm :Recycle-H2-To-Pass-B)
                                (dm :H2-Quench-To-MID-RX-A-1)
                                (dm :H2-Quench-RX-A-2-Inlet)
                                (dm :H2-Quench-To-MID-RX-B-1)
                                (dm :H2-Quench-RX-B-2-Inlet)
                                (dm :Purge-Rate)))))



                 ;Total pressure drop: Column AK
                 (swap! test-data assoc :Total-Pressure-Drop-Column-AK
                        (-> (get
                              @test-data :Total-Pressure-Drop-Column-AK)
                            (conj
                              (c/Train-B-Total-Pressure-Drop
                                (dm :RX-B-1-Overall-Delta-P)
                                (dm :Reactor-B-2-Diff-Press)
                                ))))



                 ;Normalized pressure drop: Column LD
                 (swap! test-data assoc :Normalized-Pressure-Drop-Column-LD
                        (-> (get
                              @test-data :Normalized-Pressure-Drop-Column-LD)
                            (conj
                              (c/Train-B-Normalized-Pressure-Drop
                                (dm :RX-B-1-Overall-Delta-P)
                                (dm :Reactor-B-2-Diff-Press)
                                (dm :SOR-Ref-Day-Feed-Rate)
                                (dm :RX-FEED-TO-PASS-A)
                                (dm :RX-FEED-TO-PASS-B)
                                (dm :Feed-Exp)
                                (dm :SOR-Ref-Day-Treat-Gas-Rate)
                                (dm :Make-Up-Gas-From-Unicrack-H2)
                                (dm :Recycle-H2-To-Pass-A)
                                (dm :Recycle-H2-To-Pass-B)
                                (dm :H2-Quench-To-MID-RX-A-1)
                                (dm :H2-Quench-RX-A-2-Inlet)
                                (dm :H2-Quench-To-MID-RX-B-1)
                                (dm :H2-Quench-RX-B-2-Inlet)
                                (dm :Purge-Rate)
                                (dm :Treat-Gas-Exp)
                                ))))

                 ;Train A DT: Column AY
                 (swap! test-data assoc :Train-A-DT-Column-AY
                        (-> (get
                              @test-data :Train-A-DT-Column-AY)
                            (conj
                              (c/Train-A-Total-Temperature-Rises
                                (dm :Reactor-A-Outlet)
                                (dm :RX-A-2-Inlet-Top)
                                (dm :RX-A-1-Bot-Bed-Up-G)
                                (dm :RX-A-1-Bot-Bed-Up-H)
                                (dm :RX-A-1-Bot-Bed-Up-I)
                                ;(dm :A-1-Average-Bottom-Bed-Up-Temp)
                                (dm :RX-A-1-Top-Bed-Low-D)
                                (dm :RX-A-1-Top-Bed-Low-E)
                                (dm :RX-A-1-Top-Bed-Low-F)
                                ;(dm :A-1-Average-Top-Bed-Low-Temp)
                                (dm :RX-A-1-Bot-Bed-Low-M)
                                (dm :RX-A-1-Bot-Bed-Low-N)
                                (dm :RX-A-1-Bot-Bed-Low-O)
                                ;(dm :A-1-Average-Bottom-Bed-Low-Temp)
                                (dm :RX-A-1-Feed-From-Heater-A)
                                ))))


                 ;Train B DT: Column BM
                 (swap! test-data assoc :Train-B-DT-Column-BM
                        (-> (get
                              @test-data :Train-B-DT-Column-BM)
                            (conj
                              (c/Train-B-Total-Temperature-Rises
                                (dm :Reactor-B-2-Outlet)
                                (dm :RX-B-2-Inlet-Temp)
                                (dm :RX-B-1-Top-Bed-Low-D)
                                (dm :RX-B-1-Top-Bed-Low-E)
                                (dm :RX-B-1-Top-Bed-Low-F)
                                ;(dm :B-1-Average-Top-Bed-Low-Temp)
                                (dm :RX-B-1-Bot-Bed-Up-G)
                                (dm :RX-B-1-Bot-Bed-Up-H)
                                (dm :RX-B-1-Bot-Bed-Up-I)
                                ;(dm :B-1-Average-Bottom-Bed-Up-Temp)
                                (dm :RX-B-1-Bot-Bed-Low-M)
                                (dm :RX-B-1-Bot-Bed-Low-N)
                                (dm :RX-B-1-Bot-Bed-Low-O)
                                ;(dm :B-1-Average-Bottom-Bed-Low-Temp)
                                ;(dm :B-1-Average-Bottom-Bed-Up-Temp)
                                ;(dm :B-1-Average-Top-Bed-Low-Temp)
                                (dm :RX-Feed-From-Heater-B)
                                ))))


                 ;A1 Bed 1 DT: Column AO
                 (swap! test-data assoc :A1-Bed-1-DT-Column-AO
                        (-> (get
                              @test-data :A1-Bed-1-DT-Column-AO)
                            (conj
                              (c/Train-A-1-Bed-1-Temperature-Rises
                                (dm :RX-A-1-Top-Bed-Low-D)
                                (dm :RX-A-1-Top-Bed-Low-E)
                                (dm :RX-A-1-Top-Bed-Low-F)
                                ;(dm :A-1-Average-Top-Bed-Low-Temp)
                                (dm :RX-A-1-Feed-From-Heater-A)
                                ))))


                 ;A1 Bed 2 DT: Column AS
                 (swap! test-data assoc :A1-Bed-2-DT-Column-AS
                        (-> (get
                              @test-data :A1-Bed-2-DT-Column-AS)
                            (conj
                              (c/Train-A-1-Bed-2-Temperature-Rises
                                (dm :RX-A-1-Bot-Bed-Up-G)
                                (dm :RX-A-1-Bot-Bed-Up-H)
                                (dm :RX-A-1-Bot-Bed-Up-I)
                                ;(dm :A-1-Average-Bottom-Bed-Up-Temp)
                                (dm :RX-A-1-Bot-Bed-Low-M)
                                (dm :RX-A-1-Bot-Bed-Low-N)
                                (dm :RX-A-1-Bot-Bed-Low-O)
                                ;(dm :A-1-Average-Bottom-Bed-Low-Temp)
                                ))))


                 ;A2 DT: Column AX
                 (swap! test-data assoc :A2-DT-Column-AX
                        (-> (get
                              @test-data :A2-DT-Column-AX)
                            (conj
                              (c/Train-A-2-Temperature-Rises
                                (dm :Reactor-A-Outlet)
                                (dm :RX-A-2-Inlet-Top)
                                ))))


                 ;B1 Bed 1 DT: Column BC
                 (swap! test-data assoc :B1-Bed-1-DT-Column-BC
                        (-> (get
                              @test-data :B1-Bed-1-DT-Column-BC)
                            (conj
                              (c/Train-B-1-Bed-1-Temperature-Rises
                                (dm :RX-B-1-Top-Bed-Low-D)
                                (dm :RX-B-1-Top-Bed-Low-E)
                                (dm :RX-B-1-Top-Bed-Low-F)
                                ;(dm :B-1-Average-Top-Bed-Low-Temp)
                                (dm :RX-Feed-From-Heater-B)
                                ))))


                 ;B1 Bed 2 DT: Column BG
                 (swap! test-data assoc :B1-Bed-2-DT-Column-BG
                        (-> (get
                              @test-data :B1-Bed-2-DT-Column-BG)
                            (conj
                              (c/Train-B-1-Bed-2-Temperature-Rises

                                (dm :RX-B-1-Bot-Bed-Up-G)
                                (dm :RX-B-1-Bot-Bed-Up-H)
                                (dm :RX-B-1-Bot-Bed-Up-I)
                                ;(dm :B-1-Average-Bottom-Bed-Up-Temp)
                                (dm :RX-B-1-Bot-Bed-Low-M)
                                (dm :RX-B-1-Bot-Bed-Low-N)
                                (dm :RX-B-1-Bot-Bed-Low-O)
                                ;(dm :B-1-Average-Bottom-Bed-Low-Temp)
                                ))))


                 ;B2 DT: Column BL
                 (swap! test-data assoc :B2-DT-Column-BL
                        (-> (get
                              @test-data :B2-DT-Column-BL)
                            (conj
                              (c/Train-B-2-Temperature-Rises
                                (dm :Reactor-B-2-Outlet)
                                (dm :RX-B-2-Inlet-Temp)
                                ))))

                 ;A1 Bed 1 inlet T: Column AL
                 (swap! test-data assoc :A1-Bed-1-inlet-T-Column-AL
                        (-> (get
                              @test-data :A1-Bed-1-inlet-T-Column-AL)
                            (conj
                              (c/Train-A-1-Bed-1-Inlet
                                (dm :RX-A-1-Feed-From-Heater-A)
                                ))))


                 ;A1 Bed 1 outlet T: Column AN
                 (swap! test-data assoc :A1-Bed-1-outlet-T-Column-AN
                        (-> (get
                              @test-data :A1-Bed-1-outlet-T-Column-AN)
                            (conj
                              (c/Train-A-1-Bed-1-Outlet
                                (dm :RX-A-1-Top-Bed-Low-D)
                                (dm :RX-A-1-Top-Bed-Low-E)
                                (dm :RX-A-1-Top-Bed-Low-F)
                                ;(dm :A-1-Average-Top-Bed-Low-Temp)
                                ))))


                 ;A1 Bed 2 outlet T: Column AR
                 (swap! test-data assoc :A1-Bed-2-outlet-T-Column-AR
                        (-> (get
                              @test-data :A1-Bed-2-outlet-T-Column-AR)
                            (conj
                              (c/Train-A-1-Bed-2-Outlet
                                (dm :RX-A-1-Bot-Bed-Low-M)
                                (dm :RX-A-1-Bot-Bed-Low-N)
                                (dm :RX-A-1-Bot-Bed-Low-O)
                                ;(dm :A-1-Average-Bottom-Bed-Low-Temp)
                                ))))

                 ;A2 outlet T: Column AW
                 (swap! test-data assoc :A2-outlet-T-Column-AW
                        (-> (get
                              @test-data :A2-outlet-T-Column-AW)
                            (conj
                              (c/Train-A-2-Outlet
                                (dm :Reactor-A-Outlet)
                                ))))

                 ;B1 Bed 1 inlet T: Column AZ
                 (swap! test-data assoc :B1-Bed-1-inlet-T-Column-AZ
                        (-> (get
                              @test-data :B1-Bed-1-inlet-T-Column-AZ)
                            (conj
                              (c/Train-B-1-Bed-1-Inlet
                                (dm :RX-Feed-From-Heater-B)
                                ))))

                 ;B1 Bed 1 outlet T: Column BB
                 (swap! test-data assoc :B1-Bed-1-outlet-T-Column-BB
                        (-> (get
                              @test-data :B1-Bed-1-outlet-T-Column-BB)
                            (conj
                              (c/Train-B-1-Bed-1-Outlet
                                (dm :RX-B-1-Top-Bed-Low-D)
                                (dm :RX-B-1-Top-Bed-Low-E)
                                (dm :RX-B-1-Top-Bed-Low-F)
                                ;(dm :B-1-Average-Top-Bed-Low-Temp)
                                ))))


                 ;B1 Bed 2 outlet T: Column BF
                 (swap! test-data assoc :B1-Bed-2-outlet-T-Column-BF
                        (-> (get
                              @test-data :B1-Bed-2-outlet-T-Column-BF)
                            (conj
                              (c/Train-B-2-Bed-1-Outlet
                                (dm :RX-B-1-Bot-Bed-Low-M)
                                (dm :RX-B-1-Bot-Bed-Low-N)
                                (dm :RX-B-1-Bot-Bed-Low-O)
                                ;(dm :B-1-Average-Bottom-Bed-Low-Temp)
                                ))))

                 ;B2 outlet T: Column BK
                 (swap! test-data assoc :B2-outlet-T-Column-BK
                        (-> (get
                              @test-data :B2-outlet-T-Column-BK)
                            (conj
                              (c/Train-B-2-Outlet
                                (dm :Reactor-B-2-Outlet)
                                ))))

                 ;Top radial spread: Column BZ
                 (swap! test-data assoc :Top-radial-spread-Column-BZ
                        (-> (get
                              @test-data :Top-radial-spread-Column-BZ)
                            (conj
                              (c/Train-A-Reactor-1-Bed-1-Radial-Temperature-Top-Spreads
                                (dm :RX-A-1-Top-Bed-Up-A)
                                (dm :RX-A-1-Top-Bed-Up-B)
                                (dm :RX-A-1-Top-Bed-Up-C)
                                ))))

                 ;Bottom radial spread: Column CA
                 (swap! test-data assoc :Bottom-radial-spread-Column-CA
                        (-> (get
                              @test-data :Bottom-radial-spread-Column-CA)
                            (conj
                              (c/Train-A-Reactor-1-Bed-1-Radial-Temperature-Bottom-Spreads
                                (dm :RX-A-1-Top-Bed-Low-D)
                                (dm :RX-A-1-Top-Bed-Low-E)
                                (dm :RX-A-1-Top-Bed-Low-F)
                                ))))



                 ;Top radial spread: Column CB
                 (swap! test-data assoc :Top-radial-spread-Column-CB
                        (-> (get
                              @test-data :Top-radial-spread-Column-CB)
                            (conj
                              (c/Train-A-Reactor-1-Bed-2-Radial-Temperature-Top-Spreads
                                (dm :RX-A-1-Bot-Bed-Up-G)
                                (dm :RX-A-1-Bot-Bed-Up-H)
                                (dm :RX-A-1-Bot-Bed-Up-I)
                                ))))



                 ;Middle radial spread: CC
                 (swap! test-data assoc :Middle-radial-spread-Column-CC
                        (-> (get
                              @test-data :Middle-radial-spread-Column-CC)
                            (conj
                              (c/Train-A-Reactor-1-Bed-2-Radial-Temperature-Middle-Spreads
                                (dm :RX-A-1-Bot-Bed-Mid-J)
                                (dm :RX-A-1-Bot-Bed-Mid-K)
                                (dm :RX-A-1-Bot-Bed-Mid-L)
                                ))))



                 ;Bottom radial spread: Column CD
                 (swap! test-data assoc :Bottom-radial-spread-Column-CD
                        (-> (get
                              @test-data :Bottom-radial-spread-Column-CD)
                            (conj
                              (c/Train-A-Reactor-1-Bed-2-Radial-Temperature-Bottom-Spreads
                                (dm :RX-A-1-Bot-Bed-Low-M)
                                (dm :RX-A-1-Bot-Bed-Low-N)
                                (dm :RX-A-1-Bot-Bed-Low-O)
                                ))))

                 ;Radial spread: Column CE
                 (swap! test-data assoc :Radial-spread-Column-CE
                        (-> (get
                              @test-data :Radial-spread-Column-CE)
                            (conj
                              (c/Train-A-Reactor-2-Radial-Temperature-Spreads
                                (dm :A-2-Reactor-Bed-A)
                                (dm :A-2-Reactor-Bed-B)
                                (dm :A-2-Reactor-Bed-C)
                                ))))

                 ;Top radial spread: Column CF
                 (swap! test-data assoc :Top-radial-spread-Column-CF
                        (-> (get
                              @test-data :Top-radial-spread-Column-CF)
                            (conj
                              (c/Train-B-Reactor-1-Bed-1-Radial-Temperature-Top-Spreads
                                (dm :RX-B-1-Top-Bed-Up-A)
                                (dm :RX-B-1-Top-Bed-Up-B)
                                (dm :RX-B-1-Top-Bed-Up-C)
                                ))))

                 ;Bottom radial spread: Column CG
                 (swap! test-data assoc :Bottom-radial-spread-Column-CG
                        (-> (get
                              @test-data :Bottom-radial-spread-Column-CG)
                            (conj
                              (c/Train-B-Reactor-1-Bed-1-Radial-Temperature-Bottom-Spreads
                                (dm :RX-B-1-Top-Bed-Low-D)
                                (dm :RX-B-1-Top-Bed-Low-E)
                                (dm :RX-B-1-Top-Bed-Low-F)
                                ))))


                 ;Top radial spread: Column CH
                 (swap! test-data assoc :Top-radial-spread-Column-CH
                        (-> (get
                              @test-data :Top-radial-spread-Column-CH)
                            (conj
                              (c/Train-B-Reactor-1-Bed-2-Radial-Temperature-Top-Spreads
                                (dm :RX-B-1-Bot-Bed-Up-G)
                                (dm :RX-B-1-Bot-Bed-Up-H)
                                (dm :RX-B-1-Bot-Bed-Up-I)
                                ))))

                 ;Middle radial spread: CI
                 (swap! test-data assoc :Middle-radial-spread-Column-CI
                        (-> (get
                              @test-data :Middle-radial-spread-Column-CI)
                            (conj
                              (c/Train-B-Reactor-1-Bed-2-Radial-Temperature-Middle-Spreads
                                (dm :RX-B-1-Bot-Bed-Mid-J)
                                (dm :RX-B-1-Bot-Bed-Mid-K)
                                (dm :RX-B-1-Bot-Bed-Mid-L)
                                ))))


                 ;Bottom radial spread: Column CJ
                 (swap! test-data assoc :Bottom-radial-spread-Column-CJ
                        (-> (get
                              @test-data :Bottom-radial-spread-Column-CJ)
                            (conj
                              (c/Train-B-Reactor-1-Bed-2-Radial-Temperature-Bottom-Spreads
                                (dm :RX-B-1-Bot-Bed-Low-M)
                                (dm :RX-B-1-Bot-Bed-Low-N)
                                (dm :RX-B-1-Bot-Bed-Low-O)
                                ))))

                 ;Radial spread: Column CK
                 (swap! test-data assoc :Radial-spread-Column-CK
                        (-> (get
                              @test-data :Radial-spread-Column-CK)
                            (conj
                              (c/Train-B-Reactor-2-Radial-Temperature-Spreads
                                (dm :B-2-Reactor-Bed-A)
                                (dm :B-2-Reactor-Bed-B)
                                (dm :B-2-Reactor-Bed-C)
                                ))))

                 (go
                   ;Radial spread: Column CK
                   (let [{:keys [result]}
                         (<! (c/NHDSWABT
                               (dm :eact)
                               (dm :SOR-Ref-Day-k-Inh)
                               (dm :n)
                               (dm :Active-Catalyst-Volume)
                               (dm :SOR-Ref-Day-K-sup)
                               (dm :SOR-Ref-Day-Kinh-Corr)
                               (dm :SOR-Ref-Day-A-Train-Topsoe-WABT)
                               (dm :Gravity)
                               (dm :SULFUR-WT%-X-RA-Result)
                               (dm :Net-Diesel-To-Storage)
                               (dm :ULSD-Prod-Gravity)
                               (dm :Sulfur)
                               (dm :Net-Kerosene-To-Storage)
                               (dm :Prod-API-Gravity-Naphtha)
                               (dm :Kero-Sulfur)
                               (dm :Make-Up-Gas-From-Unicrack-H2)
                               (dm :Recycle-H2-To-Pass-A)
                               (dm :Recycle-H2-To-Pass-B)
                               (dm :H2-Quench-To-MID-RX-A-1)
                               (dm :H2-Quench-RX-A-2-Inlet)
                               (dm :H2-Quench-To-MID-RX-B-1)
                               (dm :H2-Quench-RX-B-2-Inlet)
                               (dm :Purge-Rate)
                               (dm :RX-FEED-TO-PASS-A)
                               (dm :RX-FEED-TO-PASS-B)
                               (dm :RX-A-1-Inlet-Pressure)
                               (dm :RX-A-1-Overall-Delta-P)
                               (dm :Reactor-A-2-Diff-Press)
                               (dm :Makeup-H2)
                               (dm :Recycle-Gas-H2)
                               (dm :Initial-Boiling-Point-Test-Method-D2887)
                               (dm :Percent-5-Recovered-Test-Method-ASTM-D2887)
                               (dm :Percent-10-Recovered-Test-Method-ASTM-D2887)
                               (dm :Percent-20-Recovered-Test-Method-ASTM-D2887)
                               (dm :Percent-30-Recovered-Test-Method-ASTM-D2887)
                               (dm :Percent-40-Recovered-Test-Method-ASTM-D2887)
                               (dm :Percent-50-Recovered-Test-Method-ASTM-D2887)
                               (dm :Percent-60-Recovered-Test-Method-ASTM-D2887)
                               (dm :Percent-70-Recovered-Test-Method-ASTM-D2887)
                               (dm :Percent-80-Recovered-Test-Method-ASTM-D2887)
                               (dm :Percent-90-Recovered-Test-Method-ASTM-D2887)
                               (dm :Percent-95-Recovered-Test-Method-ASTM-D2887)
                               (dm :Final-Boiling-Point-Test-Method-D2887)
                               (dm :wt%-in-bed1)
                               (dm :wt%-in-bed2)
                               (dm :wt%-in-bed3)
                               (dm :dt-rule-in-bed1)
                               (dm :dt-rule-in-bed2)
                               (dm :dt-rule-in-bed3)
                               (dm :RX-A-1-Feed-From-Heater-A)
                               (dm :RX-A-1-Bot-Bed-Up-G)
                               (dm :RX-A-1-Bot-Bed-Up-H)
                               (dm :RX-A-1-Bot-Bed-Up-I)
                               ;(dm :A-1-Average-Bottom-Bed-Up-Temp)
                               (dm :RX-A-1-Top-Bed-Low-D)
                               (dm :RX-A-1-Top-Bed-Low-E)
                               (dm :RX-A-1-Top-Bed-Low-F)
                               ;(dm :A-1-Average-Top-Bed-Low-Temp)
                               (dm :RX-A-1-Bot-Bed-Low-M)
                               (dm :RX-A-1-Bot-Bed-Low-N)
                               (dm :RX-A-1-Bot-Bed-Low-O)
                               ;(dm :A-1-Average-Bottom-Bed-Low-Temp)
                               ;(dm :A-1-Average-Bottom-Bed-Up-Temp)
                               ;(dm :A-1-Average-Top-Bed-Low-Temp)
                               ;(dm :A-1-Average-Bottom-Bed-Low-Temp)
                               (dm :Reactor-A-Outlet)
                               (dm :RX-A-2-Inlet-Top)
                               (dm :Recycle-Gas-H2S)
                               (dm :KH2S)
                               ))]
                     (swap! test-data assoc :Normalized-HDS-WABT-Column-MI
                            (-> (get
                                  @test-data :Normalized-HDS-WABT-Column-MI)
                                (conj
                                  result)))))
                 (go
                   ;Normalized HDB WABT Column MS
                   (let [{:keys [result]}
                         (<! (c/NHDNWABT
                               (dm :RX-FEED-TO-PASS-A)
                               (dm :RX-FEED-TO-PASS-B)
                               (dm :Active-Catalyst-Volume)
                               (dm :Charge-N2)
                               (dm :Product-N2)
                               (dm :Make-Up-Gas-From-Unicrack-H2)
                               (dm :Makeup-H2)
                               (dm :Recycle-H2-To-Pass-A)
                               (dm :Recycle-Gas-H2)
                               (dm :Recycle-H2-To-Pass-B)
                               (dm :H2-Quench-To-MID-RX-A-1)
                               (dm :H2-Quench-RX-A-2-Inlet)
                               (dm :H2-Quench-To-MID-RX-B-1)
                               (dm :H2-Quench-RX-B-2-Inlet)
                               (dm :RX-A-1-Feed-From-Heater-A)
                               (dm :RX-A-1-Bot-Bed-Up-G)
                               (dm :RX-A-1-Bot-Bed-Up-H)
                               (dm :RX-A-1-Bot-Bed-Up-I)
                               ;(dm :A-1-Average-Bottom-Bed-Up-Temp)
                               (dm :RX-A-1-Top-Bed-Low-D)
                               (dm :RX-A-1-Top-Bed-Low-E)
                               (dm :RX-A-1-Top-Bed-Low-F)
                               ;(dm :A-1-Average-Top-Bed-Low-Temp)
                               (dm :RX-A-1-Bot-Bed-Low-M)
                               (dm :RX-A-1-Bot-Bed-Low-N)
                               (dm :RX-A-1-Bot-Bed-Low-O)
                               ;(dm :A-1-Average-Bottom-Bed-Low-Temp)
                               (dm :SOR-Ref-Day-A-Train-Topsoe-WABT)
                               (dm :SOR-Ref-Day-H2-Partial-Pressure)
                               (dm :SOR-Ref-Day-K-HDN)
                               (dm :RX-B-1-Inlet-Pressure)
                               (dm :RX-B-1-Overall-Delta-P)
                               (dm :Reactor-B-2-Diff-Press)
                               (dm :Reactor-A-Outlet)
                               (dm :RX-A-2-Inlet-Top)
                               (dm :wt%-in-bed1)
                               (dm :wt%-in-bed2)
                               (dm :wt%-in-bed3)
                               (dm :dt-rule-in-bed1)
                               (dm :dt-rule-in-bed2)
                               (dm :dt-rule-in-bed3)
                               (dm :Eact-HDN)
                               (dm :H2PP-Exp)
                               ))]
                     (swap! test-data assoc :Normalized-HDB-WABT-Column-MS
                            (-> (get
                                  @test-data :Normalized-HDB-WABT-Column-MS)
                                (conj
                                  result
                                  ))))))
               data))


(defn test-naphtha-nht-calcuolation [data]
  (map-indexed (fn [idx dm]
                 (if (= idx 0)
                   (do


                     )

                   
                   )



                 )



               data))

  (defn get-calculation-demo-data []
  (last (test-diesel-hdt-calculations test-diesel-hdt-data ))
  (add-watch test-data :watcher
             (fn [key data old-state new-state]
               (let [Normalized-HDS-WABT-Column-MI (first (:Normalized-HDS-WABT-Column-MI new-state))
                     Normalized-HDB-WABT-Column-MS (first (:Normalized-HDB-WABT-Column-MS  new-state))]
                 (if (and Normalized-HDS-WABT-Column-MI Normalized-HDB-WABT-Column-MS )
                   ;(print new-state "\n")
                   (log/info "Calculation Complete")
                   )))))




