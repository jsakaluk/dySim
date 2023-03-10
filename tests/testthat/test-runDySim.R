test_that("runDySim produces correct paramTable output for P: L-APIM and S: L-APIM", {
  popModList.apim <- list(nItemsX = 5,
                          loadValuesX_A = "moderate", loadValuesX_B = "moderate",
                          residCorrValuesX = "moderate", iccX = "weak",
                          nItemsY = 5,
                          loadValuesY_A = "weak", loadValuesY_B = "mixed",
                          residCorrValuesY = "mixed", iccY = "strong",
                          actorA = "moderate", actorB = "very strong",
                          partnerA = "weak", partnerB = "weak")

  expect_equal(runDySim(seed = 123,
                        popMod = "L-APIM",
                        popModList = popModList.apim,
                        sampSize = 100,
                        sampMod = "L-APIM",
                        nSims = 2,
                        output = "paramTable")$sim.dat,
               structure(list(sim_num = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2
               ), pop_mod = c("L-APIM", "L-APIM", "L-APIM", "L-APIM", "L-APIM",
                              "L-APIM", "L-APIM", "L-APIM", "L-APIM", "L-APIM", "L-APIM", "L-APIM"
               ), samp_mod = c("L-APIM", "L-APIM", "L-APIM", "L-APIM", "L-APIM",
                               "L-APIM", "L-APIM", "L-APIM", "L-APIM", "L-APIM", "L-APIM", "L-APIM"
               ), samp_n = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100,
                             100, 100), lhs = c("YA", "YB", "YA", "YB", "k1", "k2", "YA",
                                                "YB", "YA", "YB", "k1", "k2"), op = c("~", "~", "~", "~", ":=",
                                                                                      ":=", "~", "~", "~", "~", ":=", ":="), rhs = c("XA", "XB", "XB",
                                                                                                                                     "XA", "p1/a1", "p2/a2", "XA", "XB", "XB", "XA", "p1/a1", "p2/a2"
                                                                                      ), label = c("a1", "a2", "p1", "p2", "k1", "k2", "a1", "a2",
                                                                                                   "p1", "p2", "k1", "k2"), est = c(-0.0549120119229995, 0.415750311284535,
                                                                                                                                    -0.0142349924319804, 0.069289969045606, 0.259232760437579, 0.166662458607721,
                                                                                                                                    0.150800347632958, 0.480510269812234, -0.161639408319174, -0.0848906409074196,
                                                                                                                                    -1.07187689455861, -0.176667693160006), se = c(0.139935080053524,
                                                                                                                                                                                   0.148767085894018, 0.140215778991522, 0.144132567827298, 2.4711018565607,
                                                                                                                                                                                   0.335998941327079, 0.136442590105336, 0.145426235779743, 0.136030380932957,
                                                                                                                                                                                   0.139419075601121, 1.17631023711616, 0.283588486322321), z = c(-0.392410622854514,
                                                                                                                                                                                                                                                  2.79463907480661, -0.101522043627066, 0.480737768639704, 0.1049057365844,
                                                                                                                                                                                                                                                  0.496020784915162, 1.10522929472782, 3.30415118864794, -1.18825961678986,
                                                                                                                                                                                                                                                  -0.608888278317757, -0.911219558189361, -0.622972023480563),
               pvalue = c(0.694754838305126, 0.00519576869796778, 0.919136059676198,
                          0.630702884457495, 0.916450606952826, 0.619879746542468,
                          0.269060246213779, 0.00095264441940146, 0.234731142487561,
                          0.542598494261072, 0.362179700880569, 0.533302906533279),
               ci.lower = c(-0.329179729001635, 0.124172180847282, -0.289052869319592,
                            -0.213204672895174, -4.58403788055145, -0.49188336523694,
                            -0.116622214930862, 0.195480085276707, -0.428254055751035,
                            -0.358147007843483, -3.37740259395206, -0.732490912781984
               ), ci.upper = c(0.219355705155636, 0.707328441721788, 0.260582884455631,
                               0.351784610986386, 5.10250340142661, 0.825208282452382, 0.418222910196778,
                               0.765540454347761, 0.104975239112686, 0.188365726028644,
                               1.23364880483484, 0.379155526461972)), row.names = c(NA,
                                                                                    -12L), class = c("lavaan.data.frame", "data.frame"))
  )
})

test_that("runDySim produces correct paramTable output for P: L-APIM and S: O-APIM", {
  popModList.apim <- list(nItemsX = 5,
                          loadValuesX_A = "moderate", loadValuesX_B = "moderate",
                          residCorrValuesX = "moderate", iccX = "weak",
                          nItemsY = 5,
                          loadValuesY_A = "weak", loadValuesY_B = "mixed",
                          residCorrValuesY = "mixed", iccY = "strong",
                          actorA = "moderate", actorB = "very strong",
                          partnerA = "weak", partnerB = "weak")

  expect_equal(runDySim(seed = 123,
                        popMod = "L-APIM",
                        popModList = popModList.apim,
                        sampSize = 100,
                        sampMod = "O-APIM",
                        nSims = 2,
                        output = "paramTable")$sim.dat,
               structure(list(sim_num = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2
               ), pop_mod = c("L-APIM", "L-APIM", "L-APIM", "L-APIM", "L-APIM",
                              "L-APIM", "L-APIM", "L-APIM", "L-APIM", "L-APIM", "L-APIM", "L-APIM"
               ), samp_mod = c("O-APIM", "O-APIM", "O-APIM", "O-APIM", "O-APIM",
                               "O-APIM", "O-APIM", "O-APIM", "O-APIM", "O-APIM", "O-APIM", "O-APIM"
               ), samp_n = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100,
                             100, 100), lhs = c("Y_A", "Y_B", "Y_A", "Y_B", "k1", "k2", "Y_A",
                                                "Y_B", "Y_A", "Y_B", "k1", "k2"), op = c("~", "~", "~", "~",
                                                                                         ":=", ":=", "~", "~", "~", "~", ":=", ":="), rhs = c("X_A", "X_B",
                                                                                                                                              "X_B", "X_A", "p1/a1", "p2/a2", "X_A", "X_B", "X_B", "X_A", "p1/a1",
                                                                                                                                              "p2/a2"), label = c("a1", "a2", "p1", "p2", "k1", "k2", "a1",
                                                                                                                                                                  "a2", "p1", "p2", "k1", "k2"), est = c(-0.0306934264880345, 0.250325728848689,
                                                                                                                                                                                                         -0.0602157802670168, -0.034618197089765, 1.96184613961205, -0.138292604795291,
                                                                                                                                                                                                         -0.0111344804747976, 0.0820135266464922, -0.00700001507052656,
                                                                                                                                                                                                         0.0726497142052773, 0.628679091617321, 0.885825999391828), se = c(0.0749655317692394,
                                                                                                                                                                                                                                                                           0.105120664405426, 0.0798462870515836, 0.098694965014434, 5.42862326489595,
                                                                                                                                                                                                                                                                           0.399111872903358, 0.0815460202567115, 0.10121443034573, 0.0826711729823848,
                                                                                                                                                                                                                                                                           0.0998369043221762, 9.64926713247997, 1.82490124833162), z = c(-0.409433852647317,
                                                                                                                                                                                                                                                                                                                                          2.38131798599789, -0.754146278938621, -0.350759505155122, 0.361389259095999,
                                                                                                                                                                                                                                                                                                                                          -0.346500853981805, -0.136542291576531, 0.810294800517563, -0.0846729859756326,
                                                                                                                                                                                                                                                                                                                                          0.727683963144879, 0.0651530404315528, 0.48541037505546), pvalue = c(0.682221300660965,
                                                                                                                                                                                                                                                                                                                                                                                                               0.0172508135059055, 0.450761391067677, 0.725768779500888, 0.717808476234153,
                                                                                                                                                                                                                                                                                                                                                                                                               0.728966342531171, 0.891392593752607, 0.417770762974235, 0.932521372889547,
                                                                                                                                                                                                                                                                                                                                                                                                               0.466807069457751, 0.948052149947415, 0.627385274254103), ci.lower = c(-0.177623168837637,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      0.0442930125831323, -0.216711627187368, -0.228056773973496, -8.67805994522024,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      -0.920537501488201, -0.170961743260526, -0.116363111546877, -0.169032536675681,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      -0.123027022594159, -18.2835369652495, -2.69091472268033), ci.upper = c(0.116236315861568,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              0.456358445114245, 0.096280066653334, 0.158820379793966, 12.6017522244443,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              0.643952291897618, 0.148692782310931, 0.280390164839861, 0.155032506534628,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              0.268326451004714, 19.5408951484841, 4.46256672146398)), row.names = c(NA,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     -12L), class = c("lavaan.data.frame", "data.frame"))
               )
})

test_that("runDySim produces correct paramTable output for P: L-APIM and S: MLM", {
  popModList.apim <- list(nItemsX = 5,
                          loadValuesX_A = "moderate", loadValuesX_B = "moderate",
                          residCorrValuesX = "moderate", iccX = "weak",
                          nItemsY = 5,
                          loadValuesY_A = "weak", loadValuesY_B = "mixed",
                          residCorrValuesY = "mixed", iccY = "strong",
                          actorA = "moderate", actorB = "very strong",
                          partnerA = "weak", partnerB = "weak")

  expect_equal(runDySim(seed = 123,
                        popMod = "L-APIM",
                        popModList = popModList.apim,
                        sampSize = 100,
                        sampMod = "MLM",
                        nSims = 3,
                        output = "paramTable")$sim.dat,
               structure(list(sim_num = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2,
                                          3, 3, 3, 3, 3, 3), pop_mod = c("L-APIM", "L-APIM", "L-APIM",
                                                                         "L-APIM", "L-APIM", "L-APIM", "L-APIM", "L-APIM", "L-APIM", "L-APIM",
                                                                         "L-APIM", "L-APIM", "L-APIM", "L-APIM", "L-APIM", "L-APIM", "L-APIM",
                                                                         "L-APIM"), samp_mod = c("MLM", "MLM", "MLM", "MLM", "MLM", "MLM",
                                                                                                 "MLM", "MLM", "MLM", "MLM", "MLM", "MLM", "MLM", "MLM", "MLM",
                                                                                                 "MLM", "MLM", "MLM"), samp_n = c(100, 100, 100, 100, 100, 100,
                                                                                                                                  100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100),
                              lhs = c("Y_A", "Y_B", "Y_A", "Y_B", "k1", "k2", "Y_A", "Y_B",
                                      "Y_A", "Y_B", "k1", "k2", "Y_A", "Y_B", "Y_A", "Y_B", "k1",
                                      "k2"), op = c("~", "~", "~", "~", ":=", ":=", "~", "~", "~",
                                                    "~", ":=", ":=", "~", "~", "~", "~", ":=", ":="), rhs = c("X_A",
                                                                                                              "X_B", "X_B", "X_A", "p1/a1", "p2/a2", "X_A", "X_B", "X_B",
                                                                                                              "X_A", "p1/a1", "p2/a2", "X_A", "X_B", "X_B", "X_A", "p1/a1",
                                                                                                              "p2/a2"), label = c("a1", "a2", "p1", "p2", "k1", "k2", "a1",
                                                                                                                                  "a2", "p1", "p2", "k1", "k2", "a1", "a2", "p1", "p2", "k1",
                                                                                                                                  "k2"), est = c(-0.0306934264880344, 0.250325728848689, -0.0602157802670169,
                                                                                                                                                 -0.0346181970897649, 1.96184613961206, -0.138292604795291,
                                                                                                                                                 -0.0111344804747975, 0.0820135266464923, -0.00700001507052668,
                                                                                                                                                 0.0726497142052772, 0.628679091617337, 0.885825999391826,
                                                                                                                                                 0.131558928924028, 0.26320489663308, 0.0649417922262329,
                                                                                                                                                 -0.0119434163282345, 0.493632722289302, -0.0453768774100135
                                                                                                                                  ), se = c(0.0761159653814894, 0.106733862676509, 0.0810716223487215,
                                                                                                                                            0.10020955250621, NA, NA, 0.0827974406078169, 0.10276768138794,
                                                                                                                                            0.0839398597692459, 0.101369016096909, NA, NA, 0.115046551906992,
                                                                                                                                            0.101519494252679, 0.084963234822492, 0.137464961044782,
                                                                                                                                            NA, NA), z = c(-0.403245578430235, 2.34532624015848, -0.742747937225244,
                                                                                                                                                           -0.345458054885731, NA, NA, -0.134478558673543, 0.798047844797607,
                                                                                                                                                           -0.0833932185468264, 0.716685600813407, NA, NA, 1.14352778717249,
                                                                                                                                                           2.59265374173328, 0.764351691198098, -0.086883350036696,
                                                                                                                                                           NA, NA), pvalue = c(0.6872116410864, 0.0200206831247657,
                                                                                                                                                                               0.458532723643062, 0.730124332229571, NA, NA, 0.89316357768276,
                                                                                                                                                                               0.425818515409186, 0.933624866146542, 0.474430020354127,
                                                                                                                                                                               NA, NA, 0.254228629378664, 0.0102491963092475, 0.445586378927608,
                                                                                                                                                                               0.930853844656676, NA, NA), ci.lower = c(-0.179877977284251,
                                                                                                                                                                                                                        0.0411312020718868, -0.219113240238744, -0.231025310908812,
                                                                                                                                                                                                                        NA, NA, -0.173414482078213, -0.119407427648556, -0.171519117085591,
                                                                                                                                                                                                                        -0.126029906492926, NA, NA, -0.0939281693591948, 0.0642303441691072,
                                                                                                                                                                                                                        -0.101583088035871, -0.281369789112208, NA, NA), ci.upper = c(0.118491124308182,
                                                                                                                                                                                                                                                                                      0.459520255625491, 0.0986816797047098, 0.161788916729282,
                                                                                                                                                                                                                                                                                      NA, NA, 0.151145521128618, 0.283434480941541, 0.157519086944538,
                                                                                                                                                                                                                                                                                      0.27132933490348, NA, NA, 0.357046027207251, 0.462179449097052,
                                                                                                                                                                                                                                                                                      0.231466672488336, 0.257482956455739, NA, NA)), row.names = c(NA,
                                                                                                                                                                                                                                                                                                                                                    -18L), class = "data.frame")
               )
})

test_that("runDySim produces correct modelFit output for P: L-APIM and S: L-APIM", {
  popModList.apim <- list(nItemsX = 5,
                          loadValuesX_A = "moderate", loadValuesX_B = "moderate",
                          residCorrValuesX = "moderate", iccX = "weak",
                          nItemsY = 5,
                          loadValuesY_A = "weak", loadValuesY_B = "mixed",
                          residCorrValuesY = "mixed", iccY = "strong",
                          actorA = "moderate", actorB = "very strong",
                          partnerA = "weak", partnerB = "weak")

  expect_equal(runDySim(seed = 125,
                        popMod = "L-APIM",
                        popModList = popModList.apim,
                        sampSize = 100,
                        sampMod = "L-APIM",
                        nSims = 3,
                        output = "modelFit")$sim.dat,
               structure(list(sim_num = c(1, 1, 1), pop_mod = c("L-APIM", "L-APIM",
                                                                "L-APIM"), samp_mod = c("L-APIM", "L-APIM", "L-APIM"), samp_n = c(100,
                                                                                                                                  100, 100), cfi = c(0.854361627334296, 0.838330831498373, 0.738757756499453
                                                                                                                                  ), tli = c(0.849612549964762, 0.833059010786364, 0.730238987689652
                                                                                                                                  ), aic = c(5297.01787879608, 5242.59277037589, 5357.6872871806
                                                                                                                                  ), bic = c(5416.85570735153, 5362.43059893135, 5477.52511573605
                                                                                                                                  ), rmsea = c(0.0689311439592113, 0.068101136278879, 0.0881757244718191
                                                                                                                                  ), srmr = c(0.0872742711295682, 0.101033307529217, 0.103201107596133
                                                                                                                                  )), row.names = c("fitMeasures...1", "fitMeasures...2", "fitMeasures...3"
                                                                                                                                  ), class = "data.frame")
  )
})

test_that("runDySim produces correct modelFit output for P: L-APIM and S: O-APIM", {
  popModList.apim <- list(nItemsX = 5,
                          loadValuesX_A = "moderate", loadValuesX_B = "moderate",
                          residCorrValuesX = "moderate", iccX = "weak",
                          nItemsY = 5,
                          loadValuesY_A = "weak", loadValuesY_B = "mixed",
                          residCorrValuesY = "mixed", iccY = "strong",
                          actorA = "moderate", actorB = "very strong",
                          partnerA = "weak", partnerB = "weak")

  expect_equal(runDySim(seed = 125,
                        popMod = "L-APIM",
                        popModList = popModList.apim,
                        sampSize = 100,
                        sampMod = "O-APIM",
                        nSims = 3,
                        output = "modelFit")$sim.dat,
               structure(list(sim_num = c(1, 1, 1), pop_mod = c("L-APIM", "L-APIM",
                                                                "L-APIM"), samp_mod = c("O-APIM", "O-APIM", "O-APIM"), samp_n = c(100,
                                                                                                                                  100, 100), cfi = c(1, 1, 1), tli = c(1, 1, 1), aic = c(857.80648099422,
                                                                                                                                                                                         858.064362985424, 874.707765988494), bic = c(883.8581828541,
                                                                                                                                                                                                                                      884.116064845305, 900.759467848375), rmsea = c(0, 0, 0), srmr = c(4.88059531700345e-09,
                                                                                                                                                                                                                                                                                                        2.71216224481394e-09, 7.07544218088821e-09)), row.names = c("fitMeasures...1",
                                                                                                                                                                                                                                                                                                                                                                    "fitMeasures...2", "fitMeasures...3"), class = "data.frame")
               )
})
