(ns dashboard.countries)

(defrecord Country [id code name delta-s])

(def ^:private country-id {"AF" 4
                           "AX" 248
                           "AL" 8
                           "DZ" 12
                           "AS" 16
                           "AD" 20
                           "AO" 24
                           "AI" 660
                           "AQ" 10
                           "AG" 28
                           "AR" 32
                           "AM" 51
                           "AW" 533
                           "AU" 36
                           "AT" 40
                           "AZ" 31
                           "BS" 44
                           "BH" 48
                           "BD" 50
                           "BB" 52
                           "BY" 112
                           "BE" 56
                           "BZ" 84
                           "BJ" 204
                           "BM" 60
                           "BT" 64
                           "BO" 68
                           "BQ" 535
                           "BA" 70
                           "BW" 72
                           "BV" 74
                           "BR" 76
                           "IO" 86
                           "BN" 96
                           "BG" 100
                           "BF" 854
                           "BI" 108
                           "CV" 132
                           "KH" 116
                           "CM" 120
                           "CA" 124
                           "KY" 136
                           "CF" 140
                           "TD" 148
                           "CL" 152
                           "CN" 156
                           "CX" 162
                           "CC" 166
                           "CO" 170
                           "KM" 174
                           "CG" 178
                           "CD" 180
                           "CK" 184
                           "CR" 188
                           "CI" 384
                           "HR" 191
                           "CU" 192
                           "CW" 531
                           "CY" 196
                           "CZ" 203
                           "DK" 208
                           "DJ" 262
                           "DM" 212
                           "DO" 214
                           "EC" 218
                           "EG" 818
                           "SV" 222
                           "GQ" 226
                           "ER" 232
                           "EE" 233
                           "SZ" 748
                           "ET" 231
                           "FK" 238
                           "FO" 234
                           "FJ" 242
                           "FI" 246
                           "FR" 250
                           "GF" 254
                           "PF" 258
                           "TF" 260
                           "GA" 266
                           "GM" 270
                           "GE" 268
                           "DE" 276
                           "GH" 288
                           "GI" 292
                           "GR" 300
                           "GL" 304
                           "GD" 308
                           "GP" 312
                           "GU" 316
                           "GT" 320
                           "GG" 831
                           "GN" 324
                           "GW" 624
                           "GY" 328
                           "HT" 332
                           "HM" 334
                           "VA" 336
                           "HN" 340
                           "HK" 344
                           "HU" 348
                           "IS" 352
                           "IN" 356
                           "ID" 360
                           "IR" 364
                           "IQ" 368
                           "IE" 372
                           "IM" 833
                           "IL" 376
                           "IT" 380
                           "JM" 388
                           "JP" 392
                           "JE" 832
                           "JO" 400
                           "KZ" 398
                           "KE" 404
                           "KI" 296
                           "KP" 408
                           "KR" 410
                           "KW" 414
                           "KG" 417
                           "LA" 418
                           "LV" 428
                           "LB" 422
                           "LS" 426
                           "LR" 430
                           "LY" 434
                           "LI" 438
                           "LT" 440
                           "LU" 442
                           "MO" 446
                           "MG" 450
                           "MW" 454
                           "MY" 458
                           "MV" 462
                           "ML" 466
                           "MT" 470
                           "MH" 584
                           "MQ" 474
                           "MR" 478
                           "MU" 480
                           "YT" 175
                           "MX" 484
                           "FM" 583
                           "MD" 498
                           "MC" 492
                           "MN" 496
                           "ME" 499
                           "MS" 500
                           "MA" 504
                           "MZ" 508
                           "MM" 104
                           "NA" 516
                           "NR" 520
                           "NP" 524
                           "NL" 528
                           "NC" 540
                           "NZ" 554
                           "NI" 558
                           "NE" 562
                           "NG" 566
                           "NU" 570
                           "NF" 574
                           "MK" 807
                           "MP" 580
                           "NO" 578
                           "OM" 512
                           "PK" 586
                           "PW" 585
                           "PS" 275
                           "PA" 591
                           "PG" 598
                           "PY" 600
                           "PE" 604
                           "PH" 608
                           "PN" 612
                           "PL" 616
                           "PT" 620
                           "PR" 630
                           "QA" 634
                           "RE" 638
                           "RO" 642
                           "RU" 643
                           "RW" 646
                           "BL" 652
                           "SH" 654
                           "KN" 659
                           "LC" 662
                           "MF" 663
                           "PM" 666
                           "VC" 670
                           "WS" 882
                           "SM" 674
                           "ST" 678
                           "SA" 682
                           "SN" 686
                           "RS" 688
                           "SC" 690
                           "SL" 694
                           "SG" 702
                           "SX" 534
                           "SK" 703
                           "SI" 705
                           "SB" 90
                           "SO" 706
                           "ZA" 710
                           "GS" 239
                           "SS" 728
                           "ES" 724
                           "LK" 144
                           "SD" 729
                           "SR" 740
                           "SJ" 744
                           "SE" 752
                           "CH" 756
                           "SY" 760
                           "TW" 158
                           "TJ" 762
                           "TZ" 834
                           "TH" 764
                           "TL" 626
                           "TG" 768
                           "TK" 772
                           "TO" 776
                           "TT" 780
                           "TN" 788
                           "TR" 792
                           "TM" 795
                           "TC" 796
                           "TV" 798
                           "UG" 800
                           "UA" 804
                           "AE" 784
                           "GB" 826
                           "US" 840
                           "UM" 581
                           "UY" 858
                           "UZ" 860
                           "VU" 548
                           "VE" 862
                           "VN" 704
                           "VG" 92
                           "VI" 850
                           "WF" 876
                           "EH" 732
                           "YE" 887
                           "ZM" 894
                           "ZW" 716})

(count country-id)

(def ^:private country-name {"AF" "Afghanistan"
                             "AX" "Åland Islands"
                             "AL" "Albania"
                             "DZ" "Algeria"
                             "AS" "American Samoa"
                             "AD" "Andorra"
                             "AO" "Angola"
                             "AI" "Anguilla"
                             "AQ" "Antarctica"
                             "AG" "Antigua and Barbuda"
                             "AR" "Argentina"
                             "AM" "Armenia"
                             "AW" "Aruba"
                             "AU" "Australia"
                             "AT" "Austria"
                             "AZ" "Azerbaijan"
                             "BS" "Bahamas"
                             "BH" "Bahrain"
                             "BD" "Bangladesh"
                             "BB" "Barbados"
                             "BY" "Belarus"
                             "BE" "Belgium"
                             "BZ" "Belize"
                             "BJ" "Benin"
                             "BM" "Bermuda"
                             "BT" "Bhutan"
                             "BO" "Bolivia"
                             "BQ" "Bonaire"
                             "BA" "Bosnia and Herzegovina"
                             "BW" "Botswana"
                             "BV" "Bouvet Island"
                             "BR" "Brazil"
                             "IO" "British Indian Ocean Territory"
                             "BN" "Brunei Darussalam"
                             "BG" "Bulgaria"
                             "BF" "Burkina Faso"
                             "BI" "Burundi"
                             "CV" "Cabo Verde"
                             "KH" "Cambodia"
                             "CM" "Cameroon"
                             "CA" "Canada"
                             "KY" "Cayman Islands"
                             "CF" "Central African Republic"
                             "TD" "Chad"
                             "CL" "Chile"
                             "CN" "China"
                             "CX" "Christmas Island"
                             "CC" "Cocos (Keeling) Islands"
                             "CO" "Colombia"
                             "KM" "Comoros"
                             "CG" "Congo"
                             "CD" "Congo, Democratic Republic of the"
                             "CK" "Cook Islands"
                             "CR" "Costa Rica"
                             "CI" "Côte d'Ivoire"
                             "HR" "Croatia"
                             "CU" "Cuba"
                             "CW" "Curaçao"
                             "CY" "Cyprus"
                             "CZ" "Czechia"
                             "DK" "Denmark"
                             "DJ" "Djibouti"
                             "DM" "Dominica"
                             "DO" "Dominican Republic"
                             "EC" "Ecuador"
                             "EG" "Egypt"
                             "SV" "El Salvador"
                             "GQ" "Equatorial Guinea"
                             "ER" "Eritrea"
                             "EE" "Estonia"
                             "SZ" "Eswatini"
                             "ET" "Ethiopia"
                             "FK" "Malvinas"
                             "FO" "Faroe Islands"
                             "FJ" "Fiji"
                             "FI" "Finland"
                             "FR" "France"
                             "GF" "French Guiana"
                             "PF" "French Polynesia"
                             "TF" "French Southern Territories"
                             "GA" "Gabon"
                             "GM" "Gambia"
                             "GE" "Georgia"
                             "DE" "Germany"
                             "GH" "Ghana"
                             "GI" "Gibraltar"
                             "GR" "Greece"
                             "GL" "Greenland"
                             "GD" "Grenada"
                             "GP" "Guadeloupe"
                             "GU" "Guam"
                             "GT" "Guatemala"
                             "GG" "Guernsey"
                             "GN" "Guinea"
                             "GW" "Guinea-Bissau"
                             "GY" "Guyana"
                             "HT" "Haiti"
                             "HM" "Heard Island and McDonald Islands"
                             "VA" "Holy See"
                             "HN" "Honduras"
                             "HK" "Hong Kong"
                             "HU" "Hungary"
                             "IS" "Iceland"
                             "IN" "India"
                             "ID" "Indonesia"
                             "IR" "Iran"
                             "IQ" "Iraq"
                             "IE" "Ireland"
                             "IM" "Isle of Man"
                             "IL" "Israel"
                             "IT" "Italy"
                             "JM" "Jamaica"
                             "JP" "Japan"
                             "JE" "Jersey"
                             "JO" "Jordan"
                             "KZ" "Kazakhstan"
                             "KE" "Kenya"
                             "KI" "Kiribati"
                             "KP" "Korea, Democratic People's Republic of"
                             "KR" "Korea, Republic of"
                             "KW" "Kuwait"
                             "KG" "Kyrgyzstan"
                             "LA" "Lao People's Democratic Republic"
                             "LV" "Latvia"
                             "LB" "Lebanon"
                             "LS" "Lesotho"
                             "LR" "Liberia"
                             "LY" "Libya"
                             "LI" "Liechtenstein"
                             "LT" "Lithuania"
                             "LU" "Luxembourg"
                             "MO" "Macao"
                             "MG" "Madagascar"
                             "MW" "Malawi"
                             "MY" "Malaysia"
                             "MV" "Maldives"
                             "ML" "Mali"
                             "MT" "Malta"
                             "MH" "Marshall Islands"
                             "MQ" "Martinique"
                             "MR" "Mauritania"
                             "MU" "Mauritius"
                             "YT" "Mayotte"
                             "MX" "Mexico"
                             "FM" "Micronesia"
                             "MD" "Moldova"
                             "MC" "Monaco"
                             "MN" "Mongolia"
                             "ME" "Montenegro"
                             "MS" "Montserrat"
                             "MA" "Morocco"
                             "MZ" "Mozambique"
                             "MM" "Myanmar"
                             "NA" "Namibia"
                             "NR" "Nauru"
                             "NP" "Nepal"
                             "NL" "Netherlands"
                             "NC" "New Caledonia"
                             "NZ" "New Zealand"
                             "NI" "Nicaragua"
                             "NE" "Niger"
                             "NG" "Nigeria"
                             "NU" "Niue"
                             "NF" "Norfolk Island"
                             "MK" "North Macedonia"
                             "MP" "Northern Mariana Islands"
                             "NO" "Norway"
                             "OM" "Oman"
                             "PK" "Pakistan"
                             "PW" "Palau"
                             "PS" "Palestine, State of"
                             "PA" "Panama"
                             "PG" "Papua New Guinea"
                             "PY" "Paraguay"
                             "PE" "Peru"
                             "PH" "Philippines"
                             "PN" "Pitcairn"
                             "PL" "Poland"
                             "PT" "Portugal"
                             "PR" "Puerto Rico"
                             "QA" "Qatar"
                             "RE" "Réunion"
                             "RO" "Romania"
                             "RU" "Russian Federation"
                             "RW" "Rwanda"
                             "BL" "Saint Barthélemy"
                             "SH" "Saint Helena, Ascension and Tristan da Cunha"
                             "KN" "Saint Kitts and Nevis"
                             "LC" "Saint Lucia"
                             "MF" "Saint Martin (French part)"
                             "PM" "Saint Pierre and Miquelon"
                             "VC" "Saint Vincent and the Grenadines"
                             "WS" "Samoa"
                             "SM" "San Marino"
                             "ST" "Sao Tome and Principe"
                             "SA" "Saudi Arabia"
                             "SN" "Senegal"
                             "RS" "Serbia"
                             "SC" "Seychelles"
                             "SL" "Sierra Leone"
                             "SG" "Singapore"
                             "SX" "Sint Maarten (Dutch part)"
                             "SK" "Slovakia"
                             "SI" "Slovenia"
                             "SB" "Solomon Islands"
                             "SO" "Somalia"
                             "ZA" "South Africa"
                             "GS" "South Georgia and the South Sandwich Islands"
                             "SS" "South Sudan"
                             "ES" "Spain"
                             "LK" "Sri Lanka"
                             "SD" "Sudan"
                             "SR" "Suriname"
                             "SJ" "Svalbard and Jan Mayen"
                             "SE" "Sweden"
                             "CH" "Switzerland"
                             "SY" "Syrian Arab Republic"
                             "TW" "Taiwan"
                             "TJ" "Tajikistan"
                             "TZ" "Tanzania"
                             "TH" "Thailand"
                             "TL" "Timor-Leste"
                             "TG" "Togo"
                             "TK" "Tokelau"
                             "TO" "Tonga"
                             "TT" "Trinidad and Tobago"
                             "TN" "Tunisia"
                             "TR" "Türkiye"
                             "TM" "Turkmenistan"
                             "TC" "Turks and Caicos Islands"
                             "TV" "Tuvalu"
                             "UG" "Uganda"
                             "UA" "Ukraine"
                             "AE" "United Arab Emirates"
                             "GB" "United Kingdom"
                             "US" "United States of America"
                             "UM" "United States Minor Outlying Islands"
                             "UY" "Uruguay"
                             "UZ" "Uzbekistan"
                             "VU" "Vanuatu"
                             "VE" "Venezuela, Bolivarian Republic of"
                             "VN" "Viet Nam"
                             "VG" "Virgin Islands (British)"
                             "VI" "Virgin Islands (U.S.)"
                             "WF" "Wallis and Futuna"
                             "EH" "Western Sahara"
                             "YE" "Yemen"
                             "ZM" "Zambia"
                             "ZW" "Zimbabwe"})

(def ^private country-tz {"EH" (* 1 60 60)
                          "BV" (* 1 60 60)
                          "HM" (* 5 60 60)
                          "AF" 16200
                          "AL" 3600
                          "DZ" 3600
                          "AS" -39600
                          "AD" 3600
                          "AO" 3600
                          "AI" -14400
                          "AQ" 0
                          "AG" -14400
                          "AR" -10800
                          "AM" 14400
                          "AW" -14400
                          "AU" 39600
                          "AT" 3600
                          "AZ" 14400
                          "BS" -18000
                          "BH" 10800
                          "BD" 21600
                          "BB" -14400
                          "BY" 10800
                          "BE" 3600
                          "BZ" -21600
                          "BJ" 3600
                          "BM" -14400
                          "BT" 21600
                          "BO" -14400
                          "BQ" -14400
                          "BA" 3600
                          "BW" 7200
                          "BR" -10800
                          "IO" 21600
                          "BN" 28800
                          "BG" 7200
                          "BF" 0
                          "BI" 7200
                          "KH" 25200
                          "CM" 3600
                          "CA" -18000
                          "CV" -3600
                          "KY" -18000
                          "CF" 3600
                          "TD" 3600
                          "CL" -10800
                          "CN" 28800
                          "CX" 25200
                          "CC" 23400
                          "CO" -18000
                          "KM" 10800
                          "CG" 3600
                          "CD" 3600
                          "CK" -36000
                          "CR" -21600
                          "HR" 3600
                          "CU" -18000
                          "CW" -14400
                          "CY" 7200
                          "CZ" 3600
                          "CI" 0
                          "DK" 3600
                          "DJ" 10800
                          "DM" -14400
                          "DO" -14400
                          "EC" -18000
                          "EG" 7200
                          "SV" -21600
                          "GQ" 3600
                          "ER" 10800
                          "EE" 7200
                          "ET" 10800
                          "FK" -10800
                          "FO" 0
                          "FJ" 43200
                          "FI" 7200
                          "FR" 3600
                          "GF" -10800
                          "PF" -36000
                          "TF" 18000
                          "GA" 3600
                          "GM" 0
                          "GE" 14400
                          "DE" 3600
                          "GH" 0
                          "GI" 3600
                          "GR" 7200
                          "GL" -7200
                          "GD" -14400
                          "GP" -14400
                          "GU" 36000
                          "GT" -21600
                          "GG" 0
                          "GN" 0
                          "GW" 0
                          "GY" -14400
                          "HT" -18000
                          "VA" 3600
                          "HN" -21600
                          "HK" 28800
                          "HU" 3600
                          "IS" 0
                          "IN" 19800
                          "ID" 25200
                          "IR" 12600
                          "IQ" 10800
                          "IE" 0
                          "IM" 0
                          "IL" 7200
                          "IT" 3600
                          "JM" -18000
                          "JP" 32400
                          "JE" 0
                          "JO" 10800
                          "KZ" 18000
                          "KE" 10800
                          "KI" 50400
                          "KP" 32400
                          "KR" 32400
                          "KW" 10800
                          "KG" 21600
                          "LA" 25200
                          "LV" 7200
                          "LB" 7200
                          "LS" 7200
                          "LR" 0
                          "LY" 7200
                          "LI" 3600
                          "LT" 7200
                          "LU" 3600
                          "MO" 28800
                          "MK" 3600
                          "MG" 10800
                          "MW" 7200
                          "MY" 28800
                          "MV" 18000
                          "ML" 0
                          "MT" 3600
                          "MH" 43200
                          "MQ" -14400
                          "MR" 0
                          "MU" 14400
                          "YT" 10800
                          "MX" -21600
                          "FM" 39600
                          "MD" 7200
                          "MC" 3600
                          "MN" 28800
                          "ME" 3600
                          "MS" -14400
                          "MA" 3600
                          "MZ" 7200
                          "MM" 23400
                          "NA" 7200
                          "NR" 43200
                          "NP" 20700
                          "NL" 3600
                          "NC" 39600
                          "NZ" 46800
                          "NI" -21600
                          "NE" 3600
                          "NG" 3600
                          "NU" -39600
                          "NF" 43200
                          "MP" 36000
                          "NO" 3600
                          "OM" 14400
                          "PK" 18000
                          "PW" 32400
                          "PS" 7200
                          "PA" -18000
                          "PG" 36000
                          "PY" -10800
                          "PE" -18000
                          "PH" 28800
                          "PN" -28800
                          "PL" 3600
                          "PT" 0
                          "PR" -14400
                          "QA" 10800
                          "RO" 7200
                          "RU" 10800
                          "RW" 7200
                          "RE" 14400
                          "BL" -14400
                          "SH" 0
                          "KN" -14400
                          "LC" -14400
                          "MF" -14400
                          "PM" -10800
                          "VC" -14400
                          "WS" 46800
                          "SM" 3600
                          "ST" 0
                          "SA" 10800
                          "SN" 0
                          "RS" 3600
                          "SC" 14400
                          "SL" 0
                          "SG" 28800
                          "SX" -14400
                          "SK" 3600
                          "SI" 3600
                          "SB" 39600
                          "SO" 10800
                          "ZA" 7200
                          "GS" -7200
                          "SS" 7200
                          "ES" 3600
                          "LK" 19800
                          "SD" 7200
                          "SR" -10800
                          "SJ" 3600
                          "SZ" 7200
                          "SE" 3600
                          "CH" 3600
                          "SY" 10800
                          "TW" 28800
                          "TJ" 18000
                          "TZ" 10800
                          "TH" 25200
                          "TL" 32400
                          "TG" 0
                          "TK" 46800
                          "TO" 46800
                          "TT" -14400
                          "TN" 3600
                          "TR" 10800
                          "TM" 18000
                          "TC" -18000
                          "TV" 43200
                          "UG" 10800
                          "UA" 7200
                          "AE" 14400
                          "GB" 0
                          "US" -21600
                          "UM" 43200
                          "UY" -10800
                          "UZ" 18000
                          "VU" 39600
                          "VE" -14400
                          "VN" 25200
                          "VG" -14400
                          "VI" -14400
                          "WF" 43200
                          "YE" 10800
                          "ZM" 7200
                          "ZW" 7200
                          "AX" 7200})

(def with-code 
  (memoize (fn [code]
             (try (let [id (country-id code)
                        name (country-name code)
                        delta-s (country-tz code)]
                    (Country. id code name delta-s))
                  (catch :default err
                    (println (str "ERROR trying to get country data for: " code) 
                             err)
                    nil)))))

(def all-countries (map with-code (keys country-id)))