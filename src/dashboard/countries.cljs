(ns dashboard.countries)

(defrecord Country [id code name])

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
                             "BO" "Bolivia, Plurinational State of"
                             "BQ" "Bonaire, Sint Eustatius and Saba"
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
                             "FK" "Falkland Islands (Malvinas)"
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
                             "IR" "Iran, Islamic Republic of"
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
                             "FM" "Micronesia, Federated States of"
                             "MD" "Moldova, Republic of"
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
                             "NL" "Netherlands, Kingdom of the"
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
                             "TW" "Taiwan, Province of China"
                             "TJ" "Tajikistan"
                             "TZ" "Tanzania, United Republic of"
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
                             "GB" "United Kingdom of Great Britain and Northern Ireland"
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

(def with-code 
  (memoize (fn [code]
             (try (let [id (country-id code)
                        name (country-name code)]
                    (Country. id code name))
                  (catch :default err
                    (println (str "ERROR trying to get country data for: " code) 
                             err)
                    nil)))))

(def all-countries (map with-code (keys country-id)))