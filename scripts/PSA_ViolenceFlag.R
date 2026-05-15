library(readr)
library(dplyr)
library(stringr)

# ------------------------------------------------------------
# 1. Read in recidivism / supervision event outcome data
# ------------------------------------------------------------

x <- read_csv("pstat-197-data.git/slo_recidivism_for_SupEvents_after_7-1-2021_sent.csv")
# ------------------------------------------------------------
# 2. filtering fields
# ------------------------------------------------------------

x %>% count(REFERRAL_TYPE, sort = TRUE)
x %>% count(Case_Close_Status, sort = TRUE)
x %>% count(SupFinal_Result, sort = TRUE)
# ------------------------------------------------------------
# 3. Filter to PSA/pretrial population
#    - only pretrial monitoring cases
#    - only closed cases
#    - must have a supervision end date
# ------------------------------------------------------------
pretrial_x <- x %>%
  filter(REFERRAL_TYPE == "Pretrial") %>%
  filter(Case_Close_Status == "Y") %>%
  filter(!is.na(SupEvent_End), SupEvent_End != "")
# ------------------------------------------------------------
# 4. Inspect charge structure
#    EVTIDs repeat, so this file is charge-level / many-rows-per-event.
# ------------------------------------------------------------
pretrial_x %>%
  select(EVTID, MNID, vioDate_During_Sup, vio_Charge_During_Sup) %>%
  filter(!is.na(vio_Charge_During_Sup), vio_Charge_During_Sup != "") %>%
  count(EVTID, sort = TRUE) %>%
  head(20)
pretrial_x %>%
  select(vio_Charge_During_Sup) %>%
  filter(!is.na(vio_Charge_During_Sup), vio_Charge_During_Sup != "") %>%
  mutate(prefix = sub("^([A-Za-z]+).*", "\\1", vio_Charge_During_Sup)) %>%
  count(prefix, sort = TRUE)
# ------------------------------------------------------------
# 5. Create row-level violent charge flag
#    A charge row is flagged if it matches the SLO violent offense list.
#    NF is ignored / treated as nonviolent.
#
#    Note:
#    This uses county-style compressed charge codes, e.g.
#    PC2735A = PC 273.5(a), PC245A1 = PC 245(a)(1).
# ------------------------------------------------------------
x_vio <- pretrial_x %>%
  mutate(
    charge_raw = str_to_upper(str_trim(vio_Charge_During_Sup)),
    charge = str_replace(charge_raw, "-F$", ""),
    
    violent_row_flag =
      !is.na(charge) &
      charge != "" &
      charge != "NF" &
      (
        # PC 69
        str_detect(charge, "^PC69($|[^0-9])") |
          
          # PC 136.1(c)(1)
          str_detect(charge, "^PC1361C1") |
          
          # PC 140(a)
          str_detect(charge, "^PC140A") |
          
          # PC 148(b), 148(c), 148(d), 148.10(a)
          str_detect(charge, "^PC148B") |
          str_detect(charge, "^PC148C") |
          str_detect(charge, "^PC148D") |
          str_detect(charge, "^PC14810A") |
          
          # PC 149
          str_detect(charge, "^PC149($|[^0-9])") |
          
          # PC 151
          str_detect(charge, "^PC151($|[^0-9])") |
          
          # PC 186.26(c)
          str_detect(charge, "^PC18626C") |
          
          # PC 187(a)
          str_detect(charge, "^PC187A") |
          
          # PC 191.5(a)
          str_detect(charge, "^PC1915A") |
          
          # PC 192(a), 192(b), 192(c)(1), 192(c)(3)
          str_detect(charge, "^PC192A") |
          str_detect(charge, "^PC192B") |
          str_detect(charge, "^PC192C1") |
          str_detect(charge, "^PC192C3") |
          
          # PC 192.5(a), 192.5(b), 192.5(c)
          str_detect(charge, "^PC1925A") |
          str_detect(charge, "^PC1925B") |
          str_detect(charge, "^PC1925C") |
          
          # PC 203, 205, 206
          # Mayhem, aggravated mayhem, torture
          str_detect(charge, "^PC203($|[^0-9])") |
          str_detect(charge, "^PC205($|[^0-9])") |
          str_detect(charge, "^PC206($|[^0-9])") |

          # PC 207(a)-(d)
          # Kidnapping offenses
          str_detect(charge, "^PC207A") |
          str_detect(charge, "^PC207B") |
          str_detect(charge, "^PC207C") |
          str_detect(charge, "^PC207D") |
          # PC 208(b)
          # Kidnap child under 14
          str_detect(charge, "^PC208B") |
          
          # PC 209(a), 209(b)(1), 209.5(a)
          # Kidnapping for ransom / robbery / carjacking
          str_detect(charge, "^PC209A") |
          str_detect(charge, "^PC209B1") |
          str_detect(charge, "^PC2095A") |
          
          # PC 211
          # Robbery
          str_detect(charge, "^PC211($|[^0-9])") |
          
          # PC 214
          # Train robbery
          str_detect(charge, "^PC214($|[^0-9])") |
          
          # PC 215
          # Carjacking
          # Use safer match so PC215 does not accidentally match PC21510B
          str_detect(charge, "^PC215($|[^0-9])") |
          
          # PC 217.1(a), 217.1(b)
          # Assault / attempted murder of public official
          str_detect(charge, "^PC2171A") |
          str_detect(charge, "^PC2171B") |
          
          # PC 218, 218.1
          # Train wrecking / obstructing railroad track
          str_detect(charge, "^PC218($|[^0-9])") |
          str_detect(charge, "^PC2181") |
          
          # PC 219, 219.1
          # Train derailing / throwing missile at common carrier
          str_detect(charge, "^PC219($|[^0-9])") |
          str_detect(charge, "^PC2191") |
          
          # PC 220
          # All 220 offenses included
          str_detect(charge, "^PC220") |
          
          # PC 222
          # Administering chloroform/controlled substance/etc.
          str_detect(charge, "^PC222($|[^0-9])") |
          
          # PC 236.1(b), 236.1(c)
          # Human trafficking; only added subdivisions b/c per list
          str_detect(charge, "^PC2361B") |
          str_detect(charge, "^PC2361C") |
          
          # PC 237(a)
          # False imprisonment
          str_detect(charge, "^PC237A") |
          
          # PC 240
          # Assault
          str_detect(charge, "^PC240($|[^0-9])") |
          
          # PC 241, 241.1-241.8(a)
          # Assault-related offenses
          str_detect(charge, "^PC241($|[^0-9])") |
          str_detect(charge, "^PC2411") |
          str_detect(charge, "^PC2412") |
          str_detect(charge, "^PC2413") |
          str_detect(charge, "^PC2414") |
          str_detect(charge, "^PC2415") |
          str_detect(charge, "^PC2416") |
          str_detect(charge, "^PC2417") |
          str_detect(charge, "^PC2418A") |
          
          # PC 242
          # Battery
          str_detect(charge, "^PC242($|[^0-9])") |
          
          # PC 243
          str_detect(charge, "^PC243($|[^0-9A-Z])") |  # plain PC243 only, if listed as "243 Battery"
          str_detect(charge, "^PC2431") |              # 243.1
          str_detect(charge, "^PC2432A1") |            # 243.2(a)(1)
          str_detect(charge, "^PC24325") |             # 243.25
          str_detect(charge, "^PC2433") |              # 243.3
          str_detect(charge, "^PC24335") |             # 243.35
          str_detect(charge, "^PC2434") |              # 243.4
          str_detect(charge, "^PC2435A1") |            # 243.5(a)(1)
          str_detect(charge, "^PC2436") |              # 243.6
          str_detect(charge, "^PC24365A") |            # 243.65(a)
          str_detect(charge, "^PC2437") |              # 243.7
          str_detect(charge, "^PC2438A") |             # 243.8(a)
          str_detect(charge, "^PC2439A") |             # 243.9(a)
          
          # PC 244 and 244.5
          str_detect(charge, "^PC244($|[^0-9])") |     # 244
          str_detect(charge, "^PC2445B") |             # 244.5(b)
          str_detect(charge, "^PC2445C") |             # 244.5(c)
          
          # PC 245 assault offenses
          # Specific listed 245 subdivisions
          str_detect(charge, "^PC245A1") |             # 245(a)(1)
          str_detect(charge, "^PC245A2") |             # 245(a)(2)
          str_detect(charge, "^PC245A3") |             # 245(a)(3)
          str_detect(charge, "^PC245A4") |             # 245(a)(4)
          str_detect(charge, "^PC245B") |              # 245(b)
          str_detect(charge, "^PC245C") |              # 245(c)
          str_detect(charge, "^PC245D1") |             # 245(d)(1)
          str_detect(charge, "^PC245D2") |             # 245(d)(2)
          str_detect(charge, "^PC245D3") |             # 245(d)(3)
          str_detect(charge, "^PC2452") |              # 245.2
          str_detect(charge, "^PC2453") |              # 245.3
          str_detect(charge, "^PC2455A") |             # 245.5(a)
          str_detect(charge, "^PC2455B") |             # 245.5(b)
          str_detect(charge, "^PC2455C") |             # 245.5(c)
          str_detect(charge, "^PC2456") |             # 245.6
          
          # PC 261(a)
          # Rape
          str_detect(charge, "^PC261A") |
          
          # PC 261.5(a)-(d)
          # Unlawful sex with minor
          str_detect(charge, "^PC2615A") |
          str_detect(charge, "^PC2615B") |
          str_detect(charge, "^PC2615C") |
          str_detect(charge, "^PC2615D") |
          
          # PC 262(a)(1)-(6)
          # Rape of spouse
          str_detect(charge, "^PC262A1") |
          str_detect(charge, "^PC262A2") |
          str_detect(charge, "^PC262A3") |
          str_detect(charge, "^PC262A4") |
          str_detect(charge, "^PC262A5") |
          str_detect(charge, "^PC262A6") |
          
          # PC 264.1
          # Rape/etc. in concert by force/violence
          str_detect(charge, "^PC2641") |
          
          # PC 266a, 266b, 266c
          # Taking/abduction/sex act by fear or fraud
          str_detect(charge, "^PC266A") |
          str_detect(charge, "^PC266B") |
          str_detect(charge, "^PC266C") |
          
          # PC 266h(b), 266i(b), 266j
          # Minor pimping/pandering/procurement
          str_detect(charge, "^PC266HB") |
          str_detect(charge, "^PC266IB") |
          str_detect(charge, "^PC266J") |
          
          # PC 267
          # Abduction of person under 18 for prostitution
          str_detect(charge, "^PC267($|[^0-9])") |
          
          # PC 269(a)
          # Aggravated sexual assault of minor
          str_detect(charge, "^PC269A") |
          
          # PC 273.4
          # Female genital mutilation
          str_detect(charge, "^PC2734") |
          
          # PC 273.5(a), 273.5(f)
          # Corporal injury to spouse/cohabitant/etc.
          str_detect(charge, "^PC2735A") |
          str_detect(charge, "^PC2735F") |
          
          # PC 273.6(b), 273.6(d)
          # DV court order violation with injury / violence or credible threat
          # Important: do NOT broadly match PC2736, because PC2736A is not listed here.
          str_detect(charge, "^PC2736B") |
          str_detect(charge, "^PC2736D") |
          
          # PC 273a(a), 273a(b)
          # Child cruelty
          str_detect(charge, "^PC273AA") |
          str_detect(charge, "^PC273AB?$") |
          
          # PC 273ab(a), 273ab(b)
          # Assault on child under 8 causing death/serious injury
          str_detect(charge, "^PC273ABA") |
          str_detect(charge, "^PC273ABB") |
          
          # PC 273d(a)
          # Inflict injury upon child
          str_detect(charge, "^PC273DA") |
          
          # PC 285
          # Incest
          str_detect(charge, "^PC285($|[^0-9])") |

          # PC 286
          # Sodomy - all 286 offenses
          str_detect(charge, "^PC286") |
          
          # PC 287
          # Oral copulation - all 287 offenses
          str_detect(charge, "^PC287") |
          
          # PC 288
          # Lewd/lascivious acts with child - all 288 offenses
          # This will also catch PC2885 and PC2887 formats if coded without decimal.
          str_detect(charge, "^PC288") |
          
          # PC 288a
          # Oral copulation - all 288a offenses
          str_detect(charge, "^PC288A") |
          
          # PC 289
          # Sexual penetration with force/etc.
          str_detect(charge, "^PC289($|[^0-9])") |
          
          # PC 289.6(a)(3)
          # Sex by employee/etc. in confinement/detention facility
          str_detect(charge, "^PC2896A3") |
          
          # PC 311.4
          # Using minors for sex acts - all 311.4 offenses
          str_detect(charge, "^PC3114") |
          
          # PC 347(a)
          # Poisoning food/drink/medicine/etc.
          str_detect(charge, "^PC347A") |
          
          # PC 368(b), 368(c), 368(f)
          # Elder/dependent adult harm/cruelty/false imprisonment by violence
          str_detect(charge, "^PC368B") |
          str_detect(charge, "^PC368C") |
          str_detect(charge, "^PC368F") |
          
          # PC 404(a)
          # Rioting
          str_detect(charge, "^PC404A") |
          
          # PC 417(a), 417(b), 417(c)
          # Brandishing/exhibiting weapon/firearm
          str_detect(charge, "^PC417A") |
          str_detect(charge, "^PC417B") |
          str_detect(charge, "^PC417C") |
          
          # PC 417.3, 417.6, 417.8
          str_detect(charge, "^PC4173") |
          str_detect(charge, "^PC4176") |
          str_detect(charge, "^PC4178") |
          
          # PC 422.6(a)
          # Civil rights violation by force/threat
          str_detect(charge, "^PC4226A") |
          
          # PC 451
          # Arson - all 451 offenses
          # This also catches PC4511 and PC4515 because all 451 offenses are included.
          str_detect(charge, "^PC451") |
          
          # PC 452(a)
          # Causing fire that causes GBI
          str_detect(charge, "^PC452A") |
          
          # PC 455
          # Arson attempts / preliminary acts
          str_detect(charge, "^PC455($|[^0-9])") |
          
          # PC 646.9(a), 646.9(b)
          # Stalking / stalking with restraining order
          str_detect(charge, "^PC6469A") |
          str_detect(charge, "^PC6469B") |
          
          # PC 647.6(a)(1), 647.6(b), 647.6(c)
          # Annoy/molest child
          str_detect(charge, "^PC6476A1") |
          str_detect(charge, "^PC6476B") |
          str_detect(charge, "^PC6476C") |
          
          # PC 667.61
          # Felony sex offenses - all 667.61 offenses
          str_detect(charge, "^PC66761") |
          # PC 667.8, 667.85
          # Kidnapping to commit sex offense / deprive parent
          str_detect(charge, "^PC6678($|[^0-9])") |
          str_detect(charge, "^PC66785") |
          
          # PC 674
          # Sex offense by daycare provider
          str_detect(charge, "^PC674($|[^0-9])") |
          
          # PC 836.6(c)
          # Escape from custody by force or violence
          str_detect(charge, "^PC8366C") |
          
          # PC 4500, 4501
          # Assault by life prisoner / state prisoner
          str_detect(charge, "^PC4500($|[^0-9])") |
          str_detect(charge, "^PC4501($|[^0-9])") |
          
          # PC 4501.1(a), 4501.5
          # Aggravated battery / battery by prisoner
          str_detect(charge, "^PC45011A") |
          str_detect(charge, "^PC45015") |
          
          # PC 4503
          # Holding hostages
          str_detect(charge, "^PC4503($|[^0-9])") |
          
          # PC 4530(a)
          # Escape from custody by force/violence
          str_detect(charge, "^PC4530A") |
          
          # PC 4532(a)(2), 4532(b)(2)
          # Escape from alternative custody by force/violence
          str_detect(charge, "^PC4532A2") |
          str_detect(charge, "^PC4532B2") |
          
          # PC 11413(a), 11413(b)
          # Terrorism by explosion
          str_detect(charge, "^PC11413A") |
          str_detect(charge, "^PC11413B") |
          
          # PC 11418(b), 11418(c), 11418(d)
          # Weapons of mass destruction
          str_detect(charge, "^PC11418B") |
          str_detect(charge, "^PC11418C") |
          str_detect(charge, "^PC11418D") |
          
          # PC 18740, 18745, 18750, 18755
          # Destructive device / explosion offenses
          str_detect(charge, "^PC18740($|[^0-9])") |
          str_detect(charge, "^PC18745($|[^0-9])") |
          str_detect(charge, "^PC18750($|[^0-9])") |
          str_detect(charge, "^PC18755($|[^0-9])") |
          
          # PC 26100(c)
          # Discharge firearm at another person from motor vehicle
          str_detect(charge, "^PC26100C") |
          
          # PC 664/187(a), 664/211
          # Attempted murder / attempted robbery
          # Handles formats like PC664-PC187A, PC664/187A, PC664187A, PC664-PC211
          str_detect(charge, "^PC664[-/]?PC?187A") |
          str_detect(charge, "^PC664[-/]?PC?211") |
          
          # Election Code 18540
          # Use of firearm to intimidate voter
          str_detect(charge, "^EC18540($|[^0-9])") |
          str_detect(charge, "^ELEC18540($|[^0-9])") |
          
          # Vehicle Code 2800.3(a), 2800.3(b)
          # SBI/death caused by flight from peace officer
          str_detect(charge, "^VC28003A") |
          str_detect(charge, "^VC28003B")
          
        
        
      )
  )

# ------------------------------------------------------------
# 6. QA: Review row-level charge matching
# ------------------------------------------------------------

# Charges that were flagged as violent
flagged_charge_counts <- x_vio %>%
  filter(violent_row_flag) %>%
  count(charge, sort = TRUE)

flagged_charge_counts %>%
  print(n = 100)

# Charges that were NOT flagged as violent
unflagged_charge_counts <- x_vio %>%
  filter(!violent_row_flag) %>%
  count(charge, sort = TRUE)

unflagged_charge_counts %>%
  print(n = 150)

# Check NF rows specifically
nf_rows <- x_vio %>%
  filter(charge == "NF") %>%
  select(EVTID, MNID, vioDate_During_Sup, vio_Charge_During_Sup, charge)

nf_rows %>%
  head(30)


# ------------------------------------------------------------
# 7. Collapse from charge-level rows to event-level outcome
#    Final outcome:
#    violent_rearrest_during_sup = 1 if any charge for EVTID is violent
#    violent_rearrest_during_sup = 0 otherwise
# ------------------------------------------------------------

violent_evt <- x_vio %>%
  group_by(EVTID, MNID) %>%
  summarise(
    violent_rearrest_during_sup = as.integer(any(violent_row_flag, na.rm = TRUE)),
    n_charge_rows = n(),
    n_violent_charge_rows = sum(violent_row_flag, na.rm = TRUE),
    .groups = "drop"
  )


# ------------------------------------------------------------
# 8. Final outcome summary
# ------------------------------------------------------------

violent_summary <- violent_evt %>%
  count(violent_rearrest_during_sup) %>%
  mutate(
    outcome = ifelse(
      violent_rearrest_during_sup == 1,
      "Violent rearrest",
      "No violent rearrest"
    ),
    percent = round(100 * n / sum(n), 1)
  ) %>%
  select(outcome, n, percent)

violent_summary


# ------------------------------------------------------------
# 9. QA: Event-level examples coded as violent
# ------------------------------------------------------------

violent_evt %>%
  filter(violent_rearrest_during_sup == 1) %>%
  arrange(desc(n_violent_charge_rows)) %>%
  print(n = 50)

x_vio %>%
  filter(violent_row_flag) %>%
  select(EVTID, MNID, vioDate_During_Sup, vio_Charge_During_Sup, charge) %>%
  arrange(EVTID, vioDate_During_Sup) %>%
  print(n = 50)


# ------------------------------------------------------------
# 10. Export final event-level violence outcome
# ------------------------------------------------------------

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

write_csv(
  violent_evt,
  "data/processed/violent_rearrest_during_sup_by_evtid.csv"
)


# ------------------------------------------------------------
# 11. Create poster figures
# ------------------------------------------------------------

library(ggplot2)

dir.create("figures", recursive = TRUE, showWarnings = FALSE)

# Chart 1: Violent rearrest outcome counts
p_violence_outcome <- ggplot(violent_summary, aes(x = outcome, y = n)) +
  geom_col() +
  geom_text(
    aes(label = paste0(n, " (", percent, "%)")),
    vjust = -0.3
  ) +
  labs(
    title = "PSA Violence Outcome: Rearrest for Violent Offense",
    x = "",
    y = "Number of closed pretrial events"
  ) +
  theme_minimal()

p_violence_outcome

ggsave(
  "figures/psa_violence_outcome_counts.png",
  p_violence_outcome,
  width = 7,
  height = 5
)

# Chart 2: Most common flagged violent charge codes
common_violent_charges <- flagged_charge_counts %>%
  head(8)

common_violent_charges

p_common_charges <- ggplot(common_violent_charges, aes(x = reorder(charge, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Most Common Flagged Violent Charge Codes",
    x = "Charge code",
    y = "Charge rows"
  ) +
  theme_minimal()

p_common_charges

ggsave(
  "figures/common_violent_charge_codes.png",
  p_common_charges,
  width = 7,
  height = 5
)