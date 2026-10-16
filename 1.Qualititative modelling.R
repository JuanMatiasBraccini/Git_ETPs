# Script for modelling ETP interactions in TDGDLF ----------------------------------------------------------------
#note: This is a classic ecosystem-to-socioeconomic management dilemma
#      A mix of biological interactions and social factors (human safety) with unquantified components,
#       hence use Loop Analysis (Signed Digraphs) and Fuzzy Cognitive Maps (FCMs).
#     Fuzzy Cognitive Maps allow you to add relative weights (fuzzy values between -1 and 1) to represent your data-poor knowledge 
#      Use 6 system variables: 
#           ASL (Australian Sea Lions),
#           WS (White Sharks),
#           FISH (Shark Fishery Effort),
#           HUM (Human Presence / Exposure)
#           ATT (Shark Attacks, i.e., the inverse of human safety)
#           OP (Other Prey, Whales, scalefish, etc.)
#           DET non-lethal deterrents

# Fishery Direct Impacts:
#   FISH ---> ASL: Negative due to incidental catch.
#   FISH ---> WS: Negative due to target/incidental catch (~30/year).
#   FISH ---> ATT: Negative because fishing effort removes sharks from areas where humans are, directly reducing attacks.
# Shark-Prey Dynamics:
#   ASL ---> WS: Positive via predation/energy transfer.
#   OP ---> WS Positive: More whales/scalefish provide an influx of energy, increasing White Shark productivity
#   WS ---> ASL: Negative  via predation mortality.
#   WS ---> OP (-): White Sharks feed on and suppress these other prey species
#   OP ---> OP (-): Added to represent natural carrying capacity limits.
# Human & Attack Dynamics:
#   WS ---> ATT: Positive because more sharks mean a higher probability of attacks.
#   HUM ---> ATT: Positive  because more humans in the water increase exposure.
# Self-Limitation (Diagonal Elements):
#   We assume biological variables (ASL, WS) have self-limitation (-) due to carrying capacities or territoriality. 
#   HUM and FISH can be modeled as externally controlled (0 or -) depending on management setup).
# Press scenario
#   To capture the real-world observation that whales and scalefish have increased recently, apply a positive "press" 
#     or influx directly to the OP variable to see how it cascades
# intriguing conflict built in:
#   The Direct Pathway: Increasing FISH directly reduces ATT (fewer sharks locally ---> fewer attacks)
#   The Indirect Pathway: Increasing FISH suppresses ASL, which in turn starves/reduces WS, which also reduces ATT

# Header-------------------------------------------------------------------------
library(LoopAnalyst)
library(FCMapper) #remotes::install_github("cran/FCMapper")
library(DiagrammeR)

# Loop Analysis-------------------------------------------------------------------------

#1. Community matrix
Prop.ASL.in.WS.diet=0.3   
labels <- c("ASL", "WS", "FISH", "HUM", "ATT", "OP","DET")
CM <- matrix(c(
            #  ASL    WS  FISH   HUM   ATT    OP   DET
            -1,    1,    0,    0,    0,    0,    0,  # ASL row: self-limiting (-), feeds WS (+)
            -Prop.ASL.in.WS.diet,   -1,    0,    0,    1,   -(1-Prop.ASL.in.WS.diet),    0,  # WS row: eats ASL (-), self-limiting (-), attacks hum (+), eats OP (-)
            -1,   -1,   -1,    0,   -1,    -1,    0,  # FISH row: bycatch ASL and WS (-), catches OP (-), deters attacks (-), Fish self-limiting due to management regulations (-1)
            0,    0,    0,   -1,    1,    0,    0,  # HUM row: increases exposure/attacks (+), HUM self-limiting (-1)
            0,    0,    0,    0,   -1,    0,    0,  # ATT row: self-limiting feedback (-)
            0,    1,    0,    0,    0,   -1,    0,  # OP row: feeds WS (+), self-limiting (-)
            0,    0,    0,    0,    0,    0,   -1   # DET row: non-lethal deterrents (Off), self-limiting due to budget constraints (-)
          ),
          nrow = length(labels), byrow = TRUE, dimnames = list(labels, labels))

#check weighted predictions to see if all paths can be resolved
weighted.predictions(CM)  #any '?'

#2. List of alternative state of nature    #missing: try alternative states, e.g ASL positive effect on HUM
Scenario.description=data.frame(Scenario=c('S1','S2','S3', 'S4'),
                                Description=c('base case',
                                              'weakened FISH-->ASL to deal with NA',
                                              'fishery closure protecting ASL',
                                              'use of non-lethal shark deterrents'))
Scenarios=list(CM, CM, CM, CM)   
names(Scenarios)=Scenario.description$Scenario
Scenarios$S2[match('FISH',rownames(Scenarios$S2)), match('ASL',colnames(Scenarios$S2))]=-0.1
Scenarios$S3[match('FISH',rownames(Scenarios$S3)), match('ASL',colnames(Scenarios$S3))]=0
Scenarios$S4[match('DET',rownames(Scenarios$S4)), match('ATT',colnames(Scenarios$S4))]=-1


#3. Compute the Adjoint Matrix to see raw magnitudes
#note: rows are the causes and columns are the effects (ie ecosystem results)
Adjoint_Results <- lapply(Scenarios, function(x) make.adjoint(x))

#4. Compute net Community Effect Matrix
#note: rows are the causes and columns are the effects (ie ecosystem results)
CME_Results <- lapply(Scenarios, function(x) t(make.cem(x)))

#Interpretation of S1 (Adjoint_Results/CME_Results)
# The OP row tracks what happens when alternative prey like whales and scalefish increase in the region
# • OP ---> WS (1 / 1): White sharks increase. The abundance of alternative prey acts as a major food subsidy that directly boosts the predator population.
# • OP ---> ASL (-1 / -1): Australian Sea Lions decrease. This explicitly verifies your apparent competition hypothesis. The boom in whales/scalefish expands the shark population, which cascadingly inflicts higher predation mortality on sea lions.
# • OP ---> ATT (1 / 1): Shark attacks increase. Because other prey supports a larger baseline population of white sharks, the net risk to human safety rises.

# The FISH row tracks the impact of increasing commercial shark fishing effort
# • FISH ---> WS (-2 / -1): White sharks reliably decrease. The raw pathway count of -2 highlights a strong direct and indirect depletion effect.
# • FISH ---> ATT (-5 / -1): Shark attacks drop drastically. A value of -5 indicates a highly effective reduction, driven by the direct removal of sharks coupled with the indirect effect of drawing down their sea lion prey base.
# • FISH ---> OP (2 / 1): Other prey species increase. Suppressing the top predator (sharks) releases whales and scalefish from predator control.
# • FISH ---> ASL (-1 / NA): This is your system's critical structural conflict. While the raw adjoint shows a net pathway value of -1, make.cem flags it as NA (ambiguous). This alerts you that two forces are in a dead heat: the direct negative effect of fishery bycatch vs. the indirect positive effect of sharks being removed. The structure alone cannot predict if sea lions will go up or down.

# The HUM row tracks the social footprint of more people utilizing the marine environment
# • HUM ---> ATT (3 / 1): Shark attacks reliably increase due to higher encounter probabilities.
# • HUM ---> All Others (0 / 0): Humans behave as a pure "sink". In this model structure, human presence does not possess a feedback loop that alters the biological dynamics of sharks, sea lions, or fish.

# The ASL row models what happens if you directly protect or boost sea lions (e.g., via colony-specific management)
# • ASL ---> WS (1 / -1 conflict): The raw adjoint pathways total 1, but make.cem evaluates the net sign as -1. This flip indicates complex, self-damping feedback loops between sharks and sea lions that can cause non-intuitive population shifts when sea lions are directly manipulated.
# • ASL ---> OP (-1 / -1): An increase in sea lions eventually results in a decrease in other prey. This happens because more sea lions support a larger shark population, which then exerts heavier predator pressure on whales and scalefish.

# Interpret the 'CME_Results' as 'A direct increase in row X yields a net positive/negative/nill effect on column Y

#Resolving the NA in CME_results
# In qualitative modeling, an NA occurs because there is a perfect structural tie between opposing pathways.
# The direct path Fish --> ASL says more Fishing = less ASL (direct bycatch) but the indirect path says 
#      more Fishing = more ASL thru less WS.
# Because both a negative path and a positive path occur simultaneously without any numerical magnitudes
#        to break the tie, the system experiences competing feedback pathways
        
#Resolving the NA in CME_results
#   make.wfm() calculates the ratio of positive to negative, pathways for every interaction. This allows you to evaluate  
#    how heavily weighted or  easily tipped an ambiguous relationship is
  
WFM_Results <- lapply(Scenarios, function(x) t(make.wfm(x)))
#A value of 0.3 mathematically in this system) tells you that the system is heavily conflicted, but it is not a perfect 50/50 tie
# The Pathway Ratio: In your system, there are a total of 3 active mathematical pathways connecting a press on the fishery down to the sea lion population
# The Conflict: 2 of those pathways are pulling in one direction, while 1 pathway is pulling in the opposite direction
# Net Outcome: When you subtract them (\(2 - 1 = 1\)), you are left with a net pathway of 1 unit. The weighted feedback formula divides that net result by the total number of pathways: 1/3
# any interaction that contains any level of opposing feedback (where the total pathways exceed the net absolute pathways) is automatically filtered out and flagged as NA
# For other interactions, A weighted feedback value of 1 is the cleanest possible result in qualitative modelling. It means there is zero ambiguity or structural conflict for those interactions

#missing, display and sort out the NA for FISH on ASL

# Fuzzy Cognitive Maps-------------------------------------------------------------------------
#missing: develop properly
labels_6 <- c("ASL", "WS", "FISH", "HUM", "ATT", "OP")

# Create a fuzzy cognitive matrix with fractional weights
fcm_matrix <- matrix(c(
  #  ASL    WS  FISH   HUM   ATT    OP
  -0.5,  0.6,  0.0,  0.0,  0.0,  0.0,  # ASL row
  -0.6, -0.4,  0.0,  0.0,  0.8, -0.5,  # WS row
  -0.7, -0.4, -0.9,  0.0, -0.3,  0.0,  # FISH row (Strong direct bycatch of ASL: -0.7)
  0.0,  0.0,  0.0, -0.5,  0.7,  0.0,  # HUM row
  0.0,  0.0,  0.0,  0.0, -0.6,  0.0,  # ATT row
  0.0,  0.5,  0.0,  0.0,  0.0, -0.4   # OP row
), nrow = 6, byrow = TRUE, dimnames = list(labels_6, labels_6))

# Validation step: Ensures matrix is square and bounded between -1 and 1
check.matrix(fcm_matrix)


# Plot the network map
# concept.sizes accepts a vector of 6 numbers determining the circle sizes
graph.fcm(fcm_matrix, concept.names = labels_6, concept.sizes = rep(0.5, 6))

# Generate the baseline state
baseline <- nochanges.scenario(fcm_matrix, iter = 25, concept.names = labels_6)

# View the final equilibrium vector
print(baseline)

# Run "What-If" Interventions (Scenarios)
#Let’s simulate an intervention where FISH is heavily increased/incentivised (clamped at a maximum value of 1) 
# to observe whether the direct bycatch path (-0.7) or the indirect predator-relief path wins out on ASL

# Run a policy scenario holding FISH constant at 1.0
fish_boost_scenario <- changes.scenario(
  fcm_matrix, 
  concept.names = labels_6, 
  iter = 25, 
  set.concepts = c("FISH"), 
  set.values = c(1.0)
)

# Compare the baseline and intervention scenarios side-by-side
comparison <- comp.scenarios(baseline, fish_boost_scenario)
print(comparison)
