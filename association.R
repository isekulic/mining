# DEADLINE: 20.04.
# Author: Ivan Sekulic

# Task: find interestin arules
# Important: proper deffinition of objective
# Conclusions -> how to evaluate, how to choose best arules
# Eg, supp> 0.1, conf> 0.8, rhs = 'apple'

#libray loading
Library (arules) # association rules
Library (arulesViz) # visualization of rules

########################################
# 1. Inspecting the data
########################################
Date (Groceries)
? Groceries
class (Groceries)
p (Groceries)
SUMMARY (Groceries)
inspect (head (Groceries))
structure (Groceries)

# Getting the relative support
FreqTbl = itemFrequency (Groceries, type = "relative")
head (freqTbl)
#printing only items having support> 10%
Print (freqTbl [freqTbl> 0.1])

########################################
# 2. Frequent itemsets -> still playing
########################################
# How to find good params?
AParam = new ("APparameter", "confidence" = 0.01, "support" = 0.01, "minlen" = 2)
# A class with params
print (camera)
AParam @ target = "frequent itemsets"

? apriori
Asets <-apriori (Groceries, aParam)
#analysis of discovered frequent itemsets
length (asets)
SUMMARY (asets)
Inspect (head (sort (asets, by = "support"), 10)
inspect (asets [size (asets)> 3])

########################################
# 3. Discover arules -> real deal
########################################
# Doing a grid search to find the best params
# I began with, supports and confs with [0.01, 0.1]
# After first run, it was obvious that some params are
# Not producing any rules. 
# So, i've constructed this lists which produce
# Some rules. After printing and examining the rules,
# I can decide on the best parameters

Supps = list (0.01, 0.05)
Confs = list (0.1)
Lifts = list (1,2)
Minlens = list (2,3,3)
I = 1

For (s in supps) {
  For (c in confs) {
    For (l in lifts) {
      For (ml in minlens) {
        Cat ("######### iteration ###########", i, '\ n')
        sink ( "/ dev / null")
        AParam = new ("APparameter", "confidence" = c, "support" = s, "minlen" = ml, target = "rules") 
        ARules <- apriori (Groceries, aParam)
        RulesLift <- subset (aRules, subset = lift> l)
        sink ()
        
        #print (camera)
        Cat ("confidence =", c, '\ n')
        Cat ("support =", s, '\ n')
        Cat ("lift =", l, '\ n')
        Cat ("length (aRules) =", length (aRules), '\ n')
        Cat ("length (rulesLift) =", length (rulesLift), '\ n')
  
        # Sink ('results.txt')
        
        # Filtering rules - selection of interesting rules
        # When I was testing, I've observed more rules, but
        # I have to admit, I am not sure how to tell if those
        # Are good rules
        # Trying out by = 'lift' and by = 'support' mostly
        Print (inspect (sort (ruleLift, by = "lift"), 10)))
        
        #sink ()
        I = i + 1
      }
    }
  }
}

# CONCLUSION:
# What i found out is that smallest diviations in params,
# Produce drasticaly different number of rules, hence rules of 
# Different quality. In my opinion, lift param is very important
# If we want to find interesting rules that would not come up just 
# By basic frequencies. For example, if we look at iteration 4:
# C = 0.1, s = 0.01, ml = 3
# We can see rule with highst support is:
# {Root vegetables, whole milk} => {other vegetables}
# Which is pretty general and would not help a potential groceries store owner
# With the same params, rule with the highest lift is:
# {Whole milk, yogurt} => {curd}
# It is much more specific, even tho all items are of the same type.
# Mentioned parametars produced 96 rules, which sounds like a reasonable amount.



#closeAllConnections ()
#rm (list = ls ())
