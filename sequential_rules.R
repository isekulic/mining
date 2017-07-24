# Deadline 9.5. 12:00
# Author: Ivan Sekulic

setwd ( '~ / lab4');
library (arules)
library (arulesSequences)

# Download.file ('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/diab_trans_no_header.data','diab_trans_no_header.data')
################################################## ##
# About the data
################################################## ##
# Why should we diabetes date? Being one of widespread diseases, learning about it is very important.
# Data mining enables us to discover some rules / patterns that are not easily found by humans.
We can analyze diabetes data for various purpuses, like health care flow analysis, adverse drug 
# Effect analysis, clinical guidelines enrichment, and prediction of early mortality etc.
# Sequential rules carry an implicit temporal element, which can reveal more unexpected cases.
# Discovery of sequential rules can benefit patients in more than one way.
? read_baskets
DiabSeq <- read_baskets (con = "diab_trans_no_header.data", sep = ",", info = c ("sequenceID", "eventID"))

# Inspecting the data
SUMMARY (diabSeq)
Df = as (diabSeq, 'data.frame')
View (df)

################################################## ##
# Frequent sequences discovery
################################################## ##
# First step on our way to help society is discovering frequent sequences.
# How do we know our parameters are good? We do not really.
# Basic approach is to try out few different sets of parameters and evaluate the result.
We can insert some of our knowledge, or hypothesis, by directing the params in certain ways.
# Evaluation of results can be done in terms of quantity (we want a 'reasonable' amount of rules),
# And quality (hard to check in this case -> requires medical knowledge?)

# Example params
SPparameter ??
# Here we try to mine sequences with minimum 10 min gap between consecutive events, and 
# Maximum 2 days. Maximum time difference between any two elements of a sequence is set to 10 days
SeqParam = new ("SPparameter", support = 0.5, maxsize = 4, mingap = 600, maxgap = 172800, maxlen = 3; maxwin = (172800 * 5))
PatSeq = cspade (diabSeq, seqParam, control = list (verbose = TRUE, tidLists = FALSE, summary = TRUE))
SeqRules = ruleInduction (patSeq, confidence = 0.8)
# Inspecting the results
length (patSeq)
inspect (head (patSeq))
length (seqRules)
SUMMARY (seqRules)
inspect (head (seqRules, 10))
Inspect (head (sort (seqRules, by = "support"), 10)
#lhs rhs support confidence lift 
# 1 <{"id_33"}> => <{"id_33"}> 0.9242424 1.0000000 1.081967 
# 2 <{"id_33"},                   
# {"Id_33"}> => <{"id_33"}> 0.9242424 1.0000000 1.081967 
# 3 <{"id_58"}> => <{"id_58"}> 0.9090909 1.0000000 1.100000

# Trying out different parameters for gap size (None here)
SeqParam2 = new ("SPparameter", support = 0.5, maxsize = 4, maxlen = 3)
PatSeq2 = cspade (diabSeq, seqParam2, control = list (verbose = TRUE, tidLists = FALSE, summary = TRUE))
SeqRules2 = ruleInduction (patSeq2, confidence = 0.8)

length (patSeq2)
length (seqRules2)
SUMMARY (seqRules2)
Inspect (head (seqRules2, 10))
Inspect (head (sort (seqRules2, by = "support"), 10)

# Gap 1 day - 3 days
Max =
PatSeq3 = cspade (diabSeq, seqParam3, control = list (verbose = TRUE, tidLists = FALSE, summary = TRUE))
SeqRules3 = ruleInduction (patSeq3, confidence = 0.8)

length (patSeq3)
length (seqRules3)
SUMMARY (seqRules3)
Inspect (head (seqRules3, 10))
Inspect (head (sort (seqRules3, by = "support"), 10)
# We get a lot of meaningless things, but when we check only for the frequent ones, 
# We can find some good results

# Let's try and find longer seq rules
Ps3_long <- patSeq3 [size (patSeq3)> 2]
Inspect (head (sort (ps3_long, by = "support"), 10))
length (ps3_long)
# There are a lot of rules. We will not use this filter because it does not help a lot

# I tried to concatenate the data (code and value), so mining would be easier, but I failed.
# That's why we are looking for the most frequent (code, value) pairs and finding rules about em
inspect (patSeq elements @ @ items)
# 46 {"id_33", 3} is the most frequent pair. Let's see what we get for it
For333 = subset (seqRules, lhs (seqRules)% ein% c ("\\" id_33 \ "", 3)
length (for333)
Inspect (head (sort (for333, decreasing = TRUE, = "support"), 10)
#lhs rhs support confidence lift 
# 1 <{"id_33",                    
# 3}> => <{"id_33"}> 0.7575758 0.9803922 1.060752 
# 2 <{"id_33"},                   
# { "Id_33"                    
# 3}> => <{"id_33"}> 0.7575758 0.9803922 1.060752 

# Not too interesting. We can just try and find rules like before, and do not take rules like
# {8} -> {9} into account and see what happens. That way, we can find also some interesting rules,
# Not just the frequent ones

# For gap 10min - 2 days
Inspect (head (sort (seqRules, by = "support"), 10)
# 1 <{"id_33"}> => <{"id_33"}> 0.9242424 1.0000000 1.081967 
# 2 <{"id_33"},                   
# {"Id_33"}> => <{"id_33"}> 0.9242424 1.0000000 1.081967 
# 3 <{"id_58"}> => <{"id_58"}> 0.9090909 1.0000000 1.100000 
# 4 <{"id_62"}> => <{"id_58"}> 0.9090909 1.0000000 1.100000 
# The first rule tells us that people who take regular insulin doses, realy take them regulary

# Let's find some interesting ones, that point out of the others
Inspect (head (sort (seqRules, by = "lift"), 30)
#lhs rhs support confidence lift 
# 1 <{"id_33",                    
# 8},                         
# {6}> => <{"id_33", 0.5151515 1.0000000 1.885714 
# 8}>          
# ...
# 7 <{"id_58"},                   
# {"Id_64"}> => <{"id_64"}> 0.6060606 0.9756098 1.533101 
# ...
# 30 <{"id_64"},                   
# {3}> => <{"id_64"}> 0.5303030 0.9459459 1.486486

# Looking for rules for gap 1day-3days
Inspect (head (sort (seqRules3, by = "support"), 10)
Inspect (head (sort (seqRules3, by = "confidence"), 10))
# 10 <{"id_60"},                   
# { "Id_33"                    
# 7}> => <{"id_62"}> 0.5151515 1 1.100000 
# Pre lunch glucose measurment with regular insulin dose leads to ok 
# Measurment before supper

# So far, not so interesting rules. I am no expert, but I would think it would be interesting
# To find when it comes to 65 = Hypoglycemic symptoms.
Hypogly = subset (seqRules3, rhs (seqRules3)% ein% c ("\" id_65 \ ""))
length (hypogly)
# Never. Good for the people with diabetes i guess
# We can see when it occurs:
Hypogly_left = subset (seqRules3, lhs (seqRules3)% ein% c ("\" id_65 \ ""))
length (hypogly_left)
Inspect (head (hypogly_left, 10))
# We can see that it is not that often
# <{"Id_65", 0}, {"id_33"}> => <{"id_33"}> 0.5151515 1.0000000 1.0819672
# Hypoglycemic symptoms are absent and with regular dose of insulin, leads to regular dose of insulin.

# Looking for 34 = NPH insulin dose
Nhp = subset (seqRules, rhs (seqRules)% ein% c ("\" id_34 \ ""))
length (NHP)
Inspect (head (sort (nhp, by = "support"), 10)
# 8 <{"id_34"}, {"id_33"}> => <{"id_34"}> 0.7878788 1.0000000 1.200000 
# NHP insulin dose, with regular one, sometimes leads to other NHP dose

# It is hard to search for something when you are not sure what it is. Some medical knowledge would help
# A lot in this kind of tasks. I tried to inspect the cases
# Real hypotesis in the play would bring more good results.
# Diabetes paitents could, and have already benefited from applying computer science methods, like sequential
# Rules on data about the disease. I do not think we've found anything super interesting, but
# We've seen a bit of sequential rules in real world data -> it's hard to find em! 