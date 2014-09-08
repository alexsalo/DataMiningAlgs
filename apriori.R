{###Alex Salo###########
###Data Mining Class###
###Fall 2014###########
#Description
#We want to investigate gene expression patterns related to two diseases: breast cancer and colon cancer. We monitored gene expressions for 100 patients having breast cancer or colon cancer. The gene expressions were described as "up" regulated or "down" regulated for 100 genes. Implement CHARM algorithm to read the data set provided and find frequent closed gene-expression/disease sets with 30% minimum support. List the frequent closed gene-expression/disease sets with the size greater than or equal to 3 (including the disease). For each frequent closed gene-expression/disease set you selected, show its support. For example, {gene13 down, gene59 up, ColonCancer}: 40% support. Using the frequent closed sets you selected, find all association rules from a set of gene expressions to a disease with 60% minimum confidence. For each association rule, show its confidence. For example, {gene13 down, gene59 up} ? {ColonCancer}: 75% confidence. Measure the runtime of CHARM algorithm and Apriori algorithm implemented in assignment 1.

#Data Set
#Gene expression data ("up" or "down" regulated) for 100 samples (patients) and 100 genes are given in a tab-delimited text file. Each row represents each sample, and each column (from 2nd column to 101st column) represents each gene. The disease for each sample is shown on the last column (102nd column).
}
start = Sys.time()

min_sup = 0.3
min_conf = 0.6
inputname = 'apriori_charm_data.txt'

###PREPARATIONS
#read input
x = read.table(inputname, header = F, fill = T)
x = as.matrix(x)

#make 200 different feature. Each row has a set of 100 out 200 features. 
xx = matrix(rep(as.character(seq(1,100)), 100), nrow = 100, byrow = T)
xx[x[, -1] == 'UP'] = paste(xx[x[, -1] == 'UP'], 'UP', sep = '')
xx = matrix(xx, nrow = 100, byrow = F)
xx[x[, -1] == 'Down'] = paste(xx[x[, -1] == 'Down'], 'Down', sep = '')

#add 2 more features
xx[x[, 102] == 'BreastCancer', 101] = 'BreastCancer'
xx[x[, 102] == 'ColonCancer', 101] = 'ColonCancer'
x = xx

###APRIORI
sup = table(x) / nrow(x)
#frequent item-set
fis = as.matrix(names(sup[sup > min_sup])[-1])

final_fis = list(fis)

iter = 0
while (is.matrix(fis)){
	iter = iter + 1	
	print(paste('Iteration: ', iter, sep = ''))

	#selective joining
	new_is = NULL
	fis_len = dim(fis)[1]
	for (i in 1:(fis_len-1))
	for (j in (i+1):fis_len)
			#if have k-2 elements in common -> union them
			if ("BreastCancer" %in% union(fis[i, ], fis[j, ]) ||
				"ColonCancer" %in% union(fis[j, ], fis[j, ]))
				if (length(intersect(fis[i, ], fis[j, ])) == iter - 1)
					new_is = rbind(new_is, union(fis[i, ], fis[j, ]))

	#prunning
	new_is_len = dim(new_is)[1]
	sup = rep(0, new_is_len)
	for (i in 1:new_is_len)
	for (j in 1:dim(x)[1])
		#if new set is fully in the row of x -> increase support value
		if (length(intersect(new_is[i, ], x[j, ])) == length(new_is[i, ]))
			sup[i] = sup[i] + 1
	#freq item set contains sets with support >= min_sup
	perc_sup = sup / dim(x)[1]
	fis = new_is[perc_sup >= min_sup, ]
	#add sup value
	fisF = cbind(fis, perc_sup[perc_sup >= min_sup])
	#delete duplicate rows
	if (is.matrix(fis)){
		fis = fis[!duplicated(t(apply(fis, 1, sort))), ]
		fisF = fisF[!duplicated(t(apply(fisF, 1, sort))), ]
		}
	final_fis[[iter + 1]] = fisF
	print('...Done')
}
end = Sys.time()
print(final_fis)
print(end - start)

###ASSOCIATION RULES
#4
predicat = final_fis[[4]][-c(2, 5)]
rule = final_fis[[4]][-5]
sup_predicat = 0
sup_rule = 0
for (j in 1:dim(x)[1]){
	if (length(intersect(predicat, x[j, ])) == 3)
		sup_predicat = sup_predicat + 1
	if (length(intersect(rule, x[j, ])) == 4)
		sup_rule = sup_rule + 1
	}
confidence = sup_rule / sup_predicat
final_fis[[4]] = c(final_fis[[4]], confidence)

#3
predicats = unlist(final_fis[[3]][, -c(2,4)])
rules = unlist(final_fis[[3]][, -4])
len = dim(rules)[1]
sup_pred = rep(0, len)
sup_rule = rep(0, len)
for (i in 1:len)
for (j in 1:dim(x)[1]){
	if (length(intersect(predicats[i, ], x[j, ])) == length(predicats[i, ]))
		sup_pred[i] = sup_pred[i] + 1
	if (length(intersect(rules[i, ], x[j, ])) == length(rules[i, ]))
		sup_rule[i] = sup_rule[i] + 1
	}
confidences = sup_rule / sup_pred
final_fis[[3]] = cbind(final_fis[[3]], confidences)

#3 Print'em all
print(final_fis[[4]])
print(final_fis[[3]][confidences >= min_conf, ])