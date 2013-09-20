import re

def vipercharts_create_integers(input_dict):
    intStr = input_dict['intStr']
    intList = []
    for i in re.findall(r'\w+', intStr):
        try:
            intList.append(int(i))
        except:
            pass
    if input_dict['sort'].lower() == "true":
        intList.sort()
    return {'intList':intList}

def vipercharts_sum_integers(input_dict):
    intList = input_dict['intList']
    return {'sum':sum(intList)}

def vipercharts_pre_filter_integers(input_dict):
    return input_dict

def vipercharts_post_filter_integers(postdata,input_dict,output_dict):
    intListOut = postdata['intListOut']
    intList = []
    for i in intListOut:
        try:
            intList.append(int(i))
        except:
            pass
    return {'intList': intList}

def vipercharts_pre_display_summation(input_dict):
    return {}
	
# Prepare curve data
def vipercharts_prepareCurveData(input_dict): #, subtype
	import math
	nPoints=4
	performance = input_dict['predictions']#chartdata
	subtype = input_dict['subtype']
	kenmax = 0.5
	ratemax = 0.5
	for curve in performance:
		n = len(curve['actual'])
		negs = curve['actual'].count(0)
		poss = curve['actual'].count(1)
		if poss == 0 or negs == 0:
			print "Class Error, zero poss or zero negs, only one class or other type error."
			return []
		try:
			ranks = curve['rank']
		except:
			ranks = range(n+1)[1:] # ranks from 1
		paralel =[]
		for i in range(n):
			paralel.append([curve['actual'][i], float(curve['predicted'][i])])
		if (subtype == '-score'):
			ROCseries = [[0,0, '-Inf']]; PRseries = [[0,1, '-Inf']]; LIFTseries = [[0,0, '-Inf']]
			ROChull = [[0,0,'-Inf']]; COSTseries = [[0,0,'-Inf']]; RATEseries = []; KENseries = [[0,0]]; KENup=[[0,1]]; KENdown=[[0,0]]
			_oldrate = 0
			_oldloss = 0
			AUC = 0
			AUPR = 0
			ranked = sorted(paralel, key = lambda pair:pair[1], reverse=True)
			print "ranked:"
			print ranked
			k = 0
			tp = 0; fp = 0; tp_old = 0; fp_old = 0; n1 = 0; concordant_pairs = 0; discordant_pairs = 0;
			while k < len(ranked):
				addedconc = 0; addeddisc = 0;
				threshold = ranked[k][1];
				group = [x[0] for x in ranked if x[1] >= threshold]
				tp = group.count(1)
				fp = group.count(0)
				#next k is len(group).
				ties = len(group) - k
				n1 += ties * (ties-1)/2				
				concordant_pairs += tp_old * (fp - fp_old)
				discordant_pairs += fp_old * (tp - tp_old)
				
				ROCpoint = [fp*1.0/negs,tp*1.0/poss, threshold]
				ROCseries.append(ROCpoint)
				AUC += (ROCpoint[1] + ROCseries[-2][1]) * (ROCpoint[0] - ROCseries[-2][0]) * 0.5
				PRseries.append([tp*1.0/poss, tp*1.0/(tp+fp), threshold])
				AUPR += (PRseries[-1][1] + PRseries[-2][1]) * (PRseries[-1][0] - PRseries[-2][0]) * 0.5
				LIFTseries.append([len(group)*1.0/n, tp*1.0/poss, threshold])
				
				#Convex hull and lower envelope:
				while len(ROChull)>=2 and (ROChull[-1][0]==ROCpoint[0]  or (ROChull[-2][0]!=ROChull[-1][0] and (ROChull[-1][1]-ROChull[-2][1])/(ROChull[-1][0]-ROChull[-2][0]) <= (ROCpoint[1]-ROChull[-1][1])/(ROCpoint[0]-ROChull[-1][0]))):
					ROChull.pop()
					COSTseries.pop()
				ROChull.append(ROCpoint)
				if(ROCpoint[0] != ROChull[-2][0]):
					slope = (ROCpoint[1] - ROChull[-2][1]) / (ROCpoint[0] - ROChull[-2][0])
					intercept = ROCpoint[1] - slope * ROCpoint[0]
					COSTseries.append([1 / (slope + 1), (1 - intercept) / (1 + slope), threshold])
				else:
					if len(COSTseries) == 0:
						COSTseries.append([0,0,threshold])
					else:
						COSTseries[0][2] = threshold	
				COSTend = 1 - ROCpoint[1]
				
				#Rate driven curve:
				#The Rate driven curve is a list of intervals. Each interval is a set of points on the appropriate parabola. There are nPoints number of points 
				RATEinterval = []
				pi0 = poss * 1.0 / n
				pi1 = 1 - pi0
				_newrate = pi1*ROCpoint[0]+pi0*ROCpoint[1]
				_newloss = 2*(_newrate*(pi0-_newrate) + pi1*ROCpoint[0])
				RATEinterval.append([_oldrate, _oldloss, threshold, performance.index(curve)+1])
				for i in range(1, nPoints):
					alpha = i * 1.0/nPoints
					rate = _oldrate + alpha * (_newrate - _oldrate)
					loss = 2 * (rate * (pi0 - rate) + pi1 * (ROCseries[-2][0] + alpha * (ROCpoint[0] - ROCseries[-2][0])))
					RATEinterval.append([rate, loss, 0])
				RATEinterval.append([_newrate, _newloss, 0])
				RATEseries.append(RATEinterval)
				if _newloss > ratemax:
					ratemax = _newloss
				m = 0.5*(pi0+pi1*(ROCseries[-2][0]-ROCpoint[0])/(_newrate-_oldrate))
				if m<_newrate and m>_oldrate:
					mvalue=2*(m*(pi0-m)+pi1*((_newrate-m)*ROCseries[-2][0] + (m-_oldrate)*ROCpoint[0])/(_newrate - _oldrate))
					if mvalue > ratemax:
						ratemax = mvalue
				
				#Kendall curve:
				if _newrate <= pi0:
					KENseries.append([_newrate, 2*pi1*ROCpoint[0], threshold])
				else:
					if _oldrate < pi0:
						KENseries.append([pi0,(2*pi1*ROCpoint[0]-KENseries[-1][1])*(pi0-KENseries[-1][0])/(_newrate - KENseries[-1][0])+(KENseries[-1][1]), ''])
					KENseries.append([_newrate, 2*pi0*(1-ROCpoint[1]), threshold])
				if KENseries[-1][1] > kenmax:
					kenmax = KENseries[-1][1]
				_oldrate = _newrate
				_oldloss = _newloss
				
				k += len(group) - k
				tp_old = tp
				fp_old = fp
		else:
			ROCseries = [[0,0,0]]; PRseries = [[0,1,0]];  LIFTseries = [[0,0,0]]# x: y: rank:
			ranked = sorted(paralel, key=lambda pair:pair[1])
			print ranked
			k = 0
			while k < len(ranked):
				tp = 0; fp = 0;
				threshold = ranked[k][1];
				group = [x[0] for x in ranked if x[1] <= threshold]
				print group
				tp = group.count('1')
				fp = group.count('0')
				ROCpoint = [fp*1.0/negs,tp*1.0/poss, threshold]
				ROCseries.append([fp*1.0/negs, tp*1.0/poss, int(threshold)])
				PRseries.append([tp*1.0/poss, tp*1.0/(tp+fp), int(threshold)])
				LIFTseries.append([len(group)*1.0/n, tp*1.0/poss, int(threshold)])
				while len(ROChull)>=2 and (ROChull[-1][0]==ROCpoint[0]  or (ROChull[-2][0]!=ROChull[-1][0] and (ROChull[-1][1]-ROChull[-2][1])/(ROChull[-1][0]-ROChull[-2][0]) <= (ROCpoint[1]-ROChull[-1][1])/(ROCpoint[0]-ROChull[-1][0]))):
					ROChull.pop()
					COSTseries.pop()
				ROChull.append(ROCpoint)
				if(ROCpoint[0]!=ROChull[-2][0]):
					slope=(ROCpoint[1]-ROChull[-2][1])/(ROCpoint[0]-ROChull[-2][0])
					intercept=ROCpoint[1]-slope*ROCpoint[0]
					COSTseries.append([1/(1+slope), (1-intercept)/(1+slope)])
				else:
					COSTseries.append([0.0, ROCpoint[0]])
				k += len(group) - k
		
		if COSTseries[-1][0]<1:
			#append final point with max threshold
			COSTseries.append([1, COSTend, ranked[-1][1]])		
		
		curve['ROCpoints'] = ROCseries
		curve['PRpoints'] = PRseries
		curve['LIFTpoints'] = LIFTseries
		curve['ROChull'] = ROChull
		curve['COSTpoints'] = COSTseries
		curve['RATEintervals'] = RATEseries
		curve['KENpoints'] = KENseries
		curve['AUC'] = AUC
		curve['Gini'] = 2 * AUC - 1
		n0=n*(n-1)/2
		curve['KENtau'] = (concordant_pairs - discordant_pairs) / math.sqrt((n0 - n1) * (n0 - (negs*(negs-1) + poss*(poss-1))/2))
		curve['AUPR'] = AUPR
		AUCH = 0
		for i in range(1, len(ROChull)):
			AUCH += (ROChull[i][1] + ROChull[i-1][1]) * (ROChull[i][0] - ROChull[i-1][0]) * 0.5
		curve['AUCH'] = AUCH
		performance[0]['KENmax'] = kenmax
		performance[0]['RATEmax'] = ratemax

	output_dict = {}
	output_dict['performance'] = performance
	return output_dict
	
def vipercharts_compare_class_ActualPredicted_orange(input_dict):
	actual = input_dict['actual']
	predicted = input_dict['predicted']
	target = input_dict['target']
	out_dict = {'actual':[], 'predicted':[], 'name':input_dict['name']}
	for i, ex in enumerate(actual):
		# set actual
		if target == ex.getclass().value:
			out_dict['actual'].append(1)
		else:
			out_dict['actual'].append(0)
		#set predicted
		if predicted[i].getclass().value == target:
			out_dict['predicted'].append(1)
		else:
			out_dict['predicted'].append(0)
			
	output_dict = {}
	output_dict['apv'] = out_dict
	return output_dict
	
def class_from_odt(input_dict):
	output_dict = {}
	output_dict['target'] = []
	return output_dict
	
def class_from_odt_selection(postdata, input_dict, output_dict):
	print postdata['selected']
	output_dict['target'] = postdata['selected'][0] #input_dict['data'].domain.class_var.values[postdata['selected']]
	return output_dict
	
# Scatter charts
def vipercharts_pr_space(input_dict):
    return {}
	
def vipercharts_roc_space(input_dict):
    return {}

# Curve charts	
def vipercharts_roc_curve(input_dict):
    return {}
	
def vipercharts_roc_hull(input_dict):
    return {}
	
def vipercharts_pr_curve(input_dict):
    return {}
	
def vipercharts_lift_curve(input_dict):
    return {}
	
def vipercharts_cost_curve(input_dict):
    return {}
	
def vipercharts_kendall_curve(input_dict):
    return {}
	
def vipercharts_rate_curve(input_dict):
    return {}

# Column charts
def vipercharts_column_chart(input_dict):
    return {}
	
def vipercharts_eval_bar_chart(input_dict):
    return {}

# Related results table    
def vipercharts_related_table(input_dict):
    return {}