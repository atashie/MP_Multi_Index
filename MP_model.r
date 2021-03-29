library(data.table)
library(lubridate)
mp = fread('C:\\Users\\arik\\Documents\\money_please\\^GSPC.csv')
dj = fread('C:\\Users\\arik\\Documents\\money_please\\^DJI.csv')
ndq = fread('C:\\Users\\arik\\Documents\\money_please\\^IXIC.csv')
rut = fread('C:\\Users\\arik\\Documents\\money_please\\^RUT.csv')
ief = fread('C:\\Users\\arik\\Documents\\money_please\\IEF.csv')
tlt = fread('C:\\Users\\arik\\Documents\\money_please\\TLT.csv')
nik = fread('C:\\Users\\arik\\Documents\\money_please\\^N225.csv')
	while(any(nik$Close == "null")) {nik[which(nik$Close == "null"), "Close"] = nik[(which(nik$Close == "null") - 1), "Close"]}
	nik$Close = as.numeric(nik$Close)
gcf = fread('C:\\Users\\arik\\Documents\\money_please\\GC=F.csv')
	while(any(gcf$Close == "null")) {gcf[which(gcf$Close == "null"), "Close"] = gcf[(which(gcf$Close == "null") - 1), "Close"]}
	gcf$Close = as.numeric(gcf$Close)
xau = fread('C:\\Users\\arik\\Documents\\money_please\\^XAU.csv')
	while(any(xau$Close == "null")) {xau[which(xau$Close == "null"), "Close"] = xau[(which(xau$Close == "null") - 1), "Close"]}
	xau$Close = as.numeric(xau$Close)
hsi = fread('C:\\Users\\arik\\Documents\\money_please\\^HSI.csv')
	while(any(hsi$Close == "null")) {hsi[which(hsi$Close == "null"), "Close"] = hsi[(which(hsi$Close == "null") - 1), "Close"]}
	hsi$Close = as.numeric(hsi$Close)
gdaxi = fread('C:\\Users\\arik\\Documents\\money_please\\^GDAXI.csv')
	while(any(gdaxi$Close == "null")) {gdaxi[which(gdaxi$Close == "null"), "Close"] = gdaxi[(which(gdaxi$Close == "null") - 1), "Close"]}
	gdaxi$Close = as.numeric(gdaxi$Close)

	

# variance and summary stats of daily changes in the S&P500
var(diff(mp$Close) / mp$Close[-nrow(mp)])
summary(diff(mp$Close) / mp$Close[-nrow(mp)])

# variance and summary stats of daily changes in the DowJones
var(diff(dj$Close) / dj$Close[-nrow(dj)])
summary(diff(dj$Close) / dj$Close[-nrow(dj)])

# variance and summary stats of daily changes in the Russel
var(diff(rut$Close) / rut$Close[-nrow(rut)])
summary(diff(rut$Close) / rut$Close[-nrow(rut)])

# variance and summary stats of daily changes in the Nasdaq
var(diff(ndq$Close) / ndq$Close[-nrow(ndq)])
summary(diff(ndq$Close) / ndq$Close[-nrow(ndq)])

# variance and summary stats of daily changes in the 7-10 year treasuries
var(diff(ief$Close) / ief$Close[-nrow(ief)])
summary(diff(ief$Close) / ief$Close[-nrow(ief)])

# variance and summary stats of daily changes in the 20 year treasuries
var(diff(tlt$Close) / tlt$Close[-nrow(tlt)])
summary(diff(tlt$Close) / tlt$Close[-nrow(tlt)])

# variance and summary stats of daily changes in the nikkei
var(diff(nik$Close) / nik$Close[-nrow(nik)])
summary(diff(nik$Close) / nik$Close[-nrow(nik)])

# variance and summary stats of daily changes in the gold gc=f
var(diff(gcf$Close) / gcf$Close[-nrow(gcf)])
summary(diff(gcf$Close) / gcf$Close[-nrow(gcf)])

# variance and summary stats of daily changes in the phlx gold and silver sector I
var(diff(xau$Close) / xau$Close[-nrow(xau)])
summary(diff(xau$Close) / xau$Close[-nrow(xau)])

# variance and summary stats of daily changes in the Hang Seng Index
var(diff(hsi$Close) / hsi$Close[-nrow(hsi)])
summary(diff(hsi$Close) / hsi$Close[-nrow(hsi)])

# variance and summary stats of daily changes in the german DAX
var(diff(gdaxi$Close) / gdaxi$Close[-nrow(gdaxi)])
summary(diff(gdaxi$Close) / gdaxi$Close[-nrow(gdaxi)])


	# autocorrelation of each data
acf(diff(mp$Close) / mp$Close[-nrow(mp)], lag.max=10, ylim=c(-.2,.2))
acf(diff(dj$Close) / dj$Close[-nrow(dj)], lag.max=10, ylim=c(-.2,.2))
acf(diff(ndq$Close) / ndq$Close[-nrow(ndq)], lag.max=10, ylim=c(-.2,.2))
acf(diff(rut$Close) / rut$Close[-nrow(rut)], lag.max=10, ylim=c(-.2,.2))
acf(diff(ief$Close) / ief$Close[-nrow(ief)], lag.max=10, ylim=c(-.2,.2))
acf(diff(tlt$Close) / tlt$Close[-nrow(tlt)], lag.max=10, ylim=c(-.2,.2))
acf(diff(nik$Close) / nik$Close[-nrow(nik)], lag.max=10, ylim=c(-.2,.2))
acf(diff(gcf$Close) / gcf$Close[-nrow(gcf)], lag.max=10, ylim=c(-.2,.2))
acf(diff(xau$Close) / xau$Close[-nrow(xau)], lag.max=10, ylim=c(-.2,.2))
acf(diff(hsi$Close) / hsi$Close[-nrow(hsi)], lag.max=10, ylim=c(-.2,.2))
acf(diff(gdaxi$Close) / gdaxi$Close[-nrow(gdaxi)], lag.max=10, ylim=c(-.2,.2))




mp_model = function(
	Market_Index , # value of ETF we are trading into and out of; vector
	cash_ratio ,		# the initial cash ratio (updated according to market conditions)
	starting_cash , # initial value; will change with market conditions
	beta_portfolio,	# this is the ideal value that is bought / sold toward; can be calibrated
	portfolio_beta_min = .2,	# how much do we allow beta to drop below beta_portfolio? calibrated
	portfolio_beta_max = .2,	# how much do we allow beta to go above beta_portfolio? calibrated
	buy_rate_scalar = .1,		# how quickly do we buy back towards beta_portfolio? calibrated
	sell_rate_scalar = .1,		# how quickly do we sell back towards beta_portfolio? calibrated
	max_buy_rate = .2,			# can be calibrated... 
	max_sell_rate = .2,			# can be calibrated... 
	buy_rate_exponent = 1,	# exponential modifier of buy rates... calibrated
	sell_rate_exponent = 1, # exponential modifier of sell rates... calibrated
	beta_scalar = 3,	# leverage of cash_in_market
	randomize = TRUE)	{
	
	
	ETF_change = if(randomize)	{
		sample(diff(Market_Index) / Market_Index[-length(Market_Index)], replace=FALSE)
		} else {
			diff(Market_Index) / Market_Index[-length(Market_Index)]
		}
	#plot(ETF_change)
	
	cash_in_bank = starting_cash * (1 - cash_ratio)
	cash_in_mrkt = (starting_cash * cash_ratio)
	cash_total = starting_cash
	ETF_newvalues = Market_Index[1]
	buy_sell_rate = NA
	for (i in 1:length(ETF_change))	{
		daily_gains_losses = cash_in_mrkt[i] * ETF_change[i] * beta_scalar
		cash_in_mrkt[i+1] = cash_in_mrkt[i]	+ daily_gains_losses # calculate end of day cash in market
		cash_total[i+1] = cash_in_mrkt[i+1] + cash_in_bank[i]
		current_beta_portfolio = cash_in_mrkt[i+1] * beta_scalar / cash_total[i+1]

		#cash_in_bank[i+1] = cash_in_bank[i]
		# check to see if the portfolio needs to be adjusted
		if(current_beta_portfolio < beta_portfolio)	{
			buy_rate = buy_rate_scalar * ((beta_portfolio - current_beta_portfolio) / portfolio_beta_min) ^ buy_rate_exponent
			buy_sell_rate[i+1] = min(buy_rate, max_buy_rate)
			buy_funds = cash_in_bank[i] * buy_rate
			cash_in_mrkt[i+1] = cash_in_mrkt[i+1] + buy_funds
			cash_in_bank[i+1] = cash_in_bank[i] - buy_funds
		}
		if(current_beta_portfolio > beta_portfolio)	{
			sell_rate = sell_rate_scalar * ((current_beta_portfolio - beta_portfolio) / portfolio_beta_max) ^ sell_rate_exponent
			buy_sell_rate[i+1] = min(sell_rate, max_sell_rate)
			sell_funds = cash_in_mrkt[i] * sell_rate
			cash_in_mrkt[i+1] = cash_in_mrkt[i+1] - sell_funds
			cash_in_bank[i+1] = cash_in_bank[i] + sell_funds
		}
		
		ETF_newvalues = c(ETF_newvalues, ETF_newvalues[i] * (1+ETF_change[i]))
	}
	
	output = data.frame(cbind(cash_in_bank, cash_in_mrkt, buy_sell_rate, ETF_newvalues))
	return(output)
}



portfolio_beta_min = 1.2 #multiplier; may need to be calibrated; value of beta at which you begin ramping up / down actual_portfolio_beta; may need to be calibrated
portfolio_beta_max = 1.8
starting_cash = 1		# initial cash on hand
cash_ratio = .6				#ratio; initial value which shouldn't neet to be calibrated (should equilibriate)
buy_rate_scalar = .1	#ratio; should be calibrated, and likely exponential; rate at which you move funds from cash to market and vice versa 
sell_rate_scalar = .1
beta_portfolio = 1.4
sell_rate_exponent = 1.2
buy_rate_exponent = 1.2

mp_model(
		Market_Index = SnP_sub, # value of ETF we are trading into and out of; vector
		cash_ratio = cash_ratio,		# the initial cash ratio (updated according to market conditions
		starting_cash = starting_cash, # initial value; will change with market conditions
		beta_portfolio = beta_portfolio,	# this is the ideal value that is bought / sold toward
		portfolio_beta_min = portfolio_beta_min,	# the minimum value allowed for the portfolio beta
		portfolio_beta_max = portfolio_beta_max,	# the maximum value allowed for the portfolio beta
		buy_rate_scalar = buy_rate_scalar,
		sell_rate_scalar = sell_rate_scalar, 
		max_buy_rate = .2,
		max_sell_rate = .2,
		buy_rate_exponent = buy_rate_exponent,	# exponential modifier of buy rates
		sell_rate_exponent = sell_rate_exponent, # exponential modifier of sell rates
		beta_scalar = 3,	# leverage of cash_in_market
		randomize = TRUE
		)



#set.seed(5)
#set.seed(NULL)
n_runs = 5000

	yearly_subsample_start = runif(n_runs, 1, nrow(mp)-(10*253))
	starting_cash = 1		# initial cash on hand
	cash_ratio = .6				#ratio; initial value which shouldn't neet to be calibrated (should equilibriate)
	beta_portfolio = 1.4
	

	test_output = data.frame(beta_portfolio=rep(NA,n_runs), portfolio_beta_min=rep(NA,n_runs), portfolio_beta_max=rep(NA,n_runs), 
		buy_rate_scalar=rep(NA,n_runs), sell_rate_scalar=rep(NA,n_runs), buy_rate_exponent=rep(NA,n_runs), sell_rate_exponent=rep(NA,n_runs),
		final_returns_5th=rep(NA,n_runs), relative_returns_5th=rep(NA,n_runs),
		final_returns_25th=rep(NA,n_runs), relative_returns_25th=rep(NA,n_runs),
		final_returns_50th=rep(NA,n_runs), relative_returns_50th=rep(NA,n_runs),
		final_returns_75th=rep(NA,n_runs), relative_returns_75th=rep(NA,n_runs))

#kk=0
beta_portfolio_dist = runif(n_runs,1.3,2)#		1.2,1.45)		#,1.2,2.2)			#1.1,1.8)
portfolio_beta_min_dist = runif(n_runs,.1,.28)		#,.14,.41)		#.1,.5)
portfolio_beta_max_dist = runif(n_runs,.11,.47)		#,.16,.42)		#.1,.5)
buy_rate_scalar_dist = runif(n_runs,.04,.19)			#,.08,.16)		#0.01,.17)
sell_rate_scalar_dist = runif(n_runs,.03,.29)		#,.008,.13)		#0.01,.17)
buy_rate_exponent_dist = runif(n_runs,1.1,2.0)		#,1,1.35)		#1,1.8)
sell_rate_exponent_dist = runif(n_runs,.9,1.8)		#,1.17,1.9)		#1,1.8)

loss_likelihood = 0.5 # if n% of model runs generate returns below the index, then we toss that variable set
num_years = 1

while(kk <= n_runs)	{
kk=kk+1
	
test_output$beta_portfolio[kk] = sample(beta_portfolio_dist,1)
test_output$portfolio_beta_min[kk] = sample(portfolio_beta_min_dist,1)
test_output$portfolio_beta_max[kk] = sample(portfolio_beta_max_dist,1)
test_output$buy_rate_scalar[kk] = sample(buy_rate_scalar_dist,1)
test_output$sell_rate_scalar[kk] = sample(sell_rate_scalar_dist,1) 
test_output$buy_rate_exponent[kk] = sample(buy_rate_exponent_dist,1)
test_output$sell_rate_exponent[kk] = sample(sell_rate_exponent_dist,1)

	fin_returns = NULL
	rel_returns = NULL
	for(ll in 1:5000){
		sample_start = sample(yearly_subsample_start,1)
		SnP_sub = mp$Close[sample_start:(sample_start+num_years*253)]		# vector of historical SnP data
		
		test = mp_model(
			Market_Index = SnP_sub, # value of ETF we are trading into and out of; vector
			cash_ratio = cash_ratio,		# the initial cash ratio (updated according to market conditions
			starting_cash = starting_cash, # initial value; will change with market conditions bought / sold toward
			portfolio_beta_min = test_output$portfolio_beta_min[kk],	# the minim
			beta_portfolio = test_output$beta_portfolio[kk],	# this is the ideal value that isum value allowed for the portfolio beta
			portfolio_beta_max = test_output$portfolio_beta_max[kk],	# the maximum value allowed for the portfolio beta
			buy_rate_scalar = test_output$buy_rate_scalar[kk],
			sell_rate_scalar = test_output$sell_rate_scalar[kk], 
			max_buy_rate = .2,
			max_sell_rate = .2,
			buy_rate_exponent = test_output$buy_rate_exponent[kk],	# exponential modifier of buy rates
			sell_rate_exponent = test_output$sell_rate_exponent[kk], # exponential modifier of sell rates
			beta_scalar = 3,	# leverage of cash_in_market
			randomize = TRUE
			)
		
		fin_returns = c(fin_returns, last(test$cash_in_bank) + last(test$cash_in_mrkt))
		rel_returns = c(rel_returns, fin_returns[ll] -
			(starting_cash * last(test$ETF_newvalues) / test$ETF_newvalues[1]))

	}

if(length(which(rel_returns <= 0)) <= loss_likelihood*2000)	{
	test_output$final_returns_5th[kk] = quantile(fin_returns, .05, na.rm=TRUE)
	test_output$relative_returns_5th[kk] = quantile(rel_returns, .05, na.rm=TRUE)
	test_output$final_returns_25th[kk] = quantile(fin_returns, .25, na.rm=TRUE)
	test_output$relative_returns_25th[kk] = quantile(rel_returns, .25, na.rm=TRUE)
	test_output$final_returns_50th[kk] = quantile(fin_returns, .50, na.rm=TRUE)
	test_output$relative_returns_50th[kk] = quantile(rel_returns, .50, na.rm=TRUE)
	test_output$final_returns_75th[kk] = quantile(fin_returns, .75, na.rm=TRUE)
	test_output$relative_returns_75th[kk] = quantile(rel_returns, .75, na.rm=TRUE)
	} else {
	test_output$final_returns_5th[kk] = NA
	test_output$relative_returns_5th[kk] = NA
	test_output$final_returns_25th[kk] = NA
	test_output$relative_returns_25th[kk] = NA
	test_output$final_returns_50th[kk] = NA
	test_output$relative_returns_50th[kk] = NA
	test_output$final_returns_75th[kk] = NA
	test_output$relative_returns_75th[kk] = NA
}
	plot(test$cash_in_bank + test$cash_in_mrkt, type = 'l', col='red3', lwd=3, main=paste(kk))
	lines((starting_cash * test$ETF_newvalues / test$ETF_newvalues[1]), col='black', lwd=3)
}

sorted_output = subset(test_output, !is.na(relative_returns_25th))
sorted_output = sorted_output[rev(order(sorted_output$relative_returns_75th)),] 

head(sorted_output,20)

calibration_output = sorted_output



mp_sub = mp[1800:5536,]	#using the snp
mp_sub = dj[1:7341,]	#using the dow
mp_sub = ndq[1800:7341,]	#using the nasdaq
mp_sub = rut[1800:7341,]	#using the rus

mp_sub = mp[1800:5536,]	#using a subsample of the snp index
mp_sub = mp	# or just using the entire index
n_runs = 5000

	numb_years = 1
	yearly_subsample_start = runif(n_runs, 1, nrow(mp_sub)-(numb_years*253))
	starting_cash = 1		# initial cash on hand
	cash_ratio = .6				#ratio; initial value which shouldn't neet to be calibrated (should equilibriate)
	beta_portfolio = 1.4
	
	valid_output = data.frame(portfolio_beta_min=rep(NA,n_runs), portfolio_beta_max=rep(NA,n_runs), 
		buy_rate_scalar=rep(NA,n_runs), sell_rate_scalar=rep(NA,n_runs), buy_rate_exponent=rep(NA,n_runs), sell_rate_exponent=rep(NA,n_runs),
		market_returns=rep(NA,n_runs), relative_returns=rep(NA,n_runs), model_returns=rep(NA,n_runs))
kk=0
while(kk <= n_runs)	{
kk=kk+1
sample_start = sample(yearly_subsample_start,1)
SnP_sub = mp_sub$Close[sample_start:(sample_start+numb_years*253)]		# vector of historical SnP data
	
valid_output$beta_portfolio[kk] = sorted_output$beta_portfolio[1]
valid_output$portfolio_beta_min[kk] = sorted_output$portfolio_beta_min[1]
valid_output$portfolio_beta_max[kk] = sorted_output$portfolio_beta_max[1]
valid_output$buy_rate_scalar[kk] = sorted_output$buy_rate_scalar[1]
valid_output$sell_rate_scalar[kk] = sorted_output$sell_rate_scalar[1]
valid_output$buy_rate_exponent[kk] = sorted_output$buy_rate_exponent[1]
valid_output$sell_rate_exponent[kk] = sorted_output$sell_rate_exponent[1]

	test = mp_model(
		Market_Index = SnP_sub, # value of ETF we are trading into and out of; vector
		cash_ratio = cash_ratio,		# the initial cash ratio (updated according to market conditions
		starting_cash = starting_cash, # initial value; will change with market conditions
		beta_portfolio = valid_output$beta_portfolio[kk],	# this is the ideal value that is bought / sold toward
		portfolio_beta_min = valid_output$portfolio_beta_min[kk],	# the minimum value allowed for the portfolio beta
		portfolio_beta_max = valid_output$portfolio_beta_max[kk],	# the maximum value allowed for the portfolio beta
		buy_rate_scalar = valid_output$buy_rate_scalar[kk],
		sell_rate_scalar = valid_output$sell_rate_scalar[kk], 
		max_buy_rate = .2,
		max_sell_rate = .2,
		buy_rate_exponent = valid_output$buy_rate_exponent[kk],	# exponential modifier of buy rates
		sell_rate_exponent = valid_output$sell_rate_exponent[kk], # exponential modifier of sell rates
		beta_scalar = 3,	# leverage of cash_in_market
		randomize = TRUE
		)


	valid_output$model_returns[kk] = last(test$cash_in_bank) + last(test$cash_in_mrkt)
	valid_output$market_returns[kk] = starting_cash * last(test$ETF_newvalues) / test$ETF_newvalues[1]
	valid_output$relative_returns[kk] = valid_output$model_returns[kk] - valid_output$market_returns[kk]



	plot(test$cash_in_bank + test$cash_in_mrkt, type = 'l', col='red3', lwd=3, main=paste(kk))
	lines((starting_cash * test$ETF_newvalues / test$ETF_newvalues[1]), col='black', lwd=3)
}

length(which(valid_output$relative_returns < 0)) / n_runs
summary(valid_output$relative_returns)

boxplot(valid_output[, c("market_returns","model_returns")],
	main=paste("S&P500,",
		numb_years,
		"yrs, \nrandomized", first(year(mp_sub$Date)), "to", last(year(mp_sub$Date))),
	xlab = NULL, ylab="Total Returns [-]"
	)
	























####################################################################
## same model but incorporating shorts, too


ls_model = function(
	Market_Index , # value of ETF we are trading into and out of; vector
	starting_cash = 1, # initial value; will change with market conditions
	beta_long = 1.6,	# this is the ideal value of beta relative to longs
	beta_short = 0.2,	# this is the ideal value of beta relative to shorts
	cash_ratio = .5,		#  the fraction of cash we start with in the bank
	portfolio_split = 10, 	# ratio for splitting the portfolio between shorts and longs
	portfolio_beta_min = .2,	# the minimum value allowed for the portfolio beta
	portfolio_beta_max = .2,	# the maximum value allowed for the portfolio beta
	buy_rate_scalar = .1,
	sell_rate_scalar = .1,
	max_buy_rate = .2,
	max_sell_rate = .2,
	buy_rate_exponent = 1,	# exponential modifier of buy rates
	sell_rate_exponent = 1, # exponential modifier of sell rates
	beta_scalar = 3,	# leverage of cash_in_market
	randomize = TRUE)	{
	
	
	ETF_change = if(randomize)	{
		sample(diff(Market_Index$Close) / Market_Index$Close[-length(Market_Index$Close)], replace=FALSE)
		} else {
			diff(Market_Index$Close) / Market_Index$Close[-length(Market_Index$Close)]
		}
	#plot(ETF_change)
	
	cash_in_bank = starting_cash * (cash_ratio)
	cash_in_long = starting_cash * (1-cash_ratio) * ((portfolio_split - 1) / portfolio_split)
	cash_in_short = starting_cash * (1-cash_ratio) * (1 / portfolio_split)
	cash_total = starting_cash
	ETF_newvalues = Market_Index$Close[1]
	buy_sell_rate = NA
	for (i in 1:length(ETF_change))	{
		daily_gains = cash_in_long[i] * ETF_change[i] * beta_scalar
		daily_losses = cash_in_short[i] * ETF_change[i] * beta_scalar
		
		cash_in_long[i+1] = cash_in_long[i] + daily_gains
		cash_in_short[i+1] = cash_in_short[i] - daily_losses
		
		current_beta_short = (cash_in_short[i+1] * beta_scalar) / (cash_in_short[i+1] + cash_in_bank[i])
		current_beta_long = (cash_in_long[i+1] * beta_scalar) / (cash_in_long[i+1] + cash_in_bank[i])

		# check to see if the longs portfolio needs to be adjusted
		if(current_beta_long < beta_long)	{
			buy_rate = buy_rate_scalar * ((beta_long - current_beta_long) / portfolio_beta_min) ^ buy_rate_exponent
			buy_sell_rate[i+1] = min(buy_rate, max_buy_rate)
			buy_funds = cash_in_bank[i] * buy_rate
			cash_in_long[i+1] = cash_in_long[i+1] + buy_funds
			cash_in_bank[i+1] = cash_in_bank[i] - buy_funds
		}
		if(current_beta_long > beta_long)	{
			sell_rate = sell_rate_scalar * ((current_beta_long - beta_long) / portfolio_beta_max) ^ sell_rate_exponent
			buy_sell_rate[i+1] = min(sell_rate, max_sell_rate)
			sell_funds = cash_in_mrkt[i] * sell_rate
			cash_in_long[i+1] = cash_in_long[i+1] - sell_funds
			cash_in_bank[i+1] = cash_in_bank[i] + sell_funds
		}
		
		# check to see if the shorts portfolio needs to be adjusted
		if(current_beta_short < beta_short)	{
			buy_rate = buy_rate_scalar * ((beta_short - current_beta_short) / portfolio_beta_min) ^ buy_rate_exponent
			buy_sell_rate[i+1] = min(buy_rate, max_buy_rate)
			buy_funds = cash_in_bank[i+1] * buy_rate
			cash_in_short[i+1] = cash_in_short[i+1] + buy_funds
			cash_in_bank[i+1] = cash_in_bank[i+1] - buy_funds
		}
		if(current_beta_short > beta_short)	{
			sell_rate = sell_rate_scalar * ((current_beta_short - beta_short) / portfolio_beta_max) ^ sell_rate_exponent
			buy_sell_rate[i+1] = min(sell_rate, max_sell_rate)
			sell_funds = cash_in_mrkt[i+1] * sell_rate
			cash_in_short[i+1] = cash_in_short[i+1] - sell_funds
			cash_in_bank[i+1] = cash_in_bank[i+1] + sell_funds
		}
		
		ETF_newvalues = c(ETF_newvalues, ETF_newvalues[i] * (1+ETF_change[i]))
	}
	
	output = data.frame(cbind(cash_in_bank, cash_in_long, cash_in_short, ETF_newvalues))
	return(output)
}






mp$Year = year(mp$Date)
mp_sub = subset(mp, Year > 2000 & Year < 2011)

n_runs = 2000

beta_long_dist = runif(n_runs,1.2,1.45)		#,1.2,2.2)			#1.1,1.8)
beta_short_dist = runif(n_runs,.3,.5)		#,1.2,2.2)			#1.1,1.8)
portfolio_beta_min_dist = runif(n_runs,.35,.51)		#,.14,.41)		#.1,.5)
portfolio_beta_max_dist = runif(n_runs,.25,.4)		#,.16,.42)		#.1,.5)
buy_rate_scalar_dist = runif(n_runs,.14,.23)			#,.08,.16)		#0.01,.17)
sell_rate_scalar_dist = runif(n_runs,.14,.27)		#,.008,.13)		#0.01,.17)
buy_rate_exponent_dist = runif(n_runs,1.05,1.35)		#,1,1.35)		#1,1.8)
sell_rate_exponent_dist = runif(n_runs,1.1,1.7)		#,1.17,1.9)		#1,1.8)

	
calib_output = data.frame(portfolio_beta_min=rep(NA,n_runs), portfolio_beta_max=rep(NA,n_runs), 
	buy_rate_scalar=rep(NA,n_runs), sell_rate_scalar=rep(NA,n_runs), buy_rate_exponent=rep(NA,n_runs), sell_rate_exponent=rep(NA,n_runs),
	beta_long=rep(NA,n_runs), beta_short=rep(NA,n_runs),
	final_returns_5th = rep(NA,n_runs),
	relative_returns_5th = rep(NA,n_runs),
	final_returns_25th = rep(NA,n_runs),
	relative_returns_25th = rep(NA,n_runs),
	final_returns_50th = rep(NA,n_runs),
	relative_returns_50th = rep(NA,n_runs),
	final_returns_75th = rep(NA,n_runs),
	relative_returns_75th = rep(NA,n_runs))

loss_likelihood = 1 # if n% of model runs generate returns below the index, then we toss that variable set

while(kk <= n_runs)	{
kk=kk+1
	
calib_output$beta_long[kk] = sample(beta_long_dist,1)
calib_output$beta_short[kk] = sample(beta_short_dist,1)
calib_output$portfolio_beta_min[kk] = sample(portfolio_beta_min_dist,1)
calib_output$portfolio_beta_max[kk] = sample(portfolio_beta_max_dist,1)
calib_output$buy_rate_scalar[kk] = sample(buy_rate_scalar_dist,1)
calib_output$sell_rate_scalar[kk] = sample(sell_rate_scalar_dist,1) 
calib_output$buy_rate_exponent[kk] = sample(buy_rate_exponent_dist,1)
calib_output$sell_rate_exponent[kk] = sample(sell_rate_exponent_dist,1)

	fin_returns = NULL
	rel_returns = NULL
	for(ll in 1:1000){
		yearly_subsample_start = floor(runif(n_runs, 1, nrow(mp_sub)-(1*253)))
		sample_start = sample(yearly_subsample_start,1)
		SnP_sub = mp_sub[sample_start:(sample_start+1*253),]		# vector of historical SnP data
		
		test = ls_model(
			Market_Index = SnP_sub, # value of ETF we are trading into and out of; vector
			cash_ratio = .4,		# the initial cash ratio (updated according to market conditions
			starting_cash = 1, # initial value; will change with market conditions bought / sold toward
			beta_long = calib_output$beta_long[kk],	# this is the ideal value of beta relative to longs
			beta_short = calib_output$beta_short[kk],	# this is the ideal value of beta relative to shorts
			portfolio_beta_min = calib_output$portfolio_beta_min[kk],	# the minim
			portfolio_beta_max = calib_output$portfolio_beta_max[kk],	# the maximum value allowed for the portfolio beta
			buy_rate_scalar = calib_output$buy_rate_scalar[kk],
			sell_rate_scalar = calib_output$sell_rate_scalar[kk], 
			portfolio_split = 10, 	# ratio for splitting the portfolio between shorts and longs
			max_buy_rate = .2,
			max_sell_rate = .2,
			buy_rate_exponent = calib_output$buy_rate_exponent[kk],	# exponential modifier of buy rates
			sell_rate_exponent = calib_output$sell_rate_exponent[kk], # exponential modifier of sell rates
			beta_scalar = 3,	# leverage of cash_in_market
			randomize = TRUE
			)
		
		fin_returns = c(fin_returns, last(test$cash_in_bank) + last(test$cash_in_long) + last(test$cash_in_short))
		rel_returns = c(rel_returns, fin_returns[ll] -
			(starting_cash * last(test$ETF_newvalues) / test$ETF_newvalues[1]))

	}

if(length(which(rel_returns <= 0)) <= loss_likelihood*1000)	{
	calib_output$final_returns_5th[kk] = quantile(fin_returns, .05, na.rm=TRUE)
	calib_output$relative_returns_5th[kk] = quantile(rel_returns, .05, na.rm=TRUE)
	calib_output$final_returns_25th[kk] = quantile(fin_returns, .25, na.rm=TRUE)
	calib_output$relative_returns_25th[kk] = quantile(rel_returns, .25, na.rm=TRUE)
	calib_output$final_returns_50th[kk] = quantile(fin_returns, .50, na.rm=TRUE)
	calib_output$relative_returns_50th[kk] = quantile(rel_returns, .50, na.rm=TRUE)
	calib_output$final_returns_75th[kk] = quantile(fin_returns, .75, na.rm=TRUE)
	calib_output$relative_returns_75th[kk] = quantile(rel_returns, .75, na.rm=TRUE)
	} else {
	calib_output$final_returns_5th[kk] = NA
	calib_output$relative_returns_5th[kk] = NA
	calib_output$final_returns_25th[kk] = NA
	calib_output$relative_returns_25th[kk] = NA
	calib_output$final_returns_50th[kk] = NA
	calib_output$relative_returns_50th[kk] = NA
	calib_output$final_returns_75th[kk] = NA
	calib_output$relative_returns_75th[kk] = NA
}
	plot(test$cash_in_bank + test$cash_in_long + test$cash_in_short, type = 'l', col='red3', lwd=3, main=paste(kk))
	abline(h=1, lty=3, lwd=1)
	lines((starting_cash * test$ETF_newvalues / test$ETF_newvalues[1]), col='black', lwd=3)
}

sorted_output = subset(calib_output, !is.na(relative_returns_25th))
sorted_output = sorted_output[rev(order(sorted_output$relative_returns_25th)),] 

head(sorted_output,10)











mp_sub = mp[1800:7341,]	#using the snp
mp_sub = dj[1:7341,]	#using the dow
mp_sub = ndq[1800:7341,]	#using the nasdaq
mp_sub = rut[1800:7341,]	#using the nasdaq
mp_sub = subset(mp, Year > 1999 & Year < 2012)

	n_runs = 5000
	num_years = 1
	yearly_subsample_start = runif(n_runs, 1, nrow(mp_sub)-(num_years*253))
	
	valid_output = data.frame(beta_long=rep(NA,n_runs), beta_short=rep(NA,n_runs),
		portfolio_beta_min=rep(NA,n_runs), portfolio_beta_max=rep(NA,n_runs), 
		buy_rate_scalar=rep(NA,n_runs), sell_rate_scalar=rep(NA,n_runs), buy_rate_exponent=rep(NA,n_runs), sell_rate_exponent=rep(NA,n_runs),
		model_returns=rep(NA,n_runs), relative_returns=rep(NA,n_runs), market_returns=rep(NA,n_runs))
kk=0
while(kk <= n_runs)	{
kk=kk+1
sample_start = sample(yearly_subsample_start,1)
SnP_sub = mp_sub[sample_start:(sample_start+num_years*253),]		# vector of historical SnP data
	
valid_output$beta_long[kk] = sorted_output$beta_long[1]
valid_output$beta_short[kk] = sorted_output$beta_short[1]
valid_output$portfolio_beta_min[kk] = sorted_output$portfolio_beta_min[1]
valid_output$portfolio_beta_max[kk] = sorted_output$portfolio_beta_max[1]
valid_output$buy_rate_scalar[kk] = sorted_output$buy_rate_scalar[1]
valid_output$sell_rate_scalar[kk] = sorted_output$sell_rate_scalar[1]
valid_output$buy_rate_exponent[kk] = sorted_output$buy_rate_exponent[1]
valid_output$sell_rate_exponent[kk] = sorted_output$sell_rate_exponent[1]

	test = ls_model(
		Market_Index = SnP_sub, # value of ETF we are trading into and out of; vector
		cash_ratio = .4,		# the initial cash ratio (updated according to market conditions
		starting_cash = 1, # initial value; will change with market conditions
		beta_long = valid_output$beta_long[kk],	# this is the ideal value of beta relative to longs
		beta_short = valid_output$beta_short[kk],	# this is the ideal value of beta relative to shorts
		portfolio_beta_min = valid_output$portfolio_beta_min[kk],	# the minimum value allowed for the portfolio beta
		portfolio_beta_max = valid_output$portfolio_beta_max[kk],	# the maximum value allowed for the portfolio beta
		buy_rate_scalar = valid_output$buy_rate_scalar[kk],
		sell_rate_scalar = valid_output$sell_rate_scalar[kk], 
		portfolio_split = 10, 	# ratio for splitting the portfolio between shorts and longs
		max_buy_rate = .2,
		max_sell_rate = .2,
		buy_rate_exponent = valid_output$buy_rate_exponent[kk],	# exponential modifier of buy rates
		sell_rate_exponent = valid_output$sell_rate_exponent[kk], # exponential modifier of sell rates
		beta_scalar = 3,	# leverage of cash_in_market
		randomize = FALSE
		)



	valid_output$model_returns[kk] = last(test$cash_in_bank) + last(test$cash_in_long) + last(test$cash_in_short)
	valid_output$market_returns[kk] = starting_cash * last(test$ETF_newvalues) / test$ETF_newvalues[1]
	valid_output$relative_returns[kk] = valid_output$model_returns[kk] - valid_output$market_returns[kk]



	plot(test$cash_in_bank + test$cash_in_long + test$cash_in_short, type = 'l', col='red3', lwd=3, main=paste(kk))
	lines((starting_cash * test$ETF_newvalues / test$ETF_newvalues[1]), col='black', lwd=3)
}

length(which(valid_output$relative_returns < 0)) / n_runs
summary(valid_output$relative_returns)

boxplot(valid_output[, c("market_returns","model_returns")],
	main=paste("S&P500,",
		num_years,
		"yrs, \nhistorical", first(year(mp_sub$Date)), "to", last(year(mp_sub$Date))),
	xlab = NULL, ylab="Total Returns [-]"
	)
	

































####################################################################
## same model but incorporating shorts and putting house money in the bank (bonds)


lsb_model = function(
	Market_Index , # value of ETF we are trading into and out of; vector
	starting_cash = 1, # initial value; will change with market conditions
	beta_long = 1.6,	# this is the ideal value of beta relative to longs
	beta_short = 0.2,	# this is the ideal value of beta relative to shorts
	cash_ratio = .5,		#  the fraction of cash we start with in the bank
	portfolio_split = 10, 	# ratio for splitting the portfolio between shorts and longs
	portfolio_beta_min = .2,	# the minimum value allowed for the portfolio beta
	portfolio_beta_max = .2,	# the maximum value allowed for the portfolio beta
	buy_rate_scalar = .1,
	sell_rate_scalar = .1,
	max_buy_rate = .2,
	max_sell_rate = .2,
	buy_rate_exponent = 1,	# exponential modifier of buy rates
	sell_rate_exponent = 1, # exponential modifier of sell rates
	bond_buy_rate = 0.1, # the rate at which we are willing to buy bonds relatie to gains in our bank account
	bond_yield = 0.01, # the annual yield on our bonds
	beta_scalar = 3,	# leverage of cash_in_market
	randomize = TRUE)	{
	
	
	ETF_change = if(randomize)	{
		sample(diff(Market_Index$Close) / Market_Index$Close[-length(Market_Index$Close)], replace=FALSE)
		} else {
			diff(Market_Index$Close) / Market_Index$Close[-length(Market_Index$Close)]
		}
	#plot(ETF_change)
	
	cash_in_bank = starting_cash * (cash_ratio)
	cash_in_long = starting_cash * (1-cash_ratio) * ((portfolio_split - 1) / portfolio_split)
	cash_in_short = starting_cash * (1-cash_ratio) * (1 / portfolio_split)
	cash_in_bond = 0
	cash_total = starting_cash
	ETF_newvalues = Market_Index$Close[1]
	buy_sell_rate = NA
	for (i in 1:length(ETF_change))	{
		daily_gains = cash_in_long[i] * ETF_change[i] * beta_scalar
		daily_losses = cash_in_short[i] * ETF_change[i] * beta_scalar
		
		cash_in_long[i+1] = cash_in_long[i] + daily_gains
		cash_in_short[i+1] = cash_in_short[i] - daily_losses
		
		current_beta_short = (cash_in_short[i+1] * beta_scalar) / (cash_in_short[i+1] + cash_in_bank[i])
		current_beta_long = (cash_in_long[i+1] * beta_scalar) / (cash_in_long[i+1] + cash_in_bank[i])

		# check to see if the longs portfolio needs to be adjusted
		if(current_beta_long < beta_long)	{
			buy_rate = buy_rate_scalar * ((beta_long - current_beta_long) / portfolio_beta_min) ^ buy_rate_exponent
			buy_sell_rate[i+1] = min(buy_rate, max_buy_rate)
			buy_funds = cash_in_bank[i] * buy_rate
			cash_in_long[i+1] = cash_in_long[i+1] + buy_funds
			cash_in_bank[i+1] = cash_in_bank[i] - buy_funds
		}
		if(current_beta_long > beta_long)	{
			sell_rate = sell_rate_scalar * ((current_beta_long - beta_long) / portfolio_beta_max) ^ sell_rate_exponent
			buy_sell_rate[i+1] = min(sell_rate, max_sell_rate)
			sell_funds = cash_in_mrkt[i] * sell_rate
			cash_in_long[i+1] = cash_in_long[i+1] - sell_funds
			cash_in_bank[i+1] = cash_in_bank[i] + sell_funds
		}
		
		# check to see if the shorts portfolio needs to be adjusted
		if(current_beta_short < beta_short)	{
			buy_rate = buy_rate_scalar * ((beta_short - current_beta_short) / portfolio_beta_min) ^ buy_rate_exponent
			buy_sell_rate[i+1] = min(buy_rate, max_buy_rate)
			buy_funds = cash_in_bank[i+1] * buy_rate
			cash_in_short[i+1] = cash_in_short[i+1] + buy_funds
			cash_in_bank[i+1] = cash_in_bank[i+1] - buy_funds
		}
		if(current_beta_short > beta_short)	{
			sell_rate = sell_rate_scalar * ((current_beta_short - beta_short) / portfolio_beta_max) ^ sell_rate_exponent
			buy_sell_rate[i+1] = min(sell_rate, max_sell_rate)
			sell_funds = cash_in_mrkt[i+1] * sell_rate
			cash_in_short[i+1] = cash_in_short[i+1] - sell_funds
			cash_in_bank[i+1] = cash_in_bank[i+1] + sell_funds
		}
		
		ETF_newvalues = c(ETF_newvalues, ETF_newvalues[i] * (1+ETF_change[i]))
		
		# adjusting the bond portfolio
		cash_in_bond[i+1] = cash_in_bond[i] * (1 + bond_yield/253)
		if(cash_in_bank[i+1] > cash_in_bank[i])	{
			bond_buys = (cash_in_bank[i+1] - cash_in_bank[i]) * bond_buy_rate
			cash_in_bond[i+1] = cash_in_bond[i+1] + bond_buys
			cash_in_bank[i+1] = cash_in_bank[i+1] - bond_buys
		}
		
	}
	
	
	
	output = data.frame(cbind(cash_in_bank, cash_in_long, cash_in_short, ETF_newvalues))
	return(output)
}










n_runs = 1000

beta_long_dist = runif(n_runs,1.65,1.76)		#,1.2,2.2)			#1.1,1.8)
beta_short_dist = runif(n_runs,0.23,0.43)		#,1.2,2.2)			#1.1,1.8)
portfolio_beta_min_dist = runif(n_runs,.28,.42)		#,.14,.41)		#.1,.5)
portfolio_beta_max_dist = runif(n_runs,.28,.38)		#,.16,.42)		#.1,.5)
buy_rate_scalar_dist = runif(n_runs,.08,.23)			#,.08,.16)		#0.01,.17)
sell_rate_scalar_dist = runif(n_runs,.07,.19)		#,.008,.13)		#0.01,.17)
buy_rate_exponent_dist = runif(n_runs,.90,1.20)		#,1,1.35)		#1,1.8)
sell_rate_exponent_dist = runif(n_runs,1.32,1.54)		#,1.17,1.9)		#1,1.8)

yearly_subsample_start = floor(runif(n_runs, 1, nrow(mp)-(10*253)))
	
calib_output = data.frame(portfolio_beta_min=rep(NA,n_runs), portfolio_beta_max=rep(NA,n_runs), 
	buy_rate_scalar=rep(NA,n_runs), sell_rate_scalar=rep(NA,n_runs), buy_rate_exponent=rep(NA,n_runs), sell_rate_exponent=rep(NA,n_runs),
	beta_long=rep(NA,n_runs), beta_short=rep(NA,n_runs),
	final_returns_5th = rep(NA,n_runs),
	relative_returns_5th = rep(NA,n_runs),
	final_returns_25th = rep(NA,n_runs),
	relative_returns_25th = rep(NA,n_runs),
	final_returns_50th = rep(NA,n_runs),
	relative_returns_50th = rep(NA,n_runs),
	final_returns_75th = rep(NA,n_runs),
	relative_returns_75th = rep(NA,n_runs))

loss_likelihood = 0.5 # if n% of model runs generate returns below the index, then we toss that variable set

while(kk <= n_runs)	{
kk=kk+1
	
calib_output$beta_long[kk] = sample(beta_long_dist,1)
calib_output$beta_short[kk] = sample(beta_short_dist,1)
calib_output$portfolio_beta_min[kk] = sample(portfolio_beta_min_dist,1)
calib_output$portfolio_beta_max[kk] = sample(portfolio_beta_max_dist,1)
calib_output$buy_rate_scalar[kk] = sample(buy_rate_scalar_dist,1)
calib_output$sell_rate_scalar[kk] = sample(sell_rate_scalar_dist,1) 
calib_output$buy_rate_exponent[kk] = sample(buy_rate_exponent_dist,1)
calib_output$sell_rate_exponent[kk] = sample(sell_rate_exponent_dist,1)

	fin_returns = NULL
	rel_returns = NULL
	for(ll in 1:1000){
		sample_start = sample(yearly_subsample_start,1)
		SnP_sub = mp[sample_start:(sample_start+10*253),]		# vector of historical SnP data
		
		test = lsb_model(
			Market_Index = SnP_sub, # value of ETF we are trading into and out of; vector
			cash_ratio = .4,		# the initial cash ratio (updated according to market conditions
			starting_cash = 1, # initial value; will change with market conditions bought / sold toward
			beta_long = calib_output$beta_long[kk],	# this is the ideal value of beta relative to longs
			beta_short = calib_output$beta_short[kk],	# this is the ideal value of beta relative to shorts
			portfolio_beta_min = calib_output$portfolio_beta_min[kk],	# the minim
			portfolio_beta_max = calib_output$portfolio_beta_max[kk],	# the maximum value allowed for the portfolio beta
			buy_rate_scalar = calib_output$buy_rate_scalar[kk],
			sell_rate_scalar = calib_output$sell_rate_scalar[kk], 
			portfolio_split = 10, 	# ratio for splitting the portfolio between shorts and longs
			max_buy_rate = .2,
			max_sell_rate = .2,
			buy_rate_exponent = calib_output$buy_rate_exponent[kk],	# exponential modifier of buy rates
			sell_rate_exponent = calib_output$sell_rate_exponent[kk], # exponential modifier of sell rates
			bond_buy_rate = 0.1, # the rate at which we are willing to buy bonds relatie to gains in our bank account
			bond_yield = 0.01, # the annual yield on our bonds
			beta_scalar = 3,	# leverage of cash_in_market
			randomize = TRUE
			)
		
		fin_returns = c(fin_returns, last(test$cash_in_bank) + last(test$cash_in_long) + last(test$cash_in_short))
		rel_returns = c(rel_returns, fin_returns[ll] -
			(starting_cash * last(test$ETF_newvalues) / test$ETF_newvalues[1]))

	}

if(length(which(rel_returns <= 0)) <= loss_likelihood*100)	{
	calib_output$final_returns_5th[kk] = quantile(fin_returns, .05, na.rm=TRUE)
	calib_output$relative_returns_5th[kk] = quantile(rel_returns, .05, na.rm=TRUE)
	calib_output$final_returns_25th[kk] = quantile(fin_returns, .25, na.rm=TRUE)
	calib_output$relative_returns_25th[kk] = quantile(rel_returns, .25, na.rm=TRUE)
	calib_output$final_returns_50th[kk] = quantile(fin_returns, .50, na.rm=TRUE)
	calib_output$relative_returns_50th[kk] = quantile(rel_returns, .50, na.rm=TRUE)
	calib_output$final_returns_75th[kk] = quantile(fin_returns, .75, na.rm=TRUE)
	calib_output$relative_returns_75th[kk] = quantile(rel_returns, .75, na.rm=TRUE)
	} else {
	calib_output$final_returns_5th[kk] = NA
	calib_output$relative_returns_5th[kk] = NA
	calib_output$final_returns_25th[kk] = NA
	calib_output$relative_returns_25th[kk] = NA
	calib_output$final_returns_50th[kk] = NA
	calib_output$relative_returns_50th[kk] = NA
	calib_output$final_returns_75th[kk] = NA
	calib_output$relative_returns_75th[kk] = NA
}
	plot(test$cash_in_bank + test$cash_in_long + test$cash_in_short, type = 'l', col='red3', lwd=3, main=paste(kk))
	abline(h=1, lty=3, lwd=1)
	lines((starting_cash * test$ETF_newvalues / test$ETF_newvalues[1]), col='black', lwd=3)
}

sorted_output = subset(calib_output, !is.na(relative_returns_25th))
sorted_output = sorted_output[rev(order(sorted_output$relative_returns_25th)),] 

head(sorted_output,10)







































########################################################################
#### same model, except we now have multiple indices in our portfolio

multi_model = function(
	Market_Index =list(), # value of ETF we are trading into and out of; a list of each index
	cash_ratio = .1 ,		# the initial cash ratio (updated according to market conditions
	starting_cash = 1 , # initial value; will change with market conditions
	beta_portfolio = 1.4,	# this is the ideal value that is bought / sold toward
	portfolio_beta_min = .5,	# the minimum value allowed for the portfolio beta
	portfolio_beta_max = .5,	# the maximum value allowed for the portfolio beta
	buy_rate_scalar = .1,
	sell_rate_scalar = .1,
	max_buy_rate = .2,
	max_sell_rate = .2,
	buy_rate_exponent = 1,	# exponential modifier of buy rates
	sell_rate_exponent = 1, # exponential modifier of sell rates
	beta_scalar = 3,	# leverage of cash_in_market
	randomize = TRUE)	{
	
		
	ETF_change = if(randomize)	{
		sample(diff(Market_Index[[1]]$Close) / Market_Index[[1]]$Close[-length(Market_Index[[1]]$Close)], replace=TRUE)
		} else {
			diff(Market_Index[[1]]$Close) / Market_Index[[1]]$Close[-length(Market_Index[[1]]$Close)]
		}
	
	num_indices = length(Market_Index)
	for(mm in 2:num_indices)	{
		ETF_change_each = if(randomize)	{
			sample(diff(Market_Index[[mm]]$Close) / Market_Index[[mm]]$Close[-length(Market_Index[[mm]]$Close)], replace=TRUE)
			} else {
				diff(Market_Index[[mm]]$Close) / Market_Index[[mm]]$Close[-length(Market_Index[[mm]]$Close)]
			}
		ETF_change = cbind(ETF_change, ETF_change_each)
	}
	ETF_newvalues = matrix(starting_cash, ncol=num_indices)
	colnames(ETF_newvalues) = 1:num_indices
	
	#Initializing the distribution of cash
	cash_in_bank = starting_cash * (1 - cash_ratio)
	cash_in_mrkt = starting_cash * cash_ratio
	buy_sell_rate = NA
	cash_total = starting_cash
	invest_distribution = rep(1/num_indices,num_indices)	# for distributing cash among the indices; this can vary if we want it ti
	
	for (i in 1:nrow(ETF_change))	{						# calculating the gains/loss from each index
		daily_gains_losses = sum(cash_in_mrkt[i] * invest_distribution * ETF_change[i,] * beta_scalar)
		
		cash_in_mrkt[i+1] = cash_in_mrkt[i]	+ daily_gains_losses # calculate end of day cash in market
		cash_total[i+1] = cash_in_mrkt[i+1] + cash_in_bank[i]
		current_beta_portfolio = cash_in_mrkt[i+1] * beta_scalar / cash_total[i+1]

		#cash_in_bank[i+1] = cash_in_bank[i]
		# check to see if the portfolio needs to be adjusted
		if(current_beta_portfolio < beta_portfolio)	{
			buy_rate = buy_rate_scalar * ((beta_portfolio - current_beta_portfolio) / portfolio_beta_min) ^ buy_rate_exponent
			buy_sell_rate = min(buy_rate, max_buy_rate)
			buy_funds = cash_in_bank[i] * buy_sell_rate
			cash_in_mrkt[i+1] = cash_in_mrkt[i+1] + buy_funds
			cash_in_bank[i+1] = cash_in_bank[i] - buy_funds
		}
		if(current_beta_portfolio > beta_portfolio)	{
			sell_rate = sell_rate_scalar * ((current_beta_portfolio - beta_portfolio) / portfolio_beta_max) ^ sell_rate_exponent
			buy_sell_rate = min(sell_rate, max_sell_rate)
			sell_funds = cash_in_mrkt[i] * buy_sell_rate
			cash_in_mrkt[i+1] = cash_in_mrkt[i+1] - sell_funds
			cash_in_bank[i+1] = cash_in_bank[i] + sell_funds
		}
		ETF_newvalues = rbind(ETF_newvalues, ETF_newvalues[i,] * (1+ETF_change[i,]))
	}
	if(any(cash_in_mrkt < 0)) stop('cash_in_mrkt < 0')
	if(any(cash_in_bank < 0)) stop('cash_in_bank < 0')
	
	output = data.frame(cbind(cash_in_bank, cash_in_mrkt, buy_sell_rate, ETF_newvalues))
	return(output)
}


plot(cash_in_bank+cash_in_mrkt, type='l', lwd=3, ylim=c(.7,1.3))
lines(output[,4])
lines(output[,5])
lines(output[,6])
lines(output[,7])









n_runs = 10000
test_output = data.frame(beta_portfolio=rep(NA,n_runs),
	portfolio_beta_min=rep(NA,n_runs), portfolio_beta_max=rep(NA,n_runs), 
	max_buy_rate=rep(NA,n_runs), max_sell_rate=rep(NA,n_runs),
	buy_rate_scalar=rep(NA,n_runs), sell_rate_scalar=rep(NA,n_runs),
	buy_rate_exponent=rep(NA,n_runs), sell_rate_exponent=rep(NA,n_runs),
	final_returns_5th=rep(NA,n_runs), relative_returns_5th=rep(NA,n_runs),
	final_returns_25th=rep(NA,n_runs), relative_returns_25th=rep(NA,n_runs),
	final_returns_50th=rep(NA,n_runs), relative_returns_50th=rep(NA,n_runs),
	final_returns_75th=rep(NA,n_runs), relative_returns_75th=rep(NA,n_runs))

#kk=0
beta_portfolio_dist = runif(n_runs,1.8,2.4)#		1.2,1.45)		#,1.2,2.2)			#1.1,1.8)
portfolio_beta_min_dist = runif(n_runs,.4,.8)		#,.14,.41)		#.1,.5)
portfolio_beta_max_dist = runif(n_runs,.15,.3)		#,.16,.42)		#.1,.5)
max_buy_rate_dist = runif(n_runs,.11,.4)			#
max_sell_rate_dist = runif(n_runs,.11,.4)			#
buy_rate_scalar_dist = runif(n_runs,.1,.16)		#,.08,.16)		#0.01,.17)
sell_rate_scalar_dist = runif(n_runs,.1,.29)		#,.008,.13)		#0.01,.17)
buy_rate_exponent_dist = runif(n_runs,1.0,2.0)		#,1,1.35)		#1,1.8)
sell_rate_exponent_dist = runif(n_runs,.8,1.8)		#,1.17,1.9)		#1,1.8)

		
num_years = 1
the_timeframe = as.Date(c("1992-01-02", "2021-01-01"))
Market_Indices_long = list(										# vector of historical SnP data
			subset(mp, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
			subset(dj, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
			subset(ndq, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
			subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2])
			)		
numb_rows = nrow(Market_Indices_long[[1]])
start_rows = 1:(numb_rows - num_years*254)


#kk=0
while(kk <= n_runs)	{
kk=kk+1

test_output$beta_portfolio[kk] = sample(beta_portfolio_dist, 1)
test_output$portfolio_beta_min[kk] = sample(portfolio_beta_min_dist, 1)
test_output$portfolio_beta_max[kk] = sample(portfolio_beta_max_dist, 1)
test_output$max_buy_rate[kk] = sample(max_buy_rate_dist, 1)
test_output$max_sell_rate[kk] = sample(max_sell_rate_dist, 1)
test_output$buy_rate_scalar[kk] = sample(buy_rate_scalar_dist, 1)
test_output$sell_rate_scalar[kk] = sample(sell_rate_scalar_dist, 1)
test_output$buy_rate_exponent[kk] = sample(buy_rate_exponent_dist, 1)
test_output$sell_rate_exponent[kk] = sample(sell_rate_exponent_dist, 1)


	fin_returns = NULL
	mrkt_returns = NULL
	rel_returns = NULL
	for(ll in 1:1000){
		sample_start = sample(start_rows, 1)
		n_indices = length(Market_Indices_long)	
		for(pp in 1:n_indices)	{Market_Indices[[pp]] = Market_Indices_long[[pp]][sample_start:(sample_start+num_years*253),]}
		
		test = multi_model(
			Market_Index = Market_Indices, # value of ETF we are trading into and out of; vector
			cash_ratio = .1,		# the initial cash ratio (updated according to market conditions
			starting_cash = 1, # initial value; will change with market conditions
			beta_portfolio = test_output$beta_portfolio[kk],	# this is the ideal value that is bought / sold toward
			portfolio_beta_min = test_output$portfolio_beta_min[kk],	# the minimum value allowed for the portfolio beta
			portfolio_beta_max = test_output$portfolio_beta_max[kk],	# the maximum value allowed for the portfolio beta
			buy_rate_scalar = test_output$buy_rate_scalar[kk],
			sell_rate_scalar = test_output$sell_rate_scalar[kk], 
			max_buy_rate = test_output$max_buy_rate_scalar[kk],
			max_sell_rate = test_output$max_sell_rate_scalar[kk],
			buy_rate_exponent = test_output$buy_rate_exponent[kk],	# exponential modifier of buy rates
			sell_rate_exponent = test_output$sell_rate_exponent[kk], # exponential modifier of sell rates
			beta_scalar = 3,	# leverage of cash_in_market
			randomize = FALSE
		)
					
		fin_returns = c(fin_returns, last(test$cash_in_bank) + last(test$cash_in_mrkt))
		mrkt_returns = c(mrkt_returns, sum(last(test[,4:(4+n_indices-1)])) / n_indices)
		rel_returns = c(rel_returns, fin_returns[ll] - mrkt_returns[ll])
	}

	plot(test$cash_in_bank + test$cash_in_mrkt, type = 'l', col='red3', lwd=3, main=paste(kk))
		
	first_returns_col = which(names(test) == "X1")
	returns_each = NULL
	for (gg in first_returns_col:(first_returns_col + (n_indices-1)))	{
		returns_each = c(returns_each, starting_cash * last(test[,gg]) / test[1,gg])
		lines((starting_cash * test[,gg] / test[1,gg]), col='black', lwd=2)
		}
	
	test_output$final_returns_5th[kk] = quantile(fin_returns, .05, na.rm=TRUE)
	test_output$relative_returns_5th[kk] = quantile(rel_returns, .05, na.rm=TRUE)
	test_output$final_returns_25th[kk] = quantile(fin_returns, .25, na.rm=TRUE)
	test_output$relative_returns_25th[kk] = quantile(rel_returns, .25, na.rm=TRUE)
	test_output$final_returns_50th[kk] = quantile(fin_returns, .50, na.rm=TRUE)
	test_output$relative_returns_50th[kk] = quantile(rel_returns, .50, na.rm=TRUE)
	test_output$final_returns_75th[kk] = quantile(fin_returns, .75, na.rm=TRUE)
	test_output$relative_returns_75th[kk] = quantile(rel_returns, .75, na.rm=TRUE)
}


sorted_output = subset(test_output, !is.na(relative_returns_25th))
sorted_output = sorted_output[rev(order(sorted_output$relative_returns_75th)),] 

head(sorted_output,20)


length(which(test_output$relative_returns < 0)) / n_runs
summary(test_output$relative_returns)
boxplot(test_output$relative_returns)








	# now validating the model
num_years = 10
n_runs = 1000
the_timeframe = as.Date(c("1992-01-02", "2021-01-01"))
the_timeframe = as.Date(c("2002-08-01", "2021-01-01"))
the_timeframe = as.Date(c("1992-02-01", "2021-01-01"))

Market_Indices_long = list(										
#	subset(mp, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
#	subset(dj, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
#	subset(ndq, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
	subset(rut, Date >= the_timeframe[1] & Date <= the_timeframe[2])
#	subset(tlt, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
#	subset(ief, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
#	subset(gcf, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
#	subset(nik, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
#	subset(xau, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
#	subset(hsi, Date >= the_timeframe[1] & Date <= the_timeframe[2]),
#	subset(gdaxi, Date >= the_timeframe[1] & Date <= the_timeframe[2])
	)
			
numb_rows = nrow(Market_Indices_long[[1]])
start_rows = 1:(numb_rows - num_years*254)

	valid_output = data.frame(beta_portfolio=rep(NA,n_runs),
		portfolio_beta_min=rep(NA,n_runs), portfolio_beta_max=rep(NA,n_runs), 
		max_buy_rate=rep(NA,n_runs), max_sell_rate=rep(NA,n_runs),
		buy_rate_scalar=rep(NA,n_runs), sell_rate_scalar=rep(NA,n_runs),
		buy_rate_exponent=rep(NA,n_runs), sell_rate_exponent=rep(NA,n_runs),
		market_returns=rep(NA,n_runs), relative_returns=rep(NA,n_runs), model_returns=rep(NA,n_runs))
kk=0
while(kk < n_runs)	{
kk=kk+1
sample_start = sample(start_rows, 1)
n_indices = length(Market_Indices_long)	
Market_Indices = list()
for(pp in 1:n_indices)	{Market_Indices[[pp]] = Market_Indices_long[[pp]][sample_start:(sample_start+num_years*253),]}
			
valid_output$beta_portfolio[kk] = sorted_output$beta_portfolio[1]
valid_output$portfolio_beta_min[kk] = sorted_output$portfolio_beta_min[1]
valid_output$portfolio_beta_max[kk] = sorted_output$portfolio_beta_max[1]
valid_output$max_buy_rate[kk] = sorted_output$max_buy_rate[1]
valid_output$max_sell_rate[kk] = sorted_output$max_sell_rate[1]
valid_output$buy_rate_scalar[kk] = sorted_output$buy_rate_scalar[1]
valid_output$sell_rate_scalar[kk] = sorted_output$sell_rate_scalar[1]
valid_output$buy_rate_exponent[kk] = sorted_output$buy_rate_exponent[1]
valid_output$sell_rate_exponent[kk] = sorted_output$sell_rate_exponent[1]

	test = multi_model(
			Market_Index = Market_Indices, # value of ETF we are trading into and out of; vector
			cash_ratio = .1,		# the initial cash ratio (updated according to market conditions
			starting_cash = 1, # initial value; will change with market conditions
			beta_portfolio = valid_output$beta_portfolio[kk],	# this is the ideal value that is bought / sold toward
			portfolio_beta_min = valid_output$portfolio_beta_min[kk],	# the minimum value allowed for the portfolio beta
			portfolio_beta_max = valid_output$portfolio_beta_max[kk],	# the maximum value allowed for the portfolio beta
			buy_rate_scalar = valid_output$buy_rate_scalar[kk],
			sell_rate_scalar = valid_output$sell_rate_scalar[kk], 
			max_buy_rate = valid_output$max_buy_rate[kk],
			max_sell_rate = valid_output$max_sell_rate[kk],
			buy_rate_exponent = valid_output$buy_rate_exponent[kk],	# exponential modifier of buy rates
			sell_rate_exponent = valid_output$sell_rate_exponent[kk], # exponential modifier of sell rates
			beta_scalar = 3,	# leverage of cash_in_market
			randomize = TRUE
	)
	

	valid_output$model_returns[kk] = last(test$cash_in_bank) + last(test$cash_in_mrkt)
	valid_output$market_returns[kk] = sum(last(test[,4:(4+n_indices-1)])) / n_indices
	valid_output$relative_returns[kk] = valid_output$model_returns[kk] - valid_output$market_returns[kk]

	plot(test$cash_in_bank + test$cash_in_mrkt, type = 'l', col='red3', lwd=3, main=paste(kk))
		
	first_returns_col = which(names(test) == "X1")
	returns_each = NULL
	for (gg in first_returns_col:(first_returns_col + (n_indices-1)))	{
		returns_each = c(returns_each, starting_cash * last(test[,gg]) / test[1,gg])
		lines((starting_cash * test[,gg] / test[1,gg]), col='black', lwd=2)
	}
}

length(which(valid_output$relative_returns < 0)) / n_runs
summary(valid_output$relative_returns)

boxplot(valid_output[, c("market_returns","model_returns")], ylim=c(0,25),lwd=2,
	main=paste("4 Indices,",
		num_years,
		"yrs, \nhistorical", first(year(Market_Indices_long[[1]]$Date)), "to", last(year(Market_Indices_long[[1]]$Date))),
	xlab = NULL, ylab="Total Returns [-]"
	)
abline(h=c(0,.5,1,1.5,2,3,4,5,10,20,30,40), lty=c(1,3,2,3,2,2,2,2,1,1,1,1), col='grey80')
	



































### machine learning models

library(data.table)
library(lubridate)
library(CAST)
library(caret)

setwd("C:\\Users\\arik\\Documents\\money_please")

mp = fread('C:\\Users\\arik\\Documents\\money_please\\GSPC.csv')
#mp$Date_ = as.Date(mp$Date)

mp$pct = (mp$Close - mp$Close) / mp$Close
mp$vlm_chng = c(NA, diff(as.numeric(mp$Volume)))
mp$vlm_pctChng = mp$vlm_chng / mp$Volume
mp$Day = wday(mp$Date, label=TRUE)
mp$Month = month(mp$Date)
mp$prv1 = c(NA, mp$pct[-nrow(mp)])
mp$prv2 = c(rep(NA, 2), mp$pct[-c((nrow(mp)-1):nrow(mp))])
mp$prv3 = c(rep(NA, 3), mp$pct[-c((nrow(mp)-2):nrow(mp))])
mp$prv4 = c(rep(NA, 4), mp$pct[-c((nrow(mp)-3):nrow(mp))])
mp$prv5 = c(rep(NA, 5), mp$pct[-c((nrow(mp)-4):nrow(mp))])
mp$prv2vol = c(rep(NA, 1), mp$vlm_pctChng[-c((nrow(mp)-0):nrow(mp))])
mp$prv3vol = c(rep(NA, 2), mp$vlm_pctChng[-c((nrow(mp)-1):nrow(mp))])
mp$prv4vol = c(rep(NA, 3), mp$vlm_pctChng[-c((nrow(mp)-2):nrow(mp))])
mp$prv5vol = c(rep(NA, 4), mp$vlm_pctChng[-c((nrow(mp)-3):nrow(mp))])


mp_test = mp[-c(1:5),]


# use ffs() to identify our only significant vars
ffsmodel_LLO <- ffs(mp_test[,c(11:21)],
					mp_test$pct,
#					metric="RMSE",
					metric="Rsquared",
                   method="rf",
#					method="avNNet",
					tuneLength=2,
					verbose=FALSE,
                    trControl=trainControl(method="cv"))#,
					#index = indices$index))
ffsmodel_LLO


mp_test$year = year(mp_test$Date)


set.seed(2)
validation = sample(nrow(mp_test), ceiling(nrow(mp_test) * .2))
indices <- CreateSpacetimeFolds(mp_test[-validation,],spacevar = "year",
	k=5)																# k=5 to crossvalidate on 80:20

selected_vars = names(mp_test[,c(11:21)])


# establish a baseline for identifying good variables
model_LLO <- train(mp_test[-validation,
					#ffsmodel_LLO$selectedvars,
					selected_vars,
					with=FALSE],
				unlist(mp_test[-validation,"pct"]),			# initial model
                method = "rf",
				tuneLength = length(
					#ffsmodel_LLO$selectedvars),
					selected_vars),
				importance = TRUE,
                trControl = trainControl(method="cv",
					index = indices$index))
model_LLO																# just checking to see what it looks like



pred_data_ffs = predict(model_LLO, mp_test[validation,])


valid_mae = MAE(mp_test$pct[validation], pred_data_ffs)
valid_rmse = RMSE(mp_test$pct[validation], pred_data_ffs)
fit <- lm(pred_data_ffs  ~ mp_test$pct[validation])
valid_sd = sd(summary(fit)$residuals)
valid_r2 = summary(fit)$r.squared



#maxval = max(c(pred_data_ffs, mp_test$pct[validation]))
plot(mp_test$pct[validation], pred_data_ffs, col=alpha('grey10',.2), lwd=1.5,
	ylab="Model Prediction (% change / day)",
	xlab="Actual Value (% change / day)")
abline(0,1, lty=3,lwd=3,col='red3')
abline(fit$coeff[1], fit$coeff[2], col='blue3', lwd=3, lty=1)
text(x=-.05,y=.02,paste0('r2=',round(valid_r2,4)))
text(x=-.05,y=.0175,paste0('MAE=',round(valid_mae,4)))
text(x=-.05,y=.015,paste0('RMSE=',round(valid_rmse,4)))
text(x=.06,y=-.005,"one-to-one line", col="red3")
text(x=.06,y=-.0075,paste0("line of best fit; slope=", round(fit$coeff[2],2)), col="blue3")


chosen_year = which(mp_test$year == 2019)[1:30]
fit <- lm(mp_test$predictions[chosen_year]  ~ mp_test$pct[chosen_year])
summary(fit)$r.squared
valid_sd = sd(summary(fit)$residuals)
valid_r2 = summary(fit)$r.squared


mp_test$predictions = predict(model_LLO, mp_test)
plot(as.Date(mp_test$Date[chosen_year]), mp_test$pct[chosen_year], col='grey10', lwd=3, type='l',
	ylab="% change / day",
	xlab="Date")
abline(h=mean(mp_test$pct), col='red')
lines(as.Date(mp_test$Date[chosen_year]), mp_test$predictions[chosen_year], col='blue3', lwd=3, lty=2)
points(as.Date(mp_test$Date[chosen_year]), mp_test$predictions[chosen_year], col='blue3', lwd=3, lty=2)
lines(as.Date(mp_test$Date[chosen_year]), mp_test$predictions[chosen_year]+valid_mae, col='blue3', lwd=1.5, lty=2)
lines(as.Date(mp_test$Date[chosen_year]), mp_test$predictions[chosen_year]-valid_mae, col='blue3', lwd=1.5, lty=2)

NSE(mp_test$pct, mp_test$predictions)






validation_results = cbind(validation, pred_data_ffs, mp_test$B_depth_dry[validation])
	setwd("C:\\Users\\arik\\Documents\\PhD Research\\D3\\models")
	write.csv(validation_results, "valid_data_depth_dry.csv")
	write.csv(ffsmodel_LLO$selectedvars, "selected_vars_depth_dry.csv")

best_mtry = as.numeric(model_LLO$bestTune)
best_mtry_row = which(model_LLO$results$mtry == best_mtry)
cv_mae = model_LLO$results$MAE[best_mtry_row]
cv_rmse = model_LLO$results$RMSE[best_mtry_row]
#cv_sd = model_LLO$results$MAE[best_mtry_row]
cv_r2 = model_LLO$results$Rsquared[best_mtry_row]

model_results_depth_dry = data.frame(cbind(valid_mae, valid_rmse, valid_sd, valid_r2,
	cv_mae, cv_rmse, cv_r2), row.names="depth_dry")
write_feather(model_results_depth_dry, "model_results_depth_dry.feather")


	# using the model to predict on NHD huc12 dat
	# replacing NAs at HUC12s with 0s (this isn't efficient... need to vectorize)

for(i in ffsmodel_LLO$selectedvars)	{
	for(j in 1:nrow(huc12_wNHDatts_vars))		{
		if(is.na(huc12_wNHDatts_vars[j,i])) huc12_wNHDatts_vars[j,i] = 0	
	}
}


pred_data_NHD = predict(model_LLO, huc12_wNHDatts_vars[,ffsmodel_LLO$selectedvars])
huc12_wNHDatts_vars$B_depth_dry = NA
huc12_wNHDatts_vars$B_depth_dry = pred_data_NHD


