  " <H3>COUNTRY SPECIFIC </H3>
		<H4>Data source</H4>
	<p>Data on COVID-19 diagnosed cases and mortality from January 1st, 2020, onwards, is collected from the <a href='https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide'>European Centre for Disease Prevention and Control geographical distribution of COVID-19 cases worldwide (ECDC, 2020).</a> </p>
		<br>              
		<H4>Trend analysis and projections</H4>
                <p>To estimate the daily trends and calculate short-term projections, we used Poisson regression model (Dyba and Hakulinen, 2000), corrected for over-dispersion (Navarro et al., 2001), adjusting linear, quadratic and cubic trends:</p>
 
		$$\\log(E(c_t))= \\beta_0 + \\beta_1  t + \\beta_2  t^2 + \\beta_3  t^3$$
      
		<p>where \\(t=1,2,\\ldots,T\\) is the time unit (from the first day to the last observed, \\(T\\) consecutive days in total), and \\(c_t\\) is the daily number of events.</p>
		<p>The three-day projections, and their 95% confidence interval, are obtained from the parameters estimated by the model. Trends and projections are also calculated by age group (0-39, 40-49, 50-59, 60-69, 70-79, and 80 or more years).</p>
		<p>In previous versions of the application, an alternative model was also considered, including only the linear trend and linear trend plus quadratic term, and the models were compared using a similarity ratio test. Based on the evolution of the epidemic, we observed that the best fit was provided by the cubic model, described above, making it the model used in the current version. Except for incidecent cases and mortality that we use a quadrati model. The goodness of fit of the models is regularly evaluated if a reformulation is necessary, to provide a better fit of the data during the epidemic.</p>
		<br>
		<H4>Case fatality rate</H4>
		<p>The case fatality rate is the ratio of the deceased among the diagnosed cases (Rothman and Greenland, 1998). We used a Poisson regression model (Dyba and Hakulinen, 2000), corrected for over-dispersion (Dyba and Hakulinen, 2000), adjusting linear, quadratic and cubic trends, and fitting an offset term with the logarithm of the diagnosed cases:</p>

		$$\\log(E(m_t))= \\beta_0 + \\beta_1  t + \\beta_2  t^2 + \\beta_3  t^3 + \\log(c_t)$$
      
		<p>where \\(m_t\\) is the dalily counts of mortality, and \\(c_t\\) the disgnosed cases. The case fatality rate is also calculated for the same age groups.</p>
		<p>Currently, it is not possible to make an accurate estimate of the case fatality rate due to the degree of underreporting of cases diagnosed in official statistics (Battegay et al., 2020). Although its estimation and monitoring is of interest considering this limitation.</p>
		<br>
		
		<H4>Estimation of time to infection</H4>
		<p>Lauer et al. (Lauer et al., 2020) have recently analyzed the incubation period for COVID-19 in a cohort of symptomatic patients. From each patient, they collected the interval of exposure to SARS-CoV-2 and the date of appearance of symptoms. They assumed that the incubation time would follow, as in other viral respiratory tract infections, a Lognormal distribution.</p>

		$$\\ Lognormal(\\mu,\\sigma^2) = Lognormal(1.621 , 0.418)$$

		<p>In a naïve exercise with some limitations, we have replicated this distribution in the group of diagnosed cases to approximate the date of exposure to SARS-CoV-2 recursively:</p>

		$$\\ q(i)= \\sum_{j=1}^{14} P(j)*c_{j+i}$$

		<p>where \\(q\\) is the number of infected cases on day \\(i\\), \\(c\\) is the number of diagnosted cases on day \\(j+i\\), and \\(P(j)\\) is the probability of presenting symptoms on day \\(j\\) according to a Lognormal law with the parameters defined by Lauer et al. (Lauer et al., 2020)</p>
		<p>To estimate the last 14 days, since the information on the diagnosed cases was not available for the next 14 days, the quadratic model described previously was used to project diagnosed cases. These latest estimates are displayed in the application with a different color.</p>
		<br>
		<H4>R index evolution</H4>
			<p>The reproduction number, \\(R\\), is the average number of secondary cases of disease caused by a single infected individual over his or her infectious period. This statistic, which is time and situation specific, is commonly used to characterize pathogen transmissibility during an epidemic. The monitoring of \\(R\\) over time provides feedback on the effectiveness of interventions and on the need to intensify control efforts, given that the goal of control efforts is to reduce \\(R\\) below the threshold value of 1 and as close to 0 as possible, thus bringing an epidemic under control.</p>
		<p>Here, we use the package EpiEstim to obtain the reproduction number \\(R_t\\) using the Wallinga and Teunis method. This parametric method assumes a gamma distribution for the serial interval. The serial interval is the time between the onset of symptoms in a primary case and the onset of symptoms of secondary cases, which is needed to estimated \\(R_t\\) over the course of an epidemic.</p>
		<p>The mean and standard deviation of the serial interval distribution can vary depending on the disease. For measles it was estimated a mean (SD) of 14.9 (3.9) days (Groendyke et al.); for pandemic influenza 2.6 (1.5) (Ferguson et al.; Boelle et al.); for Smallpox 22.4 (6.1) (Riley and Ferguson); for SARS 8.4 (3.8) (Lipsitch et al.).</p>
		<p>A recent paper by Nishiura et al. (Serial interval of novel coronavirus (COVID-19) infections. Int J Infect Dis. 2020 Mar 4;93:284-286) estimated a mean and standard deviation for COVID-19 of 4.7 days (95% CrI: 3.7, 6.0) and 2.9 days (95% CrI: 1.9, 4.9), respectively, which is much shorteer than of SARS and MERS. These are the values, 4.7 (2.9), that we are using in our analyses.</p>
		<p>For the calculation the EpiEstim R package, developed by Cori et al (American Journal of Epidemiology, 2013), is used.</p>
		<br>
		<H4>Limitations </H4>
		
		<p>Estimates might not be accurate due to the underreporting of diagnosed cases and mortality in official statistics (<a href='https://www.ncbi.nlm.nih.gov/pubmed/32031234'>Battegay et al. 2020</a>). </p>
		
		<p>The selection of the number of people who have been tested is critical for an accurate estimation (<a href='https://www.ncbi.nlm.nih.gov/pubmed/32324422'>Pearce et al. 2020</a>). Accurate estimation of the rates depends on the testing strategy, the prevalence of infection, and the test accuracy. Differences between countries or overtime may merely reflect differences in selection for testing and test performance (<a href='https://www.ncbi.nlm.nih.gov/pubmed/32324422'>Pearce et al. 2020</a>).</p> 
		
		<p>The application does not take into account the changes in the definition of diagnosed cases, nor the lockdown measures are undertaken in each country, aiming to flatten the curve.</p>
		
		<br>
		<H4>Refefences</H4>
		<ul>
				<li>Battegay M, Kuehl R, Tschudin-Sutter S, Hirsch HH, Widmer AF, Neher RA.<a href='https://www.ncbi.nlm.nih.gov/pubmed/32031234'>2019-novel Coronavirus (2019-nCoV): estimating the case fatality rate - a word of caution</a>. Swiss Med Wkly. 2020;150:w20203.</li>
		<li>Cori A, Ferguson NM, Fraser C, Cauchemez S. <a href='https://www.ncbi.nlm.nih.gov/pubmed/24043437'>A new framework and software to estimate time-varying reproduction numbers during epidemics</a>. Am J Epidemiol. 2013;178(9):1505-12.</li>

		<li>Dyba T, Hakulinen T. <a href='https://onlinelibrary.wiley.com/doi/abs/10.1002/1097-0258(20000715)19:13<1741::AID-SIM496>3.0.CO;2-O'> Comparison of different approaches to incidence prediction based on simple interpolation techniques</a>. Statistics in Medicine 2000; 19: 1741-1752.</li>
					<li>European Centre for Disease Prevention and Control. Download today’s data on the geographic distribution of COVID-19 cases worldwide Stockholm. Available from: <a href='https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide/'> https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide/ </a>.</li>
						<li>Lauer SA, Grantz KH, Bi Q, Jones FK, Zheng Q, Meredith HR, et al. <a href='https://www.acpjournals.org/doi/10.7326/M20-0504'> The Incubation Period of Coronavirus Disease 2019 (COVID-19) From Publicly Reported Confirmed Cases: Estimation and Application </a>. Ann Intern Med 2020.</li>
			<li>Navarro A, Utzet F, Puig P, Caminal J, Martín M. <a href='https://www.gacetasanitaria.org/es-la-distribucion-binomial-negativa-frente-articulo-resumen-S0213911101715993'> La distribución binomial negativa frente a la de Poisson en el análisis de fenómenos recurrentes</a>. Gaceta Sanitaria 2001; 15: 447-452.</li>
						<li>Pearce N, Vandenbroucke JP, VanderWeele TJ, Greenland S.<a href='https://www.ncbi.nlm.nih.gov/pubmed/32324422'>Accurate Statistics on COVID-19 Are Essential for Policy Guidance and Decisions</a>. Am J Public Health. 2020:e1-e3.</li>
			<li>Rothman K, Greenland S. Modern epidemiology. Philadelphia, PA: Lippincott-Raven Publishers, 1998.</li>
		</ul>
			
"