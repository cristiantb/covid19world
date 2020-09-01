  " 	 	<H3> ESPECÍFIC DEL PAÍS </H3>
		<H4>Font de dades</H4>
		
		<p>Les dades de COVID-19 sobre els casos diagnosticats i la mortalitat des de l'1 de Gener del 2020 en endavant es troben en <a href='https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide'>European Centre for Disease Prevention and Control geographical distribution of COVID-19 cases worldwide (ECDC, 2020).</a> </p>
				<br>              

		<H4>Anàlisis de la tendència i projeccions</H4>
                <p>Per analitzar la tendència i obtenir projeccions a tres dies, s'utilitza un model de regressió de Poisson (Dyba and Hakulinen, 2000), corregit per sobredispersió (Navarro et al., 2001), ajustant termes lineal, quadràtic i cúbic de tendència diària:</p>
 
		$$\\log(E(c_t))= \\beta_0 + \\beta_1  t + \\beta_2  t^2 + \\beta_3  t^3$$
      
		<p>on \\(t=1,2,\\ldots,T\\) representa la unitat temporal (des del primer dia fins a l'últim observat, \\(T\\) dies consecutius en total) i \\(c_t\\) el nombre d'esdeveniments diaris.</p>
		<p>Les projeccions a tres dies, i el seu interval de confiança al 95%, s'obtenen a partir dels paràmetres estimats pel model. També es calculen les tendències i projeccions per grups d'edat (0-39, 40-49, 50-59, 60-69, 70-79 i 80 o més anys).</p>
		<p>En versions prèvies de l'aplicació també es va considerar un model alternatiu, incloent-hi únicament la tendència lineal o tendència lineal i la quadràtica  i els models es comparaven mitjançant una prova de raó de similituds. A partir de l'evolució de l'epidèmia, observem que el millor ajust el proporcionava el model cúbic, descrit anteriorment, de manera que és el model que s'utilitza en la versió actual. Excepte per als casos incidents que s'utilitza el model quadràtic. En qualsevol cas, la bondat d'ajust dels models s'avalua regularment en cas de ser necessari una reformulació que pogués proporcionar un millor ajust de les dades durant el transcurs de l'epidèmia.</p>
		<br>
		<H4>Anàlisis de la letalitat</H4>
		<p>La taxa de letalitat es defineix com el quocient entre els morts i els casos diagnosticats (Rothman and Greenland, 1998). Per avaluar la tendència s'utilitza un model de regressió de Poisson (Dyba and Hakulinen, 2000), corregit per sobredispersió (Dyba and Hakulinen, 2000), ajustant termes lineal, quadràtic i cúbic de tendència diària i incloent un terme òfset amb els casos diagnosticats:  </p>

		$$\\log(E(m_t))= \\beta_0 + \\beta_1  t + \\beta_2  t^2 + \\beta_3  t^3 + \\log(c_t)$$
      
		<p>on \\(m_t\\) representa el nombre diari de pacients morts, \\(c_t\\) els casos diagnósticats.També es calculen les taxes de letalitat pels mateixos grups d'edat.</p>
		<p>En l'actualitat, no és possible realitzar una estimació precisa de la taxa de letalitat causa del grau de subregistre dels casos diagnosticats en les estadístiques oficials (Battegay et al., 2020). Si bé és d'interès la seva estimació i seguiment considerant aquesta limitació.</p>
		<br>
		<H4>Anàlisis del moment estimat d'infecció</H4>
		<p>Lauer et al. (Lauer et al., 2020) han analitzat recentment el període d'incubació de la COVID-19 en una cohort de pacients simptomàtics. De cada pacient van recollir l'interval d'exposició al SARS-CoV-2 i la data d'aparició de símptomes. Van assumir que el temps d'incubació seguiria, com en altres infeccions virals del tracte respiratori, una distribució Lognormal.</p>

		$$\\ Lognormal(\\mu,\\sigma^2) = Lognormal(1.621 , 0.418)$$

		<p>En un exercici naïf i no exempt de limitacions, hem replicat aquesta distribució en el conjunt de casos diagnosticats per aproximar la data d'exposició al SARS-CoV-2 de forma recursiva:</p>

		$$\\ q(i)= \\sum_{j=1}^{14} P(j)*c_{j+i}$$

		<p>On \\(q\\) és el nombre de casos infectats a la data \\(i\\), \\(c\\) el nombre de casos diagnosticas a la data \\(j+i\\), i \\(P(j)\\) és la probabilitat de presentar símptomes el dia \\(j\\) segons una llei Lognormal amb els paràmetres definits per Lauer et al. (Lauer et al., 2020)
		Per a l'estimació dels últims 14 dies, al no disposar de la informació dels casos diagnosticats per als següents 14, s'ha utilitzat el model quadràtic descrit amb anterioritat per a la projecció de casos diagnosticats incidents. En l'aplicació es mostren aquestes últimes estimacions d'un color diferent. </p>
		<br>
				<H4>Evolució índex R</H4>
			<p>El nombre de reproducció, \\(R\\), és el valor mitjà de segones infeccions causades per un sol individu infectat al llarg del seu període infecciós. Aquest estadístic, que varia en el temps i la situació, és utilitzat per a caracteritzar la transmissibilitat d'un patogen en una epidèmia. El monitoratge de \\(R\\) en el temps et dóna una possible lectura de l'efectivitat de les intervencions i de la necessitat d'intensificar les mesures de control, ja que l'objectiu d'aquestes és reduir \\(R\\) per sota del valor llindar d'1 i el més a prop possible de 0, moment en el qual estaria l'epidèmia sota control.</p>
		<p>Aquí hem utilitzat el paquet EpiEstim per a obtenir el nombre de reproducció \\(R_t\\) utilitzant el mètode de Wallinga i Teunis. Aquest mètode paramètric parteix d'una distribució gamma per l'interval de sèrie. L'interval de sèrie és el temps entre l'aparició de símptomes en el primer cas i l'aparició de símptomes en el segon, que és necessari per a estimar \\(R_t\\) en el curs d'una epidèmia.</p>
		<p> La mitjana i la desviació estàndard de la distribució de l'interval de sèrie pot variar entre malalties. Pel xarampió es va estimar una mitjana (SD) de 14.9 (3.9) dies (Groendyke et al.); per a la influenza pandèmica 2.6 (1.5) (Ferguson et al.; Boelle et al.); per a la verola 22.4 (6.1) (Riley and Ferguson); per la SARS 8.4 (3.8) (Lipsitch et al.).</p>
		<p>En un article recent de Nishiura et al. (Serial interval of novel coronavirus (COVID-19) infections. Int J Infect Dis. 2020 Mar 4;93:284-286) es va estimar una mitjana i una desviació estàndard per la COVID-19 de 4.7 dies (95% CrI: 3.7, 6.0) i 2.9 dies (95% CrI: 1.9, 4.9) respectivament, que és molt més petita que la de la SARS i la MERS. Aquests són els valors, 4.7 (2.9), que utilitzem en la nostre anàlisi.</p>
		<p> Pel càlcul s'ha utilitzat el paquet de R EpiEstim, desenvolupat per Cori et al (American Journal of Epidemiology, 2013).</p>
		<br>
			<H4>Limitacions </H4>
		
		<p>Les estimacions no són exactes degut a la infradeclaració de casos diagnosticats i mortalitat en les estadístiques oficials (<a href='https://www.ncbi.nlm.nih.gov/pubmed/32031234'>Battegay et al. 2020</a>). </p>
		
		<p>La selecció del nombre de persones que s'han fet una proba diagnòstica és crucial per a una estimació precisa (<a href='https://www.ncbi.nlm.nih.gov/pubmed/32324422'>Pearce et al. 2020</a>). La precisió de l'estimació de les taxes depèn de l'estratègia a l'hora de fer els tests, de la prevalència de la infecció i de la precisió dels tests. Diferències entre països podrien significar únicament diferències en la selecció a l'hora de fer tests i del funcionament dels mateixos.(<a href='https://www.ncbi.nlm.nih.gov/pubmed/32324422'>Pearce et al. 2020</a>).</p> 
		
		<p>L'aplicació no té en compte els canvis en la definició de casos diagnosticats ni en les mesures de confinament contra el coronavirus que han tingut lloc en els diferents països amb la intenció d'aplanar la corba.</p>
		<H4>Referències</H4>
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